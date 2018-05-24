------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . M u t e x e s . I n t e r n a l s'
--
--                                  Body
--
--
--  File 'k-mutexes-internals.adb'                                     By MAR.
--
--
--  Mutexes related procedures not included in the POSIX standard. Only for
--  internal use.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------

with MaRTE.Kernel.Scheduler;
pragma Elaborate_All (MaRTE.Kernel);
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Tasks_Lists_Prio;
with MaRTE.SLL.Prio;
pragma Elaborate_All (MaRTE.SLL.Prio);
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Mutexes.SRP_Ceiling;

--  Debug
--  with MaRTE.Direct_IO;
with MaRTE.Kernel.Mutexes.Debug;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Mutexes.Internals is

   use K.Mutexes_Lists;
   use K.Tasks_Lists;

   use type Int;

   --  Renames
   package TOI          renames K.Tasks_Operations.Internals;
   package SCHD         renames K.Scheduler;
   package MLST         renames K.Mutexes_Lists;
   package TLST         renames K.Tasks_Lists;
   package APPSCHD      renames K.Application_Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package TCLST        renames K.Task_Containers_Lists;
   package TCLST_ORDER  renames K.Task_Containers_Lists_Order;
   package DBG          renames K.Debug;
   subtype AS           is      APPSCHD_DATA.AppScheduler_Data_Ac;
   subtype SA           is      APPSCHD_DATA.Sched_App_Data_Ac;

   --------------
   -- Mutex_OK --
   --------------
   function Mutex_OK (M : in Mutex_Descriptor) return Boolean is
   begin
      return M /= null and then M.Magic = INITIALIZED;
   end Mutex_OK;
   function Mutex_OK (M           : in Mutex_Descriptor;
                      Extra_Magic : in Magic_Check) return Boolean is
   begin
      return M /= null and then (M.Magic = INITIALIZED or
                                 M.Magic = Extra_Magic);
   end Mutex_OK;
   function Mutex_OK (M : in Mutex) return Boolean is
   begin
      return M.Magic = INITIALIZED;
   end Mutex_OK;

   --------------------------------
   -- package Mutexes_Lists_Prio --
   --------------------------------
   function ">" (Left, Right : in MLST.Element_Ac) return Boolean;
   pragma Inline (">");

   function ">" (Left, Right : in MLST.Element_Ac) return Boolean is
   begin
      return Mutex (Left.all).Prio > Mutex_Descriptor (Right).Prio;
   end ">";

   function Get_Priority (M : in MLST.Element_Ac) return Task_Priority;
   pragma Inline (Get_Priority);

   function Get_Priority (M : in MLST.Element_Ac) return Task_Priority is
   begin
      return Mutex (M.all).Prio;
   end Get_Priority;

   package MLST_Prio is new MLST.Prio (Task_Priority, ">", Get_Priority);

   ------------------------------
   -- Running_Task_Locks_Mutex --
   ------------------------------
   procedure Running_Task_Locks_Mutex (M : in Mutex_Descriptor) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));

      M.Owner := SCHD.Self;
      MLST.Enqueue_Head (MLST.Element_Ac (M), SCHD.Self.Mutexes_Owned);

      ------------
      --  Keep track of the SRP system ceiling
      ----
      M.SRP_Mutex_Lock.M := Mutex_Descriptor (M);
      Mutexes.SRP_Ceiling.Add_Mutex_Lock (M.SRP_Mutex_Lock'Access);

      ------------
      --  Inherits Priority
      ----
      if SCHD.Self.Sched_Policy = SCHED_APP then
         --  Revise the active priority of the task
         if SCHD.Self.Active_Prio < M.Prio then
            SCHD.Ready_Task_Raises_Active_Priority (SCHD.Self, M.Prio);
         end if;
         --  Send event to the scheduler
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_PRIORITY_INHERIT,
            ST         => SA (SCHD.Self.Sched_App).Scheduler,
            T          => SCHD.Self,
            Prio       => M.Prio);

      else -- Policy /= SCHED_AP
         --  revise the active priority of the task
         if SCHD.Self.Active_Prio < M.Prio then
            if not SCHD.Self.Reenqueuement_Deferred then
               SCHD.Self.Old_Active_Prio := SCHD.Self.Active_Prio;
               SCHD.Self.Old_Active_Preemption_Level :=
                 SCHD.Self.Active_Preemption_Level;
               SCHD.Self.Reenqueuement_Deferred := True;
               --  Only reenqueued if necesary (a higher priority task gets
               --  activated while this task is still looking the mutex).
            end if;
            SCHD.Self.Active_Prio := M.Prio;

         elsif SCHD.Self.Active_Prio = M.Prio
           and then SCHD.Self.Active_Preemption_Level < M.Preemption_Level
         then
            --  Even if the mutex prio is equal to the task prio, the
            --  preemption level can be higher and then it must be "inherited"
            --  by the task

            SCHD.Self.Active_Preemption_Level := M.Preemption_Level;
         end if;
      end if;

      pragma Debug (Debug.Running_Task_Locks_Mutex (M)); -- Trace
   end Running_Task_Locks_Mutex;

   ------------------------------
   -- Raise_HBT_Mutex_Priority --
   ------------------------------
   procedure Raise_HBT_Mutex_Priority (M        : in MLST.Element_Ac;
                                       New_Prio : in Task_Priority) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (Mutex_Descriptor (M))));

      if New_Prio > Mutex_Descriptor (M).Prio then
         Mutex_Descriptor (M).Prio := New_Prio;
         Mutexes.SRP_Ceiling.Reorder_Mutex_Lock
           (Mutex_Descriptor (M).SRP_Mutex_Lock'Access);
         if New_Prio > Mutex_Descriptor (M).Owner.Active_Prio then
            TOI.Raise_Active_Prio (Mutex_Descriptor (M).Owner, New_Prio);
         end if;
      end if;

      --  Send event to the scheduler (if SCHED_APP)
      if Mutex_Descriptor (M).Owner.Sched_Policy = SCHED_APP then
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_PRIORITY_INHERIT,
            ST         =>
              SA (Mutex_Descriptor (M).Owner.Sched_App).Scheduler,
            T          => Mutex_Descriptor (M).Owner,
            Prio       => Mutex_Descriptor (M).Prio);
      end if;
   end Raise_HBT_Mutex_Priority;

   -------------------------------
   -- Reduce_HBT_Mutex_Priority --
   -------------------------------
   procedure Reduce_HBT_Mutex_Priority (M        : in MLST.Element_Ac;
                                        New_Prio : in Task_Priority;
                                        Head_Of_New_Prio_Q : in Boolean) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (Mutex_Descriptor (M))));

      if New_Prio = Mutex_Descriptor (M).Prio then
         Mutex_Descriptor (M).Prio :=
           Tasks_Lists_Prio.Find_Mx_Prio (Mutex_Descriptor (M).Blocked_Tasks);
         Mutexes.SRP_Ceiling.Reorder_Mutex_Lock
           (Mutex_Descriptor (M).SRP_Mutex_Lock'Access);
         if (Mutex_Descriptor (M).Owner.Active_Prio <
             MLST_Prio.Find_Mx_Prio (Mutex_Descriptor (M).Owner.Mutexes_Owned))
         then
            TOI.Reduce_Active_Prio
              (Mutex_Descriptor (M).Owner,
               MLST_Prio.Find_Mx_Prio (Mutex_Descriptor (M).
                                                          Owner.Mutexes_Owned),
               Head_Of_New_Prio_Q);
         end if;
      end if;

      --  Send event to the scheduler (if SCHED_APP)
      if Mutex_Descriptor (M).Owner.Sched_Policy = SCHED_APP then
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_PRIORITY_UNINHERIT,
            ST => SA (Mutex_Descriptor (M).Owner.Sched_App).Scheduler,
            T          => Mutex_Descriptor (M).Owner,
            Prio       => Mutex_Descriptor (M).Prio);
      end if;
   end Reduce_HBT_Mutex_Priority;

   ----------------------------------
   -- Task_Gets_Ready_Owning_Mutex --
   ----------------------------------
   procedure Task_Gets_Ready_Owning_Mutex (T : in Task_Id;
                                           M : in Mutex_Descriptor) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));
      pragma Debug (DBG.Assert (K.Debug.Task_OK (T)));

      M.Owner := T;
      MLST.Enqueue_Head (MLST.Element_Ac (M), T.Mutexes_Owned);
      T.HBT_Mutex_Where_Blocked := null;

      --  revise the active priority of the task
      if T.Active_Prio < M.Prio then
         T.Active_Prio := M.Prio;
         T.Active_Preemption_Level := M.Preemption_Level;
      end if;

      pragma Debug (Debug.Task_Gets_Ready_Owning_Mutex (T, M)); -- Trace

      --  Now 'T' is ready
      SCHD.Task_Gets_Ready (T);

      --  Keep track of the SRP system ceiling
      Mutexes.SRP_Ceiling.Add_Mutex_Lock (M.SRP_Mutex_Lock'Access);

      --  Send event to the scheduler (if SCHED_APP)
      if T.Sched_Policy = SCHED_APP then
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_PRIORITY_INHERIT,
            ST         => SA (T.Sched_App).Scheduler,
            T          => T,
            Prio       => M.Prio);
      end if;
   end Task_Gets_Ready_Owning_Mutex;

   -----------------------------------
   -- Enqueue_Blocked_Task_In_Mutex --
   -----------------------------------
   procedure Enqueue_Blocked_Task_In_Mutex (T : in Task_Id;
                                            M : in Mutex_Descriptor) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));
      pragma Debug (DBG.Assert (K.Debug.Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status = BLOCKED or
                                T.Status = TIMED_WAITING_MUTEX));

      --  Enqueue at tail of queue to keep the FIFO order between same
      --  priority tasks
      TLST.Enqueue_Tail (T, M.Blocked_Tasks);
      --  Must the priority of the mutex (and of the task which owns
      --  the mutex) be raised ??
      if M.Policy = HIGHEST_BLOCKED_TASK then
         T.HBT_Mutex_Where_Blocked := MLST.Element_Ac (M);
         if M.Prio < T.Active_Prio then
            --  Yes, they both must be raised.
            Raise_HBT_Mutex_Priority (MLST.Element_Ac (M),
                                      T.Active_Prio);
         end if;
      end if;

      pragma Debug (Debug.Enqueue_Blocked_Task_In_Mutex (T, M)); -- Trace
   end Enqueue_Blocked_Task_In_Mutex;

   ------------------------
   -- Task_Unlocks_Mutex --
   ------------------------
   procedure Task_Unlocks_Mutex (T : in Task_Id;
                                 M : in Mutex_Descriptor) is
      Mx_Prio       : K.Task_Priority;
      Mx_Prio_Mutex : Mutex_Descriptor;
      T_Heir : Task_Id;
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));
      pragma Debug (DBG.Assert (K.Debug.Task_OK (T)));

      pragma Debug (Debug.Task_Unlocks_Mutex1 (T, M)); -- Trace

      MLST.Dequeue (MLST.Element_Ac (M), T.Mutexes_Owned);

      --  Find the highest priority mutexes owned by the task
      Mx_Prio_Mutex :=
        Mutex_Descriptor (MLST_Prio.Find_Element_Mx_Prio (T.Mutexes_Owned));
      if Mx_Prio_Mutex /= null then
         Mx_Prio := Mx_Prio_Mutex.Prio;
      else
         Mx_Prio := K.Task_Priority'First;
      end if;

      --  Application-schedulers inherit all the priorities inherited by its
      --  scheduled tasks.
      if T.AppScheduler /= null then
         --  The task is an application-scheduler
         declare
            Mx_Prio_Sched : Task_Priority;
         begin
            if not TCLST.Is_Empty (AS (T.AppScheduler).Tasks_Q) then
               Mx_Prio_Sched := TCLST_ORDER.Find_Mx_Prio
                 (AS (T.AppScheduler).Tasks_Q).T.Active_Prio;
               if Mx_Prio_Sched > Mx_Prio then
                  Mx_Prio := Mx_Prio_Sched;
               end if;
            end if;
         end;
      end if;

      --  Is Reenqueuement Deferred finished ??
      if (T.Reenqueuement_Deferred and then
          MLST.Is_Empty (T.Mutexes_Owned)) then
         T.Reenqueuement_Deferred := False;
         T.Active_Prio := T.Old_Active_Prio;
         T.Active_Preemption_Level := T.Old_Active_Preemption_Level;
      end if;

      --  Does the priority of the task have to be reduced ??
      if T.Active_Prio >= Mx_Prio then
         if T.Base_Prio <= Mx_Prio then
            if Mx_Prio_Mutex /= null then
               T.Active_Preemption_Level := Mx_Prio_Mutex.Preemption_Level;
            else
               T.Active_Preemption_Level := T.Base_Preemption_Level;
            end if;
            TOI.Reduce_Active_Prio (T, Mx_Prio,
                                    Head_Of_New_Prio_Q => True);
         else --  T.Base_Prio > Mx_Prio
            T.Active_Preemption_Level := T.Base_Preemption_Level;
            TOI.Reduce_Active_Prio (T, T.Base_Prio,
                                    Head_Of_New_Prio_Q => True);
         end if;
      end if;

      --  Keep track of the SRP system ceiling
      Mutexes.SRP_Ceiling.Del_Mutex_Lock (M.SRP_Mutex_Lock'Access);

      --  Send event to the scheduler (if SCHED_APP)
      if T.Sched_Policy = SCHED_APP then
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_PRIORITY_UNINHERIT,
            ST         => SA (T.Sched_App).Scheduler,
            T          => T,
            Prio       => M.Prio);
      end if;

      --------------
      --  Ulock mutex or activate most prioritary task
      ----

      --  unlock mutex (only for a while if there is some task blocked in the
      --  queue)
      M.Owner := null;

      if TLST.Is_Empty (M.Blocked_Tasks) then
         if M.Policy = HIGHEST_BLOCKED_TASK then
            M.Prio := Task_Priority'First;
         end if;

         pragma Debug (Debug.Task_Unlocks_Mutex2); -- Trace

      else
         --  There is at least one task blocked in the mutex, then the most
         --  prioritary becomes ready
         Tasks_Lists_Prio.Dequeue_Element_Mx_Prio (M.Blocked_Tasks, T_Heir);
         --  'T_Heir' is the most prioritary task blocked in 'M'

         --  Decrease the priority of the mutex ??
         if M.Policy = HIGHEST_BLOCKED_TASK then
            if TLST.Is_Empty (M.Blocked_Tasks) then
               M.Prio := Task_Priority'First;
            else
               M.Prio := Tasks_Lists_Prio.Find_Mx_Prio (M.Blocked_Tasks);
            end if;
         end if;

         --  'T_Heir' becomes ready
         SCHD.Task_Gets_Ready (T_Heir);
      end if;
   end Task_Unlocks_Mutex;

   ---------------
   -- Set_Owner --
   ---------------
   procedure Set_Owner (M : in Mutex_Descriptor;
                        T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));
      pragma Debug (DBG.Assert (K.Debug.Task_OK (T)));

      M.Owner := T;
   end Set_Owner;

   ---------------
   -- Get_Owner --
   ---------------
   function Get_Owner (M : in Mutex_Descriptor) return Task_Id is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));

      return M.Owner;
   end Get_Owner;

   -------------------------------------
   -- Decrement_Num_Of_Associated_CVs --
   -------------------------------------
   procedure Decrement_Num_Of_Associated_CVs (M : in Mutex_Descriptor) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));

      M.Num_Of_Associated_CVs := M.Num_Of_Associated_CVs - 1;
   end Decrement_Num_Of_Associated_CVs;

   -------------------------------------
   -- Increment_Num_Of_Associated_CVs --
   -------------------------------------
   procedure Increment_Num_Of_Associated_CVs (M : in Mutex_Descriptor) is
   begin
      pragma Debug (DBG.Assert (Mutex_OK (M)));

      M.Num_Of_Associated_CVs := M.Num_Of_Associated_CVs + 1;
   end Increment_Num_Of_Associated_CVs;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached  (T : in Task_Id; Mb : in Mutex_Base_Ac) is
   begin
      pragma Debug (DBG.Assert (Task_OK (T) and
                                Mutex_OK (Mutex_Descriptor (Mb))));
      pragma Debug (DBG.Assert (T.Timed_Waiting_Mutex));

      --  Set the internal notification to record the task is
      --  activated due to a timeout (it will be checked in
      --  'Mutexes.Pthread_Mutex_Timedlock' y en 'Mutexes.Pthread_Mutex_Lock'
      --  when 'T' is scheduled again)
      pragma Debug (DBG.Assert (T.Internal_Error_Code = NO_ERROR));
      T.Internal_Error_Code := TIMED_OUT;

      case T.Status is
         when TIMED_WAITING_MUTEX =>
            --  Dequeue task from the mutex's blocked tasks list
            TLST.Dequeue (T, Mutex_Descriptor (Mb).Blocked_Tasks);

            if Mutex_Descriptor (Mb).Policy = HIGHEST_BLOCKED_TASK then
               --  Reduce mutex ceiling
               Reduce_HBT_Mutex_Priority
                 (MLST.Element_Ac (Mutex_Descriptor (Mb)),
                  T.Active_Prio,
                  Head_Of_New_Prio_Q => False);
            end if;

            --  Task becomes ready
            SCHD.Task_Gets_Ready (T);
         when READY =>
            null;
         when others =>
            pragma Debug (DBG.Assert (False));
            null;
      end case;

   end Timeout_Reached;

   ----------------------------
   --  Initialize_HCP_Mutex  --
   ----------------------------
   --
   --  Used internally in the kernel. Currently only from
   --  'k-file_system.Open'.
   procedure Initialize_HCP_Mutex (M       : access Mutex;
                                   Ceiling : in     Ceiling_Priority) is
   begin
      M.all              := PTHREAD_MUTEX_INITIALIZER;
      M.Policy           := HIGHEST_CEILING_PRIORITY;
      M.Prio             := K.Task_Priority (Ceiling);
      M.SRP_Mutex_Lock.M := Mutex_Descriptor (M);
   end Initialize_HCP_Mutex;

end MaRTE.Kernel.Mutexes.Internals;
