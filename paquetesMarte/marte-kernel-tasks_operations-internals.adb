------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--      'K e r n e l . T a s k s _ O p e r a t i o n s . I n t e r n a l s'
--
--                                 Body
--
--
--  File 'k-to-internals.adb'                                          By MAR.
--
--
--  Tasks operations procedures needed by the rest of 'Kernel'. Only for
--  internal use inside the herarchy of 'Kernel'.
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
-------------------------------------------------------------------------------
with System;
with Ada.Unchecked_Conversion;

with MaRTE.Configuration_Parameters;
with MaRTE.HAL; use MaRTE.HAL;

with MaRTE.Kernel.Pool_TCBs;
with MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Condition_Variables.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Signals.Pool_SDBs;
with MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Signals.Handler;
with MaRTE.Kernel.Application_Scheduling_Data;
use MaRTE.Kernel;
with MaRTE.Direct_IO;
with MaRTE.Timespec;

--  Debug
with MaRTE.Kernel.Debug;
with MaRTE.Kernel.Tasks_Operations.Debug;

package body MaRTE.Kernel.Tasks_Operations.Internals is

   package TSTE         renames K.Task_Suspension_Timed_Events; use TSTE;
   package SDBs         renames K.Signals.Pool_SDBs;
   package SI           renames K.Signals.Internals;
   package MI           renames K.Mutexes.Internals;
   package CVI          renames K.Condition_Variables.Internals;
   package SCHD         renames K.Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package CP           renames MaRTE.Configuration_Parameters;
   package TO           renames K.Tasks_Operations;
   package DBG          renames K.Debug;

   use K.Tasks_Lists, MaRTE.Kernel.Mutexes_Lists, Timed_Events_Lists;

   use type Int;

   subtype SA is APPSCHD_DATA.Sched_App_Data_Ac;

   -------------
   -- Attr_OK --
   -------------
   function Attr_OK (Attr : in Pthread_Attr_T_Ac_Const) return Boolean is
   begin
      return Attr /= null and then Attr.Magic = ATTR_INITIALIZED;
   end Attr_OK;

   ---------------------------------------------------------------------------
   -- Scheduling -------------------------------------------------------------
   ---------------------------------------------------------------------------

   -----------------------
   -- Wrapper procedure --
   -----------------------
   procedure Task_Wrapper (Task_Body : in K.Task_Body_Function) is
      Ret_Val : System.Address;
   begin
      HAL.Enable_Interrupts;
      Ret_Val := Task_Body.all (SCHD.Self.Arg);

      HAL.Disable_Interrupts;
      Terminate_Running_Task (Ret_Val);

      --  This line should never be reached
      MaRTE.Direct_IO.Error ("#40#", Fatal => True);
   end Task_Wrapper;

   ---------------------------------------------------------------------------
   -- Basic Task Operations --------------------------------------------------
   ---------------------------------------------------------------------------

   -------------------------
   -- Get_Self_Attributes --
   -------------------------
   function Get_Self_Attributes return Pthread_Attr_T is
      App_Scheduler_State : Regular_Or_App_Scheduler_T := PTHREAD_REGULAR;
      AppSched_Param      : AppSched_Param_T := (others => 0);
      AppSched_Param_Size : AppSched_Param_Size_T := 0;
   begin
      if MaRTE.Configuration_Parameters.Use_Application_Defined_Scheduling then
         if SCHD.Self.AppScheduler /= null then
            App_Scheduler_State := PTHREAD_APPSCHEDULER;
         end if;
         AppSched_Param_Size := SA (SCHD.Self.Sched_App).Param_Size;
         AppSched_Param (1 .. AppSched_Param_Size) :=
           SA (SCHD.Self.Sched_App).Param  (1 .. AppSched_Param_Size);
      end if;
      return
        (Magic         => ATTR_INITIALIZED,
         Sched_Policy  => SCHD.Self.Sched_Policy,
         Detach_State  => SCHD.Self.Detach_State,
         Inherit_Sched => PTHREAD_EXPLICIT_SCHED,
         Param => (Sched_Priority        => SCHD.Self.Base_Prio,
                   Sched_SS_Low_Priority => SCHD.Self.Sched_SS_Low_Priority,
                   Sched_SS_Repl_Period  => MaRTE.Timespec.HWTime_To_Timespec
                   (SCHD.Self.Sched_SS_Repl_Period),
                   Sched_SS_Init_Budget  => MaRTE.Timespec.HWTime_To_Timespec
                   (SCHD.Self.Capacity),
                   Sched_SS_Max_Repl     => SCHD.Self.Sched_SS_Max_Repl),
         Stack_Size          => Size_T
           (MaRTE.Stacks_Management.Get_Stack_Size_In_Bytes (SCHD.Self.Stack)),
         Rel_Deadline        => SCHD.Self.Rel_Deadline,
         Preemption_Level    => SCHD.Self.Base_Preemption_Level,
         App_Scheduler       => SA (SCHD.Self.Sched_App).Scheduler,
         AppSched_Param      => AppSched_Param,
         AppSched_Param_Size => AppSched_Param_Size,
         App_Scheduler_State => App_Scheduler_State);
   end Get_Self_Attributes;

   -----------------
   -- Create_Task --
   -----------------
   Tasks_Counter : Integer := Pool_TCBs.First_User_Task_Id;
   --  'Tasks_Counter' is used in order to give a different ID to each
   --  new task. IDs 0, 1 and 2 are reserved for the main, idle and
   --  signal handler tasks.
   function Create_Task (Task_Body   : in Task_Body_Function;
                         Arg         : in System.Address;
                         Attr        : in Pthread_Attr_T)  return Task_Id is
      New_Task          : Task_Id;
      use type K.Task_Id, APPSCHD_DATA.AppScheduler_Data_Ac;
   begin
      pragma Debug (Debug.Create_Task1 (Tasks_Counter, Attr)); -- Trace

      --  Request free TCB
      New_Task :=
        Pool_TCBs.Request_TCB (Task_Type    => K.USER_TASK,
                               Stack_Size   => Integer (Attr.Stack_Size),
                               AppScheduler => Attr.App_Scheduler_State =
                                               PTHREAD_APPSCHEDULER);
      if New_Task = null then
         --  Impossible create more tasks
         pragma Debug (MaRTE.Direct_IO.Error ("Reached max. num. of tasks"));
         return null;
      end if;

      --  Reached limit of scheduled tasks ?
      if (MaRTE.Configuration_Parameters.Use_Application_Defined_Scheduling
          and then Attr.Sched_Policy = SCHED_APP
          and then not APPSCHD_DATA.Any_Task_Container_Free_In_Pool) then
         --  Impossible create more application scheduled tasks
         New_Task.Magic := K.TERMINATED;  --  to keep 'Release_TCB' happy
         Pool_TCBs.Release_TCB (New_Task);
         return null;
      end if;

      --  TCB inicialization
      Initialize_TCBs.Initialize
        (T                 => New_Task,
         ID                => Tasks_Counter,
         Attr              => Attr,
         Task_Body         => Task_Body,
         Arg               => Arg,
         Task_Wrapper      => Task_Wrapper'Address);

      if New_Task.Sched_Policy = SCHED_APP then
         New_Task.Task_Waiting_Appscheduler_Response := TCB_Ac (SCHD.Self);
         --  Out of 'Initialize_TCB' to avoid an elaboration circularity
         --  problem.
      end if;

      Signals.Global.Sig_Data_Ac (New_Task.Sig_Data).Mask :=
        Signals.Global.Sig_Data_Ac (SCHD.Self.Sig_Data).Mask;
      SDBs.Register_Used_SDB (New_Task);

      Tasks_Counter := Tasks_Counter + 1;

      return New_Task;
   end Create_Task;

   ----------------------------
   -- Terminate_Running_Task --
   ----------------------------
   procedure Terminate_Running_Task (Value_Ptr : in System.Address) is
   begin
      pragma Debug (DBG.Assert (K.Task_OK (SCHD.Self)));
      SCHD.Self.Ret_Val := Value_Ptr;
      SCHD.Self.Magic := TERMINATED;

      --  No more signals for this task
      SI.Task_Stops_Getting_Signals (SCHD.Self);

      pragma Debug (Debug.Remove_Task1 (SCHD.Self)); -- Trace

      --  Joinable or detached related actions
      if SCHD.Self.Detach_State = PTHREAD_CREATE_JOINABLE then
         --  Joinable, then block
         SCHD.Running_Task_Gets_Blocked (With_Status => BLOCKED);

         --  Remove task's events in the timed events queue (if any).
         SCHD.Remove_Task (SCHD.Self);

         --  There is a task waiting for its termination ?
         if SCHD.Self.Task_Waiting_For_Termination /= null then
            --  Activate it
            SCHD.Task_Gets_Ready
              (TCB_Ac_To_Task_Id (SCHD.Self.Task_Waiting_For_Termination));
            --  TCB of 'T' will be released for the task pointed
            --  by 'SCHD.Self.Task_Waiting_For_Termination' in the function
            --  'TO.Pthread_Join'.

         end if;
      else
         --  Detached

         --  Remove task's events in the timed events queue (if any).
         SCHD.Remove_Task (SCHD.Self);

      end if;

      --  Change to the new "most prioritary task" and release the
      --  resources of the "just terminated" task.
      SCHD.Do_Scheduling;

      --  This line should never be reached
      MaRTE.Direct_IO.Error ("#42#", Fatal => True);
   end Terminate_Running_Task;

   -------------------
   -- Set_Base_Prio --
   -------------------
   procedure Set_Base_Prio (T                  : in Task_Id;
                            New_Base_Prio      : in Task_Priority;
                            Head_Of_New_Prio_Q : in Boolean := False) is
   begin
      T.Base_Prio := New_Base_Prio;
      if Mutexes_Lists.Is_Empty (T.Mutexes_Owned) then
         if New_Base_Prio > T.Active_Prio then
            Internals.Raise_Active_Prio (T, New_Base_Prio);
         else
            Internals.Reduce_Active_Prio (T, New_Base_Prio,
                                          Head_Of_New_Prio_Q);
         end if;
      else -- not Mutexes_Lists.Is_Empty (Mutexes_Owned)
         if New_Base_Prio > T.Active_Prio then
            Internals.Raise_Active_Prio (T, New_Base_Prio);
         end if;
      end if;
   end Set_Base_Prio;

   ------------------------
   -- Reduce_Active_Prio --
   ------------------------
   procedure Reduce_Active_Prio (T                  : in Task_Id;
                                 New_Prio           : in Task_Priority;
                                 Head_Of_New_Prio_Q : in Boolean) is
   begin
      if T.Status = READY then
         SCHD.Ready_Task_Reduces_Active_Priority (T,
                                                  New_Prio,
                                                  Head_Of_New_Prio_Q);
      else -- T.State /= READY
         pragma Debug  --  never both conditions
           (DBG.Assert (not (T.CV_Where_Blocked /= null
                             and T.HBT_Mutex_Where_Blocked /= null)));
         if T.HBT_Mutex_Where_Blocked /= null then
            T.Active_Prio := New_Prio;
            MI.Reduce_HBT_Mutex_Priority (T.HBT_Mutex_Where_Blocked,
                                          New_Prio,
                                          Head_Of_New_Prio_Q);
         elsif T.CV_Where_Blocked /= null then
            CVI.Reorder_Task_In_Queue (T, New_Prio => New_Prio);
            --  This procedure also sets 'T.Active_Prio' to
            --  'New_Prio'.

         else
            --  Task blocked or suspended without any inherited priority:
            --  change active priority

            pragma Debug
              (DBG.Assert (Mutexes_Lists.Is_Empty (T.Mutexes_Owned)));
            T.Active_Prio := New_Prio;
         end if;
      end if;
   end Reduce_Active_Prio;

   -----------------------
   -- Raise_Active_Prio --
   -----------------------
   procedure Raise_Active_Prio (T        : in Task_Id;
                                New_Prio : in Task_Priority) is
   begin
      if T.Status = READY then
         SCHD.Ready_Task_Raises_Active_Priority (T, New_Prio);
      else -- T.State /= READY
         pragma Debug  --  never both conditions
           (DBG.Assert (not (T.CV_Where_Blocked /= null
                             and T.HBT_Mutex_Where_Blocked /= null)));
         if T.HBT_Mutex_Where_Blocked /= null then
            T.Active_Prio := New_Prio;
            MI.Raise_HBT_Mutex_Priority (T.HBT_Mutex_Where_Blocked,
                                         New_Prio);
         elsif T.CV_Where_Blocked /= null then
            CVI.Reorder_Task_In_Queue (T, New_Prio => New_Prio);
            --  This procedure also sets 'T.Active_Prio' to
            --  'New_Prio'.

         else
            --  Task blocked or suspended without any inherited priority:
            --  change active priority

            pragma Debug
              (DBG.Assert (Mutexes_Lists.Is_Empty (T.Mutexes_Owned)));
            T.Active_Prio := New_Prio;
         end if;
      end if;
   end Raise_Active_Prio;

   ----------------------------------------------
   -- Set_POSIX_Error_And_Leave_Critic_Section --
   ----------------------------------------------
   procedure Set_POSIX_Error_And_Leave_Critic_Section (Error : in Error_Code;
                                                       Flags : in Integer) is
   begin
      SCHD.Self.Error_Code  := Error;
      SCHD.Self.POSIX_Error := True;
      K.Leave_Critic_Section (Flags);
   end Set_POSIX_Error_And_Leave_Critic_Section;

   ---------------------
   -- Set_POSIX_Error --
   ---------------------
   procedure Set_POSIX_Error (Error : in Error_Code) is
   begin
      SCHD.Self.Error_Code  := Error;
      SCHD.Self.POSIX_Error := True;
   end Set_POSIX_Error;

   ---------------------
   -- Set_POSIX_Error --
   ---------------------
   procedure Set_POSIX_Error (T : in Task_Id; Error : in Error_Code) is
   begin
      pragma Debug (DBG.Assert (K.Task_OK (T)));
      T.Error_Code  := Error;
      T.POSIX_Error := True;
   end Set_POSIX_Error;

   -----------------------
   -- Reset_POSIX_Error --
   -----------------------
   procedure Reset_POSIX_Error is
   begin
      SCHD.Self.POSIX_Error := False;
   end Reset_POSIX_Error;

   ----------------------------------
   -- There_Has_Been_A_POSIX_Error --
   ----------------------------------
   function There_Has_Been_A_POSIX_Error return Boolean is
   begin
      return SCHD.Self.POSIX_Error;
   end There_Has_Been_A_POSIX_Error;

   -------------------------------
   -- Get_Last_POSIX_Error_Code --
   -------------------------------
   function Get_Last_POSIX_Error_Code return Error_Code is
   begin
      return SCHD.Self.Error_Code;
   end Get_Last_POSIX_Error_Code;

end MaRTE.Kernel.Tasks_Operations.Internals;
