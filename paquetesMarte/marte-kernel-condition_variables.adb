------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . C o n d i t i o n _ V a r i a b l e s'
--
--                                  Body
--
--
--  File 'k-condition_variables.adb'                                   By MAR.
--
--  Condition Variables management.
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
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Kernel.Mutexes.Internals; use MaRTE.Kernel.Mutexes.Internals;
use MaRTE.Kernel.Mutexes;
use MaRTE.Kernel;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Condition_Variables.Internals;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Timers;

--  Debug
with MaRTE.Kernel.Condition_Variables.Debug;
package body MaRTE.Kernel.Condition_Variables is

   package SCHD renames K.Scheduler;
   package TSTE renames K.Task_Suspension_Timed_Events;
   package ML   renames K.Tasks_Map_Lists;
   package M_INTERNALS renames K.Mutexes.Internals;

   use K.Tasks_Lists;

   use type Int;

   type Attributes_Ac is access all Attributes;

   -------------
   -- Attr_OK --
   -------------
   function Attr_OK (Attr : access Attributes) return Boolean;
   function Attr_OK (Attr : access Attributes) return Boolean is
   begin
      return Attributes_Ac (Attr) /= null
        and then Attr.Magic = ATTR_INITIALIZED;
   end Attr_OK;
   pragma Inline (Attr_OK);

   ---------------------------
   -- Pthread_Condattr_Init --
   ---------------------------
   function Pthread_Condattr_Init (Attr : access Attributes)
                                  return Int is
   begin
      if Attributes_Ac (Attr) = null then
         return INVALID_ARGUMENT;
      end if;

      Attr.Magic := ATTR_INITIALIZED;
      return 0;
   end Pthread_Condattr_Init;

   ------------------------------
   -- Pthread_Condattr_Destroy --
   ------------------------------
   function Pthread_Condattr_Destroy (Attr : access Attributes)
                                      return Int is
   begin
      --  Error handling
      if not Attr_OK (Attr) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Magic := ATTR_NOT_INITIALIZED;

      return 0;
   end Pthread_Condattr_Destroy;

   -----------------------
   -- Pthread_Cond_Init --
   -----------------------
   CVs_Counter : Integer := 0;
   Default_Cond : aliased Condition;
   procedure Memory_Copy (From  : access Condition;
                          To    : access Condition;
                          Bytes : in Integer);
   pragma Import (C, Memory_Copy, "memory_copy");

   function Pthread_Cond_Init (Cond : access Condition;
                               Attr : access Attributes)
                               return Int is
   begin
      --  Error handling
      if ((Attributes_Ac (Attr) /= null and then not Attr_OK (Attr)) or else
          Condition_Descriptor (Cond) = null) then
         return INVALID_ARGUMENT;
      end if;
      if Cond.Magic = INITIALIZED then
         return RESOURCE_BUSY;
      end if;
      --  Checking if the CV is already initialized can't be
      --  performed here. This is because for a just created C condition
      --  variable can't be assured if it's initialized or not due to
      --  its initial random value. This check is only performed for
      --  the POSIX.Ada interface in 'POSIX_Condition_Variables'.

      --  Set the 'tag' properly in a condition created from a C program
      Memory_Copy (From => Default_Cond'Access, To => Cond,
                   Bytes => Condition'Size / 8);

      Cond.Magic := INITIALIZED;
      Cond.Id := CVs_Counter;   CVs_Counter := CVs_Counter + 1;
      ML.Initialize (Cond.Blocked_Tasks);
      Cond.M := Mutexes.Null_Mutex_Descriptor;
      Cond.Is_Associated_With_Mutex_Now := False;
      Cond.Descriptor := Condition_Descriptor (Cond);

      pragma Debug (Debug.Initialize (Cond.all)); --  Trace

      return 0;
   end Pthread_Cond_Init;

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (Cond : Condition) return Condition_Descriptor is
   begin
      --  Error handling
      if not Internals.Cond_OK (Cond) then
         return null;
      end if;

      return Cond.Descriptor;
   end Descriptor_Of;
   pragma Inline (Descriptor_Of);

   --------------------------
   -- Pthread_Cond_Destroy --
   --------------------------
   function Pthread_Cond_Destroy (Cond : access Condition)
                                  return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not Internals.Cond_OK (Condition_Descriptor (Cond)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if not ML.Is_Empty (Cond.Blocked_Tasks) then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_BUSY;
      end if;

      Cond.Magic := FINALIZED;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Cond_Destroy;

   -------------------------
   -- Pthread_Cond_Signal --
   -------------------------
   function Pthread_Cond_Signal (Cond : access Condition)
                                return Int is
      Flags : Integer;
      T_Mx_Prio : Task_Id;
   begin
      --  Error handling
      if not Internals.Cond_OK (Condition_Descriptor (Cond)) then
         return INVALID_ARGUMENT;
      end if;

      pragma Assert (not Cond.Is_Associated_With_Mutex_Now or else
                     M_INTERNALS.Mutex_OK (Cond.M),
                     "Signal: Null or Invalid Mutex associated with CV");

      K.Enter_Critic_Section (Flags);

      pragma Debug (Debug.Signal1 (Condition_Descriptor (Cond))); --  Trace

      --  Take the most prioritary task blocked in 'Cond'
      T_Mx_Prio := ML.Head (Cond.Blocked_Tasks);

      if T_Mx_Prio /= null then
         ML.Dequeue_Head (T_Mx_Prio, Cond.Blocked_Tasks);
         T_Mx_Prio.CV_Where_Blocked := null;

         if ML.Is_Empty (Cond.Blocked_Tasks) then
            Decrement_Num_Of_Associated_CVs (Cond.M);
            Cond.Is_Associated_With_Mutex_Now := False;
         end if;

         if T_Mx_Prio.Status = TIMED_WAITING_CV then

            pragma Debug (Debug.Signal2 (T_Mx_Prio)); --  Trace

            --  The task had used 'Timed_Wait' to wait on the CV and the
            --  timeout hasn't been reached yet.
            SCHD.Remove_Timed_Event (T_Mx_Prio.Suspension_Event);
         end if;

         if Mutexes.Internals.Get_Owner (Cond.M) = null then
            --  Mutex free: put task ready. It will try to take mutex
            --  in the final part of 'Pthread_Cond_Wait' or
            --  'Pthread_Cond_Timedwait'.
            SCHD.Task_Gets_Ready (T_Mx_Prio);
            SCHD.Do_Scheduling;
         else
            --  Mutex Locked: task is added to mutex queue.
            T_Mx_Prio.Status := BLOCKED;
            Mutexes.Internals.Enqueue_Blocked_Task_In_Mutex (T_Mx_Prio,
                                                             Cond.M);
         end if;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Cond_Signal;

   ----------------------------
   -- Pthread_Cond_Broadcast --
   ----------------------------
   function Pthread_Cond_Broadcast (Cond : in Condition_Descriptor)
                                   return Int is
      Flags : Integer;
      T, T_Mx_Prio : Task_Id;
   begin
      pragma Assert (not Cond.Is_Associated_With_Mutex_Now or else
                       M_INTERNALS.Mutex_OK (Cond.M),
                     "Broadcast: Null or Invalid Mutex associated with CV");

      --  Error handling
      if not Internals.Cond_OK (Cond) then
         return INVALID_ARGUMENT;
      end if;

      K.Enter_Critic_Section (Flags);

      pragma Debug (Debug.Broadcast1 (Cond)); --  Trace

      if Cond.Is_Associated_With_Mutex_Now then
         pragma Assert (not ML.Is_Empty (Cond.Blocked_Tasks));
         Decrement_Num_Of_Associated_CVs (Cond.M);
         Cond.Is_Associated_With_Mutex_Now := False;

         --  For every task blocked in CV put it in the mutex
         --  queue. 'T_Mx_Prio' points to the most prioritary.
         T_Mx_Prio := ML.Head (Cond.Blocked_Tasks);
         T := T_Mx_Prio;
         loop
            ML.Dequeue_Head (T, Cond.Blocked_Tasks);
            T.CV_Where_Blocked := null;
            if T.Status = TIMED_WAITING_CV then
               pragma Debug (Debug.Broadcast1 (Cond)); -- Trace

               --  The task had used 'Timed_Wait' to wait on the CV
               --  and the timeout haven't been reached yet.
               SCHD.Remove_Timed_Event (T.Suspension_Event);
            end if;
            T.Status := BLOCKED;

            if (T = T_Mx_Prio and then
                Mutexes.Internals.Get_Owner (Cond.M) = null) then
               --  The mutex is free, then put most prioritary task
               --  ready. It will try to take mutex in the final part
               --  of 'Pthread_Cond_Wait' or 'Pthread_Cond_Timedwait'.
               SCHD.Task_Gets_Ready (T_Mx_Prio);
            else
               Mutexes.Internals.Enqueue_Blocked_Task_In_Mutex (T, Cond.M);
            end if;
            T := ML.Head (Cond.Blocked_Tasks);
            exit when T = null;
         end loop;

         if Mutexes.Internals.Get_Owner (Cond.M) = null  then
            --  The mutex is free and then the most prioritary task
            --  has been put into the ready status. We give it the
            --  chance to execute and try to take mutex in the final
            --  part of 'Pthread_Cond_Wait' or
            --  'Pthread_Cond_Timedwait'.
            SCHD.Do_Scheduling;
         end if;
      end if; --  Cond.Is_Associated_With_Mutex_Now

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Cond_Broadcast;

   ------------------------------
   -- Actions_Before_Cond_Wait --
   ------------------------------
   function Actions_Before_Cond_Wait
     (Cond  : in Condition_Descriptor;
      M     : in K.Mutexes.Mutex_Descriptor) return MaRTE.Kernel.Error_Code;
   function Actions_Before_Cond_Wait
     (Cond  : in Condition_Descriptor;
      M     : in K.Mutexes.Mutex_Descriptor) return MaRTE.Kernel.Error_Code is
   begin
      --  Error handling
      if (not Internals.Cond_OK (Cond)                               or else
          not M_INTERNALS.Mutex_OK (M)                               or else
          (Cond.Is_Associated_With_Mutex_Now and then Cond.M /= M))
      then
         return INVALID_ARGUMENT;
      end if;
      if M_INTERNALS.Get_Owner (M) /= SCHD.Self then
         return OPERATION_NOT_PERMITTED;
      end if;

      --  Increment number of mutex associated CVs
      if not Cond.Is_Associated_With_Mutex_Now then
         Cond.M := M;
         Increment_Num_Of_Associated_CVs (Cond.M);
         Cond.Is_Associated_With_Mutex_Now := True;
      end if;

      return 0;
   end Actions_Before_Cond_Wait;
   pragma Inline (Actions_Before_Cond_Wait);

   -----------------------
   -- Pthread_Cond_Wait --
   -----------------------
   function Pthread_Cond_Wait (Cond : in Condition_Descriptor;
                               M    : in K.Mutexes.Mutex_Descriptor)
                               return Int is
      Flags : Integer;
      Error : K.Error_Code;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error check and unlock mutex
      Error := Actions_Before_Cond_Wait (Cond, M);
      Mutexes.Internals.Task_Unlocks_Mutex (SCHD.Self, M);
      if Error /= NO_ERROR then
         K.Leave_Critic_Section (Flags);
         return Error;
      end if;

      SCHD.Running_Task_Gets_Blocked (With_Status => BLOCKED);
      ML.Enqueue_In_Order (SCHD.Self, Cond.Blocked_Tasks);
      SCHD.Self.CV_Where_Blocked := K.Condition_Base_Ac (Cond);

      pragma Debug (Debug.Wait (Cond)); -- Trace

      SCHD.Do_Scheduling; -- leave CPU

      --  Here tasks continues after having been signaled
      if Mutexes.Internals.Get_Owner (M) = SCHD.Self then
         --  Mutex was locked while signaling CV. Then task was added
         --  to mutex blocked tasks list and eventually mutex has been
         --  granted to it.
         K.Leave_Critic_Section (Flags);
         return 0;
      else
         --  Mutex was free while signaling CV. Then task was put into
         --  the ready state and now must get mutex.
         K.Leave_Critic_Section (Flags);
         return Pthread_Mutex_Lock (M);
      end if;
   end Pthread_Cond_Wait;

   -----------------------
   -- Timed_Wait_HWTime --
   -----------------------
   function Timed_Wait_HWTime (Cond    : Condition_Descriptor;
                               M       : K.Mutexes.Mutex_Descriptor;
                               Timeout : HAL.HWTime) return Int is
      Flags : Integer;
      Now   : HAL.HWTime;
      Error : K.Error_Code;
      use type HAL.HWTime;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error check
      Error := Actions_Before_Cond_Wait (Cond, M);
      if Error /= NO_ERROR then
         Mutexes.Internals.Task_Unlocks_Mutex (SCHD.Self, M);
         SCHD.Do_Scheduling;  --  allow task to lock the mutex (if any)
         K.Leave_Critic_Section (Flags);
         return Error;
      end if;

      Now := HAL.Get_HWTime;
      if not (Timeout >
                Now + Timers.Realtime_Clock_Offset + Suspension_Time_Minimum)
      then
         --  'Timeout' is too small to be waited for: return error
         --  code without unlocking the mutex
         if ML.Is_Empty (Cond.Blocked_Tasks) then
            --  Undo what done in 'Actions_Before_Cond_Wait'
            Decrement_Num_Of_Associated_CVs (Cond.M);
            Cond.Is_Associated_With_Mutex_Now := False;
         end if;
         K.Leave_Critic_Section (Flags);
         return TIMED_OUT;

      else --  'Timeout' long enough
         --  Unlocks the mutex
         Mutexes.Internals.Task_Unlocks_Mutex (SCHD.Self, M);

         --  Configure Task Timed Event
         SCHD.Self.Suspension_Event.T :=
           Timeout - Timers.Realtime_Clock_Offset;
         TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
           TSTE.CV_TIMEDWAIT;
         TSTE.Task_Suspension_TE_Ac
           (SCHD.Self.Suspension_Event).CV_Where_Blocked :=
           Condition_Base_Ac (Cond);

         --  Suspend task
         SCHD.Running_Task_Gets_Suspended (With_Status => TIMED_WAITING_CV,
                                           At_Time     => Now);
         ML.Enqueue_In_Order (SCHD.Self, Cond.Blocked_Tasks);
         SCHD.Self.CV_Where_Blocked := K.Condition_Base_Ac (Cond);

         pragma Debug (Debug.Timed_Wait (Cond)); -- Trace

         SCHD.Do_Scheduling; -- leave CPU

         --  After a 'Signal' a 'Broadcast' or a timeout the calling task
         --  continues here...
         pragma Assert (SCHD.Self.Internal_Error_Code = NO_ERROR or
                          SCHD.Self.Internal_Error_Code = TIMED_OUT);
         if SCHD.Self.Internal_Error_Code = TIMED_OUT then
            --  Timeout has been reached (mutex has been granted to
            --  task in 'Internals.Timeout_Reached')
            SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
            SCHD.Self.CV_Where_Blocked := null; -- end of blocking in CV
            K.Leave_Critic_Section (Flags);
            return TIMED_OUT;

         else  --  after a 'Signal' a 'Broadcast'

            --  Take mutex if necessary
            if Mutexes.Internals.Get_Owner (M) = SCHD.Self then
               --  Mutex was locked while signaling CV. Then task was
               --  added to mutex blocked tasks list and eventually
               --  mutex has been granted to it.
               K.Leave_Critic_Section (Flags);
               return 0;
            else
               --  Mutex was free while signaling CV. Then task was
               --  put into the ready state and now must get mutex.
               K.Leave_Critic_Section (Flags);
               return Pthread_Mutex_Lock (M);
            end if;
         end if;
      end if;
   end Timed_Wait_HWTime;

end MaRTE.Kernel.Condition_Variables;
