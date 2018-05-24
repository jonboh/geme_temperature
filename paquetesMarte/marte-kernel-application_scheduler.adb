------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--        'K e r n e l . A p p l i c a t i o n _ S c h e d u l e r'
--
--                                 Body
--
--
--  File 'k-application_scheduler.adb'                                 By MAR.
--
--
--  Application-defined scheduling. Scheduler operations.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Mutexes.Internals_Appsched;
with MaRTE.Kernel.Mutexes.SRP_Ceiling;
with MaRTE.Kernel.Application_Scheduling_Data;

with MaRTE.Configuration_Parameters;

package body MaRTE.Kernel.Application_Scheduler is

   package CP                 renames MaRTE.Configuration_Parameters;
   package TOI                renames K.Tasks_Operations.Internals;
   package SCHD               renames K.Scheduler;
   package SI                 renames K.Signals.Internals;
   package MI_APPSCHD         renames K.Mutexes.Internals_Appsched;
   package APPSCHD_EVENTS_LST renames APPSCHD_DATA.AppSched_Events_Lists;
   package TCLST              renames K.Task_Containers_Lists;
   package TCLST_ORDER        renames K.Task_Containers_Lists_Order;
   package SCHDAPP_DATA_LST   renames APPSCHD_DATA.Sched_App_Data_Lists;
   use APPSCHD_EVENTS_LST, TCLST;

   use type Int;

   --  Unchecked conversions
   function AS (Base : K.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_DATA.UC_To_AppScheduler_Data_Ac;
   pragma Inline (AS);
   function SA (Base : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;
   pragma Inline (SA);

   --------------------------
   -- Send_Event_From_Task --
   --------------------------
   procedure Send_Event_From_Task
     (Event_Code      : in APPSCHD_DATA.Event_Code_T;
      ST              : in Task_Id; --  Scheduler task
      T               : in Task_Id;
      Prio            : in Task_Priority := Task_Priority'First;
      M               : in Mutexes.Mutex_Descriptor :=
        Mutexes.Null_Mutex_Descriptor) is
      use type APPSCHD_DATA.Event_Code_T;
      E : APPSCHD_DATA.AppSched_Event_Ac;
      use APPSCHD_DATA;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  -- For size optimization when not using App. Scheduling
      end if;
      pragma Assert (Event_Code /= APPSCHED_TIMEOUT and
                     Event_Code /= APPSCHED_SIGNAL  and
                     Event_Code /= APPSCHED_READY   and
                     Event_Code /= APPSCHED_TASK_NOTIFICATION and
                     Event_Code /= APPSCHED_EXPLICIT_CALL_WITH_DATA);
      if K.Tasks_Lists."=" (ST, MaRTE.Kernel.Null_Task_Id) then
         --  When a scheduler task finish, the field 'App_Scheduler'
         --  of its scheduled tasks is made null, so no event have to
         --  be sent in that situation and no action has to be performed.
         return;
      end if;
      pragma Assert (Event_Code /= APPSCHED_NEW or else
                     Task_OK (T, NEW_APPSCHED));
      pragma Assert (Event_Code = APPSCHED_NEW or else
                     Task_OK (T, TERMINATED));
      pragma Assert (Task_OK (ST) and then ST.AppScheduler /= null);

      ------------
      --  Event compulsory actions.
      ----
      --
      --  These actions have to be performed even if the event is filtered out
      case Event_Code is

         when APPSCHED_NEW =>
            if APPSCHD_DATA.Event_In_Set
              (APPSCHED_NEW, AS (ST.AppScheduler).Events_Mask) then
               --  When the 'APPSCHED_NEW' event is masked out a 'EMASKED'
               --  error is produced for the task that is creating 'T' with
               --  SCHED_APP policy or changing the policy of 'T' to SCHED_APP
               pragma Assert
                 (TCB_Ac_To_Task_Id (T.Task_Waiting_Appscheduler_Response).
                  Internal_Error_Code = NO_ERROR);
               TCB_Ac_To_Task_Id (T.Task_Waiting_Appscheduler_Response).
                 Internal_Error_Code := APPSCHED_EVENT_MASKED;
            else
               --  Suspend task that is creating or changing the policy of
               --  another task
               SCHD.Running_Task_Gets_Blocked (WAITING_APPSCHEDULER_RESPONSE);

               --  Reserve a "TC". TCs have to be reserved here, because we
               --  want to ensure that, when the appscheduler accept a task,
               --  there are resources to create it.
               pragma Assert (APPSCHD_DATA.Any_Task_Container_Free_In_Pool);
               TCLST.Enqueue_Head (APPSCHD_DATA.Request_Task_Container,
                                   AS (ST.AppScheduler).Reserved_TCs);
            end if;

         when APPSCHED_TERMINATE =>
            declare
               TC : K.Task_Container_Ac;
            begin
               --  Remove 'T' to its scheduler task queue
               TC := TCLST.Head (AS (ST.AppScheduler).Tasks_Q);
               loop
                  pragma Assert (TC /= null);
                  exit when Tasks_Lists."=" (TC.T, T);
                  TC := TCLST.Next (TC);
               end loop;
               TCLST.Dequeue (TC, AS (ST.AppScheduler).Tasks_Q);
               APPSCHD_DATA.Release_Task_Container (TC);

            end;

         when APPSCHED_PRIORITY_INHERIT =>
            --  Update scheduler task active priority
            if Prio > ST.Active_Prio then
               TOI.Raise_Active_Prio (ST, Prio);
            end if;

         when APPSCHED_PRIORITY_UNINHERIT =>
            declare
               New_Prio : Task_Priority;
            begin
               --  Update scheduler task active priority
               if Prio = ST.Active_Prio then
                  New_Prio := TCLST_ORDER.Find_Mx_Prio
                    (AS (ST.AppScheduler).Tasks_Q).T.Active_Prio;
                  if New_Prio < ST.Active_Prio then
                     TOI.Reduce_Active_Prio (ST, New_Prio,
                                             Head_Of_New_Prio_Q => False);
                  end if;
               end if;
            end;

         when APPSCHED_EXPLICIT_CALL =>
            if APPSCHD_DATA.Event_In_Set
              (APPSCHED_EXPLICIT_CALL, AS (ST.AppScheduler).Events_Mask) then
               --  'APPSCHED_EXPLICIT_CALL' is masked out
               pragma Assert (T.Internal_Error_Code = NO_ERROR);
               T.Internal_Error_Code := APPSCHED_EVENT_MASKED;
            end if;

         when APPSCHED_INIT_MUTEX =>
            if APPSCHD_DATA.Event_In_Set
              (APPSCHED_INIT_MUTEX, AS (ST.AppScheduler).Events_Mask) then
               --  When the 'APPSCHED_INIT_MUTEX' event is masked out a
               --  'APPSCHED_EVENT_MASKED' error is produced for the task
               --  that is creating 'M' with APPSCHED_PROTOCOL
               pragma Assert
                 (MI_APPSCHD.Task_Waiting_Response (M).Internal_Error_Code =
                  NO_ERROR);
               MI_APPSCHD.Task_Waiting_Response (M).Internal_Error_Code :=
                 APPSCHED_EVENT_MASKED;
            else
               --  Suspend task that is creating mutex
               SCHD.Running_Task_Gets_Blocked (WAITING_APPSCHEDULER_RESPONSE);
            end if;

         when APPSCHED_READY | APPSCHED_BLOCK | APPSCHED_SIGNAL
           | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM
           | APPSCHED_EXPLICIT_CALL_WITH_DATA
           | APPSCHED_TIMEOUT | APPSCHED_DESTROY_MUTEX
           | APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX
           | APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX
           | APPSCHED_CHANGE_SCHED_PARAM_MUTEX
           | APPSCHED_TASK_NOTIFICATION =>
            null;
      end case;

      --  If the event filtered out no more things have to be done.
      if APPSCHD_DATA.Event_In_Set (Event_Code,
                                    AS (ST.AppScheduler).Events_Mask) then
         return;
      end if;

      --  The scheduler task is no longer waiting for signals
      if AS (ST.AppScheduler).Waiting_For_Signals then
         AS (ST.AppScheduler).Waiting_For_Signals := False;
         SI.Remove_Task_From_Waiting_For_Signals (ST);
      end if;

      --  Request an app. scheduling event to be enqueued in the scheduler
      E := APPSCHD_DATA.Request_AppSched_Event;
      pragma Assert (E /= null); --  Event resources pool exausted.

      ------------
      --  Set the relevant fields of the event
      ----
      E.Event_Code := Event_Code;
      E.T := T;

      case Event_Code is

         when APPSCHED_NEW | APPSCHED_TERMINATE
           | APPSCHED_BLOCK | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM =>
            null;

         when APPSCHED_EXPLICIT_CALL =>
            null;
         when APPSCHED_EXPLICIT_CALL_WITH_DATA =>
            pragma Assert (False); --  Nerver sent with this function
            null;

         when APPSCHED_TIMEOUT | APPSCHED_SIGNAL | APPSCHED_READY
           | APPSCHED_TASK_NOTIFICATION =>
            pragma Assert (False); --  Nerver sent with this function
            null;

         when APPSCHED_PRIORITY_INHERIT | APPSCHED_PRIORITY_UNINHERIT =>
            E.Sched_Priority := Prio;

         when APPSCHED_INIT_MUTEX | APPSCHED_DESTROY_MUTEX
           | APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX
           | APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX
           | APPSCHED_CHANGE_SCHED_PARAM_MUTEX =>
            E.M := M;
      end case;

      --  Enqueue the "AppSched" event
      APPSCHD_EVENTS_LST.Enqueue_Tail (E, AS (ST.AppScheduler).Events_Q);

      ------------
      --  Suspend task or add it to the 'Suspension_Deferred_Tasks' list
      ----
      if (Event_Code = APPSCHED_YIELD                   or
          Event_Code = APPSCHED_PRIORITY_INHERIT        or
          Event_Code = APPSCHED_PRIORITY_UNINHERIT      or
          Event_Code = APPSCHED_DESTROY_MUTEX           or
          Event_Code = APPSCHED_UNLOCK_MUTEX            or
          Event_Code = APPSCHED_CHANGE_SCHED_PARAM_MUTEX) then
         if (ST.Status = READY                  or
             ST.Status = WAITING_APPSCHED_EVENT or
             ST.Status = TIMED_WAITING_APPSCHED_EVENT) then
            --  Add to the 'Suspension_Deferred_Tasks' list
            if T.Status = READY then
               SCHDAPP_DATA_LST.Enqueue_Head
                 (SA (T.Sched_App),
                  AS (ST.AppScheduler).Suspension_Deferred_Tasks);
               pragma Assert (K.Tasks_Lists."=" (T,
                                                     SA (T.Sched_App).Owner));
            end if;
         else
            --  Suspend task
            if T.Status = READY then
               if K.Tasks_Lists."=" (SCHD.Self, T) then
                  SCHD.Running_Task_Gets_Blocked (APPSCHED_SUSPENDED);
               else
                  SCHD.Non_Running_Task_Gets_Blocked (T, APPSCHED_SUSPENDED);
               end if;
            end if;
         end if;
      end if;

      ------------
      --  Scheduler task gets ready if waiting for "AppSched" events.
      ----
      if ST.Status = WAITING_APPSCHED_EVENT then
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      elsif ST.Status = TIMED_WAITING_APPSCHED_EVENT then
         --  Remove the timeout
         SCHD.Remove_Timed_Event (ST.Suspension_Event);
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      end if;
   end Send_Event_From_Task;

   ------------------------
   -- Send_Timeout_Event --
   ------------------------
   procedure Send_Timeout_Event (ST : in Task_Id) is
      E : APPSCHD_DATA.AppSched_Event_Ac;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  --  For size optimization when not using App. Scheduling
      end if;
      pragma Assert (Task_OK (ST) and then ST.AppScheduler /= null and then
                     ST.Status = TIMED_WAITING_APPSCHED_EVENT);

      --  If the event filtered out nothing has to be done.
      if APPSCHD_DATA.Event_In_Set (APPSCHD_DATA.APPSCHED_TIMEOUT,
                                    AS (ST.AppScheduler).Events_Mask) then
         return;
      end if;

      --  The scheduler task is no longer waiting for signals
      if AS (ST.AppScheduler).Waiting_For_Signals then
         AS (ST.AppScheduler).Waiting_For_Signals := False;
         SI.Remove_Task_From_Waiting_For_Signals (ST);
      end if;

      --  Request an app. scheduling event to be enqueued in the scheduler
      E := APPSCHD_DATA.Request_AppSched_Event;
      pragma Assert (E /= null); --  Event resources pool exausted.

      --  Set the relevant fields of the event
      E.Event_Code := APPSCHD_DATA.APPSCHED_TIMEOUT;

      --  Enqueue the "AppSched" event
      APPSCHD_EVENTS_LST.Enqueue_Tail (E, AS (ST.AppScheduler).Events_Q);

      --  Scheduler task gets ready
      SCHD.Sched_Fifo_Task_Gets_Ready (ST);
   end Send_Timeout_Event;

   -----------------------
   -- Send_Signal_Event --
   -----------------------
   procedure Send_Signal_Event (ST      : in Task_Id; --  Scheduler task
                                Siginfo : in Signals.Siginfo_T) is
      E : APPSCHD_DATA.AppSched_Event_Ac;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  -- For size optimization when not using App. Scheduling
      end if;
      pragma Assert (Task_OK (ST) and then ST.AppScheduler /= null);

      --  If the event filtered out nothing has to be done.
      if APPSCHD_DATA.Event_In_Set (APPSCHD_DATA.APPSCHED_SIGNAL,
                                    AS (ST.AppScheduler).Events_Mask) then
         return;
      end if;

      --  The scheduler task is no longer waiting for signals
      AS (ST.AppScheduler).Waiting_For_Signals := False;
      --  Request an app. scheduling event to be enqueued in the scheduler
      E := APPSCHD_DATA.Request_AppSched_Event;
      pragma Assert (E /= null); --  Event resources pool exausted.

      --  Set the relevant fields of the event
      E.Event_Code := APPSCHD_DATA.APPSCHED_SIGNAL;
      E.Siginfo := Siginfo;

      --  Enqueue the "AppSched" event
      APPSCHD_EVENTS_LST.Enqueue_Tail (E, AS (ST.AppScheduler).Events_Q);

      ------------
      --  Scheduler task gets ready if waiting for "AppSched" events.
      ----
      if ST.Status = WAITING_APPSCHED_EVENT then
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      elsif ST.Status = TIMED_WAITING_APPSCHED_EVENT then
         --  Remove the timeout
         SCHD.Remove_Timed_Event (ST.Suspension_Event);
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      end if;
   end Send_Signal_Event;

   ----------------------------------
   --  Send_Notification_Or_Ready  --
   ----------------------------------
   --
   --  To send events with code "TASK_NOTIFICATION" or "READY". Called
   --  from 'SCHD.Interrupt_Handler' or 'SCHD.Task_Gets_Ready'.
   procedure Send_Notification_Or_Ready
     (Event_Code : in APPSCHD_DATA.Event_Code_T;
      ST         : in Task_Id; --  Scheduler task
      T          : in Task_Id) is
      E : APPSCHD_DATA.AppSched_Event_Ac;
      use type APPSCHD_DATA.Event_Code_T;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  -- For size optimization when not using App. Scheduling
      end if;
      pragma Assert (Task_OK (ST) and then ST.AppScheduler /= null);
      pragma Assert (Task_OK (T, TERMINATED));
      pragma Assert (Event_Code = APPSCHD_DATA.APPSCHED_READY   or
                     Event_Code = APPSCHD_DATA.APPSCHED_TASK_NOTIFICATION);

      --  If the event filtered out nothing has to be done.
      if APPSCHD_DATA.Event_In_Set (Event_Code,
                                    AS (ST.AppScheduler).Events_Mask) then
         return;
      end if;

      --  Request an app. scheduling event to be enqueued in the
      --  scheduler
      E := APPSCHD_DATA.Request_AppSched_Event;
      pragma Assert (E /= null); --  Event resources pool exausted.

      --  Set the relevant fields of the event
      E.Event_Code := Event_Code;
      E.T := T;

      --  Preemption level of 'T' is  above system ceiling ?
      if Mutexes.SRP_Ceiling.Task_Above_System_Ceiling (T) then
         --  It is above system ceiling so event can be sent to
         --  appscheduler.
         Send_Notification_Or_Ready_Event (ST, E);
      else
         --  Preemption level of 'T' is below system ceiling, so it is
         --  queued until system ceiling go low enough.
         Mutexes.SRP_Ceiling.Add_Task_Notification_Event_To_Queue (E);
      end if;

   end Send_Notification_Or_Ready;

   ----------------------------------------
   --  Send_Notification_Or_Ready_Event  --
   ----------------------------------------
   --
   --  To send events with code "TASK_NOTIFICATION" or "READY" that
   --  were queued in 'Mutexes.SRP_Ceiling.Pending_Task_Events_Q'
   --  waiting until ceiling go low enough. Called from
   --  'Send_Notification_Or_Ready' and
   --  'Mutexes.SRP_Ceiling.Del_Mutex_Lock' when ceiling has
   --  decreased.
   procedure Send_Notification_Or_Ready_Event
     (ST : in Task_Id; --  Scheduler task
      E  : in APPSCHD_DATA.AppSched_Event_Ac) is
      use type APPSCHD_DATA.Event_Code_T;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  -- For size optimization when not using App. Scheduling
      end if;
      pragma Assert (Task_OK (ST) and then ST.AppScheduler /= null);
      pragma Assert
        (E /= null and then
           (E.Event_Code = APPSCHD_DATA.APPSCHED_READY or
              E.Event_Code = APPSCHD_DATA.APPSCHED_TASK_NOTIFICATION));
      pragma Assert (Mutexes.SRP_Ceiling.Task_Above_System_Ceiling (E.T));

      --  The scheduler task is no longer waiting for signals
      if AS (ST.AppScheduler).Waiting_For_Signals then
         AS (ST.AppScheduler).Waiting_For_Signals := False;
         SI.Remove_Task_From_Waiting_For_Signals (ST);
      end if;

      --  Enqueue the "AppSched" event
      APPSCHD_EVENTS_LST.Enqueue_Tail (E, AS (ST.AppScheduler).Events_Q);

      --  Scheduler task gets ready if waiting for "AppSched" events.
      if ST.Status = WAITING_APPSCHED_EVENT then
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      elsif ST.Status = TIMED_WAITING_APPSCHED_EVENT then
         --  Remove the timeout
         SCHD.Remove_Timed_Event (ST.Suspension_Event);
         SCHD.Sched_Fifo_Task_Gets_Ready (ST);

      end if;
   end Send_Notification_Or_Ready_Event;

   ------------------------------------------------------------
   --  Execute_Scheduling_Operation_In_Running_Task_Context  --
   ------------------------------------------------------------
   procedure Execute_Scheduler_Operation_In_Running_Task_Context
     (Event_Code      : in APPSCHD_DATA.Event_Code_T;
      ST              : in Task_Id; --  Scheduler task
      T               : in Task_Id;
      Flags           : in out Integer;
      Actions         : in out APPSCHD_DATA.Sched_Actions_Set;
      Info            : in System.Address := System.Null_Address;
      Info_Size       : in Int := 0;
      User_Event_Code : in Int := 0) is

      Old_Urgency          : K.Task_Urgency;
      Old_Preemption_Level : K.Task_Preemption_Level;
      use type APPSCHD_DATA.Event_Code_T;
      use APPSCHD_DATA;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;  -- For size optimization when not using App. Scheduling
      end if;
      pragma Assert (K.Tasks_Lists."=" (T, SCHD.Self));
      pragma Assert
        (Event_Code = APPSCHD_DATA.APPSCHED_EXPLICIT_CALL or
           Event_Code = APPSCHD_DATA.APPSCHED_EXPLICIT_CALL_WITH_DATA);
      if K.Tasks_Lists."=" (ST, MaRTE.Kernel.Null_Task_Id) then
         --  When a scheduler task finish, the field 'App_Scheduler'
         --  of its scheduled tasks is made null, so no event have to
         --  be sent in that situation and no action has to be performed.
         return;
      end if;
      pragma Assert (Task_OK (T) and then
                     Task_OK (ST) and then ST.AppScheduler /= null);

      ------------
      --  Event compulsory actions.
      ----
      --
      --  These actions have to be performed even if the event is filtered out
      case Event_Code is

         when APPSCHED_EXPLICIT_CALL | APPSCHED_EXPLICIT_CALL_WITH_DATA =>
            null;
--              if (To_Option_Set (Event_Code) <=
--                  AS (ST.AppScheduler).Events_Mask) then
--               --  When the event is masked out a 'EMASKED' error is produced
--                 pragma Assert (T.Internal_Error_Code = NO_ERROR);
--                 T.Internal_Error_Code := APPSCHED_EVENT_MASKED;
--              end if;

         when APPSCHED_NEW | APPSCHED_TERMINATE
           | APPSCHED_READY | APPSCHED_BLOCK | APPSCHED_SIGNAL
           | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM
           | APPSCHED_PRIORITY_INHERIT | APPSCHED_PRIORITY_UNINHERIT
           | APPSCHED_TIMEOUT
           | APPSCHED_INIT_MUTEX | APPSCHED_DESTROY_MUTEX
           | APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX
           | APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX
           | APPSCHED_CHANGE_SCHED_PARAM_MUTEX
           | APPSCHED_TASK_NOTIFICATION =>
            null;
      end case;

      --  If the event filtered out no more things have to be done.
--  if To_Option_Set (Event_Code) <= AS (ST.AppScheduler).Events_Mask then
--         return;
--      end if;
      --  Raise Urgency and priority of task to avoid it can be
      --  preempted by its scheduler or for other APP_SCHED task.

      Old_Urgency          := SCHD.Self.Active_Urgency;
      Old_Preemption_Level := SCHD.Self.Active_Preemption_Level;
      SCHD.Self.Active_Urgency          := K.Task_Urgency'Last;
      SCHD.Self.Active_Preemption_Level := K.Task_Preemption_Level'Last;

      --  Reset actions object
      Actions.Initialized := APPSCHD_DATA.INITIALIZED;
      Actions.Last := 0;
      --  Executes scheduler operation out of the kernel.
      K.Leave_Critic_Section (Flags);

      --  Execute scheduler operation associated to the event
      case Event_Code is

         when APPSCHED_EXPLICIT_CALL | APPSCHED_EXPLICIT_CALL_WITH_DATA =>
            SA (T.Sched_App).Suspend_After_Scheduler_Operation := True;
            Send_Event_From_Task (APPSCHED_EXPLICIT_CALL, ST => ST, T => T);
         when APPSCHED_NEW | APPSCHED_TERMINATE
           | APPSCHED_READY | APPSCHED_BLOCK | APPSCHED_SIGNAL
           | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM
           | APPSCHED_PRIORITY_INHERIT | APPSCHED_PRIORITY_UNINHERIT
           | APPSCHED_TIMEOUT
           | APPSCHED_INIT_MUTEX | APPSCHED_DESTROY_MUTEX
           | APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX
           | APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX
           | APPSCHED_CHANGE_SCHED_PARAM_MUTEX
           | APPSCHED_TASK_NOTIFICATION =>
            null;
      end case;

      K.Enter_Critic_Section (Flags);

      --  Restore old Urgency and Preemption_Level values
      SCHD.Self.Active_Urgency          := Old_Urgency;
      SCHD.Self.Active_Preemption_Level := Old_Preemption_Level;

      if SA (T.Sched_App).Suspend_After_Scheduler_Operation then
         --  There is not a activate action for T, then it has to be
         --  blocked.
         SCHD.Running_Task_Gets_Blocked (APPSCHED_SUSPENDED);
      end if;

   end Execute_Scheduler_Operation_In_Running_Task_Context;

end MaRTE.Kernel.Application_Scheduler;
