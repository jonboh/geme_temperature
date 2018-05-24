------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . T a s k s _ O p e r a t i o n s .
--                A p p l i c a t i o n _ S c h e d u l e r'
--
--                                 Body
--
--
--  File 'k-to-appsched.adb'                                           By MAR.
--
--
--  Application-defined scheduling specific operations.
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
with Ada.Unchecked_Conversion;

with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduler_Task_Body;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Mutexes.Internals_Appsched;
with MaRTE.Kernel.Signals.Application_Scheduler;
with MaRTE.Kernel.Signals.Internals;

package body MaRTE.Kernel.Tasks_Operations.Application_Scheduler is

   package CP      renames MaRTE.Configuration_Parameters;
   package TSTE    renames K.Task_Suspension_Timed_Events;
   package SCHD    renames K.Scheduler;
   package TO      renames K.Tasks_Operations;
   package TOI     renames K.Tasks_Operations.Internals;
   package APPSCHD renames K.Application_Scheduler;
   package MI      renames K.Mutexes.Internals;
   package APPSCHD_EVENTS_LST renames
     K.Application_Scheduling_Data.AppSched_Events_Lists;
   package SCHDAPP_DATA_LST renames
     K.Application_Scheduling_Data.Sched_App_Data_Lists;
   package S_APPSCHD renames K.Signals.Application_Scheduler;
   package SI        renames K.Signals.Internals;
   package MIAPP     renames K.Mutexes.Internals_Appsched;
   package MLST      renames K.Mutexes_Lists; use MLST;
   package TCLST
     renames K.Task_Containers_Lists;
   package TE_T  renames K.Timed_Events_And_Timer;

   use K.Tasks_Lists;

   use type Int, APPSCHD_DATA.Event_Set;

   function UC (Base : K.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_DATA.UC_To_AppScheduler_Data_Ac;
   function UC (Base : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;

   function To_Int is new Ada.Unchecked_Conversion (APPSCHD_DATA.Magic_Check,
                                                    Integer);

   ----------------------------------------------------------------------------
   -- Scheduling Events Set Manipulation --------------------------------------
   ----------------------------------------------------------------------------

   -----------------------------
   -- POSIX_Appsched_Emptyset --
   -----------------------------
   function POSIX_Appsched_Emptyset (Set : access APPSCHD_DATA.Event_Set)
                                     return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Set.all := 0;
      return 0;
   end POSIX_Appsched_Emptyset;

   ----------------------------
   -- POSIX_Appsched_Fillset --
   ----------------------------
   function POSIX_Appsched_Fillset (Set : access APPSCHD_DATA.Event_Set)
                                    return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Set.all := APPSCHD_DATA.Event_Set'Last;
      return 0;
   end POSIX_Appsched_Fillset;

   ---------------------------
   -- POSIX_Appsched_Addset --
   ---------------------------
   function POSIX_Appsched_Addset
     (Set        : access APPSCHD_DATA.Event_Set;
      Event_Code : in     APPSCHD_DATA.Event_Code_T) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Set.all := Set.all or APPSCHD_DATA.To_Event_Set (Event_Code);
      return 0;
   end POSIX_Appsched_Addset;

   ---------------------------
   -- POSIX_Appsched_Delset --
   ---------------------------
   function POSIX_Appsched_Delset
     (Set        : access APPSCHD_DATA.Event_Set;
      Event_Code : in     APPSCHD_DATA.Event_Code_T) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Set.all := Set.all and not APPSCHD_DATA.To_Event_Set (Event_Code);
      return 0;
   end POSIX_Appsched_Delset;

   -----------------------------
   -- POSIX_Appsched_Ismember --
   -----------------------------
   function POSIX_Appsched_Ismember
     (Set        : in APPSCHD_DATA.Event_Set; --  By reference
      Event_Code : in APPSCHD_DATA.Event_Code_T)
      return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if APPSCHD_DATA.Event_In_Set (Event_Code, Set) then
         return 1;
      else
         return 0;
      end if;
   end POSIX_Appsched_Ismember;

   ----------------------------------------------------------------------------
   --  Set and Get the "Wait Signal Set" attribute  ---------------------------
   --  of the scheduler task.                       ---------------------------
   ----------------------------------------------------------------------------

   -------------------------------------
   -- POSIX_Appschedattr_Seteventmask --
   -------------------------------------
   function POSIX_Appschedattr_Setwaitsignalset
     (Set : in K.Signals.Signal_Set) -- By ref. (pragma Export_Function)
      return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      UC (SCHD.Self.AppScheduler).Wait_Signal_Set := Set;
      return 0;
   end POSIX_Appschedattr_Setwaitsignalset;

   ----------------------------------------------------------------------------
   -- Set and Get the "Events Mask" attribute ---------------------------------
   -- of the scheduler task.                  ---------------------------------
   ----------------------------------------------------------------------------

   -------------------------------------
   -- POSIX_Appschedattr_Seteventmask --
   -------------------------------------
   function POSIX_Appschedattr_Seteventmask
     (Set : in APPSCHD_DATA.Event_Set) -- By reference (pragma Export_Function)
      return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      UC (SCHD.Self.AppScheduler).Events_Mask := Set;
      return 0;
   end POSIX_Appschedattr_Seteventmask;

   -------------------------------------
   -- POSIX_Appschedattr_Geteventmask --
   -------------------------------------
   function POSIX_Appschedattr_Geteventmask
     (Set : access APPSCHD_DATA.Event_Set) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      Set.all := UC (SCHD.Self.AppScheduler).Events_Mask;
      return 0;
   end POSIX_Appschedattr_Geteventmask;

   ----------------------------------------------------------------------------
   -- Set and Get the "Flags" attribute ---------------------------------------
   -- of the scheduler task.            ---------------------------------------
   ----------------------------------------------------------------------------

   ---------------------------------
   -- POSIX_Appschedattr_SetFlags --
   ---------------------------------
   function POSIX_Appschedattr_SetFlags
     (Flags : in APPSCHD_DATA.AppScheduler_Data_Flags_T)
      return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      UC (SCHD.Self.AppScheduler).Flags := Flags;
      return 0;
   end POSIX_Appschedattr_SetFlags;

   ---------------------------------
   -- POSIX_Appschedattr_GetFlags --
   ---------------------------------
   function POSIX_Appschedattr_GetFlags
     (Flags : access APPSCHD_DATA.AppScheduler_Data_Flags_T)
      return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      Flags.all := UC (SCHD.Self.AppScheduler).Flags;
      return 0;
   end POSIX_Appschedattr_GetFlags;

   ----------------------------------------------------------------------------
   -- Set and Get the "Clock" attribute ---------------------------------------
   -- of the scheduler task.            ---------------------------------------
   ----------------------------------------------------------------------------

   ---------------------------------
   -- POSIX_Appschedattr_Setclock --
   ---------------------------------
   function POSIX_Appschedattr_Setclock (Clock : in Timers.Clock_Id)
                                         return Int is
      use type Timers.Clock_Id;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      if (Clock /= Timers.CLOCK_REALTIME and
          Clock /= Timers.CLOCK_MONOTONIC) then
         return INVALID_ARGUMENT;
      end if;
      UC (SCHD.Self.AppScheduler).Clock := Clock;
      return 0;
   end POSIX_Appschedattr_Setclock;

   ---------------------------------
   -- POSIX_Appschedattr_Getclock --
   ---------------------------------
   function POSIX_Appschedattr_Getclock (Clock : access Timers.Clock_Id)
                                         return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      Clock.all := UC (SCHD.Self.AppScheduler).Clock;
      return 0;
   end POSIX_Appschedattr_Getclock;

   ----------------------------------------------------------------------------
   -- Set and Get the "Replyinfo" attribute  ----------------------------------
   -- of the scheduler task.                 ----------------------------------
   ----------------------------------------------------------------------------

   ---------------------------------------
   --  POSIX_Appschedattr_Setreplyinfo  --
   ---------------------------------------
   function POSIX_Appschedattr_Setreplyinfo
     (Reply      : in APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : in APPSCHD_DATA.Scheduler_Reply_Size_T)
      return Int is
      use type APPSCHD_DATA.Scheduler_Reply_Size_T,
        APPSCHD_DATA.Scheduler_Reply_Ac;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      if (Reply_Size < APPSCHD_DATA.Scheduler_Reply_Size_T'First or else
          APPSCHD_DATA.Scheduler_Reply_Size_T'Last < Reply_Size or else
          (Reply_Size /= 0 and Reply = null))  then
         return INVALID_ARGUMENT;
      end if;

      --  Copy scheduler reply
      if Reply_Size /= 0 then
         UC (SCHD.Self.AppScheduler).Reply (1 .. Reply_Size) :=
           Reply (1 .. Reply_Size);
      end if;
      UC (SCHD.Self.AppScheduler).Reply_Size := Reply_Size;

      return 0;
   end POSIX_Appschedattr_Setreplyinfo;

   ---------------------------------------
   --  POSIX_Appschedattr_Getreplyinfo  --
   ---------------------------------------
   function POSIX_Appschedattr_Getreplyinfo
     (Reply      : in     APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : access APPSCHD_DATA.Scheduler_Reply_Size_T)
     return Int is
      use type APPSCHD_DATA.Scheduler_Reply_Ac,
        APPSCHD_DATA.Scheduler_Reply_Size_T;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if SCHD.Self.AppScheduler = null then
         return APPSCHED_POLICY_ERROR;
      end if;
      if Reply = null then
         return INVALID_ARGUMENT;
      end if;

      --  Copy scheduler reply
      if UC (SCHD.Self.AppScheduler).Reply_Size /= 0 then
         Reply (1 .. UC (SCHD.Self.AppScheduler).Reply_Size) :=
           UC (SCHD.Self.AppScheduler).Reply
           (1 .. UC (SCHD.Self.AppScheduler).Reply_Size);
      end if;
      Reply_Size.all := UC (SCHD.Self.AppScheduler).Reply_Size;

      return 0;
   end POSIX_Appschedattr_Getreplyinfo;

   ----------------------------------------------------------------------------
   -- Release_Event -----------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Called from 'POSIX_Application_Scheduling.Copy_Event' to
   --  release the event object.
   procedure Release_Event (Event_Ac : in APPSCHD_DATA.AppSched_Event_Ac) is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      APPSCHD_DATA.Release_AppSched_Event (Event_Ac);

      K.Leave_Critic_Section (Flags);
   end Release_Event;

   ----------------------------------------------------------------------------
   --  Get Events  ------------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Procedures used to get the following scheduling event from a
   --  scheduler event queue. If the queue is empty the scheduler task
   --  gets blocked.

   ---------------------------------
   -- Get_Event (without timeout) --
   ---------------------------------
   procedure Get_Event (Event_Ac : in out APPSCHD_DATA.AppSched_Event_Ac;
                        Set      : in     Signals.Signal_Set_Ac_Const) is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;
      end if;
      --  Wait for signals ?
      if Signals."/=" (Set, null) then
         S_APPSCHD.Convert_Pending_Signals_Into_Sched_Events (Set);
      end if;

      if APPSCHD_EVENTS_LST.Is_Empty (UC
                                      (SCHD.Self.AppScheduler).Events_Q) then
         --  Wait for signals ?
         if Signals."/=" (Set, null) then
            UC (SCHD.Self.AppScheduler).Waiting_For_Signals := True;
            SI.Register_Self_As_Waiting_For_Signals (Set);
         end if;

         --  Scheduler task wait for event
         SCHD.Running_Task_Gets_Blocked
           (With_Status => WAITING_APPSCHED_EVENT);

         SCHD.Do_Scheduling; -- leave CPU
         --  After a new event arrival the scheduler task continues here...
      end if;

      --  Dequeue the event
      Event_Ac :=
        APPSCHD_EVENTS_LST.Head (UC (SCHD.Self.AppScheduler).Events_Q);
      APPSCHD_EVENTS_LST.Dequeue_Head (UC (SCHD.Self.AppScheduler).Events_Q);
   end Get_Event;

   ------------------------------
   -- Get_Event (with timeout) --
   ------------------------------
   procedure Get_Event (Event_Ac : in out APPSCHD_DATA.AppSched_Event_Ac;
                        Set      : in     Signals.Signal_Set_Ac_Const;
                        Timeout  : in     HAL.HWTime) is
      use type Timers.Clock_Id, APPSCHD_DATA.AppScheduler_Data_Flags_T,
          HAL.HWTime;
      Now : HAL.HWTime;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return;
      end if;
      --  Wait for signals ?
      if Signals."/=" (Set, null) then
         S_APPSCHD.Convert_Pending_Signals_Into_Sched_Events (Set);
      end if;

      if APPSCHD_EVENTS_LST.Is_Empty (UC
                                      (SCHD.Self.AppScheduler).Events_Q) then
         --  Wait for signals ?
         if Signals."/=" (Set, null) then
            UC (SCHD.Self.AppScheduler).Waiting_For_Signals := True;
            SI.Register_Self_As_Waiting_For_Signals (Set);
         end if;

         --  Scheduler task wait for an event or a timeout
         SCHD.Self.Suspension_Event.T := Timeout;
         Now := HAL.Get_HWTime;
         if (APPSCHD_DATA.APPSCHED_ABSTIMEOUT and
               UC (SCHD.Self.AppScheduler).Flags) /= 0 then
            --  Absolute timeout
            if UC (SCHD.Self.AppScheduler).Clock = Timers.CLOCK_REALTIME then
               if Timeout < Timers.Realtime_Clock_Offset then
                  SCHD.Self.Suspension_Event.T := 0;
               else
                  SCHD.Self.Suspension_Event.T := SCHD.Self.Suspension_Event.T
                    - Timers.Realtime_Clock_Offset;
               end if;
            end if;
         else
            --  Relative timeout
            SCHD.Self.Suspension_Event.T := Timeout + Now;
         end if;

         if SCHD.Self.Suspension_Event.T < Suspension_Time_Minimum + Now then
            --  Too short timeout -> send timeout event without suspension
            APPSCHD.Send_Timeout_Event (ST => SCHD.Self);
         else
            --  Suspend scheduler task
            TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
              TSTE.APPSCHED_EVENT_TIMEDWAIT;

            SCHD.Running_Task_Gets_Suspended
              (With_Status => TIMED_WAITING_APPSCHED_EVENT,
               At_Time     => Now);

            SCHD.Do_Scheduling; -- leave CPU
            --  After a new event arrival (could be a timeout event) the
            --  scheduler task continues here...
         end if; -- Timeout too short??
      end if; -- Events queue empty??

      --  Dequeue the event
      Event_Ac :=
        APPSCHD_EVENTS_LST.Head (UC (SCHD.Self.AppScheduler).Events_Q);
      APPSCHD_EVENTS_LST.Dequeue_Head (UC (SCHD.Self.AppScheduler).Events_Q);
   end Get_Event;

   ---------------------------------------------------------------------------
   --  Posix_Appsched_Scheduler_Create  --------------------------------------
   ---------------------------------------------------------------------------
   function Posix_Appsched_Scheduler_Create
     (Sched_Ops  : access APPSCHD_DATA.Scheduler_Operations;
      Sched_Data : in     System.Address;
      Priority   : in     K.Task_Priority;
      Sched_Id   : access K.Task_Id) return Int is

      Attr      : TO.Pthread_Attr_T :=
        (Magic         => TO.ATTR_INITIALIZED,
         Sched_Policy  => K.SCHED_FIFO,
         Detach_State  => K.PTHREAD_CREATE_JOINABLE,
         Inherit_Sched => TO.PTHREAD_EXPLICIT_SCHED,
         Param  =>
           TO.Sched_Param'(Sched_Priority => Priority,
                           Sched_SS_Low_Priority => K.Task_Priority'First,
                           Sched_SS_Repl_Period  => (0, 0),
                           Sched_SS_Init_Budget  => (0, 0),
                           Sched_SS_Max_Repl     => 0),
         Stack_Size          => CP.Stack_Size_In_Bytes,
         Rel_Deadline        => HAL.HWTime'Last,
         Preemption_Level    => K.Task_Preemption_Level'Last,
         App_Scheduler       => null,
         AppSched_Param      => (others => 0),
         AppSched_Param_Size => 0,
         App_Scheduler_State => TO.PTHREAD_APPSCHEDULER);
      Flags : Integer;
      T : K.Task_Id;
   begin
      K.Enter_Critic_Section (Flags);
      T :=
        TOI.Create_Task (Application_Scheduler_Task_Body.Generic_Body'Access,
                         Sched_Data,
                         Attr);

      --  Error handling
      if K.Tasks_Lists."=" (T, null) then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_TEMPORARILY_UNAVAILABLE;
      end if;

      --  Set scheduler operations
      APPSCHD_DATA.AppScheduler_Data_Ac (T.AppScheduler).Ops := Sched_Ops.all;
      APPSCHD_DATA.AppScheduler_Data_Ac (T.AppScheduler).Data := Sched_Data;

      SCHD.New_Task_Created (T);
      SCHD.Do_Scheduling;

      K.Leave_Critic_Section (Flags);
      Sched_Id.all := T;
      return 0;
   end Posix_Appsched_Scheduler_Create;

   ----------------------------------------------------------------------------
   -- Execute_Actions ---------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Invoked from 'Sched.POSIX_Appsched_Execute_Actions' and
   --  'POSIX_Application_Scheduling.Execute_Actions'.
   procedure Execute_Actions (ST    : in  Task_Id; --  Scheduler task
                              SA    : in  APPSCHD_DATA.Sched_Actions_Set;
                              Error : out K.Error_Code) is
      use type Mutexes.Mutex_Descriptor;
      use type HAL.HWTime;
      use APPSCHD_DATA;
      type Sched_Action_Ac is access constant APPSCHD_DATA.Sched_Action;
      Action : Sched_Action_Ac;
   begin
      Error := NO_ERROR;
      if not CP.Use_Application_Defined_Scheduling then
         return;
      end if;
      if ST.AppScheduler = null then
         --  The calling task is not an application scheduler
         Error := APPSCHED_POLICY_ERROR;
         return;
      end if;
      if To_Int (SA.Initialized) /= To_Int (APPSCHD_DATA.INITIALIZED) then
         --  Not valid 'Sched_Actions_Set' object.
         Error := INVALID_ARGUMENT;
         return;
      end if;

      --  Check and execute the scheduling actions
      for I in 1 .. SA.Last loop
         Action := SA.Set (I)'Access;

         --  Detect invalid tasks
         if (Action.Action /= ACCEPT_MUTEX and
             Action.Action /= REJECT_MUTEX)     then
            if not Task_OK (Action.T, NEW_APPSCHED) then
               Error := NO_SUCH_PROCESS;
               exit;

               --  Detect not active tasks (NEW_APPSCHED only allowed
               --  for ACCEPT_TASK and REJECT_TASK actions)
            elsif ((Action.Action /= ACCEPT_TASK and
                    Action.Action /= REJECT_TASK) and
                   Action.T.Magic /= ACTIVE) then
                  Error := INVALID_ARGUMENT;
                  exit;

               --  Detect tasks not associated with this scheduler.
            elsif (UC (Action.T.Sched_App).Scheduler /= ST) then
               Error := INVALID_ARGUMENT;
               exit;
            end if;
         end if;
         --  Detect action related errors
         case Action.Action is
            when ACCEPT_TASK | REJECT_TASK =>
               pragma Assert
                 (Action.T.Status = WAITING_POLICY_CHANGE_TO_SCHED_APP or
                  (Action.T.Status = APPSCHED_SUSPENDED and
                   Action.T.Magic  = NEW_APPSCHED) or
                  Action.T.Status = WAITING_APPSCHEDULER_RESPONSE);
               if Action.T.Task_Waiting_Appscheduler_Response = null then
                  Error := INVALID_ARGUMENT;
                  exit;
               end if;
               pragma Assert
                 (K.Task_OK
                    (Action.T.Task_Waiting_Appscheduler_Response));

            when ACTIVATE | ACTIVATE_WITH_URGENCY | SUSPEND =>
               case Action.T.Status is
                  when READY | APPSCHED_SUSPENDED |
                    TRYING_TO_LOCK_APPSCHED_MUTEX =>
                     null;

                  when WAITING_TO_LOCK_APPSCHED_MUTEX =>
                     if Action.Action = ACTIVATE then
                        --  For a WAITING_TO_LOCK_APPSCHED_MUTEX task an
                        --  ACTIVATE action is an error because it should be
                        --  actived with a LOCK_MUTEX action.
                        Error := INVALID_ARGUMENT;
                        exit;
                     end if;

                  when TIME_SUSPENDED | BLOCKED |
                    WAITING_POLICY_CHANGE_TO_SCHED_APP |
                    WAITING_APPSCHEDULER_RESPONSE |
                    TIMED_WAITING_CV | TIMED_WAITING_MUTEX |
                    TIMED_WAITING_SEM |
                    WAITING_SIGNAL | WAITING_TASK_TERMINATION |
                    WAITING_HW_INTERRUPT | TIMED_WAITING_HW_INTERRUPT |
                    SIG_HANDLER_WAIT_TO_DELIVER |
                    WAITING_APPSCHED_EVENT | TIMED_WAITING_APPSCHED_EVENT =>
                     Error := INVALID_ARGUMENT;
                     exit;
               end case;

            when TIMED_TASK_ACTIVATION | TIMED_TASK_NOTIFICATION =>
               null;

            when ACCEPT_MUTEX | REJECT_MUTEX =>
               if not MIAPP.To_Accept_Or_Reject (Action.M) then
                  Error := INVALID_ARGUMENT;
                  exit;
               end if;

            when LOCK_MUTEX =>
               if not MI.Mutex_OK (Action.M) then
                  Error := INVALID_ARGUMENT;
                  exit;
               end if;
               case Action.T.Status is
                  when WAITING_TO_LOCK_APPSCHED_MUTEX
                    | TRYING_TO_LOCK_APPSCHED_MUTEX =>
                     null;
                  when READY | TIME_SUSPENDED | BLOCKED |
                    WAITING_POLICY_CHANGE_TO_SCHED_APP |
                    WAITING_APPSCHEDULER_RESPONSE |
                    TIMED_WAITING_CV | TIMED_WAITING_MUTEX |
                    TIMED_WAITING_SEM |
                    WAITING_SIGNAL | WAITING_TASK_TERMINATION |
                    WAITING_HW_INTERRUPT | TIMED_WAITING_HW_INTERRUPT |
                    SIG_HANDLER_WAIT_TO_DELIVER |
                    WAITING_APPSCHED_EVENT | TIMED_WAITING_APPSCHED_EVENT |
                    APPSCHED_SUSPENDED =>
                     Error := INVALID_ARGUMENT;
                     exit;
               end case;

         end case;

         --  Execute actions
         case Action.Action is
            when ACCEPT_TASK =>
               --  A task is accepted, so it is added to the scheduled
               --  tasks list and the task waiting for the appscheduler
               --  response is put ready without error.
               declare
                  TC : K.Task_Container_Ac;
               begin
                  --  Add 'T' to its scheduler task queue
                  TC :=
                    TCLST.Head (UC (ST.AppScheduler).Reserved_TCs);
                  pragma Assert (TCLST."/=" (TC, null));
                  TCLST.Dequeue_Head
                    (UC (ST.AppScheduler).Reserved_TCs);
                  TC.T := Action.T;
                  TCLST.Enqueue_Head (TC, UC (ST.AppScheduler).Tasks_Q);
               end;
               if Action.T /= TCB_Ac_To_Task_Id
                 (Action.T.Task_Waiting_Appscheduler_Response)
               then
                  pragma Assert
                    (TCB_Ac_To_Task_Id
                       (Action.T.Task_Waiting_Appscheduler_Response).Status
                     = WAITING_APPSCHEDULER_RESPONSE or
                     TCB_Ac_To_Task_Id
                       (Action.T.Task_Waiting_Appscheduler_Response).Status
                     = WAITING_POLICY_CHANGE_TO_SCHED_APP);
                  --  The task waiting for the appscheduler response is
                  --  put ready (this doesn't have to be done when a task
                  --  itself has changed its policy to SCHED_APP)
                  SCHD.Task_Gets_Ready
                    (TCB_Ac_To_Task_Id
                     (Action.T.Task_Waiting_Appscheduler_Response));
               end if;
               --  Set magic to ACTIVE is necessary for a just created task
               Action.T.Magic := K.ACTIVE;
               --  These are necessary when the task has changed its
               --  policy to SCHED_APP from any other (they are harmless
               --  when are performed on a just created task).
               Action.T.Status := APPSCHED_SUSPENDED;
               TOI.Set_Base_Prio (Action.T, Action.T.Base_Prio);
               SCHD.Task_Changes_Policy (Action.T, SCHED_APP);
               Action.T.Sched_Policy := SCHED_APP;

            when REJECT_TASK =>
               declare
                  TC : K.Task_Container_Ac;
               begin
                  --  A task is rejected: a reserved TC is released.
                  TC :=
                    TCLST.Head (UC (ST.AppScheduler).Reserved_TCs);
                  pragma Assert (TCLST."/=" (TC, null));
                  TCLST.Dequeue_Head
                    (UC (ST.AppScheduler).Reserved_TCs);
                  APPSCHD_DATA.Release_Task_Container (TC);

                  --  The task waiting for the appscheduler response is put
                  --  ready with an 'APPSCHED_REJECTED' error code.
                  TCB_Ac_To_Task_Id
                    (Action.T.Task_Waiting_Appscheduler_Response).
                    Internal_Error_Code := APPSCHED_REJECTED;
                  SCHD.Task_Gets_Ready
                    (TCB_Ac_To_Task_Id
                     (Action.T.Task_Waiting_Appscheduler_Response));
               end;

            when ACTIVATE | ACTIVATE_WITH_URGENCY =>
               pragma Assert
                 (Action.T.Status = READY or
                  Action.T.Status = APPSCHED_SUSPENDED or
                    Action.T.Status = TRYING_TO_LOCK_APPSCHED_MUTEX);

               if Action.Action = ACTIVATE_WITH_URGENCY then
                  Action.T.Active_Urgency := Action.Urgency;
                  Action.T.Base_Urgency   := Action.Urgency;
               end if;

               Activate_Appsched_Task (Action.T, Action.Action);

            when SUSPEND =>
               pragma Assert
                 (Action.T.Status = READY or
                  Action.T.Status = APPSCHED_SUSPENDED or
                  Action.T.Status = WAITING_TO_LOCK_APPSCHED_MUTEX or
                  Action.T.Status = TRYING_TO_LOCK_APPSCHED_MUTEX);

               if Action.T.Status = READY then
                  SCHD.Non_Running_Task_Gets_Blocked (Action.T,
                                                      APPSCHED_SUSPENDED);
                  --  Extract from the 'Suspension_Deferred_Tasks'
                  --  list (in the case it is there)
                  SCHDAPP_DATA_LST.Dequeue
                    (UC (Action.T.Sched_App),
                     UC (ST.AppScheduler).Suspension_Deferred_Tasks);
               end if;

            when TIMED_TASK_ACTIVATION =>
               if UC (Action.T.Sched_App).Activation_Event.Used then
                  SCHD.Remove_Timed_Event
                    (UC (Action.T.Sched_App).Activation_Event'Access);
               end if;
               UC (Action.T.Sched_App).Activation_Event.T :=
                 Action.At_Time - Timers.Realtime_Clock_Offset;
               UC (Action.T.Sched_App).Activation_Event :=
                 (K.Timed_Events_Lists.Element
                  (UC (Action.T.Sched_App).Activation_Event) with
                  Task_To_Notify => Action.T,
                  Urgency        => Action.Urgency,
                  Used           => True);
               TE_T.Enqueue_Standard_Event_And_Update_Timer
                 (UC (Action.T.Sched_App).Activation_Event'Access,
                  HAL.Get_HWTime);

            when TIMED_TASK_NOTIFICATION =>
               if UC (Action.T.Sched_App).Notification_Event.Used then
                  SCHD.Remove_Timed_Event
                    (UC (Action.T.Sched_App).Notification_Event'Access);
               end if;
               UC (Action.T.Sched_App).Notification_Event.T :=
                 Action.At_Time - Timers.Realtime_Clock_Offset;
               UC (Action.T.Sched_App).Notification_Event :=
                 (K.Timed_Events_Lists.Element
                  (UC (Action.T.Sched_App).Notification_Event) with
                  Task_To_Notify => Action.T,
                  Used           => True);
               TE_T.Enqueue_Standard_Event_And_Update_Timer
                 (UC (Action.T.Sched_App).Notification_Event'Access,
                  HAL.Get_HWTime);

            when ACCEPT_MUTEX | REJECT_MUTEX =>
               pragma Assert
                 (MIAPP.Task_Waiting_Response (Action.M).Status =
                  WAITING_APPSCHEDULER_RESPONSE);

               if Action.Action = REJECT_MUTEX then
                  --  The task waiting for the appscheduler response is put
                  --  ready (with an 'APPSCHED_REJECTED' error code.
                  MIAPP.Task_Waiting_Response (Action.M).
                    Internal_Error_Code := APPSCHED_REJECTED;
               end if;

               SCHD.Task_Gets_Ready
                 (MIAPP.Task_Waiting_Response (Action.M));

            when LOCK_MUTEX =>
               pragma Assert
                 (Action.T.Status = WAITING_TO_LOCK_APPSCHED_MUTEX or
                  Action.T.Status = TRYING_TO_LOCK_APPSCHED_MUTEX);
               if (Action.M /=
                   UC (Action.T.Sched_App).AppMutex_Where_Waiting) then
                  --  Trying to lock a mutex the task is no waiting
                  Error := INVALID_ARGUMENT;
                  exit;
               elsif MI.Get_Owner (Action.M) /= null then
                  --  The mutex is already owned
                  Error := RESOURCE_BUSY;
                  exit;
               else
                  MI.Set_Owner (Action.M, Action.T);
                  SCHD.Task_Gets_Ready (Action.T);
               end if;

         end case;
      end loop;
   end Execute_Actions;

   ------------------------------
   --  Activate_Appsched_Task  --
   ------------------------------
   --
   --  Assumes urgency changes has been performed before. Called from
   --  'Execute_Actions' and from 'SCHD.Timer_Interrupt_Handler' for
   --  an 'Activation_Timed_Event'.
   procedure Activate_Appsched_Task (T      : in Task_Id;
                                     Action : in APPSCHD_DATA.Actions_T) is
      use type APPSCHD_DATA.Scheduler_Reply_Ac, APPSCHD_DATA.Actions_T;
   begin
      if not (T.Status = READY or
              T.Status = APPSCHED_SUSPENDED or
              T.Status = TRYING_TO_LOCK_APPSCHED_MUTEX) then
         --  This procedure can be called from
         --  'SCHD.Timer_Interrupt_Handler' when 'T' is in any
         --  possible status. An appsched task only is reactivated
         --  when it is READY or APPSCHED_SUSPENDED or
         --  TRYING_TO_LOCK_APPSCHED_MUTEX. In any other case nothing
         --  has to be done.
         return;
      end if;

      if T.Status = TRYING_TO_LOCK_APPSCHED_MUTEX then
         --  The mutex hasn't been given to the task
         SCHD.Task_Gets_Ready (T);
      else  --  Status = APPSCHED_SUSPENDED or READY
         if T.Status = APPSCHED_SUSPENDED then
            SCHD.Task_Gets_Ready (T);
         else
            --  If READY then extract task from the
            --  'Suspension_Deferred_Tasks' list (in the case
            --  it is there)
            SCHDAPP_DATA_LST.Dequeue
              (UC (T.Sched_App),
               UC (UC (T.Sched_App).Scheduler.AppScheduler).
               Suspension_Deferred_Tasks);
            if Action = APPSCHD_DATA.ACTIVATE_WITH_URGENCY then
               --  Although the task is READY we must inform
               --  to the scheduler to change its position in
               --  the ready queue
               SCHD.Ready_Task_Changes_Urgency (T);
            end if;

         end if;
         --  Either APPSCHED_SUSPENDED or READY copy
         --  scheduler reply
         if UC (T.Sched_App).Scheduler_Reply /= null then
            for J in 1 .. UC (UC (T.Sched_App).Scheduler.AppScheduler).
              Reply_Size loop
               UC (T.Sched_App).Scheduler_Reply (J) :=
                 UC (UC (T.Sched_App).Scheduler.AppScheduler).Reply (J);
            end loop;
            UC (T.Sched_App).Reply_Size :=
              UC (UC (T.Sched_App).Scheduler.AppScheduler).Reply_Size;
         end if;
      end if;
   end Activate_Appsched_Task;
   pragma Inline (Activate_Appsched_Task);

   ----------------------------------------------------------------------------
   -- Scheduling Actions Management -------------------------------------------
   ----------------------------------------------------------------------------

   ---------------------------------
   -- POSIX_Appsched_Actions_Init --
   ---------------------------------
   function POSIX_Appsched_Actions_Init
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Sched_Actions.Initialized := APPSCHD_DATA.INITIALIZED;
      Sched_Actions.Last := 0;
      return 0;
   end POSIX_Appsched_Actions_Init;

   ------------------------------------
   -- POSIX_Appsched_Actions_Destroy --
   ------------------------------------
   function POSIX_Appsched_Actions_Destroy
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set)  return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      Sched_Actions.Initialized := APPSCHD_DATA.NOT_INITIALIZED;
      return 0;
   end POSIX_Appsched_Actions_Destroy;

   --------------------------------------
   -- POSIX_Appsched_Actions_Addaccept --
   --------------------------------------
   function POSIX_Appsched_Actions_Addaccept
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T      := T;
      Sched_Actions.Set (Sched_Actions.Last).Action :=
        APPSCHD_DATA.ACCEPT_TASK;
      return 0;
   end POSIX_Appsched_Actions_Addaccept;

   --------------------------------------
   -- POSIX_Appsched_Actions_Addreject --
   --------------------------------------
   function POSIX_Appsched_Actions_Addreject
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T      := T;
      Sched_Actions.Set (Sched_Actions.Last).Action :=
        APPSCHD_DATA.REJECT_TASK;
      return 0;
   end POSIX_Appsched_Actions_Addreject;

   ----------------------------------------
   -- POSIX_Appsched_Actions_Addactivate --
   ----------------------------------------
   function POSIX_Appsched_Actions_Addactivate
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      UC (T.Sched_App).Suspend_After_Scheduler_Operation := False;
      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T      := T;
      Sched_Actions.Set (Sched_Actions.Last).Action := APPSCHD_DATA.ACTIVATE;
      return 0;
   end POSIX_Appsched_Actions_Addactivate;

   -------------------------------------------
   -- POSIX_Appsched_Actions_Addactivateurg --
   -------------------------------------------
   function POSIX_Appsched_Actions_Addactivateurg
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id;
      Urgency       : in     K.Task_Urgency) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      UC (T.Sched_App).Suspend_After_Scheduler_Operation := False;
      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T       := T;
      Sched_Actions.Set (Sched_Actions.Last).Urgency := Urgency;
      Sched_Actions.Set (Sched_Actions.Last).Action  :=
        APPSCHD_DATA.ACTIVATE_WITH_URGENCY;
      return 0;
   end POSIX_Appsched_Actions_Addactivateurg;

   -----------------------------------------------
   -- POSIX_Appsched_Actions_Addtimedactivation --
   -----------------------------------------------
   function POSIX_Appsched_Actions_Addtimedactivation
       (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
        T             : in     K.Task_Id;
        Urgency       : in     K.Task_Urgency;
        HWT           : in     HAL.HWTime) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T       := T;
      Sched_Actions.Set (Sched_Actions.Last).Urgency := Urgency;
      Sched_Actions.Set (Sched_Actions.Last).Action  :=
        APPSCHD_DATA.TIMED_TASK_ACTIVATION;
      Sched_Actions.Set (Sched_Actions.Last).At_Time := HWT;
      return 0;
   end POSIX_Appsched_Actions_Addtimedactivation;

   -----------------------------------------------------
   -- POSIX_Appsched_Actions_Addtimedtasknotification --
   -----------------------------------------------------
   function POSIX_Appsched_Actions_Addtimedtasknotification
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id;
      HWT           : in     HAL.HWTime) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T       := T;
      Sched_Actions.Set (Sched_Actions.Last).Action  :=
        APPSCHD_DATA.TIMED_TASK_NOTIFICATION;
      Sched_Actions.Set (Sched_Actions.Last).At_Time := HWT;
      return 0;
   end POSIX_Appsched_Actions_Addtimedtasknotification;

   ---------------------------------------
   -- POSIX_Appsched_Actions_Addsuspend --
   ---------------------------------------
   function POSIX_Appsched_Actions_Addsuspend
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T      := T;
      Sched_Actions.Set (Sched_Actions.Last).Action := APPSCHD_DATA.SUSPEND;
      return 0;
   end POSIX_Appsched_Actions_Addsuspend;

   --------------------------------------------
   -- POSIX_Appsched_Actions_Addaccept_Mutex --
   --------------------------------------------
   function POSIX_Appsched_Actions_Addaccept_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).M      := M;
      Sched_Actions.Set (Sched_Actions.Last).Action :=
        APPSCHD_DATA.ACCEPT_MUTEX;
      return 0;
   end POSIX_Appsched_Actions_Addaccept_Mutex;

   --------------------------------------------
   -- POSIX_Appsched_Actions_Addreject_Mutex --
   --------------------------------------------
   function POSIX_Appsched_Actions_Addreject_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).M      := M;
      Sched_Actions.Set (Sched_Actions.Last).Action :=
        APPSCHD_DATA.REJECT_MUTEX;
      return 0;
   end POSIX_Appsched_Actions_Addreject_Mutex;

   ------------------------------------------
   -- POSIX_Appsched_Actions_Addlock_Mutex --
   ------------------------------------------
   function POSIX_Appsched_Actions_Addlock_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int is
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if To_Int (Sched_Actions.Initialized) /=
        To_Int (APPSCHD_DATA.INITIALIZED) then
         return INVALID_ARGUMENT;
      end if;
      if Sched_Actions.Last >= APPSCHD_DATA.Sched_Actions_Array'Last then
         return NOT_ENOUGH_SPACE;
      end if;

      Sched_Actions.Last := Sched_Actions.Last + 1;
      Sched_Actions.Set (Sched_Actions.Last).T      := T;
      Sched_Actions.Set (Sched_Actions.Last).M      := M;
      Sched_Actions.Set (Sched_Actions.Last).Action := APPSCHD_DATA.LOCK_MUTEX;
      return 0;
   end POSIX_Appsched_Actions_Addlock_Mutex;

   ----------------------------------------------------------------------------
   -- Invoke Scheduler --------------------------------------------------------
   ----------------------------------------------------------------------------

   ----------------------
   -- Invoke_Scheduler --
   ----------------------
   function Invoke_Scheduler (User_Event_Code : in Int) return Int is
      Flags : Integer;
      --  Actions : APPSCHD_DATA.Sched_Actions_Set;
      --  Error : Kernel.Error_Code;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if SCHD.Self.Sched_Policy /= SCHED_APP then
         K.Leave_Critic_Section (Flags);
         return APPSCHED_POLICY_ERROR;
      end if;

      --  Send APPSCHED_EXPLICIT_CALL event to the scheduler task
      APPSCHD.Send_Event_From_Task
        (Event_Code      => APPSCHD_DATA.APPSCHED_EXPLICIT_CALL,
         ST              => UC (SCHD.Self.Sched_App).Scheduler,
         T               => SCHD.Self);

      SCHD.Do_Scheduling; -- Give chance to the scheduler task to execute

      pragma Assert (SCHD.Self.Internal_Error_Code   = NO_ERROR or
                    SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED);
      if SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED then
         --  The appscheduler has the APPSCHED_EXPLICIT_CALL event masked
         SCHD.Self.Internal_Error_Code := NO_ERROR;  --  Reset flag
         K.Leave_Critic_Section (Flags);
         return APPSCHED_EVENT_MASKED;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
--        pragma Assert (SCHD.Self.Internal_Error_Code   = NO_ERROR or
--                      SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED);
--        if SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED then
--           --  The appscheduler has the APPSCHED_EXPLICIT_CALL event masked
--           SCHD.Self.Internal_Error_Code := NO_ERROR;  --  Reset flag
--           Kernel.Leave_Critic_Section (Flags);
--           return APPSCHED_EVENT_MASKED;
--
--        else
--        Execute_Actions (UC (SCHD.Self.Sched_App).Scheduler, Actions, Error);
--           if Error /= Kernel.NO_ERROR then
--              pragma Assert (False);
--              null;
--           end if;
--
--           --  Kernel.Enter_Critic_Section (Flags);
   end Invoke_Scheduler;

   --------------------------------
   -- Invoke_Scheduler_With_Data --
   --------------------------------
   function Invoke_Scheduler_With_Data
     (Info       : in  System.Address;
      Info_Size  : in  Int;
      Reply      : in  APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : in  APPSCHD_DATA.Scheduler_Reply_Size_Ac) return Int is
      --  xFlags : Integer;
      use type APPSCHD_DATA.Scheduler_Reply_Size_Ac;
   begin
      return Invoke_Scheduler (0);
--        --  Annotate 'Reply'. This memory area will be filled when task
--        --  is reactivated in 'Execute_Actions'.
--        UC (SCHD.Self.Sched_App).Scheduler_Reply := Reply;

--        --  Send APPSCHED_EXPLICIT_CALL_WITH_DATA event to the scheduler task
--        APPSCHD.Send_Event_From_Task
--          (Event_Code => APPSCHD_DATA.APPSCHED_EXPLICIT_CALL_WITH_DATA,
--           ST         => UC (SCHD.Self.Sched_App).Scheduler,
--           T          => SCHD.Self,
--           Info       => Info,
--           Info_Size  => Info_Size);

--        SCHD.Do_Scheduling; -- Give chance to the scheduler task to execute

--        pragma Assert (SCHD.Self.Internal_Error_Code   = NO_ERROR or
--                      SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED);
--        if SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED then
--           --  The appscheduler has the APPSCHED_EXPLICIT_CALL event masked
--           SCHD.Self.Internal_Error_Code := NO_ERROR;  --  Reset flag
--           Kernel.Leave_Critic_Section (Flags);
--           return APPSCHED_EVENT_MASKED;
--        end if;

--        --  Return 'Reply_Size' and set 'Scheduler_Reply' field to null.
--        if Reply_Size /= null then
--           Reply_Size.all := UC (SCHD.Self.Sched_App).Reply_Size;
--        end if;
--        UC (SCHD.Self.Sched_App).Scheduler_Reply := null;
--        UC (SCHD.Self.Sched_App).Reply_Size := 0;
--        Kernel.Leave_Critic_Section (Flags);
--        return 0;
   end Invoke_Scheduler_With_Data;

   ------------------------------------
   --  Pthread_Getappschedulerstate  --
   ------------------------------------
   function Pthread_Getappschedulerstate
     (T        : Task_Id;
      Appsched : access Regular_Or_App_Scheduler_T) return Int is
      Flags : Integer;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if T.AppScheduler = null then
         Appsched.all := PTHREAD_REGULAR;
      else
         Appsched.all := PTHREAD_APPSCHEDULER;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getappschedulerstate;

   -----------------------------
   -- Pthread_Setappscheduler --
   -----------------------------
   function Pthread_Setappscheduler (T            : in Task_Id;
                                     AppScheduler : in Task_Id) return Int is
      Flags : Integer;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Task_OK (T) then
         --  Invalid task
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if (not Task_OK (AppScheduler) or else
          AppScheduler.AppScheduler = null) then
         --  Invalid scheduler task
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if T.Sched_Policy = SCHED_APP then
         --  The scheduler can not be changed
         K.Leave_Critic_Section (Flags);
         return APPSCHED_POLICY_ERROR;
      end if;
      UC (T.Sched_App).Scheduler := AppScheduler;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setappscheduler;

   -----------------------------
   -- Pthread_Getappscheduler --
   -----------------------------
   function Pthread_Getappscheduler
     (T            : in     Task_Id;
      Appscheduler : access Task_Id) return Int is
      Flags : Integer;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Task_OK (T) then
         --  Invalid task
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      Appscheduler.all := UC (T.Sched_App).Scheduler;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getappscheduler;

   -----------------------------------
   -- Pthread_Setappschedparam --
   -----------------------------------
   function Pthread_Setappschedparam
     (T                   : in Task_Id;
      AppSched_Param      : in K.AppSched_Param_T_Ac;
      AppSched_Param_Size : in K.AppSched_Param_Size_T) return Int is
      Flags : Integer;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Task_OK (T, K.NEW_APPSCHED) then
         --  Invalid task
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if (AppSched_Param_Size < AppSched_Param_Size_T'First or else
          AppSched_Param_Size_T'Last < AppSched_Param_Size or else
          (AppSched_Param_Size /= 0 and AppSched_Param = null))  then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if AppSched_Param_Size /= 0 then
         UC (T.Sched_App).Param (1 .. AppSched_Param_Size) :=
           AppSched_Param (1 .. AppSched_Param_Size);
      end if;
      UC (T.Sched_App).Param_Size := AppSched_Param_Size;

      if T.Sched_Policy = SCHED_APP then
         --  Send event for the scheduler task
         SCHD.New_Sched_Param_In_AppSched_Task (T);
         SCHD.Do_Scheduling;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setappschedparam;

   ------------------------------
   -- Pthread_Getappschedparam --
   ------------------------------
   function Pthread_Getappschedparam
     (T                   : in     Task_Id;
      AppSched_Param      : in     K.AppSched_Param_T_Ac;
      AppSched_Param_Size : access K.AppSched_Param_Size_T) return Int is
      Flags : Integer;
   begin
      if not CP.Use_Application_Defined_Scheduling then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Task_OK (T, K.NEW_APPSCHED) then
         --  Invalid task
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if AppSched_Param = null then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if UC (T.Sched_App).Param_Size /= 0 then
         AppSched_Param (1 .. UC (T.Sched_App).Param_Size) :=
           UC (T.Sched_App).Param (1 .. UC (T.Sched_App).Param_Size);
      end if;
      AppSched_Param_Size.all := UC (T.Sched_App).Param_Size;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getappschedparam;

end MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
