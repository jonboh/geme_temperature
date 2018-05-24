------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . T a s k s _ O p e r a t i o n s'
--
--                                 Body
--
--
--  File 'k-tasks_operation.adb'                                      By MAR.
--
--
--  Operations with Tasks.
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
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Signals.Internals;

with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Pool_TCBs;

--  Debug
with MaRTE.Kernel.Debug;
with MaRTE.Direct_IO;

package body MaRTE.Kernel.Tasks_Operations is

   package SCHD         renames K.Scheduler;
   package SI           renames K.Signals.Internals;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package MLST         renames K.Mutexes_Lists;
   package DBG          renames K.Debug;

   use K.Scheduler, MaRTE.Kernel.Tasks_Lists;

   use type Int, HAL.HWTime, TSPC.Timespec_Ac;

   function UC (Base : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;
   pragma Inline (UC);

   ----------------------------------------------------------------------------
   -- Get and Set Scheduling Parameters and Policy ----------------------------
   ----------------------------------------------------------------------------

   ------------------------------------
   -- Error_In_Scheduling_Parameters --
   ------------------------------------
   --
   --  Check the 'Policy' and 'Parameters'. Called from
   --  'Pthread_Create', 'TO_ATTR.Pthread_Attr_Setschedparam' and
   --  'Pthread_Setschedparam'.
   function Error_In_Scheduling_Parameters (Param  : in Sched_Param;
                                            Policy : in Scheduling_Policies)
                                            return K.Error_Code is
      use type HAL.HWTime;
   begin
      --  Functionality supported
      if ((not CP.Use_Round_Robin_Scheduling_Policy
           and then Policy = SCHED_RR)
          or
          (not CP.Use_Sporadic_Server_Scheduling_Policy
           and then Policy = SCHED_SPORADIC)
          or
          (not CP.Use_Application_Defined_Scheduling
           and then Policy = SCHED_APP)
          or
          (not CP.Use_EDF_Scheduling_Policy'First
           and then Policy = SCHED_EDF))
      then
         return OPERATION_NOT_SUPPORTED;
      end if;
      --  Priority range
      if (Param.Sched_Priority < Task_Priority'First or
          Param.Sched_Priority > Task_Priority'Last) then
         return INVALID_ARGUMENT;
      end if;
      --  Policy range
      if (Policy < Scheduling_Policies'First or
          Policy > Scheduling_Policies'Last) then
         return INVALID_ARGUMENT;
      end if;
      --  Policy dependent errors
      case Policy is
         when SCHED_APP => --  SCHED_APP related errors
            null;

         when SCHED_SPORADIC => --  SCHED_SPORADIC related errors
            if (TSPC.Timespec_To_HWTime (Param.Sched_SS_Repl_Period) <
                TSPC.Timespec_To_HWTime (Param.Sched_SS_Init_Budget)) then
               --  Invalid repl. period or budget
               return INVALID_ARGUMENT;
            end if;
            if (Param.Sched_SS_Max_Repl > CP.Sporadic_Server_Replenishments_Mx
                or Param.Sched_SS_Max_Repl <= 0) then
               --  Invalid Max. number of simultaneous replenishment operations
               return INVALID_ARGUMENT;
            end if;
            if Param.Sched_Priority < Param.Sched_SS_Low_Priority then
               --  High priority lower than low priority
               return INVALID_ARGUMENT;
            end if;

         when SCHED_FIFO | SCHED_RR | SCHED_OTHER | SCHED_EDF =>
            null;
      end case;
      return NO_ERROR;
   end Error_In_Scheduling_Parameters;

   ---------------------------
   -- Pthread_Getschedparam --
   ---------------------------
   function Pthread_Getschedparam (T      : in Task_Id;
                                   Policy : access Scheduling_Policies;
                                   Param  : access Sched_Param)
                                   return Int is
   begin
      Policy.all := T.Sched_Policy;

      Param.all := Sched_Param'
        (Sched_Priority      => T.Base_Prio,
         Sched_SS_Low_Priority => T.Sched_SS_Low_Priority,
         Sched_SS_Repl_Period  =>
           TSPC.HWTime_To_Timespec (T.Sched_SS_Repl_Period),
         Sched_SS_Init_Budget  =>
           TSPC.HWTime_To_Timespec (T.Capacity),
         Sched_SS_Max_Repl     => T.Sched_SS_Max_Repl);

      return 0;
   end Pthread_Getschedparam;

   ---------------------------
   -- Pthread_Setschedparam --
   ---------------------------
   function Pthread_Setschedparam (T      : in     Task_Id;
                                   Policy : in     Scheduling_Policies;
                                   Param  : access Sched_Param)
                                   return Int is
      Flags : Integer;
      Original_Base_Priority : K.Task_Priority;
      Changing_From_Standard_To_Sched_App : Boolean := False;
      Error : K.Error_Code;
   begin
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if (CP.Use_Application_Defined_Scheduling and then
          Policy = SCHED_APP) then
         if T.Sched_Policy /= SCHED_APP and T.Status /= READY then
            --  Only for READY tasks is allowed changing to SCHED_APP policy
            K.Leave_Critic_Section (Flags);
            return OPERATION_NOT_SUPPORTED;
         end if;
         if (not Task_OK (UC (T.Sched_App).Scheduler) or else
             UC (T.Sched_App).Scheduler.AppScheduler = null) then
            --  Invalid "appscheduler" attribute
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
         if Param.Sched_Priority > UC (T.Sched_App).Scheduler.Base_Prio then
            --  Priority higher than its scheduler task
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
      end if; -- Configuration_Parameters.Use_Application_Defined_Scheduling
      if T.Sched_Policy = SCHED_SPORADIC and then
        Policy /= SCHED_SPORADIC and then
        T.Number_Of_Pending_Replenishments /= 0 then
            --  By now changing from SCHED_SPORADIC to other policy
            --  is not allowed when there are pending replenishments
            K.Leave_Critic_Section (Flags);
            return OPERATION_NOT_SUPPORTED;
      end if;
      Error := Error_In_Scheduling_Parameters (Param.all, Policy);
      if Error /= NO_ERROR then
         return Error;
      end if;

      --  Set task new parameters
      case Policy is
         when SCHED_FIFO | SCHED_OTHER | SCHED_EDF =>
            null;

         when SCHED_RR =>
            if CP.Use_Round_Robin_Scheduling_Policy then
               T.Capacity := HAL.Duration_To_HWTime
                 (CP.Round_Robin_Interval_Time_Period);
               T.End_Of_RR_Slice_Pending := False;
            end if;

         when SCHED_APP =>
            if (CP.Use_Application_Defined_Scheduling
                and then T.Sched_Policy /= SCHED_APP) then
               --  Change from standard policy to SCHED_APP: before
               --  actually changing the policy the appscheduler must
               --  accept the new task.
               Changing_From_Standard_To_Sched_App := True;

               if not APPSCHD_DATA.Any_Task_Container_Free_In_Pool then
                  K.Leave_Critic_Section (Flags);
                  return RESOURCE_TEMPORARILY_UNAVAILABLE;
               end if;

               --  Annotate task waiting appscheduler response
               T.Task_Waiting_Appscheduler_Response := TCB_Ac (Self);
               --  Set new base prio
               Original_Base_Priority := T.Base_Prio;
               T.Base_Prio := Param.Sched_Priority;

               --  Suspend task(s) and send NEW event to the scheduler
               SCHD.Trying_To_Change_Policy_To_Sched_App (T);
               SCHD.Do_Scheduling;
               --  When here is because the appscheduler has responded

               pragma Assert
                 (SCHD.Self.Internal_Error_Code = NO_ERROR or
                    SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED or
                    SCHD.Self.Internal_Error_Code = APPSCHED_REJECTED);
               if SCHD.Self.Internal_Error_Code /= NO_ERROR then
                  --  Task cannot change its policy: the appscheduler has
                  --  the NEW_TASK event masked or task has been rejected
                  --  by its appscheduler
                  declare
                     Error : K.Error_Code := Self.Internal_Error_Code;
                  begin
                     SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
                     T.Base_Prio := Original_Base_Priority;
                     T.Task_Waiting_Appscheduler_Response := null;
                     if (T /= SCHD.Self and then
                         T.Status = WAITING_POLICY_CHANGE_TO_SCHED_APP) then
                        SCHD.Task_Gets_Ready (T);
                     end if;
                     K.Leave_Critic_Section (Flags);
                     return Error;
                  end;
               else
                  --  NO_ERROR: give correct values to 'Sched_App'
                  --  fields. Some of them do not change since they
                  --  must have been set before this function is
                  --  called.
                  APPSCHD_DATA.Sched_App_Data (T.Sched_App.all) :=
                    (Scheduler              => UC (T.Sched_App).Scheduler,
                     Param                  => UC (T.Sched_App).Param,
                     Param_Size             => UC (T.Sched_App).Param_Size,
                     AppMutex_Where_Waiting => Mutexes.Null_Mutex_Descriptor,
                     Scheduler_Reply        => null,
                     Reply_Size             => 0,
                     Owner                  => T,
                     Notification_Event     =>
                       UC (T.Sched_App).Notification_Event,
                     Activation_Event       =>
                       UC (T.Sched_App).Activation_Event,
                     Suspend_After_Scheduler_Operation => True);
               end if;
            end if;

         when SCHED_SPORADIC =>
            if CP.Use_Sporadic_Server_Scheduling_Policy then
               T.Sched_SS_High_Priority := Param.Sched_Priority;
               T.Sched_SS_Low_Priority  := Param.Sched_SS_Low_Priority;
               T.Sched_SS_Repl_Period   :=
                 TSPC.Timespec_To_HWTime (Param.Sched_SS_Repl_Period);
               T.Capacity               :=
                 TSPC.Timespec_To_HWTime (Param.Sched_SS_Init_Budget);
               T.Sched_SS_Max_Repl      := Param.Sched_SS_Max_Repl;
               if T.Sched_Policy /= SCHED_SPORADIC then
                  T.Capacity_Exhausted := False;
                  --  Only when the old policy of the task wasn't
                  --  SCHED_SPORADIC. In the case it is SCHED_SPORADIC
                  --  we don't want to set 'Capacity_Exhausted' to
                  --  true because in that case the task will be
                  --  placed in the high priority queue.
                  T.Replenish_Amount_Pending         := 0;
                  T.Number_Of_Pending_Replenishments := 0;
                  --  If the old policy of the task was SCHED_SPORADIC
                  --  we don't want to change these values.

               end if;
            end if;

      end case;

      if not Changing_From_Standard_To_Sched_App then
         --  (When a task is changed from any standard policy to SCHED_APP
         --  these operations are performed in
         --  'TO_APPSCHD.Execute_Actions')

         --  Set new base priority
         if Policy = SCHED_SPORADIC and T.Capacity_Exhausted then
            Internals.Set_Base_Prio (T, Param.Sched_SS_Low_Priority);
         else
            --  Standard case: NOT SCHED_SPORADIC tasks and
            --  SCHED_SPORADIC tasks with capacity remaining
            Internals.Set_Base_Prio (T, Param.Sched_Priority);
         end if;

         --  Set new policy
         if Policy = SCHED_APP and T.Sched_Policy = SCHED_APP then
            --  Send event CHANGE_SCHED_PARAM to appscheduler
            SCHD.New_Sched_Param_In_AppSched_Task (T);
         else
            --  Inform the scheduler a task has changed its policy
            SCHD.Task_Changes_Policy (T, Policy);
            T.Sched_Policy := Policy;
         end if;

         Scheduler.Do_Scheduling;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setschedparam;

   --------------------------
   -- Pthread_Setschedprio --
   --------------------------
   function Pthread_Setschedprio (T    : in Task_Id;
                                  Prio : in MaRTE.Integer_Types.Int)
                                 return Int is
      Flags : Integer;
   begin
      if not CP.Use_POSIX_Thread_Execution_Scheduling'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if (Prio < Int (Task_Priority'First) or
          Prio > Int (Task_Priority'Last))    then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      --  Set new base priority
      if T.Sched_Policy = SCHED_SPORADIC then
         T.Sched_SS_High_Priority := Task_Priority (Prio);
      end if;
      if not (T.Sched_Policy = SCHED_SPORADIC and then
              T.Capacity_Exhausted) then
         Internals.Set_Base_Prio (T, Task_Priority (Prio),
                                  Head_Of_New_Prio_Q => True);
         --  When a SCHED_SPORADIC task has its capacity exhausted it
         --  is executing at the low priority and then is not logical
         --  to raise its priority
      end if;

      if T.Sched_Policy = SCHED_APP then
         --  Send event CHANGE_SCHED_PARAM to appscheduler
         SCHD.New_Sched_Param_In_AppSched_Task (T);
      end if;

      Scheduler.Do_Scheduling;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setschedprio;

   -------------------------
   -- Pthread_Setdeadline --
   -------------------------

   function Pthread_Setdeadline (T         : K.Task_Id;
                                 Deadline  : TSPC.Timespec_Ac;
                                 Immediate : Int) return Int is
      Flags : Integer;
   begin
      if not CP.Use_EDF_Scheduling_Policy'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);
      pragma Debug (DBG.Assert (Deadline /= null));
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      if T.Sched_Policy = SCHED_EDF then
         T.New_Deadline_For_Next_Activation := Immediate = 0;
         if Immediate = 1 then
            --  Change of deadline has an immediate effect on task's urgency

            T.Base_Urgency :=
              Task_Urgency (HAL.HWTime'Last
                            - TSPC.Timespec_To_HWTime (Deadline.all));
            T.Active_Urgency := T.Base_Urgency;
            --  Place task in its new place in its priority queue

            if T.Status = K.READY then
               SCHD.Ready_Task_Changes_Urgency (T);
               SCHD.Do_Scheduling;
            end if;

         else
            --  Just set deadline field in the TCB. It will be translated to a
            --  urgency value the next time the task is reactivated after
            --  having been suspended or blocked

            T.Deadline := TSPC.Timespec_To_HWTime (Deadline.all);
         end if;

      else  --  Not SCHED_EDF
         --  Just set deadline field in the TCB. It will be translated to
         --  a urgency value when the policy of the task change to SCHED_APP

         T.Deadline := TSPC.Timespec_To_HWTime (Deadline.all);
         T.New_Deadline_For_Next_Activation := False;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setdeadline;

   -------------------------
   -- Pthread_Getdeadline --
   -------------------------

   function Pthread_Getdeadline (T        : K.Task_Id;
                                 Deadline : TSPC.Timespec_Ac) return Int is
      Flags : Integer;
   begin
      if not CP.Use_EDF_Scheduling_Policy'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);
      pragma Debug (DBG.Assert (Deadline /= null));
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      Deadline.all := TSPC.HWTime_To_Timespec (HAL.HWTime'Last
                                               - HAL.HWTime (T.Base_Urgency));

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getdeadline;

   --------------------------------
   -- Pthread_Setpreemptionlevel --
   --------------------------------

   function Pthread_Setpreemptionlevel (T  : K.Task_Id;
                                        PL : K.Task_Preemption_Level)
                                        return Int is
         Flags : Integer;
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      T.Base_Preemption_Level := PL;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setpreemptionlevel;

   --------------------------------
   -- Pthread_Getpreemptionlevel --
   --------------------------------

   function Pthread_Getpreemptionlevel
     (T     : K.Task_Id;
      PL_Ac : access K.Task_Preemption_Level) return Int is
         Flags : Integer;
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);
      if not Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      PL_Ac.all := T.Base_Preemption_Level;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getpreemptionlevel;

   -------------------------------------------------------------------------
   --  Basic Operations  ---------------------------------------------------
   -------------------------------------------------------------------------

   --------------------
   -- Pthread_Create --
   --------------------
   function Pthread_Create (T         : access Task_Id;
                            Attr      : Pthread_Attr_T_Ac_Const;
                            Task_Body : Task_Body_Function;
                            Arg       : System.Address) return Int is
      Flags : Integer;
      Error : K.Error_Code;
   begin
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if Attr /= null and then not Internals.Attr_OK (Attr) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if (CP.Use_Application_Defined_Scheduling and then
          Attr /= null and then
          Attr.Sched_Policy = SCHED_APP) then
         if (not Task_OK (Attr.App_Scheduler) or else
             Attr.App_Scheduler.AppScheduler = null) then
            --  Invalid "appscheduler" attribute
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
         if Attr.Param.Sched_Priority > Attr.App_Scheduler.Base_Prio then
            --  Priority higher than its scheduler task
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
      end if;

      if Attr = null then
         T.all := Internals.Create_Task (Task_Body, Arg, Attr_Deffault);
      else
         Error := Error_In_Scheduling_Parameters (Attr.Param,
                                                  Attr.Sched_Policy);
         if Error /= NO_ERROR then
            K.Leave_Critic_Section (Flags);
            return Error;
         end if;
         if Attr.Inherit_Sched = PTHREAD_INHERIT_SCHED then
            --  Inherits father attributes
            T.all := Internals.Create_Task (Task_Body, Arg,
                                            Internals.Get_Self_Attributes);

         else
            T.all := Internals.Create_Task (Task_Body, Arg, Attr.all);
         end if;
      end if;

      --  Error handling
      if K.Tasks_Lists."=" (T.all, null) then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_TEMPORARILY_UNAVAILABLE;
      end if;

      Scheduler.New_Task_Created (T.all);
      Scheduler.Do_Scheduling;

      if (CP.Use_Application_Defined_Scheduling and then
          T.all.Sched_Policy = SCHED_APP) then
         --  Here continues the "father" task.

         pragma Assert
           (SCHD.Self.Internal_Error_Code   = NO_ERROR or
              SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED or
              SCHD.Self.Internal_Error_Code = APPSCHED_REJECTED);
         if SCHD.Self.Internal_Error_Code /= NO_ERROR then
            --  The appscheduler has the NEW_TASK event masked or
            --  created task has been rejected by its appscheduler: task
            --  cannot be created and its TCB is Released
            declare
               Error : K.Error_Code := SCHD.Self.Internal_Error_Code;
            begin
               SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
               T.all.Magic := TERMINATED;
               SI.Task_Stops_Getting_Signals (T.all);
               Pool_TCBs.Release_TCB (T.all);
               K.Leave_Critic_Section (Flags);
               return Error;
            end;
         end if;
      end if; -- SCHED_APP

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Create;

   ------------------
   -- Pthread_Exit --
   ------------------
   procedure Pthread_Exit (Value_Ptr : in System.Address) is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      Internals.Terminate_Running_Task (Value_Ptr);
      K.Leave_Critic_Section (Flags);
      --  This line should never be reached
      MaRTE.Direct_IO.Error ("#61#", Fatal => True);
   end Pthread_Exit;

   ------------------
   -- Pthread_Join --
   ------------------
   function Pthread_Join (T : Task_Id; Value_Ptr : System_Address_Ac)
                          return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T, TERMINATED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if T.Detach_State = PTHREAD_CREATE_DETACHED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if T = Self or else Self.Task_Waiting_For_Termination = TCB_Ac (T) then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_DEADLOCK_AVOIDED;
      end if;

      if T.Magic /= TERMINATED then
         --  Wait for the target task termination
         T.Task_Waiting_For_Termination := TCB_Ac (Self);
         Self.Task_Whose_Termination_Is_Waited := TCB_Ac (T);
         SCHD.Running_Task_Gets_Blocked (WAITING_TASK_TERMINATION);
         SCHD.Do_Scheduling;

         --  Reactivation
         Self.Task_Whose_Termination_Is_Waited := null;
         --  Check the reason for the task reactivation
         pragma Assert (SCHD.Self.Internal_Error_Code = NO_ERROR or
                          SCHD.Self.Internal_Error_Code = INVALID_ARGUMENT);
         if SCHD.Self.Internal_Error_Code = INVALID_ARGUMENT then
            --  Some other task has call 'Pthread_Detach' for the target
            --  task
            SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
      end if;

      --  The target task is terminated.
      pragma Debug (DBG.Assert (T.Magic = TERMINATED));
      if Value_Ptr /= null then
         Value_Ptr.all := T.Ret_Val;
      end if;
      Pool_TCBs.Release_TCB (T);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Join;

   --------------------
   -- Pthread_Detach --
   --------------------
   function Pthread_Detach (T : Task_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T, TERMINATED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if T.Detach_State = PTHREAD_CREATE_DETACHED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      T.Detach_State := PTHREAD_CREATE_DETACHED;

      --  Is there some task waiting for 'T' termination ?
      if T.Task_Waiting_For_Termination /= null then
         T.Task_Waiting_For_Termination.Task_Whose_Termination_Is_Waited :=
           null;
         --  Indicate error in the 'Internal_Notification' flag
         pragma Assert (T.Internal_Error_Code = NO_ERROR);
         pragma Assert
           (TCB_Ac_To_Task_Id
              (T.Task_Waiting_For_Termination).Internal_Error_Code = NO_ERROR);
         TCB_Ac_To_Task_Id
           (T.Task_Waiting_For_Termination).Internal_Error_Code :=
           INVALID_ARGUMENT;
         --  Put waiting task ready
         SCHD.Task_Gets_Ready
           (TCB_Ac_To_Task_Id (T.Task_Waiting_For_Termination));
         SCHD.Do_Scheduling;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Detach;

   -----------
   -- Yield --
   -----------
   function Sched_Yield return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      --  Reenqueue occuping the last position in its priority
      Scheduler.Running_Task_Yields_CPU;

      --  Call Scheduler
      Scheduler.Do_Scheduling;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Sched_Yield;

   -------------------------------------------------------------------------
   --  Enable/Disable round robin quantum  ---------------------------------
   -------------------------------------------------------------------------

   -----------------------------
   -- MaRTE_Enable_RR_Quantum --
   -----------------------------

   function MaRTE_Enable_RR_Quantum (T : Task_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T, TERMINATED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      T.RR_Quantum_Enabled := True;

      --  If the task is SCHED_RR and its quantum got exhausted while
      --  RR_Quantum_Enabled was reset, now could be the time to send the task
      --  to the tail of its priority queue

      if T.Sched_Policy = K.SCHED_RR
        and then T.End_Of_RR_Slice_Pending
        and then MLST.Is_Empty (T.Mutexes_Owned)
      then
         SCHD.Ready_Task_Reduces_Active_Priority (T,
                                                  T.Active_Prio,
                                                  Head_Of_New_Prio_Q => False);
         T.End_Of_RR_Slice_Pending := False;

         SCHD.Do_Scheduling;
      end if;

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Enable_RR_Quantum;

   ------------------------------
   -- MaRTE_Disable_RR_Quantum --
   ------------------------------

   function MaRTE_Disable_RR_Quantum (T : Task_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if not Task_OK (T, TERMINATED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      T.RR_Quantum_Enabled := False;

      K.Leave_Critic_Section (Flags);
      return 0;
   end MaRTE_Disable_RR_Quantum;

end MaRTE.Kernel.Tasks_Operations;
