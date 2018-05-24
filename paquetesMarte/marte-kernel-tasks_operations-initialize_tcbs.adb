------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T a s k s _ O p e r a t i o n s .
--                       I n i t i a l i z e _ T C B s'
--
--                                 Body
--
--
--  File 'k-to-initialize_TCBs.adb'                                   By MAR.
--
--
--  Initialize Task Control Blocks (TCBs) of tasks. Provides
--  procedures to initialize the special tasks: main, idle and
--  signal handler.
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

with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Kernel.Pool_TCBs;
with MaRTE.Kernel.Signals.Pool_SDBs;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.HAL;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Timers.Internals;

--  Debug
with MaRTE.Kernel.Tasks_Operations.Debug;

package body MaRTE.Kernel.Tasks_Operations.Initialize_TCBs is

   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package SDBs         renames K.Signals.Pool_SDBs;
   package TSTE         renames K.Task_Suspension_Timed_Events; use TSTE;
   package DBG          renames K.Tasks_Operations.Debug;
   package TMRI         renames K.Timers.Internals;

   function Task_Body_Function_To_Address is new Ada.Unchecked_Conversion
     (Task_Body_Function, System.Address);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize
     (T                 : in Task_Id;
      Id                : in Integer;
      Attr              : in Pthread_Attr_T;
      Task_Body         : in Task_Body_Function;
      Arg               : in System.Address;
      Task_Wrapper      : in System.Address) is
      use K.Tasks_Lists;
      use type K.Timed_Event_Ac;
      use type MaRTE.Stacks_Management.Stack_Id;
   begin
      T.Magic                   := K.ACTIVE;
      T.Id                      := Id;
      --  T.Task_Type is set in 'Pool_TCBs.Request_TCB'
      T.Status                  := BLOCKED;
      T.Kernel_Nesting_Level    := 0;
      T.Detach_State            := Attr.Detach_State;
      T.Task_Waiting_For_Termination     := null;
      T.Task_Whose_Termination_Is_Waited := null;
      T.Sched_Policy            := Attr.Sched_Policy;
      T.Active_Prio             := Attr.Param.Sched_Priority;
      T.Base_Prio               := Attr.Param.Sched_Priority;
      if T.AppScheduler /= null then
         --  Appschedulers has maximum urgency and preemption level to
         --  take precedence over its scheduled tasks.
         T.Active_Preemption_Level          := Task_Preemption_Level'Last;
         T.Base_Preemption_Level            := Task_Preemption_Level'Last;
         T.Active_Urgency                   := K.Task_Urgency'Last;
         T.Base_Urgency                     := K.Task_Urgency'Last;
      else
         --  Not an appscheduler task
         T.Active_Preemption_Level := Attr.Preemption_Level;
         T.Base_Preemption_Level   := Attr.Preemption_Level;
         T.Active_Urgency          := 0;
         T.Base_Urgency            := 0;
      end if;
      T.Rel_Deadline := Attr.Rel_Deadline;
      T.Deadline := HAL.HWTime'Last;
      T.New_Deadline_For_Next_Activation := False;
      T.Capacity  := 0;

      --  Set tag of T.Scheduler_Timed_Event. When not using
      --  prealocated resources the tag is not set since the TCB is created as
      --  a whole and the tag is only set for the TCB itself but not for any
      --  other tagged objects inside
      declare
         Dummy_Scheduler_Timed_Event : K.CPU_Time_Timed_Events_Lists.Element;
         pragma Warnings (Off);
         for Dummy_Scheduler_Timed_Event'Address
            use T.Scheduler_Timed_Event'Address;
         pragma Warnings (On);
      begin
         null;
      end;
      K.Timed_Event (T.Scheduler_Timed_Event) :=
        (T                       => 0);
      K.CPU_Time_Timed_Event (T.Scheduler_Timed_Event) :=
        (K.Timed_Events_Lists.Element (T.Scheduler_Timed_Event) with
         CPU_Time                => 0,
         Group_Expiration_Time   => 0,
         Is_Based_On_Group_Clock => False,
         Base_Clock              => TMRI.To_Clock_Id (T),
         Task_Where_Queued       => null,
         Armed                   => False);
      CPU_Time_Timed_Events_Lists.Initialize (T.CPU_Time_TEs_Q);
      T.Running_Starting_Time := 0;
      T.Used_CPU_Time         := 0;
      T.RR_Quantum_Enabled := True;
      T.End_Of_RR_Slice_Pending := False;
      T.Number_Of_Pending_Replenishments := 0;
      T.Capacity_Exhausted               := False;
      T.Sched_SS_Max_Repl                := 0;
      T.Sched_SS_Repl_Period             := 0;
      T.Sched_SS_High_Priority           := Task_Priority'First;
      T.Sched_SS_Low_Priority            := Task_Priority'First;
      T.Replenish_Amount_Pending         := 0;
      T.Task_Body               := Task_Body;
      T.Arg                     := Arg;
      T.Ret_Val                 := System.Null_Address;
      --  T.Stack is set in 'Pool_TCBs.Request_TCB'
      pragma Assert (T.Stack /= MaRTE.Stacks_Management.Null_Stack_Id);
      T.HBT_Mutex_Where_Blocked := null;
      T.CV_Where_Blocked        := null;
      T.Task_Waiting_Appscheduler_Response := null;
      if T.Suspension_Event /= null then
         T.Suspension_Event.T              := 0;
         TSTE.Task_Suspension_TE_Ac (T.Suspension_Event).Suspended_Task := T;
      end if;
      T.Timed_Waiting_Mutex := False;
      Mutexes_Lists.Initialize (T.Mutexes_Owned);
      T.Reenqueuement_Deferred  := False;
      T.Old_Active_Prio             := Attr.Param.Sched_Priority;
      T.Old_Active_Preemption_Level := Attr.Preemption_Level;
      T.Specific_Data := (others => (System.Null_Address, 0));
      T.Set := null;
      T.Error_Code  := NO_ERROR;
      T.POSIX_Error := False;
      T.Internal_Error_Code := NO_ERROR;

      --  Policy related parameters
      case Attr.Sched_Policy is
         when SCHED_FIFO | SCHED_OTHER | SCHED_EDF=>
            null;

         when SCHED_RR =>
            pragma Assert (CP.Use_Round_Robin_Scheduling_Policy);
            if CP.Use_Round_Robin_Scheduling_Policy then
               T.Capacity :=
                 HAL.Duration_To_HWTime
                 (Duration
                  (MaRTE.Configuration_Parameters.Round_Robin_Interval_Time_Period));
            end if;

         when SCHED_SPORADIC =>
            pragma Assert (CP.Use_Sporadic_Server_Scheduling_Policy);
            if CP.Use_Sporadic_Server_Scheduling_Policy then
               T.Sched_SS_High_Priority := Attr.Param.Sched_Priority;
               T.Sched_SS_Low_Priority  := Attr.Param.Sched_SS_Low_Priority;
               T.Sched_SS_Repl_Period   := MaRTE.Timespec.Timespec_To_HWTime
                 (Attr.Param.Sched_SS_Repl_Period);
               T.Capacity               := MaRTE.Timespec.Timespec_To_HWTime
                 (Attr.Param.Sched_SS_Init_Budget);
               T.Sched_SS_Max_Repl      := Attr.Param.Sched_SS_Max_Repl;
               T.Replenish_Amount_Pending         := 0;
               T.Number_Of_Pending_Replenishments := 0;
            end if;

         when SCHED_APP =>
            pragma Assert (CP.Use_Application_Defined_Scheduling and
                             T.Sched_App /= null);
            if CP.Use_Application_Defined_Scheduling then
               T.Magic         := K.NEW_APPSCHED;
               T.Status        := K.APPSCHED_SUSPENDED;
               APPSCHD_DATA.Sched_App_Data (T.Sched_App.all) :=
                 (Scheduler              => Attr.App_Scheduler,
                  Param                  => Attr.AppSched_Param,
                  Param_Size             => Attr.AppSched_Param_Size,
                  AppMutex_Where_Waiting =>
                    K.Mutexes.Null_Mutex_Descriptor,
                  Scheduler_Reply        => null,
                  Reply_Size             => 0,
                  Owner                  => T,
                  Notification_Event     => APPSCHD_DATA.Null_Notification_TE,
                  Activation_Event       => APPSCHD_DATA.Null_Activation_TE,
                  Suspend_After_Scheduler_Operation => True);
            end if;
      end case;

      --  Stack initialization
      if T.Task_Type /= K.MAIN_TASK then
         --  Configure the task stack for the first activation
         --
         --  Stack_Ptr -> --  --  --  --  --  --  --  --  --  --  --  --  --
         --                         Address of 'Task_Wrapper'
         --   4 bytes     (This will be used as return addres in the 'ret'
         --                     instruction of 'HWI.Context_Switch')
         --               --  --  --  --  --  --  --  --  --  --  --  --  --
         --                                      0
         --   4 bytes      (Fake return address for 'Task_Wrapper', it can
         --                   be 0 because 'Task_Wrapper' never returns)
         --               --  --  --  --  --  --  --  --  --  --  --  --  --
         --                            Address of 'Body'
         --   4 bytes            (Parameter of 'Task_Wrapper')
         --
         --  Stack_Base-> --  --  --  --  --  --  --  --  --  --  --  --  --
         MaRTE.Stacks_Management.Write_In_Stack (Addr => Task_Wrapper,
                                           P    => 3,
                                           S    => T.Stack);
         MaRTE.Stacks_Management.Write_In_Stack (Int => 0,
                                           P   => 2,
                                           S   => T.Stack);
         MaRTE.Stacks_Management.Write_In_Stack
           (Addr => Task_Body_Function_To_Address (Task_Body),
            P    => 1,
            S    => T.Stack);
         T.Stack_Ptr :=
           MaRTE.Stacks_Management.DWord_In_Stack_Address (P => 3, S => T.Stack);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_Main_Task --
   --------------------------
   function Initialize_Main_Task (Prio : in Task_Priority) return Task_Id is
      Main_TCB : Task_Id :=
        Pool_TCBs.Request_TCB (Task_Type    => MAIN_TASK,
                               Stack_Size   => CP.Main_Stack_Size_In_Bytes,
                               AppScheduler => False);
      use type K.Task_Id;
   begin
      pragma Assert (Main_TCB /= Null_Task_Id);
      pragma Debug (DBG.Initialize_Main_Task1 (Prio, SCHED_FIFO)); -- Trace

      --  Give value to fields
      Main_TCB.Stack_Ptr := 0;
      --  It'll get a valid value the first time the task leave the CPU

      Initialize
        (T         => Main_TCB,
         ID        => Pool_TCBs.Main_Task_Id,
         Attr => (Magic        => ATTR_INITIALIZED,
                  Sched_Policy => SCHED_FIFO,
                  Detach_State => PTHREAD_CREATE_DETACHED,
                  Inherit_Sched => PTHREAD_EXPLICIT_SCHED,
                  Param        =>
                    Sched_Param'(Sched_Priority      => Prio,
                                 Sched_SS_Low_Priority => Task_Priority'First,
                                 Sched_SS_Repl_Period  => (0, 0),
                                 Sched_SS_Init_Budget  => (0, 0),
                                 Sched_SS_Max_Repl     => 0),
                  Stack_Size   => CP.Main_Stack_Size_In_Bytes,
                  Rel_Deadline        => HAL.HWTime'Last,
                  Preemption_Level    => 0,
                  App_Scheduler       => null,
                  AppSched_Param      => (others => 0),
                  AppSched_Param_Size => 0,
                  App_Scheduler_State => PTHREAD_REGULAR),
         Task_Body         => null,
         Arg               => System.Null_Address,
         Task_Wrapper      => System.Null_Address);

      SDBs.Register_Used_SDB (Main_TCB);

      return Main_TCB;
   end Initialize_Main_Task;

   --------------------------
   -- Initialize_Idle_Task --
   --------------------------
   function Initialize_Idle_Task (Idle_Task_Body : in Task_Body_Function;
                                  Task_Wrapper   : in System.Address)
                                 return Task_Id  is
      Idle_TCB : Task_Id :=
        Pool_TCBs.Request_TCB (Task_Type    => IDLE_TASK,
                               Stack_Size   => CP.Idle_Stack_Size_In_Bytes,
                               AppScheduler => False);
      --  Pool_TCBs.Get_TCB_By_Number (Pool_TCBs.Idle_Task_Id);
      use type K.Task_Id;
   begin
      pragma Assert (Idle_TCB /= Null_Task_Id);
      pragma Debug
        (DBG.Initialize_Idle_Task1 (Pool_TCBs.Idle_Task_Id)); -- Trace

      --  TCB of the Idle Task inicialization
      Initialize
        (T                => Idle_TCB,
         ID               => Pool_TCBs.Idle_Task_Id,
         Attr => (Magic        => ATTR_INITIALIZED,
                  Sched_Policy => SCHED_FIFO,
                  Detach_State => PTHREAD_CREATE_DETACHED,
                  Inherit_Sched => PTHREAD_EXPLICIT_SCHED,
                  Param        =>
                    Sched_Param'(Sched_Priority => K.Task_Priority'First,
                                 Sched_SS_Low_Priority => Task_Priority'First,
                                 Sched_SS_Repl_Period  => (0, 0),
                                 Sched_SS_Init_Budget  => (0, 0),
                                 Sched_SS_Max_Repl     => 0),
                  Stack_Size   => CP.Idle_Stack_Size_In_Bytes,
                  Rel_Deadline        => HAL.HWTime'Last,
                  Preemption_Level    => 0,
                  App_Scheduler       => null,
                  AppSched_Param      => (others => 0),
                  AppSched_Param_Size => 0,
                  App_Scheduler_State => PTHREAD_REGULAR),
         Task_Body         => Idle_Task_Body,
         Arg               => System.Null_Address,
         Task_Wrapper      => Task_Wrapper);

      return Idle_TCB;
   end Initialize_Idle_Task;

   ------------------------------------
   -- Initialize_Signal_Handler_Task --
   ------------------------------------
   function Initialize_Signal_Handler_Task
     (Signal_Handler_Task_Body : in Task_Body_Function;
      Task_Wrapper             : in System.Address) return Task_Id is
      Handler_TCB : Task_Id := Pool_TCBs.Request_TCB
        (Task_Type    => SIGNAL_HANDLER_TASK,
         Stack_Size   => CP.Signal_Handler_Stack_Size_In_Bytes,
         AppScheduler => False);
      use type K.Task_Id;
   begin
      pragma Assert (Handler_TCB /= Null_Task_Id);
      --  TCB of the Signal Handler Task inicialization
      Initialize
        (T                => Handler_TCB,
         ID               => Pool_TCBs.Signal_Handler_Task_Id,
         Attr => (Magic         => ATTR_INITIALIZED,
                  Sched_Policy  => SCHED_FIFO,
                  Detach_State  => PTHREAD_CREATE_DETACHED,
                  Inherit_Sched => PTHREAD_EXPLICIT_SCHED,
                  Param        =>
                    Sched_Param'(Sched_Priority => K.Task_Priority'Last,
                                 Sched_SS_Low_Priority => Task_Priority'First,
                                 Sched_SS_Repl_Period  => (0, 0),
                                 Sched_SS_Init_Budget  => (0, 0),
                                 Sched_SS_Max_Repl     => 0),
                  Stack_Size          => CP.Signal_Handler_Stack_Size_In_Bytes,
                  Rel_Deadline        => HAL.HWTime'Last,
                  Preemption_Level    => 0,
                  App_Scheduler       => null,
                  AppSched_Param      => (others => 0),
                  AppSched_Param_Size => 0,
                  App_Scheduler_State => PTHREAD_REGULAR),
         Task_Body         => Signal_Handler_Task_Body,
         Arg               => System.Null_Address,
         Task_Wrapper      => Task_Wrapper);

      return Handler_TCB;
   end Initialize_Signal_Handler_Task;

end MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
