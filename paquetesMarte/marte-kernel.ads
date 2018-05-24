------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'K e r n e l'
--
--                                   Spec
--
--
--  File 'kernel.ads'                                                  By MAR.
--
--
--  Declaration of general types such as 'Task_Priority', 'Task_ID',
--  'TCB' and 'Timed_Event'.
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
with System; --  for 'Address'
with Ada.Unchecked_Conversion;

with MaRTE.POSIX_Constants;
with MaRTE.HAL; --  for 'HWTime' and 'HW_Interrupt'
pragma Elaborate_All (MaRTE.HAL);
with MaRTE.Configuration_Parameters;
with MaRTE.Stacks_Management;
with MaRTE.SLL;
pragma Elaborate_All (MaRTE.SLL);
with MaRTE.SLL.Advanced;
pragma Elaborate_All (MaRTE.SLL.Advanced);
with MaRTE.SLL.Order;
pragma Elaborate_All (MaRTE.SLL.Order);
with MaRTE.SLL.Map;
pragma Elaborate_All (MaRTE.SLL.Map);
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.Kernel is

   use MaRTE;

   package CP  renames MaRTE.Configuration_Parameters;
   package HAL renames MaRTE.HAL;

   -----------------------------
   -- Suspension_Time_Minimum --
   -----------------------------
   --  Under this limit a suspension operation isn't performed, in
   --  particular when a task tries to suspend itself for an amount of
   --  time smaller than this limit a 'yield' operation is performed
   --  instead.
   --  They are logical constants initialized in
   --  Kernel.Initialization.Initialize
   Suspension_Time_Minimum     : MaRTE.HAL.HWTime;
   Suspension_Time_Minimum_Seg : Duration;

   ------------------------
   -- Task Priority type --
   ------------------------
   type Task_Priority is new Integer
     range 0 .. MaRTE.Configuration_Parameters.Task_Priority_Mx;
   for Task_Priority'Size use Int'Size;

   --------------------------------
   -- Task Preemption Level type --
   --------------------------------
   type Task_Preemption_Level is new MaRTE.Integer_Types.Unsigned_16;
   for Task_Preemption_Level'Size use MaRTE.Integer_Types.Unsigned_16'Size;

   -----------------------
   -- Task Urgency type --
   -----------------------
   type Task_Urgency is new MaRTE.Integer_Types.Unsigned_64;
   for Task_Urgency'Size use MaRTE.Integer_Types.Unsigned_64'Size;

   ---------------------
   -- Critic Sections --
   ---------------------
   procedure Enter_Critic_Section (Flags : out Integer);
   pragma Export (C, Enter_Critic_Section, "kernel_enter_critic_section");
   pragma Export_Procedure (Enter_Critic_Section,
                            Parameter_Types => (Integer),
                            Mechanism       => (Reference));
   --  Exported to C programs by <sys/kernel.h>

   procedure Enter_Critic_Section_From_Interrupt;
   --  To be called from inside a kernel interrupt handler. Doesn't
   --  touch the EFlags register, only sets to one the
   --  'Kernel_Nesting_Level' counter.
   pragma Inline (Enter_Critic_Section_From_Interrupt);

   procedure Leave_Critic_Section (Flags : in Integer);
   pragma Export (C, Leave_Critic_Section, "kernel_leave_critic_section");
   pragma Export_Procedure (Leave_Critic_Section,
                            Parameter_Types => (Integer),
                            Mechanism       => (Value));
   --  Exported to C programs by <sys/kernel.h>

   procedure Leave_Critic_Section_From_Interrupt;
   --  To be called from inside a kernel interrupt handler. Doesn't
   --  touch the EFlags register, only sets to zero the
   --  'Kernel_Nesting_Level' counter.
   pragma Inline (Leave_Critic_Section_From_Interrupt);

   -----------------------------------
   -- Type 'AppScheduler_Data_Base' --
   -----------------------------------
   --
   --  Defined here for visibility reasons. The 'AppScheduler_Data' type is
   --  defined in 'Kernel.Application_Scheduling_Data'.
   type AppScheduler_Data_Base is abstract tagged null record;
   type AppScheduler_Data_Base_Ac is access all AppScheduler_Data_Base'Class;

   --------------------------------
   -- Type 'Sched_App_Data_Base' --
   --------------------------------
   --
   --  Defined here for visibility reasons. The 'Sched_App_Data' type is
   --  defined in 'Kernel.Application_Scheduling_Data'.
   type Sched_App_Data_Base is abstract tagged null record;
   type Sched_App_Data_Base_Ac is access all Sched_App_Data_Base'Class;

   -----------------------
   -- Type 'Mutex_Base' --
   -----------------------
   --
   --  Defined here for visibility reasons.
   type Mutex_Base is tagged null record;
   package Mutexes_Lists is new MaRTE.SLL (Mutex_Base);
   subtype Mutex_Base_Ac is Mutexes_Lists.Element_Ac;

   ---------------------------
   -- Type 'Condition_Base' --
   ---------------------------
   --
   --  Defined here for visibility reasons.
   type Condition_Base is abstract tagged null record;
   type Condition_Base_Ac is access all Condition_Base'Class;

   --------------------------
   -- Type 'Sig_Data_Base' --
   --------------------------
   --
   --  Defined here for visibility reasons.
   type Sig_Data_Base is abstract tagged null record;
   type Sig_Data_Base_Ac is access all Sig_Data_Base'Class;

   ------------------------------
   -- Type 'Semaphore_Base_Ac' --
   ------------------------------
   --
   --  Defined here for visibility reasons.
   type Semaphore_Base_Ac is new System.Address;

   ----------------
   --  Clock_Id  --
   ----------------
   type Clock_Id is new MaRTE.Integer_Types.Unsigned_32;

   -----------------------------------------------------
   -- Types 'Intr_T' and 'Interrupt_Handler_Function' --
   -----------------------------------------------------
   --
   --  It is necessary to define then here because 'TCB' includes
   --  fields of these types.
   subtype Intr_T is MaRTE.HAL.HW_Interrupt;
   type Handler_Return_Code is new Int;
   type Interrupt_Handler_Function is access
     function (Area : in System.Address; Intr : in Intr_T)
              return Handler_Return_Code;

   ------------------------------------------
   --  Types 'Task_Set_Base' and 'TCB_Ac'  --
   ------------------------------------------
   type Task_Set_Base;
   type Task_Set_Base_Ac is access all Task_Set_Base'Class;
   type TCB;
   type TCB_Ac is access all TCB'Class;

   ------------------------
   -- Type 'Timed_Event' --
   ------------------------
   type Timed_Event is tagged record
      T : MaRTE.HAL.HWTime;
   end record;

   package Timed_Events_Lists is new MaRTE.SLL (Timed_Event);

   subtype Timed_Event_Ac is Timed_Events_Lists.Element_Ac;

   ---------------------------------
   -- Type 'CPU_Time_Timed_Event' --
   ---------------------------------
   type CPU_Time_Timed_Event is new Timed_Events_Lists.Element with record
      CPU_Time : MaRTE.HAL.HWTime := 0;
      --  The event will expire when the execution time consumed by the
      --  associated task (task where this event is queued) reaches this
      --  value

      Group_Expiration_Time : MaRTE.HAL.HWTime;
      --  Only used in events based on a "group clock". The even will expire
      --  when field Consumed_Time of the task set associated with the event
      --  reaches this value

      Is_Based_On_Group_Clock : Boolean;
      --  Used to identify CPU_Time_Timed_Events based on group clocks

      Base_Clock : Clock_Id;
      --  Clock used as the timing base of the event (it could be a thread
      --  CPU-time clock or a group clock)

      Task_Where_Queued : TCB_Ac;
      --  Pointer to the task that has the event queued in its CPU_Time_TEs_Q
      --  list

      Armed : Boolean;
      --  The event is armed (active). This usualy means it is queued in some
      --  task but not always: an group event (timed handler or timer) created
      --  from a task that doesn't belong to the group is armed but not queued
      --  until some task in the group gets the CPU
   end record;
   package CPU_Time_Timed_Events_Lists is
     new MaRTE.SLL (CPU_Time_Timed_Event);

   subtype CPU_Time_Timed_Event_Ac is CPU_Time_Timed_Events_Lists.Element_Ac;

   function ">" (Left, Right : CPU_Time_Timed_Events_Lists.Element_Ac)
                 return Boolean;
   pragma Inline (">");
   package CPU_Time_Timed_Events_Lists_Order is
      new CPU_Time_Timed_Events_Lists.Order (">");

   function CPU_TE_OK (CPU_TE_Ac : CPU_Time_Timed_Event_Ac) return Boolean;
   pragma Inline (CPU_TE_OK);

   ---------------------------------------------------------------------------
   -- Type 'TCB' -------------------------------------------------------------
   ---------------------------------------------------------------------------

   ----------------
   -- Task_Types --
   ----------------
   type Task_Types is (MAIN_TASK, IDLE_TASK, USER_TASK, SIGNAL_HANDLER_TASK);

   -------------------------
   -- Scheduling_Policies --
   -------------------------
   subtype Scheduling_Policies is Int
     range MaRTE.POSIX_Constants.SCHED_FIFO .. MaRTE.POSIX_Constants.SCHED_EDF;

   SCHED_FIFO     : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_FIFO;
   SCHED_RR       : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_RR;
   SCHED_SPORADIC : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_SPORADIC;
   SCHED_OTHER    : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_OTHER;
   SCHED_APP      : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_APP;
   SCHED_EDF      : constant Scheduling_Policies :=
     MaRTE.POSIX_Constants.SCHED_EDF;

   -------------------
   -- Detachstate_T --
   -------------------
   subtype Detach_State_T is Int
     range MaRTE.POSIX_Constants.PTHREAD_CREATE_JOINABLE ..
           MaRTE.POSIX_Constants.PTHREAD_CREATE_DETACHED;
   PTHREAD_CREATE_JOINABLE : constant Detach_State_T :=
     MaRTE.POSIX_Constants.PTHREAD_CREATE_JOINABLE;
   PTHREAD_CREATE_DETACHED : constant Detach_State_T :=
     MaRTE.POSIX_Constants.PTHREAD_CREATE_DETACHED;

   -----------------
   -- Task_Status --
   -----------------
   type Task_Status is
      (READY,
       TIME_SUSPENDED,
       BLOCKED,
       WAITING_POLICY_CHANGE_TO_SCHED_APP,
       WAITING_APPSCHEDULER_RESPONSE,
       TIMED_WAITING_CV,
       TIMED_WAITING_MUTEX,
       TIMED_WAITING_SEM,
       WAITING_SIGNAL,
       WAITING_TASK_TERMINATION,
       WAITING_HW_INTERRUPT,
       TIMED_WAITING_HW_INTERRUPT,
       SIG_HANDLER_WAIT_TO_DELIVER,  -- Only for the signal handler task
       WAITING_APPSCHED_EVENT,       -- Only for application scheduler tasks
       TIMED_WAITING_APPSCHED_EVENT, -- Only for application scheduler tasks
       APPSCHED_SUSPENDED,             -- Only for application scheduled tasks
       WAITING_TO_LOCK_APPSCHED_MUTEX, -- Only for application scheduled tasks
       TRYING_TO_LOCK_APPSCHED_MUTEX); -- Only for application scheduled tasks

   ------------------------
   -- Task_Body_Function --
   ------------------------
   type Task_Body_Function is access
     function (Arg : System.Address) return System.Address;

   -----------------------------------------------
   -- Application-defined scheduling parameters --
   -----------------------------------------------
   type AppSched_Param_Size_T is range 0 .. CP.AppSched_Param_Bytes_Size_Mx;
   for AppSched_Param_Size_T'Size use MaRTE.Integer_Types.Int'Size;
   type AppSched_Param_T is array (AppSched_Param_Size_T
                                   range 1 .. AppSched_Param_Size_T'Last)
     of MaRTE.Integer_Types.Unsigned_8;
   pragma Pack (AppSched_Param_T);
   pragma Convention (C, AppSched_Param_T);
   type AppSched_Param_T_Ac is access all AppSched_Param_T;

   ----------------
   -- Error_Code --
   ----------------
   subtype Error_Code is POSIX_Constants.Error_Code;

   NO_ERROR                         : constant Error_Code :=
     POSIX_Constants.NO_ERROR;
   RESOURCE_TEMPORARILY_UNAVAILABLE : constant Error_Code :=
     POSIX_Constants.RESOURCE_TEMPORARILY_UNAVAILABLE;
   RESOURCE_BUSY                    : constant Error_Code :=
     POSIX_Constants.RESOURCE_BUSY;
   RESOURCE_DEADLOCK_AVOIDED        : constant Error_Code :=
     POSIX_Constants.RESOURCE_DEADLOCK_AVOIDED;
   INTERRUPTED_OPERATION            : constant Error_Code :=
     POSIX_Constants.INTERRUPTED_OPERATION;
   INVALID_ARGUMENT                 : constant Error_Code :=
     POSIX_Constants.INVALID_ARGUMENT;
   NO_SUCH_PROCESS                  : constant Error_Code :=
     POSIX_Constants.NO_SUCH_PROCESS;
   OPERATION_NOT_PERMITTED          : constant Error_Code :=
     POSIX_Constants.OPERATION_NOT_PERMITTED;
   OPERATION_NOT_SUPPORTED          : constant Error_Code :=
     POSIX_Constants.OPERATION_NOT_SUPPORTED;
   OPERATION_NOT_IMPLEMENTED        : constant Error_Code :=
     POSIX_Constants.OPERATION_NOT_IMPLEMENTED;
   TIMED_OUT                        : constant Error_Code :=
     POSIX_Constants.TIMED_OUT;
   NOT_ENOUGH_SPACE                 : constant Error_Code :=
     POSIX_Constants.NOT_ENOUGH_SPACE;
   APPSCHED_EVENT_MASKED            : constant Error_Code :=
     POSIX_Constants.APPSCHED_EVENT_MASKED;
   APPSCHED_REJECTED                : constant Error_Code :=
     POSIX_Constants.APPSCHED_REJECTED;
   APPSCHED_POLICY_ERROR            : constant Error_Code :=
     POSIX_Constants.APPSCHED_POLICY_ERROR;
   TOO_MANY_OPEN_FILES              : constant Error_Code :=
     POSIX_Constants.TOO_MANY_OPEN_FILES;
   FILENAME_TOO_LONG                : constant Error_Code :=
     POSIX_Constants.FILENAME_TOO_LONG;
   BAD_FILE_DESCRIPTOR              : constant Error_Code :=
     POSIX_Constants.BAD_FILE_DESCRIPTOR;
   NO_SUCH_FILE_OR_DIRECTORY        : constant Error_Code :=
     POSIX_Constants.NO_SUCH_FILE_OR_DIRECTORY;
   PERMISSION_DENIED                : constant Error_Code :=
     POSIX_Constants.PERMISSION_DENIED;
   NO_ISR_ASSOCIATED                : constant Error_Code :=
     POSIX_Constants.NO_ISR_ASSOCIATED;

   ------------------------
   -- Task-Specific Data --
   ------------------------
   type Specific_Data_Element is record
      Data : System.Address;
      Use_Count : Natural;
   end record;
   type Specific_Data_T is array (CP.Keys_Range) of Specific_Data_Element;

   ------------------------
   -- TCB's Magic values --
   ------------------------
   --
   --  TCB life cycle states
   --  NOT_USED: free TCB not used by any task
   --  NEW_APPSCHED: temporal state of a new task that requires being
   --                scheduled by an application scheduler.
   --  ACTIVE: used by an active task
   --  TERMINATED: temporal state of a task which is going to be removed
   --              from the system and its TCB freed
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   NOT_USED     : constant Magic_Check := ('T', 'N', '_', 'U');
   NEW_APPSCHED : constant Magic_Check := ('T', 'N', 'A', 'P');
   ACTIVE       : constant Magic_Check := ('T', 'A', 'C', 'T');
   TERMINATED   : constant Magic_Check := ('T', 'T', 'E', 'R');

   -----------------------------------
   -- Type TCB (Task Control Block) --
   -----------------------------------
   type TCB is tagged record
      Stack_Ptr : MaRTE.Integer_Types.Unsigned_32 := 0;
      --  Top of the stack. The top of the stack is stored here while the
      --  task isn't running.

      Magic : Magic_Check := NOT_USED;
      --  To detect invalid TCBs when debugging. It is changed in
      --  the functions 'Create_Task', 'Initialize_Main_Task',
      --  'Initialize_Idle_Task', 'Task_Exit' and 'Remove_Task' of
      --  'Tasks_Operations.Internals'.

      Id : Integer := 0;
      --  Task identification number (different for each task)

      Task_Type : Task_Types := USER_TASK;
      --  Task type. It is changed in
      --  'Tasks_Operations.Internals.Create_Task'.

      Status : Task_Status := BLOCKED;
      --  The state of the task

      Kernel_Nesting_Level : Natural;
      --  Stores the Kernel nesting level count for the task when it
      --  is NOT running (for the running task
      --  'Kernel.Kernel_Nesting_Level' is used). 0 when the task
      --  wasn't executing inside MaRTE OS kernel. It can have a value
      --  greater than 1 since calls to MaRTE OS can be nested in some
      --  (few) cases.

      Detach_State : Detach_State_T := PTHREAD_CREATE_JOINABLE;
      --  Detach state of the task (joinable or detached).

      Task_Waiting_For_Termination : TCB_Ac := null;
      --  The task which called 'Pthread_Join' to wait for the termination
      --  of this task.

      Task_Whose_Termination_Is_Waited : TCB_Ac := null;
      --  When this task has called 'Pthread_Join' the waited task is
      --  pointed by this.

      Sched_Policy : Scheduling_Policies := SCHED_FIFO;
      --  Scheduling policy of the task

      Active_Prio : Task_Priority := 0;
      --  The active (efective) priority of the task

      Base_Prio : Task_Priority := 0;
      --  The base priority of the task

      Active_Preemption_Level : Task_Preemption_Level := 0;
      --  Active (inherited) "Preemption Level". Used with SRP mutexes.

      Base_Preemption_Level : Task_Preemption_Level := 0;
      --  The task "Preemption Level". Used with SRP mutexes.

      Active_Urgency : Task_Urgency := 0;
      --  Active task urgency. This value can be different from
      --  'Base_Urgency' when using generalized inheritance mutexes
      --  (not implemented yet).

      Base_Urgency : Task_Urgency := 0;
      --  Task urgency. Used with the application defined scheduling
      --  to order task inside a priority queue.
      Rel_Deadline : HAL.HWTime;
      --  Relative deadline of the task. Used for EDF tasks. Is only relevant
      --  when the task is about to be created. Once the task is created
      --  Task_Urgency is used to represent task deadline.

      Deadline : HAL.HWTime;
      --  Absolute deadline of the task. Used to store the deadline of a
      --  non-EDF task prior to change to that policy. Set by
      --  TO.Pthread_Setdeadline.

      New_Deadline_For_Next_Activation : Boolean;
      --  Used by TO.Pthread_Setdeadline in order to mark a deadline change
      --  has to be preformed in the next activation of the task

      Capacity : MaRTE.HAL.HWTime := 0;
      --  In round-robin tasks this parameter takes the value of the RR
      --  interval period. In sporadic server tasks is the initial budget.

      Scheduler_Timed_Event : aliased CPU_Time_Timed_Events_Lists.Element;
      --  If the task has the round-robin or sporadic server policies this
      --  timed event is enqueued in 'CPU_Time_TEs_Q' programmed to expire
      --  at the end of its RR interval or execution capacity.

      CPU_Time_TEs_Q : CPU_Time_Timed_Events_Lists.List;
      --  Task's CPU-time timed events ordered queue. The task's active
      --  CPU-timers and, if active, the task's 'Scheduler_Timed_Event' are
      --  enqueued here.

      Running_Starting_Time : MaRTE.HAL.HWTime := 0;
      --  The last time at which the task has reached the "runnig" status.

      Used_CPU_Time : MaRTE.HAL.HWTime := 0;
      --  Total amount of CPU time used for the task since its creation to
      --  the last time it got the CPU.

      RR_Quantum_Enabled : Boolean;
      --  While this flag is reset an expiration of the round robin quantum
      --  won't cause the re-queuing of the task but only
      --  End_Of_RR_Slice_Pending will be set. The re-queuing will be delayed
      --  until this flag is set again and the task doesn't own any mutex.

      End_Of_RR_Slice_Pending : Boolean;
      --  The quantum of a SCHED_RR task has got exhausted while holding a
      --  mutex or RR_Quantum_Enabled is reset. The action of sending the task
      --  to the tail of its priority queue is delayed until it unlocks its
      --  last mutex and RR_Quantum_Enabled is set.

      Number_Of_Pending_Replenishments : Natural := 0;
      --  Number of pending replenishment operations for a 'SCHED_SPORADIC'
      --  task.

      Capacity_Exhausted : Boolean := False;
      --  Indicates if the capacity of a 'SCHED_SPORADIC' task is exhausted.

      Sched_SS_Max_Repl : Natural := 0;
      --  Maximum number of pending replenishment operations for a
      --  'SCHED_SPORADIC' task.

      Sched_SS_Repl_Period  : MaRTE.HAL.HWTime := 0;
      --  Replenishment period for 'SCHED_SPORADIC' tasks.

      Sched_SS_High_Priority : Task_Priority := Task_Priority'First;
      --  Priority at which a 'SCHED_SPORADIC' task executes when its
      --  available execution capacity is greater than 0.

      Sched_SS_Low_Priority : Task_Priority := Task_Priority'First;
      --  Priority at which a 'SCHED_SPORADIC' task executes when its
      --  available execution capacity is 0.

      Activation_Time : MaRTE.HAL.HWTime := 0;
      --  Last time a 'SCHED_SPORADIC' task became ready at high priority.

      Replenish_Amount_Pending : MaRTE.HAL.HWTime := 0;
      --  Sum of all the replenish amount in pending replenishment
      --  operations for a 'SCHED_SPORADIC' task.

      Sched_App : Sched_App_Data_Base_Ac := null;
      --  Specific data asociated with tasks of SCHED_APP policy.

      AppScheduler : AppScheduler_Data_Base_Ac := null;
      --  Data used for application scheduler task to manage its associated
      --  task, scheduling events and in general all the functionality
      --  related with an application-defined scheduler.

      Task_Waiting_Appscheduler_Response : TCB_Ac := null;
      --  Identifies the task that is creating this task with SCHED_APP
      --  policy. Also used to point the task that is changing the policy
      --  of this TCB to SCHED_APP.

      Task_Body : Task_Body_Function := null;
      --  Body (code) of the task

      Arg : System.Address;
      --  Argument 'Arg' of 'Create_Task'

      Ret_Val : System.Address;
      --  Return value of the thread. It can be consulted from another
      --  thread using 'pthread_join'.

      Stack : MaRTE.Stacks_Management.Stack_Id;
      --  Stack of the task. Size can be got using
      --  Stacks_Management.Get_Stack_Size_In_Bytes (Stack)

      HBT_Mutex_Where_Blocked : Mutexes_Lists.Element_Ac;
      --  Mutex with 'HIGHEST_BLOCKED_TASK' policy where the task is
      --  blocked. A change in the priority of the task could supose a
      --  change in the priority of this mutex.

      CV_Where_Blocked : Condition_Base_Ac;
      --  Condition variable where the task is blocked. A change in
      --  the priority of the task forces a change in its position
      --  in the CV's blocked tasks queue.

      Suspension_Event : Timed_Event_Ac;
      --  Used when the task is temporaly suspended in a 'sleep' or a timed
      --  operation like 'pthread_cond_timedwait'.

      Timed_Waiting_Mutex : Boolean;
      --  True when a task has called 'pthread_mutex_timedlock'

      Sig_Data : aliased Sig_Data_Base_Ac;
      --  Signals management related data.

      Mutexes_Owned : Mutexes_Lists.List;
      --  List of mutexes owned by the task

      Reenqueuement_Deferred : Boolean := False;
      --  To avoid innecesary reenqueuements when the priority of the tasks
      --  has to be raised when looking a mutex.

      Old_Active_Prio : Task_Priority := 0;
      --  Used with 'Reenqueuement_Deferred'.

      Old_Active_Preemption_Level : Task_Preemption_Level := 0;
      --  Used with 'Reenqueuement_Deferred'.

      Specific_Data : Specific_Data_T;
      --  To implement the "Thread-Specific Data" (Section 17 POSIX.1).

      Last_Deliver_Intr : Intr_T;
      --  Last hardware interrupt delivered to this task

      Last_Executed_Intr_Handler : Interrupt_Handler_Function;
      --  Last interrupt handler connected to this task that has been
      --  executed

      Set : Task_Set_Base_Ac;
      --  A task can belong to a task set

      Error_Code : MaRTE.Kernel.Error_Code := NO_ERROR;
      --  Error code associated with the last "POSIX error".

      POSIX_Error : Boolean := False;
      --  Indicates if there has been a POSIX error in the last operation
      --  performed by the task, if True the associated error code is the
      --  one in 'Error_Code'. Also used after a context swicht: it allows
      --  the task to know the reason of its wakeup ('CV.Timed_Wait').

      Internal_Error_Code : MaRTE.Kernel.Error_Code := NO_ERROR;
      --  Used to notify the reason a task is reactivated (for example
      --  after a timeout when waiting for a hardware interrupt).

   end record;

   -------------
   -- Task_OK --
   -------------
   function Task_OK (T : TCB) return Boolean;
   pragma Inline (Task_OK);

   function Task_OK (T : TCB_Ac) return Boolean;
   pragma Inline (Task_OK);

   -----------------------
   -- Basic Tasks Lists --
   -----------------------
   package Tasks_Lists is new MaRTE.SLL (TCB);
   package Tasks_Lists_Advanced is new Tasks_Lists.Advanced;

   function ">" (Left, Right : Tasks_Lists.Element_Ac) return Boolean;
   pragma Inline (">");
   function Get_Active_Prio (T : in Tasks_Lists.Element_Ac)
                             return Task_Priority;
   pragma Inline (Get_Active_Prio);

   ---------------------
   -- Tasks Map Lists --
   ---------------------
   --
   --  Array of ordered singly linked lists, one for each priority level,
   --  among with a map of bits that indicates whether there is an active
   --  task at any given priority. Used for the ready queue and for the
   --  "blocked task list" in a CV.
   package Tasks_Map_Lists is
     new Tasks_Lists.Map (Priority     => Task_Priority,
                          ">"          => ">",
                          Get_Priority => Get_Active_Prio);

   ------------------------------
   -- Type 'Task_Id' and 'TCB' --
   ------------------------------
   subtype Task_Id is Tasks_Lists.Element_Ac;
   Null_Task_Id : constant Task_Id := null;
   function TCB_Ac_To_Task_Id is
     new Ada.Unchecked_Conversion (TCB_Ac, Task_Id);

   -------------
   -- Task_OK --
   -------------
   function Task_OK (T : Task_Id) return Boolean;
   pragma Inline (Task_OK);

   function Task_Terminated (T : Task_Id) return Boolean;
   pragma Inline (Task_Terminated);

   function Task_OK (T           : Task_Id;
                     Extra_Magic : Magic_Check) return Boolean;
   pragma Inline (Task_OK);

   ----------------------------------------------------------------------------
   -- Type 'Task_Container' ---------------------------------------------------
   ----------------------------------------------------------------------------
   type Task_Container_Base is tagged record
      T : Task_Id;
   end record;
   package Task_Containers_Lists is
      new MaRTE.SLL (Task_Container_Base);
   subtype Task_Container_Ac is Task_Containers_Lists.Element_Ac;

   function ">" (Left, Right : in Task_Container_Ac) return Boolean;
   package Task_Containers_Lists_Order is
      new Task_Containers_Lists.Order (">");

   ----------------------------------------------------------------------------
   --  Type 'Task_Set'  -------------------------------------------------------
   ----------------------------------------------------------------------------
   --  Set of task. Can be used to create a gruop execution time clock

   --  Magic values
   --  To detect invalid 'Task_Set'. It is an array of characters for easier
   --  reading when debugging.
   type Magic_Check_TS is array (1 .. 4) of Character;
   pragma Pack (Magic_Check_TS);
   TS_NOT_INITIALIZED : constant Magic_Check_TS := ('t', 's', 'N', 'i');
   TS_INITIALIZED     : constant Magic_Check_TS := ('T', 'S', '_', 'I');

   type Task_Set_Base is tagged record
      Set : MaRTE.Kernel.Task_Containers_Lists.List;
      --  List of task in the set

      Consumed_Time : MaRTE.HAL.HWTime;
      --  CPU-time consumed for all the task in the group

      Group_TE_Ac : CPU_Time_Timed_Event_Ac;
      --  A Task_Set can be associated with one (and only one)
      --  CPU_Time_Timed_Event

      Iterator : Task_Container_Ac;
      --  Used by Marte_Thread_Set_First and Marte_Thread_Set_Next

      Magic : Magic_Check_TS;
      --  Allows detecting invalid objets
   end record;
   package Task_Sets_Lists is
        new MaRTE.SLL (Task_Set_Base);
   subtype Task_Set_Id is Task_Sets_Lists.Element_Ac;

   ----------------------------------------------------------------------------
   --  Kernel Nesting Level  --------------------------------------------------
   ----------------------------------------------------------------------------
   Kernel_Nesting_Level : Natural;
   --  Kernel nesting level count for the running task. 0 when the
   --  running task is not executing inside MaRTE OS kernel. It can
   --  have a value greater than 1 since calls to MaRTE OS can be
   --  nested in some (few) cases.
   --
   --  Used to avoid disabling interrupts twice when entering to
   --  nested critic sections or re-enabling interrupts too early when
   --  leaving internal nested calls to the kernel.
   --
   --  Initialized to 0 in 'Kernel.Scheduler'.

end MaRTE.Kernel;
