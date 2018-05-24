------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'C o n f i g u r a t i o n _ P a r a m e t e r s'
--
--                                  Spec
--
--
--  File 'configuration_parameters.ads'                         By MAR. and
--                                                                 Fguerreira
--
--
--  This is the x86 architecture version of this package.
--
--
--
--  Kernel configuration parameters.
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

package MaRTE.Configuration_Parameters is
   pragma Pure;

   ----------------------------------------------------------------------------
   --  Architecture  ----------------------------------------------------------
   ----------------------------------------------------------------------------
   type Supported_Architectures is (ARCH_X86, ARCH_LINUX, ARCH_LINUX_LIB);
   subtype MaRTE_Architecture is Supported_Architectures
     range ARCH_X86 .. ARCH_X86; --  DO NOT EDIT !!
   --  To change architecture run 'minstall'.

   ----------------------------------------------------------------------------
   --  Preallocated Resources  ------------------------------------------------
   ----------------------------------------------------------------------------
   subtype Preallocated_Resources is Boolean range True .. True;
   --  If set to "True", resources are preallocated before starting
   --  application execution. The maximum number of every resource can
   --  be set modifying the value of constants in this package. Those
   --  constants are labeled as "{PREALLOCATED}".
   --
   --  If set to "False", resources are created in dynamic memory, so
   --  its maximum number depends on the size of the dynamic memory
   --  pool and not on the value of the constants labeled as
   --  "{PREALLOCATED}" in this package. The value of those constants
   --  can be set to 0 to save memory usage (notice that the default
   --  value of most of these constants depends on
   --  'Num_User_Tasks_Mx', so setting this constant to 0 is enough to
   --  reduce the memory usage).

   ----------------------------------------------------------------------------
   --  Tasks and Priorities ---------------------------------------------------
   ----------------------------------------------------------------------------
   Num_User_Tasks_Mx : constant := 20;  --  {PREALLOCATED}
   --  Maximum number of user's tasks or threads that can exist at the same
   --  time (including the main).
   --
   --  Can be set to 0 to reduce memory usage when
   --  'Preallocated_Resources' is set to "False".
   --
   --  For each Ada task, GNAT uses about 12Kb of dynamic memory, so
   --  for a large number of Ada tasks the value assigned to constant
   --  'Dynamic_Memory_Pool_Size_In_Bytes' in this file should be
   --  increased as well.
   --
   --  Appart from these tasks, it will be other two (system) tasks:
   --  the idle and the signal handler task.

   Num_MaRTE_Tasks_Mx : constant := Num_User_Tasks_Mx + 2;  --  {PREALLOCATED}
   --  Used internally by MaRTE kernel.
   --
   --  Only two diferent values are allowed for this parameter:
   --  'Num_User_Tasks_Mx + 2' when 'Preallocated_Resources' is set to
   --  "True" or '0' if 'Preallocated_Resources' is set to "False".

   Task_Priority_Mx : constant := 98; --  Priority range  0..Task_Priority_Mx
   --  Should be at least 31.
   ----------------------------------------------------------------------------
   --  General Parameters  ----------------------------------------------------
   ----------------------------------------------------------------------------
   Num_Timed_Events_Mx :  constant :=
     Num_User_Tasks_Mx * 2;  --  {PREALLOCATED}
   --  Maximum number of timed events that can be pending in the
   --  "timed events queue". Go above this number is a fatal error
   --  that finishes the program. Only used when
   --  'Preallocated_Resources' is set to "True"
   Num_Timed_Events_Initial : constant := 1000;
   --  Only used when 'Preallocated_Resources' is set to "False". In
   --  that configuration, this constant defines the initial size of
   --  the timed events queue. This size is doubled every time the
   --  limit is reached. So real-time behavior is only granted up to
   --  this value but, on the other hand, the number of timed events
   --  is only limited by the size of the dynamic memory pool.

   Queued_Signals_Maximum : constant := 32;  --  {PREALLOCATED}
   --  Never below 32 because a POSIX limit.

   type Keys_Range is range 1 .. 4;
   for Keys_Range'Size use Integer'Size;
   --  Maximun number of task-specific data keys.

   Num_Timers_Mx : constant := Num_User_Tasks_Mx;  --  {PREALLOCATED}
   --  Maximum number of timers that can exist at the same time.

   ----------------------------------------------------------------------------
   --  Stack Size  ------------------------------------------------------------
   ----------------------------------------------------------------------------
   Stack_Size_In_Bytes : constant := 40 * 1024;
   --  Every task shall have a stack of this size.

   Main_Stack_Size_In_Bytes : constant := 64 * 1024;
   --  Size of the main task's stack (only used in x86 architecture).

   Top_Main_Task_Stack : constant Integer;   --  Do NOT edit
   pragma Import (C, Top_Main_Task_Stack, "base_stack_start");
   --  Defined in 'hwi/boot/base_stack.S' for architecture x86
   Base_Main_Task_Stack : constant Integer;  --  Do NOT edit
   pragma Import (C, Base_Main_Task_Stack, "base_stack_end");
   --  Defined in 'hwi/boot/base_stack.S' for architecture x86

   Signal_Handler_Stack_Size_In_Bytes : constant := 10 * 1024;
   --  Size of the signal handler task stack

   Idle_Stack_Size_In_Bytes : constant := 10 * 1024;
   --  Size of the idle task stack. Hardware interrupts produced while
   --  the idle task is executing are served in its context, so its
   --  stack can NOT be too small.

   ----------------------------------------------------------------------------
   --  Minimum Suspension Time  -----------------------------------------------
   ----------------------------------------------------------------------------
   Suspension_Time_Minimum : constant := 2000; -- processor cycles
   --  Under this limit a suspension operation isn't performed, in
   --  particular when a task tries to suspend itself for an amount of
   --  time smaller than this limit a 'yield' operation is performed
   --  instead. It is measured in processor cycles to make this
   --  interval proportional to the CPU speed.

   ----------------------------------------------------------------------------
   --  Dynamic Memory  --------------------------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_TLSF_Memory_Allocator is Boolean range True .. True;
   --  When set to true the TLSF algorithm is used. This algorithm
   --  allows memory allocation and deallocation with bounded response
   --  times (written by Miguel Masmano Tello <mimastel@doctor.upv.es>
   --  from the Universidad Politécnica de Valencia).
   --
   --  When false, a much more simple algorithm is used. It allows
   --  allocation of memory blocks with not deallocation. It is useful
   --  for applications that do not uses dynamic memory at all (to
   --  reduce footprint) or uses it only at initialization time.

   TLSF_MAX_SL_LOG2_INDEX : constant := 5;
   --  Only relevant when TLSF is used. This parameter allows to the
   --  user to tune the maximum internal fragmentation, but a high
   --  value will cause big TLSF structure.
   --
   --  max_sl_log2_index  max. internal fragmentation (approximately)
   --  -----------------  -------------------------------------------
   --      1                             25 %
   --      2                           12.5 %
   --      3                           6.25 %
   --      4                          3.125 %
   --      5                          1.563 %

   Dynamic_Memory_Pool_Size_In_Bytes : constant := 0; --  20*1024*1024;
   --  Size of the dynamic memory pool (on x86_arch, if this value is '0'
   --  MaRTE OS will use all the available free memory as dynamic
   --  memory).

   ----------------------------------------------------------------------------
   --  POSIX Thread Execution Scheduling  -------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_POSIX_Thread_Execution_Scheduling is Boolean range True .. True;
   --  POSIX option 'Thread Execution Scheduling' currently only
   --  involves function 'pthread_setschedprio'.

   ----------------------------------------------------------------------------
   --  Round Robin Scheduling Policy  -----------------------------------------
   ----------------------------------------------------------------------------
   Use_Round_Robin_Scheduling_Policy : constant Boolean := True;
   --  Enable or disable the "Round-Robin" scheduling policy. Set this flag
   --  to False if you are not using the "Round-Robin" scheduling policy
   --  and want to improve the system performance.

   Round_Robin_Interval_Time_Period : constant := 0.03; -- seconds
   --  Round-Robin interval time period.

   ----------------------------------------------------------------------------
   --  Sporadic Server Scheduling Policy  -------------------------------------
   ----------------------------------------------------------------------------
   Use_Sporadic_Server_Scheduling_Policy : constant Boolean := True;
   --  Enable or disable the "Sporadic Server" scheduling policy. Set this
   --  flag to False if you are not using the "Sporadic Server" scheduling
   --  policy and want to improve the system performance.

   Sporadic_Server_Tasks_Mx : constant :=
     Num_User_Tasks_Mx / 5;  --  {PREALLOCATED}
   --  Maximum number of tasks with the sporadic server policy that can
   --  exist at the same time.

   Sporadic_Server_Replenishments_Mx : constant := 4;
   --  Maximum number of pending replenishment operations for each task
   --  with the sporadic server policy. Therefore, it will be allocated
   --  enough resources for 'Sporadic_Server_Replenishments_Mx *
   --  Sporadic_Server_Tasks_Mx' replenishment operations.

   ----------------------------------------------------------------------------
   --  EDF Scheduling Policy  -------------------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_EDF_Scheduling_Policy is Boolean range True .. True;
   --  Enable or disable the EDF scheduling policy. Set this flag to False if
   --  you are not using the this policy and want to improve the system
   --  performance and footprint.

   ----------------------------------------------------------------------------
   --  CPU Time Clocks And Timers  --------------------------------------------
   ----------------------------------------------------------------------------
   Use_CPU_Time_Clocks_And_Timers : constant Boolean := True;
   --  Enable or disable the CPU-time clocks and timers.

   subtype Use_Group_Clocks is Boolean range True .. True;
   --  Enable or disable the group CPU-time clocks and timers. To use
   --  this functionality 'Use_CPU_Time_Clocks_And_Timers' and
   --  'Use_Task_Sets' have to be set to true.

   ----------------------------------------------------------------------------
   --  Task Sets  -------------------------------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_Task_Sets is Boolean range True .. True;
   --  Enable or disable the task sets.

   Num_Task_Sets_Mx : constant := Num_User_Tasks_Mx / 5;  --  {PREALLOCATED}
   --  Maximum number of task sets that can exist at the same time.

   ----------------------------------------------------------------------------
   --  Timed Handlers  --------------------------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_Timed_Handlers is Boolean range True .. True;
   --  Enable or disable the timed handlers.

   ---------------------------------------------------------------------------
   --  Semaphores  -----------------------------------------------------------
   ---------------------------------------------------------------------------
   Use_Semaphores : constant Boolean := True;
   --  Enable or disable the POSIX semaphores functionality.

   ----------------------------------------------------------------------------
   --  Application-Defined Scheduling  ----------------------------------------
   ----------------------------------------------------------------------------
   Use_Application_Defined_Scheduling : constant Boolean := True;
   --  Enable or disable the application defined scheduling functionality.

   subtype Use_Stack_Resource_Policy is Boolean
     range Use_Application_Defined_Scheduling ..
     Use_Application_Defined_Scheduling;

   Num_Application_Schedulers_Mx : constant :=
     Num_User_Tasks_Mx / 10;  --  {PREALLOCATED}
   --  Maximum number of application schedulers that can exist at the same
   --  time.

   Num_Application_Scheduled_Tasks_Mx : constant :=
     Num_User_Tasks_Mx - 1;  --  {PREALLOCATED}
   --  Maximum number of tasks that can be associated simultaneously with
   --  application schedulers.

   Num_Of_Sched_App_Data : constant := Num_User_Tasks_Mx;  --  {PREALLOCATED}
   --  MaRTE associates one 'Sched_App_Data' structure with every user
   --  task. So the number of this structures only can be
   --  'Num_User_Tasks_Mx' (if 'Use_Application_Defined_Scheduling' is
   --  true) or 0 (if 'Use_Application_Defined_Scheduling' is false).

   Mx_Num_Of_Actions_In_Scheduling_Actions_Object : constant := 10;
   --  Maximum number of scheduling actions that can be added to a
   --  scheduling actions object.
   --
   --  If you want to use a larger, almost sure value, you can set a
   --  value equal to 'Num_Application_Scheduled_Tasks_Mx'. This value
   --  should be enough in most of the cases since it is always larger
   --  than the maximum number of scheduled tasks attached to an
   --  application scheduler (and it would be very strange an
   --  application scheduler could need to perform more than one
   --  action per scheduled task).

   Num_Application_Scheduling_Events_Mx :
     constant := Num_Application_Scheduled_Tasks_Mx;  --  {PREALLOCATED}
   --  Maximum number of scheduling events that can be pending in the
   --  system at the same time (adding the pending events for all the
   --  application schedulers).
   --
   --  The default value equal to 'Num_Application_Scheduled_Tasks_Mx'
   --  should be enough in most of the cases, but it could be
   --  necessary increment this number when it is going to be done an
   --  intensive use of signals or priority inheritance mutexes.

   AppSched_Param_Bytes_Size_Mx : constant := 64;
   --  Maximum size (in bytes) of the application-defined scheduling
   --  parameters.

   AppSched_Info_Mx : constant := 32;
   --  Maximum size (in bytes) of the data exchanged between an
   --  application-scheduler and its scheduled tasks (both information
   --  from scheduled tasks and reply from the scheduler).

   Mutex_AppSched_Param_Bytes_Size_Mx : constant := 64;
   --  Maximum size (in bytes) of the mutex application-defined scheduling
   --  parameters. (POSIX_APPMUTEXPARAM_MAX)

   ----------------------------------------------------------------------------
   --  Devices filesystem  ----------------------------------------------------
   ----------------------------------------------------------------------------
   Use_Devices_Filesystem :  constant Boolean := True;
   --  Enable or disable support for a reduced RAM file system for
   --  devices files.

   Open_Files_Mx : constant := 9;
   --  The number of files that can be opened at the same time.

   Devices_Files_Mx : constant := 20;
   --  The number of device files that system can support.

   Devices_Mx : constant := 16;
   --  Maximun number of devices drivers that can be
   --  supported. Devices major number range is 1 .. 'Devices_Mx'.

   Minor_Number_Mx : constant := 255;
   --  The maximun 'minor' number used for devices. Devices minor
   --  number range is 0 .. 'Minor_Number_Mx'.

   Path_Mx : constant := 16;
   --  The number of characters in a pathname.
   --  {--MAR} Should be at least 255 (POSIX). No creo que esto me
   --  afecte, ya que en MaRTE nunca se pueden crear ficheros por lo
   --  que no hace falta definir paths de esa longitud.

   ----------------------------------------------------------------------------
   -- User's interrupt handlers -----------------------------------------------
   ----------------------------------------------------------------------------
   subtype Use_HW_Interrupts_Control is Boolean range True .. True;
   --  Support for user's hardware interrupts management
   --  operations. Based in a POSIX draft for "Interrupt Control API"
   --  (P1003.2X/D1.0, February 2001).

   Intr_Connect_Max : constant := 4;
   --  Maximum number of interrupt handlers that may be connected to a
   --  given interrupt source (POSIX constant
   --  _POSIX_INTR_CONNECT_MAX). POSIX: at least should be 4.

   Mx_Number_Of_Interrupt_Associations : constant := 20;  --  {PREALLOCATED}
   --  In theory it should be 'Intr_Connect_Max * number of hw
   --  interrupts' but normally the number of user interrupt handlers
   --  will be much lower. Then, it would be a waste of memory to
   --  preallocate this maximum number.

   ----------------------------------------------------------------------------
   -- User's code language ----------------------------------------------------
   ----------------------------------------------------------------------------

   --  Set this to "True" when user's applications are not written in Ada. In
   --  that situation it is possible to reduce kernel footprint by removing
   --  some parts that are only required by the Ada run-time system.
   subtype Not_Ada_User_Code is Boolean range False .. False;

end MaRTE.Configuration_Parameters;
