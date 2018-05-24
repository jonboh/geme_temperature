------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--    'K e r n e l . A p p l i c a t i o n _ S c h e d u l i n g _ D a t a'
--
--                                   Spec
--
--
--
--  File 'k-appsched_data.ads'                                         By MAR.
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

with MaRTE.Configuration_Parameters;

with Ada.Unchecked_Conversion;
with MaRTE.POSIX_Constants;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Signals;
with MaRTE.Kernel.Timers;
with MaRTE.SLL;
pragma Elaborate_All (MaRTE.SLL);
with MaRTE.SLL.Order;
pragma Elaborate_All (MaRTE.SLL.Order);

package MaRTE.Kernel.Application_Scheduling_Data is

   package K renames MaRTE.Kernel;

   package PC renames MaRTE.POSIX_Constants;
   package CP renames MaRTE.Configuration_Parameters;

   ---------------------------------------------------------------------------
   -- Type 'AppSched_Event' --------------------------------------------------
   ---------------------------------------------------------------------------
   type Event_Code_T is
      (APPSCHED_NEW,
       APPSCHED_TERMINATE,
       APPSCHED_READY,
       APPSCHED_BLOCK,
       APPSCHED_YIELD,
       APPSCHED_SIGNAL,
       APPSCHED_CHANGE_SCHED_PARAM,
       APPSCHED_EXPLICIT_CALL,
       APPSCHED_EXPLICIT_CALL_WITH_DATA,
       APPSCHED_TIMEOUT,
       APPSCHED_PRIORITY_INHERIT,
       APPSCHED_PRIORITY_UNINHERIT,
       APPSCHED_INIT_MUTEX,
       APPSCHED_DESTROY_MUTEX,
       APPSCHED_LOCK_MUTEX,
       APPSCHED_TRY_LOCK_MUTEX,
       APPSCHED_UNLOCK_MUTEX,
       APPSCHED_BLOCK_AT_MUTEX,
       APPSCHED_CHANGE_SCHED_PARAM_MUTEX,
       APPSCHED_TASK_NOTIFICATION);
   for Event_Code_T use
      (APPSCHED_NEW                     => PC.APPSCHED_NEW,
       APPSCHED_TERMINATE               => PC.APPSCHED_TERMINATE,
       APPSCHED_READY                   => PC.APPSCHED_READY,
       APPSCHED_BLOCK                   => PC.APPSCHED_BLOCK,
       APPSCHED_YIELD                   => PC.APPSCHED_YIELD,
       APPSCHED_SIGNAL                  => PC.APPSCHED_SIGNAL,
       APPSCHED_CHANGE_SCHED_PARAM      => PC.APPSCHED_CHANGE_SCHED_PARAM,
       APPSCHED_EXPLICIT_CALL           => PC.APPSCHED_EXPLICIT_CALL,
       APPSCHED_EXPLICIT_CALL_WITH_DATA => PC.APPSCHED_EXPLICIT_CALL_WITH_DATA,
       APPSCHED_TIMEOUT                 => PC.APPSCHED_TIMEOUT,
       APPSCHED_PRIORITY_INHERIT        => PC.APPSCHED_PRIORITY_INHERIT,
       APPSCHED_PRIORITY_UNINHERIT      => PC.APPSCHED_PRIORITY_UNINHERIT,
       APPSCHED_INIT_MUTEX              => PC.APPSCHED_INIT_MUTEX,
       APPSCHED_DESTROY_MUTEX           => PC.APPSCHED_DESTROY_MUTEX,
       APPSCHED_LOCK_MUTEX              => PC.APPSCHED_LOCK_MUTEX,
       APPSCHED_TRY_LOCK_MUTEX          => PC.APPSCHED_TRY_LOCK_MUTEX,
       APPSCHED_UNLOCK_MUTEX            => PC.APPSCHED_UNLOCK_MUTEX,
       APPSCHED_BLOCK_AT_MUTEX          => PC.APPSCHED_BLOCK_AT_MUTEX,
       APPSCHED_CHANGE_SCHED_PARAM_MUTEX
                                   => PC.APPSCHED_CHANGE_SCHED_PARAM_MUTEX,
       APPSCHED_TASK_NOTIFICATION       => PC.APPSCHED_TASK_NOTIFICATION);
   for Event_Code_T'Size use Int'Size;

   ------------------------
   --  Type 'Event_Set'  --
   ------------------------
   type Event_Set is new MaRTE.Integer_Types.Unsigned_32;

   ---------------------------
   -- type 'AppSched_Event' --
   ---------------------------
   type AppSched_Event is tagged record
      Event_Code           : Event_Code_T;
      T                    : Task_Id;
      Sched_Priority       : Task_Priority;
      M                    : Mutexes.Mutex_Descriptor;
      Siginfo              : Signals.Siginfo_T;
      Info                 : System.Address;
      Info_Size            : Int;
      User_Event_Code      : Int;
   end record;
   package AppSched_Events_Lists is new MaRTE.SLL (AppSched_Event);
   subtype AppSched_Event_Ac is AppSched_Events_Lists.Element_Ac;
   --  Changes in the 'AppSched_Event' type must be reflected in the types
   --  'Sched.Posix_Appsched_Event' and its C equivalent
   --  'posix_appsched_event' in 'sched.h'.

   ---------------------------------------------------
   --  Release and request 'AppSched_Event' objects --
   ---------------------------------------------------
   procedure Release_AppSched_Event (E : in AppSched_Event_Ac);
   pragma Inline (Release_AppSched_Event);

   function Request_AppSched_Event return AppSched_Event_Ac;
   pragma Inline (Request_AppSched_Event);

   ---------------------------------------------------
   --  Release and request 'Task_Container' objects --
   ---------------------------------------------------
   procedure Release_Task_Container (TC : in Task_Container_Ac);

   function Request_Task_Container return Task_Container_Ac;

   function Any_Task_Container_Free_In_Pool return Boolean;

   ----------------------------------------------------------------------------
   --  Type 'Scheduler_Reply_T'  ----------------------------------------------
   ----------------------------------------------------------------------------
   type Scheduler_Reply_Size_T is range 0 .. CP.AppSched_Info_Mx;
   for Scheduler_Reply_Size_T'Size use MaRTE.Integer_Types.Int'Size;
   type Scheduler_Reply_Size_Ac is access all Scheduler_Reply_Size_T;
   type Scheduler_Reply_T is array (Scheduler_Reply_Size_T
                                    range 1 .. Scheduler_Reply_Size_T'Last)
     of MaRTE.Integer_Types.Unsigned_8;
   pragma Pack (Scheduler_Reply_T);
   pragma Convention (C, Scheduler_Reply_T);
   type Scheduler_Reply_Ac is access all Scheduler_Reply_T;

   ----------------------------------------------------------------------------
   --  Type 'Notification_Timed_Event' ----------------------------------------
   ----------------------------------------------------------------------------
   type Notification_Timed_Event is new K.Timed_Events_Lists.Element
     with record
        Task_To_Notify : Task_Id;
        Used           : Boolean;
     end record;
   Foo_Timed_Events_Lists_Element : K.Timed_Events_Lists.Element;
   Null_Notification_TE : constant Notification_Timed_Event :=
     (Foo_Timed_Events_Lists_Element with
      Task_To_Notify => null,
      Used           => False);

   ----------------------------------------------------------------------------
   --  Type 'Activation_Timed_Event' ----------------------------------------
   ----------------------------------------------------------------------------
   type Activation_Timed_Event is new K.Timed_Events_Lists.Element
     with record
        Task_To_Notify : Task_Id;
        Urgency        : K.Task_Urgency;
        Used           : Boolean;
     end record;
   Null_Activation_TE : constant Activation_Timed_Event :=
     (Foo_Timed_Events_Lists_Element with
      Task_To_Notify => null,
      Urgency        => K.Task_Urgency'First,
      Used           => False);

   ----------------------------------------------------------------------------
   --  Type 'Sched_App_Data'  -------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Specific data asociated with tasks of SCHED_APP policy in
   --  'TCB.Sched_App'.
   type Sched_App_Data is new K.Sched_App_Data_Base with record
      Scheduler : Task_Id;
      --  Application scheduler task.

      Param : aliased K.AppSched_Param_T;
      --  Application-defined scheduling parameters.

      Param_Size : K.AppSched_Param_Size_T;
      --  Size of the application-defined scheduling parameters.

      AppMutex_Where_Waiting : Mutexes.Mutex_Descriptor;
      --  Used by tasks whose policy is SCHED_APP.

      Scheduler_Reply : Scheduler_Reply_Ac;
      --  Points to the memory area provided for the task to store
      --  the appscheduler reply.

      Reply_Size : Scheduler_Reply_Size_T;
      --  Size in bytes of the appscheduler reply.

      Owner : Task_Id;
      --  Task associated with this record

      Notification_Event : aliased Notification_Timed_Event;
      --  Timed event used to program a task notification event

      Activation_Event : aliased Activation_Timed_Event;
      --  Timed event used to program a timed task activation event

      Suspend_After_Scheduler_Operation : Boolean;
      --  Used in
      --  'APPSCHD.Execute_Scheduler_Operation_In_Running_Task_Context'
      --  to suspend the task when there is not activation operation
      --  for it.
   end record;
   package Sched_App_Data_Lists is new MaRTE.SLL (Sched_App_Data);
   subtype Sched_App_Data_Ac is Sched_App_Data_Lists.Element_Ac;

   function UC_To_Sched_App_Data_Ac is
     new Ada.Unchecked_Conversion (K.Sched_App_Data_Base_Ac,
                                   Sched_App_Data_Ac);

   ---------------------------------------------------------------
   --  Request and Release 'AppSchedulerSched_App_Data' objects --
   ---------------------------------------------------------------
   function Request_Sched_App_Data return Sched_App_Data_Ac;
   procedure Release_Sched_App_Data (S : Sched_App_Data_Ac);

   ----------------------------------------------------------------------------
   --  Type 'Sched_Actions_Set'  ----------------------------------------------
   ----------------------------------------------------------------------------
   type Actions_T is (ACTIVATE, ACTIVATE_WITH_URGENCY, SUSPEND,
                      ACCEPT_TASK, REJECT_TASK,
                      TIMED_TASK_ACTIVATION,
                      TIMED_TASK_NOTIFICATION,
                      ACCEPT_MUTEX, REJECT_MUTEX, LOCK_MUTEX);

   type Sched_Action is record
      T       : Task_Id;
      Urgency : K.Task_Urgency;
      M       : K.Mutexes.Mutex_Descriptor;
      At_Time : MaRTE.HAL.HWTime;
      Action  : Actions_T;
   end record;

   type Sched_Actions_Array is
     array (1 .. CP.Mx_Num_Of_Actions_In_Scheduling_Actions_Object)
     of aliased Sched_Action;

   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);

   type Sched_Actions_Set is record
      Initialized : Magic_Check;
      Set         : Sched_Actions_Array;
      Last        : Integer; --  Last position used in 'set'.
   end record;
   type Sched_Actions_Set_Ac is access all Sched_Actions_Set;

   NOT_INITIALIZED : constant Magic_Check := ('s', 'a', 'n', 'i');
   INITIALIZED     : constant Magic_Check := ('S', 'A', 'I', 'N');

   ----------------------------------------------------------------------------
   --  Type 'Scheduler_Operations'  -------------------------------------------
   ----------------------------------------------------------------------------
   type Init_Op is access procedure (Sched : in System.Address);
   type New_Task_Op is access procedure
     (Sched   : in System.Address;
      Tid     : in K.Task_Id;
      Actions : in Sched_Actions_Set_Ac);
   type Ready_Op is access procedure
     (Sched   : in System.Address;
      Tid     : in K.Task_Id;
      Actions : in Sched_Actions_Set_Ac);
   type Explicit_Call_Op is access procedure
     (Sched   : in System.Address;
      Tid     : in     K.Task_Id;
      User_Event_Code : in Int;
      Actions : in Sched_Actions_Set_Ac);
   type Task_Notification_Op is access procedure
     (Sched : in System.Address;
      Tid : in  K.Task_Id;
      Actions : in Sched_Actions_Set_Ac);
   type Timeout_Op is access procedure
     (Sched : in System.Address;
      Actions : in Sched_Actions_Set_Ac);
   type Signal_Op is access procedure
     (Sched    : in System.Address;
      Signinfo : in K.Signals.Siginfo_T;
      Actions  : in Sched_Actions_Set_Ac);
   --  procedure Priority_Inherit
   --    (Sched : in out Scheduler;
   --     Tid : in  Kernel.Task_Id;
   --     From_Tid : in Kernel.Task_Id;
   --     Inherited_Prio : in  System.Any_Priority;
   --     Actions : in APPSCHD.Sched_Actions_Set_Ac);
   --  procedure Priority_Uninherit
   --    (Sched : in out Scheduler;
   --     Tid : in  Kernel.Task_Id;
   --     From_Tid : in Kernel.Task_Id;
   --     Uninherited_Prio : in  System.Any_Priority;
   --     Actions : in APPSCHD.Sched_Actions_Set_Ac);
   --  procedure Error
   --    (Sched : in out Scheduler;
   --     Tid : in Kernel.Task_Id;
   --     Cause : in  Error_Cause;
   --     Actions : in APPSCHD.Sched_Actions_Set_Ac)
   --     is abstract;

   type Scheduler_Operations is record
      Init : Init_Op;
      New_Task : New_Task_Op;
      Ready : Ready_Op;
      Explicit_Call : Explicit_Call_Op;
      Task_Notification : Task_Notification_Op;
      Timeout : Timeout_Op;
      Signal : Signal_Op;
   end record;
   pragma Convention (C, Scheduler_Operations);

   ----------------------------------------------------------------------------
   -- Type 'AppScheduler_Data' ------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Data structure associated with application-scheduler tasks in
   --  'TCB.AppScheduler'.
   type AppScheduler_Data_Flags_T is new Unsigned_32;
   type AppScheduler_Data is new K.AppScheduler_Data_Base with record
      Tasks_Q  : K.Task_Containers_Lists.List
        := K.Task_Containers_Lists.Null_List;
      Events_Q : AppSched_Events_Lists.List := AppSched_Events_Lists.Null_List;
      Flags       : AppScheduler_Data_Flags_T := 0;
      Events_Mask : Event_Set := 0;
      Wait_Signal_Set : aliased K.Signals.Signal_Set :=
        K.Signals.Empty_Set;
      Waiting_For_Signals : Boolean := False;
      Clock : Timers.Clock_Id := Timers.CLOCK_REALTIME;
      Reserved_TCs : K.Task_Containers_Lists.List
        := K.Task_Containers_Lists.Null_List;
      Suspension_Deferred_Tasks : Sched_App_Data_Lists.List
        := Sched_App_Data_Lists.Null_List;
      Reply      : Scheduler_Reply_T      := (others => 0);
      Reply_Size : Scheduler_Reply_Size_T := 0;
      Ops : Scheduler_Operations;
      Data : System.Address;
   end record;
   package AppScheduler_Data_Lists is
     new MaRTE.SLL (AppScheduler_Data);
   subtype AppScheduler_Data_Ac is AppScheduler_Data_Lists.Element_Ac;

   --------------------------------------
   -- 'AppScheduler_Data.Flags' values --
   --------------------------------------
   APPSCHED_ABSTIMEOUT : constant AppScheduler_Data_Flags_T :=
     MaRTE.POSIX_Constants.APPSCHED_ABSTIMEOUT;

   --------------------
   -- 'Event_In_Set' --
   --------------------
   --
   --  Returns true if Event is in Set
   function Event_In_Set (Event : in Event_Code_T;
                          Set   : in Event_Set)
                          return Boolean;
   pragma Inline (Event_In_Set);

   --------------------
   -- 'To_Event_Set' --
   --------------------
   --
   --  Transform an 'Event_Code_T' into an 'Event_Set' that only contains
   --  the that event
   function To_Event_Set (Event : in Event_Code_T)
                          return Event_Set;
   pragma Inline (To_Event_Set);

   ------------------------------------------------------
   --  Release and request 'AppScheduler_Data' objects --
   ------------------------------------------------------
   procedure Release_AppScheduler_Data
     (AppScheduler_Data : in AppScheduler_Data_Ac);
   --  It also releases the tasks containers and the events in its
   --  'Tasks_Q' and 'Events_Q' queues.

   function Request_AppScheduler_Data return AppScheduler_Data_Ac;

   ---------------------------------------------
   --  Unchecked Conversion from              --
   --  'Kernel.AppScheduler_Data_Base_Ac' to  --
   --  'AppScheduler_Data_Ac'                 --
   ---------------------------------------------
   function UC_To_AppScheduler_Data_Ac is
     new Ada.Unchecked_Conversion (K.AppScheduler_Data_Base_Ac,
                                   AppScheduler_Data_Ac);

   ------------------------
   --  Initialize_Pools  --
   ------------------------
   --
   --  Initialize pools of: 'AppSched_Events', TCs, 'AppScheduler_Data' and
   --  'Sched_App_Data'
   procedure Initialize_Pools;

end MaRTE.Kernel.Application_Scheduling_Data;
