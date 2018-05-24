------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                       'K e r n e l . M u t e x e s'
--
--                                  Spec
--
--
--
--  File 'k-mutexes.ads'                                               By MAR.
--
--
--  POSIX mutexes management.
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
with System;
with MaRTE.Integer_Types;

with MaRTE.Configuration_Parameters;
with MaRTE.POSIX_Constants;
with MaRTE.SLL;
pragma Elaborate_All (MaRTE.SLL);

with MaRTE.Timespec;

package MaRTE.Kernel.Mutexes is

   package K renames MaRTE.Kernel;

   package GC renames MaRTE.POSIX_Constants;
   package CP renames MaRTE.Configuration_Parameters;

   type Magic_Check is private;

   ------------------------------------------
   -- Types 'Mutex' and 'Mutex_Descriptor' --
   ------------------------------------------
   type Mutex is private;
   type Mutex_Descriptor is access all Mutex;
   Null_Mutex_Descriptor : constant Mutex_Descriptor;

   ----------------------------
   -- 'SRP_Mutex_Lock' lists --
   ----------------------------
   --
   --  Used in 'Mutexes.SRP_Ceiling' to keep track of SRP system
   --  ceiling.
   type SRP_Mutex_Lock_Base is tagged record
      M : Mutex_Descriptor;
   end record;
   package SRP_Mutex_Lock_Lists is
      new MaRTE.SLL (SRP_Mutex_Lock_Base);
   subtype SRP_Mutex_Lock_Ac is SRP_Mutex_Lock_Lists.Element_Ac;

   -----------------------------------
   -- Type 'Mutex_AppSched_Param_T' --
   -----------------------------------
   type Mutex_AppSched_Param_Size_T is
      range 0 .. CP.Mutex_AppSched_Param_Bytes_Size_Mx;
   for Mutex_AppSched_Param_Size_T'Size use MaRTE.Integer_Types.Int'Size;

   type Mutex_AppSched_Param_T is
      array (Mutex_AppSched_Param_Size_T
             range 1 .. Mutex_AppSched_Param_Size_T'Last)
     of MaRTE.Integer_Types.Unsigned_8;
   pragma Pack (Mutex_AppSched_Param_T);
   type Mutex_AppSched_Param_Ac is access all Mutex_AppSched_Param_T;

   ----------------
   -- Attributes --
   ----------------
   type Attributes is private;
   Attr_Default : constant Attributes;

   function Pthread_Mutexattr_Init (Attr : access Attributes)
                                   return Int;
   pragma Export (C, Pthread_Mutexattr_Init, "pthread_mutexattr_init");
   function Pthread_Mutexattr_Destroy (Attr : access Attributes)
                                      return Int;
   pragma Export (C, Pthread_Mutexattr_Destroy, "pthread_mutexattr_destroy");

   subtype Ceiling_Priority is Int
     range Int (Task_Priority'First) .. Int (Task_Priority'Last);
   subtype Locking_Policy is Int
     range GC.NO_PRIORITY_INHERITANCE .. GC.APPSCHED_PROTOCOL;
   NO_PRIORITY_INHERITANCE  : constant Locking_Policy :=
     GC.NO_PRIORITY_INHERITANCE;
   HIGHEST_BLOCKED_TASK     : constant Locking_Policy :=
     GC.HIGHEST_BLOCKED_TASK;
   HIGHEST_CEILING_PRIORITY : constant Locking_Policy :=
     GC.HIGHEST_CEILING_PRIORITY;
   APPSCHED_PROTOCOL        : constant Locking_Policy :=
     GC.APPSCHED_PROTOCOL;

   function Pthread_Mutexattr_Setprotocol (Attr    : access Attributes;
                                           Locking : in     Locking_Policy)
                                          return Int;
   pragma Export (C, Pthread_Mutexattr_Setprotocol,
                    "pthread_mutexattr_setprotocol");
   function Pthread_Mutexattr_Getprotocol (Attr     : access Attributes;
                                           Protocol : access Int)
                                          return Int;
   pragma Export (C, Pthread_Mutexattr_Getprotocol,
                    "pthread_mutexattr_getprotocol");

   function Pthread_Mutexattr_Setprioceiling
     (Attr        : access Attributes;
      New_Ceiling : in     Int)
     return Int;
   pragma Export (C, Pthread_Mutexattr_Setprioceiling,
                    "pthread_mutexattr_setprioceiling");
   function Pthread_Mutexattr_Getprioceiling
     (Attr        : access Attributes;
      Prioceiling : access Int)
     return Int;
   pragma Export (C, Pthread_Mutexattr_Getprioceiling,
                    "pthread_mutexattr_getprioceiling");

   function Pthread_Mutexattr_Setappscheduler
     (Attr      : access Attributes;
      Scheduler : in     K.Task_Id) return Int;
   pragma Export (C, Pthread_Mutexattr_Setappscheduler,
                    "pthread_mutexattr_setappscheduler");
   function Pthread_Mutexattr_Getappscheduler
     (Attr      : access Attributes;
      Scheduler : access K.Task_Id) return Int;
   pragma Export (C, Pthread_Mutexattr_Getappscheduler,
                    "pthread_mutexattr_getappscheduler");

   function Pthread_Mutexattr_SetappSchedparam
     (Attr       : access Attributes;
      Param      : in     Mutex_AppSched_Param_Ac;
      Param_Size : in     Mutex_AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Mutexattr_SetappSchedparam,
                    "pthread_mutexattr_setappschedparam");
   function Pthread_Mutexattr_GetappSchedparam
     (Attr       : access Attributes;
      Param      : in     Mutex_AppSched_Param_Ac;
      Param_Size : access Mutex_AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Mutexattr_GetappSchedparam,
                    "pthread_mutexattr_getappschedparam");

   ----------------------
   -- Initialize Mutex --
   ----------------------
   function Pthread_Mutex_Init (M    : access Mutex;
                                Attr : access Attributes) return Int;
   pragma Export (C, Pthread_Mutex_Init, "pthread_mutex_init");

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (M : Mutex) return Mutex_Descriptor;

   --------------------
   -- Finalize Mutex --
   --------------------
   function Pthread_Mutex_Destroy (M : access Mutex) return Int;
   pragma Export (C, Pthread_Mutex_Destroy, "pthread_mutex_destroy");

   ----------------------------------
   -- Set and Get Ceiling Priority --
   ----------------------------------
   function Pthread_Mutex_Setprioceiling
     (M           : in     Mutex_Descriptor;
      New_Ceiling : in     Int;
      Old_Ceiling : access Int) return Int;
   pragma Export (C, Pthread_Mutex_Setprioceiling,
                    "pthread_mutex_setprioceiling");
   function Pthread_Mutex_Getprioceiling
     (M           : in     Mutex_Descriptor;
      Prioceiling : access Int) return Int;
   pragma Export (C, Pthread_Mutex_Getprioceiling,
                    "pthread_mutex_getprioceiling");
   function Pthread_Mutex_Setprioceiling_Locked
     (M           : in     Mutex_Descriptor;
      New_Ceiling : in     Int) return Int;
   pragma Export (C, Pthread_Mutex_Setprioceiling_Locked,
                    "pthread_mutex_setprioceiling_locked");

   ---------------------------------------------------
   -- Set and Get Application-Scheduling parameters --
   ---------------------------------------------------
   function Pthread_Mutex_Setappschedparam
     (M          : in Mutex_Descriptor;
      Param      : in Mutex_AppSched_Param_Ac;
      Param_Size : in Mutex_AppSched_Param_Size_T)
     return Int;
   pragma Export (C, Pthread_Mutex_Setappschedparam,
                  "pthread_mutex_setappschedparam");
   function Pthread_Mutex_Getappschedparam
     (M          : in     Mutex_Descriptor;
      Param      : Mutex_AppSched_Param_Ac;
      Param_Size : access Mutex_AppSched_Param_Size_T)
      return Int;
   pragma Export (C, Pthread_Mutex_Getappschedparam,
                  "pthread_mutex_getappschedparam");

   -----------------------------------
   -- Get the Application-Scheduler --
   -----------------------------------
   function Pthread_Mutex_Getappscheduler (M : in     Mutex_Descriptor;
                                           T : access Task_Id) return Int;
   pragma Export (C, Pthread_Mutex_Getappscheduler,
                    "pthread_mutex_getappscheduler");

   -------------------------------------
   -- Set and Get Mutex-Specific Data --
   -------------------------------------
   function Posix_Appsched_Mutex_Setspecific
     (M    : in Mutex_Descriptor;
      Data : in System.Address) return Int;
   pragma Export (C, Posix_Appsched_Mutex_Setspecific,
                    "posix_appsched_mutex_setspecific");
   function Posix_Appsched_Mutex_Getspecific
     (M    : in Mutex_Descriptor;
      Data : access System.Address) return Int;
   pragma Export (C, Posix_Appsched_Mutex_Getspecific,
                    "posix_appsched_mutex_getspecific");

   ----------------------------------------------------------------------------
   -- Lock, timed lock, try lock and unlock -----------------------------------
   ----------------------------------------------------------------------------
   function Pthread_Mutex_Lock (M : in Mutex_Descriptor) return Int;
   pragma Export (C, Pthread_Mutex_Lock, "pthread_mutex_lock");
   function Pthread_Mutex_Timedlock
     (M           : in Mutex_Descriptor;
      Abs_Timeout : in MaRTE.Timespec.Timespec_Ac)
     return Int;
   pragma Export (C, Pthread_Mutex_Timedlock, "pthread_mutex_timedlock");
   function Pthread_Mutex_Trylock (M : in Mutex_Descriptor) return Int;
   pragma Export (C, Pthread_Mutex_Trylock, "pthread_mutex_trylock");
   function Pthread_Mutex_Unlock (M : in Mutex_Descriptor) return Int;
   pragma Export (C, Pthread_Mutex_Unlock, "pthread_mutex_unlock");

   ------------------------------------------------
   -- POSIX 'PTHREAD_MUTEX_INITIALIZER' constant --
   ------------------------------------------------
   PTHREAD_MUTEX_INITIALIZER : aliased constant Mutex;

private
   --  Magic values to detect invalid Mutexes. It is an array of characters
   --  for easier reading when debugging.
   type Attr_Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Attr_Magic_Check);
   ATTR_NOT_INITIALIZED : constant Attr_Magic_Check := ('m', 'a', 'n', 'i');
   ATTR_INITIALIZED     : constant Attr_Magic_Check := ('M', 'A', '_', 'I');

   type Attributes is record
      Magic               : Attr_Magic_Check := ATTR_NOT_INITIALIZED;
      Policy              : Locking_Policy := NO_PRIORITY_INHERITANCE;
      Ceiling_Prio        : K.Task_Priority := Task_Priority'First;
      Preemption_Level    : K.Task_Preemption_Level :=
        Task_Preemption_Level'First;
      App_Scheduler       : Task_Id := null;
      AppSched_Param      : Mutex_AppSched_Param_T := (others => 0);
      AppSched_Param_Size : Mutex_AppSched_Param_Size_T := 0;
   end record;

   Attr_Default : constant Attributes :=
     (Magic               => ATTR_INITIALIZED,
      Policy              => NO_PRIORITY_INHERITANCE,
      Ceiling_Prio        => K.Task_Priority'First,
      Preemption_Level    => K.Task_Preemption_Level'Last,
      App_Scheduler       => null,
      AppSched_Param      => (others => 0),
      AppSched_Param_Size => 0);

   function Attr_OK (Attr : access Attributes) return Boolean;
   pragma Inline (Attr_OK);

   ----------------------------------------------------------------------------
   -- Type 'Mutex' ------------------------------------------------------------
   ----------------------------------------------------------------------------

   -----------------
   -- Magic_Check --
   -----------------
   --
   --  Magic values to detect invalid Mutexes. It is an array of characters
   --  for easier reading when debugging.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   NOT_INITIALIZED : constant Magic_Check := ('M', 'n', '_', 'i');
   NEW_APPSCHED    : constant Magic_Check := ('m', 'n', 'a', 'p');
   INITIALIZED     : constant Magic_Check := ('M', 'I', 'N', 'I');
   FINALIZED       : constant Magic_Check := ('m', 'F', 'I', 'N');

   ------------------------
   -- Foo_SRP_Mutex_Lock --
   ------------------------
   Foo_SRP_Mutex_Lock : SRP_Mutex_Lock_Lists.Element;

   ---------------------------
   -- Null_Mutex_Descriptor --
   ---------------------------
   Null_Mutex_Descriptor : constant Mutex_Descriptor := null;

   -----------
   -- Mutex --
   -----------
   --
   --  Every field added here must be initialized in
   --  'PTHREAD_MUTEX_INITIALIZER' and possibly in 'Pthread_Mutex_Init'
   --  and/or 'Internals.Initialize_HCP_Mutex'
   type Mutex is new Mutexes_Lists.Element with record
      Magic : Magic_Check := NOT_INITIALIZED;
      --  To detect invalid Mutexes

      Id : Integer := -1;
      --  For easier identification when debugging

      Policy : Locking_Policy := NO_PRIORITY_INHERITANCE;
      --  Mutex Policy

      Prio : Task_Priority := Task_Priority'First;
      --  This parameter has no meaning when the mutex has the
      --  'NO_PRIORITY_INHERITANCE' policy, is a constant ceiling when
      --  its policy is 'HIGHEST_CEILING_PRIORITY' or has a variable
      --  value (the highest active priority among all blocked tasks)
      --  if its policy is 'HIGHEST_BLOCKED_TASK'.

      Preemption_Level : Task_Preemption_Level := Task_Preemption_Level'First;
      --  The mutex "Preemption Level". Used with SRP mutexes.

      Owner : Task_Id := null;
      --  The mutex is locked if 'Owner' isn't 'null'.

      Blocked_Tasks : K.Tasks_Lists.List := MaRTE.Kernel.Tasks_Lists.Null_List;
      --  List of tasks blocked in the mutex

      Num_Of_Associated_CVs : Integer := 0;
      --  Counts the number of CVs associated with the mutex

      App_Scheduler : Task_Id := null;
      --  Associated application-scheduler task.

      AppSched_Param : Mutex_AppSched_Param_T := (others => 0);
      --  Application-defined scheduling parameters

      AppSched_Param_Size : Mutex_AppSched_Param_Size_T := 0;
      --  Size of the application-defined scheduling parameters

      Specific_Data : System.Address := System.Null_Address;
      --  Mutex-Specific Data. Used for the application-defined scheduler
      --  tasks

      Task_Waiting_Appscheduler_Response : Task_Id := null;
      --  Identifies the task that is creating this mutex with protocol
      --  APPSCHED_PROTOCOL.

      SRP_Mutex_Lock : aliased SRP_Mutex_Lock_Lists.Element :=
        Foo_SRP_Mutex_Lock;
      --  Used to keep track of the system ceiling. When the mutex is
      --  locked this element is queued using
      --  'Mutexes.SRP_Ceiling.Add_Mutex_Lock' in
      --  'Mutexes.SRP_Ceiling.Ceiling_List'.
   end record;

   ------------------------------------------------
   -- POSIX 'PTHREAD_MUTEX_INITIALIZER' constant --
   ------------------------------------------------
   Foo_Mutexes_Lists_Element : Mutexes_Lists.Element;
   PTHREAD_MUTEX_INITIALIZER : aliased constant Mutex :=
     (Foo_Mutexes_Lists_Element with
      Magic                              => INITIALIZED,
      Id                                 => 101,
      Policy                             => NO_PRIORITY_INHERITANCE,
      Prio                               => Task_Priority'First,
      Preemption_Level                   => Task_Preemption_Level'First,
      Owner                              => null,
      Blocked_Tasks                      => K.Tasks_Lists.Null_List,
      Num_Of_Associated_CVs              => 0,
      App_Scheduler                      => null,
      AppSched_Param                     => (others => 0),
      AppSched_Param_Size                => 0,
      Specific_Data                      => System.Null_Address,
      Task_Waiting_Appscheduler_Response => null,
      SRP_Mutex_Lock                     => Foo_SRP_Mutex_Lock);
   --  Policy and others default values should not be changed since
   --  others parts of the kernel relies in this particular values.
end MaRTE.Kernel.Mutexes;
