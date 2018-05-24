------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--                              'P T h r e a d'
--
--                                   Spec
--
--
--
--  File 'pthread.ads'                                                 By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over MaRTE OS. In particular this package defines
--  the functions in the C header file 'pthread.h'.
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

with MaRTE.HAL;
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel; use MaRTE.Kernel;
pragma Elaborate_All (MaRTE.Kernel);
with MaRTE.Kernel.Scheduler;
pragma Elaborate_All (MaRTE.Kernel.Scheduler);
with MaRTE.Kernel.Tasks_Operations;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Operations);
with MaRTE.Kernel.Tasks_Operations.Attributes;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Operations.Attributes);
with MaRTE.Kernel.Condition_Variables;
pragma Elaborate_All (MaRTE.Kernel.Condition_Variables);
with MaRTE.Kernel.Mutexes;
pragma Elaborate_All (MaRTE.Kernel.Mutexes);

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

with MaRTE.Timespec;

package MaRTE.POSIX_Pthread is

   package K renames MaRTE.Kernel;

   package TO      renames K.Tasks_Operations;
   package TO_ATTR renames K.Tasks_Operations.Attributes;
   package CV      renames K.Condition_Variables;

   ----------------------------------------------------------------------------
   -- Errno -------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Pthread_Errno return System.Address;
   pragma Export (C, Pthread_Errno, External_Name => "pthread_errno");

   procedure Set_Pthread_Errno (Errno : in Int);
   pragma Export (C, Set_Pthread_Errno, External_Name => "__set_errno");

   ----------------------------------------------------------------------------
   -- Mutexes -----------------------------------------------------------------
   ----------------------------------------------------------------------------

   function Descriptor_Of (M : Mutexes.Mutex) return Mutexes.Mutex_Descriptor
     renames Mutexes.Descriptor_Of;

   --  13.3 Mutexes
   subtype Mutex_Descriptor    is Mutexes.Mutex_Descriptor;
   subtype Pthread_Mutex_T     is Mutexes.Mutex;
   subtype Pthread_Mutexattr_T is Mutexes.Attributes;
   subtype Locking_Policy      is Mutexes.Locking_Policy;
   subtype Ceiling_Priority    is Mutexes.Ceiling_Priority;

   function Pthread_Mutexattr_Init (Attr : access Mutexes.Attributes)
                                   return Int
     renames Mutexes.Pthread_Mutexattr_Init;

   function Pthread_Mutexattr_Destroy (Attr : access Mutexes.Attributes)
                                      return Int
     renames Mutexes.Pthread_Mutexattr_Destroy;

   function Pthread_Mutex_Init (M    : access Mutexes.Mutex;
                                Attr : access Mutexes.Attributes)
                               return Int
     renames Mutexes.Pthread_Mutex_Init;

   function Pthread_Mutex_Destroy (M : access Mutexes.Mutex)
                                  return Int
     renames Mutexes.Pthread_Mutex_Destroy;

   function Pthread_Mutex_Lock (M : Mutexes.Mutex_Descriptor)
                               return Int
     renames Mutexes.Pthread_Mutex_Lock;

   function Pthread_Mutex_Trylock (M : Mutexes.Mutex_Descriptor)
                                  return Int
     renames Mutexes.Pthread_Mutex_Trylock;

   function Pthread_Mutex_Unlock (M : Mutexes.Mutex_Descriptor)
                                 return Int
     renames Mutexes.Pthread_Mutex_Unlock;

   --  13.6 Synchronization Scheduling

   function Pthread_Mutexattr_Setprotocol
     (Attr     : access Mutexes.Attributes;
      Protocol : in     Mutexes.Locking_Policy) return Int
     renames Mutexes.Pthread_Mutexattr_Setprotocol;

   function Pthread_Mutexattr_Getprotocol
     (Attr     : access Mutexes.Attributes;
      Protocol : access Int) return Int
     renames Mutexes.Pthread_Mutexattr_Getprotocol;

   function Pthread_Mutexattr_Setprioceiling
     (Attr        : access Mutexes.Attributes;
      Prioceiling :        Mutexes.Ceiling_Priority) return Int
     renames Mutexes.Pthread_Mutexattr_Setprioceiling;

   function Pthread_Mutexattr_Getprioceiling
     (Attr        : access Mutexes.Attributes;
      Prioceiling : access Int) return Int
     renames Mutexes.Pthread_Mutexattr_Getprioceiling;

   function Pthread_Mutexattr_Setappscheduler
     (Attr      : access Mutexes.Attributes;
      Scheduler :        K.Task_Id) return Int
     renames Mutexes.Pthread_Mutexattr_Setappscheduler;

   function Pthread_Mutexattr_Getappscheduler
     (Attr      : access Mutexes.Attributes;
      Scheduler : access K.Task_Id) return Int
     renames Mutexes.Pthread_Mutexattr_Getappscheduler;

   function Pthread_Mutexattr_SetappSchedparam
     (Attr       : access Mutexes.Attributes;
      Param      :        Mutexes.Mutex_AppSched_Param_Ac;
      Param_Size :        Mutexes.Mutex_AppSched_Param_Size_T) return Int
     renames Mutexes.Pthread_Mutexattr_SetappSchedparam;

   function Pthread_Mutexattr_GetappSchedparam
     (Attr       : access Mutexes.Attributes;
      Param      :        Mutexes.Mutex_AppSched_Param_Ac;
      Param_Size : access Mutexes.Mutex_AppSched_Param_Size_T) return Int
     renames Mutexes.Pthread_Mutexattr_GetappSchedparam;

   function POSIX_Appsched_Mutex_Setspecific (Mutex : Mutexes.Mutex_Descriptor;
                                              Data  : System.Address)
                                             return Int
     renames Mutexes.Posix_Appsched_Mutex_Setspecific;

   function POSIX_Appsched_Mutex_Getspecific (Mutex : Mutexes.Mutex_Descriptor;
                                              Data  : access System.Address)
                                             return Int
     renames Mutexes.Posix_Appsched_Mutex_Getspecific;

   ----------------------------------------------------------------------------
   -- Condition Variables -----------------------------------------------------
   ----------------------------------------------------------------------------

   function Descriptor_Of (M : CV.Condition) return CV.Condition_Descriptor
     renames CV.Descriptor_Of;

   --  11.4 Condition Variables
   subtype Condition_Descriptor is CV.Condition_Descriptor;
   subtype Pthread_Cond_T       is CV.Condition;
   subtype Pthread_Condattr_T   is CV.Attributes;
   function Pthread_Condattr_Init (Attr : access CV.Attributes)
                                  return Int
     renames CV.Pthread_Condattr_Init;

   function Pthread_Condattr_Destroy (Attr : access CV.Attributes)
                                     return Int
     renames CV.Pthread_Condattr_Destroy;

   function Pthread_Cond_Init (Cond : access CV.Condition;
                               Attr : access CV.Attributes)
                              return Int
     renames CV.Pthread_Cond_Init;

   function Pthread_Cond_Destroy (Cond : access CV.Condition)
                                 return Int
     renames CV.Pthread_Cond_Destroy;

   function Pthread_Cond_Signal (Cond : access CV.Condition)
                                return Int
     renames CV.Pthread_Cond_Signal;

   function Pthread_Cond_Broadcast (Cond : CV.Condition_Descriptor)
                                   return Int
     renames CV.Pthread_Cond_Broadcast;

   function Pthread_Cond_Wait (Cond :    CV.Condition_Descriptor;
                               M    :    Mutexes.Mutex_Descriptor)
                              return Int
     renames CV.Pthread_Cond_Wait;

   function Pthread_Cond_Timedwait_Gnat (Cond    : CV.Condition_Descriptor;
                                         M       : Mutexes.Mutex_Descriptor;
                                         Abstime : MaRTE.HAL.HWTime)
                                         return Int
     renames CV.Timed_Wait_HWTime;
   function Pthread_Cond_Timedwait (Cond    : CV.Condition_Descriptor;
                                    M       : Mutexes.Mutex_Descriptor;
                                    Abstime : access MaRTE.Timespec.Timespec)
                                    return    Int;
   pragma Export (C, Pthread_Cond_Timedwait, "pthread_cond_timedwait");

   ----------------------------------------------------------------------------
   --  PThreads  --------------------------------------------------------------
   ----------------------------------------------------------------------------

   subtype Task_Priority              is K.Task_Priority;
   subtype Pthread_T                  is K.Task_Id;
   subtype Pthread_Attr_T             is TO.Pthread_Attr_T;
   subtype Pthread_Attr_T_Ac_Const    is TO.Pthread_Attr_T_Ac_Const;
   subtype Sched_Param                is TO.Sched_Param;
   subtype Scheduling_Policies        is K.Scheduling_Policies;
   subtype Task_Body_Function         is TO.Task_Body_Function;
   subtype Regular_Or_App_Scheduler_T is TO.Regular_Or_App_Scheduler_T;

   ------------------------------------
   -- Pthread_Attr_Init (16 Threads) --
   ------------------------------------
   function Pthread_Attr_Init (Attr : access TO.Pthread_Attr_T)
                               return Int
     renames TO_ATTR.Pthread_Attr_Init;

   ---------------------------------------
   -- Pthread_Attr_Destroy (16 Threads) --
   ---------------------------------------
   function Pthread_Attr_Destroy (Attr : access TO.Pthread_Attr_T)
                                  return Int
     renames TO_ATTR.Pthread_Attr_Destroy;

   --------------------------------------------
   -- Pthread_Attr_Setstackaddr (16 Threads) --
   --------------------------------------------
   --
   --  Returns 'ENOSYS' (not implemented).
   function Pthread_Attr_Setstackaddr (Attr      : access Pthread_Attr_T;
                                       Stackaddr : System.Address)
                                       return Int;
   pragma Export (C, Pthread_Attr_Setstackaddr, "pthread_attr_setstackaddr");

   --------------------------------------------
   -- Pthread_Attr_Getstackaddr (16 Threads) --
   --------------------------------------------
   --
   --  Returns 'ENOSYS' (not implemented).
   function Pthread_Attr_Getstackaddr (Attr      : access Pthread_Attr_T;
                                       Stackaddr : access System.Address)
                                       return Int;
   pragma Export (C, Pthread_Attr_Getstackaddr, "pthread_attr_getstackaddr");

   ---------------------------------
   -- Pthread_Create (16 Threads) --
   ---------------------------------
   function Pthread_Create (Thread        : access K.Task_Id;
                            Attr          : TO.Pthread_Attr_T_Ac_Const;
                            Start_Routine : TO.Task_Body_Function;
                            Arg           : System.Address)
                            return Int
     renames TO.Pthread_Create;

   ------------------
   -- Pthread_Join --
   ------------------
   function Pthread_Join (Thread    : K.Task_Id;
                          Value_Ptr : TO.System_Address_Ac) return Int
     renames TO.Pthread_Join;

   --------------------
   -- Pthread_Detach --
   --------------------
   function Pthread_Detach (Thread : K.Task_Id) return Int
     renames TO.Pthread_Detach;

   ------------------
   -- Pthread_Exit --
   ------------------
   procedure Pthread_Exit (Value_Ptr : System.Address)
     renames TO.Pthread_Exit;

   -------------------------------
   -- Pthread_Self (16 Threads) --
   -------------------------------
   function Pthread_Self return K.Task_Id
     renames K.Scheduler.Self;

   --------------------------------
   -- Pthread_Equal (16 Threads) --
   --------------------------------
   function Pthread_Equal (T1 : K.Task_Id;
                           T2 : K.Task_Id) return Int;
   pragma Export (C, Pthread_Equal, "pthread_equal");

   ------------------------------------
   --  Pthread_Attr_Setinheritsched  --
   ------------------------------------
   function Pthread_Attr_Setinheritsched (Attr         : access Pthread_Attr_T;
                                          Inheritsched : Int)
                                         return Int
     renames TO_ATTR.Pthread_Attr_Setinheritsched;

   --------------------------------------------------------------
   -- Pthread_Attr_Setscope (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   --
   --  Returns ENOSYS.
   function Pthread_Attr_Setscope
     (Attr            : access TO.Pthread_Attr_T;
      contentionscope : Int) return Int;
   pragma Export (C, Pthread_Attr_Setscope, "pthread_attr_setscope");

   --------------------------------------------------------------
   -- Pthread_Attr_Getscope (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   --
   --  Returns ENOSYS.
   function Pthread_Attr_Getscope
     (Attr            : access TO.Pthread_Attr_T;
      contentionscope : access Int) return Int;
   pragma Export (C, Pthread_Attr_Getscope, "pthread_attr_getscope");

   -------------------------------------------------------------------
   -- Pthread_Attr_Setschedparam (13.5 Thread Scheduling Functions) --
   -------------------------------------------------------------------
   function Pthread_Attr_Setschedparam (Attr   : access TO.Pthread_Attr_T;
                                        Param  : access TO.Sched_Param)
                                        return Int
     renames TO_ATTR.Pthread_Attr_Setschedparam;

   -------------------------------------------------------------------
   -- Pthread_Attr_Getschedparam (13.5 Thread Scheduling Functions) --
   -------------------------------------------------------------------
   function Pthread_Attr_Getschedparam (Attr   : access TO.Pthread_Attr_T;
                                        Param  : access TO.Sched_Param)
                                        return Int
     renames TO_ATTR.Pthread_Attr_Getschedparam;

   --------------------------------------------------------------
   -- Pthread_Setschedparam (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   function Pthread_Setschedparam (T      : Task_Id;
                                   Policy : Scheduling_Policies;
                                   Param  : access TO.Sched_Param)
                                   return Int
     renames TO.Pthread_Setschedparam;

   function Pthread_Setschedparam_Gnat
     (T              : Task_Id;
      Policy         : Scheduling_Policies;
      Sched_Priority : Task_Priority) return Int;
   --  Gnat only knows about the 'sched_priority' param. So the others
   --  must be set "by hand" internally in this function.

   --------------------------------------------------------------
   -- Pthread_Getschedparam (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   function Pthread_Getschedparam (T      : Task_Id;
                                   Policy : access Scheduling_Policies;
                                   Param  : access TO.Sched_Param)
                                   return Int
     renames TO.Pthread_Getschedparam;

   ----------------------------------------------------------------------------
   --  POSIX.1c  Section 17  --------------------------------------------------
   ----------------------------------------------------------------------------

   --------------------------
   -- Type 'Pthread_Key_T' --
   --------------------------
   subtype Pthread_Key_T is MaRTE.Configuration_Parameters.Keys_Range;
   type Pthread_Key_T_Ac is access all Pthread_Key_T;

   ------------------------
   -- Pthread_Key_Create --
   ------------------------
   --
   --  'Destructor' must be null (functionality not implemented yet).
   function Pthread_Key_Create (Key        : Pthread_Key_T_Ac;
                                Destructor : System.Address) return Int;
   pragma Export (C, Pthread_Key_Create, "pthread_key_create");
   pragma Export_Function (Pthread_Key_Create, "pthread_key_create",
                             Mechanism => (Key        => Value,
                                           Destructor => Value));

   -------------------------
   -- Pthread_Setspecific --
   -------------------------
   function Pthread_Setspecific (Key   : Pthread_Key_T;
                                 Value : System.Address) return Int;
   pragma Export (C, Pthread_Setspecific, "pthread_setspecific");

   -----------------------------
   -- Pthread_Setspecific_For --
   -----------------------------
   function Pthread_Setspecific_For (Key   : Pthread_Key_T;
                                     T     : K.Task_Id;
                                     Value : System.Address) return Int;
   pragma Export (C, Pthread_Setspecific_For, "pthread_setspecific_for");

   -------------------------
   -- Pthread_Getspecific --
   -------------------------
   function Pthread_Getspecific (Key : Pthread_Key_T) return System.Address;
   pragma Export (C, Pthread_Getspecific, "pthread_getspecific");

   ------------------------------
   -- Pthread_Getspecific_From --
   ------------------------------
   function Pthread_Getspecific_From (Key   :        Pthread_Key_T;
                                      T     :        K.Task_Id;
                                      Value : access System.Address)
                                      return Int;
   pragma Export (C, Pthread_Getspecific_From, "pthread_getspecific_from");

   ------------------------
   -- Pthread_Key_Delete --
   ------------------------
   function Pthread_Key_Delete (Key : Pthread_Key_T) return Int;
   pragma Export (C, Pthread_Key_Delete, "pthread_key_delete");

end MaRTE.POSIX_Pthread;
