------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . T a s k s _ O p e r a t i o n s'
--
--                                 Spec
--
--
--  File 'k-tasks_operation.ads'                                      By MAR.
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
with System;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Timespec;
with MaRTE.Configuration_Parameters;

package MaRTE.Kernel.Tasks_Operations is

   package K renames MaRTE.Kernel;

   package GC   renames MaRTE.POSIX_Constants;
   package CP   renames MaRTE.Configuration_Parameters;
   package TSPC renames MaRTE.Timespec;

   subtype Task_Id is K.Task_Id;

   Null_Task_Id : Task_Id renames K.Null_Task_Id;

   -------------------------------------------------------------------------
   --  Type 'Sched_Param'  -------------------------------------------------
   -------------------------------------------------------------------------
   --
   --  Ada version of 'struct sched_param' defined in 'sched.h'.  Any
   --  modification here should be synchronized with the related type
   --  in 'sched.h'.
   type Sched_Param is record
       --  Scheduling priority (only sched. param used in FIFO and RR)
      Sched_Priority : Task_Priority;

      --  Sporadic Server policy parameters
      Sched_SS_Low_Priority : Task_Priority;
      Sched_SS_Repl_Period  : MaRTE.Timespec.Timespec;
      Sched_SS_Init_Budget  : MaRTE.Timespec.Timespec;
      Sched_SS_Max_Repl     : Natural;
   end record;
   pragma Convention (C, Sched_Param);

   -------------------------------------------------------------------------
   --  Type 'Pthread_Attr_T'  ----------------------------------------------
   -------------------------------------------------------------------------

   subtype Scheduling_Policies is K.Scheduling_Policies;

   SCHED_FIFO  : Scheduling_Policies renames K.SCHED_FIFO;
   SCHED_RR    : Scheduling_Policies renames K.SCHED_RR;
   SCHED_OTHER : Scheduling_Policies renames K.SCHED_OTHER;
   SCHED_APP   : Scheduling_Policies renames K.SCHED_APP;
   SCHED_EDF   : Scheduling_Policies renames K.SCHED_EDF;

   --  Regular or application-scheduler tasks
   type Regular_Or_App_Scheduler_T is range
     GC.PTHREAD_REGULAR .. GC.PTHREAD_APPSCHEDULER;
   for Regular_Or_App_Scheduler_T'Size use Int'Size;
   PTHREAD_REGULAR : Regular_Or_App_Scheduler_T :=
     MaRTE.POSIX_Constants.PTHREAD_REGULAR;
   PTHREAD_APPSCHEDULER : Regular_Or_App_Scheduler_T :=
     MaRTE.POSIX_Constants.PTHREAD_APPSCHEDULER;

   --  Inherit or explicit scheduling parameters
   subtype Inherit_Or_Explicit_Sched is Int
     range GC.PTHREAD_INHERIT_SCHED .. GC.PTHREAD_EXPLICIT_SCHED;
   PTHREAD_INHERIT_SCHED  : constant := GC.PTHREAD_INHERIT_SCHED;
   PTHREAD_EXPLICIT_SCHED : constant := GC.PTHREAD_EXPLICIT_SCHED;

   --  Magic values to detect invalid attributes objects. It is an
   --  array of characters for easier reading when debugging.
   type Attr_Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Attr_Magic_Check);
   ATTR_NOT_INITIALIZED : constant Attr_Magic_Check := ('t', 'a', 'N', 'i');
   ATTR_INITIALIZED     : constant Attr_Magic_Check := ('T', 'A', '_', 'I');

   type Pthread_Attr_T is record
      Magic         : Attr_Magic_Check := ATTR_NOT_INITIALIZED;
      Sched_Policy  : Scheduling_Policies := SCHED_FIFO;
      Detach_State  : Detach_State_T := PTHREAD_CREATE_JOINABLE;
      Inherit_Sched : Inherit_Or_Explicit_Sched := PTHREAD_EXPLICIT_SCHED;
      Param         : Sched_Param :=
        Sched_Param'(Sched_Priority      => Task_Priority'First,
                     Sched_SS_Low_Priority => Task_Priority'First,
                     Sched_SS_Repl_Period  => (0, 0),
                     Sched_SS_Init_Budget  => (0, 0),
                     Sched_SS_Max_Repl     => 0);
      Stack_Size : MaRTE.Integer_Types.Size_T :=
        CP.Stack_Size_In_Bytes;
      Rel_Deadline     : HAL.HWTime := HAL.HWTime'Last;
      Preemption_Level : Task_Preemption_Level := Task_Preemption_Level'First;
      App_Scheduler       : Task_Id := null;
      AppSched_Param      : K.AppSched_Param_T := (others => 0);
      AppSched_Param_Size : K.AppSched_Param_Size_T := 0;
      App_Scheduler_State : Regular_Or_App_Scheduler_T := PTHREAD_REGULAR;
      --  Regular or app scheduler task
   end record;
   type Pthread_Attr_T_Ac_Const is access constant Pthread_Attr_T;

   -------------------
   -- Attr_Deffault --
   -------------------
   Attr_Deffault : constant Pthread_Attr_T :=
     (Magic         => ATTR_INITIALIZED,
      Sched_Policy  => SCHED_FIFO,
      Detach_State  => PTHREAD_CREATE_JOINABLE,
      Inherit_Sched => PTHREAD_EXPLICIT_SCHED,
      Param  => Sched_Param'(Sched_Priority => Task_Priority'First,
                             Sched_SS_Low_Priority => Task_Priority'First,
                             Sched_SS_Repl_Period  => (0, 0),
                             Sched_SS_Init_Budget  => (0, 0),
                             Sched_SS_Max_Repl     => 0),
      Stack_Size          => CP.Stack_Size_In_Bytes,
      Rel_Deadline        => HAL.HWTime'Last,
      Preemption_Level    => Task_Preemption_Level'Last,
      App_Scheduler       => null,
      AppSched_Param      => (others => 0),
      AppSched_Param_Size => 0,
      App_Scheduler_State => PTHREAD_REGULAR);

   ------------------------------------
   -- Error_In_Scheduling_Parameters --
   ------------------------------------
   --
   --  Check the 'Policy' and 'Parameters'. Called from
   --  'Pthread_Created', 'TO_ATTR.Pthread_Attr_Setschedparam' and
   --  'Pthread_Setschedparam'.
   function Error_In_Scheduling_Parameters (Param  : in Sched_Param;
                                            Policy : in Scheduling_Policies)
                                            return K.Error_Code;

   ---------------------------
   -- Pthread_Setschedparam --
   ---------------------------
   function Pthread_Setschedparam (T      : Task_Id;
                                   Policy : Scheduling_Policies;
                                   Param  : access Sched_Param) return Int;
   pragma Export (C, Pthread_Setschedparam, "pthread_setschedparam");

   --------------------------
   -- Pthread_Setschedprio --
   --------------------------
   function Pthread_Setschedprio (T    : in Task_Id;
                                  Prio : in MaRTE.Integer_Types.Int)
                                 return Int;
   pragma Export (C, Pthread_Setschedprio, "pthread_setschedprio");

   -------------------------
   -- Pthread_Setdeadline --
   -------------------------

   function Pthread_Setdeadline (T        : K.Task_Id;
                                 Deadline : TSPC.Timespec_Ac;
                                 Immediate : MaRTE.Integer_Types.Int)
                                 return Int;
   pragma Export (C, Pthread_Setdeadline, "pthread_setdeadline");
   pragma Export_Function (Pthread_Setdeadline, "pthread_setdeadline",
                           Mechanism => (T         => Value,
                                         Deadline  => Value,
                                         Immediate => Value));

   -------------------------
   -- Pthread_Getdeadline --
   -------------------------

   function Pthread_Getdeadline (T        : K.Task_Id;
                                 Deadline : TSPC.Timespec_Ac) return Int;
   pragma Export (C, Pthread_Getdeadline, "pthread_getdeadline");
   pragma Export_Function (Pthread_Getdeadline, "pthread_getdeadline",
                           Mechanism => (T       => Value,
                                         Deadline => Value));

   --------------------------------
   -- Pthread_Setpreemptionlevel --
   --------------------------------

   function Pthread_Setpreemptionlevel (T  : K.Task_Id;
                                        PL : K.Task_Preemption_Level)
                                        return Int;
   pragma Export (C, Pthread_Setpreemptionlevel, "pthread_setpreemptionlevel");
   pragma Export_Function (Pthread_Setpreemptionlevel,
                           "pthread_setpreemptionlevel",
                           Mechanism => (T  => Value,
                                         PL => Value));

   --------------------------------
   -- Pthread_Getpreemptionlevel --
   --------------------------------

   function Pthread_Getpreemptionlevel (T     : K.Task_Id;
                                        PL_Ac : access K.Task_Preemption_Level)
                                        return Int;
   pragma Export (C, Pthread_Getpreemptionlevel, "pthread_getpreemptionlevel");
   pragma Export_Function (Pthread_Getpreemptionlevel,
                           "pthread_getpreemptionlevel",
                           Mechanism => (T  => Value,
                                         PL_Ac => Value));

   ---------------------------
   -- Pthread_Getschedparam --
   ---------------------------
   function Pthread_Getschedparam (T      : in Task_Id;
                                   Policy : access Scheduling_Policies;
                                   Param  : access Sched_Param)
                                   return Int;
   pragma Export (C, Pthread_Getschedparam, "pthread_getschedparam");

   --------------------
   -- Pthread_Create --
   --------------------
   subtype Task_Body_Function is K.Task_Body_Function;
   function Pthread_Create (T         : access Task_Id;
                            Attr      : Pthread_Attr_T_Ac_Const;
                            Task_Body : Task_Body_Function;
                            Arg       : System.Address) return Int;
   pragma Export (C, Pthread_Create, "pthread_create");
   pragma Inline (Pthread_Create);

   ------------------
   -- Pthread_Join --
   ------------------
   type System_Address_Ac is access System.Address;
   function Pthread_Join (T : Task_Id; Value_Ptr : System_Address_Ac)
                          return Int;
   pragma Export (C, Pthread_Join, "pthread_join");
   pragma Inline (Pthread_Join);

   --------------------
   -- Pthread_Detach --
   --------------------
   function Pthread_Detach (T : Task_Id) return Int;
   pragma Export (C, Pthread_Detach, "pthread_detach");
   pragma Inline (Pthread_Detach);

   ------------------
   -- Pthread_Exit --
   ------------------
   procedure Pthread_Exit (Value_Ptr : System.Address);
   pragma Export (C, Pthread_Exit, "pthread_exit");
   pragma Inline (Pthread_Exit);

   -----------
   -- Yield --
   -----------
   function Sched_Yield return Int;
   pragma Inline (Sched_Yield);
   pragma Export (C, Sched_Yield, "sched_yield");

   -------------------------------------------------------------------------
   --  Enable/Disable round robin quantum  ---------------------------------
   -------------------------------------------------------------------------

   -----------------------------
   -- MaRTE_Enable_RR_Quantum --
   -----------------------------

   function MaRTE_Enable_RR_Quantum (T : Task_Id) return Int;
   pragma Export (C, MaRTE_Enable_RR_Quantum, "marte_enable_rr_quantum");
   pragma Export_Function (MaRTE_Enable_RR_Quantum, "marte_enable_rr_quantum",
                           Mechanism => (T => Value));

   ------------------------------
   -- MaRTE_Disable_RR_Quantum --
   ------------------------------

   function MaRTE_Disable_RR_Quantum (T : Task_Id) return Int;
   pragma Export (C, MaRTE_Disable_RR_Quantum, "marte_disable_rr_quantum");
   pragma Export_Function
     (MaRTE_Disable_RR_Quantum, "marte_disable_rr_quantum",
      Mechanism => (T => Value));

end MaRTE.Kernel.Tasks_Operations;
