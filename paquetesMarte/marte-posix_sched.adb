------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'S c h e d'
--
--                                   Body
--
--
--
--  File 'sched.adb'                                                   By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'sched.h'.
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
with Ada.Unchecked_Conversion;

with MaRTE.Configuration_Parameters;

with MaRTE.HAL;
with MaRTE.Kernel.Tasks_Operations.Application_scheduler;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Scheduler;

package body MaRTE.POSIX_Sched is

   use K.Application_Scheduling_Data;

   package TO   renames K.Tasks_Operations;
   package TMRS renames K.Timers;
   package SCHD renames K.Scheduler;

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Max (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_Get_Priority_Max (Policy : in Int) return Int is
   begin
      return Int (K.Task_Priority'Last);
   end Sched_Get_Priority_Max;

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Min (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_Get_Priority_Min (Policy : in Int) return Int is
   begin
      return Int (K.Task_Priority'First);
   end Sched_Get_Priority_Min;

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Min (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_RR_Get_Interval (Pid      : in Int;
                                   Interval : in MaRTE.Timespec.Timespec_Ac)
                                   return Int is
   begin
      Interval.all := MaRTE.Timespec.To_Timespec
        (Duration(MaRTE.Configuration_Parameters.Round_Robin_Interval_Time_Period));
      return 0;
   end Sched_RR_Get_Interval;


   ----------------------------------------------------------------------------
   -- Application-defined Scheduling ------------------------------------------
   ----------------------------------------------------------------------------

   function To_Int is new Ada.Unchecked_Conversion (K.Task_Priority, Int);
   function To_Int is new Ada.Unchecked_Conversion
     (K.Mutexes.Mutex_Descriptor, Int);
   function To_Int is new Ada.Unchecked_Conversion (System.Address, Int);
   function To_Int is
     new Ada.Unchecked_Conversion (K.Signals.Signal, Int);
   function To_Int is
     new Ada.Unchecked_Conversion (K.Signals.Cause_Of_Signal, Int);

   function UC (Base : K.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_Data.UC_To_AppScheduler_Data_Ac;
   --------------------------------
   -- Execute Scheduling Actions --
   --------------------------------
   function POSIX_Appsched_Execute_Actions
     (Sched_Actions  : in APPSCHD_DATA.Sched_Actions_Set_Ac;
      Set            : in K.Signals.Signal_Set_Ac_Const;
      Timeout        : in MaRTE.Timespec.Timespec_Ac;
      Current_Time   : in MaRTE.Timespec.Timespec_Ac;
      POSIX_Event_Ac : in Posix_Appsched_Event_Ac) return Int is
      MaRTE_Event_Ac : aliased APPSCHD_DATA.AppSched_Event_Ac;
      Flags : Integer;
      Error : K.Error_Code;
      use type K.Error_Code;
      use type HAL.HWTime;
      use type K.Clock_Id;
   begin
      K.Enter_Critic_Section (Flags);

      --  Execute Scheduling Actions
      if APPSCHD_DATA."/=" (Sched_Actions, null) then
         TO_APPSCHD.Execute_Actions (SCHD.Self, Sched_Actions.all, Error);
         if Error /= K.NO_ERROR then
            K.Leave_Critic_Section (Flags);
            return Error;
         end if;
      end if;

      --  Get a Scheduling Event or wait for it
      if MaRTE.Timespec."/=" (Timeout, null) then
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, Set,
                               MaRTE.Timespec.Timespec_To_HWTime (Timeout.all));
      else
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, Set);
      end if;

      --  Copy information from the MaRTE event to the POSIX event
      POSIX_Event_Ac.Event_Code := MaRTE_Event_Ac.Event_Code;
      POSIX_Event_Ac.T := MaRTE_Event_Ac.T;
      case MaRTE_Event_Ac.Event_Code is

         when APPSCHED_PRIORITY_INHERIT | APPSCHED_PRIORITY_UNINHERIT =>
            POSIX_Event_Ac.Union_Int0 :=
              To_Int (MaRTE_Event_Ac.Sched_Priority);

         when APPSCHED_SIGNAL =>
            POSIX_Event_Ac.Union_Int0 := To_Int (MaRTE_Event_Ac.Siginfo.Signo);
            POSIX_Event_Ac.Union_Int1 := To_Int (MaRTE_Event_Ac.Siginfo.Code);
            POSIX_Event_Ac.Union_Int2 := MaRTE_Event_Ac.Siginfo.Value;

         when APPSCHED_INIT_MUTEX | APPSCHED_DESTROY_MUTEX |
           APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX |
           APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX |
           APPSCHED_CHANGE_SCHED_PARAM_MUTEX =>
            POSIX_Event_Ac.Union_Int0 := To_Int (MaRTE_Event_Ac.M);

         when APPSCHED_EXPLICIT_CALL =>

            POSIX_Event_Ac.Union_Int0 := MaRTE_Event_Ac.User_Event_Code;

         when APPSCHED_EXPLICIT_CALL_WITH_DATA =>

            POSIX_Event_Ac.Union_Int0 := To_Int (MaRTE_Event_Ac.Info);
            POSIX_Event_Ac.Info_Size  := MaRTE_Event_Ac.Info_Size;

         when APPSCHED_NEW | APPSCHED_TERMINATE | APPSCHED_READY |
           APPSCHED_BLOCK | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM |
           APPSCHED_TIMEOUT | APPSCHED_TASK_NOTIFICATION =>
            null;
      end case;

      --  Release the event object
      APPSCHD_DATA.Release_AppSched_Event (MaRTE_Event_Ac);

      --  Record current time
      pragma Assert
        (UC (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME or
         UC (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_MONOTONIC);
      if MaRTE.Timespec."/=" (Current_Time, null) then
         if UC (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME then
            --  Realtime Clock
            Current_Time.all :=
              MaRTE.Timespec.HWTime_To_Timespec (HAL.Get_HWTime +
                                                TMRS.Realtime_Clock_Offset);
         else
            --  Monotonic Clock
            Current_Time.all :=
              MaRTE.Timespec.HWTime_To_Timespec (HAL.Get_HWTime);
         end if;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end POSIX_Appsched_Execute_Actions;


end MaRTE.POSIX_Sched;
