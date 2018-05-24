------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                'T i m e'
--
--                                   Body
--
--
--
--  File 'time.adb'                                                   By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'time.h'.
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

with MaRTE.Kernel; use MaRTE.Kernel;
pragma Elaborate_All (MaRTE.Kernel);
with MaRTE.Kernel.Scheduler;
pragma Elaborate_All (MaRTE.Kernel.Scheduler);
with MaRTE.Kernel.Tasks_Operations;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Operations);
with MaRTE.Kernel.Tasks_Operations.Internals;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Operations.Internals);
with MaRTE.Kernel.Timers;
pragma Elaborate_All (MaRTE.Kernel.Timers);
with MaRTE.Kernel.Debug;
with MaRTE.Configuration_Parameters;

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package body MaRTE.POSIX_Time is

   package TO  renames K.Tasks_Operations;
   package DBG renames K.Debug;
   package CP  renames MaRTE.Configuration_Parameters;

   use type Int;

   function Minus_1_On_Error return Int;
   pragma Inline (Minus_1_On_Error);
   function Minus_1_On_Error return Int is
   begin
      if MaRTE.Kernel.Scheduler.Self.POSIX_Error then
         return -1;
      else
         return 0;
      end if;
   end Minus_1_On_Error;

   -----------------
   -- To_Duration --
   -----------------
   function To_Duration (TV : Timeval) return Duration is
   begin
      return Duration (TV.Tv_Sec) + Duration (TV.Tv_Usec) / 10#1#E6;
   end To_Duration;

   ----------------
   -- To_Timeval --
   ----------------
   function To_Timeval (D : Duration) return Timeval is
      S : Int;
      F : Duration;

   begin
      S := Int (D);
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return Timeval'(Tv_Sec => S, Tv_Usec => Int (F * 10#1#E6));
   end To_Timeval;

   ----------------------------------------------------
   -- Clock_Settime (14.2 Clock and Timer Functions) --
   ----------------------------------------------------
   function Clock_Settime (Clock_Id : Clockid_T;
                           Tp       : MaRTE.Timespec.Timespec_Ac_Const)
                           return Int is
   begin
      Timers.Set_Time (Clock_Id, MaRTE.Timespec.Timespec_To_HWTime (Tp.all));
      return Minus_1_On_Error;
   end Clock_Settime;

   ----------------------------------------------------
   -- Clock_Gettime (14.2 Clock and Timer Functions) --
   ----------------------------------------------------
   function Clock_Gettime (Clock_Id : Clockid_T;
                           Tp       : access MaRTE.Timespec.Timespec)
                           return Int is
   begin
      Tp.all := MaRTE.Timespec.HWTime_To_Timespec (Timers.Get_Time (Clock_Id));
      return Minus_1_On_Error;
   end Clock_Gettime;

   ---------------------------------------------------
   -- Clock_Getres (14.2 Clock and Timer Functions) --
   ---------------------------------------------------
   function Clock_Getres (Clock_Id : Clockid_T;
                          Tp       : access MaRTE.Timespec.Timespec)
                          return Int is
   begin
      Tp.all := MaRTE.Timespec.To_Timespec (Timers.Get_Resolution (Clock_Id));
      return Minus_1_On_Error;
   end Clock_Getres;

   ---------------------------------------------------
   -- Timer_Create (14.2 Clock and Timer Functions) --
   ---------------------------------------------------
   function Timer_Create (Clock_Id  : Clockid_T;
                          Evp       : Signal_Event_Ac_Const;
                          Timerid   : access Timer_T) return Int is
   begin
      if Evp = null then
         Timerid.all :=
           Timers.Create_Timer
             (Clock => Clock_Id,
              Event => (Event_Notification => Signals.SIGNAL_NOTIFICATION,
                        Event_Signal       => Signals.Signal_User_1,
                        Event_Sigval       => 0),
              Default_Event => True);
      else
         Timerid.all := Timers.Create_Timer (Clock_Id, Evp.all);
      end if;
      return Minus_1_On_Error;
   end Timer_Create;

   ---------------------------------------------
   -- Conversions between 'Timer.Timer_State' --
   -- y 'Itimerspec'                          --
   ---------------------------------------------
   function To_Itimerspec (TS : Timers.Timer_State) return Itimerspec;
   function To_Itimerspec (TS : Timers.Timer_State) return Itimerspec is
   begin
      return Itimerspec'
        (It_Value    =>
           MaRTE.Timespec.To_Timespec (HAL.HWTime_To_Duration (TS.Initial)),
         It_Interval =>
           MaRTE.Timespec.To_Timespec (HAL.HWTime_To_Duration (TS.Interval)));
   end To_Itimerspec;
   pragma Inline (To_Itimerspec);

   function To_Timer_State (ITS : Itimerspec) return Timers.Timer_State;
   function To_Timer_State (ITS : Itimerspec) return Timers.Timer_State is
   begin
      return Timers.Timer_State'
        (Initial  =>
           HAL.Duration_To_HWTime (MaRTE.Timespec.To_Duration (ITS.It_Value)),
         Interval => HAL.Duration_To_HWTime
           (MaRTE.Timespec.To_Duration (ITS.It_Interval)));
   end To_Timer_State;
   pragma Inline (To_Timer_State);

   ----------------------------------------------------
   -- Timer_Settime (14.2 Clock and Timer Functions) --
   ----------------------------------------------------
   function Timer_Settime (Timerid : Timer_T;
                           Flags   : Timers.Timer_Options;
                           Value   : Itimerspec_Ac_Const;
                           Ovalue  : Itimerspec_Ac) return Int is
      Old_State : Timers.Timer_State;
   begin
      if Value.It_Value.Tv_Sec = 0 and Value.It_Value.Tv_Nsec = 0 then
         Timers.Disarm_Timer (Timerid);
      else
         if Ovalue /= null then
            Timers.Arm_Timer (Timerid, Flags,
                              To_Timer_State (Value.all), Old_State);
            Ovalue.all := To_Itimerspec (Old_State);
         else
            Timers.Arm_Timer (Timerid, Flags,
                              To_Timer_State (Value.all));
         end if;
      end if;
      return Minus_1_On_Error;
   end Timer_Settime;

   ----------------------------------------------------
   -- Timer_Settime (14.2 Clock and Timer Functions) --
   ----------------------------------------------------
   function Timer_Gettime (Timerid : Timer_T;
                           Value   : Itimerspec_Ac) return Int is
   begin
      Value.all := To_Itimerspec (Timers.Get_Timer_State (Timerid));
      return Minus_1_On_Error;
   end Timer_Gettime;

   ----------
   -- Time --
   ----------
   function Time (Tloc : in Int_Ac) return Int is
      use type CP.Supported_Architectures, HAL.HWTime;
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First /=
                                CP.ARCH_LINUX_LIB));
      if Tloc /= null then
         Tloc.all :=
           Int (HAL.HWTime_To_Duration (HAL.Get_HWTime_Slow +
                                        K.Timers.Realtime_Clock_Offset));
         return Tloc.all;
      else
         return
           Int (HAL.HWTime_To_Duration (HAL.Get_HWTime_Slow +
                                        K.Timers.Realtime_Clock_Offset));
      end if;
   end Time;

   --------------------
   --  Gettimeofday  --
   --------------------
   function Gettimeofday (Tv : access Timeval;
                          Tz : access Timezone) return Int is
      use type CP.Supported_Architectures, HAL.HWTime;
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First /=
                                CP.ARCH_LINUX_LIB));
      Tv.all := To_Timeval
        (HAL.HWTime_To_Duration (HAL.Get_HWTime_Slow +
                                 K.Timers.Realtime_Clock_Offset));
      return 0;
   end Gettimeofday;

end MaRTE.POSIX_Time;
