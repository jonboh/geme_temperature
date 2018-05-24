------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'K e r n e l . T i m e r s'
--
--                                   Body
--
--
--
--  File 'k-timers.adb'                                                By MAR.
--
--  Timers management (including CPU-Time timers).
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
with Ada.Unchecked_Conversion;
with MaRTE.Kernel.Timers.Internals;
with MaRTE.Kernel.Timer_Timed_Events_Pool;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Task_Sets;
with MaRTE.Configuration_Parameters;
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timers is

   use type HWI.HWTime;
   package TOI  renames K.Tasks_Operations.Internals;
   package SCHD renames K.Scheduler;
   package SI   renames K.Signals.Internals;
   package CP   renames MaRTE.Configuration_Parameters;
   package TS   renames K.Task_Sets;
   package TE_LST           renames K.Timed_Events_Lists;
   package CPU_TE_LST       renames K.CPU_Time_Timed_Events_Lists;
   package DBG  renames K.Debug;

   use K.Tasks_Lists;
   use K.CPU_Time_Timed_Events_Lists;
   use K.CPU_Time_Timed_Events_Lists_Order;
   use K.Signals.Global.Signal_Instances_Lists;

   use type Int;

   ------------------
   --  RTC_Offset  --
   ------------------
   RTC_Offset : HWI.HWTime;
   RTC_Offset_Initialized : Boolean := False;

   -----------------------------
   --  Realtime_Clock_Offset  --
   -----------------------------
   function Realtime_Clock_Offset return HWI.HWTime is
   begin
      pragma Debug (DBG.Assert (RTC_Offset_Initialized));
      return RTC_Offset;
   end Realtime_Clock_Offset;
   pragma Inline (Realtime_Clock_Offset);

   --------------------------------------------------
   -- Conversions between 'Task_Id' and 'Clock_Id' --
   --------------------------------------------------
   function To_Task_Id is
     new Ada.Unchecked_Conversion (Clock_Id, K.Task_Id);
   function To_Clock_Id is
     new Ada.Unchecked_Conversion (K.Task_Id, Clock_Id);

   ---------------------------------------------
   -- Conversion between 'Timer_Id' and 'Int' --
   ---------------------------------------------
   function Timer_Id_To_Int is new Ada.Unchecked_Conversion (Timer_Id, Int);

   ----------------
   --  Set_Time  --
   ----------------
   procedure Set_Time (Clock : in Clock_Id;
                       Value : in HWI.HWTime) is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if (Clock = CLOCK_MONOTONIC or else
          not Internals.Clock_Id_OK (Clock)) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return;
      end if;
      TOI.Reset_POSIX_Error;

      if Clock = CLOCK_REALTIME then
         RTC_Offset := Value - HWI.Get_HWTime;

      elsif (Clock = CLOCK_THREAD_CPUTIME_ID or else
             To_Task_Id (Clock) = SCHD.Self) then
         --  The clock is associated with the running task
         SCHD.Self.Running_Starting_Time := HWI.Get_HWTime;
         SCHD.Self.Used_CPU_Time := Value;

      elsif K.Task_OK (To_Task_Id (Clock)) then
         --  The clock is associated with a task other than the running
         To_Task_Id (Clock).Used_CPU_Time := Value;

      elsif TS.Task_Set_OK (Internals.To_Task_Set_Base_Ac (Clock)) then
         TOI.Set_POSIX_Error (OPERATION_NOT_SUPPORTED);
         K.Leave_Critic_Section (Flags);
         return;

      else
         pragma Debug (DBG.Assert (False));
         null;

      end if;
      K.Leave_Critic_Section (Flags);
   end Set_Time;

   ----------------
   --  Get_Time  --
   ----------------
   function Get_Time (Clock : Clock_Id) return HWI.HWTime is
      Flags    : Integer;
      Ret_Time : HWI.HWTime;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Clock_Id_OK (Clock) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;
      TOI.Reset_POSIX_Error;

      if Clock = CLOCK_REALTIME then
         Ret_Time := HWI.Get_HWTime + Realtime_Clock_Offset;

      elsif Clock = CLOCK_MONOTONIC then
         Ret_Time := HWI.Get_HWTime;

      elsif (Clock = CLOCK_THREAD_CPUTIME_ID or else
             To_Task_Id (Clock) = SCHD.Self) then
         --  CPU clock of the running task
         Ret_Time := (SCHD.Self.Used_CPU_Time + HWI.Get_HWTime)
           - SCHD.Self.Running_Starting_Time;

      elsif K.Task_OK (To_Task_Id (Clock), NEW_APPSCHED) then
         --  The clock is associated with a task other than the running
         Ret_Time := To_Task_Id (Clock).Used_CPU_Time;

      elsif Internals.To_Task_Set_Base_Ac (Clock) = SCHD.Self.Set then
         --  Group clock of the running task
         Ret_Time := (SCHD.Self.Set.Consumed_Time + HWI.Get_HWTime)
           - SCHD.Self.Running_Starting_Time;

      elsif TS.Task_Set_OK (Internals.To_Task_Set_Base_Ac (Clock)) then
         --  Group clock that does NOT include the running task
         Ret_Time := Internals.To_Task_Set_Base_Ac (Clock).Consumed_Time;

      else
         pragma Debug (DBG.Assert (False));
         null;

      end if;
      K.Leave_Critic_Section (Flags);
      return Ret_Time;
   end Get_Time;

   -------------------------------
   --  Get_RealTime_Clock_Gnat  --
   -------------------------------

   function Get_RealTime_Clock_Gnat return Duration is
      Flags    : Integer;
      Ret_Time : Duration;
   begin
      K.Enter_Critic_Section (Flags);
      TOI.Reset_POSIX_Error;

      Ret_Time :=
        HWI.HWTime_To_Duration (HWI.Get_HWTime + Realtime_Clock_Offset);

      K.Leave_Critic_Section (Flags);
      return Ret_Time;
   end Get_RealTime_Clock_Gnat;

   --------------------------------
   --  Get_Monotonic_Clock_Gnat  --
   --------------------------------

   function Get_Monotonic_Clock_Gnat return Duration is
   begin
      pragma Debug
        (DBG.Assert (CP."/=" (CP.MaRTE_Architecture'First, CP.ARCH_LINUX_LIB)));
      --  In Linux_lib architecture the monotonic clock and the real-time clock
      --  are both based on gettimeofday

      return HWI.HWTime_To_Duration (HWI.Get_HWTime_Slow);
   end Get_Monotonic_Clock_Gnat;

   ----------------------
   --  Get_Resolution  --
   ----------------------
   function Get_Resolution (Clock : Clock_Id) return Duration is
      Flags    : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Clock_Id_OK (Clock) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return 0.0;
      end if;
      TOI.Reset_POSIX_Error;

      K.Leave_Critic_Section (Flags);

      return 1.0 / Duration (HWI.Get_HWClock_Frequency);
   end Get_Resolution;

   ---------------------------
   -- Pthread_Getcpuclockid --
   ---------------------------
   function Pthread_Getcpuclockid (T     : in     Task_Id;
                                   Clock : access Clock_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not MaRTE.Configuration_Parameters.Use_CPU_Time_Clocks_And_Timers then
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;
      if not Task_OK (T, NEW_APPSCHED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      Clock.all := To_Clock_Id (T);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getcpuclockid;

   --------------------
   --  Create_Timer  --
   --------------------
   function Create_Timer (Clock         : Clock_Id;
                          Event         : Signals.Signal_Event;
                          Default_Event : Boolean := False) return Timer_Id is
      Timer : Timer_Id;
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Clock_Id_OK (Clock) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return null;
      end if;

      --  Request Timer event and Signal Instance
      Timer := K.Timer_Timed_Events_Pool.Request_Timer_Timed_Event;
      if Timer = null then
         TOI.Set_POSIX_Error (RESOURCE_TEMPORARILY_UNAVAILABLE);
         K.Leave_Critic_Section (Flags);
         return null;
      end if;
      Timer.Siginst_Ac := SI.Request_And_Reserve_Signal_Instance;
      if Timer.Siginst_Ac = null then
         TOI.Set_POSIX_Error (RESOURCE_TEMPORARILY_UNAVAILABLE);
         K.Leave_Critic_Section (Flags);
         return null;
      end if;
      TOI.Reset_POSIX_Error;

      --  Set timer fields
      Timer.Is_Based_On_Group_Clock := False;
      if Clock = CLOCK_THREAD_CPUTIME_ID then
         Timer.Base_Clock := To_Clock_Id (SCHD.Self);
      else
         Timer.Base_Clock := Clock;

         --  Is based on a group-clock?
         if Internals.Is_Group_Clock_Id (Clock) then
            --  Link set to this timed event
            Internals.To_Task_Set_Id (Clock).Group_TE_Ac :=
              CPU_Time_Timed_Event_Ac (Timer);

            Timer.Is_Based_On_Group_Clock := True;
         end if;
      end if;
      if Signals."=" (Event.Event_Notification,
                      Signals.SIGNAL_NOTIFICATION) then
         Timer.Siginst_Ac.Signo   := Event.Event_Signal;
         Timer.Siginst_Ac.Code    := Signals.SI_TIMER;
         if Default_Event then
            Timer.Siginst_Ac.Value := Timer_Id_To_Int (Timer);
         else
            Timer.Siginst_Ac.Value := Event.Event_Sigval;
         end if;
      end if;

      --  Assign CPU_Time_Timed_Event fields
      K.CPU_Time_Timed_Event (Timer.all) :=
        (TE_LST.Element (Timer.all) with
         CPU_Time => 0,
         Group_Expiration_Time => 0,
         Is_Based_On_Group_Clock => Timer.Is_Based_On_Group_Clock,
         Base_Clock => Timer.Base_Clock,
         Task_Where_Queued => null,
         Armed => False);

      --  Assign Timer fields
      Timer.all :=
        (CPU_TE_Lists.Element (Timer.all) with
         State => (0, 0),
         Event => Event,
         Overruns => 0,
         Magic => INITIALIZED,
         Siginst_Ac => Timer.Siginst_Ac);

      K.Leave_Critic_Section (Flags);
      return Timer;
   end Create_Timer;

   --------------------
   --  Timer_Delete  --
   --------------------
   function Timer_Delete (Timer : Timer_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      if Timer.Armed and then Internals.Clock_Id_OK (Timer.Base_Clock) then
         --  Disarm timer
         Internals.Disarm_Timer (Timer);
      end if;

      if CP.Use_Group_Clocks'First and then
        Timer.Is_Based_On_Group_Clock
      then
         --  Unlink set from this timed event
         pragma Debug
           (DBG.Assert (TS.Task_Set_OK
                        (Internals.To_Task_Set_Base_Ac (Timer.Base_Clock))));
         Internals.To_Task_Set_Id (Timer.Base_Clock).Group_TE_Ac := null;
      end if;

      Timer.Magic := FINALIZED;
      K.Timer_Timed_Events_Pool.Set_Timer_Timed_Event_Free (Timer);
      SI.Cancel_Reserve_And_Release_Signal_Instance (Timer.Siginst_Ac);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Timer_Delete;

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Old_State : out Timer_State) is
      Flags : Integer;
      Now   : HWI.HWTime;
   begin
      K.Enter_Critic_Section (Flags);
      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return;
      end if;
      pragma Debug (DBG.Assert (Timer.Base_Clock /= CLOCK_THREAD_CPUTIME_ID));
      TOI.Reset_POSIX_Error;

      Now := HWI.Get_HWTime;
      Old_State := Internals.Get_Timer_State (Timer, Now);
      Internals.Arm_Timer (Timer, Options, New_State, Now);

      K.Leave_Critic_Section (Flags);
   end Arm_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State) is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return;
      end if;
      pragma Debug (DBG.Assert (Timer.Base_Clock /= CLOCK_THREAD_CPUTIME_ID));
      TOI.Reset_POSIX_Error;

      Internals.Arm_Timer (Timer, Options, New_State, HWI.Get_HWTime);

      K.Leave_Critic_Section (Flags);
   end Arm_Timer;

   -----------------------
   --  Get_Timer_State  --
   -----------------------
   function Get_Timer_State (Timer : Timer_Id) return Timer_State is
      TS : Timer_State;
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return Timer_State'(0, 0);
      end if;
      TOI.Reset_POSIX_Error;

      TS := Internals.Get_Timer_State (Timer, HWI.Get_HWTime);

      K.Leave_Critic_Section (Flags);
      return TS;
   end Get_Timer_State;

   --------------------
   --  Disarm_Timer  --
   --------------------
   procedure Disarm_Timer (Timer : in Timer_Id) is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return;
      end if;
      TOI.Reset_POSIX_Error;

      if Timer.Armed then
         Internals.Disarm_Timer (Timer);
      end if;

      K.Leave_Critic_Section (Flags);
   end Disarm_Timer;

   ------------------------
   --  Timer_Getoverrun  --
   ------------------------
   function Timer_Getoverrun (Timer : Timer_Id) return Int is
      Flags    : Integer;
      Overruns : Natural;
   begin
      K.Enter_Critic_Section (Flags);
      if not Internals.Timer_Id_OK (Timer) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      Overruns := Timer.Overruns;
      K.Leave_Critic_Section (Flags);

      return Int (Overruns);
   end Timer_Getoverrun;

   ----------------------------------------
   --  Initialize_Realtime_Clock_Offset  --
   ----------------------------------------
   procedure Initialize_Realtime_Clock_Offset is
   begin
      pragma Debug (DBG.Assert (not RTC_Offset_Initialized));
      if HWI.RTC_Available then
         RTC_Offset := HWI.RTC_HWTime_Since_Epoch;
         RTC_Offset := RTC_Offset - HWI.Get_HWTime;
      else
         --  CLOCK_REALTIME and CLOCK_MONOTONIC will start being the same.
         RTC_Offset := 0;
      end if;

      RTC_Offset_Initialized := True;
   end Initialize_Realtime_Clock_Offset;

end MaRTE.Kernel.Timers;
