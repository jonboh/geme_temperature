------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T i m e r s . I n t e r n a l s'
--
--                                 Body
--
--
--  File 'k-timers-internals.adb'                                      By MAR.
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
with Ada.Unchecked_Conversion;

with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.Kernel.Task_Sets;
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timers.Internals is

   package SCHD  renames K.Scheduler;
   package TE_T  renames K.Timed_Events_And_Timer;
   package SI    renames K.Signals.Internals;
   package CP    renames MaRTE.Configuration_Parameters;
   package TS    renames K.Task_Sets;
   package DBG   renames K.Debug;

   use K.Tasks_Lists;
   use K.CPU_Time_Timed_Events_Lists;
   use K.CPU_Time_Timed_Events_Lists_Order;

   use type HAL.HWTime;

   ----------------------------
   --  Is_CPU_Clock_Of_Self  --
   ----------------------------
   function Is_CPU_Clock_Of_Self (Clock : Clock_Id) return Boolean is
   begin
      return (Clock = CLOCK_THREAD_CPUTIME_ID or else
              SCHD.Self = To_Task_Id (Clock));
   end Is_CPU_Clock_Of_Self;

   --------------------
   --  Is_CPU_Clock  --
   --------------------
   function Is_CPU_Clock (Clock : Clock_Id) return Boolean is
   begin
      return Task_OK (To_Task_Id (Clock), NEW_APPSCHED);
   end Is_CPU_Clock;
   pragma Inline (Is_CPU_Clock);

   -----------------
   -- Clock_Id_OK --
   -----------------
   function Clock_Id_OK (Clock : Clock_Id) return Boolean is
   begin
      return
        Clock = CLOCK_REALTIME
        or else Clock = CLOCK_MONOTONIC
        or else (CP.Use_CPU_Time_Clocks_And_Timers and then
                 (Clock = CLOCK_THREAD_CPUTIME_ID or else
                  Task_OK (To_Task_Id (Clock), NEW_APPSCHED)))
        or else (CP.Use_Group_Clocks'First and then
                 TS.Task_Set_OK (To_Task_Set_Base_Ac (Clock)));
   end Clock_Id_OK;
   pragma Inline (Clock_Id_OK);

   ------------------------
   -- Is_Group_Clock_Id  --
   ------------------------
   function Is_Group_Clock_Id (Clock : Clock_Id) return Boolean is
   begin
      return Clock /= CLOCK_REALTIME
        and then Clock /= CLOCK_MONOTONIC
        and then (CP.Use_Group_Clocks'First and then
                  TS.Task_Set_OK (To_Task_Set_Base_Ac (Clock)));
   end Is_Group_Clock_Id;
   pragma Inline (Is_Group_Clock_Id);

   -----------------
   -- Timer_Id_OK --
   -----------------
   function Timer_Id_OK (Timer : Timer_Id) return Boolean is
   begin
      return Timer /= null
        and then Timer.Magic = INITIALIZED

      --  Based on a standard clock or valid CPU time timed event
        and then (Timer.Base_Clock = CLOCK_REALTIME
                  or else Timer.Base_Clock = CLOCK_MONOTONIC
                  or else K.CPU_TE_OK
                    (K.CPU_Time_Timed_Event_Ac (Timer)));
   end Timer_Id_OK;
   pragma Inline (Timer_Id_OK);

   ---------------------
   --  Timer_Expired  --
   ---------------------
   --
   --  Called from the timer interrupt handler
   --  'SCHD.Timer_Interrupt_Handler'.
   procedure Timer_Expired (Timer : in Timer_Id; At_Time : in HAL.HWTime) is
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (Timer.Base_Clock)));
      pragma Debug (DBG.Assert (Timer_Id_OK (Timer)));
      --  Is it a one-shot timer ?
      if Timer.State.Interval = 0 then
         --  It is a one-shot timer
         Timer.Armed := False;
         Timer.Task_Where_Queued := null;

      else  --  It is a periodic timer... then rearms it.

         if (Timer.Base_Clock = CLOCK_REALTIME or
             Timer.Base_Clock = CLOCK_MONOTONIC) then
            --  Timer NOT based on a execution-time clock
            Timer.T := Timer.T + Timer.State.Interval;
            TE_T.Enqueue_Standard_Event_Without_Updating_Timer
              (Timed_Event_Ac (Timer));

         else
            --  Execution-Time timer.
            pragma Debug
              (DBG.Assert (Timer.Task_Where_Queued /= null and then
                           Timer.Task_Where_Queued = TCB_Ac (SCHD.Self)));
            Timer.CPU_Time := Timer.CPU_Time + Timer.State.Interval;
            Timer.T := Timer.T + Timer.State.Interval;
            if CP.Use_Group_Clocks'First
              and then Timer.Is_Based_On_Group_Clock
            then
               Timer.Group_Expiration_Time :=
                 Timer.Group_Expiration_Time + Timer.State.Interval;
            end if;
            Enqueue_In_Order (CPU_Time_Timed_Event_Ac (Timer),
                              Timer.Task_Where_Queued.CPU_Time_TEs_Q);
         end if;
      end if;

      --  Perform timer action
      case Timer.Event.Event_Notification is
         when K.Signals.SIGNAL_NOTIFICATION =>
            if (not
                SI.Is_Reserved_Signal_Instance_Pending (Timer.Siginst_Ac)) then
               Timer.Overruns := 0;
               SI.Send_Signal_To_Process (Timer.Siginst_Ac);
            else
               Timer.Overruns := Timer.Overruns + 1;
            end if;
         when K.Signals.NO_NOTIFICATION =>
            null;
      end case;
   end Timer_Expired;

   --------------------
   --  Disarm_Timer  --
   --------------------
   procedure Disarm_Timer (Timer : in Timer_Id) is
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (Timer.Base_Clock)));
      Timer.Armed := False;
      if (Timer.Base_Clock = CLOCK_REALTIME or
          Timer.Base_Clock = CLOCK_MONOTONIC) then
         TE_T.Remove_Standard_Event (Timed_Event_Ac (Timer));

      else
         --  Execution-Time timer.
         pragma Debug (DBG.Assert (Is_In_The_List
                                   (CPU_Time_Timed_Event_Ac (Timer),
                                    Timer.Task_Where_Queued.CPU_Time_TEs_Q)));
         Dequeue (CPU_Time_Timed_Event_Ac (Timer),
                  Timer.Task_Where_Queued.CPU_Time_TEs_Q);
         Timer.Task_Where_Queued := null;
      end if;
   end Disarm_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Now       : in  HAL.HWTime) is
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (Timer.Base_Clock)));
      --  If armed disarm first
      if Timer.Armed then
         Disarm_Timer (Timer);
      end if;
      --  Initialize timer fields
      Timer.State := New_State;
      Timer.Armed := True;

      if (Timer.Base_Clock = CLOCK_REALTIME or
          Timer.Base_Clock = CLOCK_MONOTONIC) then
         Set_Time_Event (TE_Ac => K.Timed_Event_Ac (Timer),
                         T => Timer.State.Initial,
                         Abs_Time => (ABSOLUTE_TIMER and Options) /= 0,
                         Clock => Timer.Base_Clock,
                         Now => Now);
         --  At this point in 'Timer.T' is the timer expiration ABSOLUTE
         --  time respect the hardware clock (monotonic clock that measures
         --  time from system reset).
         if Timer.T < Now then
            --  The expiration time is before the current time, then it is
            --  normalized to expire just now.
            Timer.T := Now;
         end if;
         TE_T.Enqueue_Standard_Event_And_Update_Timer (Timed_Event_Ac (Timer),
                                                       Now);

      else
         --  CPU-time timer (including group timers)
         Set_CPU_Time_Event (K.CPU_Time_Timed_Event_Ac (Timer),
                             Timer.State.Initial,
                             (ABSOLUTE_TIMER and Options) /= 0,
                             Now);

         --  Enqueue timer in a "CPU_Time_TEs_Q"
         if (To_Task_Id (Timer.Base_Clock) = SCHD.Self or
               To_Task_Set_Base_Ac (Timer.Base_Clock) = SCHD.Self.Set) then
            --  Timer based on the CPU-time clock of the running task or the
            --  group-clock of the running task
            Timer.Task_Where_Queued := TCB_Ac (SCHD.Self);
            Enqueue_In_Order (CPU_Time_Timed_Event_Ac (Timer),
                              SCHD.Self.CPU_Time_TEs_Q);
            TE_T.Update_Timer_Due_To_New_CPU_Event (Timed_Event_Ac (Timer),
                                                    Now);
         else
            if not Timer.Is_Based_On_Group_Clock then
               --  Timer based on the CPU-time of another task: just enqueue it
               Timer.Task_Where_Queued :=
                 TCB_Ac (To_Task_Id (Timer.Base_Clock));
               Enqueue_In_Order (CPU_Time_Timed_Event_Ac (Timer),
                                 To_Task_Id (Timer.Base_Clock).CPU_Time_TEs_Q);
            end if;
            --  Timers based on a group-clock that doesn't include the running
            --  task will be queued in Do_Scheduling
         end if;
      end if;
   end Arm_Timer;

   -----------------------
   --  Get_Timer_State  --
   -----------------------
   function Get_Timer_State (Timer : Timer_Id;
                             Now : HAL.HWTime) return Timer_State is
      TS : Timer_State;
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (Timer.Base_Clock)));
      if Timer.Armed then
         TS.Interval := Timer.State.Interval;
         if (Timer.Base_Clock = CLOCK_REALTIME or
             Timer.Base_Clock = CLOCK_MONOTONIC) then
            if Timer.T < Now then
               TS.Initial := 0;
            else
               TS.Initial := Timer.T - Now;
            end if;

         else
            --  CPU-time timer (including group timers)
            TS.Initial :=
              Time_Remaining_To_CPU_Time_Event
                (K.CPU_Time_Timed_Event_Ac (Timer),
                 Now);
         end if;

      else
         --  timer disarmed
         TS.Initial := 0;
         TS.Interval := 0;
      end if;

      return TS;
   end Get_Timer_State;

   ----------------------
   --  Set_Time_Event  --
   ----------------------
   --  Sets 'TE_Ac.T' as an ABSOLUTE time respect the hardware clock (monotonic
   --  clock that measures time from system reset).
   --  'TE_Ac.T' is NOT normalized: can return a value of time before the
   --  current time
   procedure Set_Time_Event (TE_Ac    : in K.Timed_Event_Ac;
                             T        : in HAL.HWTime;
                             Abs_Time : in Boolean;
                             Clock    : in Clock_Id;
                             Now      : in HAL.HWTime) is
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (Clock)));
      pragma Debug (DBG.Assert (Clock = CLOCK_REALTIME or
                                Clock = CLOCK_MONOTONIC));

      if Abs_Time then
         --  Absolute time
         if Clock = CLOCK_REALTIME then
            if T < Realtime_Clock_Offset then
               TE_Ac.T := 0;
            else
               TE_Ac.T := T - Realtime_Clock_Offset;
            end if;
         else  --  monotonic clock
            TE_Ac.T := T;
         end if;
      else
         --  Relative time
         TE_Ac.T := Now + T;
      end if;
   end Set_Time_Event;

   --------------------------
   --  Set_CPU_Time_Event  --
   --------------------------
   --
   --  * CPU clock (or group clock) of the running task (absolute)
   --    |.....................|..............................|       Time
   --  Start                  Now                         TE_Ac.T
   --    |....................................................|       CPU-Time
   --  Consumed                                    TE_Ac.CPU_Time = T
   --
   --  * CPU clock (or group clock) of the running task (relative)
   --    |.....................|..............................|       Time
   --  Start                  Now                         TE_Ac.T
   --     < - - - - - - - - - - -   T  - - - - - - - - - - - >
   --    |....................................................|       CPU-Time
   --  Consumed                                           TE_Ac.CPU_Time
   --
   --  * CPU clock (or group clock) of other task (absolute)
   --    |....................................................|       CPU-Time
   --  Consumed                                    TE_Ac.CPU_Time = T
   --
   --  * CPU clock (or group clock) of other task (relative)
   --     < - - - - - - - - - - -   T  - - - - - - - - - - - >
   --    |....................................................|       CPU-Time
   --  Consumed                                           TE_Ac.CPU_Time
   --
   procedure Set_CPU_Time_Event (TE_Ac     : in K.CPU_Time_Timed_Event_Ac;
                                 T         : in HAL.HWTime;
                                 Abs_Time  : in Boolean;
                                 Now       : in HAL.HWTime) is
      Consumed_Time : HAL.HWTime;
      --  Time already consumed by the task (Task.Used_CPU_Time) or by the
      --  group-clock (Set.Consumed_Time). Using this variable allows using the
      --  same code for CPU-Time clocks and for group-clocks
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (TE_Ac.Base_Clock)));
      pragma Debug (DBG.Assert (TE_Ac.Base_Clock /= CLOCK_REALTIME and
                                TE_Ac.Base_Clock /= CLOCK_MONOTONIC));

      if not TE_Ac.Is_Based_On_Group_Clock then
         --  Thread CPU-time clock
         pragma Debug
           (DBG.Assert (K.Task_OK (To_Task_Id (TE_Ac.Base_Clock),
                                        NEW_APPSCHED)));
         Consumed_Time := To_Task_Id (TE_Ac.Base_Clock).Used_CPU_Time;
      else
         --  Group clock
         pragma Debug (DBG.Assert (TS.Task_Set_OK
                                   (To_Task_Set_Base_Ac (TE_Ac.Base_Clock))));
         Consumed_Time := To_Task_Set_Id (TE_Ac.Base_Clock).Consumed_Time;
      end if;

      if To_Task_Id (TE_Ac.Base_Clock) = SCHD.Self or
        To_Task_Set_Base_Ac (TE_Ac.Base_Clock) = SCHD.Self.Set then

         --  CPU clock (or group clock) of the running task
         if Abs_Time then
            --  Absolute time
            TE_Ac.CPU_Time := T;
            if T < Consumed_Time then
               --  Time in the past
               TE_Ac.T := SCHD.Self.Running_Starting_Time
                 - (Consumed_Time - T);
            else
               --  Standard case: time in the future
               TE_Ac.T := SCHD.Self.Running_Starting_Time
                 + T - Consumed_Time;
            end if;
         else
            --  Relative time
            TE_Ac.CPU_Time := Consumed_Time +
              Now - SCHD.Self.Running_Starting_Time + T;
            TE_Ac.T := Now + T;
         end if;

      else
         --  CPU clock (or group clock) of another task
         --  TE_Ac.T si also set since some functions check if TE_Ac.T < now to
         --  perform the action inmediately in that case
         TE_Ac.T := HAL.HWTime'Last;
         if Abs_Time then
            TE_Ac.CPU_Time := T;
            if T < Consumed_Time then
               TE_Ac.T := HAL.HWTime'First;  --  time has passed
            end if;
         else
            TE_Ac.CPU_Time := Consumed_Time + T;
         end if;
      end if;

      if CP.Use_Group_Clocks'First
        and then TE_Ac.Is_Based_On_Group_Clock
      then
         --  Set Group_Expiration_Time
         TE_Ac.Group_Expiration_Time := TE_Ac.CPU_Time;
      end if;
   end Set_CPU_Time_Event;

   ----------------------------------------
   --  Time_Remaining_To_CPU_Time_Event  --
   ----------------------------------------
   --  Returns the execution time interval that remains until the CPU even
   --  TE_Ac would expire
   --
   --  * CPU clock (or group clock) of the running task
   --    |.....................|..............................|       Time
   --  Start                  Now                         TE_Ac.T
   --                           < - - - -  Remaining - - - - >
   --    |....................................................|       CPU-Time
   --  Consumed                                           TE_Ac.CPU_Time
   --
   --  * CPU clock (or group clock) of other task
   --     < - - - - - - - - -   Remaining  - - - - - - - - - >
   --    |....................................................|       CPU-Time
   --  Consumed                                           TE_Ac.CPU_Time
   --
   function Time_Remaining_To_CPU_Time_Event
     (TE_Ac     : K.CPU_Time_Timed_Event_Ac;
      Now       : HAL.HWTime)
      return HAL.HWTime is
      Consumed_Time : HAL.HWTime;
      --  Time already consumed by the task (Task.Used_CPU_Time) or by the
      --  group-clock (Set.Consumed_Time). Using this variable allows using the
      --  same code for CPU-Time clocks and for group-clocks
   begin
      pragma Debug (DBG.Assert (Clock_Id_OK (TE_Ac.Base_Clock)));
      pragma Debug (DBG.Assert (TE_Ac.Base_Clock /= CLOCK_REALTIME and
                                TE_Ac.Base_Clock /= CLOCK_MONOTONIC));

      if not TE_Ac.Is_Based_On_Group_Clock then
         --  Thread CPU-time clock
         pragma Debug
           (DBG.Assert (K.Task_OK (To_Task_Id (TE_Ac.Base_Clock))));
         Consumed_Time := To_Task_Id (TE_Ac.Base_Clock).Used_CPU_Time;
      else
         --  Group clock
         pragma Debug (DBG.Assert (TS.Task_Set_OK
                                   (To_Task_Set_Base_Ac (TE_Ac.Base_Clock))));
         Consumed_Time := To_Task_Set_Id (TE_Ac.Base_Clock).Consumed_Time;
      end if;

      if TE_Ac.Task_Where_Queued = TCB_Ac (SCHD.Self) then

         --  CPU clock (or group clock) of the running task
         if TE_Ac.T  < Now then
            return 0;
         else
            return TE_Ac.T - Now;
         end if;

      else
         --  CPU clock (or group clock) of other task
         if TE_Ac.CPU_Time < Consumed_Time then
            return 0;
         else
            return TE_Ac.CPU_Time - Consumed_Time;
         end if;
      end if;
   end Time_Remaining_To_CPU_Time_Event;

end MaRTE.Kernel.Timers.Internals;
