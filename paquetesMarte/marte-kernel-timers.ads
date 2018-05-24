------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'K e r n e l . T i m e r s'
--
--                                   Spec
--
--
--
--  File 'k-timers.ads'                                                By MAR.
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
with MaRTE.POSIX_Constants;
with MaRTE.Integer_Types;
with MaRTE.Kernel.Signals;
with MaRTE.HAL;
with MaRTE.Kernel.Signals.Global;
--  pragma Elaborate_All (Kernel.Signals.Global);

package MaRTE.Kernel.Timers is

   package K renames MaRTE.Kernel;

   use MaRTE;

   package HWI          renames MaRTE.HAL;
   package CPU_TE_Lists renames K.CPU_Time_Timed_Events_Lists;

   subtype Clock_Id is K.Clock_Id;

   CLOCK_REALTIME          : constant Clock_Id :=
     MaRTE.POSIX_Constants.CLOCK_REALTIME;
   CLOCK_MONOTONIC          : constant Clock_Id :=
     MaRTE.POSIX_Constants.CLOCK_MONOTONIC;
   CLOCK_THREAD_CPUTIME_ID : constant Clock_Id :=
     MaRTE.POSIX_Constants.CLOCK_THREAD_CPUTIME_ID;

   ------------------------------------
   -- Timer_Timed_Event and Timer_Id --
   ------------------------------------
   --  Magic values
   --  To detect invalid Timer_Timed_Event when debugging. It is an array
   --  of characters for easier reading.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   NOT_INITIALIZED : constant Magic_Check := ('t', 't', 'e', 'N');
   INITIALIZED     : constant Magic_Check := ('T', 'T', 'E', 'I');
   FINALIZED       : constant Magic_Check := ('t', 'T', 'e', 'F');

   type Timer_State is record
      Initial  : HWI.HWTime;
      Interval : HWI.HWTime;
   end record;

   type Timer_Timed_Event is new CPU_TE_Lists.Element with record
      State : Timer_State;
      --  Its field 'Initial' stores the absolute time in which the timer
      --  is going to expire (or has expired) for the first time. The
      --  field 'Interval' is the reload value (periodic timer when
      --  nonzero).

      --  Armed : Boolean; Inherit from CPU_TE_Lists.Element
      --  Timer armed currently or not

      Event : Signals.Signal_Event;
      --  Determine the asynchronous notification that will occur when
      --  the timer expires: kind of notification (signal or none),
      --  signal value and an application-defined value.

      Overruns : Natural;
      --  The number of extra expirations between the signal was generated
      --  and it is delivered or accepted.

      Magic : Magic_Check;
      --  Allows detecting invalid timers.

      Siginst_Ac : K.Signals.Global.Signal_Instance_Ac;
      --  Timer associated Signal_Instance, the association is performed
      --  at timer creation time.
   end record;
   type Timer_Id is access all Timer_Timed_Event;

   type Timer_Options is new Unsigned_32;

   ABSOLUTE_TIMER : constant Timer_Options := POSIX_Constants.ABSOLUTE_TIMER;

   -----------------------------
   --  Realtime_Clock_Offset  --
   -----------------------------
   --
   --  Offset between the hardware (monotonic) time get with 'HWI.HWTime'
   --  and the real-time clock (time from epoch). It's initialized in
   --  Initialize_Realtime_Clock_Offset.
   function Realtime_Clock_Offset return HWI.HWTime;
   pragma Inline (Realtime_Clock_Offset);

   -----------------
   --  Set_Time   --
   -----------------
   procedure Set_Time (Clock : in Clock_Id;
                       Value : in HWI.HWTime);

   ----------------
   --  Get_Time  --
   ----------------
   function Get_Time (Clock : Clock_Id) return HWI.HWTime;
   pragma Inline (Get_Time);

   -------------------------------
   --  Get_RealTime_Clock_Gnat  --
   -------------------------------
   --
   --  Used in GNARL file 's-taprop.adb'. It is used by the run-time to support
   --  Ada.Real_Time.Clock. It could be also used by GNARL ('s-osinte.adb' and
   --  's-osprim.adb')
   --  See comment in Get_Monotonic_Clock_Gnat
   function Get_RealTime_Clock_Gnat return Duration;
   pragma Export (C, Get_RealTime_Clock_Gnat,
                  "timers__get_realtime_clock_gnat");

   --------------------------------
   --  Get_Monotonic_Clock_Gnat  --
   --------------------------------
   --
   --  Not used just now. It could be used in GNARL file 's-taprop.adb' to
   --  provide support Ada.Real_Time.Clock. The problem is the rest of the
   --  runtime uses gettimeofday and condition variables (based on realtime
   --  clock). Timing events also use real-time clock, athough they should use
   --  monotonic clock when this problem is solved.
   function Get_Monotonic_Clock_Gnat return Duration;
   pragma Export (C, Get_Monotonic_Clock_Gnat,
                  "timers__get_monotonic_clock_gnat");

   ----------------------
   --  Get_Resolution  --
   ----------------------
   function Get_Resolution (Clock : Clock_Id) return Duration;

   ---------------------------
   -- Pthread_Getcpuclockid --
   ---------------------------
   function Pthread_Getcpuclockid (T     : in     Task_Id;
                                   Clock : access Clock_Id) return Int;
   pragma Export (C, Pthread_Getcpuclockid, "pthread_getcpuclockid");

   --------------------
   --  Create_Timer  --
   --------------------
   function Create_Timer (Clock : Clock_Id;
                          Event : Signals.Signal_Event;
                          Default_Event : Boolean := False) return Timer_Id;
   --------------------
   --  Timer_Delete  --
   --------------------
   function Timer_Delete (Timer : in Timer_Id) return Int;
   pragma Export (C, Timer_Delete, "timer_delete");

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Old_State : out Timer_State);

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State);

   -----------------------
   --  Get_Timer_State  --
   -----------------------
   function Get_Timer_State (Timer : Timer_Id) return Timer_State;

   --------------------
   --  Disarm_Timer  --
   --------------------
   procedure Disarm_Timer (Timer : in Timer_Id);

   ------------------------
   --  Timer_Getoverrun  --
   ------------------------
   function Timer_Getoverrun (Timer : Timer_Id) return Int;
   pragma Export (C, Timer_Getoverrun, "timer_getoverrun");

   ----------------------------------------
   --  Initialize_Realtime_Clock_Offset  --
   ----------------------------------------
   --
   --  Gives value to Realtime_Clock_Offset.
   procedure Initialize_Realtime_Clock_Offset;

end MaRTE.Kernel.Timers;
