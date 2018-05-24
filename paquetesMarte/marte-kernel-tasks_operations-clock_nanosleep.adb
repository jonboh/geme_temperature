------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T a s k s _ O p e r a t i o n s .
--                      C l o c k _ N a n o s l e e p'
--
--                                 Body
--
--
--  File 'k-to-clock_nanosleep.adb'                                   By MAR.
--
--
--  Function 'clock_nanosleep' (POSIX, 14.2 Clock and Timer Functions).
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
with MaRTE.HAL;
with MaRTE.Kernel.Timers.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Tasks_Operations.Debug;

package body MaRTE.Kernel.Tasks_Operations.Clock_Nanosleep is

   package SCHD renames K.Scheduler;
   package TSTE renames K.Task_Suspension_Timed_Events;
   package TO   renames K.Tasks_Operations;

   ---------------------
   -- Clock_Nanosleep --
   ---------------------
   function Clock_Nanosleep (Clock   : in K.Timers.Clock_Id;
                             Options : in K.Timers.Timer_Options;
                             Rqtp    : in Timespec_Ac;
                             Rmtp    : in Timespec_Ac) return Int is
      use type K.Timers.Clock_Id;
      use type K.Timers.Timer_Options;
      use type HAL.HWTime;
      Flags: Integer;
      Now : HAL.HWTime;
   begin
      --  POSIX Errors
      if (not     K.Timers.Internals.Clock_Id_OK (Clock)
          or else K.Timers.Internals.Is_CPU_Clock_Of_Self (Clock)) then
         --  It's an invalid clock
         return INVALID_ARGUMENT;
      end if;
      if (Clock /= K.Timers.CLOCK_REALTIME and
          Clock /= K.Timers.CLOCK_MONOTONIC) then
         --  It's a CPU-time clock
         return OPERATION_NOT_SUPPORTED;
      end if;

      K.Enter_Critic_Section (Flags);
      Now := HAL.Get_HWTime;
      SCHD.Self.Suspension_Event.T := Timespec_To_HWTime (Rqtp.all);
      if (K.Timers.ABSOLUTE_TIMER and Options) /= 0 then
         --  Absolute suspension
         if Clock = K.Timers.CLOCK_REALTIME then
            if (SCHD.Self.Suspension_Event.T <
                K.Timers.Realtime_Clock_Offset) then
               SCHD.Self.Suspension_Event.T := 0;
            else
               SCHD.Self.Suspension_Event.T := SCHD.Self.Suspension_Event.T
                 - K.Timers.Realtime_Clock_Offset;
            end if;
         end if;
      else
         --  Relative suspension
         SCHD.Self.Suspension_Event.T := SCHD.Self.Suspension_Event.T + Now;
      end if;

      if SCHD.Self.Suspension_Event.T < Suspension_Time_Minimum + Now then
         pragma Debug (TO.Debug.Suspend1 (SCHD.Self, HAL.Get_HWTime)); -- Trace
         Scheduler.Running_Task_Yields_CPU;
      else
         TSTE.Task_Suspension_TE_Ac(SCHD.Self.Suspension_Event).Reason :=
           TSTE.SUSPENSION;

         SCHD.Running_Task_Gets_Suspended (With_Status => TIME_SUSPENDED,
                                           At_Time     => now);

         pragma Debug
           (TO.Debug.Suspend1 (SCHD.Self,
                               SCHD.Self.Suspension_Event.T)); -- Trace
      end if;

      --  Call Scheduler
      SCHD.Do_Scheduling;
      --  The operation hasn't been interrupted
      K.Leave_Critic_Section (Flags);
      return 0;
   end Clock_Nanosleep;

end MaRTE.Kernel.Tasks_Operations.Clock_Nanosleep;
