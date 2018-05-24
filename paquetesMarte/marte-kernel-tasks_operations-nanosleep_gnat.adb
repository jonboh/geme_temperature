------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T a s k s _ O p e r a t i o n s .
--                       N a n o s l e e p _ G n a t'
--
--                                 Body
--
--
--  File 'k-to-nanosleep_gnat.adb'                                     By MAR.
--
--
--  Function 'nanosleep_gnat'. This function has the same functionality
--  than the POSIX 'nanosleep' function but with parameters of type 'access
--  Duration' instead of 'struct timespec *'.
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
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Tasks_Operations.Debug;

package body MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat is

   use type HAL.HWTime;
   package TOI  renames K.Tasks_Operations.Internals;
   package SCHD renames K.Scheduler;
   package TSTE renames K.Task_Suspension_Timed_Events;
   package TO   renames K.Tasks_Operations;

   --------------------
   -- Nanosleep_Gnat --
   --------------------
   function Nanosleep_Gnat (Interval  : access Duration;
                            Remaining : access Duration) return Int is
      Flags : Integer;
      Now : HAL.HWTime;
   begin
      K.Enter_Critic_Section (Flags);

      --  Suspend the Task if the interval is big enough
      if Interval.all < K.Suspension_Time_Minimum_Seg then
         pragma Debug (TO.Debug.Suspend1 (SCHD.Self, HAL.Get_HWTime)); -- Trace
         Scheduler.Running_Task_Yields_CPU;
      else
         Now := HAL.Get_HWTime;
         SCHD.Self.Suspension_Event.T :=
           HAL.Duration_To_HWTime (Interval.all) + Now;

         TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
           TSTE.SUSPENSION;

         SCHD.Running_Task_Gets_Suspended (With_Status => TIME_SUSPENDED,
                                           At_Time     => Now);

         pragma Debug
           (TO.Debug.Suspend1 (SCHD.Self,
                               SCHD.Self.Suspension_Event.T)); -- Trace
      end if;

      --  Call Scheduler
      SCHD.Do_Scheduling;

      TOI.Reset_POSIX_Error;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Nanosleep_Gnat;

end MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
