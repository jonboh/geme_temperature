------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--       'K e r n e l . T i m e r _ T i m e d _ E v e n t s _ P o o l'
--
--                                 Body
--
--
--  File 'k-timer_timed_events_pool.adb'                               By Mar.
--
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
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

--  Debug
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timer_Timed_Events_Pool is
   use K.Timers;

   package DBG renames K.Debug;

   Initialized : Boolean := False;

   package Timer_Timed_Events_Resources is
     new CPU_Time_Timed_Events_Lists.Resources (Timers.Timer_Timed_Event);

   Free_Timer_Timed_Events :
     array (0 .. MaRTE.Configuration_Parameters.Num_Timers_Mx - 1) of
     aliased Timers.Timer_Timed_Event;

   procedure Set_Timer_Timed_Event_Free (T : in Timers.Timer_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      Timer_Timed_Events_Resources.Release (CPU_Time_Timed_Event_Ac (T));
   end Set_Timer_Timed_Event_Free;

   function Request_Timer_Timed_Event return Timers.Timer_Id is
   begin
      pragma Debug (DBG.Assert (Initialized));
      return Timer_Id (Timer_Timed_Events_Resources.Request);
   end Request_Timer_Timed_Event;

   -----------------------
   --  Initialize_Pool  --
   -----------------------
   procedure Initialize_Pool is
   begin
      pragma Debug (DBG.Assert (not Initialized));
      Initialized := True;

      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         for I in 0 .. MaRTE.Configuration_Parameters.Num_Timers_Mx - 1 loop
            Set_Timer_Timed_Event_Free (Free_Timer_Timed_Events (I)'Access);
         end loop;
      end if;
   end Initialize_Pool;

end MaRTE.Kernel.Timer_Timed_Events_Pool;
