------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . S c h e d u l e r . D e b u g'
--
--                                 Spec
--
--
--  File 'k-scheduler-debug.ads'                                       By MAR.
--
--
--  Scheduler debugging.
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
package MaRTE.Kernel.Scheduler.Debug is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   -----------------------
   -- Assert_Task_Is_Ok --
   -----------------------
   procedure Assert_Task_Is_Ok (T : in Task_Id);

   -------------------------
   -- Assert_Tasks_Are_OK --
   -------------------------
   procedure Assert_Tasks_Are_OK (Heir, Old : in Task_Id);

--     ---------------------------
--     -- Assert_TSTEvent_Is_OK --
--     ---------------------------
--     --
--     --  Task suspension timed event
--     procedure Assert_TSTEvent_Is_OK
--       (Event : in Task_Suspension_Timed_Events.Task_Suspension_TE_Ac;
--        Time  : in HWI.HWTime);

--     --------------------------
--     -- Assert_STEvent_Is_OK --
--     --------------------------
--     --
--     --  Scheduler timed event (RR and SS)
--     procedure Assert_STEvent_Is_OK (T     : in Task_Id;
--                                     Event : in Timed_Event_Ac;
--                                     Time  : in HWI.HWTime);

--     --------------------------
--     -- Assert_RTEvent_Is_OK --
--     --------------------------
--     --
--     --  Sporadic Server Replenishment timed event
--     procedure Assert_RTEvent_Is_OK
--       (Event : in Replenishment_TE.Replenishment_Timed_Event_Ac;
--        Time  : in HWI.HWTime);

--     --------------------------
--     -- Assert_TTEvent_Is_OK --
--     --------------------------
--     --
--     --  Timer timed event
--     procedure Assert_TTEvent_Is_OK (Event : in Kernel.Timers.Timer_Id;
--                                         Time  : in HWI.HWTime);

--     ---------------------------------
--     -- Assert_Not_A_Sched_App_Task --
--     ---------------------------------
--     procedure Assert_Not_A_Sched_App_Task (Running_Task : in Task_Id);

end MaRTE.Kernel.Scheduler.Debug;
