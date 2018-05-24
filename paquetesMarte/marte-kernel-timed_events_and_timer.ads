------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--         'K e r n e l . T i m e d _ E v e n t s _ A n d _ T i m e r'
--
--                                  Spec
--
--
--  File 'k-timed_events_and_timer.ads'                                By MAR.
--
--
--  Timer and Timed Events Management.
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

with MaRTE.HAL; use MaRTE.HAL; -- to use 'HWTime'
pragma Elaborate_All (MaRTE.HAL);

package MaRTE.Kernel.Timed_Events_And_Timer is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   package HWI renames MaRTE.HAL;

   procedure Reprogram_Timer_After_Expiration (Running_Task : in Task_Id);
   pragma Inline (Reprogram_Timer_After_Expiration);
   --  Look for the most urgent timed event and call 'Program_Timer' with
   --  it.  The timer is always programmed except in the case there is no
   --  timed event to be programmed and 'Compulsory_Timer_Reprogramming'
   --  is FALSE.

   procedure Update_Timer_Due_To_New_CPU_Event (E   : in Timed_Event_Ac;
                                                Now : in HWTime);
   pragma Inline (Update_Timer_Due_To_New_CPU_Event);
   --  If the new CPU timed event 'E' is more urgent than the currently
   --  programmed timer expiration the timer is reporgrammed invoking
   --  'Program_Timer'.

   procedure Update_Timer_Due_To_New_Running_Task (Running_Task : in Task_Id;
                                                   Now          : in HWTime);
   pragma Inline (Update_Timer_Due_To_New_Running_Task);
   --  If the most urgent CPU timed event of the new running task is more
   --  urgent than the currently programmed timer expiration the timer is
   --  reporgrammed invoking 'Program_Timer'.

   procedure Enqueue_Standard_Event_And_Update_Timer (E   : in Timed_Event_Ac;
                                                      Now : in HWTime);
   pragma Inline (Enqueue_Standard_Event_And_Update_Timer);
   --  The new timed event is enqueued and, in the case it is more
   --  urgent than the currently programmed timer expiration the timer is
   --  reporgrammed invoking 'Program_Timer'.

   procedure Enqueue_Standard_Event_Without_Updating_Timer
     (E : in Timed_Event_Ac);
   pragma Inline (Enqueue_Standard_Event_Without_Updating_Timer);
   --  Just enqueue the event in the events queue.

   procedure Remove_Standard_Event (E : in Timed_Event_Ac);
   pragma Inline (Remove_Standard_Event);

   function Extract_Expired_Event (Running_Task : in Task_Id)
                                   return Timed_Event_Ac;
   --  Extracts the most urgent timed event, either CPU or standard, from
   --  its queue only when its activation time is before
   --  'Timer_Activation_Time + Suspension_Time_Minimum'.
   pragma Inline (Extract_Expired_Event);

   function Get_Timer_Activation_Time return HWI.HWTime;
   pragma Inline (Get_Timer_Activation_Time);

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize;

end MaRTE.Kernel.Timed_Events_And_Timer;
