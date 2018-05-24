------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . S R P _ S y s t e m _ C e i l i n g'
--
--                                  Spec
--
--
--
--  File 'k-srp_system_ceiling.ads'                                    By MAR.
--
--
--  Stack Resource Policy system ceiling management and list of
--  pending application-scheduling events of type "Task Notification".
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
with MaRTE.Integer_Types;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Mutexes;

package MaRTE.Kernel.Mutexes.SRP_Ceiling is

   package K renames MaRTE.Kernel;

   ----------------------------------------------------------------------------
   --  Get SRP System Ceiling  ------------------------------------------------
   ----------------------------------------------------------------------------
   function Pthread_Get_SRP_System_Ceiling
     (Prio  : access Task_Priority;
      Level : access Task_Preemption_Level) return MaRTE.Integer_Types.Int;
   pragma Export (C, Pthread_Get_SRP_System_Ceiling,
                    "pthread_getsrpsystemceiling");

   ----------------------------------------------------------------------------
   --  SRP System Ceiling management  -----------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Each mutex lock/unlock operation in the system is notified in
   --  order to keep track of the system ceiling. Also when the
   --  inheritance priority of an inheritance mutex changes it has to
   --  be reordered.
   function Task_Above_System_Ceiling (T : in Task_Id) return Boolean;
   procedure Add_Mutex_Lock (ML : in SRP_Mutex_Lock_Ac);
   pragma Inline (Add_Mutex_Lock);
   procedure Del_Mutex_Lock (ML : in Mutexes.SRP_Mutex_Lock_Ac);
   pragma Inline (Del_Mutex_Lock);
   procedure Reorder_Mutex_Lock (ML : in Mutexes.SRP_Mutex_Lock_Ac);
   pragma Inline (Reorder_Mutex_Lock);

   ----------------------------------------------------------------------------
   --  Task Notification Events Queue  ----------------------------------------
   ----------------------------------------------------------------------------
   procedure Add_Task_Notification_Event_To_Queue
     (Event : in K.Application_Scheduling_Data.AppSched_Event_Ac);
   pragma Inline (Add_Task_Notification_Event_To_Queue);
   --  To be called with "task notification" appsched events that
   --  cannot be sent to the appscheduler. That happens when the
   --  preemption level of associated task is below the system
   --  ceiling.
   --
   --  Events queued with this procedure will be sent to the
   --  appscheduler as soon as the system ceiling gets low
   --  enough. This action will be performed in 'Del_Mutex_Lock'.

end MaRTE.Kernel.Mutexes.SRP_Ceiling;
