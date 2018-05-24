------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . S R P _ S y s t e m _ C e i l i n g'
--
--                                  Body
--
--
--
--  File 'k-srp_system_ceiling.adb'                                    By MAR.
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
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.SLL.Order;
pragma Elaborate_All (MaRTE.SLL.Order);

package body MaRTE.Kernel.Mutexes.SRP_Ceiling is

   package CP                 renames MaRTE.Configuration_Parameters;
   package APPSCHD_EVENTS_LST renames
     K.Application_Scheduling_Data.AppSched_Events_Lists;
   package APPSCHD            renames K.Application_Scheduler;
   package APPSCHD_DATA       renames K.Application_Scheduling_Data;

   function UC (Base : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;
   pragma Inline (UC);

   ---------------------------
   -- Pending_Task_Events_Q --
   ---------------------------
   --
   --  List ordered by preemption level of appsched events of type
   --  "Task notification" and "Ready". They are queued here until the
   --  system ceiling is below the preemption level of their
   --  associated task.
   Pending_Task_Events_Q :
     APPSCHD_EVENTS_LST.List := APPSCHD_EVENTS_LST.Null_List;
   function ">" (Left, Right : APPSCHD_DATA.AppSched_Event_Ac) return Boolean;
   function ">" (Left, Right : APPSCHD_DATA.AppSched_Event_Ac)
                return Boolean is
   begin
      return (Left.T.Active_Prio > Right.T.Active_Prio) or else
        (Left.T.Active_Prio = Right.T.Active_Prio and then
         Left.T.Active_Preemption_Level > Right.T.Active_Preemption_Level);
   end ">";
   pragma Inline (">");
   package Ordered_Events_Lst is new APPSCHD_EVENTS_LST.Order (">");

   ----------------------------------------------------------------------------
   --  SRP System Ceiling management  -----------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------------
   -- Ordered 'SRP_Mutex_Lock' lists --
   ------------------------------------
   function ">" (Left, Right : in SRP_Mutex_Lock_Ac) return Boolean;
   function ">" (Left, Right : in SRP_Mutex_Lock_Ac) return Boolean is
   begin
      return (Left.M.Prio > Right.M.Prio) or else
        (Left.M.Prio = Right.M.Prio and then
         Left.M.Preemption_Level > Right.M.Preemption_Level);
   end ">";
   package SRP_Mutex_Lock_Lists_Order is new SRP_Mutex_Lock_Lists.Order (">");

   ------------------
   -- Ceiling_List --
   ------------------
   Ceiling_List : SRP_Mutex_Lock_Lists.List := SRP_Mutex_Lock_Lists.Null_List;

   -------------------------------
   -- Task_Above_System_Ceiling --
   -------------------------------
   function Task_Above_System_Ceiling (T : in Task_Id) return Boolean is
      use SRP_Mutex_Lock_Lists;
   begin
      if SRP_Mutex_Lock_Lists.Is_Empty (Ceiling_List) then
         return True;
      else
         return (T.Active_Prio > Head (Ceiling_List).M.Prio) or else
           (T.Active_Prio = Head (Ceiling_List).M.Prio and then
            T.Active_Preemption_Level >
              Head (Ceiling_List).M.Preemption_Level);
      end if;
   end Task_Above_System_Ceiling;

   --------------------
   -- Add_Mutex_Lock --
   --------------------
   procedure Add_Mutex_Lock (ML : in Mutexes.SRP_Mutex_Lock_Ac) is
      use type SRP_Mutex_Lock_Ac;
   begin
      if not CP.Use_Stack_Resource_Policy'First then
         return;
      end if;
      pragma Assert (Internals.Mutex_OK (ML.M));
      SRP_Mutex_Lock_Lists_Order.Enqueue_In_Order (ML, Ceiling_List);
   end Add_Mutex_Lock;

   --------------------
   -- Del_Mutex_Lock --
   --------------------
   procedure Del_Mutex_Lock (ML : in Mutexes.SRP_Mutex_Lock_Ac) is
      Event : K.Application_Scheduling_Data.AppSched_Event_Ac;
      use type SRP_Mutex_Lock_Ac;
      use type K.Application_Scheduling_Data.AppSched_Event_Ac;
   begin
      if not CP.Use_Stack_Resource_Policy'First then
         return;
      end if;
      pragma Assert (not SRP_Mutex_Lock_Lists.Is_Empty (Ceiling_List));
      pragma Assert (Internals.Mutex_OK (ML.M));

      --  Dequeue element from 'Ceiling_List'
      SRP_Mutex_Lock_Lists.Dequeue (ML, Ceiling_List);

      --  Send to appschedulers those "Task Notification" events with
      --  preemption level above the (maybe) different system ceiling
      --  (of course if system ceiling has not changed no one will be
      --  sent)
      loop
         Event := APPSCHD_EVENTS_LST.Head (Pending_Task_Events_Q);
         exit when Event = null
           or else not Task_Above_System_Ceiling (Event.T);
         APPSCHD_EVENTS_LST.Dequeue_Head (Pending_Task_Events_Q);
         APPSCHD.Send_Notification_Or_Ready_Event
           (ST => UC (Event.T.Sched_App).Scheduler,
            E  => Event);
      end loop;
   end Del_Mutex_Lock;

   ------------------------
   -- Reorder_Mutex_Lock --
   ------------------------
   procedure Reorder_Mutex_Lock (ML : in Mutexes.SRP_Mutex_Lock_Ac) is
   begin
      if not CP.Use_Stack_Resource_Policy'First then
         return;
      end if;
      pragma Assert (not SRP_Mutex_Lock_Lists.Is_Empty (Ceiling_List));
      pragma Assert (Internals.Mutex_OK (ML.M));

      --  Dequeue element from 'Ceiling_List'
      SRP_Mutex_Lock_Lists.Dequeue (ML, Ceiling_List);
      --  Reorder it
      SRP_Mutex_Lock_Lists_Order.Enqueue_In_Order (ML, Ceiling_List);
   end Reorder_Mutex_Lock;

   ----------------------------------------------------------------------------
   --  Task Notification Events Queue  ----------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------------------
   -- Add_Task_Notification_Event_To_Queue --
   ------------------------------------------
   procedure Add_Task_Notification_Event_To_Queue
     (Event : in K.Application_Scheduling_Data.AppSched_Event_Ac) is
      use type APPSCHD_DATA.Event_Code_T;
   begin
      pragma Assert
        (Event.Event_Code = APPSCHD_DATA.APPSCHED_READY or
           Event.Event_Code = APPSCHD_DATA.APPSCHED_TASK_NOTIFICATION);
      Ordered_Events_Lst.Enqueue_In_Order (Event, Pending_Task_Events_Q);
   end Add_Task_Notification_Event_To_Queue;

   ----------------------------------------------------------------------------
   --  Get SRP System Ceiling  ------------------------------------------------
   ----------------------------------------------------------------------------
   function Pthread_Get_SRP_System_Ceiling
     (Prio  : access Task_Priority;
      Level : access Task_Preemption_Level) return MaRTE.Integer_Types.Int is
   begin
      if SRP_Mutex_Lock_Lists.Is_Empty (Ceiling_List) then
         Prio.all := 0;
         Level.all := 0;
      else
         Prio.all  := SRP_Mutex_Lock_Lists.Head (Ceiling_List).M.Prio;
         Level.all :=
           SRP_Mutex_Lock_Lists.Head (Ceiling_List).M.Preemption_Level;
      end if;
      return 0;
   end Pthread_Get_SRP_System_Ceiling;

end MaRTE.Kernel.Mutexes.SRP_Ceiling;
