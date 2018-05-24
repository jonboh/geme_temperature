------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--    'K e r n e l . A p p l i c a t i o n _ S c h e d u l i n g _ D a t a'
--
--                                   Body
--
--
--
--  File 'k-appsched_data.adb'                                         By MAR.
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
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

package body MaRTE.Kernel.Application_Scheduling_Data is

   use AppSched_Events_Lists;
   use Task_Containers_Lists;
   use AppScheduler_Data_Lists;

   Pools_Initialized : Boolean := False;

   ----------------------------------------------------------------------------
   -- Application scheduling events pool --------------------------------------
   ----------------------------------------------------------------------------

   Free_AppSched_Events : array (1 .. CP.Num_Application_Scheduling_Events_Mx)
     of aliased AppSched_Events_Lists.Element;
   package AppSched_Events_Pool_Mangement is
     new AppSched_Events_Lists.Resources (AppSched_Events_Lists.Element);

   ----------------------------
   -- Release_AppSched_Event --
   ----------------------------
   procedure Release_AppSched_Event (E : in AppSched_Event_Ac)
     renames AppSched_Events_Pool_Mangement.Release;

   ----------------------------
   -- Request_AppSched_Event --
   ----------------------------
   function Request_AppSched_Event return AppSched_Event_Ac
     renames AppSched_Events_Pool_Mangement.Request;

   ----------------------------------------------------------------------------
   -- Task Containers pool ----------------------------------------------------
   ----------------------------------------------------------------------------

   ----------
   -- Pool --
   ----------
   Free_TCs : array (1 .. CP.Num_Application_Scheduled_Tasks_Mx)
     of aliased K.Task_Containers_Lists.Element;
   package TCs_Pool_Mangement is new
     Task_Containers_Lists.Resources (K.Task_Containers_Lists.Element);

   ----------------------------
   -- Release_Task_Container --
   ----------------------------
   procedure Release_Task_Container (TC : in Task_Container_Ac)
     renames TCs_Pool_Mangement.Release;

   ----------------------------
   -- Request_Task_Container --
   ----------------------------
   function Request_Task_Container return Task_Container_Ac
     renames TCs_Pool_Mangement.Request;

   -----------------------------------
   -- Task_Container_Pool_Exhausted --
   -----------------------------------
   function Any_Task_Container_Free_In_Pool return Boolean
     renames TCs_Pool_Mangement.Resources_Left;

   ----------------------------------------------------------------------------
   -- 'AppScheduler_Data' pool ------------------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------
   -- Null 'AppScheduler_Data' --
   ------------------------------
   AppScheduler_Data_Null : constant AppScheduler_Data :=
     (Tasks_Q                   => Task_Containers_Lists.Null_List,
      Events_Q                  => AppSched_Events_Lists.Null_List,
      Flags                     => 0,
      Events_Mask               => 0,
      Wait_Signal_Set           => K.Signals.Empty_Set,
      Waiting_For_Signals       => False,
      Clock                     => Timers.CLOCK_REALTIME,
      Reserved_TCs              => Task_Containers_Lists.Null_List,
      Suspension_Deferred_Tasks => Sched_App_Data_Lists.Null_List,
      Reply                     => (others => 0),
      Reply_Size                => 0,
      Ops                       => (Init => null,
                                    New_Task => null,
                                    Ready => null,
                                    Explicit_Call => null,
                                    Task_Notification => null,
                                    Timeout => null,
                                    Signal => null),
      Data                      => System.Null_Address);

   ----------
   -- Pool --
   ----------
   Free_AppScheduler_Data : array (1 .. CP.Num_Application_Schedulers_Mx)
     of aliased AppScheduler_Data_Lists.Element;
   package AppScheduler_Data_Pool_Mangement is
     new AppScheduler_Data_Lists.Resources (AppScheduler_Data_Lists.Element);

   --------------------
   -- 'Event_In_Set' --
   --------------------
   function Event_In_Set (Event : in Event_Code_T;
                          Set   : in Event_Set)
                          return Boolean is
   begin
      return (To_Event_Set (Event) and Set) /= 0;
   end Event_In_Set;

   --------------------
   -- 'To_Event_Set' --
   --------------------
   function To_Event_Set (Event : in Event_Code_T)
                          return Event_Set is
   begin
      return Shift_Left (Event_Set'(2), Event'Enum_Rep);
   end To_Event_Set;

   -------------------------------
   -- Release_AppScheduler_Data --
   -------------------------------
   procedure Release_AppScheduler_Data
     (AppScheduler_Data : in AppScheduler_Data_Ac) is
      TC : Task_Container_Ac := Head (AppScheduler_Data.Tasks_Q);
      Event_Ac : AppSched_Event_Ac := Head (AppScheduler_Data.Events_Q);
   begin
      pragma Assert (Pools_Initialized);
      --  Release TCs in 'Tasks_Q'
      while TC /= null loop
         Dequeue_Head (AppScheduler_Data.Tasks_Q);
         Release_Task_Container (TC);
         TC := Head (AppScheduler_Data.Tasks_Q);
      end loop;
      --  Release Events in 'Events_Q'
      while Event_Ac /= null loop
         Dequeue_Head (AppScheduler_Data.Events_Q);
         Release_AppSched_Event (Event_Ac);
         Event_Ac := Head (AppScheduler_Data.Events_Q);
      end loop;

      AppScheduler_Data_Pool_Mangement.Release (AppScheduler_Data);
   end Release_AppScheduler_Data;

   -------------------------------
   -- Request_AppScheduler_Data --
   -------------------------------
   function Request_AppScheduler_Data return AppScheduler_Data_Ac is
      Ac : AppScheduler_Data_Ac := AppScheduler_Data_Pool_Mangement.Request;
   begin
      pragma Assert (Pools_Initialized);
      if Ac /= null then
         AppScheduler_Data (Ac.all) := AppScheduler_Data_Null;
      end if;
      return Ac;
   end Request_AppScheduler_Data;

   ----------------------------------------------------------------------------
   -- 'Sched_App_Data' pool ---------------------------------------------------
   ----------------------------------------------------------------------------
   Free_Sched_App_Data : array (1 .. CP.Num_Of_Sched_App_Data)
     of aliased Sched_App_Data_Lists.Element;
   package Sched_App_Data_Pool_Management is
      new Sched_App_Data_Lists.Resources (Sched_App_Data_Lists.Element);

   function Request_Sched_App_Data return Sched_App_Data_Ac
     renames Sched_App_Data_Pool_Management.Request;

   procedure Release_Sched_App_Data (S : Sched_App_Data_Ac)
     renames Sched_App_Data_Pool_Management.Release;

   ------------------------
   --  Initialize_Pools  --
   ------------------------
   procedure Initialize_Pools is
   begin
      pragma Assert (not Pools_Initialized);
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         --  Initialize 'AppSched_Events' pool
         for I in Free_AppSched_Events'Range loop
            AppSched_Events_Pool_Mangement.Release
              (Free_AppSched_Events (I)'Access);
         end loop;

         --  Initialize TCs pool
         for I in Free_TCs'Range loop
            TCs_Pool_Mangement.Release (Free_TCs (I)'Access);
         end loop;

         --  Initialize 'AppScheduler_Data' pool
         for I in Free_AppScheduler_Data'Range loop
            AppScheduler_Data_Pool_Mangement.Release
              (Free_AppScheduler_Data (I)'Access);
         end loop;

         --  Initialize 'Sched_App_Data' pool
         for I in Free_Sched_App_Data'Range loop
            Release_Sched_App_Data (Free_Sched_App_Data (I)'Access);
         end loop;
      end if;

      Pools_Initialized := True;
   end Initialize_Pools;

end MaRTE.Kernel.Application_Scheduling_Data;
