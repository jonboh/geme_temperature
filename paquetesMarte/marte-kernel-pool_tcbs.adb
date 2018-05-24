------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . P o o l _ T C B s'
--
--                                 Body
--
--
--  File 'k-pool_tcbs.adb'                                            By MAR.
--
--
--  Free Task Control Blocks management.
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

with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);
with MaRTE.Kernel.Signals.Pool_SDBs;
--  pragma Elaborate_All (Kernel.Signals.Pool_SDBs);
with MaRTE.Kernel.Application_Scheduling_Data;
--  pragma Elaborate_All (Kernel.Application_Scheduling_Data);
with MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
--  pragma Elaborate_All (Kernel.Task_Suspension_Timed_Events);
with MaRTE.Kernel;
pragma Elaborate_All (MaRTE.Kernel);
with MaRTE.Kernel.Pool_TCBs_Debug;
with MaRTE.Stacks_Management;

package body MaRTE.Kernel.Pool_TCBs is

   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package SDBs         renames K.Signals.Pool_SDBs;
   package TSTE         renames K.Task_Suspension_Timed_Events; use TSTE;

   package TCBs_Mangement is new Tasks_Lists.Resources (Tasks_Lists.Element);

   Free_TCBs : array (TCBs_Range) of aliased Tasks_Lists.Element;
   --  TCBs for all the tasks: user's task (included the main) plus
   --  the idle and the signal handler tasks.
   --
   --  (Only used when using preallocated resources)

   Initialized : Boolean := False;

   -------------------
   --  Release_TCB  --
   -------------------
   procedure Release_TCB (T : in Task_Id) is
      use type K.Task_Types;
   begin
      pragma Assert (Initialized);
      pragma Assert (T.Task_Type /= K.IDLE_TASK and
                       T.Task_Type /= K.SIGNAL_HANDLER_TASK);
      pragma Assert (Task_Terminated (T));
      pragma Debug (K.Pool_TCBs_Debug.Remove_Active_TCB (T));

      if CP.Use_Application_Defined_Scheduling then
         APPSCHD_DATA.Release_Sched_App_Data
           (APPSCHD_DATA.Sched_App_Data_Ac (T.Sched_App));
      end if;
      SDBs.Release_SDB (K.Signals.Global.Sig_Data_Ac (T.Sig_Data));
      TSTE.Release_Task_Suspension_TE (T.Suspension_Event);
      if T.Task_Type /= K.MAIN_TASK then
         MaRTE.Stacks_Management.Release_Stack (T.Stack);
      end if;
      if T.AppScheduler /= null then
         APPSCHD_DATA.Release_AppScheduler_Data
           (APPSCHD_DATA.AppScheduler_Data_Ac (T.AppScheduler));
      end if;
      T.Magic := K.NOT_USED;
      TCBs_Mangement.Release (T);
   end Release_TCB;

   -------------------
   --  Request_TCB  --
   -------------------
   function Request_TCB (Task_Type    : in K.Task_Types;
                         Stack_Size   : in Positive;
                         AppScheduler : in Boolean) return Task_Id is
      T : Task_Id := TCBs_Mangement.Request;
      use type K.Task_Id;
      use type MaRTE.Stacks_Management.Stack_Id;
      use type Timed_Event_Ac;
   begin
      pragma Assert (Initialized);
      if T = null then
         return null;
      end if;

      --  Request task's stack
      case Task_Type is
         when K.MAIN_TASK =>
            T.Stack := MaRTE.Stacks_Management.Get_Main_Task_Stack;
            pragma Assert (T.Stack /= MaRTE.Stacks_Management.Null_Stack_Id);

         when K.USER_TASK | MaRTE.Kernel.IDLE_TASK |
           K.SIGNAL_HANDLER_TASK =>
            T.Stack := MaRTE.Stacks_Management.Request_Stack
              (MaRTE.Stacks_Management.Stack_Size (Stack_Size));
            if T.Stack = MaRTE.Stacks_Management.Null_Stack_Id then
               TCBs_Mangement.Release (T);
               return null;
            end if;
      end case;

      --  Default values for any task
      T.AppScheduler := null;

      T.Task_Type := Task_Type;
      --  Request remainder resources
      case Task_Type is
         when K.MAIN_TASK | MaRTE.Kernel.USER_TASK =>
            if CP.Use_Application_Defined_Scheduling then
               --  Assign a 'APPSCHD_DATA.Sched_App_Data'
               T.Sched_App :=
                 K.Sched_App_Data_Base_Ac
                 (APPSCHD_DATA.Request_Sched_App_Data);
               APPSCHD_DATA.Sched_App_Data_Ac (T.Sched_App).Owner := T;
            end if;
            --  Assign a 'SDB'
            T.Sig_Data := Sig_Data_Base_Ac (SDBs.Request_SDB);
            Signals.Global.Sig_Data_Ac (T.Sig_Data).Owner := T;
            --  Assign a 'Task_Suspension_TE'
            T.Suspension_Event :=
              K.Timed_Event_Ac (TSTE.Request_Task_Suspension_TE);

            --  Assign a 'AppSched_Data' if the task is an app-scheduler
            if AppScheduler then
               T.AppScheduler :=
                 AppScheduler_Data_Base_Ac
                 (APPSCHD_DATA.Request_AppScheduler_Data);
            end if;

            --  Enough resources ??
            if ((CP.Use_Application_Defined_Scheduling and T.Sched_App = null)
                or T.Sig_Data = null
                or T.Suspension_Event = null
                or (AppScheduler and T.AppScheduler = null)) then
               --  Not enough resources. Release and return null
               if T.Sched_App /= null then
                  APPSCHD_DATA.Release_Sched_App_Data
                    (APPSCHD_DATA.Sched_App_Data_Ac (T.Sched_App));
               end if;
               if T.Sig_Data /= null then
                  SDBs.Release_SDB
                    (K.Signals.Global.Sig_Data_Ac (T.Sig_Data));
               end if;
               if T.Suspension_Event /= null then
                  TSTE.Release_Task_Suspension_TE (T.Suspension_Event);
               end if;
               if Task_Type /= K.MAIN_TASK then
                  MaRTE.Stacks_Management.Release_Stack (T.Stack);
               end if;
               if T.AppScheduler /= null then
                  APPSCHD_DATA.Release_AppScheduler_Data
                    (APPSCHD_DATA.AppScheduler_Data_Ac (T.AppScheduler));
               end if;
               TCBs_Mangement.Release (T);

               return null;
            end if;

         when K.IDLE_TASK | MaRTE.Kernel.SIGNAL_HANDLER_TASK =>
            --  Kernel tasks don't need most of the stuff
            T.Sched_App := null;
            T.Sig_Data := null;
            T.Suspension_Event := null;
      end case;

      pragma Debug (K.Pool_TCBs_Debug.Register_Active_TCB (T));

      return T;
   end Request_TCB;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      pragma Assert
        (not MaRTE.Configuration_Parameters.Preallocated_Resources'First or
           MaRTE.Configuration_Parameters.Num_User_Tasks_Mx + 2 =
             MaRTE.Configuration_Parameters.Num_MaRTE_Tasks_Mx);

      --  Initialize pools of secondary resources
      K.Application_Scheduling_Data.Initialize_Pools;
      K.Pool_TCBs_Debug.Initialize_Pool;
      K.Signals.Pool_SDBs.Initialize_Pool;
      K.Task_Suspension_Timed_Events.Initialize_Pool;
      MaRTE.Stacks_Management.Initialize_Pool;

      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         --  Initialize 'Free_TCBs'
         --  Free_TCBs(0) -> Main Task
         --  Free_TCBs(Num_Tasks_Mx) -> Idle Task
         --  Free_TCBs(Num_Tasks_Mx + 1) -> Signal Handler Task
         for I in Free_TCBs'Range loop
            TCBs_Mangement.Release (Free_TCBs (I)'Access);
         end loop;
      end if;

      Initialized := True;
   end Initialize;

end MaRTE.Kernel.Pool_TCBs;
