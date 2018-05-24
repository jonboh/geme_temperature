------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . M u t e x e s . D e b u g'
--
--                                 Body
--
--
--  File 'k-mutexes-debug.adb'                                         By MAR.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.SLL.Show;
pragma Elaborate_All (MaRTE.SLL.Show);
with MaRTE.Kernel.Tasks_Lists_Show;
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.Spy;
pragma Elaborate_All (MaRTE.Spy);

package body MaRTE.Kernel.Mutexes.Debug is

   use K.Tasks_Lists;

   package MLST renames K.Mutexes_Lists;
   package SCHD renames K.Scheduler;
   package TLST renames K.Tasks_Lists;

   --------------------------------
   -- package Mutexes_Lists_Show --
   --------------------------------
   procedure Show_Mutex_On_Console (M : in MLST.Element_Ac);
   procedure Show_Mutex_On_Console (M : in MLST.Element_Ac) is
   begin
      if Mutexes_Debug_Messages then
         Put ("{"); Put (Mutex (M.all).Id);
         if Mutex (M.all).Owner = null then
            Put (",Free");
         else
            Put (",("); Put (Mutex (M.all).Owner.Id);
         end if;
         Put (","); Put (Integer (Mutex (M.all).Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put (")}");
      end if;
   end Show_Mutex_On_Console;

   package Mutexes_Lists_Show is new MLST.Show (Show_Mutex_On_Console,
                                                MaRTE.Direct_IO.Put);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (M : access Mutex) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime_Slow, "NM",
                            M.Id, M.Prio);
      if Mutexes_Debug_Messages then
         Put ("  |   NEW{"); Put (M.Id);
         case M.Policy is
            when NO_PRIORITY_INHERITANCE  =>
               Put (",NPI");
            when HIGHEST_BLOCKED_TASK  =>
               Put (",HBT");
            when HIGHEST_CEILING_PRIORITY  =>
               Put (",HCP");
            when APPSCHED_PROTOCOL  =>
               Put (",APP");
         end case;
         Put (","); Put (Integer (M.Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put ("}");
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (M : access Mutex) is
   begin
      if Mutexes_Debug_Messages then
         Put ("  |   END{");
         case M.Policy is
            when NO_PRIORITY_INHERITANCE  =>
               Put (",NPI");
            when HIGHEST_BLOCKED_TASK  =>
               Put (",HBT");
            when HIGHEST_CEILING_PRIORITY  =>
               Put (",HCP");
            when APPSCHED_PROTOCOL  =>
               Put (",APP");
         end case;
         Put (","); Put (Integer (M.Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put ("}");
      end if;
   end Finalize;

   ------------------------------
   -- Running_Task_Locks_Mutex --
   ------------------------------
   procedure Running_Task_Locks_Mutex (M : in Mutex_Descriptor) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime, "LM",
                            M.Id, M.Prio);
      if Mutexes_Debug_Messages then
         Put ("  |   {"); Put (M.Id);
         Put (","); Put (Integer (M.Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put (",BT:");
         Tasks_Lists_Show.Show_On_Console (M.Blocked_Tasks);
         Put ("} Locked by ("); Put (SCHD.Self.Id);
         Put (","); Put (Integer (SCHD.Self.Active_Prio));
         Put (",MO:");
         Mutexes_Lists_Show.Show_On_Console (SCHD.Self.Mutexes_Owned);
         Put (")");
      end if;
   end Running_Task_Locks_Mutex;

   ----------------------------------
   -- Task_Gets_Ready_Owning_Mutex --
   ----------------------------------
   procedure Task_Gets_Ready_Owning_Mutex (T : in Task_Id;
                                           M : in Mutex_Descriptor) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime, "IM",
                            M.Id, M.Prio);
      if Mutexes_Debug_Messages then
         Put ("  |  ("); Put (T.Id);
         Put (","); Put (Integer (T.Active_Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put (",MO:");
         Mutexes_Lists_Show.Show_On_Console (T.Mutexes_Owned);
         Put (") Get {"); Put (M.Id);
         Put (","); Put (Integer (M.Prio));
         Put (",BT:");
         Tasks_Lists_Show.Show_On_Console (M.Blocked_Tasks);
         Put ("}");
      end if;
   end Task_Gets_Ready_Owning_Mutex;

   -----------------------------------
   -- Enqueue_Blocked_Task_In_Mutex --
   -----------------------------------
   procedure Enqueue_Blocked_Task_In_Mutex (T : in Task_Id;
                                            M : in Mutex_Descriptor) is
   begin
      if Mutexes_Debug_Messages then
         Put ("  |  ("); Put (T.Id);
         Put (","); Put (Integer (T.Active_Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put (",MO:");
         Mutexes_Lists_Show.Show_On_Console (T.Mutexes_Owned);
         Put (") Block in {"); Put (M.Id);
         Put (","); Put (Integer (M.Prio));
         Put (",BT:");
         Tasks_Lists_Show.Show_On_Console (M.Blocked_Tasks);
         Put ("}");
      end if;
   end Enqueue_Blocked_Task_In_Mutex;

   -------------------------
   -- Task_Unlocks_Mutex1 --
   -------------------------
   procedure Task_Unlocks_Mutex1 (T : in Task_Id;
                                  M : in Mutex_Descriptor) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime, "UM",
                            M.Id, M.Prio);
      if Mutexes_Debug_Messages then
         Put ("  |  ("); Put (T.Id);
         Put (","); Put (Integer (T.Active_Prio));
         Put (","); Put (Integer (Mutex (M.all).Preemption_Level));
         Put (",MO:");
         Mutexes_Lists_Show.Show_On_Console (T.Mutexes_Owned);
         Put (") ULock {"); Put (M.Id);
         Put (","); Put (Integer (M.Prio));
         Put (",BT:");
         Tasks_Lists_Show.Show_On_Console (M.Blocked_Tasks);
         Put ("}");
      end if;
   end Task_Unlocks_Mutex1;

   -------------------------
   -- Task_Unlocks_Mutex2 --
   -------------------------
   procedure Task_Unlocks_Mutex2 is
   begin
      if Mutexes_Debug_Messages then
         Put ("->Mutex_Unlocked");
      end if;
   end Task_Unlocks_Mutex2;

   ---------------------------------
   -- Running_Task_Locks_AppMutex --
   ---------------------------------
   procedure Running_Task_Locks_AppMutex (M : in Mutex_Descriptor) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime, "LM",
                            M.Id, 0);
   end Running_Task_Locks_AppMutex;

   -----------------------------------
   -- Running_Task_Unlocks_AppMutex --
   -----------------------------------
   procedure Running_Task_Unlocks_AppMutex (M : in Mutex_Descriptor) is
   begin
      MaRTE.Spy.Send_Event (MaRTE.HAL.Get_HWTime, "UM",
                            M.Id, 0);
   end Running_Task_Unlocks_AppMutex;

end MaRTE.Kernel.Mutexes.Debug;
