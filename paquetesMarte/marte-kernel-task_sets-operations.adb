------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--           'K e r n e l . T a s k _ S e t s . O p e r a t i o n s'
--
--                                 Body
--
--  File 'kernel-task_sets-operations.adb'                          By MAR.
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
with MaRTE.Kernel.Timers.Internals;
with MaRTE.Kernel.Debug;
with MaRTE.Kernel.Group_Clocks.Internals;
with MaRTE.Kernel.Timed_Handlers.Operations;
with MaRTE.Kernel.Task_Sets.Internals;

package body MaRTE.Kernel.Task_Sets.Operations is

   package TS       renames K.Task_Sets;
   package GCLK     renames K.Group_Clocks;
   package GCLKI    renames K.Group_Clocks.Internals;
   package TMRI     renames K.Timers.Internals;
   package TC_Lists renames K.Task_Containers_Lists;
   package CP       renames MaRTE.Configuration_Parameters;
   package DBG      renames K.Debug;
   package THO      renames MaRTE.Kernel.Timed_Handlers.Operations;
   package TH       renames MaRTE.Kernel.Timed_Handlers;
   package TSI      renames MaRTE.Kernel.Task_Sets.Internals;

   use type K.Task_Id, K.Task_Set_Id, K.CPU_Time_Timed_Event_Ac,
       K.Task_Container_Ac;

   -------------------------------
   --  Marte_Thread_Set_Create  --
   -------------------------------
   function Marte_Thread_Set_Create
     (Set : access K.Task_Set_Id)
      return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;

      K.Enter_Critic_Section (Flags);

      Set.all := TSI.Request_Task_Set;

      if Set.all = null then
         --  No resources free
         K.Leave_Critic_Section (Flags);
         return RESOURCE_TEMPORARILY_UNAVAILABLE;
      end if;

      Task_Set_Base (Set.all.all) := (Set           => TC_Lists.Null_List,
                                      Consumed_Time => 0,
                                      Group_TE_Ac   => null,
                                      Iterator      => null,
                                      Magic         => K.TS_INITIALIZED);
      pragma Debug
        (DBG.Assert (Set.all.all in K.Task_Sets_Lists.Element'Class));

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Create;

   --------------------------------
   --  Marte_Thread_Set_Destroy  --
   --------------------------------
   function Marte_Thread_Set_Destroy
     (Set : K.Task_Set_Id)
      return Int is
      Flags : Integer;
      Ret : Int;
      use type Int;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;

      K.Enter_Critic_Section (Flags);

      --  Empty task group (in order to free tasks in the set, otherwise they
      --  wouldn't can be attached to a different set in the future)
      Ret := Marte_Thread_Set_Empty (Set);
      if Ret /= 0 then
         K.Leave_Critic_Section (Flags);
         return Ret;
      end if;

      --  Destroy the timed handler based on the set (if any)
      if Set.Group_TE_Ac /= null then
         pragma Debug (DBG.Assert (GCLK.Group_TE_OK (Set.Group_TE_Ac)));
         Ret := THO.MaRTE_Timed_Handler_Destroy
           (TH.Timed_Handler_TE_Ac (Set.Group_TE_Ac));
         pragma Debug (DBG.Assert (Ret = 0));
      end if;

      --  Delete task group
      Set.all.Magic := K.TS_NOT_INITIALIZED;
      TSI.Release_Task_Set (Set);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Destroy;

   ------------------------------
   --  Marte_Thread_Set_Empty  --
   ------------------------------
   function Marte_Thread_Set_Empty
     (Set : K.Task_Set_Id)
      return Int is
      Flags : Integer;
      TC : K.Task_Container_Ac;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set)) then
         --  Invalid argument
         --   * invalid Set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      pragma Debug (DBG.Assert (Set.Group_TE_Ac = null or else
                                GCLK.Group_TE_OK (Set.Group_TE_Ac)));

      --  Release TCs
      TC := TC_Lists.Head (Set.Set);
      while TC /= null loop
         pragma Debug (DBG.Assert (K.Task_Set_Base_Ac (Set) = TC.T.Set));
         TC_Lists.Dequeue_Head (Set.Set);
         if Set.Group_TE_Ac /= null then
            GCLKI.Del_Task (TC.T, Set.Group_TE_Ac);
         end if;
         TC.T.Set := null;
         TSI.Release_TC (TC);
         TC := TC_Lists.Head (Set.Set);
      end loop;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Empty;

   -----------------------------------
   --  Marte_Thread_Set_Add_Thread  --
   -----------------------------------
   function Marte_Thread_Set_Add_Thread
     (Set : K.Task_Set_Id;
      T   : K.Task_Id)
      return Int is
      Flags : Integer;
      TC : K.Task_Container_Ac;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not K.Task_OK (T) then
         --  No such proccess
         --   * T doesn't identify a valid thread
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      if T.Set /= null and then K.Task_Set_Base_Ac (Set) /= T.Set then
         --  Operation not supported
         --   * Task already in a set different from 'Set'. This is a
         --     restrictive limitation that could be removed in the future. If
         --     removed, s-etgrbu should be revised.
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;

      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set)) then
         --  Invalid argument
         --   * invalid Set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if K.Task_Set_Base_Ac (Set) /= T.Set then
         pragma Debug
           (DBG.Assert (not TSI.Is_In_The_Group_Of_Tasks (Set.Set, T)));

         --  Request "Task_Container"
         TC := TSI.Request_TC;
         if TC = null then
            K.Leave_Critic_Section (Flags);
            return RESOURCE_TEMPORARILY_UNAVAILABLE;
         end if;

         --  Add T to Set
         TC.T := T;
         TC_Lists.Enqueue_Head (TC, Set.Set);

         --  Link Set with T
         T.Set := K.Task_Set_Base_Ac (Set);

      else
         --  If T is already in Set nothing has to be done
         pragma Debug (DBG.Assert (TSI.Is_In_The_Group_Of_Tasks (Set.Set, T)));
         null;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Add_Thread;

   -----------------------------------
   --  Marte_Thread_Set_Del_Thread  --
   -----------------------------------
   function Marte_Thread_Set_Del_Thread
     (Set : K.Task_Set_Id;
      T   : K.Task_Id)
      return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not K.Task_OK (T) then
         --  No such proccess
         --   * T doesn't identify a valid thread
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set))
        or else K.Task_Set_Base_Ac (Set) /= T.Set
      then
         --  Invalid argument
         --   * invalid Set
         --   * T not in Set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      --  Delete task from the set and from the group clock

      TSI.Delete_Task_From_Its_Set (T);
      pragma Debug
        (DBG.Assert (not TSI.Is_In_The_Group_Of_Tasks (Set.Set, T)));

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Del_Thread;

   ------------------------------
   --  Marte_Thread_Set_First  --
   ------------------------------
   function Marte_Thread_Set_First (Set : K.Task_Set_Id;
                                    T   : access K.Task_Id) return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set)) then
         --  Invalid argument
         --   * invalid Set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      --  Get first task in set

      Set.Iterator := TC_Lists.Head (Set.Set);
      if Set.Iterator /= null then
         --  Return first
         pragma Debug (DBG.Assert (K.Task_OK (Set.Iterator.T)));
         T.all := Set.Iterator.T;
      else
         --  empty set
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_First;

   -----------------------------
   --  Marte_Thread_Set_Next  --
   -----------------------------
   function Marte_Thread_Set_Next (Set : K.Task_Set_Id;
                                   T   : access K.Task_Id) return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set))
        or else (Set.Iterator /= null
                 and then not K.Task_OK (Set.Iterator.T)
                 and then K.Task_Set_Base_Ac (Set) /= Set.Iterator.T.Set)
      then
         --  Invalid argument
         --   * invalid Set
         --   * iterator not null and task not OK
         --                           or task OK but not in the set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      --  Look for the next task

      if Set.Iterator = null then
         Set.Iterator := TC_Lists.Head (Set.Set); -- return the first one
      else
         Set.Iterator := TC_Lists.Next (Set.Iterator);
      end if;

      if Set.Iterator /= null then
         --  Return next
         pragma Debug (DBG.Assert (K.Task_OK (Set.Iterator.T)));
         pragma Debug
           (DBG.Assert (Set.Iterator.T.Set = K.Task_Set_Base_Ac (Set)));
         T.all := Set.Iterator.T;
      else
         --  reached the end of the list
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Next;

   -------------------------------
   --  Marte_Thread_Set_Getset  --
   -------------------------------
   function Marte_Thread_Set_Getset (T   : K.Task_Id;
                                     Set : access K.Task_Set_Id) return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not K.Task_OK (T) then
         --  No such proccess
         --   * T doesn't identify a valid thread
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      Set.all := K.Task_Set_Id (T.Set);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Getset;

   --------------------------------
   -- Marte_Thread_Set_Is_Member --
   --------------------------------

   function Marte_Thread_Set_Is_Member
     (Set       : K.Task_Set_Id;
      T         : Task_Id;
      Is_Member : access Int)
      return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not K.Task_OK (T) then
         --  No such proccess
         --   * T doesn't identify a valid thread
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set)) then
         --  Invalid argument
         --   * invalid Set
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if K.Task_Set_Base_Ac (Set) = T.Set then
         --  It is member
         pragma Debug (DBG.Assert (TSI.Is_In_The_Group_Of_Tasks (Set.Set, T)));
         Is_Member.all := 1;
      else
         --  It isn't member
         pragma Debug
           (DBG.Assert (not TSI.Is_In_The_Group_Of_Tasks (Set.Set, T)));
         Is_Member.all := 0;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Thread_Set_Is_Member;

   -----------------------------
   --  Marte_Getgroupclockid  --
   -----------------------------
   function Marte_Getgroupclockid
     (Set   : K.Task_Set_Id;
      Clock : access K.Clock_Id)
      return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (TSI.Is_Initialized));

      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First
        or not MaRTE.Configuration_Parameters.Use_CPU_Time_Clocks_And_Timers
        or not MaRTE.Configuration_Parameters.Use_Group_Clocks'First
      then
         return OPERATION_NOT_SUPPORTED;
      end if;
      pragma Debug (DBG.Assert (Set.all in K.Task_Sets_Lists.Element'Class));

      K.Enter_Critic_Section (Flags);
      --  Check errors
      if not TS.Task_Set_OK (K.Task_Set_Base_Ac (Set)) then
         --  Invalid argument
         --   * invalid Set
         --   * null clock pointer
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      --  Return Clock
      Clock.all := TMRI.To_Clock_Id (Set);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Marte_Getgroupclockid;

end MaRTE.Kernel.Task_Sets.Operations;
