------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--           'K e r n e l . T a s k _ S e t s . I n t e r n a l s'
--
--                                 Body
--
--  File 'marte-kernel-task_sets-internals.adb'                        By MAR.
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
with MaRTE.Kernel.Group_Clocks.Internals;
with MaRTE.Kernel.Debug;
with MaRTE.Configuration_Parameters;
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

package body MaRTE.Kernel.Task_Sets.Internals is

   package GCLKI renames K.Group_Clocks.Internals;
   package GCLK  renames K.Group_Clocks;
   package DBG   renames K.Debug;

   use type K.Task_Set_Id, K.Task_Container_Ac, K.Task_Id,
       CPU_Time_Timed_Event_Ac;

   Initialized : Boolean := False;

   --------------------------
   --  Pools of Task_Sets  --
   --------------------------

   package Task_Sets_Pool is
     new K.Task_Sets_Lists.Resources (MaRTE.Kernel.Task_Sets_Lists.Element);
   Free_Task_Sets : array (0 .. CP.Num_Task_Sets_Mx - 1) of
     aliased K.Task_Sets_Lists.Element;

   function Request_Task_Set return K.Task_Set_Id
     renames Task_Sets_Pool.Request;

   procedure Release_Task_Set (TS : K.Task_Set_Id)
     renames Task_Sets_Pool.Release;

   --------------------------------
   --  Pools of Task_Containers  --
   --------------------------------

   package Task_Containers_Pool is
     new TC_Lists.Resources (K.Task_Containers_Lists.Element);
   Free_Task_Containers : array (0 .. CP.Num_User_Tasks_Mx - 1) of
     aliased K.Task_Containers_Lists.Element;

   function Request_TC return K.Task_Container_Ac
     renames Task_Containers_Pool.Request;

   procedure Release_TC (TC : K.Task_Container_Ac)
     renames Task_Containers_Pool.Release;

   --------------------------------
   --  Is_In_The_Group_Of_Tasks  --
   --------------------------------

   function Is_In_The_Group_Of_Tasks (L : TC_Lists.List;
                                      T : K.Task_Id) return Boolean is
      TC : K.Task_Container_Ac;
   begin
      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return False;
      end if;

      TC := TC_Lists.Head (L);
      while TC /= null loop
         if TC.T = T then
            return True;  --  Task found in list
         end if;
         TC := TC_Lists.Next (TC);
      end loop;

      return False;  --  Task NOT found in list
   end Is_In_The_Group_Of_Tasks;

   ------------------------------
   -- Delete_Task_From_Its_Set --
   ------------------------------

   procedure Delete_Task_From_Its_Set (T : K.Task_Id) is
      TC : K.Task_Container_Ac;
   begin
      if not MaRTE.Configuration_Parameters.Use_Task_Sets'First then
         return;
      end if;
      pragma Debug (DBG.Assert (K.Task_OK (T, Extra_Magic => K.TERMINATED)));
      pragma Debug (DBG.Assert (T.Set /= null));
      pragma Debug (DBG.Assert (T.Set.all in K.Task_Sets_Lists.Element'Class));
      pragma Debug (DBG.Assert (Task_Set_OK (K.Task_Set_Base_Ac (T.Set))));

      --  Set associated with groupclock?: remove from it

      if T.Set.Group_TE_Ac /= null then
         pragma Debug (DBG.Assert (GCLK.Group_TE_OK (T.Set.Group_TE_Ac)));
         --  Unlink T from the Groupclock
         GCLKI.Del_Task (T, T.Set.Group_TE_Ac);
      end if;

      --  Delete task from the set

      TC := TC_Lists.Head (T.Set.Set);
      loop
         if TC = null then
            --  This should NEVER happen: task is not in the list !!
            pragma Debug (DBG.Assert (False));
            null;
         end if;

         if TC.T = T then
            --  Task found
            TC_Lists.Dequeue (TC, T.Set.Set);
            Release_TC (TC);
            T.Set := null; -- not in group
            exit;
         end if;

         TC := TC_Lists.Next (TC);
      end loop;
   end Delete_Task_From_Its_Set;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize is
   begin
      pragma Debug (DBG.Assert (not Initialized));

      pragma Debug (DBG.Assert (K.Task_Set_Id'Size = 4 * 8));
      --  s-hansup relies on the parameter Set is a pointer

      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         for I in Free_Task_Sets'Range loop
            Task_Sets_Pool.Release (Free_Task_Sets (I)'Access);
         end loop;
         for I in Free_Task_Containers'Range loop
            Task_Containers_Pool.Release (Free_Task_Containers (I)'Access);
         end loop;
      end if;

      Initialized := True;
   end Initialize;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;

end MaRTE.Kernel.Task_Sets.Internals;
