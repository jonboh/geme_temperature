------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--           'K e r n e l . T a s k _ S e t s . I n t e r n a l s'
--
--                                 Spec
--
--  File 'marte-kernel-task_sets-internals.ads'                        By MAR.
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
package MaRTE.Kernel.Task_Sets.Internals is

   package K renames MaRTE.Kernel;
   package TC_Lists renames K.Task_Containers_Lists;

   ----------------------
   -- Request_Task_Set --
   ----------------------

   function Request_Task_Set return K.Task_Set_Id;

   ----------------------
   -- Release_Task_Set --
   ----------------------

   procedure Release_Task_Set (TS : K.Task_Set_Id);

   ----------------
   -- Request_TC --
   ----------------

   function Request_TC return K.Task_Container_Ac;

   ----------------
   -- Release_TC --
   ----------------

   procedure Release_TC (TC : K.Task_Container_Ac);

   --------------------------------
   --  Is_In_The_Group_Of_Tasks  --
   --------------------------------

   --  Internal function to be used for debugging purposes

   function Is_In_The_Group_Of_Tasks (L : TC_Lists.List;
                                      T : K.Task_Id) return Boolean;

   ------------------------------
   -- Delete_Task_From_Its_Set --
   ------------------------------

   procedure Delete_Task_From_Its_Set (T : K.Task_Id);
   pragma Inline (Delete_Task_From_Its_Set);
   --  Deletes the task from its the set and, if there is a group clock
   --  associated with the set, deletes the task form the group clock as well

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized return Boolean;
   pragma Inline (Is_Initialized);

end MaRTE.Kernel.Task_Sets.Internals;
