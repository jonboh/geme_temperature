------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--           'K e r n e l . T a s k _ S e t s . O p e r a t i o n s'
--
--                                 Spec
--
--  File 'kernel-task_sets-operations.ads'                          By MAR.
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

package MaRTE.Kernel.Task_Sets.Operations is

   package K renames MaRTE.Kernel;

   function Marte_Thread_Set_Create
     (Set : access K.Task_Set_Id)
     return Int;
   pragma Export (C, Marte_Thread_Set_Create, "marte_threadset_create");

   function Marte_Thread_Set_Destroy
     (Set : K.Task_Set_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Destroy, "marte_threadset_destroy");

   function Marte_Thread_Set_Empty
     (Set : K.Task_Set_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Empty, "marte_threadset_empty");

   function Marte_Thread_Set_Add_Thread
     (Set : K.Task_Set_Id;
      T   : K.Task_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Add_Thread, "marte_threadset_add");

   function Marte_Thread_Set_Del_Thread
     (Set : K.Task_Set_Id;
      T   : K.Task_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Del_Thread, "marte_threadset_del");

   function Marte_Thread_Set_First
     (Set : K.Task_Set_Id;
      T   : access Task_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_First, "marte_threadset_first");
   --  Return NO_SUCH_PROCESS when the set is empty

   function Marte_Thread_Set_Next
     (Set : K.Task_Set_Id;
      T   : access Task_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Next, "marte_threadset_next");
   --  Return NO_SUCH_PROCESS when there aren't more tasks in set

   function Marte_Thread_Set_Is_Member
     (Set       : K.Task_Set_Id;
      T         : Task_Id;
      Is_Member : access Int)
      return Int;
   pragma Export (C, Marte_Thread_Set_Is_Member, "marte_threadset_ismember");

   function Marte_Thread_Set_Getset
     (T   : K.Task_Id;
      Set : access K.Task_Set_Id)
      return Int;
   pragma Export (C, Marte_Thread_Set_Getset, "marte_threadset_getset");

   function Marte_Getgroupclockid
     (Set   : K.Task_Set_Id;
      Clock : access K.Clock_Id)
      return Int;
   pragma Export (C, Marte_Getgroupclockid, "marte_getgroupcpuclockid");

end MaRTE.Kernel.Task_Sets.Operations;
