------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . P o o l _ T C B s'
--
--                                 Spec
--
--
--  File 'k-pool_tcbs.ads'                                            By MAR.
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
with MaRTE.Configuration_Parameters;

package MaRTE.Kernel.Pool_TCBs is

   package K renames MaRTE.Kernel;

   package CP renames MaRTE.Configuration_Parameters;

   type TCBs_Range is range 0 .. CP.Num_MaRTE_Tasks_Mx - 1;
   --  'Num_MaRTE_Tasks_Mx' user's task (included the main) plus the
   --  idle and the signal handler tasks.
   --
   --  (This range will be null when NOT using preallocated resources)

   --  TCBs preassigned to expecial tasks
   Main_Task_Id           : constant := 0;
   Idle_Task_Id           : constant := 1;
   Signal_Handler_Task_Id : constant := 2;
   First_User_Task_Id     : constant := 3;

   procedure Release_TCB (T : in Task_Id);
   pragma Inline (Release_TCB);

   function Request_TCB (Task_Type    : in K.Task_Types;
                         Stack_Size   : in Positive;
                         AppScheduler : in Boolean) return Task_Id;
   pragma Inline (Request_TCB);

   ------------------
   --  Initialize  --
   ------------------
   --
   --  Initialize TCBs pool and the pools of secondary resources
   procedure Initialize;

end MaRTE.Kernel.Pool_TCBs;
