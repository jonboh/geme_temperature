------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T a s k s _ O p e r a t i o n s .
--                       I n i t i a l i z e _ T C B s'
--
--                                 Spec
--
--
--  File 'k-to-initialize_TCBs.ads'                                   By MAR.
--
--
--  Initialize Task Control Blocks (TCBs) of tasks. Provides
--  procedures to initialize the special tasks: main, idle and
--  signal handler.
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
with System;

with MaRTE.Stacks_Management;
pragma Elaborate_All (MaRTE.Stacks_Management);

package MaRTE.Kernel.Tasks_Operations.Initialize_TCBs is

   package K renames MaRTE.Kernel;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize
     (T                 : in Task_Id;
      Id                : in Integer;
      Attr              : in Pthread_Attr_T;
      Task_Body         : in Task_Body_Function;
      Arg               : in System.Address;
      Task_Wrapper      : in System.Address);

   --------------------------
   -- Initialize_Main_Task --
   --------------------------
   function Initialize_Main_Task (Prio : in Task_Priority) return Task_Id;

   --------------------------
   -- Initialize_Idle_Task --
   --------------------------
   function Initialize_Idle_Task (Idle_Task_Body : in Task_Body_Function;
                                  Task_Wrapper   : in System.Address)
                                 return Task_Id;

   ------------------------------------
   -- Initialize_Signal_Handler_Task --
   ------------------------------------
   function Initialize_Signal_Handler_Task
     (Signal_Handler_Task_Body : in Task_Body_Function;
      Task_Wrapper             : in System.Address) return Task_Id;

end MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
