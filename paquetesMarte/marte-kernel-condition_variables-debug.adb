------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--      'K e r n e l . C o n d i t i o n _ V a r i a b l e s . D e b u g'
--
--                                  Body
--
--
--  File 'k-cv-debug.adb'                                             By MAR.
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
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Kernel.Scheduler;
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.Kernel.Tasks_Map_Lists_Show;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Map_Lists_Show);

package body MaRTE.Kernel.Condition_Variables.Debug is

   package SCHD    renames K.Scheduler;
   package ML_Show renames K.Tasks_Map_Lists_Show;

   ---------------------
   -- Initialize (CV) --
   ---------------------
   procedure Initialize (Cond : in Condition) is
   begin
      if Condition_Variables_Debug_Messages then
         Put ("  |  NEW?"); Put (Cond.Id);
         --  Put (","); Put (Cond'Unchecked_Access.all'Address);
         Put ("?");
      end if;
   end Initialize;

   -------------
   -- Signal1 --
   -------------
   procedure Signal1 (Cond : in Condition_Descriptor) is
   begin
      if Condition_Variables_Debug_Messages then
         Put ("  |  ("); Put (SCHD.Self.Id);
         Put (","); Put (Integer (SCHD.Self.Active_Prio));
         Put (") Signal ?"); Put (Cond.Id);
         Put (",BT:");
         ML_Show.Show_On_Console (Cond.Blocked_Tasks);
         Put ("?");
      end if;
   end Signal1;

   -------------
   -- Signal2 --
   -------------
   procedure Signal2 (T : in Task_Id) is
   begin
      if Condition_Variables_Debug_Messages then
         Put (" Removing Event ["); Put (Unsigned_64 (T.Suspension_Event.T));
      end if;
   end Signal2;

   ----------------
   -- Broadcast1 --
   ----------------
   procedure Broadcast1 (Cond : in Condition_Descriptor) is
   begin
      if Condition_Variables_Debug_Messages then
         Put ("  |  ("); Put (SCHD.Self.Id);
         Put (","); Put (Integer (SCHD.Self.Active_Prio));
         Put (") Broadcast ?"); Put (Cond.Id);
         Put (",BT:");
         ML_Show.Show_On_Console (Cond.Blocked_Tasks);
         Put ("?");
      end if;
   end Broadcast1;

   ----------------
   -- Broadcast2 --
   ----------------
   procedure Broadcast2 (T : in Task_Id) is
   begin
      if Condition_Variables_Debug_Messages then
         Put (" Removing Event ["); Put (Unsigned_64 (T.Suspension_Event.T));
         Put ("] ("); Put (SCHD.Self.Id);
         Put (","); Put (Integer (SCHD.Self.Active_Prio));
      end if;
   end Broadcast2;

   ----------
   -- Wait --
   ----------
   procedure Wait (Cond : in Condition_Descriptor) is
   begin
      if Condition_Variables_Debug_Messages then
         Put (" and Wait in ?"); Put (Cond.Id);
         Put (",BT:");
         ML_Show.Show_On_Console (Cond.Blocked_Tasks);
         Put ("?");
      end if;
   end Wait;

   ----------------
   -- Timed_Wait --
   ----------------
   procedure Timed_Wait (Cond    : in Condition_Descriptor) is
   begin
      if Condition_Variables_Debug_Messages then
         Put (" and Timed_Wait in ?"); Put (Cond.Id);
         Put (",BT:");
         ML_Show.Show_On_Console (Cond.Blocked_Tasks);
         Put ("? TOut:["); Put (Unsigned_64 (SCHD.Self.Suspension_Event.T));
         Put ("]");
      end if;
   end Timed_Wait;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached is
   begin
      if Condition_Variables_Debug_Messages then
         Put ("  |  TOut_Reached");
      end if;
   end Timeout_Reached;

end MaRTE.Kernel.Condition_Variables.Debug;
