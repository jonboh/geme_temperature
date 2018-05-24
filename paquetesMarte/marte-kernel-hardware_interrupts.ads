------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . H a r d w a r e _ I n t e r r u p t s'
--
--                                 Spec
--
--
--  File 'k-hardware_interrupts.ads'                                   By MAR.
--
--
--  Internal hardware interrupts management.
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
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

package MaRTE.Kernel.Hardware_Interrupts is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   -------------------
   -- Type 'Intr_T' --
   -------------------
   subtype Intr_T is K.Intr_T;
   --  subtype of 'HWI.Hardware_Interrupt'

   --------------------------------
   -- Type 'Handler_Return_Code' --
   --------------------------------
   subtype Handler_Return_Code is K.Handler_Return_Code;
   --  New 'Int' to avoid constraint error in case a handler returns
   --  something different of the possible return codes defined below
   POSIX_INTR_HANDLED_NOTIFY : constant Handler_Return_Code :=
     MaRTE.POSIX_Constants.POSIX_INTR_HANDLED_NOTIFY;
   POSIX_INTR_HANDLED_DO_NOT_NOTIFY : constant Handler_Return_Code :=
     MaRTE.POSIX_Constants.POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
   POSIX_INTR_NOT_HANDLED : constant Handler_Return_Code :=
     MaRTE.POSIX_Constants.POSIX_INTR_NOT_HANDLED;

   ---------------------------------------
   -- Type 'Interrupt_Handler_Function' --
   ---------------------------------------
   subtype Interrupt_Handler_Function is K.Interrupt_Handler_Function;
   --  access function (Area : in System.Address; Intr : in Intr_T)
   --                   return Handler_Return_Code;

   ----------------------------
   -- Interrupt associations --
   ----------------------------
   --
   --  Each hardware interrupt can be associated with
   --  'GC.Intr_Connect_Max' handlers and its connected tasks. These
   --  associations are stored in the 'Associations' array.
   type Association_Base is tagged record
      Handler : Interrupt_Handler_Function;
      Area    : System.Address;
      T       : Task_Id;
      Pending : Boolean;
   end record;
   package Associations_Lists is new MaRTE.SLL (Association_Base);
   subtype Association is Associations_Lists.Element_Ac;
   package Associations_Pool is
      new Associations_Lists.Resources (Associations_Lists.Element);

   Associations : array (Intr_T)
     of Associations_Lists.List := (others => Associations_Lists.Null_List);

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached (T : in K.Task_Id);
   pragma Inline (Timeout_Reached);

   ------------------------
   -- Reserve Interrupts --
   ------------------------
   procedure Reserve_Interrupt (Intr : in Intr_T);
   pragma Inline_Always (Reserve_Interrupt);

   function Is_Reserved_Interrupt (Intr : in Intr_T) return Boolean;
   pragma Inline_Always (Reserve_Interrupt);

   ----------------
   -- Initialize --
   ----------------
   --
   --  Initialize associations and user interrupt handlers. Called from
   --  'Kernel.Initialization.Initialize'
   procedure Initialize;

end MaRTE.Kernel.Hardware_Interrupts;
