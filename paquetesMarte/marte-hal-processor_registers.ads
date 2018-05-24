------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'P r o c e s s o r _ R e g i s t e r s'
--
--                                  Spec
--
--
--  File 'processor_registers.ads'                                    By Mar.
--
--
--  x86 registers.
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

with System; --  for 'Address'
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.HAL.Processor_Registers is

   pragma Preelaborate;
   --  pragma Elaborate_Body;

   -------------------
   -- CPU Registers --
   -------------------
   function Get_ESP_Register return Unsigned_32;
   pragma Import (C, Get_ESP_Register);
   --  Defined in 'processor_registers.c'.
   --------------------
   -- Interrupt Flag --
   --------------------
   procedure CLI;
   pragma Inline_Always (CLI);
   pragma Export (C, CLI);
   procedure STI;
   pragma Inline_Always (STI);
   pragma Export (C, STI);

   ---------------------------
   -- Are_Interrupts_Enable --
   ---------------------------
   function Are_Interrupts_Enabled return Boolean;

   --------------------------------------------
   -- Save and restore the "eflags" register --
   --------------------------------------------
   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer);
   pragma Inline_Always (Save_Flags_And_Disable_Interrupts);

   function Save_Flags return Integer;
   pragma Inline_Always (Save_Flags);

   procedure Restore_Flags (EFlags : in Integer);
   pragma Inline_Always (Restore_Flags);

   -------------------
   -- Context Swich --
   -------------------
   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address);
   pragma Inline (Change_Return_Address_Of_Preempted_Task);

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address);
   pragma Inline (Context_Switch);

   procedure Change_To_Context (New_Task : in System.Address);
   pragma Inline (Change_To_Context);

end MaRTE.HAL.Processor_Registers;
