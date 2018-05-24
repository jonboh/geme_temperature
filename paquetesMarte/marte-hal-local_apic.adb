------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                           'L o c a l _ A P I C'
--
--                                   Body
--
--
--  File 'local_apic.adb'                                              By Mar.
--
--
--  Pentium local Advanced Programmable Interrupt Controller (APIC)
--  management.
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
with Ada.Unchecked_Conversion;
with System;
with MaRTE.X86_Processor_Type;

package body MaRTE.HAL.Local_APIC is

   package X86_PROC renames MaRTE.X86_Processor_Type;
   use type X86_PROC.Processor_Type;

   use type MaRTE.HAL.HWTime;

   --------------------------------------------------------------------------
   -- Local APIC Register Map -----------------------------------------------
   --------------------------------------------------------------------------

   --  APIC base address
   APIC_BASE : constant := 16#Fee00000#;
   --  Registers offsets
   EOI_OFFSET   : constant := 16#0b0#; -- End-Of-Interrupt Register
   TMICT_OFFSET : constant := 16#380#; -- Timer Initial Count Register
   --  Any change in these values should be reflected in 'APIC_BASE' and
   --  'EOI_OFFSET' (file 'x86/boot/pc_asm.h').

   type Unsigned_32_Ac is access Unsigned_32;
   function To_Access is
      new Ada.Unchecked_Conversion (Unsigned_32, Unsigned_32_Ac);

   --  APIC Registers definitions
   EOI_Register : Unsigned_32_Ac;
   TMICT_Register : Unsigned_32_Ac;


   --------------------------------------------------------------------------
   -- Procedures and Functions ----------------------------------------------
   --------------------------------------------------------------------------


   ---------
   -- EOI --
   ---------
   procedure EOI is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.Pentium_II);

      EOI_Register.all := 0;
   end EOI;

   -------------------
   -- Program_Timer --
   -------------------
   procedure Program_Timer (Interval : in APIC_Timer_Ticks) is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.Pentium_II);

      TMICT_Register.all := Unsigned_32 (Interval);
   end Program_Timer;

   --------------------------------------------
   -- Conversions between 'Processor_Cycles' --
   -- and 'APIC_Timer_Ticks'                 --
   --------------------------------------------
   APIC_Timer_Hz : MaRTE.HAL.HWTime;
   pragma Import (C, APIC_Timer_Hz, "apic_timer_hz");
   --  defined in 'local_apic_c.c'

   function Processor_Cycles_To_APIC_Timer_Ticks
     (Proc : MaRTE.HAL.HWTime) return APIC_Timer_Ticks is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.Pentium_II);

      return APIC_Timer_Ticks ((Proc * APIC_Timer_Hz) / TSC.CPU_Hz);
   end Processor_Cycles_To_APIC_Timer_Ticks;

   function APIC_Timer_Ticks_To_Processor_Cycles
     (Ticks : APIC_Timer_Ticks) return MaRTE.HAL.HWTime is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.Pentium_II);

      return (MaRTE.HAL.HWTime (Ticks) * TSC.CPU_Hz) / APIC_Timer_Hz;
   end APIC_Timer_Ticks_To_Processor_Cycles;

   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------
   function Safe_Longest_Timer_Interval return APIC_Timer_Ticks is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.Pentium_II);

      return 16#ffffff00#;
   end Safe_Longest_Timer_Interval;

   -- Initialize --
   ----------------
   procedure Initialize
     (Timer_Int_Vector_Num : in Unsigned_32;
      Divide_Value         : in T_Divide_Value) is
   begin
      EOI_Register   := To_Access (APIC_BASE + EOI_OFFSET);
      TMICT_Register := To_Access (APIC_BASE + TMICT_OFFSET);

      Enable_In_Through_Mode(Timer_Int_Vector_Num , Divide_Value);
   end Initialize;

end MaRTE.HAL.Local_APIC;
