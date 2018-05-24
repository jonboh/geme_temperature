------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                           'L o c a l _ A P I C'
--
--                                   Spec
--
--
--  File 'local_apic.ads'                                              By Mar.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.HAL.TSC;

package MaRTE.HAL.Local_APIC is

   pragma Preelaborate;

   type T_Divide_Value is private;

   --  The timer time base is the processor bus clock divided by this
   --  factor.
   APIC_TDR_DIV_1   : constant T_Divide_Value;
   APIC_TDR_DIV_2   : constant T_Divide_Value;
   APIC_TDR_DIV_4   : constant T_Divide_Value;
   APIC_TDR_DIV_8   : constant T_Divide_Value;
   APIC_TDR_DIV_16  : constant T_Divide_Value;
   APIC_TDR_DIV_32  : constant T_Divide_Value;
   APIC_TDR_DIV_64  : constant T_Divide_Value;
   APIC_TDR_DIV_128 : constant T_Divide_Value;

   procedure Enable_In_Through_Mode
     (Timer_Int_Vector_Num : in Unsigned_32;
      Divide_Value         : in T_Divide_Value);
   pragma Import (C, Enable_In_Through_Mode, "enable_p6_local_apic");
   --  Defined in 'local_apic_c.c'.
   --  Enables the APIC in through mode (interrupts are passed through the
   --  local APIC to the core to support and external interrupt controller.
   --  The vector number associated with the APIC timer will be
   --  'Timer_Int_Vector_Num'. The processor bus frequency is also detected.

   procedure Set_In_Bypass_Mode;
   pragma Import (C, Set_In_Bypass_Mode, "set_in_bypass_mode");
   --  Defined in 'local_apic_c.c'.
   --  This mode effectively removes (bypasses the APIC from the embedded
   --  Pentium processor, causing it to operate as if there were no APIC
   --  present.

   procedure EOI;
   --  This procedure should be called at the end of a APIC timer
   --  interrupt. It is an indication for the local APIC it can issue the
   --  next interrupt.

   type APIC_Timer_Ticks is new Unsigned_32;

   function Processor_Cycles_To_APIC_Timer_Ticks
     (Proc : MaRTE.HAL.HWTime) return APIC_Timer_Ticks;

   function APIC_Timer_Ticks_To_Processor_Cycles
     (Ticks : APIC_Timer_Ticks) return MaRTE.HAL.HWTime;

   procedure Program_Timer (Interval : in APIC_Timer_Ticks);
   --  Loads the timer Initial count register (count-down is immediately
   --  begun from this value).

   function Safe_Longest_Timer_Interval return APIC_Timer_Ticks;
   --  The longest interval that can be used to program the APIC timer.

   procedure Initialize
     (Timer_Int_Vector_Num : in Unsigned_32;
      Divide_Value         : in T_Divide_Value);

private
   type T_Divide_Value is new Unsigned_32;

   APIC_TDR_DIV_1   : constant T_Divide_Value := 16#B#;
   APIC_TDR_DIV_2   : constant T_Divide_Value := 16#0#;
   APIC_TDR_DIV_4   : constant T_Divide_Value := 16#1#;
   APIC_TDR_DIV_8   : constant T_Divide_Value := 16#2#;
   APIC_TDR_DIV_16  : constant T_Divide_Value := 16#3#;
   APIC_TDR_DIV_32  : constant T_Divide_Value := 16#8#;
   APIC_TDR_DIV_64  : constant T_Divide_Value := 16#9#;
   APIC_TDR_DIV_128 : constant T_Divide_Value := 16#A#;

end MaRTE.HAL.Local_APIC;
