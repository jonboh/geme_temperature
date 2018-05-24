------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'H a r d w a r e _ I n t e r f a c e'
--
--                                  Spec
--
--
--  File 'hardware_interface.ads'                                   By MAR.
--
--
--  This package is the border between the hardware dependent and hardware
--  independent parts of the kernel.
--
--  PC Version.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Configuration_Parameters; -- to use MaRTE_Architecture

package MaRTE.HAL is

   pragma Preelaborate;

   package CP renames Configuration_Parameters;

   use type CP.Supported_Architectures;

   ------------------------
   -- MaRTE Architecture --
   ------------------------

   --  These constants are exported to be used from outside MaRTE kernel

   ARCHITECTURE : constant CP.Supported_Architectures :=
     CP.MaRTE_Architecture'First;
   for ARCHITECTURE'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, ARCHITECTURE, "hal__architecture");

   X86_ARCH : constant CP.Supported_Architectures := CP.ARCH_X86;
   for X86_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, X86_ARCH, "hal__x86_arch");

   LINUX_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX;
   for LINUX_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, LINUX_ARCH, "hal__linux_arch");

   LINUX_LIB_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX_LIB;
   for LINUX_LIB_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, LINUX_LIB_ARCH, "hal__linux_lib_arch");

   ----------------------------------------------------------------------------
   -- Interrupts --------------------------------------------------------------
   ----------------------------------------------------------------------------
   type HW_Interrupt is new Integer_32 range 0 .. 16;
   for HW_Interrupt'Size use 32;

   ---------------
   --  PC IRQs  --
   ---------------
   --  Hardware interrupts: PCs have 2 PICs with 8 IRQ each plus the local
   --  APIC timer interrupt.
   TIMER_IRQ       : constant HW_Interrupt := 0; --  Master PIC
   KEYBOARD_IRQ    : constant HW_Interrupt := 1; --  Master PIC
   CTLR2_IRQ       : constant HW_Interrupt := 2; --  Master PIC
   SERIAL2_IRQ     : constant HW_Interrupt := 3; --  Master PIC
   SERIAL1_IRQ     : constant HW_Interrupt := 4; --  Master PIC
   PARALLEL2_IRQ   : constant HW_Interrupt := 5; --  Master PIC
   DISKETTE_IRQ    : constant HW_Interrupt := 6; --  Master PIC
   PARALLEL1_IRQ   : constant HW_Interrupt := 7; --  Master PIC
   RTC_IRQ         : constant HW_Interrupt := 8;  --  Slave PIC
   SOFT_IRQ        : constant HW_Interrupt := 9;  --  Slave PIC
   RESERVED1_IRQ   : constant HW_Interrupt := 10; --  Slave PIC
   RESERVED2_IRQ   : constant HW_Interrupt := 11; --  Slave PIC
   RESERVED3_IRQ   : constant HW_Interrupt := 12; --  Slave PIC
   COPROCESSOR_IRQ : constant HW_Interrupt := 13; --  Slave PIC
   FIXED_DISK_IRQ  : constant HW_Interrupt := 14; --  Slave PIC
   RESERVED4_IRQ   : constant HW_Interrupt := 15; --  Slave PIC
   LOCAL_APIC_TIMER_IRQ : constant HW_Interrupt := 16; -- Local APIC timer
   --  If it is changed the value of 'LOCAL_APIC_TIMER_IRQ' changes
   --  should be performed in 'x86/boot/base_irq_inittab.S' as well.

   --  Interrupt used by system hardware timer
   function Timer_Interrupt return HW_Interrupt;

   type Trap_State is record --  PC
      GS : Unsigned_32;
      FS : Unsigned_32;
      ES : Unsigned_32;
      DS : Unsigned_32;

      EDI : Unsigned_32;
      ESI : Unsigned_32;
      EBP : Unsigned_32;
      CR2 : Unsigned_32;
      EBX : Unsigned_32;
      EDX : Unsigned_32;
      ECX : Unsigned_32;
      EAX : Unsigned_32;

      TRAPNO : Unsigned_32;

      ERR : Unsigned_32;

      EIP :    Unsigned_32;
      CS :     Unsigned_32;
      EFlags : Unsigned_32;
      ESP :    Unsigned_32;
      SS :     Unsigned_32;

      V86_ES : Unsigned_32;
      V86_DS : Unsigned_32;
      V86_FS : Unsigned_32;
      V86_GS : Unsigned_32;
   end record;
   pragma Convention (C, Trap_State);
   type Trap_State_Ac is access Trap_State;

   type HW_Interrupt_Handler is access
     procedure (State : in Trap_State_Ac);

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac);

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler);

   procedure Disable_Interrupts;
   pragma Export (C, Disable_Interrupts, "marte__hal__disable_interrupts");
   pragma Inline_Always (Disable_Interrupts);

   procedure Enable_Interrupts;
   pragma Export (C, Enable_Interrupts, "marte__hal__enable_interrupts");
   pragma Inline_Always (Enable_Interrupts);

   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt);

   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt);

   procedure Enable_Hardware_Interrupt_Controller;
   pragma Inline (Enable_Hardware_Interrupt_Controller);
   --  Enable the hardware interrupt controller to generate interrupts for
   --  the Processor

   function Are_Interrupts_Enabled return Boolean;
   pragma Inline (Are_Interrupts_Enabled);

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------
   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer);
   pragma Inline_Always (Save_Flags_And_Disable_Interrupts);

   function Save_Flags return Integer;
   pragma Inline_Always (Save_Flags);

   procedure Restore_Flags (EFlags : in Integer);
   --  Exported to C  with name "restore_flags"
   pragma Inline_Always (Restore_Flags);

   ----------------------------------------------------------------------------
   -- Stack Pointer Register --------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Stack_Pointer_Register return Unsigned_32;
   pragma Import (C, Get_Stack_Pointer_Register, "get_esp_register");
   pragma Inline (Get_Stack_Pointer_Register);

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------
   procedure Bit_Set (Bit_Field : in out Unsigned_32; Bit : in Unsigned_32);
   pragma Inline_Always (Bit_Set);
   --  Sets to 1 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Reset (Bit_Field : in out Unsigned_32; Bit : in Unsigned_32);
   pragma Inline_Always (Bit_Reset);
   --  Sets to 0 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Scan_Forward (Bit_Field : in Unsigned_32;
                               Bit       : out Unsigned_32);
   pragma Inline_Always (Bit_Scan_Forward);
   --  Returns in 'Bit' the position of the LEST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   procedure Bit_Scan_Reverse (Bit_Field : in Unsigned_32;
                               Bit       : out Unsigned_32);
   pragma Inline_Always (Bit_Scan_Reverse);
   --  Returns in 'Bit' the position of the MOST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   Last_HW_Timer : constant Integer_32 := 0; -- PC

   type HW_Timers is new Integer_32 range 0 .. Last_HW_Timer;
   for HW_Timers'Size use 32;

   HW_Timer_0 : constant HW_Timers := 0;

   type HWTime is new Unsigned_64;

   function Get_HWTime_Slow return HWTime;
   --  Disable and enable interrupts when using the PIC for timers and time
   --  measure purposes. Equivalent to Get_HWTime when using TSC.
   pragma Inline (Get_HWTime_Slow);

   function Get_HWTime return HWTime;
   --  Assumes interrupts disabled when using the PIC for timers and time
   --  measure purposes.
   pragma Inline (Get_HWTime);

   function Safe_Longest_Timer_Interval return HWTime;
   --  The longest interval that can be used to program the timer.

   function HWTime_To_Duration (Th : in HWTime) return Duration;
   pragma Inline (HWTime_To_Duration);

   function Duration_To_HWTime (D : in Duration)  return HWTime;
   pragma Inline (Duration_To_HWTime);

   function Get_HWClock_Frequency return HWTime;
   --  Ticks per second
   pragma Inline (Get_HWClock_Frequency);

   function CPU_Frequency return HWTime; --  Ticks per second

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime);
   pragma Inline (Program_Timer);
   function Compulsory_Timer_Reprogramming return Boolean;
   --  Returns true if the timer should always be reprogrammed even if
   --  there isn't any event. This could happen, for example, when the
   --  timer interrupts are also used for the system clock.

   ---------------------
   -- Real Time Clock --
   ---------------------
   RTC_Available : constant Boolean := True;
   function RTC_HWTime_Since_Epoch return HWTime;

   ---------------------------------------------------------------------------
   -- Context Swich ----------------------------------------------------------
   ---------------------------------------------------------------------------
   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address);
   pragma Inline (Context_Switch);

   procedure Change_To_Context (New_Task : in System.Address);
   pragma Inline (Change_To_Context);

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address);

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------
   procedure Finish_Hardware_Use;
   --  Invoked while finishing the application to leave the hardware used
   --  by MaRTE OS in a known and stable status. Called from 'our_exit' in
   --  'base_console_init.c'.

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize;

end MaRTE.HAL;
