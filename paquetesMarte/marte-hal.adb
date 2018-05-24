------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'H a r d w a r e _ I n t e r f a c e'
--
--                                  Body
--
--
--  File 'hardware_interface.adb'                                   By MAR.
--
--  PC Version.
--
--  This package is the border between the hardware dependent and hardware
--  independent parts of the kernel.
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
with Unchecked_Conversion;
with MaRTE.X86_Processor_Type;

with MaRTE.HAL.Processor_Registers;
with MaRTE.HAL.PIC;
with MaRTE.HAL.Interrupt_Tables;
with MaRTE.HAL.RTC;
with MaRTE.HAL.TSC; use MaRTE.HAL.TSC;
with MaRTE.HAL.Local_APIC;
with MaRTE.HAL.PIT; use MaRTE.HAL.PIT;

with System.Machine_Code; use System.Machine_Code;

package body MaRTE.HAL is

   Initialized : Boolean := False;

   package X86_PROC renames MaRTE.X86_Processor_Type;
   use type X86_PROC.Processor_Type;

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   ---------------------
   -- Timer_Interrupt --
   ---------------------
   function Timer_Interrupt return HW_Interrupt is
   begin
      if X86_PROC.Processor = X86_PROC.PENTIUM_II then
         --  Using local APIC timer
         return LOCAL_APIC_TIMER_IRQ;
      else
         --  PIT is used for timers
         return TIMER_IRQ;
      end if;
   end Timer_Interrupt;

   ----------------------------------
   -- Default_HW_Interrupt_Handler --
   ----------------------------------
   procedure Default_HW_Interrupt_Handler_C (State : in Trap_State_Ac);
   pragma Import (C, Default_HW_Interrupt_Handler_C,
                  "base_irq_default_handler");
   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac) is
   begin
      Default_HW_Interrupt_Handler_C (State);
   end Default_HW_Interrupt_Handler;

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------
   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is
   begin
      pragma Assert (Initialized);
      Interrupt_Tables.Install_IRQ_Handler (Int_Num, Handler);
   end Install_HW_Interrupt_Handler;
   pragma Inline (Install_HW_Interrupt_Handler);

   ------------------------
   -- Disable_Interrupts --
   ------------------------
   --  pragma Export (C, Disable_Interrupts, "hwi_disable_interrupts");
   procedure Disable_Interrupts renames Processor_Registers.CLI;
   pragma Inline_Always (Disable_Interrupts);

   -----------------------
   -- Enable_Interrupts --
   -----------------------
   procedure Enable_Interrupts renames Processor_Registers.STI;
   pragma Inline_Always (Enable_Interrupts);

   ----------------------------------------------------
   -- Hardware_Interrupt_Controller_Enable_Interrupt --
   ----------------------------------------------------
   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt) is
   begin
      if Int /= LOCAL_APIC_TIMER_IRQ then
         PIC.Enable_IRQ (Int);
      end if;
   end Hardware_Interrupt_Controller_Enable_Interrupt;

   -----------------------------------------------------
   -- Hardware_Interrupt_Controller_Disable_Interrupt --
   -----------------------------------------------------
   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt) is
   begin
      if Int /= LOCAL_APIC_TIMER_IRQ then
         PIC.Disable_IRQ (Int);
      end if;
   end Hardware_Interrupt_Controller_Disable_Interrupt;

   ------------------------------------------
   -- Enable_Hardware_Interrupt_Controller --
   ------------------------------------------
   procedure Enable_Hardware_Interrupt_Controller is
   begin
      PIC.PIC_Master_Acknowledge_IRQ;
      PIC.PIC_Slave_Acknowledge_IRQ;
   end Enable_Hardware_Interrupt_Controller;
   pragma Inline (Enable_Hardware_Interrupt_Controller);

   ----------------------------
   -- Are_Interrupts_Enabled --
   ----------------------------
   function Are_Interrupts_Enabled return Boolean
     renames Processor_Registers.Are_Interrupts_Enabled;
   pragma Inline (Are_Interrupts_Enabled);

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------
   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer)
     renames Processor_Registers.Save_Flags_And_Disable_Interrupts;
   pragma Inline_Always (Save_Flags_And_Disable_Interrupts);

   function Save_Flags return Integer
     renames Processor_Registers.Save_Flags;
   pragma Inline_Always (Save_Flags);

   procedure Restore_Flags (EFlags : in Integer)
     renames Processor_Registers.Restore_Flags;
   --  Exported to C  with name "restore_flags"
   pragma Inline_Always (Restore_Flags);

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Unsigned_32;
                      Bit : in Unsigned_32) is
   begin
      Asm ("bts %2, %0", Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
            Unsigned_32'Asm_Input ("r", Bit)), "", True);
   end Bit_Set;

   procedure Bit_Reset (Bit_Field : in out Unsigned_32;
                        Bit : in Unsigned_32) is
   begin
      Asm ("btr %2, %0", Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
            Unsigned_32'Asm_Input ("r", Bit)), "", True);
   end Bit_Reset;

   procedure Bit_Scan_Reverse (Bit_Field : in Unsigned_32;
                               Bit : out Unsigned_32) is
   begin
      Asm ("bsrl %1, %0;" &
           "jnz 1f;"      &
           "movl $-1,%0;" &
           "1:",
           Unsigned_32'Asm_Output ("=d", Bit),
           Unsigned_32'Asm_Input ("d", Bit_Field),
           "", True);
   end Bit_Scan_Reverse;

   procedure Bit_Scan_Forward (Bit_Field : in Unsigned_32;
                               Bit : out Unsigned_32) is
   begin
      Asm ("bsfl %1, %0;" &
           "jnz 1f;"      &
           "movl $-1,%0;" &
           "1:",
           Unsigned_32'Asm_Output ("=d", Bit),
           Unsigned_32'Asm_Input ("d", Bit_Field),
           "", True);
   end Bit_Scan_Forward;

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   --  Status of PIT counters (only used if TSC is NOT used)
   Cnt0_Ini : Unsigned_16 := 16#FF00#; -- Stores initial value of counter 0
   Cnt2_Ini : constant Unsigned_16 := 16#FF00#; -- Initial value of CNT 2
   -- Global system time from reset (only used if TSC is NOT used)
   Total_Time : HWTime := 0;

   --  'HWT_HZ' is the number of 'HWTime' units per second. It is a
   --  logical constant that takes value in 'Initialize'
   HWT_HZ : HWTime;

   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------
   function Safe_Longest_Timer_Interval return HWTime is
   begin
      pragma Assert (Initialized);
      case X86_PROC.Processor is
         when X86_PROC.Pentium_II =>
            --  Using local APIC timer
            return Local_APIC.APIC_Timer_Ticks_To_Processor_Cycles
              (Local_APIC.Safe_Longest_Timer_Interval);

         when X86_PROC.Pentium_I | X86_PROC.I386 =>
            return (PIT.Safe_Longest_Timer_Interval * HWT_HZ) / PIT.PIT_HZ;
      end case;
   end Safe_Longest_Timer_Interval;

   function To_HWT_NS is new Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Unchecked_Conversion (HWTime, Duration);
   NS_Per_S : constant HWTime := 10#1#E9;

   ------------------------
   -- HWTime_To_Duration --
   ------------------------
   function HWTime_To_Duration (Th : in HWTime) return Duration is
      S : HWTime; -- := Th;
   begin
      pragma Assert (HWT_HZ /= 0);
      S := Th / HWT_HZ;
      return To_Duration (S * NS_Per_S +
                          ((Th - S * HWT_HZ) * NS_Per_S) /
                          HWT_HZ);
   end HWTime_To_Duration;
   pragma Inline (HWTime_To_Duration);

   ------------------------
   -- Duration_To_HWTime --
   ------------------------
   function Duration_To_HWTime (D : in Duration) return HWTime is
      S : HWTime; -- := To_HWT_NS (D);
   begin
      pragma Assert (HWT_HZ /= 0);
      S  := To_HWT_NS (D) / NS_Per_S;
      return S * HWT_HZ +
             ((To_HWT_NS (D) - S * NS_Per_S) * HWT_HZ) / NS_Per_S;
   end Duration_To_HWTime;
   pragma Inline (Duration_To_HWTime);

   -------------------
   -- Program_Timer --
   -------------------
   --
   --  Should be called with interrupts disabled.
   procedure Program_Timer (Timer           : in  HW_Timers;
                            Interval        : in  HWTime;
                            Next_Activation : out HWTime) is
   begin
      pragma Assert (Initialized);
      case X86_PROC.Processor is
         when X86_PROC.Pentium_II => --  APIC Timer and TSC used

            --  Reprograms timer
            Local_APIC.Program_Timer
            (Local_APIC.Processor_Cycles_To_APIC_Timer_Ticks (Interval));

            --  Gets next activation
            Next_Activation := TSC.Read_TSC + Interval;

         when X86_PROC.Pentium_I =>  --  TSC used to measure the time

            --  Reprograms timer 0
            PIT.Write_Counter (PIT_CNT0,
                               Unsigned_16 ((Interval * PIT.PIT_HZ) / HWT_HZ));

            --  Gets next activation
            Next_Activation := TSC.Read_TSC + Interval;

         when X86_PROC.I386 =>  --  TSC not used to measure the time
            declare
               C0_Count, C2_Before, C2_After, Prog_Time : Unsigned_16;
            begin
               PIT.Program_Timer (PIT_LATCH_CNT0_2); -- latches both timers
               C0_Count := Cnt0_Ini - PIT.Read_Counter (PIT_CNT0);
               C2_Before := PIT.Read_Counter (PIT_CNT2);

               --  Reprograms timer 0
               PIT.Write_Counter (PIT_CNT0, Unsigned_16 (Interval));

               PIT.Program_Timer (PIT_LATCH_CNT0_2); -- latches both timers
               Cnt0_Ini := PIT.Read_Counter (PIT_CNT0);
               C2_After := PIT.Read_Counter (PIT_CNT2);

               --  Gets the programation time of timer 0
               if C2_After < C2_Before then
                  Prog_Time := (C2_Before - C2_After) / 2;
               else -- C2_After >= C2_Before
                  Prog_Time := (C2_Before - C2_After + Cnt2_Ini) / 2;
               end if;

               --  Updates total software time
               Total_Time := Total_Time
                 + HWTime (Prog_Time) + HWTime (C0_Count);

               --  Gets next activation
               Next_Activation := Total_Time + Interval;
            end;
      end case;
   end Program_Timer;

   ---------------------
   -- Get_HWTime_Slow --
   ---------------------
   function Get_HWTime_Slow return HWTime is
   begin
      pragma Assert (Initialized);
      case X86_PROC.Processor is

         when X86_PROC.Pentium_I | X86_PROC.Pentium_II =>
            --  Reads TSC
            return TSC.Read_TSC;

         when X86_PROC.I386 =>
            --  Uses the software time and the timer 0 in order to get the
            --  time. Disable and enable interrupts.
            --
            declare
               Flags : Integer;
               Ret : HWTime;
            begin
               Flags := Save_Flags;
               Disable_Interrupts;
               PIT.Program_Timer (PIT_LATCH_CNT0);
               Ret := Total_Time
                 + HWTime (Cnt0_Ini - PIT.Read_Counter (PIT_CNT0));
               Restore_Flags (Flags);
               return Ret;
            end;
      end case;
   end Get_HWTime_Slow;

   ----------------
   -- Get_HWTime --
   ----------------
   function Get_HWTime return HWTime is
   begin
      pragma Assert (Initialized);
      case X86_PROC.Processor is

         when X86_PROC.Pentium_I | X86_PROC.Pentium_II =>
            --  Reads TSC
            return TSC.Read_TSC;

         when X86_PROC.I386 =>
            --  Uses the software time and the timer 0 in order to get the
            --  time. Assumes interrupts disabled.
            pragma Assert (not Are_Interrupts_Enabled);
            PIT.Program_Timer (PIT_LATCH_CNT0);
            return Total_Time
              + HWTime (Cnt0_Ini - PIT.Read_Counter (PIT_CNT0));
      end case;
   end Get_HWTime;

   ---------------------------
   -- Get_HWClock_Frequency --
   ---------------------------
   function Get_HWClock_Frequency return HWTime is
   begin
      pragma Assert (HWT_HZ /= 0);
      return HWT_HZ;
   end Get_HWClock_Frequency;

   -------------------
   -- CPU_Frequency --
   -------------------
   function CPU_Frequency return HWTime is
   begin
      case X86_PROC.Processor is

         when X86_PROC.Pentium_I | X86_PROC.Pentium_II =>
            return Get_HWClock_Frequency;

         when X86_PROC.I386 =>
            --  Assume a 100MHz processor (only used to get the value for
            --  Time_Suspension_Minimum)
            return 100_000_000;
      end case;
   end CPU_Frequency;

   ------------------------------------
   -- Compulsory_Timer_Reprogramming --
   ------------------------------------
   function Compulsory_Timer_Reprogramming return Boolean is
   begin
      case X86_PROC.Processor is

         when X86_PROC.Pentium_I | X86_PROC.Pentium_II =>
            --  Time is measured using the TSC
            return False;

         when X86_PROC.I386 =>
            --  Time is measured using the PIT, so it should always be
            --  programmed to allow us to keep the system time
            --  'Total_Time'.
            return True;
      end case;
   end Compulsory_Timer_Reprogramming;

   ----------------------------
   -- RTC_HWTime_Since_Epoch --
   ----------------------------
   function RTC_HWTime_Since_Epoch return HWTime is
   begin
      return Duration_To_HWTime
        (Duration (RTC.RTC_Time_To_Seconds_Since_Epoch));
   end RTC_HWTime_Since_Epoch;

   ---------------------------------------------------------------------------
   -- Context Swich ----------------------------------------------------------
   ---------------------------------------------------------------------------
   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address)
     renames Processor_Registers.Context_Switch;
   pragma Inline (Context_Switch);

   procedure Change_To_Context (New_Task : in System.Address)
     renames Processor_Registers.Change_To_Context;
   pragma Inline (Change_To_Context);

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address)
     renames Processor_Registers.Change_Return_Address_Of_Preempted_Task;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------
   procedure Finish_Hardware_Use is
   begin
      case X86_PROC.Processor is

         when X86_PROC.Pentium_II =>
            Local_APIC.Set_In_Bypass_Mode;

         when X86_PROC.Pentium_I | X86_PROC.I386 =>
            null;
      end case;
   end Finish_Hardware_Use;

   ---------------
   -- PICs base --
   ---------------
   PIC_Master_IRQ_Base : constant := 16#20#;
   PIC_Slave_IRQ_Base  : constant := 16#28#;
   --  Any change in these values should be refected in
   --  'BASE_IRQ_MASTER_BASE' and 'BASE_IRQ_SLAVE_BASE' (file
   --  'include/oskit/x86/pc/base_irq.h').

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);

      pragma Assert (Duration'Small = 1.0/1e9);
      --  We rely that Duration is an exact count of nanoseconds.



      --  Initialize TSC (Pentiums) and give value to HWT_HZ
      case X86_PROC.Processor is
         when X86_PROC.Pentium_I | X86_PROC.Pentium_II =>
            TSC.Initialize;
            HWT_HZ := TSC.CPU_Hz;

         when X86_PROC.I386 =>
            HWT_HZ := PIT.PIT_HZ;
      end case;

      --  Configure interrupts.
      PIC.PICs_Init (Master_Base => PIC_Master_IRQ_Base,
                     Slave_Base  => PIC_Slave_IRQ_Base);
      Interrupt_Tables.Inicialize (Base_IDT => 16#0#);

      --  Initialize hardware timer
      case X86_PROC.Processor is
         when X86_PROC.Pentium_II =>  --  Use local APIC timer

            Local_APIC.Initialize
              (Timer_Int_Vector_Num => PIC_Slave_IRQ_Base + 8,
               Divide_Value         => Local_APIC.APIC_TDR_DIV_1);
            --  The local APIC timer interrupt is immediately
            --  following the last slave PIC interrupt. Any change
            --  here should be reflected in
            --  'x86/boot/base_irq_inittab.S'.

         when X86_PROC.Pentium_I => -- PIT is used only for timers

            PIT.Inicialize (Cnt0_Ini => 16#FF00#, Cnt2_Ini => 0);
            --  Initial value for counter 0 big enough to avoid
            --  interrupt during kernel initialization. Counter 2 is
            --  not programmed, so it can be used for the speaker.

         when X86_PROC.I386 => -- PIT used as timer and to measure time

            PIT.Inicialize (Cnt0_Ini, Cnt2_Ini);
            --  Program counters 0 and 2. So counter 2 can not be used
            --  for the speaker.
      end case;

      Initialized := True;
   end Initialize;


end MaRTE.HAL;
