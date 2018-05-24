------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'P I T'
--
--                                  Body
--
--
--  File 'pit.adb'                                                    By Mar.
--
--
--  Management of the Intel 8253/82C54/8254 Programmable Interval
--  Timer (PIT) in the PC.
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
with MaRTE.X86_Processor_Type;

package body MaRTE.HAL.PIT is

   package X86_PROC renames MaRTE.X86_Processor_Type;
   use type X86_PROC.Processor_Type;

   -------------------
   -- Program_Timer --
   -------------------
   procedure Program_Timer (Prog : in Unsigned_8) is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.I386 or
                       X86_PROC.Processor = X86_PROC.PENTIUM_I);

      Outb (PIT_CONTROL, Prog);
   end Program_Timer;
   pragma Inline (Program_Timer);

   -------------------
   -- Write_Counter --
   -------------------
   procedure Write_Counter (Counter : in IO_Port; -- PIT_CNT0, 1 o 2
                            Val : in Unsigned_16) is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.I386 or
                       X86_PROC.Processor = X86_PROC.PENTIUM_I);

      Outb   (Counter, Unsigned_8 (Val and 16#FF#)); -- less significant byte
      Outb_P (Counter,
              Unsigned_8 (Shift_Right (Val, 8) and 16#FF#)); -- most sign. byte
   end Write_Counter;
   pragma Inline (Write_Counter);

   ------------------
   -- Read_Counter --
   ------------------
   function Read_Counter (Counter : in IO_Port) return Unsigned_16 is
      Tmp : Unsigned_16;
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.I386 or
                       X86_PROC.Processor = X86_PROC.PENTIUM_I);

      Tmp := Unsigned_16 (Inb (Counter));
      return  Tmp + Shift_Left (Unsigned_16 (Inb (Counter)), 8);
   end Read_Counter;
   pragma Inline (Read_Counter);

   ----------------
   -- Inicialize --
   ----------------
   procedure Inicialize (Cnt0_Ini : in Unsigned_16;
                         Cnt2_Ini : in Unsigned_16) is
   begin
      pragma Assert (X86_PROC.Processor = X86_PROC.I386 or
                       X86_PROC.Processor = X86_PROC.PENTIUM_I);

      Program_Timer (PIT_SEL0 or PIT_INTTC or PIT_LSB or PIT_MSB);
      --             timer0       mode 0
      --               00           00          10         20      = 30
      Write_Counter (PIT_CNT0, Cnt0_Ini);
      if Cnt2_Ini /= 0 then
         Program_Timer (PIT_SEL2 or PIT_SQWAVE or PIT_LSB or PIT_MSB);
         --             timer2        mode 3
         --               80           06            10         20      = b6
         Write_Counter (PIT_CNT2, Cnt2_Ini);
         Outb (16#0061#, (Inb_P (16#61#) and 16#FD#) or 1);
         --  shut up the speaker and enable counting
      end if;
   end Inicialize;

end MaRTE.HAL.PIT;
