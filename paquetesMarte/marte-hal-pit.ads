------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'P I T'
--
--                                  Spec
--
--
--  File 'pit.ads'                                                    By Mar.
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
--  Based on file 'pit.h' from OSKit.
--
------------------------------------------------------------------------------

with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.HAL.IO; use MaRTE.HAL.IO; --  for 'IO_Port'

package MaRTE.HAL.PIT is

   pragma Preelaborate;
   --
   --  I/O port addresses of the PIT registers.

   PIT_CNT0    : constant IO_Port := 16#40#; -- timer 0 counter port
   PIT_CNT1    : constant IO_Port := 16#41#; -- timer 1 counter port
   PIT_CNT2    : constant IO_Port := 16#42#; -- timer 2 counter port
   PIT_CONTROL : constant IO_Port := 16#43#; -- timer control port

   --
   --  Control register bit definitions

   PIT_SEL0  : constant Unsigned_8 := 16#00#;   -- select counter 0
   PIT_SEL1  : constant Unsigned_8 := 16#40#;   -- select counter 1
   PIT_SEL2  : constant Unsigned_8 := 16#80#;   -- select counter 2
   PIT_INTTC   : constant Unsigned_8 := 16#00#; -- mode 0, intr on terminal cnt
   PIT_ONESHOT : constant Unsigned_8 := 16#02#; -- mode 1, one shot
   PIT_RATEGEN : constant Unsigned_8 := 16#04#; -- mode 2, rate generator
   PIT_SQWAVE  : constant Unsigned_8 := 16#06#; -- mode 3, square wave
   PIT_SWSTROBE : constant Unsigned_8 := 16#08#; -- mode 4,s/w triggered strobe
   PIT_HWSTROBE : constant Unsigned_8 := 16#0a#; -- mode 5,h/w triggered strobe
   PIT_LATCH    : constant Unsigned_8 := 16#00#; -- latch counter for reading
   PIT_LSB   : constant Unsigned_8 := 16#10#; -- r/w counter LSB
   PIT_MSB   : constant Unsigned_8 := 16#20#; -- r/w counter MSB
   PIT_16BIT : constant Unsigned_8 := 16#30#; -- r/w counter 16 bits, LSB first
   PIT_BCD   : constant Unsigned_8 := 16#01#; -- count in BCD

   --
   --  Read-back mode constants ('rtlinux/rt_time.c')
   PIT_LATCH_CNT0   : constant Unsigned_8 := 16#D2#; -- 1101 0010
   PIT_LATCH_CNT2   : constant Unsigned_8 := 16#D8#; -- 1101 1000
   PIT_LATCH_CNT0_2 : constant Unsigned_8 := 16#DA#; -- 1101 1010

   PIT_LATCH_CTR_CNT0_2 : constant Unsigned_8 := 2#1100_1010#;

   --
   --  Clock speed at which the standard interval timers in the PC are driven,
   --  in hertz (ticks per second) and nanoseconds per tick, respectively.
   --  The whole count lasts 54 ms approximately.

   PIT_HZ : constant :=  1193182; -- ticks/sec
   PIT_NS : constant :=  1000000000 / PIT_HZ; -- ~838.1 nsec/tick

   Safe_Longest_Timer_Interval : constant := 59659; -- ~50ms
   --  The longest interval that can be used to program the PIT counters.

   procedure Program_Timer (Prog : in Unsigned_8);
   pragma Inline (Program_Timer);

   procedure Write_Counter (Counter : in IO_Port; -- PIT_CNT0, 1 o 2
                            Val : in Unsigned_16);
   pragma Inline (Write_Counter);

   function Read_Counter (Counter : in IO_Port) return Unsigned_16;
   pragma Inline (Read_Counter);

   procedure Inicialize (Cnt0_Ini : in Unsigned_16;
                         Cnt2_Ini : in Unsigned_16);
end MaRTE.HAL.PIT;
