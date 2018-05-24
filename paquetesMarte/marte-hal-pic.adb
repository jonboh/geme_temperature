------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'P I C'
--
--                                  Body
--
--
--  File 'pic.adb'                                                     By MAR.
--
--
--  PC 8259A Programmable Interrupt Controller (PIC) management.
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
--  Based on files 'pic.h' and 'pic.c' from OSKit.
--
------------------------------------------------------------------------------

package body MaRTE.HAL.PIC is

   -------------------
   -- Current Masks --
   -------------------
   Current_Master_Mask : Unsigned_8 := PICM_MASK;
   Current_Slave_Mask  : Unsigned_8 := PICS_MASK;

   ---------------
   -- PICs_Init --
   ---------------
   --
   --  Initialize both PICs.
   procedure PICs_Init (Master_Base : in Unsigned_8;
                        Slave_Base  : in Unsigned_8) is
   begin
      --  Initialize the master.
      Outb_P (Master_Icw, PICM_ICW1);
      Outb_P (Master_Ocw, Master_Base);
      Outb_P (Master_Ocw, PICM_ICW3);
      Outb_P (Master_Ocw, PICM_ICW4);

      --  Initialize the slave.
      Outb_P (Slave_Icw, PICS_ICW1);
      Outb_P (Slave_Ocw, Slave_Base);
      Outb_P (Slave_Ocw, PICS_ICW3);
      Outb_P (Slave_Ocw, PICS_ICW4);

      --  Ack any bogus intrs by setting the End Of Interrupt bit.
      Outb_P (Master_Icw, NON_SPEC_EOI);
      Outb_P (Slave_Icw, NON_SPEC_EOI);

      --  Disable all the PIC's IRQ lines
      Outb_P (Master_Ocw, Current_Master_Mask);
      Outb_P (Slave_Ocw, Current_Slave_Mask);
   end PICs_Init;

   ----------------
   -- Enable_IRQ --
   ----------------
   procedure Enable_IRQ (IRQ_Num : in MaRTE.HAL.HW_Interrupt) is
   begin
      if IRQ_Num <= Last_IRQ_In_Master then
         --  IRQ in Master
         Current_Master_Mask :=
           Current_Master_Mask and not Shift_Left (1, Natural (IRQ_Num));
         Outb_P (Master_Ocw, Current_Master_Mask);
      else
         --  IRQ in Slave
         Current_Master_Mask :=
           Current_Master_Mask and not Shift_Left (1, Natural (CTLR2_IRQ));
         Current_Slave_Mask :=
           Current_Slave_Mask and
           not Shift_Left (1, Natural (IRQ_Num - Last_IRQ_In_Master - 1));
         Outb_P (Master_Ocw, Current_Master_Mask);
         Outb_P (Slave_Ocw, Current_Slave_Mask);
      end if;
   end Enable_IRQ;

   -----------------
   -- Disable_IRQ --
   -----------------
   procedure Disable_IRQ (IRQ_Num : in MaRTE.HAL.HW_Interrupt) is
   begin
      if IRQ_Num <= Last_IRQ_In_Master then
         --  IRQ in Master
         Current_Master_Mask :=
           Current_Master_Mask or Shift_Left (1, Natural (IRQ_Num));
         Outb_P (Master_Ocw, Current_Master_Mask);
      else
         --  IRQ in Slave
         Current_Slave_Mask :=
           Current_Slave_Mask or
           Shift_Left (1, Natural (IRQ_Num - Last_IRQ_In_Master - 1));
         if Current_Slave_Mask = 0 then
            Current_Master_Mask :=
              Current_Master_Mask or Shift_Left (1, Natural (CTLR2_IRQ));
            Outb_P (Master_Ocw, Current_Master_Mask);
         end if;
         Outb_P (Slave_Ocw, Current_Slave_Mask);
      end if;
   end Disable_IRQ;

   --------------------------------
   -- PIC_Master_Acknowledge_IRQ --
   --------------------------------
   procedure PIC_Master_Acknowledge_IRQ is
   begin
      Outb (Master_Icw, NON_SPEC_EOI);
   end PIC_Master_Acknowledge_IRQ;

   -------------------------------
   -- PIC_Slave_Acknowledge_IRQ --
   -------------------------------
   procedure PIC_Slave_Acknowledge_IRQ is
   begin
      Outb (Slave_Icw, NON_SPEC_EOI);
   end PIC_Slave_Acknowledge_IRQ;

end MaRTE.HAL.PIC;
