------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'P I C'
--
--                                  Spec
--
--
--  File 'pic.ads'                                                     By MAR.
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
------------------------------------------------------------------------------

with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;

package MaRTE.HAL.PIC is

   pragma Preelaborate;

   --  The following are definitions used to locate the PICs in the system
   Master_Pic_IObase : constant IO_Port := 16#20#;
   Slave_Pic_IObase  : constant IO_Port := 16#A0#;
   Off_Icw           : constant IO_Port := 0;
   Off_Ocw           : constant IO_Port := 1;

   Master_Icw : constant IO_Port := Master_Pic_IObase + Off_Icw;
   Master_Ocw : constant IO_Port := Master_Pic_IObase + Off_Ocw;
   Slave_Icw  : constant IO_Port := Slave_Pic_IObase  + Off_Icw;
   Slave_Ocw  : constant IO_Port := Slave_Pic_IObase  + Off_Ocw;

   --  The following banks of definitions ICW1, ICW2, ICW3, and ICW4 are used
   --  to define the fields of the various ICWs for initialisation of the PICs

   --      ICW1
   ICW_TEMPLATE : constant Unsigned_8 := 16#10#;

   --  LEVL_TRIGGER : constant Unsigned_8 := 16#08#;
   EDGE_TRIGGER : constant Unsigned_8 := 16#00#;
   --  ADDR_INTRVL4 : constant Unsigned_8 := 16#04#;
   ADDR_INTRVL8 : constant Unsigned_8 := 16#00#;
   --  SINGLE_MODE : constant Unsigned_8 := 16#02#;
   CASCADE_MODE : constant Unsigned_8 := 16#00#;
   ICW4_NEEDED : constant Unsigned_8 := 16#01#;
   --  NO_ICW4_NEED : constant Unsigned_8 := 16#00#;

   --      ICW2 is the programmable interrupt vector base, not defined here.
   --

   --      ICW3
   --
   --  SLAVE_ON_IR0 : constant Unsigned_8 := 16#01#;
   --  SLAVE_ON_IR1 : constant Unsigned_8 := 16#02#;
   SLAVE_ON_IR2 : constant Unsigned_8 := 16#04#;
   --  SLAVE_ON_IR3 : constant Unsigned_8 := 16#08#;
   --  SLAVE_ON_IR4 : constant Unsigned_8 := 16#10#;
   --  SLAVE_ON_IR5 : constant Unsigned_8 := 16#20#;
   --  SLAVE_ON_IR6 : constant Unsigned_8 := 16#40#;
   --  SLAVE_ON_IR7 : constant Unsigned_8 := 16#80#;

   --  I_AM_SLAVE_0 : constant Unsigned_8 := 16#00#;
   --  I_AM_SLAVE_1 : constant Unsigned_8 := 16#01#;
   I_AM_SLAVE_2 : constant Unsigned_8 := 16#02#;
   --  I_AM_SLAVE_3 : constant Unsigned_8 := 16#03#;
   --  I_AM_SLAVE_4 : constant Unsigned_8 := 16#04#;
   --  I_AM_SLAVE_5 : constant Unsigned_8 := 16#05#;
   --  I_AM_SLAVE_6 : constant Unsigned_8 := 16#06#;
   --  I_AM_SLAVE_7 : constant Unsigned_8 := 16#07#;

   --      ICW4
   --
   --  SNF_MODE_ENA : constant Unsigned_8 := 16#10#;
   SNF_MODE_DIS : constant Unsigned_8 := 16#00#;
   --  BUFFERD_MODE : constant Unsigned_8 := 16#08#;
   NONBUFD_MODE : constant Unsigned_8 := 16#00#;
   AUTO_EOI_MOD : constant Unsigned_8 := 16#02#;
   NRML_EOI_MOD : constant Unsigned_8 := 16#00#;
   I8086_EMM_MOD : constant Unsigned_8 := 16#01#;
   --  SET_MCS_MODE : constant Unsigned_8 := 16#00#;

   --      OCW1
   --
   PICM_MASK : constant Unsigned_8 := 16#FF#;
   PICS_MASK : constant Unsigned_8 := 16#FF#;

   --      OCW2
   --
   NON_SPEC_EOI : constant Unsigned_8 := 16#20#;
   --  SPECIFIC_EOI : constant Unsigned_8 := 16#60#;
   --  ROT_NON_SPEC : constant Unsigned_8 := 16#a0#;
   --  SET_ROT_AEOI : constant Unsigned_8 := 16#80#;
   --  RSET_ROTAEOI : constant Unsigned_8 := 16#00#;
   --  ROT_SPEC_EOI : constant Unsigned_8 := 16#e0#;
   --  SET_PRIORITY : constant Unsigned_8 := 16#c0#;
   --  NO_OPERATION : constant Unsigned_8 := 16#40#;

   --  SEND_EOI_IR0 : constant Unsigned_8 := 16#00#;
   --  SEND_EOI_IR1 : constant Unsigned_8 := 16#01#;
   --  SEND_EOI_IR2 : constant Unsigned_8 := 16#02#;
   --  SEND_EOI_IR3 : constant Unsigned_8 := 16#03#;
   --  SEND_EOI_IR4 : constant Unsigned_8 := 16#04#;
   --  SEND_EOI_IR5 : constant Unsigned_8 := 16#05#;
   --  SEND_EOI_IR6 : constant Unsigned_8 := 16#06#;
   --  SEND_EOI_IR7 : constant Unsigned_8 := 16#07#;

   --      OCW3
   --
   --  OCW_TEMPLATE : constant Unsigned_8 := 16#08#;
   --  SPECIAL_MASK : constant Unsigned_8 := 16#40#;
   --  MASK_MDE_SET : constant Unsigned_8 := 16#20#;
   --  MASK_MDE_RST : constant Unsigned_8 := 16#00#;
   --  POLL_COMMAND : constant Unsigned_8 := 16#04#;
   --  NO_POLL_CMND : constant Unsigned_8 := 16#00#;
   --  READ_NEXT_RD : constant Unsigned_8 := 16#02#;
   --  READ_IR_ONRD : constant Unsigned_8 := 16#00#;
   --  READ_IS_ONRD : constant Unsigned_8 := 16#01#;

   --      Standard PIC initialization values for PCs.
   --
   PICM_ICW1 : constant Unsigned_8 :=
     ICW_TEMPLATE or EDGE_TRIGGER or ADDR_INTRVL8 or CASCADE_MODE or
     ICW4_NEEDED;
   PICM_ICW3 : constant Unsigned_8 := SLAVE_ON_IR2;
   PICM_ICW4 : constant Unsigned_8 := SNF_MODE_DIS or NONBUFD_MODE or
     NRML_EOI_MOD or I8086_EMM_MOD;
   PICS_ICW1 : constant Unsigned_8 := ICW_TEMPLATE or EDGE_TRIGGER or
     ADDR_INTRVL8 or CASCADE_MODE or ICW4_NEEDED;
   PICS_ICW3 : constant Unsigned_8 := I_AM_SLAVE_2;
   PICS_ICW4 : constant Unsigned_8 := SNF_MODE_DIS or NONBUFD_MODE or
     NRML_EOI_MOD or I8086_EMM_MOD;
   Last_IRQ_In_Master : constant := 7;
   --  IRQs from 0 to Last_IRQ_In_Master in Master PIC.

   procedure PICs_Init (Master_Base : in Unsigned_8;
                        Slave_Base : in Unsigned_8);
   pragma Inline (PICs_Init);
   --  Initialize both PICs
   procedure PIC_Master_Acknowledge_IRQ;
   pragma Inline (PIC_Master_Acknowledge_IRQ);
   procedure PIC_Slave_Acknowledge_IRQ;
   pragma Inline (PIC_Slave_Acknowledge_IRQ);
   --  Should be called once at initialization time to enable
   --  the PICs after having installed the interrupt handlers.

   procedure Enable_IRQ (IRQ_Num : in MaRTE.HAL.HW_Interrupt);
   pragma Inline (Enable_IRQ);
   --  Enables IRQs by changing the PICs IRQ mask register.
   procedure Disable_IRQ (IRQ_Num : in MaRTE.HAL.HW_Interrupt);
   pragma Inline (Enable_IRQ);
   --  Disables IRQs by changing the PICs IRQ mask register.

end MaRTE.HAL.PIC;
