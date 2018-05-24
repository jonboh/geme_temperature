------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'M a R T E _ H a r d w a r e _ I n t e r r u p t s'
--
--                                 Spec
--
--
--  File 'marte_hardware_interrupts.ads'                               By MAR.
--
--
--  Direct mapping of hardware interrupts internal functions defined
--  inside MaRTE OS kernel.
--
--  It is mainly intended for Ada drivers that need to use
--  interrupts. Using this interface (instead
--  'POSIX_Hardware_Interrupts') leads to a smaller MaRTE footprint
--  since dependency with the POSIX-Ada binding is not included.
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
with MaRTE.HAL;
with MaRTE.Timespec;
with MaRTE.Kernel;
with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.Kernel.Hardware_Interrupts.Operations;
with MaRTE.POSIX_Interrupt_Control;

package MaRTE_Hardware_Interrupts is

   package K renames MaRTE.Kernel;

   -------------------
   -- Type 'Intr_T' --
   -------------------
   subtype Hardware_Interrupt is K.Intr_T;
   -- subtype of 'HWI.Hardware_Interrupt'

   TIMER_INTERRUPT       : Hardware_Interrupt renames
     MaRTE.HAL.TIMER_IRQ; -- Used by MaRTE OS
   KEYBOARD_INTERRUPT    : Hardware_Interrupt renames
     MaRTE.HAL.KEYBOARD_IRQ;
   CTLR2_INTERRUPT       : Hardware_Interrupt renames
     MaRTE.HAL.CTLR2_IRQ;
   SERIAL2_INTERRUPT     : Hardware_Interrupt renames
     MaRTE.HAL.SERIAL2_IRQ;
   SERIAL1_INTERRUPT     : Hardware_Interrupt renames
     MaRTE.HAL.SERIAL1_IRQ;
   PARALLEL2_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.PARALLEL2_IRQ;
   DISKETTE_INTERRUPT    : Hardware_Interrupt renames
     MaRTE.HAL.DISKETTE_IRQ;
   PARALLEL1_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.PARALLEL1_IRQ;
   RTC_INTERRUPT         : Hardware_Interrupt renames
     MaRTE.HAL.RTC_IRQ;
   SOFT_INTERRUPT        : Hardware_Interrupt renames
     MaRTE.HAL.SOFT_IRQ;
   RESERVED1_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED1_IRQ;
   RESERVED2_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED2_IRQ;
   RESERVED3_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED3_IRQ;
   COPROCESSOR_INTERRUPT : Hardware_Interrupt renames
     MaRTE.HAL.COPROCESSOR_IRQ;
   FIXED_DISK_INTERRUPT  : Hardware_Interrupt renames
     MaRTE.HAL.FIXED_DISK_IRQ;
   RESERVED4_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED4_IRQ;

   --------------------------------
   -- Type 'Handler_Return_Code' --
   --------------------------------
   subtype Handler_Return_Code is K.Handler_Return_Code;
   --  New 'Int' to avoid constraint error in case a handler returns
   --  something different of the possible return codes defined below
   POSIX_INTR_HANDLED_NOTIFY : Handler_Return_Code
     renames K.Hardware_Interrupts.POSIX_INTR_HANDLED_NOTIFY;
   POSIX_INTR_HANDLED_DO_NOT_NOTIFY : Handler_Return_Code
     renames K.Hardware_Interrupts.POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
   POSIX_INTR_NOT_HANDLED : Handler_Return_Code
     renames K.Hardware_Interrupts.POSIX_INTR_NOT_HANDLED;

   ---------------------------------------
   -- Type 'Interrupt_Handler_Function' --
   ---------------------------------------
   subtype Interrupt_Handler_Function is K.Interrupt_Handler_Function;
   --  access function (Area : in System.Address; Intr : in Intr_T)
   --                   return Handler_Return_Code;

   ---------------
   -- Associate --
   ---------------
   function Associate (Intr     : in Hardware_Interrupt;
                       Handler  : in Interrupt_Handler_Function;
                       Area      : in System.Address;
                       Area_Size : in Size_T)
                      return Int
     renames K.Hardware_Interrupts.Operations.POSIX_Intr_Associate;

   ------------------
   -- Disassociate --
   ------------------
   function Disassociate (Intr    : in Hardware_Interrupt;
                          Handler : in Interrupt_Handler_Function)
                         return Int
     renames K.Hardware_Interrupts.Operations.POSIX_Intr_Disassociate;

   ----------
   -- Lock --
   ----------
   function Lock (Intr : in Hardware_Interrupt) return Int
     renames K.Hardware_Interrupts.Operations.POSIX_Intr_Lock;

   ------------
   -- Unlock --
   ------------
   function Unlock (Intr : in Hardware_Interrupt) return Int
     renames K.Hardware_Interrupts.Operations.POSIX_Intr_Unlock;

   ---------------
   -- Timedwait --
   ---------------
   function Timedwait
     (Flags       : in     Int;
      Abs_Timeout : in     MaRTE.Timespec.Timespec_Ac_Const;
      Intr        : access Hardware_Interrupt;
      Handler     : access Interrupt_Handler_Function)
     return Int
     renames MaRTE.POSIX_Interrupt_Control.Posix_Intr_Timedwait;

end MaRTE_Hardware_Interrupts;
