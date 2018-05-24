------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
-- 'K e r n e l . H a r d w a r e _ I n t e r r u p t s . O p e r a t i o n s'
--
--                                 Spec
--
--
--  File 'k-hardware_interrupts-operations.ads'                        By MAR.
--
--
--  Basic user hardware interrupts management operations. Based in a
--  POSIX draft for "Interrupt Control API" (P1003.2X/D1.0, February
--  2001).
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
with MaRTE.Timespec;

package MaRTE.Kernel.Hardware_Interrupts.Operations is

   package K renames MaRTE.Kernel;

   --------------------------
   -- POSIX_Intr_Associate --
   --------------------------
   function POSIX_Intr_Associate (Intr    : in Intr_T;
                                  Handler : in Interrupt_Handler_Function;
                                  Area    : in System.Address;
                                  Size    : in Size_T)
                                 return Int;
   pragma Export (C, POSIX_Intr_Associate, "posix_intr_associate");

   -----------------------------
   -- POSIX_Intr_Disassociate --
   -----------------------------
   function POSIX_Intr_Disassociate (Intr    : in Intr_T;
                                     Handler : in Interrupt_Handler_Function)
                                    return Int;
   pragma Export (C, POSIX_Intr_Disassociate, "posix_intr_disassociate");

   ---------------------
   -- POSIX_Intr_Lock --
   ---------------------
   function POSIX_Intr_Lock (Intr : in Intr_T) return Int;
   pragma Export (C, POSIX_Intr_Lock, "posix_intr_lock");

   -----------------------
   -- POSIX_Intr_Unlock --
   -----------------------
   function POSIX_Intr_Unlock (Intr : in Intr_T) return Int;
   pragma Export (C, POSIX_Intr_Unlock, "posix_intr_unlock");

   --------------------------
   -- POSIX_Intr_Timedwait --
   --------------------------
   function POSIX_Intr_Timedwait_HWTime
     (OS_Flags     : in     Int; --  Not used
      Abs_Timeout  : in     HAL.HWTime;
      Abs_Timespec : in     MaRTE.Timespec.Timespec_Ac_Const;
      Intr         : access Intr_T;
      Handler      : access Interrupt_Handler_Function)
     return Int;
   --  'Abs_Timespec' is only used to

end MaRTE.Kernel.Hardware_Interrupts.Operations;
