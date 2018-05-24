------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'P O S I X _ I n t e r r u p t _ C o n t r o l'
--
--                                 Spec
--
--
--  File 'posix_interrupt_control.ads'                                By MAR.
--
--
--  Part of the POSIX draft for interrupt control API not directly
--  implemented in package 'Kernel.Hardware_Interrupts.Operations'.
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
with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.Timespec;
with MaRTE.Integer_Types;

package MaRTE.POSIX_Interrupt_Control is

   package K renames MaRTE.Kernel;

   --------------------------
   -- Posix_Intr_Timedwait --
   --------------------------
   function Posix_Intr_Timedwait
     (Flags       : in     MaRTE.Integer_Types.Int;
      Abs_Timeout : in     MaRTE.Timespec.Timespec_Ac_Const;
      Intr        : access K.Hardware_Interrupts.Intr_T;
      Handler   : access K.Hardware_Interrupts.Interrupt_Handler_Function)
     return MaRTE.Integer_Types.Int;
   pragma Export (C, Posix_Intr_Timedwait, "posix_intr_timedwait");

end MaRTE.POSIX_Interrupt_Control;
