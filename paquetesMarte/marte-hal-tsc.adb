------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'T S C'
--
--                                   Body
--
--
--  File 'tsc.adb'                                                     By Mar.
--
--
--  Pentium Time Stamp Counter management.
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

with System.Machine_Code; use System.Machine_Code;
with MaRTE.X86_Processor_Type;

package body MaRTE.HAL.TSC is

   package X86_PROC renames MaRTE.X86_Processor_Type;
   use type X86_PROC.Processor_Type;

   --------------
   -- Read_TSC --
   --------------
   function Read_TSC return HWTime is
      Tmp : HWTime;
   begin
      pragma Assert (X86_PROC.Processor /= X86_PROC.I386);

      Asm (".byte 0x0f,0x31",
           HWTime'Asm_Output ("=A", Tmp),
           No_Input_Operands, "", True);
      return Tmp;
   end Read_TSC;

   -------------------
   -- Calibrate_TSC --
   -------------------
   procedure Calibrate_TSC;
   pragma Import (C, Calibrate_TSC, "calibrate_tsc");
   --  function defined in 'time_stamp_counter_c.c'.

   procedure Initialize is
   begin
      pragma Assert (X86_PROC.Processor /= X86_PROC.I386);

      Calibrate_TSC;
   end Initialize;

end MaRTE.HAL.TSC;
