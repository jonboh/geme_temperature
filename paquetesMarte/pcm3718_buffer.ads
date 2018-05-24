------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                            'pcm3718_buffer'
--
--                                   body
--
--
--  File 'pcm3718_buffer.add'                               By Sangorrin
--
--  This package is a Fifo Buffer State Machine used by pcm3718_functions
--  to provide a mean throughput between Write and Read operations in the
--  analog input scan mode.
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
with Pcm3718;
with MaRTE.Integer_Types;
package Pcm3718_Buffer is

   package Bit renames MaRTE.Integer_Types;

   -----------
   -- Flush --
   -----------

   procedure Flush;

   procedure Write (
         Byte_Low  : in     Bit.Unsigned_8;
         Byte_High : in     Bit.Unsigned_8);
   pragma Inline(Write);

   function Get_Status return Pcm3718.Num_Conv_Type;

   procedure Read (
         The_Data :    out Pcm3718.Analog_Data;
         Count    :    out Pcm3718.Num_Conv_Type);

private

   subtype Ai_Index is Integer range 0 .. Pcm3718.Buffer_Mx-1;

   Write_Index,
   Read_Index  : Ai_Index := Ai_Index'First;

   Number_In_Buffer : Pcm3718.Num_Conv_Type := 0;

   type Sample_T is
      record
         Byte_Low  : Bit.Unsigned_8;
         Byte_High : Bit.Unsigned_8;
      end record;

   Fifo_Buffer : array (Pcm3718.Analog_Data_Index'range) of Sample_T;

end Pcm3718_Buffer;
