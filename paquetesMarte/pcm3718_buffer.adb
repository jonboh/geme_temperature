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
--  File 'pcm3718_buffer.adb'                               By Sangorrin
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
package body Pcm3718_Buffer is

   use type Pcm3718.Num_Conv_Type;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      Write_Index := Ai_Index'First;
      Read_Index  := Ai_Index'First;
      Number_In_Buffer := 0;
   end Flush;

   -----------
   -- Write --
   -----------

   procedure Write (
         Byte_Low  : in     Bit.Unsigned_8;
         Byte_High : in     Bit.Unsigned_8) is
   begin
      -- Overwrite if the buffer is full
      Fifo_Buffer(Write_Index + 1) := (Byte_Low,Byte_High);
      Write_Index := (Write_Index + 1) mod Pcm3718.Buffer_Mx;
      if Number_In_Buffer < Pcm3718.Buffer_Mx then
         Number_In_Buffer := Number_In_Buffer + 1;
      else
         Read_Index := (Read_Index + 1) mod Pcm3718.Buffer_Mx;
      end if;
   end Write;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status return Pcm3718.Num_Conv_Type is
   begin
      return Number_In_Buffer;
   end Get_Status;

   procedure Read (
         The_Data :    out Pcm3718.Analog_Data;
         Count    :    out Pcm3718.Num_Conv_Type) is

      function To_Data (
            Sample : in     Sample_T)
        return Pcm3718.Analog_Data_Type is
         use type Bit.Unsigned_16;
         Word : Bit.Unsigned_16;
         Data : Pcm3718.Analog_Data_Type;
      begin
         Word := Bit.Unsigned_16 (Sample.Byte_High);
         Word := Bit.Shift_Left (Word,8);
         Word := Word or Bit.Unsigned_16 (Sample.Byte_Low);
         Data.The_Channel := Pcm3718.Channel_Type(Word and 16#000F#);
         Data.The_Sample  := Pcm3718.Sample_Type(Bit.Shift_Right(Word,4));
         return Data;
      end To_Data;

   begin
      Count := 0;
      for I in The_Data'range loop
         The_Data(I) := To_Data(Fifo_Buffer(Read_Index+1));
         Read_Index := (Read_Index + 1) mod Pcm3718.Buffer_Mx;
         Count := Count + 1;
         Number_In_Buffer := Number_In_Buffer - 1;
         exit when Number_In_Buffer = 0;
      end loop;
   end Read;

end Pcm3718_Buffer;
