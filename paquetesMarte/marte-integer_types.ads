------------------------------------------------------------------------------
--  --------------------         M a R T E   O S         ---------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'B a s i c _ I n t e g e r _ T y p e s'
--
--                                    Spec
--
--
--  File 'basic_integer_types.ads'                                    By MAR.
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
--MaRTE.Integer_Types
------------------------------------------------------------------------------
with Interfaces.C;

--pongo with MaRTE.HAL.IO;por--with Io_Interface;
--este paquete ha sido actualizado. AHora el fichero se llama marte-hal-io y está en marte/x86_arch/hwi

package MaRTE.Integer_Types is
   pragma Pure (MaRTE.Integer_Types);

   type Integer_8  is range -2 **  7 .. 2 **  7 - 1;
   for Integer_8'Size use  8;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   type Unsigned_8  is mod 2 **  8;
   for Unsigned_8'Size use  8;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   --  C types (they should be equal to the types defined in
   --  'Interfaces.C')
   subtype Int is Interfaces.C.int;
   subtype Short is Interfaces.C.short;
   subtype Long  is Interfaces.C.long;
   type Signed_Char is range  -2 **  7 .. 2 **  7 - 1;
   for Signed_Char'Size use 8;

   type Unsigned       is mod 2 ** Int'Size;
   for Unsigned'Size use Int'Size;
   type Unsigned_Short is mod 2 ** Short'Size;
   for Unsigned_Short'Size use Short'Size;
   type Unsigned_Long  is mod 2 ** Long'Size;
   for Unsigned_Long'Size use Long'Size; -- 32 bit

   type Unsigned_Char  is mod 2 ** Signed_Char'Size;
   for Unsigned_Char'Size use Signed_Char'Size;

   subtype Plain_Char is Unsigned_Char;

   subtype Size_T  is Interfaces.C.size_t;
   subtype Ssize_T is Int;

   --  Operations
   function Shift_Left
     (Value  : Unsigned_8;
      Amount : Natural)
     return    Unsigned_8;

   function Shift_Right
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Rotate_Left
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Rotate_Right
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Shift_Left
     (Value  : Unsigned_16;
      Amount : Natural)
     return    Unsigned_16;

   function Shift_Right
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Rotate_Left
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Rotate_Right
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Shift_Left
     (Value  : Unsigned_32;
      Amount : Natural)
     return    Unsigned_32;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Rotate_Left
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Rotate_Right
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Shift_Left
     (Value  : Unsigned_64;
      Amount : Natural)
     return    Unsigned_64;

   function Shift_Right
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Rotate_Left
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Rotate_Right
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

end MaRTE.Integer_Types;
