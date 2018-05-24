------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.7     071213
--
--                         'O p t i o n _ S e t s'
--
--                                 Body
--
--
--  File 'option_sets.adb'                                             By MAR.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
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
with Ada.Unchecked_Conversion;

package body Option_Sets is

   use MaRTE.Integer_Types;

   ------------------
   --  To_Options  --
   ------------------
   function To_Bits is new Ada.Unchecked_Conversion (Unsigned_32, Bits);
   function To_Options (Num : Unsigned_32) return Option_Set is
   begin
      return Option_Set'(Option => To_Bits (Num));
   end To_Options;

   ----------------------
   -- To_Option_Number --
   ----------------------
   function To_Option_Number (Num : Integer)
                              return Option_Set is
   begin
      return Option_Set'(Option =>
                           To_Bits (Shift_Left (Unsigned_32'(2), Num)));
   end To_Option_Number;

   -----------------
   --  Empty_set  --
   -----------------
   function Empty_Set return Option_Set is
   begin
      return (Option => 0);
   end Empty_Set;

   -----------
   --  "+"  --
   -----------
   function "+" (L, R : Option_Set) return Option_Set is
   begin
      return (Option => Bits (Unsigned (L.Option) or Unsigned (R.Option)));
   end "+";

   -----------
   --  "-"  --
   -----------
   function "-" (L, R : Option_Set) return Option_Set is
   begin
      return (Option =>
                Bits (Unsigned (L.Option) and not (Unsigned (R.Option))));
   end "-";

   -----------
   --  "<"  --
   -----------
   function "<"  (Left, Right : Option_Set) return Boolean is
   begin
      return (Left <= Right) and (Left /= Right);
   end "<";

   -----------
   -- "<="  --
   -----------
   function "<=" (Left, Right : Option_Set) return Boolean is
   begin
      return (((not Bits (Unsigned (Right.Option)))) and
              Bits (Unsigned (Left.Option))) = 0;
   end "<=";

   -----------
   --  ">"  --
   -----------
   function ">"  (Left, Right : Option_Set) return Boolean is
   begin
      return Right < Left;
   end ">";

   ------------
   --  ">="  --
   ------------
   function ">=" (Left, Right : Option_Set) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Option_Sets;
