------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.7     071213
--
--                         'O p t i o n _ S e t s'
--
--                                 Spec
--
--
--  File 'option_sets.ads'                                             By MAR.
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
with MaRTE.Integer_Types;

package Option_Sets is

   type Option_Set is private;
   function To_Options (Num : MaRTE.Integer_Types.Unsigned_32)
                        return Option_Set;
   pragma Inline (To_Options);
   function To_Option_Number (Num : Integer) return Option_Set;
   pragma Inline (To_Option_Number);
   --  For 'Num' = 0 returns 'Option_1', for 'Num' = 1 returns 'Option_2'
   --  and so on.

   function Empty_Set return Option_Set;
   pragma Inline (Empty_Set);
   --  Returns an option set containing no options.

   function "+" (L, R : Option_Set) return Option_Set;
   pragma Inline ("+");
   --  Returns an option set containing exactly the set of options that are
   --  members of one or both of the two operand sets

   function "-" (L, R : Option_Set) return Option_Set;
   pragma Inline ("-");
   --  Returns an option set containing all the options in 'L' that are not
   --  members of 'R'.

   function "<" (Left, Right : Option_Set) return Boolean;
   pragma Inline ("<");
   --  'Left' is a proper subset of 'Right'

   function "<="(Left, Right : Option_Set) return Boolean;
   pragma Inline ("<=");
   --  'Left' is a subset of 'Right'

   function ">" (Left, Right : Option_Set) return Boolean;
   pragma Inline (">");
   --  'Right' is a proper subset of 'Left'

   function ">="(Left, Right : Option_Set) return Boolean;
   pragma Inline (">=");
   --  'Right' is a subset of 'Left'

   Empty_Option_Set : constant Option_Set;
   Full_Option_Set  : constant Option_Set;
   Option_1 :  constant Option_Set;
   Option_2 :  constant Option_Set;
   Option_3 :  constant Option_Set;
   Option_4 :  constant Option_Set;
   Option_5 :  constant Option_Set;
   Option_6 :  constant Option_Set;
   Option_7 :  constant Option_Set;
   Option_8 :  constant Option_Set;
   Option_9 :  constant Option_Set;
   Option_10 :  constant Option_Set;
   Option_11 :  constant Option_Set;
   Option_12 :  constant Option_Set;
   Option_13 :  constant Option_Set;
   Option_14 :  constant Option_Set;
   Option_15 :  constant Option_Set;
   Option_16 :  constant Option_Set;
   Option_17 :  constant Option_Set;
   Option_18 :  constant Option_Set;
   Option_19 :  constant Option_Set;
   Option_20 :  constant Option_Set;
   Option_21 :  constant Option_Set;
   Option_22 :  constant Option_Set;
   Option_23 :  constant Option_Set;
   Option_24 :  constant Option_Set;
   Option_25 :  constant Option_Set;
   Option_26 :  constant Option_Set;
   Option_27 :  constant Option_Set;
   Option_28 :  constant Option_Set;
   Option_29 :  constant Option_Set;
   Option_30 :  constant Option_Set;
   Option_31 :  constant Option_Set;

private

   type Bits is mod 2**32;
   for Bits'Size use MaRTE.Integer_Types.Int'Size;

   type Option_Set is record
      Option : Bits := 0;
   end record;
   for Option_Set'Size use MaRTE.Integer_Types.Int'Size;

   Empty_Option_Set : constant Option_Set := (Option => 0);
   Full_Option_Set  : constant Option_Set := (Option => not 0);
   Option_1   : constant Option_Set := (Option => 2**0);
   Option_2   : constant Option_Set := (Option => 2**1);
   Option_3   : constant Option_Set := (Option => 2**2);
   Option_4   : constant Option_Set := (Option => 2**3);
   Option_5   : constant Option_Set := (Option => 2**4);
   Option_6   : constant Option_Set := (Option => 2**5);
   Option_7   : constant Option_Set := (Option => 2**6);
   Option_8   : constant Option_Set := (Option => 2**7);
   Option_9   : constant Option_Set := (Option => 2**8);
   Option_10  : constant Option_Set := (Option => 2**9);
   Option_11  : constant Option_Set := (Option => 2**10);
   Option_12  : constant Option_Set := (Option => 2**11);
   Option_13  : constant Option_Set := (Option => 2**12);
   Option_14  : constant Option_Set := (Option => 2**13);
   Option_15  : constant Option_Set := (Option => 2**14);
   Option_16  : constant Option_Set := (Option => 2**15);
   Option_17  : constant Option_Set := (Option => 2**16);
   Option_18  : constant Option_Set := (Option => 2**17);
   Option_19  : constant Option_Set := (Option => 2**18);
   Option_20  : constant Option_Set := (Option => 2**19);
   Option_21  : constant Option_Set := (Option => 2**20);
   Option_22  : constant Option_Set := (Option => 2**21);
   Option_23  : constant Option_Set := (Option => 2**22);
   Option_24  : constant Option_Set := (Option => 2**23);
   Option_25  : constant Option_Set := (Option => 2**24);
   Option_26  : constant Option_Set := (Option => 2**25);
   Option_27  : constant Option_Set := (Option => 2**26);
   Option_28  : constant Option_Set := (Option => 2**27);
   Option_29  : constant Option_Set := (Option => 2**28);
   Option_30  : constant Option_Set := (Option => 2**29);
   Option_31  : constant Option_Set := (Option => 2**30);
end Option_Sets;
