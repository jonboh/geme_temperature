------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'P O S I X'
--
--                                  Body
--
--
--  File 'posix.adb'                                                  By MAR.
--
--
--  Package 'POSIX' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTe OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  This file is based on the Florist implementation.
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
with Unchecked_Conversion;
with MaRTE.Kernel.Error_Codes;
with MaRTE.Kernel.Tasks_Operations.Internals;

with POSIX.Implementation; use POSIX.Implementation;

package body POSIX is

   package TOI  renames MaRTE.Kernel.Tasks_Operations.Internals;

   -----------------------------
   --  Unchecked Conversions  --
   -----------------------------

   type String_Ptr is access all String;
   type Wide_String_Ptr is access all Wide_String;
   type Stream_Element_Array_Ptr is
      access all Ada.Streams.Stream_Element_Array;

   function sptr_to_psptr is new Unchecked_Conversion
      (String_Ptr, POSIX_String_Ptr);
   function psptr_to_sptr is new Unchecked_Conversion
      (POSIX_String_Ptr, String_Ptr);
   function smelmptr_to_psptr is new Unchecked_Conversion
      (Stream_Element_Array_Ptr, POSIX_String_Ptr);
   function psptr_to_smelmptr is new Unchecked_Conversion
      (POSIX_String_Ptr, Stream_Element_Array_Ptr);

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String (Str : String)
      return POSIX_String is
   begin
      return sptr_to_psptr (Str'Unrestricted_Access).all;
   end To_POSIX_String;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Str : POSIX_String)
      return String is
   begin
      return psptr_to_sptr (Str'Unrestricted_Access).all;
   end To_String;

   ----------------------
   --  To_Wide_String  --
   ----------------------

   function To_Wide_String (Str : POSIX_String)
      return Wide_String is
      Result : Wide_String (Str'Range);
   begin
      for I in Str'Range loop
         Result (I) :=
           Wide_Character'Val (POSIX_Character'Pos (Str (I)));
      end loop;
      return Result;
   end To_Wide_String;

   --  We cannot use direct unchecked conversion here,
   --  since the sizes of the characters are different.
   --  However, we rely that the integer codes for the
   --  first 256 wide characters are the same as those
   --  of the ordinary characters. [See ARM A.1 (36)]

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String (Str : Wide_String)
      return POSIX_String is
      Result : POSIX_String (Str'Range);
   begin
      for I in Str'Range loop
         Result (I) := POSIX_Character'Val
           (Wide_Character'Pos (Str (I)) rem 256);
      end loop;
      return Result;
   end To_POSIX_String;

   -------------------------------
   --  To_Stream_Element_Array  --
   -------------------------------

   function To_Stream_Element_Array (Buffer : POSIX_String)
      return Ada.Streams.Stream_Element_Array is
   begin
      return psptr_to_smelmptr (Buffer'Unrestricted_Access).all;
   end To_Stream_Element_Array;

   --  This is only going to work if the sizes of
   --  Stream_Element and Character are the same.
   Assert_1 : constant := Boolean'Pos (Boolean'Pred
     (Ada.Streams.Stream_Element'Size = Character'Size));

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String
     (Buffer : Ada.Streams.Stream_Element_Array) return POSIX_String is
   begin
      return smelmptr_to_psptr (Buffer'Unrestricted_Access).all;
   end To_POSIX_String;

   NUL : constant POSIX_Character := POSIX_Character'Val (0);

   -------------------
   --  Is_Filename  --
   -------------------
   function Is_Filename (Str : POSIX_String) return Boolean is
   begin
      if To_String (Str)'Length = 0 then return False; end if;
      for I in Str'Range loop
         if Str (I) = '/' or Str (I) = NUL or Str (I) = ' ' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Filename;

   --  These two functions (Is_Pathname and Is_Filename) seem
   --  not to be unimplementable in a portable way, since they are
   --  supposed to "check all constraints set on filename and
   --  pathname by the implementation that can be checked without
   --  accessing the file system directly.

   -------------------
   --  Is_Pathname  --
   -------------------

   function Is_Pathname (Str : POSIX_String) return Boolean is
   begin
      if To_String (Str)'Length = 0 then return False; end if;
      for I in Str'Range loop
         if Str (I) = NUL or Str (I) = ' ' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Pathname;

   ----------------------------
   --  Is_Portable_filename  --
   ----------------------------

   function Is_Portable_Filename (Str : POSIX_String)
      return Boolean is
   begin
      if To_String (Str)'Length = 0 then return False; end if;
      for I in Str'Range loop
         case Str (I) is
            when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' =>
               null;
            when '-' =>
               if I = Str'First then return False; end if;
            when others =>
               return False;
         end case;
      end loop;
      return True;
   end Is_Portable_Filename;

   ----------------------------
   --  Is_Portable_Pathname  --
   ----------------------------

   function Is_Portable_Pathname (Str : POSIX_String)
      return Boolean is
      Start : Positive;
      P : Positive;
   begin
      if To_String (Str)'Length = 0 then return False; end if;
      Start := Str'First;
      P := Str'First;
      loop
         if P > Str'Last or else Str (P) = '/' then
            if Start < P and then not
               Is_Portable_Filename (Str (Start .. P - 1)) then
               return False;
            end if;
            if P > Str'Last then return True; end if;
            Start := P + 1;
         end if;
         P := P + 1;
      end loop;
   end Is_Portable_Pathname;



   -----------------
   --  Empty_set  --
   -----------------

   function Empty_Set return Option_Set renames Option_Sets.Empty_Set;

   -----------
   --  "+"  --
   -----------

   function "+" (L, R : Option_Set) return Option_Set renames Option_Sets."+";

   -----------
   --  "-"  --
   -----------

   function "-" (L, R : Option_Set) return Option_Set renames Option_Sets."-";

   ---------
   --  <  --
   ---------

   function "<"  (Left, Right : Option_Set) return Boolean
     renames Option_Sets."<";

   ---------
   --  <= --
   ---------

   function "<=" (Left, Right : Option_Set) return Boolean
     renames Option_Sets."<=";

   ---------
   --  >  --
   ---------

   function ">"  (Left, Right : Option_Set) return Boolean
     renames Option_Sets.">";

   ----------
   --  >=  --
   ----------

   function ">=" (Left, Right : Option_Set) return Boolean
     renames Option_Sets.">=";

   ----------------------
   --  Get_Error_Code  --
   ----------------------

   function Get_Error_Code return Error_Code is
   begin
      return TOI.Get_Last_POSIX_Error_Code;
   end Get_Error_Code;

   ----------------------
   --  Set_Error_Code  --
   ----------------------

   procedure Set_Error_Code (Error : in Error_Code) is
   begin
      TOI.Set_POSIX_Error (Error);
   end Set_Error_Code;

   ----------------------
   --  Is_POSIX_Error  --
   ----------------------

   function Is_POSIX_Error (Error : Error_Code) return Boolean
     renames MaRTE.Kernel.Error_Codes.Is_Error_Code;

   -------------
   --  Image  --
   -------------

   function Image (Error : Error_Code) return String
      renames MaRTE.Kernel.Error_Codes.Image;



   ------------------------
   --  Time Conversions  --
   ------------------------
   type D_Int is mod 2 ** (Duration'Size);
   function To_D_Int is new Unchecked_Conversion (Duration, D_Int);
   function To_Duration is new Unchecked_Conversion (D_Int, Duration);

   -----------------------------------------
   --  Timespec Composition/Decomposition --
   -----------------------------------------

   procedure Split (D  : in Duration;
                    S  : out Duration;
                    NS : out Duration);
   pragma Inline (Split);
   --  Decompose D into seconds (S) and nanoseconds (NS) parts,
   --  with the nanosecond part in the range 0.0 .. 0.999999999.
   NS_Per_S : constant := 10#1#E9;

   procedure Split (D  : in Duration;
                    S  : out Duration;
                    NS : out Duration) is
   begin
      S := To_Duration (To_D_Int (D / NS_Per_S) * NS_Per_S);
      NS := D - S;
      if NS < 0.0 then
         S := S - 1.0;
         NS := NS + 1.0;
      end if;
   end Split;

   -------------
   --  Split  --
   -------------

   procedure Split (Time : in Timespec;
                    S    : out Seconds;
                    NS   : out Nanoseconds) is
      SD, NSD : Duration;
   begin
      Split (Time, S => SD, NS => NSD);
      S := Seconds (SD); NS := Nanoseconds (NSD * NS_Per_S);
   end Split;
   pragma Inline (Split);

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec
     (S  : Seconds;
      NS : Nanoseconds) return Timespec is
   begin
      return Duration (S) + Duration (NS) / NS_Per_S;
   end To_Timespec;

   -------------------
   --  Get_Seconds  --
   -------------------

   function Get_Seconds (Time : Timespec) return Seconds is
      SD, NSD : Duration;
   begin
      Split (Time, S => SD, NS => NSD);
      return Seconds (SD);
   end Get_Seconds;

   -----------------------
   --  Get_Nanoseconds  --
   -----------------------

   function Get_Nanoseconds (Time : Timespec) return Nanoseconds is
      SD, NSD : Duration;
   begin
      Split (Time, S => SD, NS => NSD);
      return Nanoseconds (NSD * NS_Per_S);
   end Get_Nanoseconds;

   -----------------------
   --  Set_Nanoseconds  --
   -----------------------

   procedure Set_Nanoseconds
     (Time : in out Timespec;
      NS   : in Nanoseconds) is
      SD, NSD : Duration;
   begin
      Split (Time, S => SD, NS => NSD);
      Time := SD + Duration (NS) / NS_Per_S;
   end Set_Nanoseconds;

   -------------------
   --  Set_Seconds  --
   -------------------

   procedure Set_Seconds
     (Time : in out Timespec;
      S    : in Seconds) is
      SD, NSD : Duration;
   begin
      Split (Time, S => SD, NS => NSD);
      Time :=  Duration (S) + NSD / NS_Per_S;
   end Set_Seconds;

--     -----------
--     --  "+"  --
--     -----------

--     function "+" (Left, Right : Timespec) return Timespec is
--     begin
--        return Timespec (Duration (Left) + Duration (Right));
--     end "+";

   -----------
   --  "+"  --
   -----------

   function "+" (Left : Timespec; Right : Nanoseconds)
     return Timespec is
   begin
      return Left + Duration (Right) / NS_Per_S;
   end "+";

--     -----------
--     --  "-"  --
--     -----------

--     function "-" (Right : Timespec) return Timespec is
--     begin
--        return Timespec' (Val => -Right);
--     end "-";

--     -----------
--     --  "-"  --
--     -----------

--     function "-" (Left, Right : Timespec) return Timespec is
--     begin
--        return Timespec' (Val => Left - Right);
--     end "-";

   -----------
   --  "-"  --
   -----------

   function "-" (Left : Timespec; Right : Nanoseconds)
     return Timespec is
   begin
      return Left - Duration (Right) / NS_Per_S;
   end "-";

   -----------
   --  "*"  --
   -----------

   function "*" (Left : Timespec; Right : Integer)
     return Timespec is
   begin
      return Left * Duration (Right);
   end "*";

--     -----------
--     --  "*"  --
--     -----------

--     function "*" (Left : Integer; Right : Timespec)
--       return Timespec is
--     begin
--        return Timespec' (Val => Left * Right);
--     end "*";

--     -----------
--     --  "/"  --
--     -----------

--     function "/" (Left : Timespec; Right : Integer)
--       return Timespec is
--     begin
--        return Timespec' (Val => Left / Right);
--     end "/";

--     -----------
--     --  "/"  --
--     -----------

--     function "/" (Left, Right : Timespec) return Integer is
--     begin
--        return Integer (Left / Right);
--     end "/";

--     -----------
--     --  "<"  --
--     -----------

--     function "<" (Left, Right : Timespec) return Boolean is
--     begin
--        return Left < Right;
--     end "<";

   -----------
   --  "<="  --
   -----------

   function "<=" (Left, Right : Timespec) return Boolean is
   begin
      return Left < Right or else Right = Left;
   end "<=";

   -----------
   --  ">"  --
   -----------

   function ">" (Left, Right : Timespec) return Boolean is
   begin
      return Right <= Left;
   end ">";

   ------------
   --  ">="  --
   ------------

   function ">=" (Left, Right : Timespec) return Boolean is
   begin
      return Right < Left;
   end ">=";

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec (D : Duration)  return Timespec is
   begin
      return D;
   end To_Timespec;

   -------------------
   --  To_Duration  --
   -------------------

   function To_Duration (Time : Timespec)  return Duration is
   begin
      return Time;
   end To_Duration;

begin
   pragma Assert (Duration'Small = 1.0/NS_Per_S);
   --  We rely that Duration is an exact count of nanoseconds.
   null;
end POSIX;
