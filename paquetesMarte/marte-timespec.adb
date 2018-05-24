------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'T y p e _ T i m e s p e c'
--
--                                   Body
--
--
--
--  File 'type_timespec.adb'                                           By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over MaRTE OS.
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
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));

package body MaRTE.Timespec is

   use type HAL.HWTime;

   Initialized : Boolean := False;

   --  Clock frecuency. It is a logical constant that takes value in Initialize
   HWT_HZ : HAL.HWTime;

   ------------------------
   -- HWTime_To_Timespec --
   ------------------------
   type Ts is record
      Sec : Int;
      Nsec : Int;
   end record;
   function To_Ts is new Unchecked_Conversion (HAL.HWTime, Ts);
   function HWTime_To_Timespec (Th : in HAL.HWTime) return Timespec is
      S : HAL.HWTime;
   begin
      pragma Assert (Initialized and HWT_HZ /= 0);
      S := Th / HWT_HZ;
      return Timespec'(Tv_Sec  => To_Ts (S).Sec,
                       Tv_Nsec => To_Ts (((Th - S * HWT_HZ) * 1_000_000_000) /
                                      HWT_HZ).Sec);
   end HWTime_To_Timespec;
   pragma Inline (HWTime_To_Timespec);

   ------------------------
   -- Timespec_To_HWTime --
   ------------------------
   function Timespec_To_HWTime (TS : in Timespec) return HAL.HWTime is
      use type MaRTE.Integer_Types.Int;
   begin
      pragma Assert (Initialized and HWT_HZ /= 0);
      pragma Assert (TS.Tv_Nsec >= 0 and TS.Tv_Nsec < 1_000_000_000);
      return HAL.HWTime (TS.Tv_Sec) * HWT_HZ
        + (HAL.HWTime (TS.Tv_Nsec) * HWT_HZ) / 1_000_000_000;
   end Timespec_To_HWTime;
   pragma Inline (Timespec_To_HWTime);

   -----------------
   -- To_Duration --
   -----------------
   function To_Duration (TS : Timespec) return Duration is
      use type MaRTE.Integer_Types.Int;
   begin
      pragma Assert (TS.Tv_Nsec >= 0 and TS.Tv_Nsec < 1_000_000_000);
      return Duration (TS.Tv_Sec) + Duration (TS.Tv_Nsec) / 10#1#E9;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------
   function Dur_To_I64 is new Unchecked_Conversion (Duration, Integer_64);
   function To_Timespec (D : Duration) return Timespec is
      use type MaRTE.Integer_Types.Int;
   begin
      if D >= 0.0 then
         return Timespec'(Tv_Sec  => Int (Dur_To_I64 (D) / 1_000_000_000),
                          Tv_Nsec => Int (Dur_To_I64 (D) mod 1_000_000_000));
      else
         return Timespec'(Tv_Sec  => Int (Dur_To_I64 (D) / 1_000_000_000) - 1,
                          Tv_Nsec => Int (Dur_To_I64 (D) mod 1_000_000_000));
      end if;
   end To_Timespec;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      Initialized := True;

      HWT_HZ := HAL.Get_HWClock_Frequency;
      pragma Assert (HWT_HZ /= 0);
   end Initialize;

end MaRTE.Timespec;
