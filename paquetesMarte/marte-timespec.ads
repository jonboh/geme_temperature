------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'T y p e _ T i m e s p e c'
--
--                                   Spec
--
--
--
--  File 'type_timespec.ads'                                           By MAR.
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
with MaRTE.HAL;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.Timespec is

   pragma Preelaborate;

   type Timespec is record
      Tv_Sec  : Int;
      Tv_Nsec : Int;
   end record;
   pragma Convention (C, Timespec);
   type Timespec_Ac is access all Timespec;
   type Timespec_Ac_Const is access constant Timespec;

   function HWTime_To_Timespec (Th : in MaRTE.HAL.HWTime)
                                return Timespec;
   pragma Inline (HWTime_To_Timespec);

   function Timespec_To_HWTime (TS : in Timespec)
                                return MaRTE.HAL.HWTime;
   pragma Inline (Timespec_To_HWTime);

   function To_Duration (TS : Timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return Timespec;
   pragma Inline (To_Timespec);

   procedure Initialize;

end MaRTE.Timespec;
