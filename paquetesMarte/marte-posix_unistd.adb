------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'U n i s t d'
--
--                                   Body
--
--
--
--  File 'unistd.adb'                                                 By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'unistd.h'.
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

with MaRTE.Kernel.Tasks_Operations;
with MaRTE.Kernel.Tasks_Operations.Nanosleep;
with MaRTE.Timespec; use MaRTE.Timespec;
with MaRTE.POSIX_Constants;

package body MaRTE.POSIX_Unistd is

   package K renames MaRTE.Kernel;

   use type Int;

   package TO renames K.Tasks_Operations;

   -----------
   -- Sleep --
   -----------
   function Sleep (Seconds : Unsigned) return Unsigned is
      Interval  : aliased MaRTE.Timespec.Timespec := (Int (Seconds), 0);
      Remaining : aliased MaRTE.Timespec.Timespec;
   begin
      if TO.Nanosleep.Nanosleep (Interval'Unrestricted_Access,
                                 Remaining'Unrestricted_Access) = -1 then
         --  The operation has been interrupted by a signal
         return Unsigned (Remaining.Tv_Sec);
      end if;

      --  The operation hasn't been interrupted
      return 0;
   end Sleep;

   -----------
   -- Usleep --
   -----------
   function Usleep (Useconds : Unsigned_Long) return Int is
      Interval  : aliased MaRTE.Timespec.Timespec;
      Remaining : aliased MaRTE.Timespec.Timespec;
   begin
      if (Useconds >= 1_000_000) then
         return MaRTE.POSIX_Constants.INVALID_ARGUMENT;
      end if;

      if (Useconds = 0) then
         return 0;
      end if;

      Interval.Tv_Sec  := 0;
      Interval.Tv_Nsec := Int (useconds * 1000);

      if TO.Nanosleep.Nanosleep (Interval'Unrestricted_Access,
                                 Remaining'Unrestricted_Access) = -1 then
         return 0; --  The operation has been interrupted by a signal (ignore)
      end if;

      --  The operation hasn't been interrupted
      return 0;
   end Usleep;

end MaRTE.POSIX_Unistd;
