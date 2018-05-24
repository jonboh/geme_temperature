------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                                 'R T C'
--
--                                   Spec
--
--
--  File 'rtc.ads'                                                     By Mar.
--
--
--  PC Real Time Clock management.
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

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.HAL.RTC is

   pragma Preelaborate;

   type RTC_Time is record
      Sec   : Unsigned_8; -- range [0,59]
      Min   : Unsigned_8; -- range [0,59]
      Hour  : Unsigned_8; -- range [0,23]
      Day   : Unsigned_8; -- range [1,31]
      Month : Unsigned_8; -- range [1,12]
      Year  : Unsigned_8; -- year two last digits (1999 => 99 and 2000 => 0)
   end record;
   pragma Convention (C, RTC_Time);

   function Get_RTC_Time return RTC_Time;

   function RTC_Time_To_Seconds_Since_Epoch return Int;
   pragma Import (C, RTC_Time_To_Seconds_Since_Epoch,
                  "rtc_time_to_seconds_since_epoch");
   --  defined in 'rtc.c'

   procedure Show_RTC_Time_On_Console;
   pragma Import (C, Show_RTC_Time_On_Console, "show_rtc_time_on_console");
   --  defined in 'rtc.c'

   procedure Show_RTC_Registers_On_Console;
   pragma Import (C, Show_RTC_Registers_On_Console,
                  "show_rtc_registers_on_console");
   --  defined in 'rtc.c'

end MaRTE.HAL.RTC;
