------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'Time_Measurement_POSIX_Ada'
--
--                                 Spec
--
--
--  File 'time_measurement_posix_ada.ads'               By Sangorrin
--
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
with POSIX_Timers;

package Time_Measurement_POSIX_Ada is

   ---------------
   -- Constants --
   ---------------

   MX_TIME_MEASURE_NAME : constant := 40; --  from time_measurement_posix.h
   MX_TIME_MEASURES     : constant := 20; --  from time_measurement_posix.h
   MX_TIME_MEASURE_MSG  : constant := 40; --  from time_measurement_posix.h

   TIME_MEASUREMENT_POSIX_ERROR : exception; --  all functions can raise it

   -----------
   -- Types --
   -----------

   type Time_Measure_Id      is range 0 .. MX_TIME_MEASURES - 1;
   subtype Time_Measure_Name is String;
   subtype Time_Measure_Msg  is String;

   -------------------------------
   -- Time_Measure_POSIX_Create --
   -------------------------------

   procedure Time_Measure_POSIX_Create
               (Name     : in  Time_Measure_Name;
                Clock_ID : in  POSIX_Timers.Clock_Id;
                ID       : out Time_Measure_Id);

   ------------------------------
   -- Time_Measure_POSIX_Begin --
   ------------------------------

   procedure Time_Measure_POSIX_Begin (ID : in Time_Measure_Id);

   ----------------------------
   -- Time_Measure_POSIX_End --
   ----------------------------

   procedure Time_Measure_POSIX_End (ID  : in Time_Measure_Id;
                                     Msg : in Time_Measure_Msg);

end Time_Measurement_POSIX_Ada;
