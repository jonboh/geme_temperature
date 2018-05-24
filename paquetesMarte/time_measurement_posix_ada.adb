------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'Time_Measurement_POSIX_Ada'
--
--                                 Body
--
--
--  File 'time_measurement_posix_ada.adb'               By Sangorrin
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

with Interfaces.C;
with Interfaces.C.Strings;

package body Time_Measurement_POSIX_Ada is

   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   use type C.Int;

   -------------------------------
   -- Time_Measure_POSIX_Create --
   -------------------------------

   procedure Time_Measure_POSIX_Create
                  (Name     : in  Time_Measure_Name;
                   Clock_ID : in  POSIX_Timers.Clock_Id;
                   ID       : out Time_Measure_Id)
   is
      function Time_Measure_POSIX_Create_C
                  (Name     : in  CS.chars_ptr;
                   Clock_ID : in  POSIX_Timers.Clock_Id;
                   ID       : access C.unsigned) return C.Int;

      pragma Import (C, Time_Measure_POSIX_Create_C,
                     "time_measure_posix_create");

      Name_C : CS.chars_ptr := CS.New_String (Name);
      ID_C   : aliased C.unsigned;
   begin
      if Time_Measure_POSIX_Create_C (Name_C, Clock_ID, ID_C'Access) /= 0 then
         CS.Free (Name_C);
         raise TIME_MEASUREMENT_POSIX_ERROR;
      end if;
      ID := Time_Measure_Id (ID_C);
      CS.Free (Name_C);
   end Time_Measure_POSIX_Create;

   ------------------------------
   -- Time_Measure_POSIX_Begin --
   ------------------------------

   procedure Time_Measure_POSIX_Begin (ID : in Time_Measure_Id)
   is
      function Time_Measure_POSIX_Begin_C (ID : in C.unsigned) return C.Int;
      pragma Import (C, Time_Measure_POSIX_Begin_C,
                     "time_measure_posix_begin");
   begin
      if Time_Measure_POSIX_Begin_C (C.unsigned (ID)) /= 0 then
         raise TIME_MEASUREMENT_POSIX_ERROR;
      end if;
   end Time_Measure_POSIX_Begin;

   ----------------------------
   -- Time_Measure_POSIX_End --
   ----------------------------

   procedure Time_Measure_POSIX_End (ID  : in Time_Measure_Id;
                                     Msg : in Time_Measure_Msg)
   is
      function Time_Measure_POSIX_End_C
                  (ID  : in C.unsigned;
                  Msg : in CS.chars_ptr) return C.Int;

      pragma Import (C, Time_Measure_POSIX_End_C, "time_measure_posix_end");
      Msg_C : CS.chars_ptr := CS.New_String (Msg);
   begin
      if Time_Measure_POSIX_End_C (C.unsigned (ID), Msg_C) /= 0 then
         CS.Free (Msg_C);
         raise TIME_MEASUREMENT_POSIX_ERROR;
      end if;
      CS.Free (Msg_C);
   end Time_Measure_POSIX_End;

end Time_Measurement_POSIX_Ada;
