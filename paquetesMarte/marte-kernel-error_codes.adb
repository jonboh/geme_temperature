------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . E r r o r _ C o d e s'
--
--                                   Body
--
--
--  File 'k-error_codes.adb'                                           By MAR.
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
with MaRTE.Error_Codes_Info; use MaRTE.Error_Codes_Info;

package body MaRTE.Kernel.Error_Codes is

   use type Int;

   -------------------
   -- Is_Error_Code --
   -------------------
   function Is_Error_Code (Error : Error_Code) return Boolean is
   begin
      for I in MaRTE.Error_Codes_Info.List'range loop
         if Error = MaRTE.Error_Codes_Info.List(I).Code then
            return True;
         end if;
      end loop;
      return False;
   end Is_Error_Code;

   -----------
   -- Image --
   -----------
   function Image (Error : Error_Code) return String is
   begin
      for I in MaRTE.Error_Codes_Info.List'range loop
         if Error = MaRTE.Error_Codes_Info.List(I).Code then
            return Error_Names'Image(I);
         end if;
      end loop;
      return "Not a valid error code";
   end Image;


end MaRTE.Kernel.Error_Codes;
