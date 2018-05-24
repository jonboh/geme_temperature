------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'D r i v e r s _ M a R T E'
--
--                                   Body
--
--
--  File 'drivers_marte.ads'                                     By Fguerreira
--
--
--  General types and functions that any driver may need
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

with MaRTE.Kernel.Devices_Table;  use MaRTE.Kernel.Devices_Table;
with MaRTE.Kernel.File_System;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body Drivers_MaRTE is

   --  package TOI renames Kernel.Tasks_Operations.Internals;

   use type Int;

   ----------------------------------------------------------------------------
   --  Get Major Number -------------------------------------------------------
   ----------------------------------------------------------------------------

   ---------------
   -- Get_Major --
   ---------------
   function Get_Major (Fd : in File_Descriptor) return Int;
   pragma Export (C, Get_Major, "get_major");
   pragma Inline (Get_Major);

   function Get_Major (Fd : in File_Descriptor) return Int is
      use K.File_System;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      else
         return Int
           (The_Device_Files_Table (The_Fd_Table (Fd).
                                    Device_File_Assigned).Major_Number);
      end if;
   end Get_Major;

   ----------------------
   -- Get_Major_Number --
   ----------------------
   procedure Get_Major_Number (Fd         : in  File_Descriptor;
                               Mj         : out Major;
                               Invalid_Fd : out Boolean) is
      Major_Number : Int;
   begin
      Major_Number := Get_Major (Fd);
      if Major_Number = -1 then
         Invalid_Fd := True;
      else
         Mj := Major (Major_Number);
         Invalid_Fd := False;
      end if;
   end Get_Major_Number;


   ----------------------------------------------------------------------------
   --  Get Minor Number -------------------------------------------------------
   ----------------------------------------------------------------------------

   ---------------
   -- Get_Minor --
   ---------------
   function Get_Minor (Fd : in File_Descriptor) return Int;
   pragma Export (C, Get_Minor, "get_minor");
   pragma Inline (Get_Minor);

   function Get_Minor (Fd : in File_Descriptor) return Int is
      use K.File_System;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      else
         return Int
           (The_Device_Files_Table (The_Fd_Table (Fd).
                                    Device_File_Assigned).Minor_Number);
      end if;
   end Get_Minor;


   ----------------------
   -- Get_Minor_Number --
   ----------------------
   procedure Get_Minor_Number (Fd         : in  File_Descriptor;
                               Mn         : out Minor;
                               Invalid_Fd : out Boolean) is
      Minor_Number : Int;
   begin
      Minor_Number := Get_Minor (Fd);
      if Minor_Number = -1 then
         Invalid_Fd := True;
      else
         Mn := Minor (Minor_Number);
         Invalid_Fd := False;
      end if;
   end Get_Minor_Number;

   --------------------------------------------------------------------
   --  Get Path -------------------------------------------------------
   --------------------------------------------------------------------

   --------------
   -- Get_Path --
   --------------
   function Get_Path (Fd        : in File_Descriptor;
                      File_Name : access Path) return Int;
   pragma Export (C, Get_Path, "get_path");
   pragma Inline (Get_Path);

   function Get_Path (Fd        : in File_Descriptor;
                      File_Name : access Path) return Int is
      use K.File_System;
      use type FSDT.Device_Element_Type;
      -----------------------------------------
      function Index_Non_Blank_Backward (Source : String)
         return Natural is
      begin
        for J in reverse Source'Range loop
           if Source (J) /= ' ' then
              return J;
           end if;
        end loop;
        return 0;
      end Index_Non_Blank_Backward;
     -----------------------------------------
      Tmp : Path := (others => ' ');
      Char_Pos : Natural;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      else
         File_Name.all := The_Device_Files_Table
           (The_Fd_Table (Fd).Device_File_Assigned).File_Name;
         for I in The_Device_Files_Table'Range loop
            if The_Device_Files_Table (I).Device_Used and then
               The_Device_Files_Table (I).Element_Type = FSDT.Mount_Point then
               Char_Pos := Index_Non_Blank_Backward (
                              The_Device_Files_Table (I).File_Name);
               if Char_Pos /= 0 and then File_Name (1 .. Char_Pos) =
                     The_Device_Files_Table (I).File_Name (1 .. Char_Pos) then
                  --  Remove the Mount_Point
                  Tmp (1 .. Path'Length - Char_Pos + 1) :=
                     File_Name (Char_Pos .. Path'Length);
                  File_Name.all := Tmp;
               end if;
            end if;
         end loop;
         return 0;
      end if;
   end Get_Path;

   --------------
   -- Get_Path --
   --------------
   procedure Get_Path_Of_Fd (Fd         : in  File_Descriptor;
                             File_Name  : out Path;
                             Invalid_Fd : out Boolean) is
      Tmp_File_Name : aliased Path;
      Ret : Int;
   begin
      Ret := Get_Path (Fd, Tmp_File_Name'access);
      if Ret = -1 then
         Invalid_Fd := True;
      else
         File_Name := Tmp_File_Name;
         Invalid_Fd := False;
      end if;
   end Get_Path_Of_Fd;

   --------------------------------------------------------------------------
   --  POSIX error  ---------------------------------------------------------
   --------------------------------------------------------------------------

   --  Sets POSIX error for a driver operation
   procedure Set_POSIX_Error (Error : in K.Error_Code) renames
     K.Tasks_Operations.Internals.Set_POSIX_Error;

   --  Gets current POSIX error
   function Get_POSIX_Error return K.Error_Code renames
     K.Tasks_Operations.Internals.Get_Last_POSIX_Error_Code;

   --  Reset the current POSIX error
   procedure Reset_POSIX_Error renames
     K.Tasks_Operations.Internals.Reset_POSIX_Error;

   --------------------------------------------------------------------------
   --  Get_Specific_Data  ---------------------------------------------------
   --------------------------------------------------------------------------
   function Driver_GetSpecific (Fd   : in File_Descriptor;
                                Data : access Integer) return Int;
   pragma Export (C, Driver_GetSpecific, "driver_getspecific");
   pragma Inline (Driver_GetSpecific);

   function Driver_GetSpecific (Fd   : in File_Descriptor;
                                Data : access Integer) return Int is
      use K.File_System;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      end if;
      Data.all := The_Fd_Table (Fd).Specific_Data;
      return 0;
   end Driver_GetSpecific;

   procedure Get_Specific_Data (Fd         : in  File_Descriptor;
                                Data       : out Integer;
                                Invalid_Fd : out Boolean) is
      Tmp : aliased Integer;
   begin
      if Driver_GetSpecific (Fd, Tmp'Access) = -1 then
         Invalid_Fd := True;
         Data := 0;
      else
         Invalid_Fd := False;
         Data := Tmp;
      end if;
   end Get_Specific_Data;

   --------------------------------------------------------------------------
   --  Set_Specific_Data  ---------------------------------------------------
   --------------------------------------------------------------------------
   function Driver_SetSpecific (Fd   : in File_Descriptor;
                                Data : in Integer) return Int;
   pragma Export (C, Driver_SetSpecific, "driver_setspecific");
   pragma Inline (Driver_SetSpecific);

   function Driver_SetSpecific (Fd   : in File_Descriptor;
                                Data : in Integer) return Int is
      use K.File_System;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      end if;
      The_Fd_Table (Fd).Specific_Data := Data;
      return 0;
   end Driver_SetSpecific;

   procedure Set_Specific_Data (Fd         : in  File_Descriptor;
                                Data       : in Integer;
                                Invalid_Fd : out Boolean) is
   begin
      Invalid_Fd := Driver_SetSpecific (Fd, Data) = -1;
   end Set_Specific_Data;

   --------------------------------------------------------------------------
   --  Get file access mode  ------------------------------------------------
   --------------------------------------------------------------------------
   function Get_Fd_Access_Mode (Fd : in File_Descriptor) return Int;
   pragma Export (C, Get_Fd_Access_Mode, "get_fd_access_mode");
   pragma Inline (Get_Fd_Access_Mode);

   function Get_Fd_Access_Mode (Fd : in File_Descriptor) return Int is
      use K.File_System;
   begin
      if ((Int (Fd) < Int (File_Descriptor'First) or
           Int (Fd) > Int (File_Descriptor'Last)) or else
          not K.File_System.The_Fd_Table (Fd).Fd_Used) then
         return -1;
      end if;
      return Int (K.File_System.The_Fd_Table (Fd).File_Open_Status);
   end Get_Fd_Access_Mode;

   procedure Get_File_Access_Mode (Fd         : in  File_Descriptor;
                                   Oflags     : out File_Access_Mode;
                                   Invalid_Fd : out Boolean) is
      Tmp : Int;
   begin
      Tmp := Get_Fd_Access_Mode (Fd);
      if Tmp = -1 then
         Invalid_Fd := True;
         Oflags := 0;
      else
         Invalid_Fd := False;
         Oflags := File_Access_Mode (Tmp);
      end if;
   end Get_File_Access_Mode;

end Drivers_MaRTE;

