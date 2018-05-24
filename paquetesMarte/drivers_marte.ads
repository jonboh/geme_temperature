------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                       'D r i v e r s _ M a R T E'
--
--                                   Spec
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

with MaRTE.Kernel;
with MaRTE.Kernel.File_System_Data_Types;
with MaRTE.Integer_Types;
with MaRTE.POSIX_Constants;

package Drivers_MaRTE is

   package K renames MaRTE.Kernel;

   package FSDT renames K.File_System_Data_Types;

   subtype Int is MaRTE.Integer_Types.Int;
   subtype Ssize_T is MaRTE.Integer_Types.Ssize_T;

   subtype File_Descriptor   is FSDT.File_Descriptor;
   subtype File_Access_Mode  is FSDT.File_Access_Mode;
   subtype Major             is FSDT.Major;
   subtype Minor             is FSDT.Minor;
   subtype Path              is FSDT.Path;
   subtype Buffer_Length     is FSDT.Buffer_Length;
   subtype Buffer_Ac         is FSDT.Buffer_Ac;
   subtype Off_t             is FSDT.Off_t;

   subtype Ioctl_Option_Value is FSDT.Ioctl_Option_Value;

   ----------------------------------------------------------------------------
   --  Drivers Functions Specifications ---------------------------------------
   ----------------------------------------------------------------------------
   subtype Create_Function_Ac is FSDT.Create_Function_Ac;
   --  function Create return Int;

   subtype Remove_Function_Ac is FSDT.Remove_Function_Ac;
   --  function Remove return Int;

   subtype Open_Function_Ac is FSDT.Open_Function_Ac;
   --  function Open (Fd   : in File_Descriptor;
   --                 Mode : in File_Access_Mode) return Int;

   subtype Close_Function_Ac is FSDT.Close_Function_Ac;
   --  function Close (Fd : in File_Descriptor) return Int;

   subtype Read_Function_Ac is FSDT.Read_Function_Ac;
   --  function Read (Fd         : in File_Descriptor;
   --                 Buffer_Ptr : in Buffer_Ac;
   --                 Bytes      : in Buffer_Length) return Ssize_T;

   subtype Write_Function_Ac is FSDT.Write_Function_Ac;
   --  function Write (Fd         : in File_Descriptor;
   --                  Buffer_Ptr : in Buffer_Ac;
   --                  Bytes      : in Buffer_Length) return Ssize_T;

   subtype Ioctl_Function_Ac is FSDT.Ioctl_Function_Ac;
   --  function Ioctl (Fd             : in File_Descriptor;
   --                  Request        : in Ioctl_Option_Value;
   --                  Ioctl_Data_Ptr : in Buffer_Ac) return Int;

   subtype Delete_Function_Ac is FSDT.Delete_Function_Ac;
   --  function Unlink (Fd : in File_Descriptor) return Int;

   subtype Lseek_Function_Ac is FSDT.Lseek_Function_Ac;
   --  function Lseek (Fd    : in File_Descriptor;
   --                 Offset : in Off_t;
   --                 Whence : in Int) return Off_t;

   ---------------------------------------------------------------------------
   --  Lseek constants for Whence parameter ----------------------------------
   ---------------------------------------------------------------------------
   SEEK_SET : constant Int := FSDT.SEEK_SET;
   SEEK_CUR : constant Int := FSDT.SEEK_CUR;
   SEEK_END : constant Int := FSDT.SEEK_END;
   ---------------------------------------------------------------------------
   --  File Access Modes  ----------------------------------------------------
   ---------------------------------------------------------------------------
   --
   --  To perform bit operations with the 'Mode' parameter of the
   --  'Open' function.
   READ_ONLY  : constant := MaRTE.POSIX_Constants.READ_ONLY;
   WRITE_ONLY : constant := MaRTE.POSIX_Constants.WRITE_ONLY;
   READ_WRITE : constant := MaRTE.POSIX_Constants.READ_WRITE;
   O_NONBLOCK : constant := MaRTE.POSIX_Constants.O_NONBLOCK;
   O_CREAT    : constant := MaRTE.POSIX_Constants.O_CREAT;

   ----------------------------------------------------------------------------
   --  Operations available to drivers ----------------------------------------
   ----------------------------------------------------------------------------

   --  Gets the major number associated to a file descriptor
   procedure Get_Major_Number (Fd         : in  File_Descriptor;
                               Mj         : out Major;
                               Invalid_Fd : out Boolean);
   --  C version 'get_major' (defined in the package body) returns -1
   --  for an invalid file descriptor

   --  Gets the minor number associated to a file descriptor
   procedure Get_Minor_Number (Fd         : in  File_Descriptor;
                               Mn         : out Minor;
                               Invalid_Fd : out Boolean);
   --  C version 'get_minor' (defined in the package body) returns -1
   --  for an invalid file descriptor

   --  Gets the Path associated to a file descriptor
   procedure Get_Path_Of_Fd (Fd         : in  File_Descriptor;
                             File_Name  : out Path;
                             Invalid_Fd : out Boolean);
   --  C version 'get_path' (defined in the package body) returns -1
   --  for an invalid file descriptor

   --  Sets POSIX error for a driver operation
   procedure Set_POSIX_Error (Error : in K.Error_Code);

   --  Gets current POSIX error
   function Get_POSIX_Error return K.Error_Code;

   --  Reset the current POSIX error
   procedure Reset_POSIX_Error;

   --  Gets specific data associated to a file descriptor. This
   --  specific data field (an integer) can be converted into an
   --  access type using an unchecked conversion.
   procedure Get_Specific_Data (Fd         : in  File_Descriptor;
                                Data       : out Integer;
                                Invalid_Fd : out Boolean);
   --  C version 'Driver_GetSpecific' (defined in the package body) returns -1
   --  for an invalid file descriptor

   --  Sets specific data associated to a file descriptor
   procedure Set_Specific_Data (Fd         : in  File_Descriptor;
                                Data       : in  Integer;
                                Invalid_Fd : out Boolean);
   --  C version 'Driver_SetSpecific' (defined in the package body) returns -1
   --  for an invalid file descriptor

   --  Gets file access mode for the file descriptor
   procedure Get_File_Access_Mode (Fd         : in  File_Descriptor;
                                   Oflags     : out File_Access_Mode;
                                   Invalid_Fd : out Boolean);
   --  C version 'Get_Fd_Access_Mode' (defined in the package body) returns -1
   --  for an invalid file descriptor

--     function "<" (A : in Buffer_Length; B : in Buffer_Length)
--                  return Boolean
--       renames FSDT."<";
--     function "<=" (A : in Buffer_Length; B : in Buffer_Length)
--                   return Boolean
--       renames FSDT."<=";
--     function ">" (A : in Buffer_Length; B : in Buffer_Length)
--                  return Boolean
--       renames FSDT.">";
--     function ">=" (A : in Buffer_Length; B : in Buffer_Length)
--                   return Boolean
--       renames FSDT.">=";
--     function "=" (A : in Buffer_Length; B : in Buffer_Length)
--                  return Boolean
--       renames FSDT."=";

end Drivers_MaRTE;
