------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--       'K e r n e l . F i l e _ S y s t e m _ D a t a _ T y p e s'
--
--                                   Spec
--
--
--  File 'k-file_system_data_types.ads'                        By Fguerreira
--
--
--  Data types used in the I/O functions
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

with MaRTE.Configuration_Parameters;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.POSIX_Constants;

package MaRTE.Kernel.File_System_Data_Types is

   package K renames MaRTE.Kernel;

   use type Int;

   --  File Access Modes ("oflags")
   type File_Access_Mode is new MaRTE.Integer_Types.Unsigned_32;
   for File_Access_Mode'Size use Int'Size;

   READ_ONLY  : constant File_Access_Mode := MaRTE.POSIX_Constants.READ_ONLY;
   WRITE_ONLY : constant File_Access_Mode := MaRTE.POSIX_Constants.WRITE_ONLY;
   READ_WRITE : constant File_Access_Mode := MaRTE.POSIX_Constants.READ_WRITE;
   O_NONBLOCK : constant File_Access_Mode := MaRTE.POSIX_Constants.O_NONBLOCK;
   O_CREAT    : constant File_Access_Mode := MaRTE.POSIX_Constants.O_CREAT;

   --  Lseek constants for Whence parameter
   SEEK_SET : constant Int := MaRTE.POSIX_Constants.SEEK_SET;
   SEEK_CUR : constant Int := MaRTE.POSIX_Constants.SEEK_CUR;
   SEEK_END : constant Int := MaRTE.POSIX_Constants.SEEK_END;

   type File_Descriptor is new Int
     range 0 .. MaRTE.Configuration_Parameters.Open_Files_Mx - 1;
   for File_Descriptor'Size use Int'Size;

   type Major is new Int
     range 1 .. MaRTE.Configuration_Parameters.Devices_Mx;
   for Major'Size use Int'Size;

   type Minor is new Int
     range 0 .. MaRTE.Configuration_Parameters.Minor_Number_Mx;
   for Minor'Size use Int'Size;

   type Device_File_Number is new Int
     range 1 .. MaRTE.Configuration_Parameters.Devices_Files_Mx;

   subtype Path is String (1 .. MaRTE.Configuration_Parameters.Path_Mx);

   type Buffer_Length is new Size_T;

   type Buffer is array
     (1 .. Buffer_Length'Last) of Unsigned_8;
   pragma Pack (Buffer);

   type Buffer_Ac is access all Buffer;

   subtype Off_t is Int;

   type Ioctl_Option_Value is new Int;

   -------------
   -- Devices --
   -------------
   type Device_Element_Type is (Device, File, Mount_Point);

   type Device_Element is record
      File_Name          : Path;
      Major_Number       : Major;
      Minor_Number       : Minor;
      Device_Used        : Boolean := False;
      Element_Type       : Device_Element_Type;
      Delete_Flag        : Boolean := False;
      Open_Count         : Natural := 0;
   end record;

   ------------------
   -- Io_Functions --
   ------------------
   type Create_Function_Ac is access function
     return Int;

   type Remove_Function_Ac is access function
     return Int;

   type Open_Function_Ac is access function
     (Fd   : in File_Descriptor;
      Mode : in File_Access_Mode)
     return Int;

   type Close_Function_Ac is access function
     (Fd : in File_Descriptor)
     return Int;

   type Read_Function_Ac is access function
     (Fd         : in File_Descriptor;
      Buffer_Ptr : in Buffer_Ac;
      Bytes      : in Buffer_Length)
     return Ssize_T;

   type Write_Function_Ac is access function
     (Fd         : in File_Descriptor;
      Buffer_Ptr : in Buffer_Ac;
      Bytes      : in Buffer_Length)
     return Ssize_T;

   type Ioctl_Function_Ac is access function
     (Fd             : in File_Descriptor;
      Request        : in Ioctl_Option_Value;
      Ioctl_Data_Ptr : in Buffer_Ac)
     return Int;

   type Delete_Function_Ac is access function
     (Fd : in File_Descriptor)
     return Int;

   type Lseek_Function_Ac is access function
     (Fd     : in File_Descriptor;
      Offset : in Off_t;
      Whence : in Int)
     return Off_t;

   -------------
   -- Drivers --
   -------------
   Length_Driver_Name : constant := 16;
   type Driver_Element is record
      Name   : String (1 .. Length_Driver_Name);
      Create : Create_Function_Ac := null;
      Remove : Remove_Function_Ac := null;
      Open   : Open_Function_Ac   := null;
      Close  : Close_Function_Ac  := null;
      Read   : Read_Function_Ac   := null;
      Write  : Write_Function_Ac  := null;
      Ioctl  : Ioctl_Function_Ac  := null;
      Delete : Delete_Function_Ac := null;
      Lseek  : Lseek_Function_Ac  := null;
   end record;

   --------------------------
   --  Global estructures  --
   --------------------------

   type Device_Files_Table_Type is array
     (Device_File_Number) of Device_Element;
   type Driver_Table_Type is array
     (Major) of Driver_Element;

end MaRTE.Kernel.File_System_Data_Types;
