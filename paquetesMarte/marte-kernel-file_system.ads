------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'K e r n e l . F i l e _ S y s t e m'
--
--                                 Spec
--
--
--  File 'k-file_system.ads'                                    By Fguerreira
--
--
--  File system I/O functions
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
------------------------------------------------------------------------------

with MaRTE.Integer_Types;
with MaRTE.Kernel.File_System_Data_Types;
use MaRTE.Kernel.File_System_Data_Types;

package MaRTE.Kernel.File_System is

   package K renames MaRTE.Kernel;

   ---------------------------------------------------------------------------
   --  File descriptors table  -----------------------------------------------
   ---------------------------------------------------------------------------
   --
   --  In this table the system mantains the file descriptors of
   --  opened device files.

   type Fd_Element is record
      File_Open_Status     : File_Access_Mode;
      Device_File_Assigned : Device_File_Number;
      Specific_Data        : Integer;
      Ungot_Char           : MaRTE.Integer_Types.Unsigned_8;
      Ungot_Char_Pending   : Boolean;
      Fd_Used              : Boolean;
   end record;

   The_Fd_Table : array (K.File_System_Data_Types.File_Descriptor)
     of Fd_Element :=
     (--  Standard Input
      0 => (File_Open_Status     => READ_ONLY,
            Device_File_Assigned => 1,
            Specific_Data        => 0,
            Ungot_Char           => MaRTE.Integer_Types.Unsigned_8'First,
            Ungot_Char_Pending   => False,
            Fd_Used              => True),
      --  Standard Output
      1 => (File_Open_Status     => WRITE_ONLY,
            Device_File_Assigned => 2,
            Specific_Data        => 0,
            Ungot_Char           => MaRTE.Integer_Types.Unsigned_8'First,
            Ungot_Char_Pending   => False,
            Fd_Used              => True),
      --  Standard Error
      2 => (File_Open_Status     => WRITE_ONLY,
            Device_File_Assigned => 3,
            Specific_Data        => 0,
            Ungot_Char           => MaRTE.Integer_Types.Unsigned_8'First,
            Ungot_Char_Pending   => False,
            Fd_Used              => True),
      --  Unused file descriptors
      others => (File_Open_Status     => READ_WRITE,
                 Device_File_Assigned => Device_File_Number'Last,
                 Specific_Data        => 0,
                 Ungot_Char           => MaRTE.Integer_Types.Unsigned_8'First,
                 Ungot_Char_Pending   => False,
                 Fd_Used              => False)
      );


   ----------
   -- Open --
   ----------
   function Open
      (Path_Name : access Path;
       Mode : in File_Access_Mode)
      return Int;
   pragma Export (C, Open, "open");

   --  ***********************  --
   --  File_Descriptor_Correct  --
   --  ***********************  --

   function File_Descriptor_Correct (File_To_Check : in File_Descriptor)
                                    return Boolean;

   -----------
   -- Close --
   -----------
   function Close (Fd : in File_Descriptor)
                  return Int;
   pragma Export (C, Close, "close");

   ----------
   -- Read --
   ----------
   function Read
      (Fd            : in File_Descriptor;
       Buffer_Ptr    : in Buffer_Ac;
       Bytes_To_Read : in Buffer_Length)
      return Int;
   pragma Export (C, Read, "read");

   -----------
   -- Write --
   -----------
   function Write
      (Fd             : in File_Descriptor;
       Buffer_Ptr     : in Buffer_Ac;
       Bytes_To_Write : in Buffer_Length)
     return Int;
   pragma Export (C, Write, "write");

   ------------
   -- Ungetc --
   ------------
   function Ungetc (Ungot_Char : in Int;
                    Fd         : in File_Descriptor) return Int;
   pragma Export (C, Ungetc, "ungetc_fd");

   -----------
   -- Ioctl --
   -----------
   function Ioctl
     (Fd             : in File_Descriptor;
      Request        : in Ioctl_Option_Value;
      Ioctl_Data_Ptr : in Buffer_Ac)
     return Int;
   pragma Export (C, Ioctl, "ioctl");

   ------------
   -- Unlink --
   ------------
   function Unlink (Path_Name : access Path)
                  return Int;
   pragma Export (C, Unlink, "unlink");

   -----------
   -- Lseek --
   -----------
   function Lseek
     (Fd             : in File_Descriptor;
      Offset         : in Off_t;
      Whence         : in Int)
     return Off_t;
   pragma Export (C, Lseek, "lseek");

end MaRTE.Kernel.File_System;

