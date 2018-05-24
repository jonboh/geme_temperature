------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                            'P O S I X - I O'
--
--                                  Body
--
--
--  File 'posix-io.adb'                                        By Fguerreira
--
--
--  Package 'POSIX_IO' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTE OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  This package calls directly the operations defined in
--  'MaRTE.Kernel.File_System', but taking into account the necessary
--  treatment of the signals reserved for Gnat run time use.
--
--  This file is based on the Florist implementation
--  (http://www.cs.fsu.edu/~baker/florist.html)
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

with POSIX, POSIX.Implementation;

with Ada.Streams,
  System,
  MaRTE.Kernel.File_System,
  MaRTE.Kernel.File_System_Data_Types,
  MaRTE.Kernel.Devices_Table,
  MaRTE.Kernel.Tasks_Operations.Internals,
  Ada.Unchecked_Conversion,
  Ada.IO_Exceptions,
  MaRTE.Integer_Types;

package body POSIX.IO is

   package PI   renames POSIX.Implementation;
   package FSDT renames MaRTE.Kernel.File_System_Data_Types;
   package FS   renames MaRTE.Kernel.File_System;
   package TOI renames MaRTE.Kernel.Tasks_Operations.Internals;

   --  ----------------------------------------------
   --   Operations to open or close file descriptors
   --  ----------------------------------------------

   ------------
   --  Open  --
   ------------

   --  'Masked_Signals' is not useful
   function Open
     (Name           : Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := --  Empty_Set;
        Open_Option_Set (POSIX.Empty_Set);
      --  Conversion is only to work around a GNAT3.09 problem.
      Masked_Signals : Signal_Masking := RTS_Signals)
     return File_Descriptor is

      Fd : MaRTE.Integer_Types.Int;
      The_Path : aliased FSDT.Path;
   begin
      if (Name'Length + 1) > FSDT.Path'Length then
         PI.Raise_POSIX_Error (FILENAME_TOO_LONG);
      end if;
      for I in Name'Range loop
         The_Path (I) := Standard.Character (Name (I));
      end loop;
      The_Path (Name'Length + 1) := ASCII.NUL;

      Fd := MaRTE.Kernel.File_System.Open (The_Path'Access,
                                     FSDT.File_Access_Mode
                                     (File_Mode'Pos (Mode) + 1));
      PI.Raise_POSIX_Error_On_Error;

      return File_Descriptor (Fd);
   end Open;

   ----------------------
   --  Open_Or_Create  --
   ----------------------

   function Open_Or_Create
      (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      --  Permissions    : POSIX.Permissions.Permission_Set;
      Options        : Open_Option_Set := --  Empty_Set;
                              Open_Option_Set (POSIX.Empty_Set);
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
  is
     use type FSDT.File_Access_Mode;
     Fd : MaRTE.Integer_Types.Int;
     The_Path : aliased FSDT.Path;
   begin
      if (Name'Length + 1) > FSDT.Path'Length then
         PI.Raise_POSIX_Error (FILENAME_TOO_LONG);
      end if;
      for I in Name'Range loop
         The_Path (I) := Standard.Character (Name (I));
      end loop;
      The_Path (Name'Length + 1) := ASCII.NUL;

      Fd := MaRTE.Kernel.File_System.Open (The_Path'Access,
                                    FSDT.File_Access_Mode
                                    (File_Mode'Pos (Mode) + 1) Or FSDT.O_CREAT);
      PI.Raise_POSIX_Error_On_Error;

      return File_Descriptor (Fd);
   end Open_Or_Create;

   ---------------
   --  Is_Open  --
   ---------------

   function Is_Open (File : File_Descriptor) return Boolean is
   begin
      return MaRTE.Kernel.File_System.File_Descriptor_Correct
        (FSDT.File_Descriptor (File));
   end Is_Open;


   -------------
   --  Close  --
   -------------

   procedure Close
     (File           : in File_Descriptor;
      Masked_Signals : in Signal_Masking := RTS_Signals) is
      Tmp : MaRTE.Integer_Types.Int;  -- not used
   begin
      Tmp := MaRTE.Kernel.File_System.Close (FSDT.File_Descriptor (File));
      PI.Raise_POSIX_Error_On_Error;
   end Close;


   -----------------
   --  Duplicate  --
   -----------------

   function Duplicate (File   : File_Descriptor;
                       Target : File_Descriptor := 0) return File_Descriptor is
   begin
      for I in FSDT.File_Descriptor'Range loop
         if Integer (I) >= Integer (Target) then
            if not FS.The_Fd_Table (I).Fd_Used then
               FS.The_Fd_Table (I).File_Open_Status :=
                 FS.The_Fd_Table
                 (FSDT.File_Descriptor (File)).File_Open_Status;

               FS.The_Fd_Table (I).Device_File_Assigned :=
                 FS.The_Fd_Table
                 (FSDT.File_Descriptor (File)).Device_File_Assigned;

               FS.The_Fd_Table (I).Fd_Used :=
                 FS.The_Fd_Table (FSDT.File_Descriptor (File)).Fd_Used;

               return File_Descriptor (I);
            end if;
         end if;
      end loop;

      MaRTE.Kernel.Tasks_Operations.Internals.Set_POSIX_Error
                (Too_Many_Open_Files);
      PI.Raise_POSIX_Error_On_Error;

      return 0;
   end Duplicate;


   ---------------------------
   --  Duplicate_and_Close  --
   ---------------------------

   function Duplicate_and_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return File_Descriptor is
      --  'Masked_Signals' is not useful

   begin

      if MaRTE.Kernel.File_System.File_Descriptor_Correct
        (FSDT.File_Descriptor (File)) then
         PI.Raise_POSIX_Error_On_Error;
         if Target /= File then
            FS.The_Fd_Table (FSDT.File_Descriptor (Target)).File_Open_Status :=
              FS.The_Fd_Table (FSDT.File_Descriptor (File)).File_Open_Status;

            FS.The_Fd_Table
              (FSDT.File_Descriptor (Target)).Device_File_Assigned :=
              FS.The_Fd_Table
              (FSDT.File_Descriptor (File)).Device_File_Assigned;

            FS.The_Fd_Table (FSDT.File_Descriptor (Target)).Fd_Used :=
              FS.The_Fd_Table (FSDT.File_Descriptor (File)).Fd_Used;

         end if;
      end if;

      return Target;
   end Duplicate_and_Close;




   --  ------------------------------
   --   File Input/Output operations
   --  ------------------------------


   ------------
   --  Read  --
   ------------

   procedure Read
     (File           : in  File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : in Signal_Masking := RTS_Signals) is
      --  'Masked_Signals' is not useful

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, FSDT.Buffer_Ac);

      Bytes_Read : MaRTE.Integer_Types.Int;
      Buffer_Ptr : FSDT.Buffer_Ac;

      use type FSDT.Buffer_Length;
      use type MaRTE.Integer_Types.Int;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Buffer (1)'Address);
      Bytes_Read := MaRTE.Kernel.File_System.Read
        (FSDT.File_Descriptor (File), Buffer_Ptr,
         Buffer'Length * (Ada.Streams.Stream_Element'Size / 8));
      Last := Ada.Streams.Stream_Element_Offset
        (Bytes_Read / (Ada.Streams.Stream_Element'Size / 8));
      PI.Raise_POSIX_Error_On_Error;
   end Read;

   -------------
   --  Write  --
   -------------

   procedure Write
     (File           : in File_Descriptor;
      Buffer         : in Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : in Signal_Masking := RTS_Signals) is
      --  'Masked_Signals' is not useful

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, FSDT.Buffer_Ac);

      Bytes_Written : MaRTE.Integer_Types.Int;
      Buffer_Ptr : FSDT.Buffer_Ac;
      use type FSDT.Buffer_Length;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Buffer (1)'Address);
      Bytes_Written := MaRTE.Kernel.File_System.Write
        (FSDT.File_Descriptor (File), Buffer_Ptr,
         Buffer'Length * Ada.Streams.Stream_Element'Size / 8);
      Last := Ada.Streams.Stream_Element_Offset (Bytes_Written);
      PI.Raise_POSIX_Error_On_Error;
   end Write;


   --------------------
   --  Generic_Read  --
   --------------------

   procedure Generic_Read
     (File           : in File_Descriptor;
      Item           : out Data_Type;
      Masked_Signals : in Signal_Masking := RTS_Signals) is
      --  'Masked_Signals' is not useful

      function To_Buffer_Ac is
         new Ada.Unchecked_Conversion (System.Address, FSDT.Buffer_Ac);

      use type FSDT.Buffer_Length;
      use type MaRTE.Integer_Types.Int;

   begin
      if (MaRTE.Kernel.File_System.Read (FSDT.File_Descriptor (File),
                                   To_Buffer_Ac (Item'Address),
                                   Item'Size / 8)         < Item'Size / 8) then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      PI.Raise_POSIX_Error_On_Error;
   end Generic_Read;


   ---------------------
   --  Generic_Write  --
   ---------------------

   procedure Generic_Write
     (File           : in File_Descriptor;
      Item           : in Data_Type;
      Masked_Signals : in Signal_Masking := RTS_Signals) is
      --  'Masked_Signals' is not useful

      function To_Buffer_Ac is
         new Ada.Unchecked_Conversion (System.Address, FSDT.Buffer_Ac);

      use type FSDT.Buffer_Length;
      use type MaRTE.Integer_Types.Int;

   begin
      if (MaRTE.Kernel.File_System.Write (FSDT.File_Descriptor (File),
                                    To_Buffer_Ac (Item'Address),
                                    Item'Size / 8)       < Item'Size / 8) then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      PI.Raise_POSIX_Error_On_Error;
   end Generic_Write;


   ---------------------
   --  Generic_Ioctl  --
   ---------------------

   procedure Generic_Ioctl
     (File     : in     File_Descriptor;
      Request  : in     Ioctl_Options_Type;
      Data     : in out Ioctl_Data_Type) is

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, FSDT.Buffer_Ac);

      Tmp :MaRTE.Integer_Types.Int;
      Buffer_Ptr : FSDT.Buffer_Ac;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Data'Address);
      Tmp := MaRTE.Kernel.File_System.Ioctl (FSDT.File_Descriptor (File),
                                       FSDT.Ioctl_Option_Value
                                       (Ioctl_Options_Type'Enum_Rep (Request)),
                                       Buffer_Ptr);
      PI.Raise_POSIX_Error_On_Error;
   end Generic_Ioctl;

   ------------
   --  Seek  --
   ------------

   procedure Seek
     (File           : in File_Descriptor;
      Offset         : in IO_Offset;
      Result         : out IO_Offset;
      Starting_Point : in Position := From_Beginning) is
      Tmp : FSDT.Off_t;
   begin
      Tmp := MaRTE.Kernel.File_System.Lseek (FSDT.File_Descriptor (File),
                                       FSDT.Off_t (Offset),
                                       Position'Pos (Starting_Point));
      Result := IO_Offset (Tmp);
      PI.Raise_POSIX_Error_On_Error;
   end Seek;

end POSIX.IO;
