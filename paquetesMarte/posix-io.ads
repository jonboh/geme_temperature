-----------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                            'P O S I X - I O'
--
--                                  Spec
--
--
--  File 'posix-io.ads'                                        By Fguerreira
--
--
--  Package 'POSIX_IO' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTE OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  This package calls directly the operations defined in
--  'Kernel.File_System', but taking into account the necessary
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

with Ada.Streams,
     POSIX,
     MaRTE.Integer_Types;

use MaRTE.Integer_Types;

pragma Elaborate_All (POSIX);


package POSIX.IO is

   type File_Descriptor is
     range 0 .. POSIX.Open_Files_Maxima'Last - 1;
   for File_Descriptor'Size use Int'Size;

   Standard_Input  : constant File_Descriptor := 0;
   Standard_Output : constant File_Descriptor := 1;
   Standard_Error  : constant File_Descriptor := 2;

   type IO_Offset is new Integer_32;


   --  File Modes and Options

   type File_Mode is (Read_Only, Write_Only, Read_Write);
   type Open_Option_Set is new POSIX.Option_Set;

   --  Empty_Set, "+" and unary and binary "-" are derived operations
--   Non_Blocking             : constant Open_Option_Set;
--   Append                   : constant Open_Option_Set;
--   Truncate                 : constant Open_Option_Set;
--   Exclusive                : constant Open_Option_Set;
--   Not_Controlling_Terminal : constant Open_Option_Set;
--   File_Synchronized        : constant Open_Option_Set;
--   Data_Synchronized        : constant Open_Option_Set;
--   Read_Synchronized        : constant Open_Option_Set;


   --  Operations to open or close file descriptors

   function Open
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := --  Empty_Set;
         Open_Option_Set (POSIX.Empty_Set);
         --  Conversion is only to work around a GNAT3.09 problem.
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      --  'Masked_Signals' is not useful
      return File_Descriptor;

   function Open_Or_Create
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      --  Permissions    : POSIX.Permissions.Permission_Set;
      Options        : Open_Option_Set := --  Empty_Set;
        Open_Option_Set (POSIX.Empty_Set);
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return File_Descriptor;

   function Is_Open (File : File_Descriptor) return Boolean;

   procedure Close
     (File           : in File_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);
      --  'Masked_Signals' is not useful

   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
      return File_Descriptor;

   function Duplicate_and_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      --  'Masked_Signals' is not useful
      return File_Descriptor;

--   procedure Create_Pipe
--     (Read_End  : out File_Descriptor;
--      Write_End : out File_Descriptor);


   --  File Input/Output operations

--   ** obsolescent **
--   subtype IO_Buffer is POSIX.POSIX_String;

--   ** obsolescent **
--   procedure Read
--     (File           : in File_Descriptor;
--      Buffer         : out IO_Buffer;
--      Last           : out POSIX.IO_Count;
--      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);


   procedure Read
     (File           : in File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);
   --  'Masked_Signals' is not useful

   --   ** obsolescent **
   --   procedure Write
   --     (File           : in File_Descriptor;
   --      Buffer         : in IO_Buffer;
   --      Last           : out POSIX.IO_Count;
   --      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Write
     (File           : in File_Descriptor;
      Buffer         : in Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);
   --  'Masked_Signals' is not useful


   generic
      type Data_Type is private;
   procedure Generic_Read
     (File           : in File_Descriptor;
      Item           : out Data_Type;
      Masked_Signals : in POSIX.Signal_Masking
        := POSIX.RTS_Signals);
   --  'Masked_Signals' is not useful


   generic
      type Data_Type is private;
   procedure Generic_Write
     (File           : in File_Descriptor;
      Item           : in Data_Type;
      Masked_Signals : in POSIX.Signal_Masking
        := POSIX.RTS_Signals);
   --  'Masked_Signals' is not useful


   generic
      type Ioctl_Options_Type is (<>);
      type Ioctl_Data_Type is private;
   procedure Generic_Ioctl
     (File     : in     File_Descriptor;
      Request  : in     Ioctl_Options_Type;
      Data     : in out Ioctl_Data_Type);
   --  Not defined by the POSIX standard


   --  File position operations

   type Position is (From_Beginning, From_Current_Position, From_End_Of_File);

   procedure Seek
     (File           : in File_Descriptor;
      Offset         : in IO_Offset;
      Result         : out IO_Offset;
      Starting_Point : in Position := From_Beginning);

   --  function File_Size (File : File_Descriptor)
   --                    return POSIX.IO_Count;

   --  function File_Position (File : File_Descriptor)
   --                         return IO_Offset;


   --  Terminal operations

   --  function Is_A_Terminal (File : File_Descriptor)
   --                         return Boolean;

   --  function Get_Terminal_Name (File : File_Descriptor)
   --                             return POSIX.Pathname;


   --  File Control operations

   --  procedure Get_File_Control
   --    (File    : in File_Descriptor;
   --     Mode    : out File_Mode;
   --     Options : out Open_Option_Set);

   --  procedure Set_File_Control
   --    (File    : in File_Descriptor;
   --     Options : in Open_Option_Set);

   --  function Get_Close_On_Exec (File : File_Descriptor)
   --                             return Boolean;

   --  procedure Set_Close_On_Exec
   --    (File : in File_Descriptor;
   --     To   : in Boolean := True);

   --  procedure Change_Permissions
   --    (File :       in  POSIX.IO.File_Descriptor;
   --     Permission : in  POSIX.Permissions.Permission_Set);

   --  procedure Truncate_File
   --    (File :       in  POSIX.IO.File_Descriptor;
   --     Length :     in  POSIX.IO_Count);

   --  procedure Synchronize_File (File : in POSIX.IO.File_Descriptor);

   --  procedure Synchronize_Data (File : in POSIX.IO.File_Descriptor);


   --  private

   --  Non_Blocking             : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set'(Option => O_NONBLOCK));

   --  Append                   : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => O_APPEND));

   --  Truncate                 : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => O_TRUNC));

   --  Exclusive                : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => O_EXCL));

   --  Not_Controlling_Terminal : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => POSIX.C.O_NOCTTY));

   --  File_Synchronized        : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => POSIX.C.O_SYNC));

   --  Data_Synchronized        : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => POSIX.C.O_DSYNC));

   --  Read_Synchronized        : constant Open_Option_Set
   --   := Open_Option_Set (Option_Set' (Option => POSIX.C.O_RSYNC));


end POSIX.IO;
