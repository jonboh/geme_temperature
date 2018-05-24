------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'P O S I X'
--
--                                  Spec
--
--
--  File 'posix.ads'                                                  By MAR.
--
--
--  Package 'POSIX' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTe OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  Based on the 'posix.ads' file of the FLORIST (FSU Implementation of
--  POSIX.5).
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
with Ada.Streams;

with MaRTE.Kernel;
with MaRTE.POSIX_Constants;
with MaRTE.Configuration_Parameters;
with MaRTE.Integer_Types;
with Option_Sets;

package POSIX is

   --  2.4.1 Constants and Static Subtypes

   --   Version Identification

   POSIX_Version : constant := 1995_00;
   POSIX_Ada_Version : constant := 1995_00;

   --  Optional Facilities (obsolescent, 0)
   --  See package POSIX.Limits for preferred interfaces.

   subtype Job_Control_Support is Boolean range
      False .. False;
   subtype Saved_IDs_Support is Boolean range
      False .. False;
   subtype Change_Owner_Restriction is Boolean range
      False .. False;
   subtype Filename_Truncation is Boolean range
     False .. False;

   --  Bytes and I/O Counts

   Byte_Size : constant :=  8;

   type IO_Count is range -2**31 .. (2**31)-1;

   for IO_Count'Size use 32;
   subtype IO_Count_Maxima is IO_Count range
     32767 .. IO_Count'Last;

   --  System Limits (obsolescent)
   --  See package POSIX.Limits for preferred interfaces.

   Portable_Groups_Maximum :
     constant Natural := 0;
   subtype Groups_Maxima is Natural range
     1 .. Natural'Last;

   Portable_Argument_List_Maximum :
      constant Natural := 4096;
   subtype Argument_List_Maxima is Natural range
      131072 .. 131072;
   Portable_Child_Processes_Maximum :
      constant Natural := 6;
   subtype Child_Processes_Maxima is Natural range
      999 .. 999;
   Portable_Open_Files_Maximum :
      constant Natural := 16;
   subtype Open_Files_Maxima is Natural range
     MaRTE.Configuration_Parameters.Open_Files_Mx ..
     MaRTE.Configuration_Parameters.Open_Files_Mx;
   Portable_Stream_Maximum :
     constant Natural := 8;
   subtype Stream_Maxima is Natural range
     8 .. Natural'Last;
   Portable_Time_Zone_String_Maximum :
     constant Natural := 3;
   subtype Time_Zone_String_Maxima is Natural range
     3 .. Natural'Last;
   --  Pathname Variable Values (obsolescent)
   --  See package POSIX.Limits for preferred interfaces.

   Portable_Link_Limit_Maximum :
     constant Natural := 8;
   subtype Link_Limit_Maxima is Natural range
     127 .. 127;
   Portable_Input_Line_Limit_Maximum :
     constant IO_Count := 255;
   subtype Input_Line_Limit_Maxima is IO_Count range
     255 .. 255;
   Portable_Input_Queue_Limit_Maximum :
     constant IO_Count := 255;
   subtype Input_Queue_Limit_Maxima is IO_Count range
     255 .. 255;
   Portable_Filename_Limit_Maximum :
     constant Natural := 14;
   subtype Filename_Limit_Maxima is Natural range
     14 .. 14;
   Portable_Pathname_Limit_Maximum :
     constant Natural := 255;
   subtype Pathname_Limit_Maxima is Natural range
     MaRTE.Configuration_Parameters.Path_Mx .. MaRTE.Configuration_Parameters.Path_Mx;
   Portable_Pipe_Limit_Maximum :
     constant IO_Count := 512;
   subtype Pipe_Limit_Maxima is IO_Count range
     4096 .. IO_Count'Last;

   --  Blocking Behavior Values
   type Blocking_Behavior is (Tasks, Program, Special);
   subtype Text_IO_Blocking_Behavior is Blocking_Behavior
     range Tasks .. Tasks;
   IO_Blocking_Behavior               : constant Blocking_Behavior := Tasks;
   File_Lock_Blocking_Behavior        : constant Blocking_Behavior := Tasks;
   Wait_For_Child_Blocking_Behavior   : constant Blocking_Behavior := Tasks;
   subtype Realtime_Blocking_Behavior is Blocking_Behavior
     range Tasks .. Program;

   --  Signal Masking
   type Signal_Masking is (No_Signals, RTS_Signals, All_Signals);

   --  Characters and Strings
   type POSIX_Character is new Standard.Character;
   --  We rely here on the fact that the GNAT type Character
   --  is the same as the GCC type char in C,
   --  which in turn must be the same as POSIX_Character.
   type POSIX_String is array (Positive range <>) of aliased POSIX_Character;
   function To_POSIX_String (Str : String) return POSIX_String;
   function To_POSIX_String (Str : Wide_String) return POSIX_String;
   function To_String (Str : POSIX_String) return String;
   function To_Wide_String (Str : POSIX_String) return Wide_String;
   function To_Stream_Element_Array (Buffer : POSIX_String)
                                    return Ada.Streams.Stream_Element_Array;
   function To_POSIX_String (Buffer : Ada.Streams.Stream_Element_Array)
                            return POSIX_String;
   subtype Filename is POSIX_String;
   subtype Pathname is POSIX_String;
   function Is_Filename (Str : POSIX_String) return Boolean;
   function Is_Pathname (Str : POSIX_String) return Boolean;
   function Is_Portable_Filename (Str : POSIX_String) return Boolean;
   function Is_Portable_Pathname (Str : POSIX_String) return Boolean;

   --  String Lists (Not implemented yet)
--    type POSIX_String_List is limited private;
--    Empty_String_List : constant POSIX_String_List;
--    procedure Make_Empty (List : in out POSIX_String_List);
--    procedure Append (List   : in out POSIX_String_List;
--                      In_Str : in POSIX_String);
--    generic
--       with procedure Action
--         (Item : in POSIX_String;
--         Quit : in out Boolean);
--    procedure For_Every_Item (List : in POSIX_String_List);
--    function Length (List : POSIX_String_List) return Natural;
--    function Value
--      (List  : POSIX_String_List;
--       Index : Positive) return POSIX_String;

   --  option sets


   subtype Option_Set is Option_Sets.Option_Set;
   function Empty_Set return Option_Set;
   function "+" (L, R : Option_Set) return Option_Set;
   function "-" (L, R : Option_Set) return Option_Set;
   function "<" (Left, Right : Option_Set) return Boolean;
   function "<="(Left, Right : Option_Set) return Boolean;
   function ">" (Left, Right : Option_Set) return Boolean;
   function ">="(Left, Right : Option_Set) return Boolean;
   Option_1 :  constant Option_Set;
   Option_2 :  constant Option_Set;
   Option_3 :  constant Option_Set;
   Option_4 :  constant Option_Set;
   Option_5 :  constant Option_Set;
   Option_6 :  constant Option_Set;
   Option_7 :  constant Option_Set;
   Option_8 :  constant Option_Set;
   Option_9 :  constant Option_Set;
   Option_10 :  constant Option_Set;
   Option_11 :  constant Option_Set;
   Option_12 :  constant Option_Set;
   Option_13 :  constant Option_Set;
   Option_14 :  constant Option_Set;
   Option_15 :  constant Option_Set;
   Option_16 :  constant Option_Set;
   Option_17 :  constant Option_Set;
   Option_18 :  constant Option_Set;
   Option_19 :  constant Option_Set;
   Option_20 :  constant Option_Set;
   Option_21 :  constant Option_Set;
   Option_22 :  constant Option_Set;
   Option_23 :  constant Option_Set;
   Option_24 :  constant Option_Set;
   Option_25 :  constant Option_Set;
   Option_26 :  constant Option_Set;
   Option_27 :  constant Option_Set;
   Option_28 :  constant Option_Set;
   Option_29 :  constant Option_Set;
   Option_30 :  constant Option_Set;
   Option_31 :  constant Option_Set;

   --  Exceptions and error codes
   POSIX_Error : exception;
   subtype Error_Code is MaRTE.Kernel.Error_Code;
   function Get_Error_Code return Error_Code;
   procedure Set_Error_Code (Error : in Error_Code);
   function Is_POSIX_Error (Error : Error_Code) return Boolean;
   function Image (Error : Error_Code) return String;

   No_Error : constant := MaRTE.Kernel.NO_ERROR;
   --  Error code constants with negative values correspond to
   --  error codes that are not supported by the current system.
   --  error codes
   E2BIG,
   Argument_List_Too_Long : constant := -1;
   EACCES,
   Permission_Denied : constant := MaRTE.Kernel.PERMISSION_DENIED;
   EADDRINUSE,
   Address_In_Use : constant := -1;
   EAGAIN,
   Resource_Temporarily_Unavailable : constant  :=
                                      MaRTE.Kernel.RESOURCE_TEMPORARILY_UNAVAILABLE;
   EBADF,
   Bad_File_Descriptor : constant := MaRTE.Kernel.BAD_FILE_DESCRIPTOR;
   EBADMSG,
   Bad_Message : constant := -1;
   EBUSY,
   Resource_Busy : constant := MaRTE.Kernel.RESOURCE_BUSY;
   ECANCELED,
   Operation_Canceled : constant := -1;
   ECHILD,
   No_Child_Process : constant := -1;
   EDEADLK,
   Resource_Deadlock_Avoided : constant := MaRTE.Kernel.RESOURCE_DEADLOCK_AVOIDED;
   EEXIST,
   File_Exists : constant := -1;
   EFAULT,
   Bad_Address : constant := -1;
   EFBIG,
   File_Too_Large : constant := -1;
   EINPROGRESS,
   Operation_In_Progress : constant := -1;
   EINTR,
   Interrupted_Operation : constant := MaRTE.Kernel.INTERRUPTED_OPERATION;
   EINVAL,
   Invalid_Argument : constant := MaRTE.Kernel.INVALID_ARGUMENT;
   EIO,
   Input_Output_Error : constant := -1;
   EISDIR,
   Is_A_Directory : constant := -1;
   EMFILE,
   Too_Many_Open_Files : constant := MaRTE.Kernel.TOO_MANY_OPEN_FILES;
   EMLINK,
   Too_Many_Links : constant := -1;
   EMSGSIZE,
   Message_Too_Long : constant := -1;
   ENAMETOOLONG,
   Filename_Too_Long : constant := MaRTE.Kernel.FILENAME_TOO_LONG;
   ENFILE,
   Too_Many_Open_Files_In_System : constant := -1;
   ENODEV,
   No_Such_Operation_On_Device : constant := -1;
   ENOENT,
   No_Such_File_Or_Directory : constant := MaRTE.Kernel.NO_SUCH_FILE_OR_DIRECTORY;
   ENOEXEC,
   Exec_Format_Error : constant := -1;
   ENOLCK,
   No_Locks_Available : constant := -1;
   ENOMEM,
   Not_Enough_Space : constant := -1;
   ENOSPC,
   No_Space_Left_On_Device : constant := MaRTE.Kernel.NOT_ENOUGH_SPACE;
   ENOTSUP,
   Operation_Not_Supported : constant := MaRTE.Kernel.OPERATION_NOT_SUPPORTED;
   ENOTDIR,
   Not_A_Directory : constant := -1;
   ENOTEMPTY,
   Directory_Not_Empty : constant := -1;
   ENOSYS,
   Operation_Not_Implemented : constant := MaRTE.Kernel.OPERATION_NOT_IMPLEMENTED;
   ENOTTY,
   Inappropriate_IO_Control_Operation : constant := -1;
   ENXIO,
   No_Such_Device_Or_Address : constant := -1;
   EPERM,
   Operation_Not_Permitted : constant := MaRTE.Kernel.OPERATION_NOT_PERMITTED;
   EPIPE,
   Broken_Pipe : constant := -1;
   ERANGE,
   TBD2 : constant Error_Code := MaRTE.POSIX_Constants.RESULT_TOO_LARGE;
   EROFS,
   Read_Only_File_System : constant := -1;
   ESPIPE,
   Invalid_Seek : constant := -1;
   ESRCH,
   No_Such_Process : constant Error_Code := MaRTE.Kernel.NO_SUCH_PROCESS;
   ETIMEDOUT,
   Timed_Out : constant Error_Code := MaRTE.Kernel.TIMED_OUT;
   EXDEV,
   Improper_Link : constant := -1;

   --  System Identification
   --  Not implemented yet
--   function System_Name return POSIX_String;
--   function Node_Name return POSIX_String;
--   function Release return POSIX_String;
--   function Version return POSIX_String;
--   function Machine return POSIX_String;

   --  Time Types
   type Seconds is new Integer;
   type Minutes is new Integer;
   type Nanoseconds_Base is new Integer;
   subtype Nanoseconds   is Nanoseconds_Base range 0 .. (10**9) - 1;
   subtype Timespec      is Duration;
   --  The value is of type Duration because we can do more
   --  efficient arithmetic on that type than on a two-part C struct.
   --  We rely that GNAT implements type Duration with enough
   --  precision (64 bits) to hold a full C timespec value.
   --  The enclosing record is to permit implicit initialization.

   function Get_Seconds (Time : Timespec) return Seconds;
   procedure Set_Seconds
     (Time : in out Timespec;
      S    : in Seconds);
   function Get_Nanoseconds (Time : Timespec) return Nanoseconds;
   procedure Set_Nanoseconds
     (Time : in out Timespec;
      NS   : in Nanoseconds);
   procedure Split
     (Time : in  Timespec;
      S    : out Seconds;
      NS   : out Nanoseconds);
   function To_Timespec
     (S  : Seconds;
      NS : Nanoseconds) return Timespec;
   --  function "+" (Left, Right : Timespec) return Timespec;
   function "+" (Left : Timespec; Right : Nanoseconds) return Timespec;
   --  function "-" (Right : Timespec) return Timespec;
   --  function "-" (Left, Right : Timespec) return Timespec;
   function "-" (Left : Timespec; Right : Nanoseconds) return Timespec;
   function "*" (Left : Timespec; Right : Integer) return Timespec;
   --  function "*" (Left : Integer; Right : Timespec) return Timespec;
   --  function "/" (Left : Timespec; Right : Integer) return Timespec;
   --  function "/" (Left, Right  : Timespec) return Integer;
   --  function "<" (Left, Right  : Timespec) return Boolean;
   function "<=" (Left, Right : Timespec) return Boolean;
   function ">" (Left, Right  : Timespec) return Boolean;
   function ">=" (Left, Right : Timespec) return Boolean;
   function To_Duration (Time : Timespec) return Duration;
   pragma Inline (To_Duration);
   function To_Timespec (D : Duration) return Timespec;
   pragma Inline (To_Timespec);


private
   Option_1   : constant Option_Set := Option_Set (Option_Sets.Option_1);
   Option_2   : constant Option_Set := Option_Set (Option_Sets.Option_2);
   Option_3   : constant Option_Set := Option_Set (Option_Sets.Option_3);
   Option_4   : constant Option_Set := Option_Set (Option_Sets.Option_4);
   Option_5   : constant Option_Set := Option_Set (Option_Sets.Option_5);
   Option_6   : constant Option_Set := Option_Set (Option_Sets.Option_6);
   Option_7   : constant Option_Set := Option_Set (Option_Sets.Option_7);
   Option_8   : constant Option_Set := Option_Set (Option_Sets.Option_8);
   Option_9   : constant Option_Set := Option_Set (Option_Sets.Option_9);
   Option_10  : constant Option_Set := Option_Set (Option_Sets.Option_10);
   Option_11  : constant Option_Set := Option_Set (Option_Sets.Option_11);
   Option_12  : constant Option_Set := Option_Set (Option_Sets.Option_12);
   Option_13  : constant Option_Set := Option_Set (Option_Sets.Option_13);
   Option_14  : constant Option_Set := Option_Set (Option_Sets.Option_14);
   Option_15  : constant Option_Set := Option_Set (Option_Sets.Option_15);
   Option_16  : constant Option_Set := Option_Set (Option_Sets.Option_16);
   Option_17  : constant Option_Set := Option_Set (Option_Sets.Option_17);
   Option_18  : constant Option_Set := Option_Set (Option_Sets.Option_18);
   Option_19  : constant Option_Set := Option_Set (Option_Sets.Option_19);
   Option_20  : constant Option_Set := Option_Set (Option_Sets.Option_20);
   Option_21  : constant Option_Set := Option_Set (Option_Sets.Option_21);
   Option_22  : constant Option_Set := Option_Set (Option_Sets.Option_22);
   Option_23  : constant Option_Set := Option_Set (Option_Sets.Option_23);
   Option_24  : constant Option_Set := Option_Set (Option_Sets.Option_24);
   Option_25  : constant Option_Set := Option_Set (Option_Sets.Option_25);
   Option_26  : constant Option_Set := Option_Set (Option_Sets.Option_26);
   Option_27  : constant Option_Set := Option_Set (Option_Sets.Option_27);
   Option_28  : constant Option_Set := Option_Set (Option_Sets.Option_28);
   Option_29  : constant Option_Set := Option_Set (Option_Sets.Option_29);
   Option_30  : constant Option_Set := Option_Set (Option_Sets.Option_30);
   Option_31  : constant Option_Set := Option_Set (Option_Sets.Option_31);
end POSIX;
