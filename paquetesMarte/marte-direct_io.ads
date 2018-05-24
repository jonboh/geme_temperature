------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'K e r n e l _ C o n s o l e'
--
--                                   Spec
--
--
--  File 'kernel_console.ads'                                         By MAR.
--
--  This is the x86 architecture version of this package.
--
--  Basic output functions used to write on standard output and
--  standard error from inside the kernel and drivers. Some of these
--  functions can be used to write on standard output and standard
--  error at the very beginning of execution, before the file system
--  has been properly setup.
--
--  This package contains several functions that must properly been
--  set when there has been a change in standard input, output or
--  error devices defined in 'Kernel.Devices_Tables.The_Driver_Table':
--
--  - 'Stdout_Basic_Init' and 'Stderr_Basic_Init': these functions are
--    going to be called by MaRTE OS just when applications start
--    execution. They can be used to perform a basic initialization of
--    stdout and stderr devices. Their use allows displaying messages
--    from the very beginning of an execution.
--
--  - 'Stdout_End_Of_Kernel_Initialization': to be called by MaRTE OS
--    at the end of messages corresponding to kernel and devices
--    initialization. It can be used by stdout device to filter out
--    those messages or to display they in a different way (i.e. using
--    a different color).
--
--  - 'Stdout_Direct_Write' and 'Stderr_Direct_Write': basic functions
--    used to write on standard output and standard error at the very
--    beginning of execution, before the file system has been properly
--    setup. 'Direct_Write_On_Stderr' is also used by MaRTE kernel and
--    'printe' function to print error messages. Its use is more direct
--    and less error prone than using the standard 'write' function, so,
--    it is more likely to work after fatal errors.
--
--  - 'Stdin_Direct_Read': Basic function used to read a character
--    from standard input. It is used to wait a key in order to reboot
--    target computer after application finishes.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with System;

package MaRTE.Direct_IO is

   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Functions for Ada Code  ------------------------------------------------
   ----------------------------------------------------------------------------

   -------------------------------------------
   -- Direct write o standard error device --
   -------------------------------------------
   --
   --  Writes directly in standard error device.
   --
   --  If 'Fatal' is TRUE finishes the application after displaying
   --  the message.
   procedure Error (Msg : in String; Fatal : in Boolean := False);
   procedure Put_Error (Msg : in String; Fatal : in Boolean := False);

   --------------------------------------------
   -- Direct write on standard output device --
   --------------------------------------------
   --
   --  Most of them imported from 'basic_console_io_c.c'
   procedure Put (N : in Integer; Base : in Integer := 10);

   procedure Put (N : in Integer_8; Base : in Integer := 10);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_int8");

   procedure Put (N : in Integer_32; Base : in Integer := 10);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_int32");

   procedure Put (N : in Unsigned_8; Base : in Integer := 10);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_unsigned8");

   procedure Put (N : in Unsigned_32; Base : in Integer := 10);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_unsigned32");

   procedure Put (N : in Unsigned_64; Base : in Integer := 10);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_unsigned64");

   procedure Put (N : in System.Address; Base : in Integer := 16);
   --  pragma Import (C, Put, External_Name => "_marte_dio_put_unsigned32");

   pragma Import (C, Put);
   pragma Import_Function (Put, "_marte_dio_put_int32", (Integer, Integer));
   pragma Import_Function (Put, "_marte_dio_put_int8", (Integer_8, Integer));
   pragma Import_Function (Put, "_marte_dio_put_int32", (Integer_32, Integer));
   pragma Import_Function (Put, "_marte_dio_put_unsigned8",
                             (Unsigned_8, Integer));
   pragma Import_Function (Put, "_marte_dio_put_unsigned32",
                             (Unsigned_32, Integer));
   pragma Import_Function (Put, "_marte_dio_put_unsigned64",
                             (Unsigned_64, Integer));
   pragma Import_Function (Put, "_marte_dio_put_unsigned32",
                             (System.Address, Integer));

   procedure Put (Str : in String);

   procedure New_Line;
   pragma Import (C, New_Line, External_Name => "_marte_dio_newline");

   ----------------------------------------------------------------------------
   --  Basic Initialization  --------------------------------------------------
   ----------------------------------------------------------------------------

   ----------------------------------
   --  Basic_Stdout_Initialization --
   ----------------------------------
   procedure Basic_Stdout_Initialization;
   pragma Export (C, Basic_Stdout_Initialization,
                    "basic_stdout_initialization");

   ------------------------------------
   --  End_Of_Kernel_Initialization  --
   ------------------------------------
   procedure End_Of_Kernel_Initialization;

   ----------------------------------
   --  Basic_Stderr_Initialization --
   ----------------------------------
   procedure Basic_Stderr_Initialization;
   pragma Export (C, Basic_Stderr_Initialization,
                    "basic_stderr_initialization");

   ----------------------------------------------------------------------------
   --  Functions for C Code  --------------------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------
   --  Direct_Write_On_Stdout  --
   ------------------------------
   procedure Direct_Write_On_Stdout
     (Buffer_Ptr : in System.Address;
      Bytes      : in MaRTE.Integer_Types.Size_T);
   pragma Export (C, Direct_Write_On_Stdout, "direct_write_on_stdout");

   ------------------------------
   --  Direct_Write_On_Stderr  --
   ------------------------------
   procedure Direct_Write_On_Stderr
     (Buffer_Ptr : in System.Address;
      Bytes      : in MaRTE.Integer_Types.Size_T);
   pragma Export (C, Direct_Write_On_Stderr, "direct_write_on_stderr");

   ----------------------------
   -- Direct_Read_From_Stdin --
   ----------------------------
   function Direct_Read_From_Stdin return MaRTE.Integer_Types.Unsigned_8;
   pragma Export (C, Direct_Read_From_Stdin, "direct_read_from_stdin");

private

   --------------------
   --  Null funcions --
   --------------------
   function Null_Init return MaRTE.Integer_Types.Int;
   function Null_Write (Fd         : in MaRTE.Integer_Types.Int;
                        Buffer_Ptr : in System.Address;
                        Bytes      : in MaRTE.Integer_Types.Size_T)
                       return MaRTE.Integer_Types.Ssize_T;
   function Null_Read return MaRTE.Integer_Types.Unsigned_8;
   procedure Null_End_Of_Kernel_Initialization;

   ---------------------------------------------------------------------------
   --  Basic initialization of             -----------------------------------
   --  standard output and standard error  -----------------------------------
   ---------------------------------------------------------------------------
   --
   --  'Stdout_Basic_Init' and 'Stderr_Basic_Init' are called just
   --  when applications start execution. They can be used to perform
   --  a basic initialization of stdout and stderr devices. Their use
   --  allows displaying messages from the very beginning of an
   --  execution.
   --
   --  In most cases the 'create' function of driver could be used. In
   --  others cases a simpler function could be provided and a more
   --  complex initialization, that may require a full operating MaRTE
   --  environment, could be done in the 'create' of the driver.
   --
   --  'Stdout_End_Of_Kernel_Initialization'is called by MaRTE OS at
   --  the end of messages corresponding to kernel and devices
   --  initialization. It can be used by stdout device to filter out
   --  those messages or to display they in a different way
   --  (i.e. using a different color).
   --
   --  Examples:
   --
   --   - When console (the same for error) is a C driver:
   --     pragma Import (C, Stdout_Basic_Init, "console_create");
   --
   --   - When console is an Ada driver in package 'text_console.ad[sb]':
   --     pragma Import (C, Stdout_Basic_Init, "text_console__create");
   --
   --   - When you don't want to use it:
   --     function Stdout_Basic_Init return MaRTE.Integer_Types.Int
   --         renames Null_Init;

   function Stdout_Basic_Init return MaRTE.Integer_Types.Int;
   --  pragma Import (C, Stdout_Basic_Init, "text_console_basic_create");
   pragma Import (C, Stdout_Basic_Init, "console_switcher_create");

   procedure Stdout_End_Of_Kernel_Initialization;
   --  pragma Import (C, Stdout_End_Of_Kernel_Initialization,
   --                   "text_console_end_of_kernel_initialization");
   pragma Import (C, Stdout_End_Of_Kernel_Initialization,
                    "console_switcher_end_of_kernel_initialization");

   function Stderr_Basic_Init return MaRTE.Integer_Types.Int;
   --  pragma Import (C, Stderr_Basic_Init, "text_console_basic_create");
   pragma Import (C, Stderr_Basic_Init, "console_switcher_create");

   ---------------------------------------------------------------------------
   --  Direct write on                     -----------------------------------
   --  standard output and standard error  -----------------------------------
   ---------------------------------------------------------------------------
   --
   --  Basic functions used to write on standard output and standard
   --  error at the very beginning of execution, before the file
   --  system has been properly setup.
   --
   --  'Stderr_Direct_Write' is also used by MaRTE kernel to write
   --  error messages.
   --
   --  Kernel code will call the wrappers of these functions
   --  provided in the public part of this package:
   --  'Direct_Write_On_Stdout' and 'Direct_Write_On_Stderr'.
   --
   --  Examples:
   --
   --   - When console (the same for error) is a C driver:
   --     pragma Import (C, Stdout_Direct_Write, "text_console_write");
   --
   --   - When console is an Ada driver in package 'text_console.ad[sb]':
   --     pragma Import (C, Stdout_Direct_Write, "text_console__write");
   --
   --   - When you don't want to use it:
   --     function  Stdout_Direct_Write (...)
   --          return MaRTE.Integer_Types.Ssize_T renames Null_Write;
   --     (NO ERROR DISPLAYING IN CASE 'Stderr_Direct_Write' IS
   --      NOT PROVIDED !!).

   function Stdout_Direct_Write (Fd         : in MaRTE.Integer_Types.Int;
                                 Buffer_Ptr : in System.Address;
                                 Bytes      : in MaRTE.Integer_Types.Size_T)
                                return MaRTE.Integer_Types.Ssize_T;
   --  pragma Import (C, Stdout_Direct_Write, "text_console_write");
   pragma Import (C, Stdout_Direct_Write, "console_switcher_write");

   function Stderr_Direct_Write (Fd         : in MaRTE.Integer_Types.Int;
                                 Buffer_Ptr : in System.Address;
                                 Bytes      : in MaRTE.Integer_Types.Size_T)
                                return MaRTE.Integer_Types.Ssize_T;
   --  pragma Import (C, Stderr_Direct_Write, "text_console_write_error");
   pragma Import (C, Stderr_Direct_Write,
                    "console_switcher_write_error");

   ---------------------------------------------------------------------------
   --  Direct read from standard input  --------------------------------------
   ---------------------------------------------------------------------------
   --
   --  Basic function used to read a character from standar input. It
   --  is used to wait a key in order to reboot target computer after
   --  application finishes.
   --
   --  In case it is not provided (is set to 'Null_Read') reboot is
   --  immeditate.
   --
   --  IMPORTANT: if provided this function must NOT use interrupts.
   --
   --  Kernel code should call the wrapper of this function
   --  provided in the public part of this package:
   --  'Direct_Read_From_Stdin'.
   --
   --  Examples:
   --
   --   - When input is a C driver:
   --     pragma Import (C, Stdin_Direct_Read, "text_console_direct_read");
   --
   --   - When input is an Ada driver in package 'input.ad[sb]':
   --     pragma Import (C, Stdin_Direct_Read, "input__direct_read");
   --
   --   - When you don't want to use it:
   --     function  Stdin_Direct_Read return MaRTE.Integer_Types.Unsigned_8
   --          renames Null_Read;
   function Stdin_Direct_Read return MaRTE.Integer_Types.Unsigned_8;
   pragma Import (C, Stdin_Direct_Read, "keyboard_functions__direct_read");

end MaRTE.Direct_IO;
