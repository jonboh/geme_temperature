------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                          'D i r e c t _ I O'
--
--                                   Body
--
--
--  File 'kernel_console.adb'                                         By MAR.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package body MaRTE.Direct_IO is

   ----------------------------------------------------------------------------
   --  Functions for Ada Code  ------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Exit_Process (Status : in MaRTE.Integer_Types.Int);
   pragma Import (C, Exit_Process, "exit");

   Str_Internal_Error : constant String :=
     Standard.ASCII.CR & "  MaRTE OS INTERNAL ERROR: ";
   Str_Fatal_Error : constant String := " (Fatal Error) ";

   -----------
   -- Error --
   -----------
   --
   --  MaRTE_Internal_Error
   procedure Error (Msg   : in String;
                    Fatal : in Boolean := False) is
   begin
      Put_Error (Str_Internal_Error);
      Put_Error (Msg, Fatal);
   end Error;

   ---------------
   -- Put_Error --
   ---------------
   procedure Put_Error (Msg   : in String;
                        Fatal : in Boolean := False) is
      procedure Exit_Process (Status : in MaRTE.Integer_Types.Int);
      pragma Import (C, Exit_Process, "exit");
   begin
      Direct_Write_On_Stderr (Msg'Address, Size_T (Msg'Length));
      if Fatal then
         Direct_Write_On_Stderr (Str_Fatal_Error'Address,
                                 Size_T (Str_Fatal_Error'Length));
         Exit_Process (1);
      end if;
   end Put_Error;

   ------------------
   -- Put (String) --
   ------------------
   procedure Put (Str : in String) is
   begin
      Direct_Write_On_Stdout (Str'Address, Size_T (Str'Length));
   end Put;

   ----------------------------------------------------------------------------
   --  Basic Initialization  --------------------------------------------------
   ----------------------------------------------------------------------------

   ----------------------------------
   --  Basic_Stdout_Initialization --
   ----------------------------------
   procedure Basic_Stdout_Initialization is
      Ret : MaRTE.Integer_Types.Int;
   begin
      Ret := Stdout_Basic_Init;
   end Basic_Stdout_Initialization;

   ------------------------------------
   --  End_Of_Kernel_Initialization  --
   ------------------------------------
   procedure End_Of_Kernel_Initialization is
   begin
      Stdout_End_Of_Kernel_Initialization;
   end End_Of_Kernel_Initialization;

   ----------------------------------
   --  Basic_Stderr_Initialization --
   ----------------------------------
   procedure Basic_Stderr_Initialization is
      Ret : MaRTE.Integer_Types.Int;
   begin
      Ret := Stderr_Basic_Init;
   end Basic_Stderr_Initialization;

   ----------------------------------------------------------------------------
   --  Functions for C Code  --------------------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------
   --  Direct_Write_On_Stdout  --
   ------------------------------
   procedure Direct_Write_On_Stdout (Buffer_Ptr : in System.Address;
                                     Bytes      : in Size_T) is
      Ret : MaRTE.Integer_Types.Ssize_T;
   begin
      Ret := Stdout_Direct_Write (1, Buffer_Ptr, Bytes);
   end Direct_Write_On_Stdout;

   ------------------------------
   --  Direct_Write_On_Stderr  --
   ------------------------------
   procedure Direct_Write_On_Stderr (Buffer_Ptr : in System.Address;
                                     Bytes      : in Size_T) is
      Ret : MaRTE.Integer_Types.Ssize_T;
   begin
      Ret := Stderr_Direct_Write (2, Buffer_Ptr, Bytes);
   end Direct_Write_On_Stderr;

   ----------------------------
   -- Direct_Read_From_Stdin --
   ----------------------------
   function Direct_Read_From_Stdin return MaRTE.Integer_Types.Unsigned_8 is
   begin
      return Stdin_Direct_Read;
   end Direct_Read_From_Stdin;

   ----------------------------------------------------------------------------
   --  Null funcions  ---------------------------------------------------------
   ----------------------------------------------------------------------------
   function Null_Init return MaRTE.Integer_Types.Int is
   begin
      return 0;
   end Null_Init;

   function Null_Write (Fd         : in MaRTE.Integer_Types.Int;
                        Buffer_Ptr : in System.Address;
                        Bytes      : in MaRTE.Integer_Types.Size_T)
                       return MaRTE.Integer_Types.Ssize_T is
   begin
      return 0;
   end Null_Write;

   function Null_Read return MaRTE.Integer_Types.Unsigned_8 is
   begin
      return 0;
   end Null_Read;

   procedure Null_End_Of_Kernel_Initialization is
   begin
      null;
   end Null_End_Of_Kernel_Initialization;

end MaRTE.Direct_IO;
