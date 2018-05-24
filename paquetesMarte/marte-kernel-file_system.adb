------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'K e r n e l . F i l e _ S y s t e m'
--
--                                    Body
--
--
--  File 'k-file_system.adb'                                    By Fguerreira.
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
--
------------------------------------------------------------------------------

with MaRTE.Kernel.File_System_Data_Types;   use MaRTE.Kernel.File_System_Data_Types;

with MaRTE.Kernel.Devices_Table;  use MaRTE.Kernel.Devices_Table;

with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.POSIX_Constants;
with Ada.Unchecked_Conversion;
with System;

package body MaRTE.Kernel.File_System is

   package GC renames MaRTE.POSIX_Constants;

   use type Int;

   -----------
   --  EOF  --
   -----------
   EOF : constant := -1;
   --  Any change in this value should be also done in 'stdio.h'.

   package TOI renames K.Tasks_Operations.Internals;

   ---------------------------------------------------------------------------
   --  File operations  -------------------------------------------------------
   ---------------------------------------------------------------------------

   ----------
   -- Open --
   ----------
   function Open
     (Path_Name : access Path;
      Mode : in File_Access_Mode)
     return Int is
     use type K.File_System_Data_Types.File_Access_Mode;
     -----------------------------------------
     --  Some string manipulation functions --
     -----------------------------------------
      function Index
         (Source  : String;
         Pattern : Character) return Natural is
      begin
         for I in Source'Range loop
            if Source (I) = Pattern then
               return I;
            end if;
         end loop;
         return 0;
      end Index;
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
      Flags : Integer;
      Internal_Procedure_Error : Int;
      The_Fd : File_Descriptor;
      The_Device_File_Number,
      The_Mount_Point_File_Number,
      The_Free_File_Number : Device_File_Number;
      File_Found,
      Mount_Point_Found,
      Fd_Found,
      Free_File_Found : Boolean := False;
      My_Path : Path := (others => ' ');
      My_Path_Length : Natural;
      Char_Pos : Natural;
   begin
      TOI.Reset_POSIX_Error;

      Char_Pos := Index (Path_Name.all, ASCII.Nul);
      if Char_Pos in 0 .. 1 then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      My_Path_Length := Char_Pos - 1;
      My_Path (1 .. My_Path_Length) := Path_Name (1 .. My_Path_Length);

      if (((Mode and File_Access_Mode (GC.FILE_MODE_MASK)) /= READ_ONLY) and
          ((Mode and File_Access_Mode (GC.FILE_MODE_MASK)) /= WRITE_ONLY) and
          ((Mode and File_Access_Mode (GC.FILE_MODE_MASK)) /= READ_WRITE)) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;

   ---------------------------------------------------------------------------
      K.Enter_Critic_Section (Flags);
   ---------------------------------------------------------------------------
      for I in Device_File_Number'Range loop
         if The_Device_Files_Table (I).Device_Used then
            case The_Device_Files_Table (I).Element_Type is
               when Device .. File =>
                  if My_Path (1 .. My_Path_Length) =
                        The_Device_Files_Table (I).File_Name
                                                      (1 .. My_Path_Length) then
                     File_Found := True;
                     The_Device_File_Number := I;
                     exit;
                  end if;
               when Mount_Point =>
                  Char_Pos := Index_Non_Blank_Backward (
                                 The_Device_Files_Table (I).File_Name);
                  if Char_Pos /= 0 and then My_Path (1 .. Char_Pos) =
                     The_Device_Files_Table (I).File_Name (1 .. Char_Pos) then
                     Mount_Point_Found := True;
                     The_Mount_Point_File_Number := I;
                     --  We do not exit here because we prefer a file entry
                     --  more than a mount point entry. Keep searching!!
                  end if;
            end case;
         else
               Free_File_Found := True;
               The_Free_File_Number := I;
         end if;
      end loop;

      if not File_Found and Mount_Point_Found then
         if Free_File_Found then
            The_Device_Files_Table (The_Free_File_Number).File_Name
               (1 .. My_Path_Length) := My_Path (1 .. My_Path_Length);
            The_Device_Files_Table (The_Free_File_Number).File_Name
               (My_Path_Length + 1 .. Path'Length) := (others => ' ');
            The_Device_Files_Table (The_Free_File_Number).Major_Number :=
              The_Device_Files_Table (The_Mount_Point_File_Number).Major_Number;
            The_Device_Files_Table (The_Free_File_Number).Minor_Number := 0;
            The_Device_Files_Table (The_Free_File_Number).Device_Used := True;
            The_Device_Files_Table (The_Free_File_Number).Element_Type := File;
            The_Device_File_Number := The_Free_File_Number;
         else
            TOI.Set_POSIX_Error_And_Leave_Critic_Section
               (TOO_MANY_OPEN_FILES, Flags);
            return -1;
         end if;
      elsif not File_Found and not Mount_Point_Found then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section
           (NO_SUCH_FILE_OR_DIRECTORY, Flags);
         return -1;
      end if;

      for I in File_Descriptor loop
         if not The_Fd_Table (I).Fd_Used and not Fd_Found then
            The_Fd_Table (I).File_Open_Status     :=
              K.File_System_Data_Types.File_Access_Mode (Mode);
            The_Fd_Table (I).Device_File_Assigned :=
              The_Device_File_Number;
            The_Fd_Table (I).Fd_Used := True;
            Fd_Found := True;
            The_Fd := I;
         end if;
      end loop;

      if not Fd_Found then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (TOO_MANY_OPEN_FILES,
                                                       Flags);
         return -1;
      end if;

   ---------------------------------------------------------------------------
      K.Leave_Critic_Section (Flags);
   ---------------------------------------------------------------------------

      if The_Driver_Table (The_Device_Files_Table
                           (The_Device_File_Number).Major_Number).Open /= null
      then
         Internal_Procedure_Error := The_Driver_Table (The_Device_Files_Table
                           (The_Device_File_Number).Major_Number).Open.all
           (The_Fd, K.File_System_Data_Types.File_Access_Mode (Mode));

         if Internal_Procedure_Error = -1 then
            ------------------------------------------------------------------
            K.Enter_Critic_Section (Flags);
            ------------------------------------------------------------------
            The_Fd_Table (The_Fd).Fd_Used := False;
            if not File_Found and Mount_Point_Found then
             The_Device_Files_Table (The_Free_File_Number).Device_Used := False;
            end if;
            ------------------------------------------------------------------
            K.Leave_Critic_Section (Flags);
            ------------------------------------------------------------------
            return -1;
         end if;
      end if;

      The_Device_Files_Table (The_Device_File_Number).Open_Count :=
         The_Device_Files_Table (The_Device_File_Number).Open_Count + 1;

      return Int (The_Fd);
   end Open;

   --  ***********************  --
   --  File_Descriptor_Correct  --
   --  ***********************  --

   function File_Descriptor_Correct (File_To_Check : in File_Descriptor)
                                    return Boolean is
   begin
      if ((Int (File_To_Check) < Int (File_Descriptor'First) or
           Int (File_To_Check) > Int (File_Descriptor'Last)) or else
          not The_Fd_Table (File_To_Check).Fd_Used) then
         TOI.Set_POSIX_Error (BAD_FILE_DESCRIPTOR);
         return False;
      end if;
      return True;
   end File_Descriptor_Correct;
   pragma Inline (File_Descriptor_Correct);

   -----------
   -- Close --
   -----------
   function Close (Fd : in File_Descriptor)
                  return Int is

      Flags : Integer;
      Internal_Procedure_Error : Int := 0;
      The_Device_File_Number : Device_File_Number;
      The_Major : Major;

   begin
      K.Enter_Critic_Section (Flags);

      TOI.Reset_POSIX_Error;

      if not File_Descriptor_Correct (Fd) then
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;

      The_Device_File_Number := The_Fd_Table (Fd).Device_File_Assigned;

      The_Major :=
        The_Device_Files_Table (The_Device_File_Number).Major_Number;

      K.Leave_Critic_Section (Flags);

      if The_Driver_Table (The_Major).Close /= null then
         Internal_Procedure_Error :=
           The_Driver_Table (The_Major).Close.all (Fd);
      end if;

      K.Enter_Critic_Section (Flags);
      The_Device_Files_Table (The_Device_File_Number).Open_Count :=
         The_Device_Files_Table (The_Device_File_Number).Open_Count - 1;
      if The_Device_Files_Table (The_Device_File_Number).Open_Count = 0 and
         The_Device_Files_Table (The_Device_File_Number).Delete_Flag = True then
         --  Delete the File
         if The_Driver_Table (The_Major).Delete /= null then
            Internal_Procedure_Error :=
              The_Driver_Table (The_Major).Delete.all (Fd);
           The_Device_Files_Table (The_Device_File_Number).Delete_Flag := False;
         end if;
      end if;

      --  Relases the Fd table slot
      The_Fd_Table (Fd).Fd_Used := False;
      K.Leave_Critic_Section (Flags);

      return Internal_Procedure_Error;

   end Close;

   ----------
   -- Read --
   ----------
   function Read
     (Fd            : in File_Descriptor;
      Buffer_Ptr    : in Buffer_Ac;
      Bytes_To_Read : in Buffer_Length)
     return Int is

      Flags      : Integer;
      Bytes_Read : Int := 0;
      The_Device_File_Number : Device_File_Number;
      The_Major : Major;
      function To_Buffer_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                             Buffer_Ac);

   begin

      K.Enter_Critic_Section (Flags);

      TOI.Reset_POSIX_Error;

      if not File_Descriptor_Correct (Fd) then
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;

      if (The_Fd_Table (Fd).File_Open_Status and READ_ONLY) = 0 then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section
           (PERMISSION_DENIED, Flags);
         return -1;
      elsif Bytes_To_Read = 0 then
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;

      The_Device_File_Number := The_Fd_Table (Fd).Device_File_Assigned;

      The_Major :=
        The_Device_Files_Table (The_Device_File_Number).Major_Number;

      K.Leave_Critic_Section (Flags);

      if The_Driver_Table (The_Major).Read /= null then
         if The_Fd_Table (Fd).Ungot_Char_Pending then
            --  There is a char ungot.
            Buffer_Ptr (Buffer_Ptr'First) := The_Fd_Table (Fd).Ungot_Char;
            The_Fd_Table (Fd).Ungot_Char_Pending := False;
            Bytes_Read :=
              The_Driver_Table (The_Major).Read.all
              (Fd,
               To_Buffer_Ac (Buffer_Ptr (Buffer_Ptr'First + 1)'Address),
               Bytes_To_Read - 1) + 1;
         else
            Bytes_Read :=
              The_Driver_Table (The_Major).Read.all (Fd,
                                                     Buffer_Ptr,
                                                     Bytes_To_Read);
         end if;
      end if;

      return Bytes_Read;
   end Read;

   -----------
   -- Write --
   -----------
   function Write
     (Fd             : in File_Descriptor;
      Buffer_Ptr     : in Buffer_Ac;
      Bytes_To_Write : in Buffer_Length)
     return Int is

      Flags         : Integer;
      Bytes_Written : Int := 0;
      The_Device_File_Number : Device_File_Number;
      The_Major : Major;

   begin

      K.Enter_Critic_Section (Flags);

      TOI.Reset_POSIX_Error;

      if not File_Descriptor_Correct (Fd) then
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;

      if (The_Fd_Table (Fd).File_Open_Status and WRITE_ONLY) = 0 then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section
           (PERMISSION_DENIED, Flags);
         return -1;
      elsif Bytes_To_Write = 0 then
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;

      The_Device_File_Number := The_Fd_Table (Fd).Device_File_Assigned;

      The_Major :=
        The_Device_Files_Table (The_Device_File_Number).Major_Number;

      K.Leave_Critic_Section (Flags);

      if The_Driver_Table (The_Major).Write /= null then
         Bytes_Written :=
           The_Driver_Table (The_Major).Write.all (Fd, Buffer_Ptr,
                                                   Bytes_To_Write);

         if Bytes_Written < 1 then
            TOI.Set_POSIX_Error (NOT_ENOUGH_SPACE);
         end if;
      end if;

      return Bytes_Written;
   end Write;

   ------------
   -- Ungetc --
   ------------
   function Ungetc (Ungot_Char : in Int;
                    Fd         : in File_Descriptor) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      TOI.Reset_POSIX_Error;

      if not File_Descriptor_Correct (Fd) then
         K.Leave_Critic_Section (Flags);
         return EOF;
      end if;

      K.Leave_Critic_Section (Flags);

      The_Fd_Table (Fd).Ungot_Char := Unsigned_8 (Ungot_Char);
      The_Fd_Table (Fd).Ungot_Char_Pending := True;

      return Ungot_Char;
   end Ungetc;

   -----------
   -- Ioctl --
   -----------
   function Ioctl
     (Fd             : in File_Descriptor;
      Request        : in Ioctl_Option_Value;
      Ioctl_Data_Ptr : in Buffer_Ac)
     return Int is

      Flags : Integer := 0;
      Internal_Procedure_Error : Int := 0;
      The_Device_File_Number : Device_File_Number;
      The_Major : Major;
   begin

      K.Enter_Critic_Section (Flags);

      TOI.Reset_POSIX_Error;

      if not File_Descriptor_Correct (Fd) then
         K.Leave_Critic_Section (Flags);
         return -1;
      end if;

      The_Device_File_Number :=
        The_Fd_Table (Fd).Device_File_Assigned;

      The_Major :=
        The_Device_Files_Table (The_Device_File_Number).Major_Number;

      K.Leave_Critic_Section (Flags);

      if The_Driver_Table (The_Major).Ioctl /= null then
         Internal_Procedure_Error := The_Driver_Table (The_Major).Ioctl.all
           (Fd, Request, Ioctl_Data_Ptr);
      end if;

      return Internal_Procedure_Error;
   end Ioctl;

   ------------
   -- Unlink --
   ------------
   function Unlink (Path_Name : access Path)
                  return Int
   is
      The_Fd : File_Descriptor;
      Ret : Int;
      The_Device_File_Number : Device_File_Number;
   begin
      --  a) Open the file
      The_Fd := File_Descriptor (Open (Path_Name, Read_Only));
      --  b) Put the Delete_Flag and decrement the open count
      The_Device_File_Number := The_Fd_Table (The_Fd).Device_File_Assigned;
      The_Device_Files_Table (The_Device_File_Number).Delete_Flag := True;
      --  c) Close the file (the file is deleted in the function CLOSE,
      --  if the Delete_Flag is True and the Open_Count gets to 0).
      Ret := Close (The_Fd);
      return Ret;
   end Unlink;

   -----------
   -- Lseek --
   -----------
   function Lseek
     (Fd             : in File_Descriptor;
      Offset         : in Off_t;
      Whence         : in Int)
     return Off_t
   is
      File_Number : Device_File_Number;
      Flags : Integer := 0;
      Int_Error : Int := 0;
      The_Major : Major;
   begin
      TOI.Reset_POSIX_Error;
      if Whence not in 0 .. 2 then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
   ---------------------------------------------------------------------------
      K.Enter_Critic_Section (Flags);
   ---------------------------------------------------------------------------
      if not File_Descriptor_Correct (Fd) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (BAD_FILE_DESCRIPTOR,
                                                       Flags);
         return -1;
      end if;
      File_Number := The_Fd_Table (Fd).Device_File_Assigned;
      The_Major := The_Device_Files_Table (File_Number).Major_Number;
   ---------------------------------------------------------------------------
      K.Leave_Critic_Section (Flags);
   ---------------------------------------------------------------------------
      if The_Driver_Table (The_Major).Lseek /= null then
         Int_Error := The_Driver_Table (The_Major).Lseek.all (Fd,Offset,Whence);
      end if;
      return Int_Error;
   end Lseek;

end MaRTE.Kernel.File_System;

