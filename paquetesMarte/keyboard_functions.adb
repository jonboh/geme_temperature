------------------------------------------------------------------------------
----------------------        M a R T E     O S        -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'K e y b o a r d _ F u n c t i o n s'
--
--                                  Body
--
--
--  File 'keyboard_functions.adb'                                     By MAR.
--
--  Driver for the PC keyboard.
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
with System;
with Ada.Unchecked_Conversion;

with Keyboard;

with MaRTE.Integer_Types;
-- with Hardware_Interrupts;
with MaRTE.Direct_IO;
pragma Elaborate_All (MaRTE.Direct_IO);
with MaRTE_Hardware_Interrupts;
pragma Elaborate_All (MaRTE_Hardware_Interrupts);
with MaRTE_Semaphores;
pragma Elaborate_All (MaRTE_Semaphores);
with MaRTE.HAL;


package body Keyboard_Functions is

   package HW_INTS renames MaRTE_Hardware_Interrupts;
   use type MaRTE.Integer_Types.Int;

   ---------------------------------------------------------------------------
   -- General Constants ------------------------------------------------------
   ---------------------------------------------------------------------------
   Buffer_Size : constant := 128;
   --  Maximum number of characters in input buffer.

   Finishing_Key : constant := Character'Pos (ASCII.ESC);
   --  Application is immediately finished when this key is pressed.

   ---------------------------------------------------------------------------
   -- Synchronization Semaphore ----------------------------------------------
   ---------------------------------------------------------------------------
   Sem : aliased MaRTE_Semaphores.Semaphore;

   ---------------------------------------------------------------------------
   -- C Imported Functions ---------------------------------------------------
   ---------------------------------------------------------------------------
   --  XXX Not in Ada packages... yet

   function Initialize_Keyboard return Int;
   pragma Import (C, Initialize_Keyboard, "initialize_kdb");
   --  Imported from 'keyboard_c.c'.

   function Trygetchar return Int;
   pragma Import (C, Trygetchar, "keyboard_trygetchar");
   --  Imported from 'keyboard_c.c'.

   procedure Exit_Process (Status : in Int);
   pragma Import (C, Exit_Process, "exit");
   --  Imported from 'libmc/stdlib/exit.c'.


   ----------------------------------------------------------------------------
   -- Circular Buffers --------------------------------------------------------
   ----------------------------------------------------------------------------
   type Buffer_Index is mod Buffer_Size;
   type Buffer_Array is array (Buffer_Index) of Int;
   type Circular_Buffer is record
      Buffer : Buffer_Array;
      Head : Buffer_Index := 0;
      Tail : Buffer_Index := Buffer_Index'Last;
      Size : Integer range 0 .. Integer (Buffer_Index'Last) + 1;
   end record;

   procedure Take_From_Buffer (Buf : in out Circular_Buffer; C : out Int);
   procedure Take_From_Buffer (Buf : in out Circular_Buffer; C : out Int) is
   begin
      if Buf.Size > 0 then
         C := Buf.Buffer (Buf.Head);
         Buf.Head := Buf.Head + 1;
         Buf.Size := Buf.Size - 1;
      else
         C := -1;
      end if;
   end Take_From_Buffer;
   pragma Inline (Take_From_Buffer);

   procedure Store_In_Buffer (Buf : in out Circular_Buffer; C : in Int);
   procedure Store_In_Buffer (Buf : in out Circular_Buffer; C : in Int) is
   begin
      if Buf.Size < Integer (Buffer_Index'Last) + 1 then
         Buf.Tail := Buf.Tail + 1;
         Buf.Size := Buf.Size + 1;
         Buf.Buffer (Buf.Tail) := C;
      else
         --  If buffer is full the last character is replaced.
         Buf.Buffer (Buf.Tail) := C;
      end if;
   end Store_In_Buffer;
   pragma Inline (Store_In_Buffer);

   procedure Discard_Tail_Of_Buffer (Buf : in out Circular_Buffer);
   procedure Discard_Tail_Of_Buffer (Buf : in out Circular_Buffer) is
   begin
      if Buf.Size > 0 then
         Buf.Tail := Buf.Tail - 1;
         Buf.Size := Buf.Size - 1;
      end if;
   end Discard_Tail_Of_Buffer;
   pragma Inline (Discard_Tail_Of_Buffer);

   procedure Move_Between_Buffers (From : in out Circular_Buffer;
                                   To   : in out Circular_Buffer);
   procedure Move_Between_Buffers (From : in out Circular_Buffer;
                                   To   : in out Circular_Buffer) is
      Tmp : Int;
   begin
      loop
         Take_From_Buffer (From, Tmp);
         exit when Tmp = -1;
         Store_In_Buffer (To, Tmp);
      end loop;
   end Move_Between_Buffers;
   pragma Inline (Move_Between_Buffers);

   ----------------------
   -- Keyboard buffers --
   ----------------------
   Input_Buffer  : Circular_Buffer;
   --  When in "cooked mode" typed characters are stored in this buffer and
   --  are not sent to the 'Output_Buffer' util a character CR ir received.
   Output_Buffer : Circular_Buffer;
   --  The characters stored in this buffer are available for the
   --  applications. When in "raw mode" characters are stored in this buffer
   --  as soon as they are typed.

   ----------------------------------------------------------------------------
   -- Input modes -------------------------------------------------------------
   ----------------------------------------------------------------------------
   Echo_On : Boolean := True;
   --  If true the characters are echoed to the console as they are typed.

   type Modes_Of_Operation is (COOKED_MODE, RAW_MODE);
   Mode : Modes_Of_Operation := COOKED_MODE;
   --  "cooked mode": line editing is allowed, the textual unit of input in
   --  this mode is an entire "line" of text, where a "line" is a sequence
   --  of characters terminated by the line termination character CR. Thus,
   --  characters typed in this mode are not immediately made available to
   --  the calling program. They are first buffered to allow the user to
   --  perform line editing (erase characteres) on them.
   --
   --  "raw mode": Every character is made available to the calling program
   --  as soon as it is typed, so no line editing is available in this mode.

   Blocking_Mode : Boolean := True;
   --  When 'Blocking_Mode' is set, tasks are blocked in case there
   --  are not enough characters in buffer to fulfill a read
   --  operation. Otherwise, read operations returns immediately the
   --  available characters (maybe none).




   ----------------------------------------------------------------------------
   -- Keyboard_Interrupt_Handler ----------------------------------------------
   ----------------------------------------------------------------------------
   function Keyboard_Interrupt_Handler (Area : in System.Address;
                                        Intr : in HW_INTS.Hardware_Interrupt)
                                       return HW_INTS.Handler_Return_Code;
   Space : constant MaRTE.Integer_Types.Unsigned_Char :=
     MaRTE.Integer_Types.Unsigned_Char (Character'Pos (' '));
   Key_UC : MaRTE.Integer_Types.Unsigned_Char;
   function Keyboard_Interrupt_Handler (Area : in System.Address;
                                        Intr : in HW_INTS.Hardware_Interrupt)
                                       return HW_INTS.Handler_Return_Code is
      Key   : Int;
      Ret   : Int;
      Value : aliased MaRTE_Semaphores.Semaphore_Value;
      use type MaRTE_Semaphores.Semaphore_Value;
      use MaRTE.Integer_Types;
   begin
      Key := Trygetchar;
      if Key /= -1 then
         Key_UC := Unsigned_Char (Key);

         --  Finish application ?
         if Key = Finishing_Key then
            MaRTE.Direct_IO.Put
              ("Finishing key pressed: Finalizing Program. ");
            Exit_Process (1);
         end if;

         --  Characters should be echoed ?
         if Echo_On then
            MaRTE.Direct_IO.Direct_Write_On_Stdout (Key_UC'Address, 1);
         end if;

         --  Mode operations
         case (Mode) is
            when COOKED_MODE =>
               case (Key) is
                  when Character'Pos (ASCII.DEL) | Character'Pos (ASCII.BS) =>
                     if Echo_On then
                        --  The cursor is now in the position occupied for the
                        --  removed character: first we clean it ...
                        MaRTE.Direct_IO.Direct_Write_On_Stdout (Space'Address,
                                                                1);
                        --  ... and then reposition the cursor again.
                        MaRTE.Direct_IO.Direct_Write_On_Stdout (Key_UC'Address,
                                                                1);
                     end if;
                     Discard_Tail_Of_Buffer (Input_Buffer);
                  when Character'Pos (ASCII.CR) =>
                     --  Gnat "get" functions don't work with CR, they need LF.
                     Store_In_Buffer (Input_Buffer, Character'Pos (ASCII.LF));
                     Move_Between_Buffers (From => Input_Buffer,
                                           To   => Output_Buffer);

                     --  Post semaphore if not "posted" before
                     Ret := MaRTE_Semaphores.Getvalue (Sem'Access,
                                                       Value'Access);
                     if Value = 0 then
                        Ret := MaRTE_Semaphores.Post (Sem'Access);
                     end if;
                  when others =>
                     Store_In_Buffer (Input_Buffer, Key);
               end case;
            when RAW_MODE =>
               if  Key = Character'Pos (ASCII.CR) then
                  --  GNAT "get" functions don't work with CR, they need LF.
                  Store_In_Buffer (Output_Buffer, Character'Pos (ASCII.LF));
               else
                  Store_In_Buffer (Output_Buffer, Key);
               end if;

               --  Post semaphore if not "posted" before
               Ret := MaRTE_Semaphores.Getvalue (Sem'Access,
                                                 Value'Access);
               if Value = 0 then
                  Ret := MaRTE_Semaphores.Post (Sem'Access);
               end if;
         end case;
      end if;
      return HW_INTS.POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
   end Keyboard_Interrupt_Handler;


   ----------------------------------------------------------------------------
   -- Driver Functions --------------------------------------------------------
   ----------------------------------------------------------------------------

   ------------
   -- Create --
   ------------
   function Create return Int is
   begin
      --  Install handler
      if HW_INTS.Associate (Intr      => HW_INTS.KEYBOARD_INTERRUPT,
                            Handler   => Keyboard_Interrupt_Handler'Access,
                            Area      => System.Null_Address,
                            Area_Size => 0) /= 0 then
         return -1;
      end if;

      --  Configure keyboard
      if Initialize_Keyboard /= 1 then
         return -1;
      end if;

      --  Create Semaphore
      if MaRTE_Semaphores.Initialize (Sem     => Sem'Access,
                                      Pshared => 0,
                                      Value   => 0) /= 0 then
         return -1;
      end if;

      --  Enable interrupt
      if HW_INTS.Unlock (HW_INTS.KEYBOARD_INTERRUPT) /= 0 then
         return -1;
      end if;
      return 0;
   end Create;


   ----------
   -- Read --
   ----------
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T is
      Key        : Int;
      Ret        : Int;
      Value      : aliased MaRTE_Semaphores.Semaphore_Value;
      Count      : Buffer_Length := 1;
      Flags      : Integer;
      use type Buffer_Length;
      use type MaRTE_Semaphores.Semaphore_Value;
   begin
      if Bytes > 0 then
         if Blocking_Mode then
            if MaRTE_Semaphores.Wait (Sem'Access) /= 0 then
               return -1;
            end if;
         else  --  not Blocking_Mode
            if MaRTE_Semaphores.Trywait (Sem'Access) /= 0 then
               return -1;
            end if;
         end if;
      end if;

       while Count <= Bytes loop
          MaRTE.HAL.Save_Flags_And_Disable_Interrupts (Flags);
          Take_From_Buffer (Output_Buffer, Key);
          MaRTE.HAL.Restore_Flags (Flags);
          if Key = -1 then
             --  Character available ??
             if Blocking_Mode then
                --  Task waits for new char(s) available
                if MaRTE_Semaphores.Wait (Sem'Access) /= 0 then
                   return -1;
                end if;
             else  --  not Blocking_Mode
                exit when MaRTE_Semaphores.Trywait (Sem'Access) /= 0;
             end if;
         else
            Buffer_Ptr.all (Count) := MaRTE.Integer_Types.Unsigned_8 (Key);
            Count := Count + 1;
         end if;
       end loop;

       --  If buffer is not empty post semaphore
       MaRTE.HAL.Save_Flags_And_Disable_Interrupts (Flags);
       if Output_Buffer.Size > 0 then
          Ret := MaRTE_Semaphores.Getvalue (Sem'Access, Value'Access);
          if Value = 0 then
             Ret := MaRTE_Semaphores.Post (Sem'Access);
          end if;
       end if;
       MaRTE.HAL.Restore_Flags (Flags);

       --  Return read count
       return Ssize_T (Count - 1);
   end Read;


   -----------
   -- Ioctl --
   -----------
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac)  return Int is
      use Keyboard;
      type Stored_Mode_Ac is access all Keyboard.Stored_Mode;
      function To_Mod is new Ada.Unchecked_Conversion (Buffer_Ac,
                                                       Stored_Mode_Ac);
      Flags : Integer;
      Ret   : Int;
      Value : aliased MaRTE_Semaphores.Semaphore_Value;
      use type MaRTE_Semaphores.Semaphore_Value;
   begin
      MaRTE.HAL.Save_Flags_And_Disable_Interrupts (Flags);
      case Request is
         when Keyboard.Ioctl_Commads'Enum_Rep (Set_Cooked_Mode) =>
            Mode := COOKED_MODE;

         when Keyboard.Ioctl_Commads'Enum_Rep (Set_Raw_Mode) =>
            Mode := RAW_MODE;
            Move_Between_Buffers (From => Input_Buffer,
                                  To   => Output_Buffer);
            --  Post semaphore if not "posted" before
            Ret := MaRTE_Semaphores.Getvalue (Sem'Access, Value'Access);
            pragma Assert (Ret < 2);
            if Value = 0 then
               Ret := MaRTE_Semaphores.Post (Sem'Access);
            end if;

         when Keyboard.Ioctl_Commads'Enum_Rep (Enable_Echo) =>
            Echo_On := True;

         when Keyboard.Ioctl_Commads'Enum_Rep (Disable_Echo) =>
            Echo_On := False;

         when Keyboard.Ioctl_Commads'Enum_Rep (Set_Blocking_Mode) =>
            Blocking_Mode := True;

         when Keyboard.Ioctl_Commads'Enum_Rep (Reset_Blocking_Mode) =>
            Blocking_Mode := False;

         when Keyboard.Ioctl_Commads'Enum_Rep (Save_Mode) =>
            To_Mod (Ioctl_Data_Ptr).all := 0;
            if Mode = COOKED_MODE then
               To_Mod (Ioctl_Data_Ptr).all :=
                 To_Mod (Ioctl_Data_Ptr).all or Keyboard.Cooked_Mask;
            end if;
            if Echo_On then
               To_Mod (Ioctl_Data_Ptr).all :=
                 To_Mod (Ioctl_Data_Ptr).all or Keyboard.Echo_Mask;
            end if;
            if Blocking_Mode then
               To_Mod (Ioctl_Data_Ptr).all :=
                 To_Mod (Ioctl_Data_Ptr).all or Keyboard.Blocking_Mask;
            end if;

         when Keyboard.Ioctl_Commads'Enum_Rep (Restore_Mode) =>
            if ((To_Mod (Ioctl_Data_Ptr).all and
                 Keyboard.Cooked_Mask) /= 0) then
               Mode := Cooked_Mode;
            else
               Mode := Raw_Mode;
            end if;
            Echo_On :=
              (To_Mod (Ioctl_Data_Ptr).all and Keyboard.Echo_Mask) /= 0;
            Blocking_Mode :=
              (To_Mod (Ioctl_Data_Ptr).all and Keyboard.Blocking_Mask) /= 0;

         when others =>
            return -1;
      end case;
      MaRTE.HAL.Restore_Flags (Flags);

      return 0;
   end Ioctl;

   -----------------
   -- Direct_Read --
   -----------------
   function Direct_Read return MaRTE.Integer_Types.Unsigned_8 is
      Key : Int;
   begin
      loop
         Key := Trygetchar;
         exit when Key >= 0;
      end loop;
      return MaRTE.Integer_Types.Unsigned_8 (Key);
   end Direct_Read;

--        Store_Head_Of_Buffer (Output_Buffer, C);

--        Unlock_Keyboard;
--        Kernel.Leave_Critic_Section (Flags);
--        return C; -- XXX Must return EOF on error
--     end Unget_Char;

end Keyboard_Functions;
