------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                             'pcm3718_functions'
--
--                                   body
--
--
--  File 'pcm3718_functions.adb'                           By Sangorrin
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
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion; -- For Ai_Ioctl argument
--------------------------------------------------------------------------
with Drivers_Marte;
use Drivers_Marte;
with Marte_Hardware_Interrupts;
pragma Elaborate_All (Marte_Hardware_Interrupts);
with Marte_Semaphores;
pragma Elaborate_All (Marte_Semaphores);
--------------------------------------------------------------------------
with MaRTE.HAL.IO;
use MaRTE.HAL.IO; -- Inb_P, Outb_P, IO_Port
with MaRTE.Integer_Types; -- for Unsigned_8/16, shift operations
with System; -- System.Address in IRQ_handler
with MaRTE.Direct_IO;
use MaRTE.Direct_IO; -- Put, New_Line
--------------------------------------------------------------------------
with Pcm3718_Buffer; -- For the internal Buffer
with Pcm3718;
use Pcm3718; -- For the rest of types
--------------------------------------------------------------------------
package body Pcm3718_Functions is

   package Bit renames MaRTE.Integer_Types;
   package Phi renames Marte_Hardware_Interrupts;
   use type Int;

   ----------------------------
   --  1.- PCM3718 REGISTERS --
   ----------------------------
   --   a) DATA
   Pcm3718_Ai_Low  : constant Io_Port := 0;  -- RO
   Pcm3718_Ai_High : constant Io_Port := 1;  -- RO
   Pcm3718_Dio_1   : constant Io_Port := 3;  -- RW
   Pcm3718_Dio_2   : constant Io_Port := 11; -- RW
   --   b) STATUS
   Pcm3718_Status  : constant Io_Port := 8;  -- RO
   --   c) CONTROL
   Pcm3718_Control : constant Io_Port := 9;  -- RW
   Pcm3718_Range   : constant Io_Port := 1;  -- WO
   Pcm3718_Mux     : constant Io_Port := 2;  -- RW
   Pcm3718_Softrig : constant Io_Port := 0;  -- WO
   --     d) PACER
   Pacer_C1        : constant Io_Port := 13; -- RW
   Pacer_C2        : constant Io_Port := 14; -- RW
   Pacer_Enable    : constant Io_Port := 10; -- WO
   Pacer_Control   : constant Io_Port := 15; -- WO

   ------------------------------
   -- 2.- DIGITAL INPUT/OUTPUT --
   ------------------------------
   Dio_Mode        : Pcm3718.Dio_Ioctl_Cmd;
   Dio_Usage_Count : Boolean; -- Only one user allowed

   -----------------
   -- a) Dio_Open --
   -----------------
   function Dio_Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      if Dio_Usage_Count = True then
         Put("Error: The Device 'Digital I/O of PCM3718' is Locked");
         New_Line;
         return -1;
      end if;
      -- Initialisation of internal variables:
      Dio_Mode := Pcm3718.Mode_Word;
      Dio_Usage_Count := True;
      -- Opening Message
      New_Line;
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("Digital Part of PCM3718: Opening device file " &
         File_Descriptor'Image (Fd) &
         "(Major:" & Major'Image (Mj) &
         " Minor:" & Minor'Image (Mn) &
         "). Mode:" & File_Access_Mode'Image (Mode));
      New_Line;
      return 0;
   end Dio_Open;

   ------------------
   -- b) Dio_Close --
   ------------------
   function Dio_Close (
         Fd : in     File_Descriptor)
     return Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      -- Decrement Ai_Usage_Count
      Dio_Usage_Count := False;
      -- Closing message
      New_Line;
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("Digital part of PCM3718: Closing device file " &
         File_Descriptor'Image (Fd) &
         "(Major:" & Major'Image (Mj) &
         " Minor:" & Minor'Image (Mn) & ")");
      New_Line;
      return 0;
   end Dio_Close;

   -----------------
   -- c) Dio_Read --
   -----------------
   function Dio_Read (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T is
      use type Drivers_Marte.Buffer_Length;
   begin
      if Bytes /= Digital_Data'Size/8 then
         return -1;
      end if;
      case Dio_Mode is   -- NOTA: Sería más rápido con unchecked conv???
         when Mode_Byte_1 =>
            Buffer_Ptr.All (1) := Inb_P (Pcm3718_Base + Pcm3718_Dio_1);
            Buffer_Ptr.All (2) := 0;
         when Mode_Byte_2 =>
            Buffer_Ptr.All (1) := Inb_P (Pcm3718_Base + Pcm3718_Dio_2);
            Buffer_Ptr.All (2) := 0;
         when Mode_Word =>
            Buffer_Ptr.All (1) := Inb_P (Pcm3718_Base + Pcm3718_Dio_1);
            Buffer_Ptr.All (2) := Inb_P (Pcm3718_Base + Pcm3718_Dio_2);
      end case;
      return Int (Bytes);
   end Dio_Read;

   ------------------
   -- d) Dio_Write --
   ------------------
   function Dio_Write (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T is
      use type Drivers_Marte.Buffer_Length;
   begin
      if Bytes /= Digital_Data'Size/8 then
         return -1;
      end if;
      case Dio_Mode is   -- NOTA: Comprobar que el byte superior es 0???
         when Mode_Byte_1 =>
            Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Buffer_Ptr.All (1));
         when Mode_Byte_2 =>
            Outb_P (Pcm3718_Base + Pcm3718_Dio_2, Buffer_Ptr.All (1));
         when Mode_Word =>
            Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Buffer_Ptr.All (1));
            Outb_P (Pcm3718_Base + Pcm3718_Dio_2, Buffer_Ptr.All (2));
      end case;
      return Int (Bytes);
   end Dio_Write;

   ------------------
   -- e) Dio_Ioctl --
   ------------------
   function Dio_Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
     return Int is
   begin
      Dio_Mode := Dio_Ioctl_Cmd'Val(Request);
      return 0;
   end Dio_Ioctl;

   ----------------------
   -- 3.- ANALOG INPUT --
   ----------------------
   Ai_Trigger     : Pcm3718.Trigger_Type;
   Ai_Start_Stop  : Bit.Unsigned_8;
   Ai_Count       : Pcm3718.Num_Conv_Type;
   Ai_Scan_Rate   : Scan_Rate_Type;
   Ai_Mode        : Pcm3718.Mode_Type;
   Ai_Usage_Count : Boolean;               -- Only one user allowed
   Irq_Mask       : Bit.Unsigned_8;

   --------------------------------------
   -- a) Internal Macros and Functions --
   --------------------------------------
   -- a.1 Rate_To_Counters
   --------------------------------------
   subtype Counter_Type is Bit.Unsigned_16 range 2 .. 65535;

   procedure Rate_To_Counters (
         Scan_Rate : in out Pcm3718.Scan_Rate_Type;
         C1        :    out Counter_Type;
         C2        :    out Counter_Type) is
      use type Bit.Unsigned_32;
      use type Bit.Unsigned_16;
      C2_Tmp,
      New_Error : Bit.Unsigned_32;
      Min_Error : Bit.Unsigned_32 := Bit.Unsigned_32'Last;
   begin
      for C1_Tmp in Bit.Unsigned_32 range 2 .. 65535 loop
         begin
            C2_Tmp := Scan_Rate/C1_Tmp;
            if C1_Tmp*C2_Tmp > Scan_Rate then
               New_Error := C1_Tmp*C2_Tmp - Scan_Rate;
            else
               New_Error := Scan_Rate - C1_Tmp*C2_Tmp;
            end if;
            if New_Error < Min_Error then
               C2 := Counter_Type(C2_Tmp);
               -- It can raise constraint error
               C1 := Counter_Type(C1_Tmp);
               Min_Error := New_Error;
            end if;
         exception
            when others =>
               null;
         end;
         exit when Min_Error = 0;
      end loop;
      Scan_Rate := Bit.Unsigned_32(C1)*Bit.Unsigned_32(C2);
   end Rate_To_Counters;
   --------------------------------------
   -- a.2 Lock-Unlock IRQ
   --------------------------------------
   procedure Lock_Irq is
   begin
      if Phi.Lock (Pcm3718.Irq) /= 0 then
         Put("Error: Could not Lock the IRQ");
      end if;
   end Lock_Irq;
   pragma Inline(Lock_Irq);
   procedure Unlock_Irq is
   begin
      if Phi.Unlock (Pcm3718.Irq) /= 0 then
         Put("Error: Could not Unlock the IRQ");
      end if;
   end Unlock_Irq;
   pragma Inline(Unlock_Irq);

   --------------------
   -- b) IRQ_Handler --
   --------------------
   Fixed_Sem   : aliased Marte_Semaphores.Semaphore;
   Fixed_Count : Pcm3718.Num_Conv_Type;
   Scan_Sem    : aliased Marte_Semaphores.Semaphore;

   function Pcm3718_Handler (
         Area : in     System.Address;
         Intr : in     Phi.Hardware_Interrupt)
     return Phi.Handler_Return_Code is
      Ret   : Bit.Int;
      Value : aliased Marte_Semaphores.Semaphore_Value;
      use type Marte_Semaphores.Semaphore_Value;
      Byte_Low,
      Byte_High : Bit.Unsigned_8;
   begin
      if Ai_Mode = Fixed then
         if Fixed_Count > 0 then
            Byte_High := Inb_P(Pcm3718_Base + Pcm3718_Ai_High);
            Byte_Low := Inb_P(Pcm3718_Base + Pcm3718_Ai_Low);
            Pcm3718_Buffer.Write(Byte_Low,Byte_High);
            Fixed_Count := Fixed_Count - 1;
            if Fixed_Count = 0 then
               Ret := Marte_Semaphores.Post (Fixed_Sem'access);
            end if;
         end if;
      else
         Byte_High := Inb_P(Pcm3718_Base + Pcm3718_Ai_High);
         Byte_Low := Inb_P(Pcm3718_Base + Pcm3718_Ai_Low);
         Pcm3718_Buffer.Write(Byte_Low,Byte_High);
         Ret := Marte_Semaphores.Getvalue (Scan_Sem'access, Value'access);
         if Value = 0 then
            Ret := Marte_Semaphores.Post (Scan_Sem'access);
         end if;
      end if;
      Outb_P(Pcm3718_Base + Pcm3718_Status, 1);
      return Phi.Posix_Intr_Handled_Do_Not_Notify;
   end Pcm3718_Handler;

   ----------------
   -- c) Ai_Open --
   ----------------
   function Ai_Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      if Ai_Usage_Count = True then
         Put("Error: The Device 'AnalogInput of PCM3718' is Locked");
         New_Line;
         return -1;
      end if;
      -- Hardware initialisation:
      -- Interrutps=DMA=Off SoftTrigger=ON Pacer=Disabled
      -- Start_Ch=Stop_Ch=0 RangeChannel_0=unipolar_5
      Outb_P (Pcm3718_Base + Pcm3718_Control, Irq_Mask);
      Outb_P (Pcm3718_Base + Pacer_Enable, 0);
      Outb_P (Pcm3718_Base + Pcm3718_Status, 1);
      Outb_P (Pcm3718_Base + Pcm3718_Mux, 0);
      Outb_P (Pcm3718_Base + Pcm3718_Range, Range_Type'Pos(Unipolar_5));
      if Phi.Associate (Pcm3718.Irq, Pcm3718_Handler'access,
            System.Null_Address, 0) /= 0 then
         Put("Error: Could not associate to the IRQ");
      end if;
      -- Initialisation of internal variables:
      Ai_Trigger    := Software;
      Ai_Start_Stop := 0;
      Ai_Count      := 1;
      Ai_Mode       := Fixed;
      Ai_Usage_Count := True;
      Irq_Mask := Bit.Shift_Left(Bit.Unsigned_8(Pcm3718.Irq),4);
      Pcm3718_Buffer.Flush;
      -- Opening Message
      New_Line;
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("AnalogInput of PCM3718: Opening device file " &
         File_Descriptor'Image (Fd) &
         "(Major:" & Major'Image (Mj) &
         " Minor:" & Minor'Image (Mn) &
         "). Mode:" & File_Access_Mode'Image (Mode));
      New_Line;
      return 0;
   end Ai_Open;

   -----------------
   -- d) Ai_Close --
   -----------------
   function Ai_Close (
         Fd : in     File_Descriptor)
     return Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      -- Disable Interrupts and Pacer
      Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask);
      Outb_P(Pcm3718_Base + Pacer_Enable, 0);
      if Phi.Disassociate (Pcm3718.Irq,Pcm3718_Handler'access) /= 0 then
         Put("Error: Could not disassociate the IRQ");
      end if;
      -- Decrement Ai_Usage_Count
      Ai_Usage_Count := False;
      -- Closing message
      New_Line;
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("AnalogInput of PCM3718: Closing device file " &
         File_Descriptor'Image (Fd) &
         "(Major:" & Major'Image (Mj) &
         " Minor:" & Minor'Image (Mn) & ")");
      New_Line;
      return 0;
   end Ai_Close;

   ----------------
   -- e) Ai_Read --
   ----------------
   function Ai_Read (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T is
      use type Buffer_Length;
      use type Bit.Unsigned_8;
      use type Bit.Unsigned_32;
      function Address_To_Buffer_Ac is
      new Ada.Unchecked_Conversion
         (System.Address, Buffer_Ac);

      function Smaller (
            A : in     Int;
            B : in     Num_Conv_Type)
        return Num_Conv_Type is
      begin
         if A < Int(B) then
            return Num_Conv_Type(A);
         else
            return B;
         end if;
      end Smaller;
      pragma Inline(Smaller);

      Buffer_Tmp : Buffer_Ac;
      Data_Size  : constant :=  4; -- Size of Analog_Data Element in bytes
      Ai_Data    : Pcm3718.Analog_Data (1 .. Integer (Bytes) / Data_Size);
      N          : Num_Conv_Type;
      Byte       : Bit.Unsigned_8;
      Ret        : Int;
      Value      : aliased Marte_Semaphores.Semaphore_Value;
      use type Marte_Semaphores.Semaphore_Value;
      Byte_Low,
      Byte_High : Bit.Unsigned_8;
   begin
      case Ai_Trigger is
         when Software =>
            N := Smaller(Int(Bytes)/Data_Size, Ai_Count);
            Pcm3718_Buffer.Flush;
            for I in 1 .. Int(N) loop
               -- Method 1: Polling (7.5us)
               Outb_P(Pcm3718_Base + Pcm3718_Softrig, 1);
               loop
                  Byte := Inb_P(Pcm3718_Base + Pcm3718_Status);
                  Byte := Byte and 2#1000_0000#;
                  exit when Byte = 0;
               end loop;
               Byte_High := Inb_P(Pcm3718_Base + Pcm3718_Ai_High);
               Byte_Low := Inb_P(Pcm3718_Base + Pcm3718_Ai_Low);
               Pcm3718_Buffer.Write(Byte_Low,Byte_High);
            end loop;
            Pcm3718_Buffer.Read (Ai_Data,N);
         when Pacer | External =>
            if Ai_Mode = Fixed then
               N := Smaller(Int(Bytes)/Data_Size, Ai_Count);
               Fixed_Count := N;
               Pcm3718_Buffer.Flush;
               if Marte_Semaphores.Initialize (Fixed_Sem'access,0,0) /= 0 then
                  return -1;
               end if;
               -- Initialisation of Interrupts Environment ----------------
               if Ai_Trigger = Pacer then
                  Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask or
                     16#83#);
                  Outb_P(Pcm3718_Base + Pacer_Enable, 1);
               else
                  Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask or
                     16#82#);
               end if;
               Unlock_Irq;
               ------------------------------------------------------------
               if Marte_Semaphores.Wait (Fixed_Sem'access) /= 0 then
                  return -1;
               end if;
               -- Ending of Interrupts Environment ------------------------
               Lock_Irq;
               if Ai_Trigger = Pacer then
                  Outb_P(Pcm3718_Base + Pacer_Enable, 0);
                  Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask or
                     16#03#);
               else
                  Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask or
                     16#02#);
               end if;
               ------------------------------------------------------------
               Pcm3718_Buffer.Read (Ai_Data,N);
            else -- scan mode
               if Marte_Semaphores.Wait (Scan_Sem'access) /= 0 then
                  return -1;
               end if;
               Lock_Irq;
               Pcm3718_Buffer.Read (Ai_Data,N);
               if Pcm3718_Buffer.Get_Status > 0 then
                  Ret := Marte_Semaphores.Getvalue (Scan_Sem'access, Value'access);
                  if Value = 0 then
                     Ret := Marte_Semaphores.Post (Scan_Sem'access);
                  end if;
               end if;
               Unlock_Irq;
            end if;
      end case;
      -- Return the Data
      Buffer_Tmp := Address_To_Buffer_Ac (Ai_Data'Address);
      Buffer_Ptr.All(1 .. Buffer_Length(N*Data_Size)) :=
         Buffer_Tmp.All(1 .. Buffer_Length(N*Data_Size));
      return Int(N*Data_Size);
   end Ai_Read;

   -----------------
   -- f) Ai_Ioctl --
   -----------------
   function Ai_Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
     return Int is
      use type Bit.Unsigned_8;
      use type Bit.Unsigned_16;
      -- Unchecked Conversion of Ioctl Arg
      type Ioctl_Arg_Ac is access all Ai_Ioctl_Arg;
      function Buffer_Ac_To_Ioctl_Data_Ac is
      new Ada.Unchecked_Conversion (Buffer_Ac, Ioctl_Arg_Ac);
      -- Local Variables
      Arg  : Ioctl_Arg_Ac;
      Byte : Bit.Unsigned_8;
      C1,
      C2   : Counter_Type;
   begin
      Arg := Buffer_Ac_To_Ioctl_Data_Ac (Ioctl_Data_Ptr);
      case Ai_Ioctl_Cmd'Val(Request) is
         when Set_Range_Of_Channel =>
            Outb_P(Pcm3718_Base + Pcm3718_Mux, Bit.Unsigned_8(
                  Arg.Start_Ch));
            Outb_P(Pcm3718_Base+Pcm3718_Range,Range_Type'Pos(
                  Arg.Input_Range));
            -- Reset Start and Stop Channel
            Outb_P(Pcm3718_Base + Pcm3718_Mux, Ai_Start_Stop);
         when Set_Parameters =>
            -- Prepare the card for sampling!
            Ai_Trigger := Arg.Trigger;
            case Ai_Trigger is
               when Software =>
                  -- Set Start and Stop Channels
                  Byte := Bit.Shift_Left(Bit.Unsigned_8(Arg.Stop_Ch),4);
                  Ai_Start_Stop:= Byte or Bit.Unsigned_8(Arg.Start_Ch);
                  Outb_P(Pcm3718_Base + Pcm3718_Mux, Ai_Start_Stop);
                  -- Set Software trigger and clean Int
                  Ai_Count := Arg.Count;
                  Outb_P(Pcm3718_Base + Pcm3718_Control, Irq_Mask or
                     16#00#);
                  Outb_P(Pcm3718_Base + Pcm3718_Status, 1);
                  if Ai_Mode = Scan then
                     Lock_Irq;
                  end if;
                  Ai_Mode := Fixed;
               when Pacer | External =>
                  if Ai_Trigger = Pacer then
                     Rate_To_Counters(Arg.Scan_Rate, C1, C2);
                     Ai_Scan_Rate := Arg.Scan_Rate;
                     -- Set Counter C1:
                     Outb_P(Pcm3718_Base + Pacer_Control, 16#76#);
                     Byte := Bit.Unsigned_8(C1 and 16#FF#);
                     Outb_P(Pcm3718_Base + Pacer_C1, Byte); -- C1_Low
                     Byte := Bit.Unsigned_8(Bit.Shift_Right(C1,8));
                     Outb_P(Pcm3718_Base + Pacer_C1, Byte); -- C1_High
                     -- Set Counter C2:
                     Outb_P(Pcm3718_Base + Pacer_Control, 16#B6#);
                     Byte := Bit.Unsigned_8(C2 and 16#FF#);
                     Outb_P(Pcm3718_Base + Pacer_C2, Byte); -- C2_Low
                     Byte := Bit.Unsigned_8(Bit.Shift_Right(C2,8));
                     Outb_P(Pcm3718_Base + Pacer_C2, Byte); -- C2_High
                  end if;
                  -- Fixed or Scan Modes:
                  if Ai_Mode = Scan then
                     Lock_Irq; -- Lock if the previous mode was Scan
                     Outb_P(Pcm3718_Base + Pacer_Enable, 0);
                  end if;
                  -- Set Start and Stop Channels
                  Byte := Bit.Shift_Left(Bit.Unsigned_8(Arg.Stop_Ch),4);
                  Ai_Start_Stop:= Byte or Bit.Unsigned_8(Arg.Start_Ch);
                  Outb_P(Pcm3718_Base + Pcm3718_Mux, Ai_Start_Stop);
                  -- Initialisation of Interrupts Environment ---------
                  Ai_Mode := Arg.Mode;
                  if Ai_Mode = Fixed then
                     Ai_Count := Arg.Count;
                     if Ai_Trigger = Pacer then
                        Outb_P(Pcm3718_Base+Pcm3718_Control,Irq_Mask or
                           16#03#);
                     else -- External
                        Outb_P(Pcm3718_Base+Pcm3718_Control,Irq_Mask or
                           16#02#);
                     end if;
                     Outb_P(Pcm3718_Base + Pcm3718_Status, 1);
                     -- clear irq
                  else -- scan mode
                     if Marte_Semaphores.Initialize(
                           Scan_Sem'access,0,0) /= 0 then
                        return -1;
                     end if;
                     Outb_P(Pcm3718_Base + Pcm3718_Status, 1);
                     if Ai_Trigger = Pacer then
                        Outb_P(Pcm3718_Base+Pcm3718_Control,Irq_Mask or
                           16#83#);
                        Outb_P(Pcm3718_Base + Pacer_Enable, 1);
                     else
                        Outb_P(Pcm3718_Base+Pcm3718_Control,Irq_Mask or
                           16#82#);
                     end if;
                     Unlock_Irq;
                  end if;
            end case;
         when Get_Status =>
            if Ai_Mode = Scan then
               Lock_Irq;
               Arg.Count := Pcm3718_Buffer.Get_Status;
               Unlock_Irq;
            else
               Arg.Count := Pcm3718_Buffer.Get_Status;
            end if;
         when Flush =>
            if Ai_Mode = Scan then
               Lock_Irq;
               Pcm3718_Buffer.Flush;
               Unlock_Irq;
            else
               Pcm3718_Buffer.Flush;
            end if;
      end case;
      return 0;
   end Ai_Ioctl;

end Pcm3718_Functions;
