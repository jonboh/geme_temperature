------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             {MARTE_VERSION}
--
--                              'M a R T E'
--
--                                 Spec
--
--
--  File 'pcm3712_functions.adb'                                      By Barros, Puig-Peig
--
--  {MARTE_COPYRIGHT}
--
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion; -- For Ai_Ioctl argument
--------------------------------------------------------------------------
with MaRTE.Integer_Types; -- for Unsigned_8/16, shift operations
use MaRTE.Integer_Types;
with MaRTE.HAL.IO;
use MaRTE.HAL.IO; -- Inb_P, Outb_P, IO_Port

with System; -- System.Address in IRQ_handler
with MaRTE.Direct_IO;
use MaRTE.Direct_IO; -- Put, New_Line
with Ada.Text_Io;
use Ada.Text_Io;
with Ada.IO_Exceptions;

with Pcm3712;
use Pcm3712; -- For the rest of types
--------------------------------------------------------------------------
package body Pcm3712_Functions is
   ----------------------------
   --  1.- PCM3712 REGISTERS --
   ----------------------------
   Pcm3712_Ao_Low_0  : constant Io_Port := 0;  -- WO
   Pcm3712_Ao_High_0 : constant Io_Port := 1;  -- WO
   Pcm3712_Ao_Low_1  : constant Io_Port := 2;  -- WO
   Pcm3712_Ao_High_1 : constant Io_Port := 3;  -- WO
   Pcm3712_SynchronousControl  : constant Io_Port := 4;  -- WO
   Pcm3712_OutputControl : constant Io_Port := 5;  -- RW
   ----------------------
   -- 2.- ANALOG OUTPUT --
   ----------------------
   Ao_Synchronous :Boolean;
   Ao_NumChannel  :Ao_Channels;
   Ao_Polarity	:Polar_Mode:=Unipolar;
   Ao_Range: voltage;
   Ao_Configuration: Ao_Ioctl_Arg;
   ----------------
   -- Ao_Open --
   ----------------
   function Ao_Open (
                     Fd   : in     File_Descriptor;
                     Mode : in     File_Access_Mode)
                     return Drivers_Marte.Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put_line ("AnalogOutput of PCM3712: Opening device file " &
                File_Descriptor'Image (Fd) &
                "(Major:" & Major'Image (Mj) &
                " Minor:" & Minor'Image (Mn));
      return 0;
   end Ao_Open;
   -----------------
   -- Ao_Close --
   -----------------
   function Ao_Close (
                      Fd : in     File_Descriptor)
                      return Drivers_Marte.Int is
      Mj         : Major;
      Mn         : Minor;
      Invalid_Fd : Boolean;
   begin
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put_line ("AnalogInput of PCM3712: Closing device file " &
                File_Descriptor'Image (Fd) &
                "(Major:" & Major'Image (Mj) &
                " Minor:" & Minor'Image (Mn) & ")");
      return 0;
   end Ao_Close;
   -----------------
   -- Ao_Ioctl --
   ------------------
   function Ao_Ioctl (
                      Fd             : in     File_Descriptor;
                      Request        : in     Ioctl_Option_Value;
                      Ioctl_Data_Ptr : in     Buffer_Ac)
                      return Drivers_Marte.Int is

      type Ioctl_Arg_Ac is access all Ao_Ioctl_Arg;
      function Buffer_Ac_To_Ioctl_Data_Ac is
        new Ada.Unchecked_Conversion (Buffer_Ac, Ioctl_Arg_Ac);
      Arg  : Ioctl_Arg_Ac;

   begin
      arg :=Buffer_Ac_To_Ioctl_Data_Ac (Ioctl_Data_Ptr);
      case Ao_Mode'Val(Request) is
         when Configuration =>
            if (arg.Mode =Synchronous) then
               Ao_Synchronous:=True;
            else
               Ao_Synchronous:=False;
            end if;
            Ao_NumChannel  :=arg.Channel;
            Ao_Polarity	:=arg.Polarity;
            Ao_Range:= arg.Vref;
         when others =>
            null;
      end case;
      return 0;
   end Ao_Ioctl;
   ------------------
   --Ao_Write --
   ------------------
   function Ao_Write (
                      Fd         : in     File_Descriptor;
                      Buffer_Ptr : in     Buffer_Ac;
                      Bytes      : in     Buffer_Length)
                      return Drivers_Marte.Ssize_T is
      use type Drivers_Marte.Buffer_Length;

      Voltage_Ptr : Voltage_Ac;
      code,code_temp:Sample_Type;

      function Buffer_Ac_To_Voltage_Ac is
        new Ada.Unchecked_Conversion (Buffer_Ac, Voltage_Ac);

      procedure Code_Register (Code: in Sample_Type) is
         code_high: Sample_Byte_Type;
         code_low: Sample_Byte_Type;
      begin
--           Put_Line("Code" & Sample_Type'image(code));
         code_low:=Sample_Byte_Type(Unsigned_16(code) AND Unsigned_16(16#00FF#));
         code_high:=Sample_Byte_Type(MaRTE.Integer_Types.Shift_Right(Unsigned_16(code),8));
         case Ao_NumChannel is
         when channel_1=>
--              Put_Line("Channel 1");
--              Put_Line("Code  low" & Sample_Byte_Type'image(code_low));
--              Put_Line("Code  high" & Sample_Byte_Type'image(code_high));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_0, Unsigned_8(code_low));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_High_0, Unsigned_8(code_high));
         when channel_2=>
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_1, Unsigned_8(code_low));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_1, Unsigned_8(code_high));
         when channel_all=>
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_0, Unsigned_8(code_low));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_High_0, Unsigned_8(code_high));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_1, Unsigned_8(code_low));
            Outb_P (Pcm3712_Base + Pcm3712_Ao_Low_1, Unsigned_8(code_high));
         end case;
      end Code_Register;

   begin
      Voltage_Ptr := Buffer_Ac_To_Voltage_Ac (Buffer_Ptr);
      Outb_P (Pcm3712_Base + Pcm3712_OutputControl,16#80# );
      case Ao_Polarity is
         when Unipolar =>
            if (Voltage_Ptr.all> Ao_Range) then
               Put_Line("Voltage out of Range");
            else
               code:=Sample_Type(float(Voltage_Ptr.all)*4096.0/(2.0*float(Ao_Range)));
               Code_Register(code);
            end if;
         when Bipolar=>
            if (Voltage_Ptr.all> Ao_Range) then
               Put_Line("Voltage out of Range");
            else
               if (float(2048.0+Float(Voltage_Ptr.all)*2048.0/Float(Ao_Range)))>4095.0 then --Victor, Rafa
                  code:=4095;
               else
                  code:=Sample_Type(float(2048.0+Float(Voltage_Ptr.all)*2048.0/Float(Ao_Range)));
               end if;
               Code_Register(code);
            end if;
      end case;
      if(Ao_Synchronous) then
         Outb_P (Pcm3712_Base + Pcm3712_SynchronousControl,0);
      end if;
      Outb_P (Pcm3712_Base + Pcm3712_OutputControl,16#00# );
      return Drivers_Marte.Ssize_T(Bytes);
   end Ao_Write;
end Pcm3712_Functions;
