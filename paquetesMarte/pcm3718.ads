------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                              'M a R T E'
--
--                                 Spec
--
--
--  File 'pcm3718.ads'                                      By Sangorrin
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
with MaRTE.HAL.IO; -- for IO_Port in PCM3718_BASE
with MaRTE.Integer_Types; -- for Unsigned_8/16
with MARTE_Hardware_Interrupts; use MARTE_Hardware_Interrupts; -- for IRQ


package Pcm3718 is

   package BIT renames MaRTE.Integer_Types;
   use type BIT.Unsigned_8;

   -----------------------------------------------------------------------
   -- 1.- CONSTANTS
   -----------------------------------------------------------------------
   -- a) HARDWARE CONSTANTS
   -- Change these values in order to match with your board configuration
   Pcm3718_Base  : constant MaRTE.HAL.IO.Io_Port := 16#300#;
   F_Clock_Pacer : constant := 1E6; -- 1E6=1Mhz or 1E6=10Mhz
   Irq           : constant Hardware_Interrupt   := Parallel2_Interrupt;
   Num_Channels  : constant := 16; -- 16=SingleEnded  8=Differencial
   -- b) SOFTWARE CONSTANTS (Adjustable)
   Buffer_Mx   : constant := 256;  -- Internal Buffer Capacity (in samples)
   -- c) SOFTWARE CONSTANTS (Don't change)
   AD_Code_Min   : constant := 0;    -- Constants needed by the user program
   AD_Code_Max   : constant := 4095; -- to convert Samples into real varia
   AD_Code_Range : constant := 4096; -- bles like Temperature, Voltage ...

   -----------------------------------------------------------------------
   -- 2.- DATA TYPES
   -----------------------------------------------------------------------
   --    a) DIGITAL DATA
   subtype Digital_Data is BIT.Unsigned_16;
   --    b) ANALOG DATA
   type Channel_Type is range 0 .. Num_Channels-1;
   type Sample_Type is range AD_Code_Min .. AD_Code_Max;
   type Analog_Data_Type is record
      The_Sample  :   Sample_Type;
      The_Channel :  Channel_Type;
   end record;
   type Num_Conv_Type is range 0 .. Buffer_Mx;
   subtype Analog_Data_Index is Integer range 1 .. Buffer_Mx;
   type Analog_Data is array (Analog_Data_Index range <>) of Analog_Data_Type;

   -----------------------------------------------------------------------
   -- 3.- IOCTL COMMANDS and ARGUMENTS
   -----------------------------------------------------------------------
   -- Control the device through a Generic IOCTL Posix_IO function.
   -----------------------------------------------------------------------
   --    a) COMMANDS
   -----------------------------------------------------------------------
   type Ai_Ioctl_Cmd is         -- Analog Input Commands ( Arguments needed )
         (Set_Range_Of_Channel, -- (Input_Range,Start_Ch)
          Set_Parameters,       -- (Trigger,Start_Ch,Stop_Ch,[Mode,Count,C1,C2])
          Get_Status,           -- (Num_Conv) for scan_mode
          Flush);               --  (No Args) for scan_mode

   type Dio_Ioctl_Cmd is      -- Digital I/O commands
         (Mode_Byte_1,        -- (No Args) RW only the first 8bit register
          Mode_Byte_2,        -- (No Args) RW only the second 8bit register
          Mode_Word);         -- (No Args) RW both as a 16bit register (DEFAULT)
   -----------------------------------------------------------------------
   --    b) ARGUMENTS
   -----------------------------------------------------------------------
   type Range_Type is     --      Range of the input voltage
         (Bipolar_5,      --            -5 < Vin < 5
          Bipolar_2_5,    --          -2.5 < Vin < 2.5
          Bipolar_1_25,   --         -1.25 < Vin < 1.25
          Bipolar_0_625,  --        -0.625 < Vin < 0.625
          Unipolar_10,    --             0 < Vin < 10
          Unipolar_5,     --             0 < Vin < 5
          Unipolar_2_5,   --             0 < Vin < 2.5
          Unipolar_1_25,  --             0 < Vin < 1.25
          Bipolar_10);    --           -10 < Vin < 10

   -- There are FIVE configurations
   -- 1) SOFTWARE TRIGGER
   -- 2) PACER TRIGGER + FIXED MODE
   -- 3) PACER TRIGGER + SCAN MODE
   -- 4) EXTERNAL TRIGGER + FIXED MODE
   -- 5) EXTERNAL TRIGGER + SCAN MODE
   ------------------------------------------------------------------
   -- When Fixed Mode is set the driver adquires "Count" samples
   -- each time you call READ.
   ------------------------------------------------------------------
   -- When Scan Mode is set the driver is continuosly storing
   -- samples in a internal Buffer (It can overwrite the old ones),
   -- and when you call READ you read the samples in the Buffer.
   ------------------------------------------------------------------

   type Trigger_Type is (Software, External, Pacer);
   type Mode_Type is (Fixed, Scan);

   -- SCAN_RATE_TYPE
   -- This is the rate used to acquire data when using PACER TRIGGER.
   -- If F_Clock_Pacer = 1E6Hz => Scan_Rate is in seconds*10e-6 (uS)
   -- If F_Clock_Pacer = 10E6Hz => Scan_Rate is in seconds*10e-7
   -- The driver will try to aproximate this rate and return this
   -- aproximation on the same variable of the Ioctl Argument.
   subtype Scan_Rate_Type is BIT.Unsigned_32 range 4 .. 4_294_836_225;

   type Ai_Ioctl_Arg is
      record
         Input_Range : Range_Type;
         Start_Ch    : Channel_Type;
         Stop_Ch     : Channel_Type;
         Trigger     : Trigger_Type;
         Mode        : Mode_Type;
         Count       : Num_Conv_Type;
         Scan_Rate   : Scan_Rate_Type;
      end record;

private
   for Channel_Type'Size use 8;
   for Sample_Type'Size use 16;
   for Analog_Data_Type'Size use 32; -- 4 Bytes (for alignment in Analog_Data)
   for Num_Conv_Type'Size use 32;
   for Ai_Ioctl_Cmd'Size use Integer'Size;
   for Dio_Ioctl_Cmd'Size use Integer'Size;
   for Range_Type'Size use 8;
   for Range_Type use (
      Bipolar_5     => 0,  Bipolar_2_5   => 1,  Bipolar_1_25  => 2,
      Bipolar_0_625 => 3,  Unipolar_10   => 4,  Unipolar_5    => 5,
      Unipolar_2_5  => 6,  Unipolar_1_25 => 7,  Bipolar_10    => 8);
   for Trigger_Type'Size use 8;
   for Mode_Type'Size use 8;
end Pcm3718;
