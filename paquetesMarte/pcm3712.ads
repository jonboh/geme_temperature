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
--  File 'pcm3712.ads'                                      By Barros, Puig-Peig
--
--  {MARTE_COPYRIGHT}
--
-------------------------------------------------------------------------------

with MaRTE.HAL.IO; -- for IO_Port in PCM3712_BASE
with MaRTE.Integer_Types; -- for Unsigned_8/16
package Pcm3712 is
   package BIT renames MaRTE.Integer_Types;
   use type BIT.Unsigned_8;
   -----------------------------------------------------------------------
   -- 1.- CONSTANTS
   -----------------------------------------------------------------------
   -- a) HARDWARE CONSTANTS
   -- Change these values in order to match with your board configuration
   Pcm3712_Base  : constant MaRTE.HAL.IO.Io_Port := 16#220#;
   Num_Channels: constant:=2;--channel analog outputs
   -- b) SOFTWARE CONSTANTS (Don't change)
   AD_Code_Min   : constant := 0;    -- Constants needed by the user program
   AD_Code_Max   : constant := 4095; -- to convert Samples into real varia
   AD_Code_Range : constant := 4096; -- bles like Temperature, Voltage ...
   AD_Code_Byte_Max : constant := 255;--
   -----------------------------------------------------------------------
   -- 2.- DATA TYPES
   -----------------------------------------------------------------------
   --    b) ANALOG DATA
   type Sample_Type is range AD_Code_Min .. AD_Code_Max;----range supported by the sample
   type Sample_Byte_Type is range AD_Code_Min .. AD_Code_Byte_Max;--range supported by the register
   type Polar_Mode is (Unipolar,Bipolar);--Polarity
   type Voltage is new Float; --Voltage value
   type Voltage_Ac is access all Voltage;
   -----------------------------------------------------------------------
   -- 3.- IOCTL COMMANDS and ARGUMENTS
   -----------------------------------------------------------------------
   -- Control the device through a Generic IOCTL Posix_IO function.
   -----------------------------------------------------------------------
   --    a) COMMANDS
   -----------------------------------------------------------------------
   type Ao_Mode is
     (Configuration);
   -----------------------------------------------------------------------
   --    b) ARGUMENTS
   -----------------------------------------------------------------------
   type Ao_Mode_Synchronous is
     (Synchronous,Asynchronous);
   type Ao_Channels is
     (channel_1,channel_2,channel_all);
   type Ao_Ioctl_Arg is
      record
         Mode :Ao_Mode_Synchronous;
         Vref    : Voltage;
         Polarity     : Polar_Mode;
         Channel:Ao_Channels;
      end record;
private
   for Sample_Type'Size use 16; --Sample size
   for Sample_Byte_Type'Size use 8;--Register size
   for Ao_Mode'Size use Integer'Size;
end Pcm3712;
