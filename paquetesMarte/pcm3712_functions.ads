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
--  File 'pcm3712_functions.ads'                                      By Barros, Puig-Peig
--
--  {MARTE_COPYRIGHT}
--
-------------------------------------------------------------------------------
with Drivers_Marte;
use Drivers_Marte;
package Pcm3712_Functions is

   --In/Out POSIX methods
   function Ao_Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int;

   function Ao_Close (
         Fd : in     File_Descriptor)
     return Int;

   function Ao_Write (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
   return Ssize_T;

  -- Method to transmit a request for the device
   function Ao_Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
     return Int;

end Pcm3712_Functions;
