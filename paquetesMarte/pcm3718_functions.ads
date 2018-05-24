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
--  File 'pcm3718_functions.ads'                           By Sangorrin
--
--  This is the main package of the driver for Advantech PCM-3718H Module.
--  In order to be able to use it, you should add the functions to
--  marte-kernel-devices_table.ads
--
--  User applications should not use this package directly but through the
--  Ada Text_IO interface or through the POSIX IO procedures located at
--  marte/posix5/posix-io.ads in case of Ada applications, and through the
--  posix interface (open, read ..) in case of C or C++ applications.
--
--  Check test_pcm3718_ai.adb[.c] and test_pcm3718_dio.adb[.c] for further
--  information.
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
with Drivers_Marte;
use Drivers_Marte;

package Pcm3718_Functions is

   -- 1.- Digital Input-Output (Dio) functions:

   function Dio_Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int;

   function Dio_Close (
         Fd : in     File_Descriptor)
     return Int;

   function Dio_Read (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T;

   function Dio_Write (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T;

   function Dio_Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
     return Int;

   -- 2.- Analog Input (Ai) functions:

   function Ai_Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int;

   function Ai_Close (
         Fd : in     File_Descriptor)
     return Int;

   function Ai_Read (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T;

   function Ai_Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
                      return Int;

end Pcm3718_Functions;
