------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'S e r i a l _ P o r t _ D r i v e r _ I m p o r t'
--
--                                    Spec
--
--
--  File 'serial_port_driver_import.ads'                        By Fguerreira
--
--
--  Ada package importing the serial port driver functions actually
--  defined in 'serial_port_driver_c.c'.
--
--  In this package are created the ring BUFFERS used to share data
--  between the interrupt handler and the read and write functions. To
--  modify the size of this buffers just change the value assigned to
--  'Size_Mx' for each buffer.
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

with Drivers_MaRTE;  use Drivers_MaRTE;
with System, Ada.Unchecked_Conversion;

package Serial_Port_Driver_Import is

   --  Create
   function Create return Int;
   function Address_To_Create_Ac is
      new Ada.Unchecked_Conversion (System.Address, Create_Function_Ac);
   Create_Ac : Create_Function_Ac := Address_To_Create_Ac (Create'Address);

   --  Remove
   Remove_Ac : Remove_Function_Ac := null;

   --  Open
   function Open (Fd   : in File_Descriptor;
                  Mode : in File_Access_Mode) return Int;
   pragma Import (C, Open, "serial_port_open");
   function Address_To_Open_Ac is
      new Ada.Unchecked_Conversion (System.Address, Open_Function_Ac);
   Open_Ac : Open_Function_Ac := Address_To_Open_Ac (Open'Address);

   --  Close
   Close_Ac : Close_Function_Ac := null;

   --  Read
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Read, "serial_port_read");
   function Address_To_Read_Ac is
      new Ada.Unchecked_Conversion (System.Address, Read_Function_Ac);
   Read_Ac : Read_Function_Ac := Address_To_Read_Ac (Read'Address);

   --  Write
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Write, "serial_port_write");
   function Address_To_Write_Ac is
      new Ada.Unchecked_Conversion (System.Address, Write_Function_Ac);
   Write_Ac : Write_Function_Ac := Address_To_Write_Ac (Write'Address);

   --  Ioctl
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int;
   pragma Import (C, Ioctl, "serial_port_ioctl");
   function Address_To_Ioctl_Ac is
      new Ada.Unchecked_Conversion (System.Address, Ioctl_Function_Ac);
   Ioctl_Ac : Ioctl_Function_Ac := Address_To_Ioctl_Ac (Ioctl'Address);

end Serial_Port_Driver_Import;
