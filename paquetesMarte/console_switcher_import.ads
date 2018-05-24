------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                          'Console_Switcher_Import'
--
--                                    Spec
--
--
--  File 'Console_Switcher_Import.ads'                        By MAR.
--
--
--  Driver that allows switching between different consoles (text, serial,
--  membuffer,..).
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
with Serial_Port_Driver_Import;

package Console_Switcher_Import is

   --  Create
   function Create return Int;
   pragma Import (C, Create, "console_switcher_create");
   function Address_To_Create_Ac is
      new Ada.Unchecked_Conversion (System.Address, Create_Function_Ac);
   Create_Ac : Create_Function_Ac := Address_To_Create_Ac (Create'Address);

   --  Remove
   Remove_Ac : Remove_Function_Ac := null;

   --  Open
   Open_Ac : Open_Function_Ac := null;

   --  Close
   Close_Ac : Close_Function_Ac := null;

   --  Read
   Read_Ac : Read_Function_Ac := null;

   --  Write
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Write, "console_switcher_write");
   function Address_To_Write_Ac is
      new Ada.Unchecked_Conversion (System.Address, Write_Function_Ac);
   Write_Ac : Write_Function_Ac := Address_To_Write_Ac (Write'Address);

   --  Write_Error
   function Write_Error (Fd         : in File_Descriptor;
                         Buffer_Ptr : in Buffer_Ac;
                         Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Write_Error, "console_switcher_write_error");
   Write_Error_Ac : Write_Function_Ac :=
     Address_To_Write_Ac (Write_Error'Address);

   --  Ioctl
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int;
   pragma Import (C, Ioctl, "console_switcher_ioctl");
   function Address_To_Ioctl_Ac is
      new Ada.Unchecked_Conversion (System.Address, Ioctl_Function_Ac);
   Ioctl_Ac : Ioctl_Function_Ac := Address_To_Ioctl_Ac (Ioctl'Address);

end Console_Switcher_Import;
