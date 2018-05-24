------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                           'Console_Switcher'
--
--                                 Spec
--
--
--  File 'console_switcher.ads'                                         By MAR.
--
--  Allows programs to change the output system console between the
--  monitor, the serial line and membuffer.
--
--  Only works when driver Console_Switcher is installed as
--  "Standard Output" (see file 'kernel-devices_table.ads').
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

package Console_Switcher is

   ---------------------------
   --  Serial_Console_Init  --
   ---------------------------
   --  Redirect standard output to the serial line.  If serial port
   --  configuration is not modified, the first serial port (com1) is
   --  used and line speed is set to 115200 Bauds.
   --
   --  The rest of the serial line configuration parameters are in
   --  'marte/drivers/serial_port/serial_port_driver_c.c' (see
   --  variable 'raw_termios').
   procedure Serial_Console_Init;
   pragma Import (C, Serial_Console_Init, "SERIAL_CONSOLE_INIT");

   -----------------------------
   --  Standard_Console_Init  --
   -----------------------------
   --  Redirect standard output to the console.
   procedure Standard_Console_Init;
   pragma Import (C, Standard_Console_Init, "STANDARD_CONSOLE_INIT");

end Console_Switcher;
