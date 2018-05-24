------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'S e r i a l _ P o r t _ D r i v e r _ I m p o r t'
--
--                                    Body
--
--
--  File 'serial_port_driver_import.adb'                              By MAR
--
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

with Ring_Buffers;

package body Serial_Port_Driver_Import is

   --  Ring buffers used to share data between the interrupt handler
   --  and the read and write functions (they are created here but
   --  used in 'serial_port_driver_c.c')
   Com1_Inbuf  : Ring_Buffers.Input_Buffer (Size_Mx => 100);
   Com1_Outbuf : Ring_Buffers.Output_Buffer (Size_Mx => 100);
   pragma Export (C, Com1_Inbuf, "com1_inbuf");
   pragma Export (C, Com1_Outbuf, "com1_outbuf");

   Com2_Inbuf  : Ring_Buffers.Input_Buffer (Size_Mx => 100);
   Com2_Outbuf : Ring_Buffers.Output_Buffer (Size_Mx => 100);
   pragma Export (C, Com2_Inbuf, "com2_inbuf");
   pragma Export (C, Com2_Outbuf, "com2_outbuf");

   Com3_Inbuf  : Ring_Buffers.Input_Buffer (Size_Mx => 100);
   Com3_Outbuf : Ring_Buffers.Output_Buffer (Size_Mx => 100);
   pragma Export (C, Com3_Inbuf, "com3_inbuf");
   pragma Export (C, Com3_Outbuf, "com3_outbuf");

   Com4_Inbuf  : Ring_Buffers.Input_Buffer (Size_Mx => 100);
   Com4_Outbuf : Ring_Buffers.Output_Buffer (Size_Mx => 100);
   pragma Export (C, Com4_Inbuf, "com4_inbuf");
   pragma Export (C, Com4_Outbuf, "com4_outbuf");

   --  Create
   --
   --  It could be just a rename function in the ads. It is here to be
   --  sure body is elaborated (and consequently buffers are created)
   --  before calling "serial_port_create"
   function Create return Int is
      function Create_C return Int;
      pragma Import (C, Create_C, "serial_port_create");
      -- defined in 'serial_port_driver_c.c'
   begin
      return Create_C;
   end Create;

end Serial_Port_Driver_Import;
