------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                         'D e b u g _ M a r t e'
--
--                                 Body
--
--
--  File 'debug_marte.adb'                                            By MAR.
--
--
--  Debug operations.
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
with System.Machine_Code; use System.Machine_Code;

package body Debug_Marte is

   procedure Gdb_Pc_Com_Init (Com_Port : in Int; Com_Params : in Int);
   pragma Import (C, Gdb_Pc_Com_Init, "gdb_pc_com_init");

   ----------------------------------------
   -- Init_Serial_Communication_With_Gdb --
   ----------------------------------------
   procedure Init_Serial_Communication_With_Gdb (Com_Port : in Int) is
   begin
      Gdb_Pc_Com_Init (Com_Port, 0);
   end Init_Serial_Communication_With_Gdb;

   --------------------------
   -- Set_Break_Point_Here --
   --------------------------
   procedure Set_Break_Point_Here is
   begin
      Asm ("int $3");
   end Set_Break_Point_Here;

end Debug_Marte;
