------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'K e y b o a r d _ F u n c t i o n s'
--
--                                   Spec
--
--
--  File 'keyboard_functions.ads'                                     By MAR.
--
--  Driver for the PC keyboard.
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
with MaRTE.Integer_Types;

package Keyboard_Functions is

   function Create return Int;

   --  No 'Remove' function defined

   --  No 'Open' function defined

   --  No 'Close' function defined

   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T;

   --  No 'Write' function defined

   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int;

   ---------------------------
   --  Direct read function --
   ---------------------------
   function Direct_Read return MaRTE.Integer_Types.Unsigned_8;

end Keyboard_Functions;
