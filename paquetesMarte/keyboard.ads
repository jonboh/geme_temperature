------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                            'K e y b o a r d'
--
--                                   Spec
--
--
--  File 'keyboard.ads'                                               By MAR.
--
--  'Ioctl' commands for the PC keyboard driver.
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

package Keyboard is

   ------------------------
   --  'Ioctl' Commands  --
   ------------------------
   type Ioctl_Commads is
     (Set_Cooked_Mode,
      --  "cooked mode": line editing is allowed, the textual unit of
      --  input in this mode is an entire "line" of text, where a
      --  "line" is a sequence of characters terminated by the line
      --  termination character CR. Thus, characters typed in this
      --  mode are not immediately made available to the calling
      --  program. They are first buffered to allow the user to
      --  perform line editing (erase characteres) on them.

      Set_Raw_Mode,
      --  "raw mode": Every character is made available to the calling
      --  program as soon as it is typed, so no line editing is
      --  available in this mode.

      Enable_Echo,
      --  Characters are echoed to the console as they are typed.

      Disable_Echo,
      --  Characters are not echoed to the console.

      Set_Blocking_Mode,
      --  Tasks are blocked in case there are not enough characters in
      --  buffer to fulfill a read operation.

      Reset_Blocking_Mode,
      --  Read operations returns immediately the available characters
      --  (maybe none).

      Save_Mode,
      --  Save the current mode (Cooked or raw, echo and blocking)

      Restore_Mode
      --  Restore the current mode
      );
   for Ioctl_Commads use (Set_Cooked_Mode     => 150,
                          Set_Raw_Mode        => 151,
                          Enable_Echo         => 152,
                          Disable_Echo        => 153,
                          Set_Blocking_Mode   => 154,
                          Reset_Blocking_Mode => 155,
                          Save_Mode           => 156,
                          Restore_Mode        => 157);

   ------------------
   --  Ioctl Data  --
   ------------------
   type Stored_Mode is mod 2 ** 8;
   for Stored_Mode'Size use 8;
   --  Used with 'Save_Mode' and 'Restore_Mode'.

   Cooked_Mask   : constant := 16#80#;
   Echo_Mask     : constant := 16#10#;
   Blocking_Mask : constant := 16#01#;

end Keyboard;
