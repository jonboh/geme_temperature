------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'C o n s o l e _ M a n a g e m e n t'
--
--                                 Spec
--
--
--  File 'console_management.ads'                                     By MAR.
--
--  This is the x86 architecture version of this package.
--
--  Some utility functions for the text console. They allow changing
--  the text attributes, positioning the cursor on the screen and
--  changing the console input configuration.
--
--  Operations in this package should NOT be used concurrently from
--  different tasks.
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
--                  --------------------------------
--  Thanks to Daniel Lange (University of Northern Iowa) for his help with
--  this package.
--
-------------------------------------------------------------------------------
package Console_Management is

   subtype Rows is Natural range 0 .. 24;

   subtype Columns is Natural range 0 .. 79;

   type Position is
     record
       Row    : Rows;
       Column : Columns;
     end record;
   pragma Convention (C, Position);

   -- Upper left corner of the screen
   UpLeft_Position   : constant Position := (Row => 0, Column => 0);
   -- Lower right corner of the screen
   DownRight_Position: constant Position := (Row    => Rows'Last,
                                             Column => Columns'Last);

   -- Text (foreground) Colors
   type Colors is
     (Black,
      Blue,
      Green,
      Cyan,
      Red,
      Magenta,
      Brown,
      LightGray,
      DarkGray,
      LightBlue,
      LightGreen,
      LightCyan,
      LightRed,
      LightMagenta,
      Yellow,
      White);

   -- Background colors
   type BkColors is
      (Black,
       Blue,
       Green,
       Cyan,
       Red,
       Magenta,
       Brown,
       LightGray);


   ----------------------------------------------------------------------------
   -- Change the Text Attributes ----------------------------------------------
   ----------------------------------------------------------------------------

   procedure Set_Text_Background_Color (BkC : in BkColors);
   pragma Export (C, Set_Text_Background_Color, "set_text_background_color");
   --  Establish backgroung color for the characters to be printed on
   --  screen.

   procedure Set_Text_Color (C : in Colors);
   pragma Export (C, Set_Text_Color, "set_text_color");
   --  Establish foregroung color for the characters to be printed on
   --  screen.

   procedure Set_HighVideo;
   pragma Export (C, Set_HighVideo, "set_highvideo");
   --  Establish high video for the characters to be printed on screen.

   procedure Set_LowVideo;
   pragma Export (C, Set_LowVideo, "set_lowvideo");
   --  Establish low video for the characters to be printed on screen.

   procedure Set_Blink;
   pragma Export (C, Set_Blink, "set_blink");
   --  Establish blinking mode for the characters to be printed on screen.

   procedure Cancel_Blink;
   pragma Export (C, Cancel_Blink, "cancel_blink");
   --  Establish non-blinking mode for the characters to be printed on screen.

   procedure Activate_Scroll;
   pragma Export (C, Activate_Scroll, "activate_scroll");
   --  Deactivates scroll mode for the characters to be printed on screen.

   procedure Deactivate_Scroll;
   pragma Export (C, Deactivate_Scroll, "deactivate_scroll");
   --  Deactivates scroll mode for the characters to be printed on screen.

   ---------------------------------------------------------------------------
   -- Position the cursor ----------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cursor (To : in Position);
   pragma Export (C, Set_Cursor, "set_cursor");
   pragma Export_Procedure (Internal => Set_Cursor, External => "set_cursor",
                            Mechanism => Reference);
   procedure Set_Cursor (Row : in Rows; Column : in Columns);
   --  Establish the position from where the characteres will be printed.

   procedure Get_Cursor (To : out Position);
   pragma Export (C, Get_Cursor, "get_cursor");
   pragma Export_Procedure (Internal => Get_Cursor, External => "get_cursor",
                            Mechanism => Reference);

   ---------------------------------------------------------------------------
   -- Clear the screen -------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Clear_Screen;
   pragma Export (C, Clear_Screen, "clrscr");
   --  The whole screen takes the active background color.


   ---------------------------------------------------------------------------
   -- Console input configuration --------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cooked_Mode;
   pragma Export (C, Set_Cooked_Mode, "set_cooked_mode");
   -- In this mode line editing is allowed, the textual unit of input is an
   -- entire "line" of text, where a "line" is a sequence of characters
   -- terminated by the line termination character CR. Thus, characters
   -- typed in this mode are not immediately made available to the calling
   -- program. They are first buffered to allow the user to perform line
   -- editing (erase characteres) on them.

   procedure Set_Raw_Mode;
   pragma Export (C, Set_Raw_Mode, "set_raw_mode");
   -- Every character is made available to the calling program as soon as
   -- it is typed, so no line editing is available in this mode.

   procedure Enable_Echo;
   pragma Export (C, Enable_Echo, "enable_echo");
   -- Echo input characters

   procedure Disable_Echo;
   pragma Export (C, Disable_Echo, "disable_echo");
   -- Input characters are not echoed

   procedure Set_Blocking_Mode;
   pragma Export (C, Set_Blocking_Mode, "set_blocking_mode");
   --  Default behaviour. Applications wait in 'getchar' until there
   --  are characters available.

   procedure Reset_Blocking_Mode;
   pragma Export (C, Reset_Blocking_Mode, "reset_blocking_mode");
   --  'getchar' returns -1 inmediately when there is no characters
   --  available at the moment of the call.

private
   --  Foreground colors
   for Colors use
     (Black        => 16#00#,
      Blue         => 16#01#,
      Green        => 16#02#,
      Cyan         => 16#03#,
      Red          => 16#04#,
      Magenta      => 16#05#,
      Brown        => 16#06#,
      LightGray    => 16#07#,
      DarkGray     => 16#08#,
      LightBlue    => 16#09#,
      LightGreen   => 16#0a#,
      LightCyan    => 16#0b#,
      LightRed     => 16#0c#,
      LightMagenta => 16#0d#,
      Yellow       => 16#0e#,
      White        => 16#0f#);
   for Colors'Size use 8;

   -- Background colors
   for BkColors use
      (Black        => 16#00#,
       Blue         => 16#10#,
       Green        => 16#20#,
       Cyan         => 16#30#,
       Red          => 16#40#,
       Magenta      => 16#50#,
       Brown        => 16#60#,
       LightGray    => 16#70#);
   for BkColors'Size use 8;
end Console_Management;
