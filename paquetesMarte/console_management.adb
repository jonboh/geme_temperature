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
--  File 'console_management.ads'                                      By MAR.
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
with Ada.Unchecked_Conversion;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with Keyboard;
with Keyboard_Functions;
with Text_Console_Ioctl;
with Drivers_MaRTE;

package body Console_Management is

   function To_U8 is new Ada.Unchecked_Conversion (Source => Colors,
                                                   Target => Unsigned_8);
   function To_U8 is new Ada.Unchecked_Conversion (Source => BkColors,
                                                   Target => Unsigned_8);

   ----------------------------------------------------------------------------
   -- Function Ioctl of the text console --------------------------------------
   ----------------------------------------------------------------------------
   function Ioctl_Text_Console
     (Fd             : in     Drivers_MaRTE.File_Descriptor;
      Request        : in     Drivers_MaRTE.Ioctl_Option_Value;
      Ioctl_Data     : access Text_Console_Ioctl.Ioctl_Data)
     return Int;
   pragma Import (C, Ioctl_Text_Console, "text_console_ioctl");

   Ret : Int;

   ----------------------------------------------------------------------------
   -- Text Attributes ---------------------------------------------------------
   ----------------------------------------------------------------------------
   Data : aliased Text_Console_Ioctl.Ioctl_Data :=
     (Position  => 0,
      Text_Attr => To_U8 (Colors'(White)) or To_U8 (BkColors'(Black)),
      Char      => ' ');
   --  Current text attributes are stored in this object.

   BINKING_BIT    : constant Unsigned_8 := 16#80#;
   HIGH_VIDEO_BIT : constant Unsigned_8 := 16#08#;

   FG_Color_Mask : constant Unsigned_8 := not 16#0f#;
   BG_Color_Mask : constant Unsigned_8 := not 16#70#;

   -------------------------------
   -- Set_Text_Background_Color --
   -------------------------------
   procedure Set_Text_Background_Color (BkC : in BkColors) is
   begin
      Data.Text_Attr := (Data.Text_Attr and BG_Color_Mask) or To_U8 (BkC);
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Set_Text_Background_Color;

   --------------------
   -- Set_Text_Color --
   --------------------
   procedure Set_Text_Color (C : in Colors) is
   begin
      Data.Text_Attr := (Data.Text_Attr and FG_Color_Mask) or To_U8 (C);
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Set_Text_Color;

   -------------------
   -- Set_HighVideo --
   -------------------
   procedure Set_HighVideo is
   begin
      Data.Text_Attr := Data.Text_Attr or HIGH_VIDEO_BIT;
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Set_HighVideo;

   ------------------
   -- Set_LowVideo --
   ------------------
   procedure Set_LowVideo is
   begin
      Data.Text_Attr := Data.Text_Attr and not HIGH_VIDEO_BIT;
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Set_LowVideo;

   ---------------
   -- Set_Blink --
   ---------------
   procedure Set_Blink is
   begin
      Data.Text_Attr := Data.Text_Attr or BINKING_BIT;
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Set_Blink;

   ------------------
   -- Cancel_Blink --
   ------------------
   procedure Cancel_Blink is
   begin
      Data.Text_Attr := Data.Text_Attr and not BINKING_BIT;
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_TEXT_ATTR,
                                 Data'Access);
   end Cancel_Blink;

   ---------------------
   -- Activate_Scroll --
   ---------------------
   procedure Activate_Scroll is
   begin
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.ACTIVATE_SCROLL_MODE,
                                 null);
   end Activate_Scroll;

   -----------------------
   -- Deactivate_Scroll --
   -----------------------
   procedure Deactivate_Scroll is
   begin
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.DEACTIVATE_SCROLL_MODE,
                                 null);
   end Deactivate_Scroll;

   ---------------------------------------------------------------------------
   -- Position the cursor ----------------------------------------------------
   ---------------------------------------------------------------------------

   ----------------
   -- Set_Cursor --
   ----------------
   procedure Set_Cursor (To : in Position) is
   begin
      Data.Position := Unsigned(To.Row * 80 + To.Column);
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_CURSOR_POS,
                                 Data'Access);
   end Set_Cursor;

   ----------------
   -- Set_Cursor --
   ----------------
   procedure Set_Cursor (Row : in Rows; Column : in Columns) is
   begin
      Data.Position := Unsigned(Row * 80 + Column);
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.SET_CURSOR_POS,
                                 Data'Access);
   end Set_Cursor;

   ----------------
   -- Get_Cursor --
   ----------------
   procedure Get_Cursor (To : out Position) is
   begin
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.GET_CURSOR_POS,
                                 Data'Access);
      To.Row    := Natural(Data.Position / 80);
      To.Column := Natural(Data.Position mod 80);
   end Get_Cursor;

   ---------------------------------------------------------------------------
   -- Console input configuration --------------------------------------------
   ---------------------------------------------------------------------------

   ---------------------
   -- Set_Cooked_Mode --
   ---------------------
   procedure Set_Cooked_Mode is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Set_Cooked_Mode), null);
   end Set_Cooked_Mode;

   ------------------
   -- Set_Raw_Mode --
   ------------------
   procedure Set_Raw_Mode is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Set_Raw_Mode), null);
   end Set_Raw_Mode;

   -----------------
   -- Enable_Echo --
   -----------------
   procedure Enable_Echo is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Enable_Echo), null);
   end Enable_Echo;

   ------------------
   -- Disable_Echo --
   ------------------
   procedure Disable_Echo is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Disable_Echo), null);
   end Disable_Echo;

   -------------------------
   --  Set_Blocking_Mode  --
   -------------------------
   procedure Set_Blocking_Mode is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Set_Blocking_Mode),
         null);
   end Set_Blocking_Mode;

   ---------------------------
   --  Reset_Blocking_Mode  --
   ---------------------------
   procedure Reset_Blocking_Mode is
   begin
      Ret := Keyboard_Functions.Ioctl
        (0, Keyboard.Ioctl_Commads'Enum_Rep (Keyboard.Reset_Blocking_Mode),
         null);
   end Reset_Blocking_Mode;



   ----------------------------------------------------------------------------
   -- Clear_Screen ------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Clear_Screen is
      Data_Not_Used : aliased Text_Console_Ioctl.Ioctl_Data;
   begin
      Ret := Ioctl_Text_Console (1,
                                 Text_Console_Ioctl.CLRSCR,
                                 Data_Not_Used'Access);
   end Clear_Screen;

end Console_Management;
