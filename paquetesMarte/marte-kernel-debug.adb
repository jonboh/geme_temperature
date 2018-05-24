------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'K e r n e l . D e b u g'
--
--                                 Body
--
--
--  File 'k-debug.adb'                                                 By MAR.
--
--
--  General debugging functions.
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

with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with GNAT.Source_Info;

package body MaRTE.Kernel.Debug is

   use Timed_Events_Lists;

   Initialized : Boolean := False;

   --------------------------------
   -- Short_Show_Task_On_Console --
   --------------------------------
   procedure Short_Show_Task_On_Console (T : in Tasks_Lists.Element_Ac) is
   begin
      Put ("("); Put (T.Id);
      Put (","); Put (Integer (T.Active_Prio));
      --  Put (","); Put (T.all'Address);
      Put (")");
   end Short_Show_Task_On_Console;

   ------------------
   -- Show_In_Secs --
   ------------------
   Initial_HWTime : HAL.HWTime;
   procedure Show_In_Secs (Hwt : in HAL.HWTime;
                           Ref : in Relative_Absolute_T := REL) is
      Tmp : Unsigned_64 := Unsigned_64 (Hwt);
      HWClock_Freq : constant Unsigned_64 :=
        Unsigned_64 (HAL.Get_HWClock_Frequency);
   begin
      Assert (Initialized);

      if Ref = REL then
         Tmp := Tmp - Unsigned_64 (Initial_HWTime);
      end if;
      Put (Tmp / HWClock_Freq); Put ("s");
      Tmp := (Tmp - (Tmp / HWClock_Freq) * HWClock_Freq) * 1_000;
      Put (Tmp / HWClock_Freq); Put ("m");
      Tmp := (Tmp - (Tmp / HWClock_Freq) * HWClock_Freq) * 1_000;
      Put (Tmp / HWClock_Freq); Put ("u");
      Tmp := (Tmp - (Tmp / HWClock_Freq) * HWClock_Freq) * 1_000;
      Put (Tmp / HWClock_Freq); Put ("n");
   end Show_In_Secs;

   --------------
   --  Assert  --
   --------------
   --  To be used instead of pragma assert.
   Str_Assertion_Failed : constant String :=
     Standard.ASCII.CR & "MaRTE Assertion failed:";
   procedure Assert
     (Condition : in Boolean;
      Msg       : in String := GNAT.Source_Info.Source_Location) is
   begin
      if not Condition then
         MaRTE.Direct_IO.Put_Error (Str_Assertion_Failed);
         MaRTE.Direct_IO.Put_Error (Msg, Fatal => True);
      end if;
   end Assert;
   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      Assert (not Initialized);
      Initialized := True;

      Initial_HWTime := HAL.Get_HWTime;
   end Initialize;

end MaRTE.Kernel.Debug;
