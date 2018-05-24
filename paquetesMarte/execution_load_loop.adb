------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'E x e c u t i o n _ L o a d _ L o o p'
--
--                                 Body
--
--
--  File 'execution_load_loop.adb'
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
with Text_IO; use Text_IO;
with Calendar; use Calendar;

package body Execution_Load_Loop is

   type Number_Of_Loops is range 0..2_147_483_647;

   Loops_Per_Second : Number_Of_Loops := 168_341_328; --  3_234_878;

   To_Avoid_Optimization : Integer := 0;
   pragma Volatile (To_Avoid_Optimization);


   procedure Eat (For_Seconds : Duration) is
      End_Loop : Number_Of_Loops :=
                Number_Of_Loops(Float(Loops_Per_Second) * Float(For_Seconds));
      J        : Number_Of_Loops := 1;
   begin
      for I in 1 .. End_Loop loop
         J := J + 1;
         if J < I then
            J := I - J;
            To_Avoid_Optimization := 1;
         else
            J := J - I;
            To_Avoid_Optimization := - 1;
         end if;
      end loop;
   end Eat;

   procedure Non_Interactive_Adjust is
      Initial_Time,Final_Time : Time;
      Interval                : Duration;
      Number_Of_Tries         : Integer := 0;

      Adjust_Time   : constant Duration     := 0.2;
      Max_Tries     : constant Integer      := 6;
   begin
      loop
         Initial_Time := Clock;
         Eat(Adjust_Time);
         Final_Time := Clock;
         Interval := Final_Time - Initial_Time;
         if Interval = 0.0 then
            Loops_Per_Second := Loops_Per_Second * 10;
         else
            Loops_Per_Second :=
              Number_Of_Loops(Float(Loops_Per_Second)*
                              Float(Adjust_Time)/Float(Interval));
         end if;
         Number_Of_Tries:=Number_Of_Tries+1;
         exit when Number_Of_Tries>Max_Tries or
           abs(Interval-Adjust_Time)*100 < 1*Adjust_Time;
      end loop;
   end Non_Interactive_Adjust;

begin
   Put( "Execution_Load: Adjusting..." );
   Non_Interactive_Adjust;
   Put_Line (" (Loops per second:"
             & Number_Of_Loops'Image (Loops_Per_Second) & ")");
end Execution_Load_Loop;


