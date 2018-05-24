------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . S c h e d u l e r . D e b u g'
--
--                                 Body
--
--
--  File 'k-scheduler-debug.adb'                                       By MAR.
--
--
--  Scheduler debugging.
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
with MaRTE.Direct_IO;
with MaRTE.Stacks_Management; -- To perform Stack overflow checks
pragma Elaborate_All (MaRTE.Stacks_Management);
with MaRTE.Configuration_Parameters;

package body MaRTE.Kernel.Scheduler.Debug is

   -----------------------
   -- Assert_Task_Is_Ok --
   -----------------------
   Invalid_Task_Detected : Boolean := False;
   procedure Assert_Task_Is_Ok (T : in Task_Id) is
   begin
      if not Invalid_Task_Detected then -- Only interrupt the program once
         if not Task_OK (T, TERMINATED) then
            Invalid_Task_Detected := True;
            MaRTE.Direct_IO.Error ("#11#", Fatal => True);
         end if;
         if not MaRTE.Stacks_Management.Stack_OK (HAL.Get_Stack_Pointer_Register,
                                            T.Stack) then
            Invalid_Task_Detected := True;
            MaRTE.Direct_IO.Error ("STACK OVERFLOW", Fatal => True);
         end if;
      end if;
   end Assert_Task_Is_Ok;

   -------------------------
   -- Assert_Tasks_Are_OK --
   -------------------------
   procedure Assert_Tasks_Are_OK (Heir, Old : in Task_Id) is
      use K.Tasks_Lists;
      use type K.Magic_Check;
   begin
      if not Task_OK (Old, TERMINATED) then
         Invalid_Task_Detected := True;
         --  Invalid Task detected
         MaRTE.Direct_IO.Error ("#22#", Fatal => True);
      end if;

      if not Task_OK (Heir) then
         Invalid_Task_Detected := True;
         --  Invalid Task detected
         MaRTE.Direct_IO.Error ("#23#", Fatal => True);
      end if;

      if Heir.Status /= READY then
         --  Heir task not ready
         MaRTE.Direct_IO.Error ("#24#", Fatal => True);
      end if;

      if not MaRTE.Stacks_Management.Stack_OK (HAL.Get_Stack_Pointer_Register,
                                         Old.Stack) then
         --  Wrong stack detected (probably a stack overflow error)
         MaRTE.Direct_IO.Error ("Stack overflow (Old)", Fatal => True);
      end if;

      if not MaRTE.Stacks_Management.Stack_OK (Heir.Stack_Ptr,
                                         Heir.Stack) then
         --  Wrong stack detected (probably a stack overflow error)
         MaRTE.Direct_IO.Error ("Stack overflow (New)", Fatal => True);
      end if;
   end Assert_Tasks_Are_OK;

--     Allowed_Delay : constant HWI.HWTime := 150;
--     --  If a timed event expires with a delay longer than this value a
--     --  internal error is shown. (150 without optimization)

--     -- Assert_Expiration_Time_Looks_OK --
--     -------------------------------------
--     procedure Check_Expiration_Time (E   : in Kernel.Timed_Event_Ac;
--                                      Now : in HWI.HWTime);
--     procedure Check_Expiration_Time (E   : in Kernel.Timed_Event_Ac;
--                                      Now : in HWI.HWTime) is
--     begin

--        if (Now < E.T
--         - Kernel.Suspension_Time_Minimum) then
--           Kernel_Console.Error ("Timed event treated too early.");
--           --  Asm ("int $3");  -- Breakpoint interrupt
--           Kernel_Console.Error (" Advance: ", Integer (E.T - Now));
--        end if;
--        if Now > E.T + Allowed_Delay then
--           Kernel_Console.Error ("Timed event treated too late. Delay: ",
--                            Integer (Now - E.T));
--           --  Asm ("int $3");  -- Breakpoint interrupt
--        end if;
--   end Check_Expiration_Time;


end MaRTE.Kernel.Scheduler.Debug;
