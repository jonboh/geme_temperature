------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--       'K e r n e l . T i m e d _ H a n d l e r s . I n t e r n a l s'
--
--                                 Body
--
--
--  File 'kernel-timed_handlers-internals.adb'                        By MAR.
--
--
--
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
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timed_Handlers.Internals is

   package TE_T renames K.Timed_Events_And_Timer;
   package DBG  renames K.Debug;

   -----------------------------
   --  Execute_Timed_Handler  --
   -----------------------------
   procedure Execute_Timed_Handler
     (TE_Ac                  : in Timed_Handler_TE_Ac;
      From_Timer_Int_Handler : in Boolean;
      Now                    : in HAL.HWTime := HAL.HWTime'Last) is
      use type HAL.HWTime;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return;
      end if;
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (TE_Ac.Magic = INITIALIZED));
      pragma Debug (DBG.Assert (not From_Timer_Int_Handler
        or else (TE_Ac.Armed and TE_Ac.Queued)));

      TE_Ac.Task_Where_Queued := null;
      --  Currently periodic CPU-based timed handlers aren't supported, so we
      --  can set to null this field

      --  Execute handler if enabled
      if TE_Ac.Enabled and Execution_Allowed then

         --  Reset pending flags
         TE_Ac.Pending_Enable := False;
         TE_Ac.Pending_Global := False;

         if (TE_Ac.Options and PERIODIC_HANDLER) /= 0 then
            --  Periodic
            pragma Assert (Now < HAL.HWTime'Last);

            --  Set new expiration time
            TE_Ac.T := TE_Ac.T + TE_Ac.Period;

            --  Is in the past the new expiration time?
            if TE_Ac.T < Now then
               --  the timed event was programmed for a time in the
               --  past (or the execution of the handler has been
               --  delayed for some other kernel activity)
               --
               --          Now excuting handler for this period
               --         +-----------------------------------+
               --         |                                   |
               --        \|/                    P := 10       |
               --                             /---------\
               --      ---|---------|---------|---------|---------|---
               --  time  100       110       120       130    |  140
               --                   |                        Now
               --                TE_Ac.T
               --
               --  In this example Overrun count is 3
               --  OC := (137-110) / 10 + 1 := 3
               --  TE_Ac.T := TE_Ac.T + P * OC := 110 + 10 * 3 := 140
               declare
                  OC : HAL.HWTime := (Now - TE_Ac.T) / TE_Ac.Period + 1;
               begin
                  if OC >= HAL.HWTime (Overrun_Count_T'Last) then
                     TE_Ac.Overrun_Count := Overrun_Count_T'Last;
                  else
                     TE_Ac.Overrun_Count := Overrun_Count_T (OC);
                  end if;
                  TE_Ac.T := TE_Ac.T + TE_Ac.Period * OC;
               end;
            else
               TE_Ac.Overrun_Count := 0;
            end if;

            --  Execute handler
            TE_Ac.Handler.all (TE_Ac.Area, TE_Ac);

            --  Reprogram timer
            if From_Timer_Int_Handler then
               pragma Debug (DBG.Assert (TE_Ac.Queued));
               TE_T.Enqueue_Standard_Event_Without_Updating_Timer
                 (K.Timed_Event_Ac (TE_Ac));
            else
               TE_Ac.Queued := True;
               TE_T.Enqueue_Standard_Event_And_Update_Timer
                 (K.Timed_Event_Ac (TE_Ac),
                  Now);
            end if;

         else  --  One-shot
            --  Not active any more
            TE_Ac.Armed := False;
            TE_Ac.Queued := False;

            --  Execute handler
            TE_Ac.Handler.all (TE_Ac.Area, TE_Ac);
         end if;

      elsif not TE_Ac.Enabled then
         --  Disabled
         --  The handler will be executed when reenabled from
         --  'Timed_Handlers.Operations.MaRTE_Timed_Handler_Enable'

         TE_Ac.Pending_Enable := True;
         TE_Ac.Pending_Global := False;
         TE_Ac.Armed := False;  --  the event has expired
         TE_Ac.Queued := False;

      elsif  not Execution_Allowed then
         --  Not execution allowed: enqueue pending handler

         TE_Ac.Pending_Global := True;
         TE_Ac.Pending_Enable := False;
         TE_Ac.Armed := False;  --  the event has expired
         TE_Ac.Queued := False;

         K.Timed_Events_Lists.Enqueue_Tail
           (K.Timed_Event_Ac (TE_Ac), Pending_Handlers_List);

      else
         --  Never should be reached this point

         pragma Debug (DBG.Assert (False));
         null;
      end if;

   end Execute_Timed_Handler;

end MaRTE.Kernel.Timed_Handlers.Internals;
