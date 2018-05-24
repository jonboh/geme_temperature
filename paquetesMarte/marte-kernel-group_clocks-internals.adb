------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--        'K e r n e l . G r o u p _ C l o c k s . I n t e r n a l s'
--
--                                 Body
--
--  File 'kernel-group_clocks-internals.adb'                          By MAR.
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
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Group_Clocks.Internals is

   package GCLK         renames K.Group_Clocks;
   package CPU_TE_Lists renames K.CPU_Time_Timed_Events_Lists;
   package DBG          renames K.Debug;

   ----------------
   --  Del_Task  --
   ----------------
   procedure Del_Task (T           : K.Task_Id;
                       Group_TE_Ac : K.CPU_Time_Timed_Event_Ac) is
   begin
      if not MaRTE.Configuration_Parameters.Use_Group_Clocks'First then
         null;
      end if;

      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (K.Task_OK (T, K.TERMINATED)));
      pragma Debug (DBG.Assert (GCLK.Group_TE_OK (Group_TE_Ac)));

      --  Is the event queued in T?
      if Group_TE_Ac.Task_Where_Queued = K.TCB_Ac (T) then
         --  Remove event from queue
         pragma Debug
           (DBG.Assert (CPU_TE_Lists.Is_In_The_List (Group_TE_Ac,
                                                     T.CPU_Time_TEs_Q)));
         CPU_TE_Lists.Dequeue (Group_TE_Ac, T.CPU_Time_TEs_Q);
         Group_TE_Ac.Task_Where_Queued := null;
      end if;
   end Del_Task;

end MaRTE.Kernel.Group_Clocks.Internals;
