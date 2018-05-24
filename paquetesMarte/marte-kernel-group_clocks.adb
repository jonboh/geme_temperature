------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                 'K e r n e l . G r o u p _ C l o c k s'
--
--                                 Body
--
--
--  File 'kernel-group_clocks.adb'                                    By MAR.
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
with MaRTE.Kernel.Timed_Handlers;

package body MaRTE.Kernel.Group_Clocks is

   package TH renames K.Timed_Handlers;

   use type K.Task_Id, MaRTE.Kernel.CPU_Time_Timed_Event_Ac, TH.Magic_Check;

   -------------------
   --  Group_TE_OK  --
   -------------------
   function Group_TE_OK
     (Group_TE_Ac : K.CPU_Time_Timed_Event_Ac)
      return Boolean is
   begin
      return Group_TE_Ac /= null
        and then Group_TE_Ac.Is_Based_On_Group_Clock

        and then
          (Group_TE_Ac.Task_Where_Queued = null
           or else K.Task_OK (K.Task_Id (Group_TE_Ac.Task_Where_Queued),
                              Extra_Magic => K.TERMINATED))
        --  not queued in any task or queued in a valid task. The task could be
        --  terminated when it is leaving the CPU for last time

        and then
          (not (Group_TE_Ac.all in TH.Timed_Handler_Timed_Event'Class)
           or else TH.Timed_Handler_Timed_Event (Group_TE_Ac.all).Magic =
             TH.INITIALIZED);
   end Group_TE_OK;

end MaRTE.Kernel.Group_Clocks;
