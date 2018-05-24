------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . T a s k _ S e t s'
--
--                                 Spec
--
--
--  File 'kernel-task_sets.ads'                                    By MAR.
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
with MaRTE.Kernel.Group_Clocks;

package body MaRTE.Kernel.Task_Sets is

   package GCLK renames K.Group_Clocks;

   use type K.Task_Set_Id, MaRTE.Kernel.CPU_Time_Timed_Event_Ac,
       K.Magic_Check_TS;

   -------------------
   --  Task_Set_OK  --
   -------------------

   function Task_Set_OK (TS : K.Task_Set_Base_Ac) return Boolean is
   begin
      return TS /= null
        and then TS.Magic = K.TS_INITIALIZED
        and then (TS.Group_TE_Ac = null or else
                  GCLK.Group_TE_OK (TS.Group_TE_Ac));
   end Task_Set_OK;

end MaRTE.Kernel.Task_Sets;
