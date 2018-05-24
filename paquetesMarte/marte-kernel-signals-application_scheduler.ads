------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--  'K e r n e l . S i g n a l s . A p p l i c a t i o n _ S c h e d u l e r'
--
--                                 Spec
--
--
--  File 'k-signals-appsched.ads'                                      By MAR.
--
--
--  Operations used by application schedulers to manage signals.
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

with MaRTE.Kernel.Application_Scheduling_Data;

package MaRTE.Kernel.Signals.Application_Scheduler is

   package K renames MaRTE.Kernel;

   -----------------------------------------------
   -- Convert_Pending_Signals_Into_Sched_Events --
   -----------------------------------------------
   --
   --  Convert all the pending signal instances of signals in set (for the
   --  running task) in scheduling events that are enqueued in its events
   --  queue. The running task must be an application scheduler.
   procedure Convert_Pending_Signals_Into_Sched_Events
     (Set : in Signal_Set_Ac_Const);
   pragma Inline (Convert_Pending_Signals_Into_Sched_Events);

end MaRTE.Kernel.Signals.Application_Scheduler;
