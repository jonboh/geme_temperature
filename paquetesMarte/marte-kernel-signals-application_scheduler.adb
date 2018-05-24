------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--  'K e r n e l . S i g n a l s . A p p l i c a t i o n _ S c h e d u l e r'
--
--                                 Body
--
--
--  File 'k-signals-appsched.adb'                                      By MAR.
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

with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Signals.Pending;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Signals.Debug;

package body MaRTE.Kernel.Signals.Application_Scheduler is

   package SP renames K.Signals.Pending;
   package SI renames  K.Signals.Internals;
   package SILST renames Global.Signal_Instances_Lists;
   package SCHD renames K.Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package APPSCHD_EVENTS_LST renames APPSCHD_DATA.AppSched_Events_Lists;
   use K.Signals;

   function UC (Base : K.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_DATA.UC_To_AppScheduler_Data_Ac;

   -----------------------------------------------
   -- Convert_Pending_Signals_Into_Sched_Events --
   -----------------------------------------------
   procedure Convert_Pending_Signals_Into_Sched_Events
     (Set : in Signal_Set_Ac_Const) is

      Signals_Pending : Global.Signal_Instances_Lists.List;
      Siginst_Ac      : Global.Signal_Instance_Ac;
      Siginst_Old_Ac  : Global.Signal_Instance_Ac;
      Self_Sigdata_Ac : Global.Sig_Data_Ac :=
        Global.Sig_Data_Ac (SCHD.Self.Sig_Data);
      E : APPSCHD_DATA.AppSched_Event_Ac;
   begin
      pragma Assert (SCHD.Self.AppScheduler /= null);

      --  Get all the pending signals
      Signals_Pending :=
        SP.Accept_All_Pending_Signal_Instances (Self_Sigdata_Ac, Set);

      --  Convert each signal into an "appsched" event
      Siginst_Ac := SILST.Head (Signals_Pending);
      while SILST."/=" (Siginst_Ac, null) loop
         --  Convert pending signals into events
         E := APPSCHD_DATA.Request_AppSched_Event;
         E.Event_Code := APPSCHD_DATA.APPSCHED_SIGNAL;
         E.Siginfo    := (Signo => Siginst_Ac.Signo,
                          Code  => Siginst_Ac.Code,
                          Value => Siginst_Ac.Value);
         --  Enqueue the event
         APPSCHD_EVENTS_LST.Enqueue_Tail
           (E, UC (SCHD.Self.AppScheduler).Events_Q);

         --  Next signal instance
         Siginst_Old_Ac := Siginst_Ac;
         Siginst_Ac := SILST.Next (Siginst_Ac);

         --  Release the signal instance
         SI.Release_Signal_Instance (Siginst_Old_Ac);
      end loop;
   end Convert_Pending_Signals_Into_Sched_Events;

end MaRTE.Kernel.Signals.Application_Scheduler;
