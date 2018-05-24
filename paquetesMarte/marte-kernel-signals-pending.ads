------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . S i g n a l s . P e n d i n g'
--
--                                 Spec
--
--
--  File 'k-signals-pending.ads'                                       By MAR.
--
--
--  Pending signal instances management.
--  Queue of signal instances to execute signal handlers.
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

with MaRTE.Kernel.Signals.Global; use MaRTE.Kernel.Signals.Global;

package MaRTE.Kernel.Signals.Pending is

   package K renames MaRTE.Kernel;

   procedure Discard_Pending_Instances (Sig : in Signal);
   --  Release the resources of all the pending instances with signo 'Sig'
   --  on all the task and on the process.

   function Is_Signal_Pending (Sig     : in Signal;
                               Sigdata : in Sig_Data_Ac) return Boolean;
   --  Some signal instance with signo 'Sig' is pending either on the
   --  process or on the task owning 'Sigdata'.

   function First_Signal_Instance_Accepted (Sig     : in Signal;
                                            Sigdata : in Sig_Data_Ac)
                                            return Signal_Instance_Ac;
   --  Returns the first signal instance with signo 'Sig' pending on the
   --  task or else on the process. Returns null if no signal instance
   --  pending. The signal instance is accepted, that is, it is removed from
   --  the pending list.

   function Accept_All_Pending_Signal_Instances
     (Sigdata_Ac : in Sig_Data_Ac;
      Set        : in Signal_Set_Ac_Const)
      return Global.Signal_Instances_Lists.List;
   --  Return all the pending signals for the task owner of 'Sigdata_Ac'
   --  (signals pending on task and on the process). They are returned in
   --  an ordered list, low signal numbers before and for the same signal
   --  number signals pending on task before that signals pending on
   --  process. The signal instances are accepted, that is, they are
   --  removed from their pending list.

   procedure Signal_Instance_Generated_For_Process
     (Siginst_Ac : in Signal_Instance_Ac);
   --  'Siginst' is queued in the "pending on process" list (at the
   --  tail of the pending signals queue).

   procedure Signal_Instance_Generated_For_Task
     (Siginst_Ac : in Signal_Instance_Ac);
   --  'Siginst_Ac' is queued as pending in 'Siginst_Ac.To_Sigdata' (at the
   --  tail of the pending signals queue).

   function Extract_Head_Instace_From_Handler_Q return Signal_Instance_Ac;
   --  Returns the first instance from the queue of instances to execute its
   --  handler. Releases the resources taken by this instance. In this
   --  queue the instances are ordered by signo (lower signo first) and
   --  FIFO for instances with the same signo.

   procedure Add_Signal_Instaces_To_Handler_Q (Sig        : in Signal;
                                               Sigdata_Ac : in Sig_Data_Ac);
   --  Enqueue in the handler queue all the signal instances with signo
   --  'Sig' pending either on the process or on the task owning 'Sigdata'.

end MaRTE.Kernel.Signals.Pending;
