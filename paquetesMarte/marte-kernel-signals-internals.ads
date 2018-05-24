------------------------------------------------------------------------------
-- -------------------         M a R T E  O S          -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . S i g n a l s . I n t e r n a l s'
--
--                                 Spec
--
--
--  File 'k-signals-internals.ads'                                    By MAR.
--
--  Signal related types and procedures not included in the POSIX
--  standard. Only for internal use inside the herarchy of 'Scheduler'.
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
with MaRTE.Kernel.Signals.Global;

package MaRTE.Kernel.Signals.Internals is

   package K renames MaRTE.Kernel;

   function Request_Signal_Instance return Global.Signal_Instance_Ac;

   function Request_And_Reserve_Signal_Instance
     return Global.Signal_Instance_Ac;

   procedure Release_Signal_Instance
     (Siginst_Ac : in Global.Signal_Instance_Ac);

   procedure Cancel_Reserve_And_Release_Signal_Instance
     (Siginst_Ac : in Global.Signal_Instance_Ac);

   function Is_Reserved_Signal_Instance_Pending
     (Siginst_Ac : in Global.Signal_Instance_Ac) return Boolean;

   procedure Remove_Task_From_Waiting_For_Signals (T : in K.Task_Id);

   procedure Register_Self_As_Waiting_For_Signals
     (Set : in Signal_Set_Ac_Const);
   --  Do not suspend the running task, just configure its signal related
   --  data structures to register the task as waiting for signals.

   procedure Deliver_Signal_Instances (Sig        : in Signal;
                                       To_Sigdata : in Global.Sig_Data_Ac);

   procedure Send_Signal_To_Process
     (Siginst_Ac : in Global.Signal_Instance_Ac);

   procedure Send_Signal_To_Task (Siginst_Ac : in Global.Signal_Instance_Ac);

   procedure Task_Stops_Getting_Signals (T : Task_Id);

   -----------------
   --  Initialize --
   -----------------
   --
   --  Initialize 'Actions' and 'Free_Signal_Instances'
   procedure Initialize;

end MaRTE.Kernel.Signals.Internals;
