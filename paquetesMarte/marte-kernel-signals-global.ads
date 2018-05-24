------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                 'K e r n e l . S i g n a l s . G l o b a l'
--
--                                  Spec
--
--
--  File 'k-signals-global.ads'                                        By MAR.
--
--
--  Global Signal management related types and variables.
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
with MaRTE.SLL;
pragma Elaborate_All (MaRTE.SLL);
with MaRTE.SLL.Advanced;
pragma Elaborate_All (MaRTE.SLL.Advanced);

package MaRTE.Kernel.Signals.Global is

   package K renames MaRTE.Kernel;

   -----------------------------------------
   -- Unblockeable and Blockeable Signals --
   -----------------------------------------
   Unblockeable_Signals : constant Signal_Set;
   Blockeable_Signals   : constant Signal_Set;

   ------------
   -- Action --
   ------------
   type Signal_Actions is (SIGNAL_IGNORE, SIGNAL_DEFAULT,
                           EXEC_SIGACTION, EXEC_HANDLER);
   type Handler_Procedure_Ac is access procedure (Sig : in Signal);
   type Sigaction_Procedure_Ac is access procedure (Sig     : in Signal;
                                                    Siginfo : access Siginfo_T;
                                                    Extra   : System.Address);
   type Sig_Action_State is record
      Sa_Handler : System.Address;
      Action     : Signal_Actions;
      Mask       : Signal_Set;
      Flags      : Sa_Flags_T;
      Handler    : Handler_Procedure_Ac;
      Sigaction  : Sigaction_Procedure_Ac;
   end record;

   Actions : array (Signal_Set_Range) of Sig_Action_State;
   --  Initialized at the begin-end of 'Signals.Internals'.

   ----------------------
   -- Signal Instances --
   ----------------------
   type Signal_Instance is tagged record
      Signo    : Signal;
      Code     : Cause_Of_Signal;
      Value    : Int;
      To_Task  : K.Task_Id;
      Reserved : Boolean;
      Pending  : Boolean;
   end record;
   package Signal_Instances_Lists is new MaRTE.SLL (Signal_Instance);
   package Signal_Instances_Lists_Advanced is
      new Signal_Instances_Lists.Advanced;
   subtype Signal_Instance_Ac is Signal_Instances_Lists.Element_Ac;

   ---------------------------
   -- Pending_Signals_Lists --
   ---------------------------
   type Pending_Signals_List is
      array (Signal_Set_Range) of Signal_Instances_Lists.List;

   --------------
   -- Sig_Data --
   --------------
   --
   --  Every task has an access to one record of this type in its TCB, and
   --  in the opposite direction, the owner tasks is at the same time
   --  pointed by 'Owner'.
   type Sig_Data is new K.Sig_Data_Base with record
      Mask                   : Signal_Set;
      Pending_Signals        : Pending_Signals_List;
      Waited_Signals         : Signal_Set;
      Is_Waiting_For_Signals : Boolean;
      Accepted_Siginst_Ac    : Signal_Instance_Ac;
      Owner                  : Task_Id;
   end record;
   package Sig_Data_Lists is new MaRTE.SLL (Sig_Data);
   subtype Sig_Data_Ac is Sig_Data_Lists.Element_Ac;

   --------------------------
   -- Sig_Data_Blocks_Used --
   --------------------------
   --
   --  All the 'Sig_Data' objects associated with active task are in
   --  this list. It is used to find tasks to deliver or accept
   --  signals.
   Sig_Data_Blocks_Used : Sig_Data_Lists.List := Sig_Data_Lists.Null_List;

private
   Unblockeable_Signals : constant Signal_Set :=
     (SIGKILL      => True,
      SIGSTOP      => True,
      others       => False);
   Blockeable_Signals : constant Signal_Set := not Unblockeable_Signals;
end MaRTE.Kernel.Signals.Global;
