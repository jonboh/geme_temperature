------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--       'K e r n e l . S i g n a l s . P O S I X _ F u n c t i o n s'
--
--                                 Spec
--
--
--  File 'k-signals-posix_functions.ads'                               By MAR.
--
--
--  POSIX signal management functions.
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

with MaRTE.Integer_Types;
with System;

package MaRTE.Kernel.Signals.POSIX_Functions is

   package K renames MaRTE.Kernel;

   --  Signal sets

   function Sig_Empty_Set (Set : access Signal_Set)
                           return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Empty_Set, "sigemptyset");

   function Sig_Fill_Set (Set : access Signal_Set)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Fill_Set, "sigfillset");

   function Sig_Add_Set (Set : access Signal_Set; Sig : in Signal)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Add_Set, "sigaddset");

   function Sig_Del_Set (Set : access Signal_Set; Sig : in Signal)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Del_Set, "sigdelset");

   function Sig_Is_Member (Set : Signal_Set_Ac_Const; Sig : in Signal)
                           return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Is_Member, "sigismember");

   --  Blocking and Unblocking Signals

   function Pthread_Sig_Mask (How  : in MaRTE.Integer_Types.Int;
                              Set  : in Signal_Set_Ac_Const;
                              Oset : in Signal_Set_Ac)
                              return MaRTE.Integer_Types.Int;
   pragma Export (C, Pthread_Sig_Mask, "pthread_sigmask");

   --  Signal Action

   function Sig_Action (Sig  : in Signal;
                        Act  : in Struct_Sig_Action_Ac_Const;
                        Oact : in Struct_Sig_Action_Ac)
                        return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Action, "sigaction");

   --  Wait for signals

   function Sig_Wait_Info (Set    : in Signal_Set_Ac_Const;
                           Siginf : access Siginfo_T)
                           return MaRTE.Integer_Types.Int;
   pragma Export (C, Sig_Wait_Info, "sigwaitinfo");
   --  Send a Signal

   function Pthread_Kill (T : in Task_Id; Sig : Signal)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Pthread_Kill, "pthread_kill");

   function Kill (Pid : in MaRTE.Integer_Types.Int; Sig : in Signal)
                  return MaRTE.Integer_Types.Int;
   pragma Export (C, Kill, "kill");

   function Sigqueue (Pid   : in MaRTE.Integer_Types.Int; -- Parameter not used
                      Signo : in Signal;
                      Value : in Int)
                      return MaRTE.Integer_Types.Int;
   pragma Export (C, Sigqueue, "sigqueue");

   function Raise_POSIX (Sig : in Signal) return MaRTE.Integer_Types.Int;
   pragma Export (C, Raise_POSIX, "raise");

   ------------------
   --  Initialize  --
   ------------------
   --
   --  Initialize reserved "Siginst"
   procedure Initialize;

end MaRTE.Kernel.Signals.POSIX_Functions;
