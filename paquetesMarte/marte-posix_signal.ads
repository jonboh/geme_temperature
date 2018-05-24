------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'S i g n a l'
--
--                                   Spec
--
--
--  File 'signal.ads'                                                  By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'signal.h'.
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

with MaRTE.Kernel.Signals;
with MaRTE.Kernel.Signals.POSIX_Functions;
use MaRTE.Kernel;

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.POSIX_Signal is

   package K renames MaRTE.Kernel;

   --  3.3 Signals
   subtype Signal                     is Signals.Signal;
   subtype Signal_Set                 is Signals.Signal_Set;
   subtype Signal_Set_Ac              is Signals.Signal_Set_Ac;
   subtype Signal_Set_Ac_Const        is Signals.Signal_Set_Ac_Const;
   subtype Struct_Sig_Action          is Signals.Struct_Sig_Action;
   subtype Struct_Sig_Action_Ac       is Signals.Struct_Sig_Action_Ac;
   subtype Struct_Sig_Action_Ac_Const is Signals.Struct_Sig_Action_Ac_Const;

   SIG_BLOCK   : Int renames Signals.SIG_BLOCK;
   SIG_UNBLOCK : Int renames Signals.SIG_UNBLOCK;
   SIG_SETMASK : Int renames Signals.SIG_SETMASK;

   SIG_DFL : Int renames Signals.SIG_DFL;
   SIG_IGN : Int renames Signals.SIG_IGN;

   function Sigemptyset (Set : access Signal_Set) return Int
     renames Signals.POSIX_Functions.Sig_Empty_Set;

   function Sigfillset (Set : access Signal_Set) return Int
     renames Signals.POSIX_Functions.Sig_Fill_Set;

   function Sigaddset (Set : access Signal_Set;
                       Sig : in     Signal) return Int
     renames Signals.POSIX_Functions.Sig_Add_Set;

   function Sigdelset (Set : access Signal_Set;
                       Sig : in     Signal) return Int
     renames Signals.POSIX_Functions.Sig_Del_Set;

   function Sigismember (Set : in Signal_Set_Ac_Const;
                         Sig : in Signal) return Int
     renames Signals.POSIX_Functions.Sig_Is_Member;

   function Sigaction (Sig  : in Signal;
                       Act  : in Struct_Sig_Action_Ac_Const;
                       Oact : in Struct_Sig_Action_Ac) return Int
     renames Signals.POSIX_Functions.Sig_Action;

   function Pthread_Sigmask (How  : in Int;
                             Set  : in Signal_Set_Ac_Const;
                             Oset : in Signal_Set_Ac) return Int
     renames Signals.POSIX_Functions.Pthread_Sig_Mask;

   function Sigwait (Set : in Signal_Set_Ac_Const;
                     Sig : access Signal) return Int;
   pragma Export (C, Sigwait, "sigwait");
   --  pragma Export_Function (Sigwait,
   --                          "sigwait",
   --                          Mechanism => (Set => Reference));

   function Sigwaitinfo (Set    : in Signal_Set_Ac_Const;
                         Siginf : access Signals.Siginfo_T) return Int
     renames Signals.POSIX_Functions.Sig_Wait_Info;

   function Pthread_Kill (T   : in Task_Id;
                          Sig : in Signal) return Int
     renames Signals.POSIX_Functions.Pthread_Kill;

   function Kill (Pid : in Int; Sig : in Signal) return Int
     renames Signals.POSIX_Functions.Kill;

end MaRTE.POSIX_Signal;
