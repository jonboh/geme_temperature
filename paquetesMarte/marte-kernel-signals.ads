------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                      'K e r n e l . S i g n a l s'
--
--                                 Spec
--
--
--  File 'k-signals.ads'                                               By MAR.
--
--
--  Signal management.
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

with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with System;
with MaRTE.POSIX_Constants;

package MaRTE.Kernel.Signals is

   package K renames MaRTE.Kernel;

   use MaRTE;

   --  Signal Type

   type Signal is new Int range 0 .. POSIX_Constants.SIGRTMAX;
   for Signal'Size use Int'Size;
   subtype Signal_Set_Range is Signal range 1 .. Signal'Last;
   subtype Non_RT_Signals_Range is Signal
     range 1 .. POSIX_Constants.SIGRTMIN - 1;

   --  Standard Signals (required by POSIX)

   Signal_Null,                   SIGNULL : constant Signal :=
     POSIX_Constants.SIGNULL;

   Signal_Hangup,                 SIGHUP  : constant Signal :=
     POSIX_Constants.SIGHUP;

   Signal_Interrupt,              SIGINT  : constant Signal :=
     POSIX_Constants.SIGINT;

   Signal_Quit,                   SIGQUIT : constant Signal :=
     POSIX_Constants.SIGQUIT;

   Signal_Illegal_Instruction,    SIGILL  : constant Signal :=
     POSIX_Constants.SIGILL;

   Signal_Abort,                  SIGABRT : constant Signal :=
     POSIX_Constants.SIGABRT;

   Signal_Bus_Error,              SIGBUS  : constant Signal :=
     POSIX_Constants.SIGBUS;

   Signal_Floating_Point_Error,   SIGFPE  : constant Signal :=
     POSIX_Constants.SIGFPE;

   Signal_Kill,                   SIGKILL : constant Signal :=
     POSIX_Constants.SIGKILL;

   Signal_User_1,                 SIGUSR1 : constant Signal :=
     POSIX_Constants.SIGUSR1;

   Signal_Segmentation_Violation, SIGSEGV : constant Signal :=
     POSIX_Constants.SIGSEGV;

   Signal_User_2,                 SIGUSR2 : constant Signal :=
     POSIX_Constants.SIGUSR2;

   Signal_Pipe_Write,             SIGPIPE : constant Signal :=
     POSIX_Constants.SIGPIPE;

   Signal_Alarm,                  SIGALRM : constant Signal :=
     POSIX_Constants.SIGALRM;

   Signal_Terminate,              SIGTERM : constant Signal :=
     POSIX_Constants.SIGTERM;

   Signal_Child,                  SIGCHLD : constant Signal :=
     POSIX_Constants.SIGCHLD;

   Signal_Continue,               SIGCONT : constant Signal :=
     POSIX_Constants.SIGCONT;

   Signal_Stop,                   SIGSTOP : constant Signal :=
     POSIX_Constants.SIGSTOP;

   Signal_Terminal_Stop,          SIGTSTP : constant Signal :=
     POSIX_Constants.SIGTSTP;

   Signal_Terminal_Input,         SIGTTIN : constant Signal :=
     POSIX_Constants.SIGTTIN;

   Signal_Terminal_Output,        SIGTTOU : constant Signal :=
     POSIX_Constants.SIGTTOU;

   subtype Realtime_Signal is Signal range
     POSIX_Constants.SIGRTMIN .. POSIX_Constants.SIGRTMAX;

   --  Signal sets

   type Signal_Set is private;
   type Signal_Set_Ac is access all Signal_Set;
   type Signal_Set_Ac_Const is access constant Signal_Set;
   Empty_Set : constant Signal_Set;

   --  Blocking and Unblocking Signals

   SIG_BLOCK   : constant Int := MaRTE.POSIX_Constants.SIG_BLOCK;
   SIG_UNBLOCK : constant Int := MaRTE.POSIX_Constants.SIG_UNBLOCK;
   SIG_SETMASK : constant Int := MaRTE.POSIX_Constants.SIG_SETMASK;

   --  Signal Action

   SIG_DFL : constant Int := MaRTE.POSIX_Constants.SIG_DFL;
   SIG_IGN : constant Int := MaRTE.POSIX_Constants.SIG_IGN;

   type Sa_Flags_T is new MaRTE.Integer_Types.Unsigned_32;
   SA_SIGINFO : constant Sa_Flags_T := MaRTE.POSIX_Constants.SA_SIGINFO;

   type Struct_Sig_Action is record
      Sa_Handler : System.Address;  -- access to procedure (1 or 3 params.)
      Sa_Mask    : Signal_Set;
      Sa_Flags   : Sa_Flags_T;
   end record;
   pragma Convention (C, Struct_Sig_Action);

   type Struct_Sig_Action_Ac       is access all Struct_Sig_Action;
   pragma No_Strict_Aliasing (Struct_Sig_Action_Ac);

   type Struct_Sig_Action_Ac_Const is access constant Struct_Sig_Action;
   pragma No_Strict_Aliasing (Struct_Sig_Action_Ac_Const);
   --  Signal Event Notification
   subtype Sigval is Int;
   type Notification is range 0 .. 1;
   for Notification'Size use Int'Size;
   NO_NOTIFICATION : constant Notification :=
     MaRTE.POSIX_Constants.NO_NOTIFICATION;
   SIGNAL_NOTIFICATION : constant Notification :=
     MaRTE.POSIX_Constants.SIGNAL_NOTIFICATION;
   type Signal_Event is record
      Event_Notification : Notification := NO_NOTIFICATION;
      Event_Signal       : Signal       := Signal_Kill;
      Event_Sigval       : Sigval       := 0;
   end record;

   --  siginfo_t

   type Cause_Of_Signal is new Int range 0 .. 2;
   for Cause_Of_Signal'Size use Int'Size;
   SI_USER  : constant Cause_Of_Signal := MaRTE.POSIX_Constants.SI_USER;
   SI_QUEUE : constant Cause_Of_Signal := MaRTE.POSIX_Constants.SI_QUEUE;
   SI_TIMER : constant Cause_Of_Signal := MaRTE.POSIX_Constants.SI_TIMER;
   type Siginfo_T is record
      Signo : Signal;
      Code  : Cause_Of_Signal;
      Value : Int;
   end record;
   pragma Convention (C, Siginfo_T);
private

   type Signal_Set is array (Signal_Set_Range) of Boolean;
   Empty_Set : constant Signal_Set := (others => False);

end MaRTE.Kernel.Signals;
