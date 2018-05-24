------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--        'K e r n e l . S i g n a l s . P O S I X _ F u n c t i o n s'
--
--                                 Body
--
--
--  File 'k-signals-posix_functions.adb'                              By MAR.
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
with Ada.Unchecked_Conversion;
with System;

with MaRTE.Kernel.Signals.Global; use MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Signals.Pending;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Signals.Handler;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Scheduler;
--  Debug
with MaRTE.Kernel.Signals.Debug;

package body MaRTE.Kernel.Signals.POSIX_Functions is

   package TOI  renames K.Tasks_Operations.Internals;
   package SCHD renames K.Scheduler;
   package SP   renames K.Signals.Pending;

   use K.Tasks_Lists;
   use K.Signals.Global.Signal_Instances_Lists;

   use MaRTE.Integer_Types;
   use type MaRTE.Integer_Types.Int;

   Initialized : Boolean := False;

   -----------------------
   -- Reserved_Siginsts --
   -----------------------
   --
   --  Signal instances reserved for each normal (non RT)
   --  signal. Initialized at the begin-end part of this package.
   Reserved_Siginst : array (Signal_Set_Range) of
     aliased Global.Signal_Instances_Lists.Element;

   -----------------
   -- Sig_Add_Set --
   -----------------
   function Sig_Add_Set (Set : access Signal_Set; Sig : in Signal)
                         return Int is
   begin
      --   Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      Set (Sig) := True;
      return 0;
   end Sig_Add_Set;

   ------------------
   -- Sig_Fill_Set --
   ------------------
   function Sig_Fill_Set (Set : access Signal_Set) return Int is
   begin
      TOI.Reset_POSIX_Error;

      Set.all := Signal_Set'(others => True);
      return 0;
   end Sig_Fill_Set;

   -----------------
   -- Sig_Del_Set --
   -----------------
   function Sig_Del_Set (Set : access Signal_Set; Sig : in Signal)
                         return Int is
   begin
      --  Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      Set (Sig) := False;
      return 0;
   end Sig_Del_Set;

   -------------------
   -- Sig_Empty_Set --
   -------------------
   function Sig_Empty_Set (Set : access Signal_Set) return Int is
   begin
      TOI.Reset_POSIX_Error;

      Set.all := Signal_Set'(others => False);
      return 0;
   end Sig_Empty_Set;

   -------------------
   -- Sig_Is_Member --
   -------------------
   function Sig_Is_Member (Set : Signal_Set_Ac_Const; Sig : in Signal)
                           return Int is
   begin
      --  Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      if Sig = Signal_Null then
         return 1;
      end if;
      if Set (Sig) then
         return 1;
      else
         return 0;
      end if;
   end Sig_Is_Member;


   -----------------------------------------------------------------------
   -- Blocking and Unblocking Signals ------------------------------------
   -----------------------------------------------------------------------

   ----------------------
   -- Pthread_Sig_Mask --
   ----------------------
   function Pthread_Sig_Mask (How  : in Int;
                              Set  : in Signal_Set_Ac_Const;
                              Oset : in Signal_Set_Ac) return Int is
      Flags : Integer;
      Self_Sig_Data_Ac : Global.Sig_Data_Ac;
   begin
      pragma Debug (Debug.Pthread_Sig_Mask (How)); -- Trace

      --  Error Handling
      if not (How = SIG_BLOCK or How = SIG_UNBLOCK or How = SIG_SETMASK) then
         return INVALID_ARGUMENT;
      end if;

      K.Enter_Critic_Section (Flags);

      if SCHD.Self.Task_Type = SIGNAL_HANDLER_TASK then
         pragma Assert (Handler.Current_Siginst_Ac /= null and then
                        K.Task_OK (Handler.Current_Siginst_Ac.To_Task));
         Self_Sig_Data_Ac :=
           Global.Sig_Data_Ac (Handler.Current_Siginst_Ac.To_Task.Sig_Data);
      else
         Self_Sig_Data_Ac := Global.Sig_Data_Ac (SCHD.Self.Sig_Data);
      end if;

      if Oset /= null then
         Oset.all := Self_Sig_Data_Ac.Mask;
      end if;
      if Set /= null then
         case How is
            when SIG_BLOCK =>
               Self_Sig_Data_Ac.Mask := Self_Sig_Data_Ac.Mask
                 or Set.all;
               Self_Sig_Data_Ac.Mask := Self_Sig_Data_Ac.Mask
                 and not Global.Unblockeable_Signals;
            when SIG_UNBLOCK =>
               for Sig in Signal_Set_Range loop
                  if Self_Sig_Data_Ac.Mask (Sig) and Set (Sig) then
                     --  The signal gets unblocked
                     Self_Sig_Data_Ac.Mask (Sig) := False;
                     Internals.Deliver_Signal_Instances (Sig,
                                                         Self_Sig_Data_Ac);
                  end if;
               end loop;
            when SIG_SETMASK =>
               for Sig in Signal_Set_Range loop
                  if Self_Sig_Data_Ac.Mask (Sig) then
                     --  Sig was blocked
                     if not Set (Sig) then
                        --  The signal gets unblocked
                        Self_Sig_Data_Ac.Mask (Sig) := False;
                        Internals.Deliver_Signal_Instances (Sig,
                                                            Self_Sig_Data_Ac);
                     end if;
                  else --  Sig was unblocked
                     if (Set (Sig) and not
                         Global.Unblockeable_Signals (Sig)) then
                        --  The signal gets blocked
                        Self_Sig_Data_Ac.Mask (Sig) := True;
                     end if;
                  end if;
               end loop;
            when others =>
               --  Error Handling
               K.Leave_Critic_Section (Flags);
               return INVALID_ARGUMENT;
         end case;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Sig_Mask;


   -----------------------------------------------------------------------
   -- Signal Action ------------------------------------------------------
   -----------------------------------------------------------------------

   function Address_To_Handler_Procedure_Ac is new Ada.Unchecked_Conversion
     (System.Address, Global.Handler_Procedure_Ac);
   function Address_To_Sigaction_Procedure_Ac is new Ada.Unchecked_Conversion
     (System.Address, Global.Sigaction_Procedure_Ac);
   function To_Int is new Ada.Unchecked_Conversion (System.Address, Int);


   ----------------
   -- Sig_Action --
   ----------------
   function Sig_Action (Sig  : in Signal;
                        Act  : in Struct_Sig_Action_Ac_Const;
                        Oact : in Struct_Sig_Action_Ac) return Int is
      Flags : Integer;
      use type Sa_Flags_T;
   begin
      --  Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      if Global.Unblockeable_Signals (Sig) and Act /= null then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      K.Enter_Critic_Section (Flags);
      if Oact /= null then
         Oact.all := (Sa_Handler => Global.Actions (Sig).Sa_Handler,
                      Sa_Mask    => Global.Actions (Sig).Mask,
                      Sa_Flags   => Global.Actions (Sig).Flags);
      end if;
      if Act /= null then
         pragma Debug (Debug.Sig_Action1 (Act, Sig)); -- Trace

         Global.Actions (Sig).Sa_Handler := Act.Sa_Handler;
         Global.Actions (Sig).Mask       := Act.Sa_Mask;
         Global.Actions (Sig).Flags      := Act.Sa_Flags;
         case To_Int (Act.Sa_Handler) is
            when SIG_IGN =>
               SP.Discard_Pending_Instances (Sig);
               Global.Actions (Sig).Action := Global.SIGNAL_IGNORE;
            when SIG_DFL =>
               Global.Actions (Sig).Action := Global.SIGNAL_DEFAULT;
            when others =>
               if (Act.Sa_Flags and SA_SIGINFO) /= 0 then
                  Global.Actions (Sig).Action    := Global.EXEC_SIGACTION;
                  Global.Actions (Sig).Sigaction :=
                    Address_To_Sigaction_Procedure_Ac (Act.Sa_Handler);
               else
                  Global.Actions (Sig).Action  := Global.EXEC_HANDLER;
                  Global.Actions (Sig).Handler :=
                    Address_To_Handler_Procedure_Ac (Act.Sa_Handler);
               end if;
         end case;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sig_Action;


   -----------------------------------------------------------------------
   -- Wait for signals ---------------------------------------------------
   -----------------------------------------------------------------------

   function Sig_Wait_Info (Set    : in Signal_Set_Ac_Const;
                           Siginf : access Siginfo_T)
                           return Int is
      Flags : Integer;
      Self_Sigdata : Global.Sig_Data_Ac;
      Siginst_Ac : Global.Signal_Instance_Ac;
      Signo : Signal;
   begin
      --  Error Handling
      TOI.Reset_POSIX_Error;

      pragma Debug (Debug.Sig_Wait (Set)); -- Trace

      K.Enter_Critic_Section (Flags);

      Self_Sigdata := Global.Sig_Data_Ac (SCHD.Self.Sig_Data);

      --  Is already some of the signals in set pending ?
      for S in Signal_Set_Range loop
         if Set (S) and then SP.Is_Signal_Pending (S, Self_Sigdata) then
            Siginst_Ac := SP.First_Signal_Instance_Accepted (S, Self_Sigdata);
            Signo := Siginst_Ac.Signo;
            Siginf.all := (Signo => Signo, Code => Siginst_Ac.Code,
                           Value => Siginst_Ac.Value);
            --  Release the signal instance resources
            Internals.Release_Signal_Instance (Siginst_Ac);
            --  The task finishes 'Sig_Wait_Info' and continues running
            K.Leave_Critic_Section (Flags);
            return Int (Signo);
         end if;
      end loop;

      --  None of the signals in set is pending currently... so the task must
      --  wait
      SCHD.Running_Task_Gets_Blocked (WAITING_SIGNAL);
      Internals.Register_Self_As_Waiting_For_Signals (Set);
      SCHD.Do_Scheduling; -- leave CPU

      --  The task has accepted a signal
      Signo := Self_Sigdata.Accepted_Siginst_Ac.Signo;
      Siginf.all := (Signo => Signo,
                     Code  => Self_Sigdata.Accepted_Siginst_Ac.Code,
                     Value => Self_Sigdata.Accepted_Siginst_Ac.Value);
      --  Release the signal instance resources
      Internals.Release_Signal_Instance (Self_Sigdata.Accepted_Siginst_Ac);

      K.Leave_Critic_Section (Flags);
      return Int (Signo);
   end Sig_Wait_Info;

   -----------------------------------------------------------------------
   -- Send a Signal ------------------------------------------------------
   -----------------------------------------------------------------------

   ------------------
   -- Pthread_Kill --
   ------------------
   function Pthread_Kill (T : in Task_Id; Sig : in Signal) return Int is
      Flags : Integer;
   begin
      pragma Assert (Initialized);
      pragma Debug (Debug.Pthread_Kill (T, Sig)); -- Trace

      --  Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         return INVALID_ARGUMENT;
      end if;

      K.Enter_Critic_Section (Flags);

      if not K.Task_OK (T) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;

      if Sig = Signal_Null then
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;

      if not Reserved_Siginst (Sig).Pending then
         Reserved_Siginst (Sig).To_Task := T;
         Internals.Send_Signal_To_Task (Reserved_Siginst (Sig)'Access);
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Kill;


   ----------
   -- Kill --
   ----------
   function Kill (Pid : in Int; -- Parameter not used
                  Sig : in Signal) return Int is
      Flags : Integer;
   begin
      pragma Assert (Initialized);
      pragma Debug (Debug.Kill (Sig)); -- Trace

      --  Error Handling
      if Sig < Signal'First or Sig > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      K.Enter_Critic_Section (Flags);

      if Sig = Signal_Null then
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;

      if not Reserved_Siginst (Sig).Pending then
         Internals.Send_Signal_To_Process (Reserved_Siginst (Sig)'Access);
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Kill;


   --------------
   -- Sigqueue --
   --------------
   function Sigqueue (Pid   : in Int; -- Parameter not used
                      Signo : in Signal;
                      Value : in Int) return Int is
      Flags : Integer;
      Siginst_Ac : Global.Signal_Instance_Ac;
   begin
      --  Error Handling
      if Signo < Signal'First or Signo > Signal'Last then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;

      K.Enter_Critic_Section (Flags);

      if (Global.Actions (Signo).Action = Global.SIGNAL_IGNORE or
          Signo = Signal_Null) then
         K.Leave_Critic_Section (Flags);
         return 0;
      end if;

      Siginst_Ac := Internals.Request_Signal_Instance;
      if Siginst_Ac = null then
         TOI.Set_POSIX_Error (RESOURCE_TEMPORARILY_UNAVAILABLE);
         K.Leave_Critic_Section (Flags);
         return RESOURCE_TEMPORARILY_UNAVAILABLE;
      end if;
      TOI.Reset_POSIX_Error;

      Siginst_Ac.Signo      := Signo;
      Siginst_Ac.Code       := SI_QUEUE;
      Siginst_Ac.Value      := Value;
      Siginst_Ac.To_Task    := null;
      Internals.Send_Signal_To_Process (Siginst_Ac);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Sigqueue;

   -----------------
   -- Raise_POSIX --
   -----------------
   function Raise_POSIX (Sig : in Signal) return MaRTE.Integer_Types.Int is
   begin
      return Pthread_Kill (SCHD.Self, Sig);
   end Raise_POSIX;

   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      for S in Signal_Set_Range loop
         Reserved_Siginst (S).Signo      := S;
         Reserved_Siginst (S).Code       := SI_USER;
         Reserved_Siginst (S).Value      := 0;
         Reserved_Siginst (S).To_Task    := null;
         Reserved_Siginst (S).Reserved   := True;
         Reserved_Siginst (S).Pending    := False;
      end loop;

      Initialized := True;
   end Initialize;

end MaRTE.Kernel.Signals.POSIX_Functions;
