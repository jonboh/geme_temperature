------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . S i g n a l s . H a n d l e r'
--
--                                  Body
--
--
--  File 'k-signals-handler.adb'                                       By Mar.
--
--
--  General signal catching function.
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
with MaRTE.Kernel.Signals.Debug;
with MaRTE.Kernel.Signals.Pending;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body MaRTE.Kernel.Signals.Handler is

   package SI   renames K.Signals.Internals;
   package SP   renames K.Signals.Pending;
   package SCHD renames K.Scheduler;
   package TOI  renames K.Tasks_Operations.Internals;
   use K.Signals.Global.Signal_Instances_Lists;
   use K.Signals.Global;

   -------------------------------
   -- Signal_Catching_Task_Body --
   -------------------------------

   function Signal_Catching_Task_Body (Inf : System.Address)
                                      return System.Address is
      Siginfo : aliased Siginfo_T;
      Flags : Integer;
      Sig : Signal;
   begin
      K.Enter_Critic_Section (Flags);
      loop  --  infinite loop
         loop
            --  Get the next signal instance to deliver
            Current_Siginst_Ac := SP.Extract_Head_Instace_From_Handler_Q;
            exit when Current_Siginst_Ac /= null;
            --  Leave the CPU and wait for another signal. It will
            --  reactivated from 'Internals.Deliver_Signal_Instances'
            --  when there is another signal handler to execute.
            SCHD.Running_Task_Gets_Blocked (SIG_HANDLER_WAIT_TO_DELIVER);
            SCHD.Do_Scheduling;
         end loop;
         Sig := Current_Siginst_Ac.Signo;

         pragma Debug
           (Debug.Signal_Catching1 (Current_Siginst_Ac.To_Task, Sig)); -- Trace

         --  Execute the handler
         pragma Assert (Actions (Sig).Action = EXEC_SIGACTION or
                        Actions (Sig).Action = EXEC_HANDLER,
                        "Signal_Catching: neither Handler nor Sigaction");
         if Actions (Sig).Action = EXEC_SIGACTION then
            Siginfo.Signo := Sig;
            Siginfo.Code  := Current_Siginst_Ac.Code;
            Siginfo.Value := Current_Siginst_Ac.Value;
            K.Leave_Critic_Section (Flags);
            --  Execute 3 parameters function
            Actions (Sig).Sigaction (Sig, Siginfo'Access, System.Null_Address);
         elsif Actions (Sig).Action = EXEC_HANDLER then
            K.Leave_Critic_Section (Flags);
            --  Execute 1 parameter function
            Actions (Sig).Handler (Sig);
         end if;
         --  Signal handler finished

         --  Release the delivered 'Siginst' and back to the beginning
         --  of the loop to see if there are more signal handlers to
         --  execute (could be some other to execute since a new
         --  signal could have been delivered during the execution of
         --  the last signal handler).
         K.Enter_Critic_Section (Flags);
         SI.Release_Signal_Instance (Current_Siginst_Ac);

      end loop;
      return System.Null_Address; --  Never reached
   end Signal_Catching_Task_Body;

   -------------------------------------------
   -- Funtions to be used from the GNAT RTS --
   -------------------------------------------
   function Top_Of_Stack_Of_Task_Receiving_Signal
     return MaRTE.Integer_Types.Unsigned_32 is
   begin
      return Current_Siginst_Ac.To_Task.Stack_Ptr;
   end Top_Of_Stack_Of_Task_Receiving_Signal;

   --------------------------------------
   --  Initialize_Signal_Handler_Task  --
   --------------------------------------
   procedure Initialize_Signal_Handler_Task is
   begin
      Handler_Task :=
        Tasks_Operations.Initialize_TCBs.Initialize_Signal_Handler_Task
          (Signal_Catching_Task_Body'Access, TOI.Task_Wrapper'Address);
   end Initialize_Signal_Handler_Task;

end MaRTE.Kernel.Signals.Handler;
