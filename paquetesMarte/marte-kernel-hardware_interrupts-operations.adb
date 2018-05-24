------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
-- 'K e r n e l . H a r d w a r e _ I n t e r r u p t s . O p e r a t i o n s'
--
--                                 Body
--
--
--  File 'k-hardware_interrupts-operations.adb'                        By MAR.
--
--
--  Basic user hardware interrupts management operations. Based in a
--  POSIX draft for "Interrupt Control API" (P1003.2X/D1.0, February
--  2001).
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
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Timers;

package body MaRTE.Kernel.Hardware_Interrupts.Operations is

   package CP   renames MaRTE.Configuration_Parameters;
   package SCHD renames K.Scheduler;
   package TSTE renames K.Task_Suspension_Timed_Events;
   use type CP.Use_HW_Interrupts_Control;
   use type K.Hardware_Interrupts.Intr_T;
   use type Int;

   ---------------------
   -- Posix_Intr_Lock --
   ---------------------
   function Posix_Intr_Lock (Intr : in Intr_T) return Int is
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      if (Intr > Intr_T'Last or else Intr < Intr_T'First or else
          K.Hardware_Interrupts.Is_Reserved_Interrupt (Intr)) then
         return INVALID_ARGUMENT;
      end if;

      HAL.Hardware_Interrupt_Controller_Disable_Interrupt (Intr);
      return 0;
   end Posix_Intr_Lock;

   -----------------------
   -- Posix_Intr_Unlock --
   -----------------------
   function Posix_Intr_unlock (Intr : in Intr_T) return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      if (Intr > Intr_T'Last or else Intr < Intr_T'First or else
          K.Hardware_Interrupts.Is_Reserved_Interrupt (Intr)) then
         return INVALID_ARGUMENT;
      end if;

      K.Enter_Critic_Section (Flags);
      --  Is there any task associated with the interrupt ??
      if Associations_Lists.Is_Empty (Associations (Intr)) then
         --  An interrupt only can be unlocked if there is some task
         --  associated with it.
         K.Leave_Critic_Section (Flags);
         return NO_ISR_ASSOCIATED;
      end if;
      K.Leave_Critic_Section (Flags);

      HAL.Hardware_Interrupt_Controller_Enable_Interrupt (Intr);
      HAL.Enable_Interrupts;
      return 0;
   end Posix_Intr_Unlock;

   -----------------------------------
   -- Interrupts associations count --
   -----------------------------------
   --
   --  Used by 'Posix_Intr_Associate' to know if has been reached the
   --  limit of task associated with an interrupt
   Associations_Count : array (Intr_T) of Natural := (others => 0);

   --------------------------
   -- Posix_Intr_Associate --
   --------------------------
   function Posix_Intr_Associate (Intr    : in Intr_T;
                                  Handler : in Interrupt_Handler_Function;
                                  Area    : in System.Address;
                                  Size    : in Size_T)
                                 return Int is
      use type System.Address, MaRTE.Integer_Types.Size_T;
      Flags : Integer;
      Assoc : Association;
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if (Intr > Intr_T'Last or else Intr < Intr_T'First or else
          (Size > 0 and then Area = System.Null_Address) or else
          K.Hardware_Interrupts.Is_Reserved_Interrupt (Intr) or else
          Handler = null) then
         return INVALID_ARGUMENT;
      end if;
      K.Enter_Critic_Section (Flags);
      if (Associations_Count (Intr) = CP.Intr_Connect_Max or else
          not Associations_Pool.Resources_Left) then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_TEMPORARILY_UNAVAILABLE;
      end if;

      --  Get free association object
      Assoc := Associations_Pool.Request;

      --  Associate handler and running task with 'intr'
      Association_Base (Assoc.all) := (Handler => Handler,
                                       Area    => Area,
                                       T       => SCHD.Self,
                                       Pending => False);
      Associations_Lists.Enqueue_Head (Assoc, Associations (Intr));
      Associations_Count (Intr) := Associations_Count (Intr) + 1;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Posix_Intr_Associate;

   -----------------------------
   -- Posix_Intr_Disassociate --
   -----------------------------
   function Posix_Intr_Disassociate (Intr    : in Intr_T;
                                     Handler : in Interrupt_Handler_Function)
                                    return Int is
      use type Association;
      use type K.Task_Id;
      Flags : Integer;
      Assoc : Association;
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      if Intr > Intr_T'Last or Intr < Intr_T'First then
         return INVALID_ARGUMENT;
      end if;
      K.Enter_Critic_Section (Flags);

      --  Look for the association
      Assoc := Associations_Lists.Head (Associations (Intr));
      loop
         exit when Assoc = null;
         exit when Assoc.Handler = Handler;
         Assoc := Associations_Lists.Next (Assoc);
      end loop;

      --  Is the handler connected with the calling task ?
      if Assoc = null or else Assoc.T /= SCHD.Self then
         K.Leave_Critic_Section (Flags);
         return NO_ISR_ASSOCIATED;
      end if;

      --  Remove association
      Associations_Lists.Dequeue (Assoc, Associations (Intr));
      Associations_Pool.Release (Assoc);
      Associations_Count (Intr) := Associations_Count (Intr) - 1;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Posix_Intr_Disassociate;

   ---------------------------------
   -- POSIX_Intr_Timedwait_HWTime --
   ---------------------------------
   function POSIX_Intr_Timedwait_HWTime
     (OS_Flags     : in     Int;
      Abs_Timeout  : in     HAL.HWTime;
      Abs_Timespec : in     MaRTE.Timespec.Timespec_Ac_Const;
      Intr         : access Intr_T;
      Handler      : access Interrupt_Handler_Function) return Int is
      use type Association;
      use type K.Task_Id;
      use type MaRTE.HAL.HWTime;
      Flags : Integer;
      Assoc : Association;
      Connected_To_ISR : Boolean := False;
      Now : HAL.HWTime;
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      K.Enter_Critic_Section (Flags);

      --  Look for ISRs connected to the calling task. If some of then
      --  is pending the function returns
      for I in Intr_T loop
         if not K.Hardware_Interrupts.Is_Reserved_Interrupt (I) then
            Assoc := Associations_Lists.Head (Associations (I));
            loop
               exit when Assoc = null;
               if Assoc.T = SCHD.Self then
                  Connected_To_ISR := True;
                  if Assoc.Pending then
                     --  There is a pending interrupt for the task
                     Assoc.Pending := False;
                     Intr.all := I;
                     Handler.all := Assoc.Handler;
                     --  Function returns immediately
                     K.Leave_Critic_Section (Flags);
                     return 0;
                  end if;
               end if;
               Assoc := Associations_Lists.Next (Assoc);
            end loop;
         end if;
      end loop;

      if not Connected_To_ISR then
         --  Tasks isn't connected to any ISR
         K.Leave_Critic_Section (Flags);
         return NO_ISR_ASSOCIATED;
      end if;

      --  No pending interrupt => task must wait
      if MaRTE.Timespec."/=" (Abs_Timespec, null) then
         --  Invalid timeout ?
         if (Abs_Timespec.Tv_Nsec < 0 or
             Abs_Timespec.Tv_Nsec > 1_000_000_000) then
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;

         --  Has timeout already expired ?
         Now := HAL.Get_HWTime;
         if (Abs_Timeout >  Now + K.Timers.Realtime_Clock_Offset
                                + K.Suspension_Time_Minimum)     then
            --  It hasn't expired yet => Configure Task Timed Event
            SCHD.Self.Suspension_Event.T :=
              Abs_Timeout - Timers.Realtime_Clock_Offset;
            TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
              TSTE.HWINTR_TIMEDWAIT;
            --  Get suspended (Wait for interrupt or timeout)
            SCHD.Running_Task_Gets_Suspended
              (With_Status => TIMED_WAITING_HW_INTERRUPT, At_Time => Now);
            SCHD.Do_Scheduling; -- leave CPU

            --  Here task continues after interrupt or after timeout...
            pragma Assert (SCHD.Self.Internal_Error_Code = NO_ERROR or
                             SCHD.Self.Internal_Error_Code = TIMED_OUT);
            if SCHD.Self.Internal_Error_Code = TIMED_OUT then
               --  Timeout has expired
               SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
               K.Leave_Critic_Section (Flags);
               return TIMED_OUT;
            end if;

         else
            --  'Timeout' has expired or is too small to be waited for.
            K.Leave_Critic_Section (Flags);
            return TIMED_OUT;
         end if;
      else
         --  'Abs_Timespec = null' => wait for interrupt without timeout
         SCHD.Running_Task_Gets_Blocked (With_Status => WAITING_HW_INTERRUPT);
         SCHD.Do_Scheduling; -- leave CPU
      end if;

      --  If here is because an interrupt has been notified
      Intr.all    := SCHD.Self.Last_Deliver_Intr;
      Handler.all := SCHD.Self.Last_Executed_Intr_Handler;

      K.Leave_Critic_Section (Flags);
      return 0;
   end POSIX_Intr_Timedwait_HWTime;

end MaRTE.Kernel.Hardware_Interrupts.Operations;
