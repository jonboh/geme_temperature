------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--           'K e r n e l . S e m a p h o r e s . O p e r a t i o n s'
--
--                                  Body
--
--
--
--  File 'k-semaphores-operations.adb'                                By MAR.
--
--
--  POSIX semaphores implementation.
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
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Timers;

package body MaRTE.Kernel.Semaphores.Operations is

   package SCHD renames K.Scheduler;
   package TOI  renames K.Tasks_Operations.Internals;
   package ML   renames K.Tasks_Map_Lists;
   package TSTE renames K.Task_Suspension_Timed_Events;

   use type Int;

   --------------
   -- Sem_Init --
   --------------
   function Sem_Init (Sem     : in  Semaphores.Semaphore_Ac;
                      Pshared : in  Int;
                      Value   : in  Semaphores.Semaphore_Value)
                     return Int is
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      if Sem = null Then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      -- Initialize semaphore
      ML.Initialize (Sem.Blocked_Tasks);
      Sem.Value := Value;
      Sem.Magic := INITIALIZED;
      return 0;
   end Sem_Init;

   -----------------
   -- Sem_Destroy --
   -----------------
   function Sem_Destroy (Sem : in Semaphores.Semaphore_Ac) return Int is
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error (INVALID_ARGUMENT);
         return -1;
      end if;
      if not ML.Is_Empty (Sem.Blocked_Tasks) then
         TOI.Set_POSIX_Error (RESOURCE_BUSY);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      -- Destroy semaphore
      Sem.Magic := DESTROYED;
      return 0;
   end Sem_Destroy;

   --------------
   -- Sem_Wait --
   --------------
   function Sem_Wait (Sem : in Semaphores.Semaphore_Ac) return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                       Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      if Sem.Value = 0 then
         -- Block task
         SCHD.Running_Task_Gets_Blocked (With_Status => BLOCKED);
         ML.Enqueue_In_Order (SCHD.Self, Sem.Blocked_Tasks);
         SCHD.Do_Scheduling; -- leave CPU
      else
         --  Lock semaphore
         Sem.Value := Sem.Value - 1;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sem_Wait;

   -----------------
   -- Sem_Trywait --
   -----------------
   function Sem_Trywait (Sem : in Semaphores.Semaphore_Ac) return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                       Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      if Sem.Value = 0 then
         --  Already locked
         TOI.Set_POSIX_Error_And_Leave_Critic_Section
           (RESOURCE_TEMPORARILY_UNAVAILABLE, Flags);
         return -1;
      else
         --  Lock semaphore
         Sem.Value := Sem.Value - 1;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sem_Trywait;

   --------------------------
   -- Sem_Timedwait_HWTime --
   --------------------------
   --  Parameter 'Abs_Timespec' is used to check if and the
   --  nanoseconds field value less than zero or greater than or equal
   --  to 1000 million. As POSIX says "The validity of the abs_timeout
   --  need not be checked if the semaphore can be locked immediately"
   --  so it cannot be checked before calling this function.
   function Sem_Timedwait_HWTime (Sem          : in Semaphores.Semaphore_Ac;
                                  Abs_Timeout  : in HAL.HWTime;
                                  Abs_Timespec : in MaRTE.Timespec.Timespec)
                                 return Int is
      Flags : Integer;
      Now   : HAL.HWTime;
      use type HAL.HWTime;
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                       Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;

      if Sem.Value = 0 then
         if (Abs_Timespec.Tv_Nsec < 0 or
             Abs_Timespec.Tv_Nsec > 1_000_000_000) then
            TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                          Flags);
            return -1;
         end if;

         --  Block on semaphore
         Now := HAL.Get_HWTime;
         if (Abs_Timeout >  Now + K.Timers.Realtime_Clock_Offset
                                + K.Suspension_Time_Minimum)
         then
            --  Configure Task Timed Event
            SCHD.Self.Suspension_Event.T :=
              Abs_Timeout - Timers.Realtime_Clock_Offset;
            TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
              TSTE.SEM_TIMEDWAIT;
            TSTE.Task_Suspension_TE_Ac
              (SCHD.Self.Suspension_Event).Sem_Where_Blocked :=
              To_Semaphore_Base_Ac (Sem);
            --  Get suspended (Wait for unlock or timeout)
            SCHD.Running_Task_Gets_Suspended
              (With_Status => TIMED_WAITING_SEM, At_Time => Now);
            ML.Enqueue_In_Order (SCHD.Self, Sem.Blocked_Tasks);
            SCHD.Do_Scheduling; -- leave CPU

            --  Here task continues after locking mutex or after timeout...
            if TOI.There_Has_Been_A_POSIX_Error then
               K.Leave_Critic_Section (Flags);
               return -1;
            end if;

         else
            --  'Timeout' is too small to be waited for.
            TOI.Set_POSIX_Error_And_Leave_Critic_Section (TIMED_OUT, Flags);
            return -1;
         end if;
      else
         --  Lock semaphore
         Sem.Value := Sem.Value - 1;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sem_Timedwait_HWTime;

   --------------
   -- Sem_Post --
   --------------
   function Sem_Post (Sem : in Semaphores.Semaphore_Ac) return Int is
      Flags : Integer;
      T_Mx_Prio : Task_Id;
      use type K.Task_Id;
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                       Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      T_Mx_Prio := ML.Head (Sem.Blocked_Tasks);
      if T_Mx_Prio /= null then
         pragma Assert (Sem.Value = 0);
         --  There are blocked tasks => activate one of then
         ML.Dequeue_Head (T_Mx_Prio, Sem.Blocked_Tasks);

         if T_Mx_Prio.Status = TIMED_WAITING_SEM then
            --  The task had used 'Timed_Wait' to wait on the
            --  Semaphore and the timeout hasn't been reached yet.
            SCHD.Remove_Timed_Event (T_Mx_Prio.Suspension_Event);
         end if;
         SCHD.Task_Gets_Ready (T_Mx_Prio);
         SCHD.Do_Scheduling;
      else
         --  No blocked tasks => just increment the semaphore value
         Sem.Value := Sem.Value + 1;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sem_Post;

   ------------------
   -- Sem_Getvalue --
   ------------------
   function Sem_Getvalue (Sem   : in     Semaphores.Semaphore_Ac;
                          Value : access Semaphore_Value)
                         return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         TOI.Set_POSIX_Error (OPERATION_NOT_IMPLEMENTED);
         return -1;
      end if;
      K.Enter_Critic_Section (Flags);
      if not Semaphore_OK (Sem) then
         TOI.Set_POSIX_Error_And_Leave_Critic_Section (INVALID_ARGUMENT,
                                                       Flags);
         return -1;
      end if;
      TOI.Reset_POSIX_Error;
      Value.all := Sem.Value;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Sem_Getvalue;

end MaRTE.Kernel.Semaphores.Operations;
