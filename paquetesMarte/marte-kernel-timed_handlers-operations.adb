------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--      'K e r n e l . T i m e d _ H a n d l e r s . O p e r a t i o n s'
--
--                                 Body
--
--
--  File 'kernel-timed_handlers-operations.adb'                        By MAR.
--
--
--
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
with MaRTE.Kernel.Timers; --  For 'CLOCK_REALTIME' and 'CLOCK_MONOTONIC'
with MaRTE.Kernel.Timers.Internals;
with MaRTE.POSIX_Constants;
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.HAL;
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Timed_Handlers.Internals;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timed_Handlers.Operations is

   package GC    renames MaRTE.POSIX_Constants;
   package TE_T  renames K.Timed_Events_And_Timer;
   package SCHD  renames K.Scheduler;
   package TMRI  renames K.Timers.Internals;
   package TE_LST           renames K.Timed_Events_Lists;
   package CPU_TE_LST       renames K.CPU_Time_Timed_Events_Lists;
   package CPU_TE_LST_ORDER renames K.CPU_Time_Timed_Events_Lists_Order;
   package DBG              renames K.Debug;

   ------------------------
   -- Remove_From_Queues --
   ------------------------
   --  A handler can be in one of these queues: scheduler queue, CPU time queue
   --  or pending timed handlers queue (Internals.Pending_Handlers_List). This
   --  procedure removes the handler from the queue it is queued (if any).

   procedure Remove_From_Queues (TE_Ac  : Timed_Handler_TE_Ac);
   pragma Inline (Remove_From_Queues);

   --------------------------------
   --  MaRTE_Timed_Handler_Init  --
   --------------------------------
   function MaRTE_Timed_Handler_Init
     (TE_Ac   : Timed_Handler_TE_Ac;
      Clock   : K.Timers.Clock_Id;
      Handler : Handler_Procedure;
      Area    : System.Address;
      Size    : Size_T) return Int is

      use type System.Address, MaRTE.Integer_Types.Size_T;

      Tmp_Timed_Handler_TE : aliased Timed_Handler_Timed_Event;

      procedure Memory_Copy (From  : access Timed_Handler_Timed_Event;
                             To    : Timed_Handler_TE_Ac;
                             Bytes : Integer);
      pragma Import (C, Memory_Copy, "memory_copy");

   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      if not TMRI.Clock_Id_OK (Clock)
        or else TE_Ac = null
        or else (Size > 0 and then Area = System.Null_Address)
        or else Handler = null
      then
         return INVALID_ARGUMENT;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.Magic /= INITIALIZED));

      --  To set properly the 'tag' in a timed handler created from a
      --  C program.
      Memory_Copy (From  => Tmp_Timed_Handler_TE'Access,
                   To    => TE_Ac,
                   Bytes => Timed_Handler_Timed_Event'Size / 8);

      --  Assign Timed_Event fields
      K.Timed_Event (TE_Ac.all) := (T => 0);

      --  Assign CPU_Time_Timed_Event fields
      K.CPU_Time_Timed_Event (TE_Ac.all) :=
        (TE_LST.Element (TE_Ac.all) with
         CPU_Time => 0,
         Group_Expiration_Time => 0,
         Is_Based_On_Group_Clock => TMRI.Is_Group_Clock_Id (Clock),
         Base_Clock => Clock,
         Task_Where_Queued => null,
         Armed             => False);

      --  Assign Timed handler fields
      TE_Ac.all :=
        (CPU_TE_LST.Element (TE_Ac.all) with
         Handler        => Handler,
         Area           => Area,
         Options        => 0,
         Period         => HAL.HWTime'Last,
         Overrun_Count  => 0,
         Enabled        => True,
         Queued         => False,
         Pending_Enable => False,
         Pending_Global => False,
         Magic          => INITIALIZED);
      --  Link task set to this timed event (if based on group-clock)
      if TMRI.Is_Group_Clock_Id (Clock) then
         TMRI.To_Task_Set_Id (Clock).Group_TE_Ac :=
           CPU_Time_Timed_Event_Ac (TE_Ac);
      end if;
      return 0;
   end MaRTE_Timed_Handler_Init;

   -----------------------------------
   --  MaRTE_Timed_Handler_Destroy  --
   -----------------------------------
   function MaRTE_Timed_Handler_Destroy (TE_Ac   : Timed_Handler_TE_Ac)
                                        return Int is
      use type K.Timers.Clock_Id;
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac /= null
         and then TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      --  If the clock isn't OK, it means it is a CPU time TH based on a clock
      --  of a task that has already finished, so we cannot remove it from any
      --  queue. This should change in the future, since we are loosing the
      --  oportunity to do a check on the clock because of this.
      if TMRI.Clock_Id_OK (TE_Ac.Base_Clock)  then
         --  Remove from system queues in case the handler is in one of them
         Remove_From_Queues (TE_Ac);
      end if;

      --  Unlink task set to the timed event (if based on group-clock)
      if TMRI.Is_Group_Clock_Id (TE_Ac.Base_Clock) then
         TMRI.To_Task_Set_Id (TE_Ac.Base_Clock).Group_TE_Ac := null;
      end if;

      --  Mark as not initialized
      TE_Ac.Magic := NOT_INITIALIZED;

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Destroy;

   -------------------------------
   --  MaRTE_Timed_Handler_Set  --
   -------------------------------
   function MaRTE_Timed_Handler_Set
     (TE_Ac   : Timed_Handler_TE_Ac;
      Options : MaRTE.Integer_Types.Unsigned_32;
      Time    : MaRTE.Timespec.Timespec_Ac) return Int is
      Flags : Integer;
      use type MaRTE.Timespec.Timespec_Ac;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null
        or else TE_Ac.Magic /= INITIALIZED
        or else Time = null
      then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      pragma Debug (DBG.Assert (TMRI.Clock_Id_OK (TE_Ac.Base_Clock)));
      if TE_Ac.Base_Clock /= K.Timers.CLOCK_REALTIME
        and then TE_Ac.Base_Clock /= K.Timers.CLOCK_MONOTONIC
        and then (Options and PERIODIC_HANDLER) /= 0
      then
         K.Leave_Critic_Section (Flags);
         --  Periodic timed events based on CPU-time clocks are not supported
         --  yet. If supported Internals.Execute_Timed_Handler should be
         --  changed to requeue CPU events (now only requeues standard events)
         return OPERATION_NOT_SUPPORTED;
      end if;

      --  If Time is null, the expiration time remains unchanged

      if Time /= null then
         TE_Ac.Options := Options;
         TE_Ac.T := MaRTE.Timespec.Timespec_To_HWTime (Time.all);
         TE_Ac.Period := TE_Ac.T; --  Only used in periodic handlers

         --  Set the event
         Set (TE_Ac.all, HAL.Get_HWTime);
      end if;

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Set;

   ----------------------------------------
   --  MaRTE_Timed_Handler_Set_Interval  --
   ----------------------------------------
   function MaRTE_Timed_Handler_Set_Interval
     (TE_Ac    : Timed_Handler_TE_Ac;
      Interval : MaRTE.Timespec.Timespec_Ac) return Int is
      Flags : Integer;
      use type MaRTE.Timespec.Timespec_Ac, HAL.HWTime;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null
        or else TE_Ac.Magic /= INITIALIZED
        or else Interval = null
      then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      pragma Debug (DBG.Assert (TMRI.Clock_Id_OK (TE_Ac.Base_Clock)));
      if (TE_Ac.Options and PERIODIC_HANDLER) /= 0 then
         K.Leave_Critic_Section (Flags);
         --  Not possible to add an interval to a periodic timer
         return OPERATION_NOT_SUPPORTED;
      end if;

      --  Set new expiration time

      TE_Ac.T := TE_Ac.T + MaRTE.Timespec.Timespec_To_HWTime (Interval.all);

      Set (TE_Ac.all, HAL.Get_HWTime);

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Set_Interval;

   ---------------------------------------
   --  MaRTE_Timed_Handler_Set_Handler  --
   ---------------------------------------
   function MaRTE_Timed_Handler_Set_Handler
     (TE_Ac   : Timed_Handler_TE_Ac;
      Handler : Handler_Procedure;
      Options : MaRTE.Integer_Types.Unsigned_32;
      Time    : MaRTE.Timespec.Timespec_Ac) return Int is
      Flags : Integer;
      Ret : Int;
      use type Int;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);

      --  Set new expiration time

      Ret := MaRTE_Timed_Handler_Set (TE_Ac, Options, Time);

      --  Set new handler

      if Ret = 0 and then Handler /= null then
         TE_Ac.Handler := Handler;
      end if;

      K.Leave_Critic_Section (Flags);

      return Ret;
   end MaRTE_Timed_Handler_Set_Handler;

   -----------------------------------
   --  MaRTE_Timed_Handler_Disable  --
   -----------------------------------
   function MaRTE_Timed_Handler_Disable (TE_Ac : Timed_Handler_TE_Ac)
                                        return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      TE_Ac.Enabled := False;

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Disable;

   ----------------------------------
   --  MaRTE_Timed_Handler_Enable  --
   ----------------------------------
   function MaRTE_Timed_Handler_Enable (TE_Ac : Timed_Handler_TE_Ac)
                                       return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if not TE_Ac.Enabled then

         TE_Ac.Enabled := True;

         if TE_Ac.Pending_Enable then
            --  The expiration time was reached while the event was disabled.
            --  The handler has to be executed now

            Internals.Execute_Timed_Handler (TE_Ac,
                                             From_Timer_Int_Handler => False,
                                             Now => HAL.Get_HWTime);
         end if;

      end if;  --  not TE_Ac.Enabled

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Enable;

   -----------------------------------------
   --  MaRTE_Timed_Handler_Global_Enable  --
   -----------------------------------------
   function MaRTE_Timed_Handler_Global_Enable return Int is
      Flags : Integer;
      TE_Ac : Timed_Handler_TE_Ac;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      K.Enter_Critic_Section (Flags);

      Internals.Execution_Allowed := True;

      --  Execute every pending handler
      TE_Ac := Timed_Handler_TE_Ac
        (K.Timed_Events_Lists.Head (Internals.Pending_Handlers_List));
      while TE_Ac /= null loop
         pragma Debug (DBG.Assert (TE_Ac.Pending_Global
           and not TE_Ac.Pending_Enable and not TE_Ac.Queued
           and not TE_Ac.Armed));

         Internals.Execute_Timed_Handler (TE_Ac,
                                          From_Timer_Int_Handler => False,
                                          Now => HAL.Get_HWTime);
         TE_Ac := Timed_Handler_TE_Ac
           (K.Timed_Events_Lists.Next (MaRTE.Kernel.Timed_Event_Ac (TE_Ac)));
      end loop;

      K.Leave_Critic_Section (Flags);

      return 0;
   end MaRTE_Timed_Handler_Global_Enable;

   ------------------------------------------
   --  MaRTE_Timed_Handler_Global_Disable  --
   ------------------------------------------
   function MaRTE_Timed_Handler_Global_Disable return Int is
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      Internals.Execution_Allowed := False;

      return 0;
   end MaRTE_Timed_Handler_Global_Disable;

   ---------------------------------------
   --  MaRTE_Timed_Handler_Get_Overrun  --
   ---------------------------------------
   function MaRTE_Timed_Handler_Get_Overrun (TE_Ac : Timed_Handler_TE_Ac)
                                            return Int is
   begin
      return Int (TE_Ac.Overrun_Count);
   end MaRTE_Timed_Handler_Get_Overrun;

   ---------------------------------
   -- MaRTE_Timed_Handler_Expired --
   ---------------------------------

   function MaRTE_Timed_Handler_Expired (TE_Ac   : Timed_Handler_TE_Ac;
                                         Expired : access Int)
                                         return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if TE_Ac.Armed then
         Expired.all := 0;
      else
         Expired.all := 1;
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end MaRTE_Timed_Handler_Expired;

   --------------------------------------------
   -- MaRTE_Timed_Handler_Time_To_Expiration --
   --------------------------------------------

   function MaRTE_Timed_Handler_Time_To_Expiration
     (TE_Ac : Timed_Handler_TE_Ac;
      TS    : MaRTE.Timespec.Timespec_Ac)
      return Int is
      Flags : Integer;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if TE_Ac.Armed then
         TS.all := MaRTE.Timespec.HWTime_To_Timespec
           (TMRI.Time_Remaining_To_CPU_Time_Event
              (K.CPU_Time_Timed_Event_Ac (TE_Ac), MaRTE.HAL.Get_HWTime));
      else
         TS.all := (Tv_Sec => 0, Tv_Nsec => 0);
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end MaRTE_Timed_Handler_Time_To_Expiration;

   -----------------------------------------
   -- MaRTE_Timed_Handler_Expiration_Time --
   -----------------------------------------

   function MaRTE_Timed_Handler_Expiration_Time
     (TE_Ac : Timed_Handler_TE_Ac;
      TS    : MaRTE.Timespec.Timespec_Ac) return Int is
      Flags : Integer;
      T : MaRTE.HAL.HWTime;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;
      pragma Debug (DBG.Assert (TE_Ac.all in Timed_Handler_Timed_Event'Class));

      K.Enter_Critic_Section (Flags);

      if TE_Ac = null or else TE_Ac.Magic /= INITIALIZED then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if TE_Ac.Base_Clock = K.Timers.CLOCK_REALTIME
        or else TE_Ac.Base_Clock = K.Timers.CLOCK_MONOTONIC
      then
         --  Standard event
         T := TE_Ac.T;
      elsif TE_Ac.Is_Based_On_Group_Clock then
         --  Based on group clock
         T := TE_Ac.Group_Expiration_Time;
      else
         --  Based on CPU-time clock
         T := TE_Ac.CPU_Time;
      end if;

      K.Leave_Critic_Section (Flags);

      TS.all := MaRTE.Timespec.HWTime_To_Timespec (T);
      return 0;
   end MaRTE_Timed_Handler_Expiration_Time;

   -----------
   --  Set  --
   -----------
   procedure Set (TE  : in out Timed_Handler_Timed_Event;
                  Now :        HAL.HWTime) is
      use type HAL.HWTime, K.Task_Id;
   begin
      if not MaRTE.Configuration_Parameters.Use_Timed_Handlers'First then
         return;
      end if;
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (TE in Timed_Handler_Timed_Event'Class));
      pragma Debug (DBG.Assert (TE.Magic = INITIALIZED));
      pragma Debug (DBG.Assert (TMRI.Clock_Id_OK (TE.Base_Clock)));

      --  Remove "old" event
      Remove_From_Queues (TE'Unrestricted_Access);

      --  For an expired and disabled event nothing needs to be
      --  done, it will be mark as "queued" at the end of this
      --  procedure.

      --  Set "new" timed event expiration time
      if TE.Base_Clock = K.Timers.CLOCK_REALTIME
        or else TE.Base_Clock = K.Timers.CLOCK_MONOTONIC
      then
         --  timed handler based on standard clock
         TMRI.Set_Time_Event
           (TE_Ac     => TE'Unrestricted_Access,
            T         => TE.T,
            Clock     => TE.Base_Clock,
            Abs_Time  => (TE.Options and ABSOLUTE_TIMER) /= 0 and then
                         (TE.Options and PERIODIC_HANDLER) = 0,
            Now       => Now);

      else
         --  timed handler based on CPU time clock or group clock
         pragma Debug (DBG.Assert (TMRI.Is_CPU_Clock (TE.Base_Clock) or
                         TMRI.Is_Group_Clock_Id (TE.Base_Clock)));
         pragma Debug (DBG.Assert ((TE.Options and PERIODIC_HANDLER) = 0));
         TMRI.Set_CPU_Time_Event
           (TE_Ac     => TE'Unrestricted_Access,
            T         => TE.T,
            Abs_Time  => (TE.Options and ABSOLUTE_TIMER) /= 0 and then
                         (TE.Options and PERIODIC_HANDLER) = 0,
            Now       => Now);

      end if;

      --  Program event or execute handler
      TE.Armed  := True;
      TE.Queued  := False;
      TE.Pending_Enable := False;
      TE.Pending_Global := False;

      if TE.T < Now then
         --  Expiration time is in the past: Execute handler
         Internals.Execute_Timed_Handler (TE'Unrestricted_Access,
                                          From_Timer_Int_Handler => False,
                                          Now => Now);
      else
         --  Program event
         TE.Queued := True;
         if TE.Base_Clock = K.Timers.CLOCK_REALTIME
           or else TE.Base_Clock = K.Timers.CLOCK_MONOTONIC
         then
            --  Enqueue standard event
            SCHD.Enqueue_Standard_Event_And_Update_Timer
              (TE'Unrestricted_Access, Now);

         elsif TMRI.To_Task_Id (TE.Base_Clock) = SCHD.Self or
           TMRI.To_Task_Set_Base_Ac (TE.Base_Clock) = SCHD.Self.Set then
            --  Even based on the CPU-time clock of the running task or on the
            --  group-clock of the running task
            TE.Task_Where_Queued := TCB_Ac (SCHD.Self);
            CPU_TE_LST_ORDER.Enqueue_In_Order
              (TE'Unrestricted_Access, SCHD.Self.CPU_Time_TEs_Q);
            TE_T.Update_Timer_Due_To_New_CPU_Event (TE'Unrestricted_Access,
                                                    Now);
         elsif not TE.Is_Based_On_Group_Clock then
            --  Timer based on the CPU-time of another task: just enqueue it
            TE.Task_Where_Queued :=  TCB_Ac (TMRI.To_Task_Id (TE.Base_Clock));
            CPU_TE_LST_ORDER.Enqueue_In_Order
              (TE'Unrestricted_Access,
               TMRI.To_Task_Id (TE.Base_Clock).CPU_Time_TEs_Q);

         else
            --  Timers based on a group-clock that doesn't include the running
            --  task will be queued in Do_Scheduling
            TE.Queued := False;
         end if;
      end if;
   end Set;

   ------------------------
   -- Remove_From_Queues --
   ------------------------

   procedure Remove_From_Queues (TE_Ac  : Timed_Handler_TE_Ac) is
   begin
      if TE_Ac.Queued then
         pragma Debug (DBG.Assert (not TE_Ac.Pending_Enable));
         pragma Debug (DBG.Assert (not TE_Ac.Pending_Global));
         pragma Debug (DBG.Assert (TE_Ac.Armed));

         --  Queued event waiting for expiration time: remove from
         --  timed events queue

         if TE_Ac.Base_Clock = K.Timers.CLOCK_REALTIME
           or else TE_Ac.Base_Clock = K.Timers.CLOCK_MONOTONIC
         then
            --  Standard event
            SCHD.Remove_Timed_Event (K.Timed_Event_Ac (TE_Ac));
         else
            --  timed handler based on CPU time clock or group clock
            pragma Debug (DBG.Assert (TMRI.Is_CPU_Clock (TE_Ac.Base_Clock) or
                TMRI.Is_Group_Clock_Id (TE_Ac.Base_Clock)));

            --  dequeue the event from the task it is queued (if any)
            if TE_Ac.Task_Where_Queued /= null then
               pragma Debug
                 (DBG.Assert (Task_OK (Task_Id (TE_Ac.Task_Where_Queued),
                                       Extra_Magic => K.TERMINATED)));
               pragma Debug
                 (DBG.Assert (CPU_TE_LST.Is_In_The_List
                  (CPU_Time_Timed_Event_Ac (TE_Ac),
                     TE_Ac.Task_Where_Queued.CPU_Time_TEs_Q)));
               CPU_TE_LST.Dequeue
                 (CPU_Time_Timed_Event_Ac (TE_Ac),
                  TE_Ac.Task_Where_Queued.CPU_Time_TEs_Q);
               TE_Ac.Task_Where_Queued := null;
            end if;
         end if;

         TE_Ac.Queued := False;

      elsif TE_Ac.Pending_Global then
         --  Remove from the 'Pending_Handlers_List'

         pragma Debug
           (DBG.Assert (K.Timed_Events_Lists.Is_In_The_List
            (K.Timed_Event_Ac (TE_Ac),
               Internals.Pending_Handlers_List)));
         K.Timed_Events_Lists.Dequeue
           (K.Timed_Event_Ac (TE_Ac), Internals.Pending_Handlers_List);

         TE_Ac.Pending_Global := False;
      end if;

      --  If the event is Pending_Enable nothing has to be done in order to
      --  remove it from the system, appart to be marked as NOT_INITIALIZED
   end Remove_From_Queues;

end MaRTE.Kernel.Timed_Handlers.Operations;
