------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . S c h e d u l e r'
--
--                                 Body
--
--
--  File 'kernel-scheduler.adb'                                       By MAR.
--
--
--  Scheduler operations.
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
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));

with MaRTE.Integer_Types; use MaRTE.Integer_Types;
pragma Elaborate_All (MaRTE.Integer_Types);

with MaRTE.Kernel;
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
--  pragma Elaborate
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.Kernel.Timed_Events_Queue;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
--  pragm
with MaRTE.Kernel.Timed_Handlers;
with MaRTE.Kernel.Timed_Handlers.Internals;
with MaRTE.Kernel.Replenishment_TE;
with MaRTE.Kernel.Condition_Variables.Internals;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Semaphores.Internals;
with MaRTE.Kernel.Tasks_Operations.Internals;
--  pragma Elaborate (Kernel.Tasks_Operations.Internals);
with MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Timers.Internals;
with MaRTE.Direct_IO;
with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.Stacks_Management;
with MaRTE.Kernel.Pool_TCBs;
with MaRTE.Kernel.Task_Sets;
with MaRTE.Kernel.Task_Sets.Internals;
with MaRTE.Kernel.Group_Clocks;

--  Debug
with MaRTE.Kernel.Scheduler.Debug;
with MaRTE.Kernel.Scheduler_CDbg; -- {CDBG}
with MaRTE.Kernel.Debug;
package body MaRTE.Kernel.Scheduler is

   package CP            renames MaRTE.Configuration_Parameters;
   package ML            renames K.Tasks_Map_Lists;
   package TSTE          renames K.Task_Suspension_Timed_Events;
   package APPSCHD       renames K.Application_Scheduler;
   package APPSCHD_DATA  renames K.Application_Scheduling_Data;
   package TO_APPSCHD    renames K.Tasks_Operations.Application_Scheduler;
   package TE_T          renames K.Timed_Events_And_Timer;
   package TEQ           renames K.Timed_Events_Queue;
   package TOI           renames K.Tasks_Operations.Internals;
   package CDBG          renames K.Scheduler_CDbg;
   package TS            renames K.Task_Sets;
   package TSI           renames K.Task_Sets.Internals;
   package GCLK          renames K.Group_Clocks;
   package TMR           renames K.Timers;
   package TMRI          renames K.Timers.Internals;
   package TH            renames K.Timed_Handlers;
   package CVI           renames K.Condition_Variables.Internals;
   package MI            renames K.Mutexes.Internals;
   package SEMI          renames K.Semaphores.Internals;
   package DBG           renames K.Debug;
   package MLST          renames K.Mutexes_Lists;

   use HAL,
     K.Tasks_Lists,
     K.Timed_Events_Lists,
     K.CPU_Time_Timed_Events_Lists,
     K.CPU_Time_Timed_Events_Lists_Order;

   use type Int;

   Initialized : Boolean := False;

   function UC (Base : K.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_DATA.UC_To_AppScheduler_Data_Ac;
   pragma Inline (UC);
   function UC (Base : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;
   pragma Inline (UC);

   -------------------------
   -- Round_Robin_Quantum --
   -------------------------

   Round_Robin_Quantum : constant Duration :=
     CP.Round_Robin_Interval_Time_Period;
   --  Not used internally, just to be imported from the GNAT run-time

   ------------------
   -- Running_Task --
   ------------------
   Running_Task : Task_Id;

   -----------------
   -- Ready Queue --
   -----------------
   Ready_Queue : ML.Map_List;

   -----------------------------
   -- Interrupt_Nesting_Count --
   -----------------------------
   --
   --  Hardware interrupt nesting counter (modified in the interrupt
   --  handlers in 'x86/boot/base_irq_inittab.S'), used to ensure that the
   --  software interrupt handler ('Do_Scheduling') isn't called until all
   --  outstanding hardware interrupts have been processed. In addition,
   --  this byte also acts as the software interrupt pending flag: if the
   --  high bit is set, no software interrupt is pending; if the high bit
   --  is clear, a software interrupt is pending. This design allows the
   --  interrupt path to decide whether to call the software interrupt
   --  handler simply by testing the word for zero.
   Interrupt_Nesting_Count : Unsigned_8;
   pragma Import (C, Interrupt_Nesting_Count, "base_irq_nest");
   --  'base_irq_nest' is defined in 'x86/boot/base_irq.c'.
   Outside_Of_Interrupts : constant := 16#80#;
   Invoke_Do_Scheduling_Mask : constant := 16#7F#; -- not 16#80#;

   ----------
   -- Self --
   ----------
   function Self return Task_Id is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (Debug.Assert_Task_Is_Ok (Running_Task));
      return Running_Task;
   end Self;

   ------------------------------------------------
   -- Dequeue_Task_Ending_Reenqueuement_Deferred --
   ------------------------------------------------
   procedure Dequeue_Task_Ending_Reenqueuement_Deferred (T : in Task_Id);
   procedure Dequeue_Task_Ending_Reenqueuement_Deferred (T : in Task_Id) is
      Real_Active_Prio : Task_Priority;
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status /= READY));
      pragma Debug (DBG.Assert (T.Reenqueuement_Deferred));
      Real_Active_Prio := T.Active_Prio;
      T.Active_Prio    := T.Old_Active_Prio;
      ML.Dequeue_First_In_Priority (T, Ready_Queue);

      T.Active_Prio    := Real_Active_Prio;
      T.Active_Preemption_Level := T.Old_Active_Preemption_Level;
      T.Reenqueuement_Deferred := False;
   end Dequeue_Task_Ending_Reenqueuement_Deferred;
   pragma Inline (Dequeue_Task_Ending_Reenqueuement_Deferred);

   -----------------------------------------
   -- Schedule_SS_Replenishment_Operation --
   -----------------------------------------
   procedure Schedule_SS_Replenishment_Operation
     (Now : in HWTime; Limit_Reached : in Boolean := False);
   procedure Schedule_SS_Replenishment_Operation
     (Now : in HWTime; Limit_Reached : in Boolean := False) is
      Repl_Event : Replenishment_TE.Replenishment_Timed_Event_Ac;
   begin
      Running_Task.Number_Of_Pending_Replenishments :=
        Running_Task.Number_Of_Pending_Replenishments + 1;

      Repl_Event := Replenishment_TE.Request;

      Repl_Event.SS_Task := Running_Task;
      Repl_Event.T := Running_Task.Activation_Time
        + Running_Task.Sched_SS_Repl_Period;
      if Limit_Reached or else Running_Task.Scheduler_Timed_Event.T < Now then
         Repl_Event.Replenish_Amount :=
           Running_Task.Capacity - Running_Task.Replenish_Amount_Pending;
         Running_Task.Replenish_Amount_Pending := Running_Task.Capacity;
      else
         Repl_Event.Replenish_Amount :=
           Running_Task.Capacity - (Running_Task.Scheduler_Timed_Event.T - Now)
           - Running_Task.Replenish_Amount_Pending;
         Running_Task.Replenish_Amount_Pending :=
           Running_Task.Replenish_Amount_Pending + Repl_Event.Replenish_Amount;
      end if;

      pragma Debug -- {CDBG}
        (CDBG.Schedule_SS_Replenishment_Operation -- {CDBG}
         (Running_Task.Activation_Time, Repl_Event.T, -- {CDBG}
          Repl_Event.Replenish_Amount, -- {CDBG}
          Running_Task.Replenish_Amount_Pending)); -- {CDBG}

      TE_T.Enqueue_Standard_Event_Without_Updating_Timer
        (Timed_Event_Ac (Repl_Event));
   end Schedule_SS_Replenishment_Operation;
   pragma Inline (Schedule_SS_Replenishment_Operation);

   --------------------------------------------------
   -- Reenqueue_Task_Ending_Reenqueuement_Deferred --
   --------------------------------------------------
   procedure Reenqueue_Task_Ending_Reenqueuement_Deferred (T : in Task_Id);
   procedure Reenqueue_Task_Ending_Reenqueuement_Deferred (T : in Task_Id) is
      Real_Active_Prio : Task_Priority;
   begin
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status = READY));
      pragma Debug (DBG.Assert (T.Reenqueuement_Deferred));
      Real_Active_Prio := T.Active_Prio;
      T.Active_Prio    := T.Old_Active_Prio;
      ML.Dequeue_First_In_Priority (T, Ready_Queue);

      T.Active_Prio    := Real_Active_Prio;
      T.Active_Preemption_Level := T.Old_Active_Preemption_Level;
      ML.Enqueue_First_In_Priority (T, Ready_Queue);
      --  I can use 'Enqueue_First_In_Priority' instead of
      --  'Enqueue_In_Order' because I know there isn't another task in the
      --  'Running_Task' active priority queue, otherwise the reenqueuement
      --  deferred would has been already ended.
      T.Reenqueuement_Deferred := False;
   end Reenqueue_Task_Ending_Reenqueuement_Deferred;
   pragma Inline (Reenqueue_Task_Ending_Reenqueuement_Deferred);

   ------------------------
   -- Register_Main_Task --
   ------------------------
   procedure Register_Main_Task (Main : in out Task_Id);
   procedure Register_Main_Task (Main : in out Task_Id) is
   begin
      Running_Task := Main;

      --  Set starting time of running task

      Main.Running_Starting_Time := HAL.Get_HWTime;

      --  Put Main Task ready

      Sched_Fifo_Task_Gets_Ready (Main);
   end Register_Main_Task;

   ------------------------
   -- Register_Idle_Task --
   ------------------------
   procedure Register_Idle_Task (Idle : in Task_Id);
   procedure Register_Idle_Task (Idle : in Task_Id) is
   begin
      --  Idle Task in Ready Queue
      Idle.Status := READY;
      ML.Set_Last_Element (Idle, Ready_Queue);
   end Register_Idle_Task;

   ----------------------------------------------------------------------------
   -- Functions and procedures of the interface -------------------------------
   ----------------------------------------------------------------------------

   ----------------------
   -- New_Task_Created --
   ----------------------
   procedure New_Task_Created (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert
                      (Task_OK (T, NEW_APPSCHED) and then
                       (T.Status = BLOCKED or T.Status = APPSCHED_SUSPENDED)));

      case T.Sched_Policy is
         when SCHED_FIFO | SCHED_RR | SCHED_SPORADIC | SCHED_OTHER
            | SCHED_EDF=>

            if T.Sched_Policy = SCHED_EDF then
               --  Set first deadline

               T.Base_Urgency :=
                 K.Task_Urgency (HAL.HWTime'Last
                                 - (HAL.Get_HWTime + T.Rel_Deadline));
               T.Active_Urgency := T.Base_Urgency;
            end if;

            --  Put the task Ready

            Task_Gets_Ready (T);

            if T.Sched_Policy = SCHED_SPORADIC then
               --  Enqueue scheduler event in the CPU timed events queue. This
               --  event is set to expire when the capacity of the task
               --  finishes.
               T.Scheduler_Timed_Event.CPU_Time :=
                 T.Used_CPU_Time + T.Capacity;
               T.Scheduler_Timed_Event.Task_Where_Queued := TCB_Ac (T);
               CPU_Time_Timed_Events_Lists_Order.Enqueue_In_Order
                 (T.Scheduler_Timed_Event'Access, T.CPU_Time_TEs_Q);

               --  Of course the task starts with capacity remaining
               T.Capacity_Exhausted := False;
            end if;

         when SCHED_APP =>
            --  Suspend father task waiting for appscheduler response
            --  (only if the NEW event is unmasked for the appscheduler)
            --  and send event to the appscheduler.
            APPSCHD.Send_Event_From_Task
              (Event_Code => APPSCHD_DATA.APPSCHED_NEW,
               ST         => UC (T.Sched_App).Scheduler,
               T          => T);
      end case;
   end New_Task_Created;

   -------------------------------
   -- Running_Task_Gets_Blocked --
   -------------------------------
   procedure Running_Task_Gets_Blocked (With_Status : in Task_Status) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (Running_Task, TERMINATED)));
      pragma Debug (DBG.Assert (Running_Task.Status = READY));
      pragma Debug (DBG.Assert
                      (not (Running_Task.Task_Type = SIGNAL_HANDLER_TASK and
                            With_Status /= SIG_HANDLER_WAIT_TO_DELIVER)));
      pragma Debug (DBG.Assert
                      (Interrupt_Nesting_Count = Outside_Of_Interrupts));
      pragma Debug (CDBG.Task_Gets_Blocked (Running_Task)); -- {CDBG}

      Running_Task.Status := With_Status;

      if Running_Task.Reenqueuement_Deferred then
         Dequeue_Task_Ending_Reenqueuement_Deferred (Running_Task);
      else
         ML.Dequeue_Head (Running_Task, Ready_Queue);
      end if;

      case Running_Task.Sched_Policy is
         when SCHED_FIFO | SCHED_OTHER | SCHED_EDF =>
            --  Is 'Running_Task' an appscheduler ?
            if (Running_Task.AppScheduler /= null  and then
                not APPSCHD_DATA.Sched_App_Data_Lists.Is_Empty
                (UC (Running_Task.AppScheduler).Suspension_Deferred_Tasks))
            then
               --  When an appscheduler gets blocked every task in its
               --  'Suspension_Deferred_Tasks' list has to be suspended as
               --  well.
               declare
                  package SCHDAPP_DATA_LST
                    renames APPSCHD_DATA.Sched_App_Data_Lists;
                  use SCHDAPP_DATA_LST;
                  Suspension_Deferred_Tasks : SCHDAPP_DATA_LST.List renames
                    UC (Running_Task.AppScheduler).Suspension_Deferred_Tasks;
               begin
                  loop
                     pragma Debug
                       (DBG.Assert
                          (Task_OK (Head (Suspension_Deferred_Tasks).Owner)));

                     Non_Running_Task_Gets_Blocked
                       (Head (Suspension_Deferred_Tasks).Owner,
                        APPSCHED_SUSPENDED);

                     Dequeue_Head (Suspension_Deferred_Tasks);
                     exit when Is_Empty (Suspension_Deferred_Tasks);
                  end loop;
               end;
            end if;
         when SCHED_RR =>
            Running_Task.Scheduler_Timed_Event.Task_Where_Queued := null;
            CPU_Time_Timed_Events_Lists.Dequeue
              (Running_Task.Scheduler_Timed_Event'Access,
               Running_Task.CPU_Time_TEs_Q);
         when SCHED_SPORADIC =>
            if not Running_Task.Capacity_Exhausted then
               --  Limit of replenishments reached ?
               if (Running_Task.Number_Of_Pending_Replenishments >=
                   Running_Task.Sched_SS_Max_Repl - 1) then
                  --  Schedule "limit reached" Replenishment Operation
                  Schedule_SS_Replenishment_Operation (Now => Get_HWTime,
                                                       Limit_Reached => True);
                  --  Dequeue the excution CPU time limit event.
                  Running_Task.Scheduler_Timed_Event.Task_Where_Queued := null;
                  CPU_Time_Timed_Events_Lists.Dequeue
                    (Running_Task.Scheduler_Timed_Event'Access,
                     Running_Task.CPU_Time_TEs_Q);
                  --  Reduce priority
                  TOI.Set_Base_Prio (Running_Task,
                                     Running_Task.Sched_SS_Low_Priority);
                  Running_Task.Capacity_Exhausted := True;
               else
                  --  Schedule normal Replenishment Operation
                  Schedule_SS_Replenishment_Operation (Now => Get_HWTime);
               end if;
            end if;
         when SCHED_APP =>
            if (With_Status /= APPSCHED_SUSPENDED             and
                With_Status /= WAITING_TO_LOCK_APPSCHED_MUTEX and
                With_Status /= TRYING_TO_LOCK_APPSCHED_MUTEX  and
                With_Status /= WAITING_APPSCHEDULER_RESPONSE) then
               APPSCHD.Send_Event_From_Task
                 (Event_Code => APPSCHD_DATA.APPSCHED_BLOCK,
                  ST         => UC (Running_Task.Sched_App).Scheduler,
                  T          => Running_Task);
            end if;
            --  When the suspend reason is 'WAITING_TO_LOCK_APPSCHED_MUTEX'
            --  or 'TRYING_TO_LOCK_APPSCHED_MUTEX' the events are sent from
            --  functions in 'Kernel.Mutexes'.
      end case;
   end Running_Task_Gets_Blocked;

   -----------------------------------
   -- Non_Running_Task_Gets_Blocked --
   -----------------------------------
   --  By now only used for 'SCHED_APP' tasks. It do NOT send a BLOCK
   --  event to the appscheduler.
   procedure Non_Running_Task_Gets_Blocked (T           : in Task_Id;
                                            With_Status : in Task_Status) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T, TERMINATED)));
      pragma Debug (DBG.Assert (T.Sched_Policy = SCHED_APP or
                       With_Status = WAITING_POLICY_CHANGE_TO_SCHED_APP));
      pragma Debug (DBG.Assert (T.Status = READY));
      pragma Debug (CDBG.Task_Gets_Blocked (T)); -- {CDBG}

      T.Status := With_Status;

      if T.Reenqueuement_Deferred then
         Dequeue_Task_Ending_Reenqueuement_Deferred (T);
      else
         ML.Dequeue (T, Ready_Queue);
      end if;
   end Non_Running_Task_Gets_Blocked;

   --------------------------------
   -- Sched_Fifo_Task_Gets_Ready --
   --------------------------------
   procedure Sched_Fifo_Task_Gets_Ready (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (K.Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status /= READY));
      pragma Debug (DBG.Assert (not T.Reenqueuement_Deferred));
      pragma Debug (DBG.Assert (T.Sched_Policy = SCHED_FIFO or
                                T.Sched_Policy = SCHED_OTHER));
      pragma Debug (CDBG.Task_Gets_Ready (T)); -- {CDBG}

      T.Status := READY;
      ML.Enqueue_In_Order (T, Ready_Queue);
   end Sched_Fifo_Task_Gets_Ready;

   ---------------------
   -- Task_Gets_Ready --
   ---------------------
   procedure Task_Gets_Ready (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (K.Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status /= READY));
      pragma Debug (DBG.Assert (not T.Reenqueuement_Deferred));

      case T.Sched_Policy is
         when SCHED_FIFO | SCHED_OTHER =>
            null;

         when SCHED_EDF =>
            if T.New_Deadline_For_Next_Activation then
               --  Set new deadline

               T.Base_Urgency := K.Task_Urgency (HAL.HWTime'Last - T.Deadline);
               T.Active_Urgency := T.Base_Urgency;
               T.New_Deadline_For_Next_Activation := False;
            end if;

         when SCHED_RR =>
            T.Scheduler_Timed_Event.CPU_Time := T.Used_CPU_Time + T.Capacity;
            T.Scheduler_Timed_Event.Task_Where_Queued := TCB_Ac (T);
            CPU_Time_Timed_Events_Lists_Order.Enqueue_In_Order
              (T.Scheduler_Timed_Event'Access, T.CPU_Time_TEs_Q);

         when SCHED_SPORADIC =>
            if not T.Capacity_Exhausted then
               T.Activation_Time := Get_HWTime;
            end if;

         when SCHED_APP =>
            if (T.Status /= APPSCHED_SUSPENDED and
                T.Status /= WAITING_TO_LOCK_APPSCHED_MUTEX and
                T.Status /= TRYING_TO_LOCK_APPSCHED_MUTEX and
                T.Status /= WAITING_APPSCHEDULER_RESPONSE) then
               --  Only send notification to the application scheduler in
               --  the case the task activation hasn't been ordered by it.
               T.Status := APPSCHED_SUSPENDED;
               APPSCHD.Send_Notification_Or_Ready
                 (Event_Code => APPSCHD_DATA.APPSCHED_READY,
                  ST         => UC (T.Sched_App).Scheduler,
                  T          => T);
            else
               pragma Debug (CDBG.Task_Gets_Ready (T)); -- {CDBG}
               T.Status := READY;
               ML.Enqueue_In_Order (T, Ready_Queue);
            end if;
      end case;

      if T.Sched_Policy /= SCHED_APP then
         pragma Debug (CDBG.Task_Gets_Ready (T)); -- {CDBG}
         T.Status := READY;
         if T.Task_Type = SIGNAL_HANDLER_TASK then
            ML.Enqueue_First_In_Priority (T, Ready_Queue);
         else
            ML.Enqueue_In_Order (T, Ready_Queue);
         end if;
      end if;
   end Task_Gets_Ready;

   --------------------------------
   -- Ready_Task_Changes_Urgency --
   --------------------------------
   --
   --  Assumes urgency has already been changed in task
   procedure Ready_Task_Changes_Urgency (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status = READY));
      ML.Dequeue (T, Ready_Queue);
      ML.Enqueue_In_Order (T, Ready_Queue);
   end Ready_Task_Changes_Urgency;

   ---------------------------------------
   -- Ready_Task_Raises_Active_Priority --
   ---------------------------------------
   procedure Ready_Task_Raises_Active_Priority (T        : in Task_Id;
                                                New_Prio : in Task_Priority) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status = READY));
      pragma Debug (CDBG.Ready_Task_Raises_Active_Priority  -- {CDBG}
                      (T, New_Prio)); -- {CDBG}

      if T.Reenqueuement_Deferred then
         T.Active_Prio := New_Prio;
         Reenqueue_Task_Ending_Reenqueuement_Deferred (T);
      else
         ML.Dequeue (T, Ready_Queue);
         T.Active_Prio := New_Prio;
         ML.Enqueue_In_Order (T, Ready_Queue);
      end if;
   end Ready_Task_Raises_Active_Priority;

   ----------------------------------------
   -- Ready_Task_Reduces_Active_Priority --
   ----------------------------------------
   procedure Ready_Task_Reduces_Active_Priority
     (T                  : in Task_Id;
      New_Prio           : in Task_Priority;
      Head_Of_New_Prio_Q : in Boolean) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Status = READY));
      pragma Debug (CDBG.Ready_Task_Reduces_Active_Priority -- {CDBG}
                      (T, New_Prio)); -- {CDBG}

      if T.Reenqueuement_Deferred then
         T.Active_Prio := New_Prio;
      else
         ML.Dequeue (T, Ready_Queue);
         T.Active_Prio := New_Prio;
         if Head_Of_New_Prio_Q then
            ML.Enqueue_First_In_Priority (T, Ready_Queue);
         else
            ML.Enqueue_In_Order (T, Ready_Queue);
         end if;
      end if;
   end Ready_Task_Reduces_Active_Priority;

   -------------------------
   -- Task_Changes_Policy --
   -------------------------
   procedure Task_Changes_Policy (T      : in Task_Id;
                                  Policy : in TO.Scheduling_Policies) is
      Now : constant HAL.HWTime := HAL.Get_HWTime;
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      if T.Sched_Policy /= Policy then
         --  Actions due to former policy
         case T.Sched_Policy is
            when SCHED_FIFO | SCHED_OTHER =>
               null;

            when SCHED_EDF =>
               --  Stores current deadline just in case the task is converted
               --  again in SCHED_EDF in the future and TO.Pthread_Setdeadline
               --  is not called in between

               T.Deadline := HAL.HWTime'Last - HAL.HWTime (T.Base_Urgency);

            when SCHED_RR | SCHED_SPORADIC =>
               if Is_In_The_List (T.Scheduler_Timed_Event'Access,
                                  T.CPU_Time_TEs_Q) then
                  T.Scheduler_Timed_Event.Task_Where_Queued := null;
                  Dequeue (T.Scheduler_Timed_Event'Access, T.CPU_Time_TEs_Q);
               end if;
            when SCHED_APP =>
               APPSCHD.Send_Event_From_Task
                 (Event_Code => APPSCHD_DATA.APPSCHED_TERMINATE,
                  ST         => UC (T.Sched_App).Scheduler,
                  T          => T);
               UC (T.Sched_App).Scheduler := null;
         end case;
         --  Actions due to new policy
         case Policy is
            when SCHED_FIFO | SCHED_OTHER =>
               --  Urgency is not used in this policies

               T.Base_Urgency := 0;
               T.Active_Urgency := 0;
            when SCHED_EDF =>
               --  Set first deadline

               T.Base_Urgency := K.Task_Urgency (HAL.HWTime'Last - T.Deadline);
               T.Active_Urgency := T.Base_Urgency;
            when SCHED_RR | SCHED_SPORADIC =>
               --  Urgency is not used in this policies

               T.Base_Urgency := 0;
               T.Active_Urgency := 0;
               --  Set Scheduler_Timed_Event (RR quantum or SS capacity)
               --  (Notice that changing SCHED_SPORADIC task's parameters is
               --  only allowed when there are no pending replenishment
               --  operations)
               if T = Running_Task then
                  T.Scheduler_Timed_Event.CPU_Time := T.Used_CPU_Time
                    + Now - T.Running_Starting_Time
                    + T.Capacity;
                  T.Scheduler_Timed_Event.T := Now + T.Capacity;
                  Enqueue_In_Order (T.Scheduler_Timed_Event'Access,
                                    T.CPU_Time_TEs_Q);
                  TE_T.Update_Timer_Due_To_New_CPU_Event
                    (T.Scheduler_Timed_Event'Access, Now);
               else
                  T.Scheduler_Timed_Event.CPU_Time :=
                    T.Used_CPU_Time + T.Capacity;
                  Enqueue_In_Order (T.Scheduler_Timed_Event'Access,
                                    T.CPU_Time_TEs_Q);
               end if;
               T.Scheduler_Timed_Event.Task_Where_Queued := TCB_Ac (T);
            when SCHED_APP =>
               null;
         end case;
      end if;
   end Task_Changes_Policy;

   --------------------------------------
   -- New_Sched_Param_In_AppSched_Task --
   --------------------------------------
   procedure New_Sched_Param_In_AppSched_Task (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));
      pragma Debug (DBG.Assert (T.Sched_Policy = SCHED_APP));

      if UC (T.Sched_App).Scheduler /= Running_Task then
         --  Send the event to the scheduler only if the parameters
         --  are not changed from the appscheduler its self
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM,
            ST         => UC (T.Sched_App).Scheduler,
            T          => T);
      end if;
   end New_Sched_Param_In_AppSched_Task;

   -------------------------------------------
   --  Trying_To_Change_Policy_To_Sched_App --
   -------------------------------------------
   procedure Trying_To_Change_Policy_To_Sched_App (T : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (Task_OK (T)));

      --  Suspend changed task until appscheduler's response (unless change
      --  is being performed from the task itself)
      if T /= Running_Task and then T.Status = READY then
         Non_Running_Task_Gets_Blocked (T, WAITING_POLICY_CHANGE_TO_SCHED_APP);
      end if;
      --  Suspend task that changes the policy (if the NEW event is
      --  unmasked for the appscheduler) and send event to the appscheduler.
      APPSCHD.Send_Event_From_Task
        (Event_Code => APPSCHD_DATA.APPSCHED_NEW,
         ST         => UC (T.Sched_App).Scheduler,
         T          => T);
   end Trying_To_Change_Policy_To_Sched_App;

   -----------------
   -- Remove_Task --
   -----------------
   --
   --  Removes the task from the ready queue and also removes its asociated
   --  events from the timed events queue.
   procedure Remove_Task (T : in Task_Id) is
      Event_Ac : K.Timed_Event_Ac;
      I : TEQ.Index;
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (K.Task_Terminated (T)));

      if T.Status = READY then
         ML.Dequeue (T, Ready_Queue);
      elsif T.Status = TIME_SUSPENDED or T.Status = TIMED_WAITING_CV then
         TE_T.Remove_Standard_Event (T.Suspension_Event);
      end if;

      --  Delete task from its task set and group clock
      if T.Set /= null then
         TSI.Delete_Task_From_Its_Set (T);
      end if;

      case T.Sched_Policy is
         when SCHED_FIFO | SCHED_RR | SCHED_OTHER | SCHED_EDF =>
            if T.AppScheduler /= null then
               --  Set to null the field 'App_Scheduler' of its scheduled task
               declare
                  TC : K.Task_Container_Ac;
                  use K.Task_Containers_Lists;
               begin
                  TC := Head (UC (T.AppScheduler).Tasks_Q);
                  while TC /= null loop
                     UC (TC.T.Sched_App).Scheduler := null;
                     TC := Next (TC);
                  end loop;
               end;
            end if;
         when SCHED_SPORADIC =>
            --  Look for the replenishment events of 'T' and remove them
            I := 1;
            loop
               Event_Ac := TEQ.Timed_Event_In_Pos (I);
               exit when Event_Ac = null;
               if ((Event_Ac.all
                    in Replenishment_TE.Replenishment_Timed_Event'Class)
                   and then
                   Replenishment_TE.Replenishment_Timed_Event_Ac
                   (Event_Ac).SS_Task = T) then
                  --  Remove the event. 'I' isn't incremented because when
                  --  an element is removed the rest of elements in the
                  --  'TEQ' are shifted to occupy the empty position.
                  TEQ.Remove (Event_Ac);
               else
                  exit when I = TEQ.Index'Last;
                  I := I + 1;
               end if;
            end loop;
         when SCHED_APP =>
            APPSCHD.Send_Event_From_Task
              (Event_Code => APPSCHD_DATA.APPSCHED_TERMINATE,
               ST         => UC (T.Sched_App).Scheduler,
               T          => T);
      end case;
   end Remove_Task;

   -----------------------------
   -- Running_Task_Yields_CPU --
   -----------------------------
   procedure Running_Task_Yields_CPU is
   begin
      pragma Debug (DBG.Assert (Initialized));

      if (Running_Task.Sched_Policy /= SCHED_APP or else
            APPSCHD_DATA.Event_In_Set
              (APPSCHD_DATA.APPSCHED_YIELD,
               UC (UC (Running_Task.Sched_App).Scheduler.AppScheduler).
                 Events_Mask))
      then
         --  Non-SCHED_APP tasks or SCHED_APP tasks with the
         --  APPSCHED_YIELD event masked have to be reenqueued.
         if Running_Task.Reenqueuement_Deferred then
            Reenqueue_Task_Ending_Reenqueuement_Deferred (Running_Task);
         else
            ML.Reenqueue_Head_In_Order (Running_Task, Ready_Queue);
         end if;
      end if;
      case Running_Task.Sched_Policy is
         when SCHED_FIFO | SCHED_SPORADIC | SCHED_OTHER | SCHED_EDF =>
            null;
         when SCHED_RR =>
            Dequeue (Running_Task.Scheduler_Timed_Event'Access,
                     Running_Task.CPU_Time_TEs_Q);
            Running_Task.Scheduler_Timed_Event.CPU_Time :=
              Running_Task.Scheduler_Timed_Event.CPU_Time
              + Running_Task.Capacity;
            Running_Task.Scheduler_Timed_Event.T :=
              Running_Task.Scheduler_Timed_Event.T + Running_Task.Capacity;
            Enqueue_In_Order (Running_Task.Scheduler_Timed_Event'Access,
                              Running_Task.CPU_Time_TEs_Q);
         when SCHED_APP =>
            APPSCHD.Send_Event_From_Task
              (Event_Code => APPSCHD_DATA.APPSCHED_YIELD,
               ST         => UC (Running_Task.Sched_App).Scheduler,
               T          => Running_Task);
      end case;
   end Running_Task_Yields_CPU;

   ----------------------------------------------------------
   --  Global variables used by 'Timer_Interrupt_Handler'  --
   --  and 'Do_Scheduling'                                 --
   ----------------------------------------------------------
   CS_Time : HAL.HWTime; -- Context switch time
   After_Timed_Event_Expiration : Boolean := False;

   -----------------------------
   -- Timer_Interrupt_Handler --
   -----------------------------
   procedure Timer_Interrupt_Handler
     (State : in MaRTE.HAL.Trap_State_Ac);
   procedure Timer_Interrupt_Handler
     (State : in MaRTE.HAL.Trap_State_Ac) is
      Event : Timed_Event_Ac;
      subtype S_Ac is TSTE.Task_Suspension_TE_Ac;
      subtype R_Ac is K.Replenishment_TE.Replenishment_Timed_Event_Ac;
      type NTE_Ac is access all APPSCHD_DATA.Notification_Timed_Event;
      type ATE_Ac is access all APPSCHD_DATA.Activation_Timed_Event;
      subtype TCPU_Ac is K.CPU_Time_Timed_Event_Ac;
      subtype TH_Ac is K.Timed_Handlers.Timed_Handler_TE_Ac;
   begin
      --  It is an error being here with interrupts enabled
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (Interrupt_Nesting_Count = 16#81#));
      K.Enter_Critic_Section_From_Interrupt;

      CS_Time := TE_T.Get_Timer_Activation_Time;

      Event := TE_T.Extract_Expired_Event (Running_Task);

      if Event = null then
         --  Periodic timer interrupt. Just reprogram the timer again.
         TE_T.Reprogram_Timer_After_Expiration (Running_Task);
         K.Leave_Critic_Section_From_Interrupt;
         return;

      else
         loop
            After_Timed_Event_Expiration := True;

            if Event.all in TSTE.Task_Suspension_Timed_Event'Class then

               ------------
               --  Task suspension timed event
               ----
               pragma Debug (DBG.Assert
                               (Task_OK (S_Ac (Event).Suspended_Task)));
               pragma Debug -- {CDBG}
                 (CDBG.Timer_Interrupt_Handler_TSTE -- {CDBG}
                  (S_Ac (Event), CS_Time)); -- {CDBG}

               case S_Ac (Event).Reason is
                  when TSTE.CV_TIMEDWAIT =>
                     CVI.Timeout_Reached (S_Ac (Event).Suspended_Task,
                                          S_Ac (Event).CV_Where_Blocked);

                  when TSTE.MUTEX_TIMEDWAIT =>
                     MI.Timeout_Reached (S_Ac (Event).Suspended_Task,
                                         S_Ac (Event).Mutex_Where_Blocked);

                  when TSTE.SEM_TIMEDWAIT =>
                     SEMI.Timeout_Reached (S_Ac (Event).Suspended_Task,
                                           S_Ac (Event).Sem_Where_Blocked);

                  when TSTE.SUSPENSION =>
                     Task_Gets_Ready (S_Ac (Event).Suspended_Task);

                  when TSTE.APPSCHED_EVENT_TIMEDWAIT =>
                     APPSCHD.Send_Timeout_Event (S_Ac (Event).Suspended_Task);

                  when TSTE.HWINTR_TIMEDWAIT =>
                     K.Hardware_Interrupts.Timeout_Reached
                       (S_Ac (Event).Suspended_Task);
               end case;

            elsif Event = Running_Task.Scheduler_Timed_Event'Access then

               ------------
               --  Scheduler timed event (RR and SS)
               ----
               pragma Debug (DBG.Assert
                               (Task_OK (Running_Task) and then
                                (Running_Task.Sched_Policy = SCHED_RR or else
                                 Running_Task.Sched_Policy = SCHED_SPORADIC)));
               pragma Debug
                 (DBG.Assert
                    (TMRI.Clock_Id_OK (Running_Task.
                                       Scheduler_Timed_Event.Base_Clock)
                     and K.CPU_TE_OK (Running_Task.
                                           Scheduler_Timed_Event'Access)));
               pragma Debug -- {CDBG}
                 (CDBG.Timer_Interrupt_Handler_STE -- {CDBG}
                  (Running_Task, Event, CS_Time)); -- {CDBG}
               case Running_Task.Sched_Policy is
                  when SCHED_RR =>
                     ------------
                     --  End of RR slice
                     ----
                     if not Running_Task.Reenqueuement_Deferred then
                        if Running_Task.RR_Quantum_Enabled
                          and then MLST.Is_Empty (Running_Task.Mutexes_Owned)
                        then
                           --  No mutexes owned by task and quantum enabled:
                           --  task is sent to the tail of its priority queue

                           ML.Reenqueue_Head_In_Order (Running_Task,
                                                       Ready_Queue);
                        else
                           --  Mark as pending to be sent to the tail of its
                           --  priority queue. It will happen when as soon as
                           --  the task unlocks its last mutex and quantum is
                           --  enabled

                           Running_Task.End_Of_RR_Slice_Pending := True;
                        end if;
                     end if;
                     --  If the 'Reenqueuement_Deferred' flag is set it isn't
                     --  necessary to do anything because it's sure there isn't
                     --  another task in the 'Running_Task' active priority
                     --  queue, otherwise the reenqueuement deferred would has
                     --  already been ended.

                     Running_Task.Scheduler_Timed_Event.CPU_Time :=
                       Running_Task.Scheduler_Timed_Event.CPU_Time
                       + Running_Task.Capacity;
                     Running_Task.Scheduler_Timed_Event.T :=
                       Running_Task.Scheduler_Timed_Event.T
                       + Running_Task.Capacity;
                     Enqueue_In_Order
                       (Running_Task.Scheduler_Timed_Event'Access,
                        Running_Task.CPU_Time_TEs_Q);

                  when SCHED_SPORADIC =>
                     ------------
                     --  SS Task has reached its execution time limit
                     ----

                     --  A replenishment operation is scheduled.
                     Schedule_SS_Replenishment_Operation
                       (Now => CS_Time, Limit_Reached => True);
                     --  The priority of the task is lowered.
                     TOI.Set_Base_Prio (Running_Task,
                                        Running_Task.Sched_SS_Low_Priority);
                     Running_Task.Capacity_Exhausted := True;

                  when others =>
                     --  scheduler event in not RR nor SS task!!
                     MaRTE.Direct_IO.Error ("#70#");
               end case;

            elsif Event.all in K.Timers.Timer_Timed_Event'Class then

               ------------
               --  Timer timed event
               ----
               pragma Debug -- {CDBG}
                   (CDBG.Timer_Interrupt_Handler_TE -- {CDBG}
                    (K.Timers.Timer_Id (Event).T,
                     MaRTE.Integer_Types.Unsigned_32
                     (K.Timers.Timer_Id (Event).Base_Clock),
                     CS_Time)); -- {CDBG}

               TMRI.Timer_Expired (K.Timers.Timer_Id (Event),
                                   CS_Time);

            elsif Event.all
              in K.Replenishment_TE.Replenishment_Timed_Event'Class then

               ------------
               --  Sporadic Server Replenishment timed event
               ----
               pragma Debug
                 (DBG.Assert
                    (Task_OK (R_Ac (Event).SS_Task) and then
                     R_Ac (Event).SS_Task.Sched_Policy = SCHED_SPORADIC));
               pragma Debug -- {CDBG}
                 (CDBG.Timer_Interrupt_Handler_RTE -- {CDBG}
                  (R_Ac (Event), CS_Time)); -- {CDBG}

               --  Update the 'Number_Of_Pending_Replenishments' and the
               --  'Replenish_Amount_Pending'
               R_Ac (Event).SS_Task.Number_Of_Pending_Replenishments :=
                 R_Ac (Event).SS_Task.Number_Of_Pending_Replenishments - 1;
               if R_Ac (Event).
                 SS_Task.Number_Of_Pending_Replenishments = 0 then
                  R_Ac (Event).SS_Task.Replenish_Amount_Pending := 0;
               else
                  R_Ac (Event).SS_Task.Replenish_Amount_Pending :=
                    R_Ac (Event).SS_Task.Replenish_Amount_Pending
                    - R_Ac (Event).Replenish_Amount;
               end if;

               --  Sporadic Server Task at high or low priority ?
               if not R_Ac (Event).SS_Task.Capacity_Exhausted then
                  --  Task at high priority (available capacity): the
                  --  scheduler timed event is going to be updated to expire
                  --  'Replenish_Amount' latter than currently. It must be
                  --  dequeued before doing this change.
                  CPU_Time_Timed_Events_Lists.Dequeue
                    (R_Ac (Event).SS_Task.Scheduler_Timed_Event'Access,
                     R_Ac (Event).SS_Task.CPU_Time_TEs_Q);
               else
                  --  Task at low priority (capacity exhausted): the
                  --  scheduler timed event is out of the queue with an
                  --  invalid value (it is going to be initialized with the
                  --  current values). Besides, the priority of the task is
                  --  raised and the activation time is stored.
                  if R_Ac (Event).SS_Task = Running_Task then
                     Running_Task.Scheduler_Timed_Event.CPU_Time :=
                       Running_Task.Used_CPU_Time +
                       (CS_Time - Running_Task.Running_Starting_Time);
                  else
                     R_Ac (Event).SS_Task.Scheduler_Timed_Event.CPU_Time :=
                       R_Ac (Event).SS_Task.Used_CPU_Time;
                  end if;
                  R_Ac (Event).SS_Task.Scheduler_Timed_Event.T := CS_Time;

                  TOI.Set_Base_Prio
                    (R_Ac (Event).SS_Task,
                     R_Ac (Event).SS_Task.Sched_SS_High_Priority);
                  R_Ac (Event).SS_Task.Capacity_Exhausted := False;
                  R_Ac (Event).SS_Task.Activation_Time := R_Ac (Event).T;
               end if;

               --  Add 'Replenish_Amount' to the scheduler timed event and
               --  enqueue it.
               R_Ac (Event).SS_Task.Scheduler_Timed_Event.CPU_Time :=
                 R_Ac (Event).SS_Task.Scheduler_Timed_Event.CPU_Time
                 + R_Ac (Event).Replenish_Amount;
               R_Ac (Event).SS_Task.Scheduler_Timed_Event.T :=
                 R_Ac (Event).SS_Task.Scheduler_Timed_Event.T
                 + R_Ac (Event).Replenish_Amount;

               CPU_Time_Timed_Events_Lists_Order.Enqueue_In_Order
                 (R_Ac (Event).SS_Task.Scheduler_Timed_Event'Access,
                  R_Ac (Event).SS_Task.CPU_Time_TEs_Q);

               Replenishment_TE.Release (R_Ac (Event));

            elsif Event.all
              in APPSCHD_DATA.Notification_Timed_Event'Class then

               ------------
               --  SCHED_APP task notification event
               ----
               APPSCHD.Send_Notification_Or_Ready
                 (Event_Code => APPSCHD_DATA.APPSCHED_TASK_NOTIFICATION,
                  ST         =>
                    UC (NTE_Ac (Event).Task_To_Notify.Sched_App).Scheduler,
                  T          => NTE_Ac (Event).Task_To_Notify);
               NTE_Ac (Event).Used := False;

            elsif Event.all
              in APPSCHD_DATA.Activation_Timed_Event'Class then

               ------------
               --  SCHED_APP timed task activation event
               ----
               ATE_Ac (Event).Task_To_Notify.Active_Urgency :=
                 ATE_Ac (Event).Urgency;
               ATE_Ac (Event).Task_To_Notify.Base_Urgency   :=
                 ATE_Ac (Event).Urgency;
               ATE_Ac (Event).Used := False;
               TO_APPSCHD.Activate_Appsched_Task
                 (ATE_Ac (Event).Task_To_Notify,
                  APPSCHD_DATA.TIMED_TASK_ACTIVATION);

            elsif Event.all
              in K.Timed_Handlers.Timed_Handler_Timed_Event'Class then

               ------------
               --  Timed Handler
               ----
               pragma Debug
                 (DBG.Assert (K.Timed_Handlers."="
                              (TH_Ac (Event).Magic,
                               K.Timed_Handlers.INITIALIZED) and
                              TCPU_Ac (Event).Armed and
                              TH_Ac (Event).Queued));

               --  Execute handler
               K.Timed_Handlers.Internals.Execute_Timed_Handler
                 (TH_Ac (Event),
                  From_Timer_Int_Handler => True,
                  Now => CS_Time);

            else
               --  Timed Event of a non-expected class !!
               MaRTE.Direct_IO.Error ("#71#");

            end if;

            Event := TE_T.Extract_Expired_Event (Running_Task);
            exit when Event = null;

         end loop;
      end if;

      --  The "software interrupt handler" ('Do_Scheduling') is put pending.
      --  It will be called when all outstanding hardware interrupts have
      --  been processed.
      Interrupt_Nesting_Count :=
        Interrupt_Nesting_Count and Invoke_Do_Scheduling_Mask;
      pragma Debug (DBG.Assert (Interrupt_Nesting_Count = 16#01#));

      K.Leave_Critic_Section_From_Interrupt;

   end Timer_Interrupt_Handler;

   ---------------------------------
   -- Running_Task_Gets_Suspended --
   ---------------------------------
   procedure Running_Task_Gets_Suspended (With_Status : in Task_Status;
                                          At_Time : in HAL.HWTime) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      Running_Task_Gets_Blocked (With_Status);
      --  Enqueue Timed event
      TE_T.Enqueue_Standard_Event_And_Update_Timer
        (Running_Task.Suspension_Event, At_Time);
   end Running_Task_Gets_Suspended;

   ------------------------
   -- Remove_Timed_Event --
   ------------------------
   procedure Remove_Timed_Event (E : in Timed_Event_Ac)
     renames TE_T.Remove_Standard_Event;

   -----------------------------------------------
   --  Enqueue_Standard_Event_And_Update_Timer  --
   -----------------------------------------------
   procedure Enqueue_Standard_Event_And_Update_Timer (E   : in Timed_Event_Ac;
                                                      Now : in HWTime)
     renames TE_T.Enqueue_Standard_Event_And_Update_Timer;

   -----------------------------------------------------
   --  Enqueue_Standard_Event_Without_Updating_Timer  --
   -----------------------------------------------------
   procedure Enqueue_Standard_Event_Without_Updating_Timer
     (E : in Timed_Event_Ac)
     renames TE_T.Enqueue_Standard_Event_Without_Updating_Timer;

   -------------------
   -- Do_Scheduling --
   -------------------
   Heir_Task, Old_Task : Task_Id;   --  Local to 'Do_Scheduling'.
   CPU_E : CPU_Time_Timed_Event_Ac; --  Local to 'Do_Scheduling'.
   procedure Do_Scheduling is
      --  Without local variables the execution time of this procedure is
      --  faster (no allocation of variables in the stack).
      use APPSCHD_DATA;
   begin
      pragma Debug (DBG.Assert (Initialized));
      --  Should be called with interrupts disabled
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      if Interrupt_Nesting_Count /= Outside_Of_Interrupts then
         --  The context switch mustn't be performed inside an interrupt
         --  handler. We just clear the high bit in
         --  'Interrupt_Nesting_Count' to put a "software interrupt"
         --  pending. 'Do_Scheduling' (the software interrupt handler) will
         --  be called when all outstanding hardware interrupts have been
         --  processed.
         Interrupt_Nesting_Count :=
           Interrupt_Nesting_Count and Invoke_Do_Scheduling_Mask;
         return;
      end if;

      ------------
      --  Reenqueuement Deferred ?
      ----
      if Running_Task.Reenqueuement_Deferred then
         pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled and
                                   Running_Task.Sched_Policy /= SCHED_APP));
         pragma Debug (CDBG.Do_Scheduling1 (Running_Task)); -- {CDBG}

         Reenqueue_Task_Ending_Reenqueuement_Deferred (Running_Task);
      end if;

      Heir_Task := ML.Head (Ready_Queue);

      ------------
      --  Is the scheduler of the heir task ready ? (SCHED_APP)
      ----
      if (Heir_Task.Sched_Policy = SCHED_APP and then
          UC (Heir_Task.Sched_App).Scheduler /= null and then
          UC (Heir_Task.Sched_App).Scheduler.Status = READY) then
         --  An application scheduled task should never run before its
         --  scheduler in the case it is READY. When this happens the
         --  scheduler is moved from its current position in the ready
         --  queue to the head and it becomes the heir task.
         Heir_Task := UC (Heir_Task.Sched_App).Scheduler;
         ML.Dequeue (Heir_Task, Ready_Queue);
         ML.Enqueue_First_In_Priority (Heir_Task, Ready_Queue);
      end if;

      ------------
      --  Has the most prioritary ready task changed ?
      ----
      if Running_Task /= Heir_Task then
         Old_Task := Running_Task;
         pragma Debug (Debug.Assert_Tasks_Are_OK (Heir => Heir_Task,
                                                  Old  => Old_Task));

         if not After_Timed_Event_Expiration then
            CS_Time := HAL.Get_HWTime;
         end if;

         ------------
         --  Kernel Nesting Level
         ----
         Old_Task.Kernel_Nesting_Level := K.Kernel_Nesting_Level;
         K.Kernel_Nesting_Level   := Heir_Task.Kernel_Nesting_Level;

         pragma Debug -- {CDBG}
           (CDBG.Do_Scheduling2 -- {CDBG}
            (Old_Task, Heir_Task, Ready_Queue, CS_Time)); -- {CDBG}

         ------------
         --  CPU times measurements
         ----
         if (CP.Use_CPU_Time_Clocks_And_Timers    or else
             CP.Use_Round_Robin_Scheduling_Policy or else
             CP.Use_Sporadic_Server_Scheduling_Policy)
         then
            Heir_Task.Running_Starting_Time := CS_Time;
            Old_Task.Used_CPU_Time := Old_Task.Used_CPU_Time +
              (CS_Time - Old_Task.Running_Starting_Time);

            --  Group clocks
            if CP.Use_Group_Clocks'First then
               if Old_Task.Set /= null then
                  pragma Debug (DBG.Assert (TS.Task_Set_OK (Old_Task.Set)));
                  --  Increment consumed time of "old" set
                  Old_Task.Set.Consumed_Time := Old_Task.Set.Consumed_Time +
                    (CS_Time - Old_Task.Running_Starting_Time);
               end if;

               pragma Debug (DBG.Assert (Heir_Task.Set = null or else
                                         TS.Task_Set_OK (Heir_Task.Set)));
               if Heir_Task.Set /= null
                 and then Heir_Task.Set.Group_TE_Ac /= null
               then
                  --  Perform actions on Group_TE_Ac
                  pragma Debug (DBG.Assert (GCLK.Group_TE_OK
                                            (Heir_Task.Set.Group_TE_Ac)));

                  --  Set CPU_Time of Group_TE_Ac
                  Heir_Task.Set.Group_TE_Ac.CPU_Time :=
                    Heir_Task.Used_CPU_Time +
                      (Heir_Task.Set.Group_TE_Ac.Group_Expiration_Time -
                       Heir_Task.Set.Consumed_Time);

                  if Heir_Task.Set.Group_TE_Ac.Armed
                    and then Heir_Task.Set.Group_TE_Ac.Task_Where_Queued /=
                        TCB_Ac (Heir_Task)
                  then
                     --  Move Group_TE_Ac from one task to another
                     if
                       Heir_Task.Set.Group_TE_Ac.Task_Where_Queued /= null then

                        pragma Debug (DBG.Assert
                                        (Is_In_The_List
                                         (Heir_Task.Set.Group_TE_Ac,
                                          Heir_Task.Set.Group_TE_Ac.
                                          Task_Where_Queued.CPU_Time_TEs_Q)));

                        --  Dequeue from the list it was queued
                        Dequeue (Heir_Task.Set.Group_TE_Ac,
                                 Heir_Task.Set.Group_TE_Ac.
                                   Task_Where_Queued.CPU_Time_TEs_Q);
                     end if;
                     --  Enqueue in Heir's list
                     Enqueue_In_Order (Heir_Task.Set.Group_TE_Ac,
                                       Heir_Task.CPU_Time_TEs_Q);
                     Heir_Task.Set.Group_TE_Ac.Task_Where_Queued :=
                       TCB_Ac (Heir_Task);

                     if Heir_Task.Set.Group_TE_Ac.all in
                       TH.Timed_Handler_Timed_Event'Class
                     then
                        TH.Timed_Handler_TE_Ac
                          (Heir_Task.Set.Group_TE_Ac).Queued := True;
                        --  When a group TH is created and the running task
                        --  isn't in the group the TH is not queued. Here it is
                        --  queued for first time and it has to be marked as
                        --  Enqueued
                     end if;
                  end if;
               end if;
            end if;  --  group clocks

            --  Gets absolute time of heir task's CPU Time Timed Events
            CPU_E := Head (Heir_Task.CPU_Time_TEs_Q);
            while CPU_E /= null loop
               pragma Debug
                 (DBG.Assert (CPU_E.Base_Clock /= TMR.CLOCK_REALTIME
                              and CPU_E.Base_Clock /= TMR.CLOCK_MONOTONIC
                              and TMRI.Clock_Id_OK (CPU_E.Base_Clock)));
               pragma Debug (DBG.Assert (K.CPU_TE_OK (CPU_E)));

               CPU_E.T := CS_Time + (CPU_E.CPU_Time - Heir_Task.Used_CPU_Time);
               CPU_E := Next (CPU_E);
            end loop;
         end if;

         --  Timer reprogramming
         if After_Timed_Event_Expiration then
            After_Timed_Event_Expiration := False;
            TE_T.Reprogram_Timer_After_Expiration (Heir_Task);
         elsif (CP.Use_CPU_Time_Clocks_And_Timers
                or else
                CP.Use_Round_Robin_Scheduling_Policy
                or else
                CP.Use_Sporadic_Server_Scheduling_Policy)
         then
            --  The the new running task's most urgent CPU time event could
            --  be the most urgent event.
            TE_T.Update_Timer_Due_To_New_Running_Task (Heir_Task, CS_Time);
         end if;

         ------------
         --  The context switch is actually carried out in
         --  'Change_To_Context' or 'Context_Switch'.
         ----
         Running_Task := Heir_Task;
         if Old_Task.Magic = TERMINATED then
            --  Here the "old task" leaves definitively the CPU so it
            --  is not necessary to save its context.
            HAL.Change_To_Context (New_Task => Heir_Task.Stack_Ptr'Address);
            Pool_TCBs.Release_TCB (Old_Task);
            --  Release task resources (including stack). It has to be
            --  done after changing the CPU stack register, otherwise
            --  it could produce dynamic memory pool corruption since
            --  local variables would be created on a stack that has
            --  already been freed.
         else
            HAL.Context_Switch (Old_Task => Old_Task.Stack_Ptr'Address,
                                New_Task => Heir_Task.Stack_Ptr'Address);
         end if;
      elsif After_Timed_Event_Expiration then

         After_Timed_Event_Expiration := False;
         TE_T.Reprogram_Timer_After_Expiration (Running_Task);

      end if;  --  Running_Task /= Heir_Task
   end Do_Scheduling;
   pragma Inline (Do_Scheduling);

   --------------------
   -- Idle_Task_Body --
   --------------------
   function Idle_Task_Body (Arg : System.Address) return System.Address;
   function Idle_Task_Body (Arg : System.Address) return System.Address is
   begin
      while True loop
         --  pragma Debug (DBG.Assert (HWI.Are_Interrupts_Enabled));
         null;
      end loop;
      return System.Null_Address;
   end Idle_Task_Body;

   ------------------
   --  Initialize  --
   ------------------
   --  Initialize scheduler structures, main and idle tasks and install the
   --  Timer interrupt handler
   procedure Initialize is
   begin
      pragma Debug (DBG.Assert (not Initialized));
      Initialized := True;

      ------------
      --  Initialize the Ready queue
      ----
      ML.Initialize (Ready_Queue);

      ------------
      --  Initialize and Register the main and idle task
      ----
      Running_Task :=
        TO.Initialize_TCBs.Initialize_Main_Task (Initital_Prio_Of_Main_Task);
      Register_Main_Task (Running_Task);
      MaRTE.Stacks_Management.Configure_Main_Task_Stack;

      Register_Idle_Task
        (TO.Initialize_TCBs.Initialize_Idle_Task (Idle_Task_Body'Access,
         TOI.Task_Wrapper'Address));

      ------------
      --  Install the Timer interrupt handler
      ----
      HAL.Install_HW_Interrupt_Handler (HAL.Timer_Interrupt,
                                        Timer_Interrupt_Handler'Access);
      K.Hardware_Interrupts.Reserve_Interrupt (HAL.Timer_Interrupt);
      HAL.Hardware_Interrupt_Controller_Enable_Interrupt (HAL.Timer_Interrupt);
   end Initialize;

end MaRTE.Kernel.Scheduler;
