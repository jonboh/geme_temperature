------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . S c h e d u l e r'
--
--                                 Spec
--
--
--  File 'k-scheduler.ads'                                            By MAR.
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

with MaRTE.Kernel.Tasks_Operations;
with MaRTE.HAL;

package MaRTE.Kernel.Scheduler is

   package K renames MaRTE.Kernel;

   Initital_Prio_Of_Main_Task : constant K.Task_Priority :=
     K.Task_Priority'Last / 2;

   package TO  renames K.Tasks_Operations;
   package HAL renames MaRTE.HAL;

   function Self return K.Task_Id;
   pragma Export (C, Self, "pthread_self");
   pragma Inline (Self);

   procedure Running_Task_Gets_Blocked (With_Status : in Task_Status);
   --  pragma Inline (Running_Task_Gets_Blocked); {MAR OJO} para 3.15p

   procedure Non_Running_Task_Gets_Blocked (T           : in Task_Id;
                                            With_Status : in Task_Status);
   pragma Inline (Non_Running_Task_Gets_Blocked);
   --  Only used by SCHED_APP tasks. It do NOT send a BLOCK event to
   --  the appscheduler.

   procedure Running_Task_Gets_Suspended (With_Status : in Task_Status;
                                          At_Time : in HAL.HWTime);
   pragma Inline (Running_Task_Gets_Suspended);

   procedure Remove_Timed_Event (E : in Timed_Event_Ac);
   pragma Inline (Remove_Timed_Event);

   procedure Enqueue_Standard_Event_And_Update_Timer (E   : in Timed_Event_Ac;
                                                      Now : in HAL.HWTime);
   pragma Inline (Enqueue_Standard_Event_And_Update_Timer);
   --  The new timed event is enqueued and, in the case it is more
   --  urgent than the currently programmed timer expiration the timer is
   --  reporgrammed.
   --  renames TE_T.Enqueue_Standard_Event_And_Update_Timer;

   procedure Enqueue_Standard_Event_Without_Updating_Timer
     (E : in Timed_Event_Ac);
   pragma Inline (Enqueue_Standard_Event_Without_Updating_Timer);
   --  Just enqueue the event in the events queue. To be called from
   --  code invoked from the timer interrupt handler.
   --  renames TE_T.Enqueue_Standard_Event_Without_Updating_Timer;

   procedure Sched_Fifo_Task_Gets_Ready (T : in Task_Id);
   pragma Inline (Sched_Fifo_Task_Gets_Ready);

   procedure Task_Gets_Ready (T : in Task_Id);
   pragma Inline (Task_Gets_Ready);

   procedure New_Task_Created (T : in Task_Id);
   pragma Inline (New_Task_Created);

   procedure Ready_Task_Changes_Urgency (T : in Task_Id);
   pragma Inline (Ready_Task_Changes_Urgency);
   --  Assumes urgency has already been changed in task

   procedure Ready_Task_Raises_Active_Priority (T        : in Task_Id;
                                                New_Prio : in Task_Priority);
   pragma Inline (Ready_Task_Raises_Active_Priority);

   procedure Ready_Task_Reduces_Active_Priority
     (T                  : in Task_Id;
      New_Prio           : in Task_Priority;
      Head_Of_New_Prio_Q : in Boolean);
   pragma Inline (Ready_Task_Reduces_Active_Priority);
   --  'Head_Of_New_Prio_Q' True means that the task shall become the head
   --  of its new priority queue (not the tail like ususal). This is useful
   --  in the case the priority of a task is reduced because it has unlocked
   --  a mutex. In this case the task must continue executing even if there
   --  are some other tasks in the new priority queue.

   procedure Task_Changes_Policy (T      : in Task_Id;
                                  Policy : in TO.Scheduling_Policies);
   pragma Inline (Task_Changes_Policy);
   --  Don't change the 'T.Sched_Policy' until after calling this function
   --  (it needs to know both the old and new policies). Notice the
   --  'T.Sched_Policy' HAVE TO BE be changed after calling it.

   procedure New_Sched_Param_In_AppSched_Task (T : in Task_Id);
   pragma Inline (New_Sched_Param_In_AppSched_Task);

   procedure Trying_To_Change_Policy_To_Sched_App (T : in Task_Id);
   pragma Inline (Trying_To_Change_Policy_To_Sched_App);

   procedure Remove_Task (T : in Task_Id);
   pragma Inline (Remove_Task);
   --  Removes the task from the ready queue, removes its asociated
   --  events from the timed events queue (if any) and, in the case it is an
   --  "appscheduler", the field 'App_Scheduler' of its scheduled tasks is
   --  made null.

   procedure Running_Task_Yields_CPU;
   pragma Inline (Running_Task_Yields_CPU);

   procedure Do_Scheduling;
   pragma Export (C, Do_Scheduling, "do_scheduling");
   pragma Inline (Do_Scheduling);

   ------------------
   --  Initialize  --
   ------------------
   --  Initialize scheduler structures, main and idle tasks and install the
   --  Timer interrupt handler
   procedure Initialize;

end MaRTE.Kernel.Scheduler;
