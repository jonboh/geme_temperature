------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--          'K e r n e l . S c h e d u l e r . D e b u g _ S h o w'
--
--                                 Body
--
--
--  File 'k-scheduler-debug_show.adb'                                  By MAR.
--
--
--  Scheduler debugging.
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

with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.Integer_Types;
with MaRTE.POSIX_Constants;
with MaRTE.HAL;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Kernel.Debug;
with MaRTE.Stacks_Management; -- To perform Stack overflow checks
pragma Elaborate_All (MaRTE.Stacks_Management);
with MaRTE.Kernel.Tasks_Map_Lists_Show;
pragma Elaborate_All (MaRTE.Kernel.Tasks_Map_Lists_Show);
with MaRTE.Spy;
pragma Elaborate_All (MaRTE.Spy);
--  with Kernel.Timers.Internals;

package body MaRTE.Kernel.Scheduler_CDbg is

   package TSTE    renames K.Task_Suspension_Timed_Events;
   use HAL;
   package DBG     renames K.Debug;
   package ML_Show renames K.Tasks_Map_Lists_Show;
   package CP      renames MaRTE.Configuration_Parameters;
   use K.Tasks_Lists;

   -----------------------------------------
   -- Schedule_SS_Replenishment_Operation --
   -----------------------------------------
   procedure Schedule_SS_Replenishment_Operation
     (Activation_Time          : in HAL.HWTime;
      Replenish_Time           : in HAL.HWTime;
      Replenish_Amount         : in HAL.HWTime;
      Replenish_Amount_Pending : in HAL.HWTime) is
   begin
      if Scheduler_Debug_Messages then
         Put ("  |  SchedReplFor:"); DBG.Show_In_Secs (Replenish_Time);
         Put (" Amo:");             DBG.Show_In_Secs (Replenish_Amount,
                                                      DBG.ABSO);
         Put (" (ActTime:");        DBG.Show_In_Secs (Activation_Time);
         Put (",AmoPending:");      DBG.Show_In_Secs (Replenish_Amount_Pending,
                                                      DBG.ABSO);
         Put (")");
      end if;
   end Schedule_SS_Replenishment_Operation;

   ---------------------
   -- Task_Gets_Ready --
   ---------------------
   procedure Task_Gets_Ready (T : in Task_Id) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "TR", T, null);
   end Task_Gets_Ready;

   ---------------------------------------
   -- Ready_Task_Raises_Active_Priority --
   ---------------------------------------
   procedure Ready_Task_Raises_Active_Priority (T        : in Task_Id;
                                                New_Prio : in Task_Priority) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "TP", T.Id, New_Prio);
   end Ready_Task_Raises_Active_Priority;

   ----------------------------------------
   -- Ready_Task_Reduces_Active_Priority --
   ----------------------------------------
   procedure Ready_Task_Reduces_Active_Priority
     (T : in Task_Id; New_Prio : in Task_Priority) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "Tp", T.Id, New_Prio);
   end Ready_Task_Reduces_Active_Priority;

   -----------------------
   -- Task_Gets_Blocked --
   -----------------------
   procedure Task_Gets_Blocked (T : in Task_Id) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "TB", T, null);
   end Task_Gets_Blocked;

   -----------------------------------
   -- Non_Running_Task_Gets_Blocked --
   -----------------------------------
   procedure Non_Running_Task_Gets_Blocked (T : in Task_Id);
   procedure Non_Running_Task_Gets_Blocked (T : in Task_Id) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "TB", T, null);
   end Non_Running_Task_Gets_Blocked;

   ----------------------------------
   -- Timer_Interrupt_Handler_TSTE --
   ----------------------------------
   --
   --  Task suspension timed event
   procedure Timer_Interrupt_Handler_TSTE
     (Event : in TSTE.Task_Suspension_TE_Ac;
      Time  : in HAL.HWTime) is
   begin
      if Scheduler_Debug_Messages then
         Put ("  |  Actived("); Put (Event.Suspended_Task.Id);
         Put (","); Put (Integer (Event.Suspended_Task.Active_Prio));
         Put (")");
         Put (" at:"); DBG.Show_In_Secs (Time);
      end if;
   end Timer_Interrupt_Handler_TSTE;

   ---------------------------------
   -- Timer_Interrupt_Handler_STE --
   ---------------------------------
   --
   --  Scheduler timed event (RR and SS)
   procedure Timer_Interrupt_Handler_STE (T     : in Task_Id;
					  Event : in Timed_Event_Ac;
					  Time  : in HAL.HWTime) is
   begin
      if Scheduler_Debug_Messages then
	 Put ("  |  Scheduler Event ["); DBG.Show_In_Secs (Event.T);
	 Put ("] at:"); DBG.Show_In_Secs (Time);
      end if;
   end Timer_Interrupt_Handler_STE;

   --------------------------------
   -- Timer_Interrupt_Handler_RTE --
   ---------------------------------
   --
   --  Sporadic Server Replenishment timed event
   procedure Timer_Interrupt_Handler_RTE
     (Event : in Replenishment_TE.Replenishment_Timed_Event_Ac;
      Time  : in HAL.HWTime) is
   begin
      if Scheduler_Debug_Messages then
         Put ("  |  Replenishment Event ["); DBG.Show_In_Secs (Event.T);
         Put (",amount:"); DBG.Show_In_Secs (Event.Replenish_Amount, DBG.ABSO);
         Put ("] at:"); DBG.Show_In_Secs (Time);
      end if;
   end Timer_Interrupt_Handler_RTE;

   --------------------------------
   -- Timer_Interrupt_Handler_TE --
   --------------------------------
   --
   --  Timer timed event
   procedure Timer_Interrupt_Handler_TE
     (T       : in HAL.HWTime;
      Clock   : in MaRTE.Integer_Types.Unsigned_32;
      At_Time : in HAL.HWTime) is
      function To_Task_Id is
         new Ada.Unchecked_Conversion (MaRTE.Integer_Types.Unsigned_32,
				       K.Task_Id);
   begin
      if Scheduler_Debug_Messages then
	 Put (" | TimerEvent["); DBG.Show_In_Secs (T);
	 Put ("]at:"); DBG.Show_In_Secs (At_Time);
	 if Clock = MaRTE.POSIX_Constants.CLOCK_REALTIME then
	    Put (" RTCLK");
	 else
	    Put (" CPUCLK("); Put (To_Task_Id (Clock).Id);
	    Put (")");
	 end if;
      end if;
   end Timer_Interrupt_Handler_TE;

   --------------------
   -- Do_Scheduling1 --
   --------------------
   procedure Do_Scheduling1 (Running_Task : in Task_Id) is
   begin
      if Scheduler_Debug_Messages then
         Put ("  |  Reenq_Deferred("); Put (Running_Task.Id);
         Put (","); Put (Integer (Running_Task.Active_Prio));
         Put (",Old_AP:"); Put (Integer (Running_Task.Old_Active_Prio));
         Put (")");
      end if;
   end Do_Scheduling1;

   --------------------
   -- Do_Scheduling2 --
   --------------------
   procedure Do_Scheduling2
     (Old_Task  : in Task_Id;
      Heir_Task : in Task_Id;
      RQ        : in K.Tasks_Map_Lists.Map_List;
      Time      : in HAL.HWTime) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime, "CS",
                            Old_Task, Heir_Task);
      if Scheduler_Debug_Messages then
         New_Line;
         Put ("CS(");  Put (Old_Task.Id);
         Put (",");    Put (Integer (Old_Task.Active_Prio));
         Put (")->("); Put (Heir_Task.Id);
         Put (",");    Put (Integer (Heir_Task.Active_Prio));
         Put (")");
         Put (" RQ:"); ML_Show.Show_On_Console (RQ);
         Put ("at:"); DBG.Show_In_Secs (Time);
      end if;
   end Do_Scheduling2;

   ----------------
   -- Initialize --
   ----------------

--     procedure Initialize is
--     begin
--  --      pragma Debug
--  --        (MaRTE.Spy.Init (MaRTE.Configuration_Parameters.Num_MaRTE_Tasks_Mx,
--  --         MaRTE.Configuration_Parameters.Task_Priority_Mx + 1,
--  --         HAL.Get_HWClock_Frequency));
--        if CP.Preallocated_Resources'First then
--           pragma Debug
--             (MaRTE.Spy.Init (Num_Tasks_Mx => CP.Num_MaRTE_Tasks_Mx,
--                              Task_Priority_Mx => CP.Task_Priority_Mx + 1,
--                              HWClock_Freq => HAL.Get_HWClock_Frequency));
--           null;
--        else
--           --  ??? When resources are not preallocated the max number of tasks is
--           --  only limited by the amount of free memory. We put a "long" number:
--           --  50
--           pragma Debug
--             (MaRTE.Spy.Init (Num_Tasks_Mx => 50,
--                              Task_Priority_Mx => CP.Task_Priority_Mx + 1,
--                              HWClock_Freq => HAL.Get_HWClock_Frequency));
--           null;
--        end if;
--     end Initialize;

end MaRTE.Kernel.Scheduler_CDbg;
