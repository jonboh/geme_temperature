------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . S c h e d u l e r . C D b g'
--
--                                 Spec
--
--
--  File 'k-scheduler-cdbg.ads'                                        By MAR.
--
--
--  Scheduler debugging on cosole and on "tasks inspector".
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
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Replenishment_TE;
--  with Kernel.Timers;
with MaRTE.HAL;
with MaRTE.Kernel;
use MaRTE.Kernel;

package MaRTE.Kernel.Scheduler_CDbg is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   -----------------------------------------
   -- Schedule_SS_Replenishment_Operation --
   -----------------------------------------
   procedure Schedule_SS_Replenishment_Operation
     (Activation_Time          : in HAL.HWTime;
      Replenish_Time           : in HAL.HWTime;
      Replenish_Amount         : in HAL.HWTime;
      Replenish_Amount_Pending : in HAL.HWTime);

   ---------------------
   -- Task_Gets_Ready --
   ---------------------
   procedure Task_Gets_Ready (T : in Task_Id);

   ---------------------------------------
   -- Ready_Task_Raises_Active_Priority --
   ---------------------------------------
   procedure Ready_Task_Raises_Active_Priority (T        : in Task_Id;
                                                New_Prio : in Task_Priority);

   ----------------------------------------
   -- Ready_Task_Reduces_Active_Priority --
   ----------------------------------------
   procedure Ready_Task_Reduces_Active_Priority (T : in Task_Id;
                                                 New_Prio : in Task_Priority);

   -----------------------
   -- Task_Gets_Blocked --
   -----------------------
   procedure Task_Gets_Blocked (T : in Task_Id);

   -----------------------------
   -- Timer_Interrupt_Handler --
   -----------------------------
   procedure Timer_Interrupt_Handler_TSTE
     (Event : in Task_Suspension_Timed_Events.Task_Suspension_TE_Ac;
      Time  : in HAL.HWTime);
   --  Task suspension timed event

   procedure Timer_Interrupt_Handler_STE (T     : in Task_Id;
                                          Event : in Timed_Event_Ac;
                                          Time  : in HAL.HWTime);
   --  Scheduler timed event (RR and SS)

   procedure Timer_Interrupt_Handler_TE
     (T       : in HAL.HWTime;
      Clock   : in MaRTE.Integer_Types.Unsigned_32;
      At_Time : in HAL.HWTime);
   --  Timer timed event

   procedure Timer_Interrupt_Handler_RTE
     (Event : in Replenishment_TE.Replenishment_Timed_Event_Ac;
      Time  : in HAL.HWTime);
   --  Sporadic Server Replenishment timed event

   -------------------
   -- Do_Scheduling --
   -------------------
   procedure Do_Scheduling1 (Running_Task : in Task_Id);

   procedure Do_Scheduling2
     (Old_Task  : in Task_Id;
      Heir_Task : in Task_Id;
      RQ        : in K.Tasks_Map_Lists.Map_List;
      Time      : in HAL.HWTime);

   ----------------
   -- Initialize --
   ----------------

--   procedure Initialize;

end MaRTE.Kernel.Scheduler_CDbg;
