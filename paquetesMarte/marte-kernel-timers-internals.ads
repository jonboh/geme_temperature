------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T i m e r s . I n t e r n a l s'
--
--                                 Spec
--
--
--  File 'k-timers-internals.ads'                                      By MAR.
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

package MaRTE.Kernel.Timers.Internals is

   package K renames MaRTE.Kernel;

   package HAL  renames MaRTE.HAL;

   --  To_Task_Id
   --  Unchecked conversion between 'Task_Id' and 'Clock_Id'
   function To_Task_Id is
     new Ada.Unchecked_Conversion (Clock_Id, K.Task_Id);

   --  To_Clock_Id
   --  Unchecked conversion between 'Clock_Id' and 'Task_Id' --
   function To_Clock_Id is
     new Ada.Unchecked_Conversion (K.Task_Id, Clock_Id);

   --  To_Task_Set_Id
   --  Unchecked conversion between 'Task_Set_Id' and 'Clock_Id'
   function To_Task_Set_Id is
     new Ada.Unchecked_Conversion (Clock_Id, K.Task_Set_Id);

   --  To_Task_Set_Base_Ac
   --  Unchecked conversion between 'Clock_Id' and 'Task_Set_Base_Ac'
   function To_Task_Set_Base_Ac is
     new Ada.Unchecked_Conversion (Clock_Id, K.Task_Set_Base_Ac);

   --  To_Clock_Id
   --  Unchecked conversion between 'Clock_Id' and 'Task_Set_Id' --
   function To_Clock_Id is
     new Ada.Unchecked_Conversion (K.Task_Set_Id, Clock_Id);

   --  Is_CPU_Clock_Of_Self
   function Is_CPU_Clock_Of_Self (Clock : Clock_Id) return Boolean;
   pragma Inline (Is_CPU_Clock_Of_Self);

   --  Clock_Id_OK
   function Clock_Id_OK (Clock : Clock_Id) return Boolean;
   pragma Inline (Clock_Id_OK);

   --  Is_Group_Clock_Id
   function Is_Group_Clock_Id (Clock : Clock_Id) return Boolean;
   pragma Inline (Is_Group_Clock_Id);

   --  Is_CPU_Clock
   --  The clock is a CPU clock. CLOCK_THREAD_CPUTIME_ID is NOT considered
   function Is_CPU_Clock (Clock : Clock_Id) return Boolean;
   pragma Inline (Is_CPU_Clock);

   --  Timer_Id_OK
   function Timer_Id_OK (Timer : Timer_Id) return Boolean;
   pragma Inline (Timer_Id_OK);

   --  Timer_Expired
   procedure Timer_Expired (Timer : in Timer_Id; At_Time : in HAL.HWTime);
   pragma Inline (Timer_Expired);

   --  Disarm_Timer
   procedure Disarm_Timer (Timer : in Timer_Id);
   pragma Inline (Disarm_Timer);

   --  Arm_Timer
   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Now       : in  HAL.HWTime);
   pragma Inline (Arm_Timer);

   --  Get_Timer_State
   function Get_Timer_State (Timer : Timer_Id;
                             Now   : HAL.HWTime) return Timer_State;
   pragma Inline (Get_Timer_State);

   --  Set_Time_Event  --
   --  Sets 'TE_Ac.T' as an ABSOLUTE time respect the hardware clock (monotonic
   --  clock that measures time from system reset).
   --  'TE_Ac.T' is NOT normalized: can return a value of time before the
   --  current time
   procedure Set_Time_Event (TE_Ac    : in K.Timed_Event_Ac;
                             T        : in HAL.HWTime;
                             Abs_Time : in Boolean;
                             Clock    : in Clock_Id;
                             Now      : in HAL.HWTime);
   pragma Inline (Set_Time_Event);

   --  Set_CPU_Time_Event  --
   --  Sets TE_Ac.CPU_Time (and TE_Ac.T) to the appropriate value according to
   --  T, Abs_Time, Now, TE_Ac.Base_Clock and TE_Ac.Is_Based_On_Group_Clock.
   --  TE_Ac.CPU_Time  and  TE_Ac.T  are NOT normalized: can return a value of
   --  time before the current time
   procedure Set_CPU_Time_Event (TE_Ac     : in K.CPU_Time_Timed_Event_Ac;
                                 T         : in HAL.HWTime;
                                 Abs_Time  : in Boolean;
                                 Now       : in HAL.HWTime);
   pragma Inline (Set_CPU_Time_Event);

   --  Time_Remaining_To_CPU_Time_Event  --
   --  Returns the execution time interval that remains until the CPU even
   --  TE_Ac would expire
   function Time_Remaining_To_CPU_Time_Event
     (TE_Ac     : K.CPU_Time_Timed_Event_Ac;
      Now       : HAL.HWTime)
      return HAL.HWTime;
   pragma Inline (Time_Remaining_To_CPU_Time_Event);

end MaRTE.Kernel.Timers.Internals;
