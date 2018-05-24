------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . I n i t i a l i z a t i o n'
--
--                                  Body
--
--
--  File 'kernel-initialization.adb'                                   By MAR.
--
--
--  Preforms the initialization of the kernel calling to the 'Initialize'
--  procedures of the others packages. It is the only package with begin-end
--  block.
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
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Kernel.Timers;
pragma Elaborate_All (MaRTE.Kernel.Timers);
with MaRTE.Kernel.Timed_Events_Queue;
pragma Elaborate_All (MaRTE.Kernel.Timed_Events_Queue);
with MaRTE.Kernel.Hardware_Interrupts;
pragma Elaborate_All (MaRTE.Kernel.Hardware_Interrupts);
with MaRTE.Kernel.Signals.POSIX_Functions;
pragma Elaborate_All (MaRTE.Kernel.Signals.Posix_Functions);
with MaRTE.Kernel.Replenishment_TE;
pragma Elaborate_All (MaRTE.Kernel.Replenishment_Te);
with MaRTE.Kernel.Signals.Internals;
pragma Elaborate_All (MaRTE.Kernel.Signals.Internals);
with MaRTE.Kernel.Timer_Timed_Events_Pool;
pragma Elaborate_All (MaRTE.Kernel.Timer_Timed_Events_Pool);
with MaRTE.Kernel.Pool_TCBs;
pragma Elaborate_All (MaRTE.Kernel.Pool_TCBs);
with MaRTE.Kernel.Scheduler;
pragma Elaborate_All (MaRTE.Kernel.Scheduler);
with MaRTE.Kernel.Signals.Handler;
pragma Elaborate_All (MaRTE.Kernel.Signals.Handler);
with MaRTE.Kernel.Devices_Table;
pragma Elaborate_All (MaRTE.Kernel.Devices_Table);
with MaRTE.Direct_IO;
pragma Elaborate_All (MaRTE.Direct_IO);
with MaRTE.Kernel.Debug;
pragma Elaborate_All (MaRTE.Kernel.Debug);
with MaRTE.Kernel.Timed_Events_And_Timer;
pragma Elaborate_All (MaRTE.Kernel.Timed_Events_And_Timer);
with MaRTE.Kernel.Task_Sets.Internals;
pragma Elaborate_All (MaRTE.Kernel.Task_Sets.Internals);
with MaRTE.Configuration_Parameters;
with MaRTE.Timespec;
pragma Elaborate_All (MaRTE.Timespec);
with MaRTE.Spy;
pragma Elaborate_All (MaRTE.Spy);

package body MaRTE.Kernel.Initialization is

   package CP  renames MaRTE.Configuration_Parameters;
   use type CP.Supported_Architectures;

   Initialized : Boolean := False;

   procedure Initialize is
   begin
      if Initialized then
         --  This procedure can be called from two places:
         --  System.OS_Interface.pthread_init and the begin-end block
         --  of this package.
         return;
      end if;
      Initialized := True;

      --  Initialize the hardware interface
      HAL.Initialize;
      pragma Assert (CP.MaRTE_Architecture'First /= CP.ARCH_X86 or else
                     not HAL.Are_Interrupts_Enabled);
      --  HWI.Are_Interrupts_Enabled return true in Linux and Linux_Lib because
      --  interrupts are "enabled" after every call to Leave_Kernel.

      --  Give value to Kernel.Suspension_Time_Minimum
      K.Suspension_Time_Minimum :=
        HAL."/" (HAL."*" (CP.Suspension_Time_Minimum,
                          HAL.Get_HWClock_Frequency),
                 HAL.CPU_Frequency);
      K.Suspension_Time_Minimum_Seg :=
        HAL.HWTime_To_Duration (K.Suspension_Time_Minimum);

      --  Initialize HWT_HZ in Timespec
      Timespec.Initialize;

      --  Initialize Realtime Clock Offset
      K.Timers.Initialize_Realtime_Clock_Offset;

      --  Initialize the kernel debug messages
      K.Debug.Initialize;

      --  Initialize timed events a timer
      K.Timed_Events_And_Timer.Initialize;

      --  Initialize kernel structures and pools
      K.Task_Sets.Internals.Initialize;
      K.Timed_Events_Queue.Initialize;
      K.Hardware_Interrupts.Initialize;
      K.Signals.POSIX_Functions.Initialize;
      K.Replenishment_TE.Initialize_Pool;
      K.Signals.Internals.Initialize;
      K.Timer_Timed_Events_Pool.Initialize_Pool;

      --  Initialize resources for tasks
      K.Pool_TCBs.Initialize;

      --  Initialize scheduler structures, main and idle tasks and install the
      --  Timer interrupt handler
      K.Scheduler.Initialize;

      --  Initialize Kernel_Nesting_Level
      K.Kernel_Nesting_Level := 0;

      --  Initialize the signal handler task
      K.Signals.Handler.Initialize_Signal_Handler_Task;

      --  Initialize drivers
      pragma Assert (CP.MaRTE_Architecture'First /= CP.ARCH_X86 or else
                     not HAL.Are_Interrupts_Enabled);
      K.Devices_Table.Initialize_Devices;

      --  Initialize MaRTE tracer (Spy)
      --  Spy.Initialize does nothing if the flag in Debug_Messages is NOT set
      MaRTE.Spy.Initialize (Scheduler.Initital_Prio_Of_Main_Task);

      --  Inform the console kernel initialization has finished
      MaRTE.Direct_IO.End_Of_Kernel_Initialization;

      --  Enable interrupts: at this point Kernel is set up so we are ready to
      --  enable interrupts and start task scheduling
      HAL.Enable_Hardware_Interrupt_Controller;
      HAL.Enable_Interrupts;

   end Initialize;

begin
   Initialize;
end MaRTE.Kernel.Initialization;
