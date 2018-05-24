------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                         'M a R T E _ O S _ A l l'
--
--                                  Spec
--
--
--  File 'marte_os_all.ads'                                            By MAR.
--
--  Empty package used to force compilation of the whole MaRTE OS
--  kernel.
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
with MaRTE_OS;
with MaRTE.Direct_IO;
with MaRTE.Integer_Types;
with MaRTE.Configuration_Parameters;
with MaRTE.Error_Codes_Info;
with MaRTE.POSIX_Constants;
with MaRTE.Kernel;
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Condition_Variables;
with MaRTE.Kernel.Condition_Variables.Internals;
with MaRTE.Kernel.Devices_Table;
with MaRTE.Kernel.Error_Codes;
with MaRTE.Kernel.File_System;
with MaRTE.Kernel.File_System_Data_Types;
with MaRTE.Kernel.Group_Clocks.Internals;
with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.Kernel.Hardware_Interrupts.Operations;
with MaRTE.Kernel.Initialization;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Mutexes.Attributes_SRP;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Mutexes.Internals_Appsched;
with MaRTE.Kernel.Mutexes.SRP_Ceiling;
with MaRTE.Kernel.Pool_TCBs;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Semaphores;
with MaRTE.Kernel.Semaphores.Internals;
with MaRTE.Kernel.Semaphores.Operations;
with MaRTE.Kernel.Signals;
with MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Signals.Internals;
with MaRTE.Kernel.Signals.Pool_SDBs;
with MaRTE.Kernel.Signals.POSIX_Functions;
with MaRTE.Kernel.Task_Suspension_Timed_Events;
with MaRTE.Kernel.Task_Sets.Operations;
with MaRTE.Kernel.Tasks_Lists_Prio;
with MaRTE.Kernel.Tasks_Operations;
with MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
with MaRTE.Kernel.Tasks_Operations.Attributes_EDF;
with MaRTE.Kernel.Tasks_Operations.Clock_Nanosleep;
with MaRTE.Kernel.Tasks_Operations.Initialize_TCBs;
with MaRTE.Kernel.Tasks_Operations.Nanosleep;
with MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Timed_Events_And_Timer;
with MaRTE.Kernel.Timed_Events_Queue;
with MaRTE.Kernel.Timed_Handlers.Operations;
with MaRTE.Kernel.Timer_Timed_Events_Pool;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Timers.Internals;
with MaRTE.Kernel.Types_Sizes;
with MaRTE.POSIX_Pthread;
with MaRTE.Pthread_Once;
with MaRTE.POSIX_Interrupt_Control;
with MaRTE.POSIX_Sched;
with MaRTE.POSIX_Semaphore;
with MaRTE.POSIX_Signal;
with MaRTE.Stacks_Management;
with MaRTE.POSIX_Time;
with MaRTE.POSIX_Unistd;

package MaRTE_OS_All is
end MaRTE_OS_All;
