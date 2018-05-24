------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'K e r n e l . T y p e s _ S i z e s'
--
--                                 Spec
--
--
--  File 'k-types_sizes.ads'                                           By MAR.
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
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Condition_Variables;
with MaRTE.Kernel.Semaphores;
with MaRTE.Kernel.Signals;
with MaRTE.Kernel.Timed_Handlers;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.POSIX_Time;
with MaRTE.POSIX_Sched;
with MaRTE.POSIX_Pthread;
with MaRTE.Pthread_Once;
use MaRTE.Kernel;
with MaRTE.Integer_Types;

package MaRTE.Kernel.Types_Sizes is

   package K renames MaRTE.Kernel;

   --  Kernel
   Kernel_TCB_Size_In_Bytes : constant Integer :=
     K.TCB'Size / 8;
   pragma Export (C, Kernel_TCB_Size_In_Bytes,
                  "KERNEL_TCB_SIZE_IN_BYTES");

   --  Tasks_Operations
   Sched_Param_Size_In_Bytes : constant Integer :=
     Tasks_Operations.Sched_Param'Size / 8;
   pragma Export (C, Sched_Param_Size_In_Bytes, "SCHED_PARAM_SIZE_IN_BYTES");

   Attr_Size_In_Bytes : constant Integer :=
     Tasks_Operations.Pthread_Attr_T'Size / 8;
   pragma Export (C, Attr_Size_In_Bytes, "PTHREADATTR_SIZE_IN_BYTES");

   --  Mutexes
   Mutex_Size_In_Bytes : constant Integer := Mutexes.Mutex'Size / 8;
   pragma Export (C, Mutex_Size_In_Bytes, "MUTEX_SIZE_IN_BYTES");

   Mutex_Attr_Size_In_Bytes : constant Integer := Mutexes.Attributes'Size / 8;
   pragma Export (C, Mutex_Attr_Size_In_Bytes, "MUTEXATTR_SIZE_IN_BYTES");

   Mutex_AppSched_Param_T_Size_In_Bytes : constant Integer :=
     Mutexes.Mutex_AppSched_Param_T'Size / 8;
   pragma Export (C, Mutex_AppSched_Param_T_Size_In_Bytes,
                  "MUTEX_APPSCHED_PARAM_T_SIZE_IN_BYTES");

   --  Condition_Variables
   CV_Size_In_Bytes : constant Integer :=
     Condition_Variables.Condition'Size / 8;
   pragma Export (C, CV_Size_In_Bytes, "CV_SIZE_IN_BYTES");

   CV_Attr_Size_In_Bytes : constant Integer :=
     Condition_Variables.Attributes'Size / 8;
   pragma Export (C, CV_Attr_Size_In_Bytes, "CVATTR_SIZE_IN_BYTES");

   --  Semaphores
   Sem_Size_In_Bytes : constant Integer :=
     K.Semaphores.Semaphore'Size / 8;
   pragma Export (C, Sem_Size_In_Bytes, "SEM_SIZE_IN_BYTES");

   --  Signals
   Signal_Set_Size_In_Bytes : constant Integer := Signals.Signal_Set'Size / 8;
   pragma Export (C, Signal_Set_Size_In_Bytes, "SIGNAL_SET_SIZE_IN_BYTES");

   Struct_Sig_Action_Size_In_Bytes : constant Integer :=
     Signals.Struct_Sig_Action'Size / 8;
   pragma Export (C, Struct_Sig_Action_Size_In_Bytes,
                  "STRUCT_SIG_ACTION_SIZE_IN_BYTES");

   Siginfo_T_Size_In_Bytes : constant Integer :=
     Signals.Siginfo_T'Size / 8;
   pragma Export (C, Siginfo_T_Size_In_Bytes, "SIGINFO_T_SIZE_IN_BYTES");

   Signal_Event_Size_In_Bytes : constant Integer :=
     Signals.Signal_Event'Size / 8;
   pragma Export (C, Signal_Event_Size_In_Bytes,
                  "SIGNAL_EVENT_SIZE_IN_BYTES");

   --  Time
   Itimerspec_Size_In_Bytes : constant Integer :=
     MaRTE.POSIX_Time.Itimerspec'Size / 8;
   pragma Export (C, Itimerspec_Size_In_Bytes,
                  "ITIMERSPEC_SIZE_IN_BYTES");

   --  Tasks_Operations.Application_Scheduler
   Sched_Actions_Set_Size_In_Bytes : constant Integer :=
     Application_Scheduling_Data.Sched_Actions_Set'Size / 8;
   pragma Export (C, Sched_Actions_Set_Size_In_Bytes,
                  "SCHED_ACTIONS_SET_SIZE_IN_BYTES");

   --  Sched
   Posix_Appsched_Event_Size_In_Bytes : constant Integer :=
     MaRTE.POSIX_Sched.Posix_Appsched_Event'Size / 8;
   pragma Export (C, Posix_Appsched_Event_Size_In_Bytes,
                  "POSIX_APPSCHED_EVENT_SIZE_IN_BYTES");
   Posix_Appsched_Eventset_T_Size_In_Bytes : constant Integer :=
     K.Application_Scheduling_Data.Event_Set'Size / 8;
   pragma Export (C, Posix_Appsched_Eventset_T_Size_In_Bytes,
                  "POSIX_APPSCHED_EVENTSET_T_SIZE_IN_BYTES");

   --  Pthread
   Pthread_Key_T_Size_In_Bytes : constant Integer :=
     MaRTE.POSIX_Pthread.Pthread_Key_T'Size / 8;
   pragma Export (C, Pthread_Key_T_Size_In_Bytes,
                    "PTHREAD_KEY_T_SIZE_IN_BYTES");

   Pthread_Once_Size_In_Bytes : constant Integer :=
     MaRTE.Pthread_Once.Pthread_Once_T'Size / 8;
   pragma Export (C, Pthread_Once_Size_In_Bytes,
                    "PTHREAD_ONCE_SIZE_IN_BYTES");

   --  Kernel.Timed_Handlers
   Timed_Handler_Timed_Event_Size_In_Bytes : constant Integer :=
     K.Timed_Handlers.Timed_Handler_Timed_Event'Size / 8;
   pragma Export (C, Timed_Handler_Timed_Event_Size_In_Bytes,
                    "TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES");

   --  Task Sets Id
   Task_Set_Id_Size_In_Bytes : constant Integer :=
     K.Task_Set_Id'Size / 8;
   pragma Export (C, Task_Set_Id_Size_In_Bytes,
                    "TASK_SET_ID_SIZE_IN_BYTES");

end MaRTE.Kernel.Types_Sizes;
