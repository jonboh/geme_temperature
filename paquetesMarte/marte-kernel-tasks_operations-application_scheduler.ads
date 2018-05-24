------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . T a s k s _ O p e r a t i o n s .
--                A p p l i c a t i o n _ S c h e d u l e r'
--
--                                 Spec
--
--
--  File 'k-to-appsched.ads'                                           By MAR.
--
--
--  Application-defined scheduling specific operations.
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
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Signals;
with MaRTE.Kernel.Timers;

package MaRTE.Kernel.Tasks_Operations.Application_Scheduler is

   package K renames MaRTE.Kernel;

   package APPSCHD_DATA renames K.Application_Scheduling_Data;

   ----------------------------------------------------------------------------
   -- Scheduling Events Set Manipulation --------------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appsched_Emptyset
     (Set : access APPSCHD_DATA.Event_Set)
      return Int;
   pragma Export (C, POSIX_Appsched_Emptyset, "posix_appsched_emptyset");

   function POSIX_Appsched_Fillset
     (Set : access APPSCHD_DATA.Event_Set)
      return Int;
   pragma Export (C, POSIX_Appsched_Fillset, "posix_appsched_fillset");

   function POSIX_Appsched_Addset
     (Set        : access APPSCHD_DATA.Event_Set;
      Event_Code : in     APPSCHD_DATA.Event_Code_T)
      return Int;
   pragma Export (C, POSIX_Appsched_Addset, "posix_appsched_addset");

   function POSIX_Appsched_Delset
     (Set : access APPSCHD_DATA.Event_Set;
      Event_Code : in     APPSCHD_DATA.Event_Code_T)
      return Int;
   pragma Export (C, POSIX_Appsched_Delset, "posix_appsched_delset");

   function POSIX_Appsched_Ismember
     (Set        : in APPSCHD_DATA.Event_Set; -- By reference
      Event_Code : in APPSCHD_DATA.Event_Code_T)
      return Int;
   pragma Export (C, POSIX_Appsched_Ismember, "posix_appsched_ismember");
   pragma Export_Function (POSIX_Appsched_Ismember,
                           "posix_appsched_ismember",
                           Mechanism => (Set => Reference));

   ----------------------------------------------------------------------------
   --  Set and Get the "Wait Signal Set" attribute  ---------------------------
   --  of the scheduler task.                       ---------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appschedattr_Setwaitsignalset
     (Set : in K.Signals.Signal_Set) -- By ref. (pragma Export_Function)
      return Int;
   pragma Export (C, POSIX_Appschedattr_Setwaitsignalset,
                  "posix_appschedattr_setwaitsignalset");
   pragma Export_Function (POSIX_Appschedattr_Setwaitsignalset,
                           "posix_appschedattr_setwaitsignalset",
                           Mechanism => (Set => Reference));

   ----------------------------------------------------------------------------
   -- Set and Get the "Events Mask" attribute ---------------------------------
   -- of the scheduler task.                  ---------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appschedattr_Seteventmask
     (Set : in APPSCHD_DATA.Event_Set) -- By reference (pragma Export_Function)
      return Int;
   pragma Export (C, POSIX_Appschedattr_Seteventmask,
                  "posix_appschedattr_seteventmask");
   pragma Export_Function (POSIX_Appschedattr_Seteventmask,
                           "posix_appschedattr_seteventmask",
                           Mechanism => (Set => Reference));

   function POSIX_Appschedattr_Geteventmask
     (Set : access APPSCHD_DATA.Event_Set)
      return Int;
   pragma Export (C, POSIX_Appschedattr_Geteventmask,
                  "posix_appschedattr_geteventmask");

   ----------------------------------------------------------------------------
   -- Set and Get the "Flags" attribute ---------------------------------------
   -- of the scheduler task.            ---------------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appschedattr_SetFlags
     (Flags : in APPSCHD_DATA.AppScheduler_Data_Flags_T)
      return Int;
   pragma Export (C, POSIX_Appschedattr_SetFlags,
                  "posix_appschedattr_setflags");
   pragma Export_Function (POSIX_Appschedattr_SetFlags,
                           "posix_appschedattr_setflags",
                           Mechanism => (Flags => Value));

   function POSIX_Appschedattr_GetFlags
     (Flags : access APPSCHD_DATA.AppScheduler_Data_Flags_T)
      return Int;
   pragma Export (C, POSIX_Appschedattr_GetFlags,
                  "posix_appschedattr_getflags");

   ----------------------------------------------------------------------------
   -- Set and Get the "Clock" attribute ---------------------------------------
   -- of the scheduler task.            ---------------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appschedattr_Setclock (Clock : in K.Timers.Clock_Id)
                                         return Int;
   pragma Export (C, POSIX_Appschedattr_Setclock,
                  "posix_appschedattr_setclock");

   function POSIX_Appschedattr_Getclock (Clock : access K.Timers.Clock_Id)
                                         return Int;
   pragma Export (C, POSIX_Appschedattr_Getclock,
                  "posix_appschedattr_getclock");

   ----------------------------------------------------------------------------
   -- Set and Get the "Replyinfo" attribute  ----------------------------------
   -- of the scheduler task.                 ----------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appschedattr_Setreplyinfo
     (Reply      : in APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : in APPSCHD_DATA.Scheduler_Reply_Size_T)
      return Int;
   pragma Export (C, POSIX_Appschedattr_Setreplyinfo,
                  "posix_appschedattr_setreplyinfo");
   function POSIX_Appschedattr_Getreplyinfo
     (Reply      : in     APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : access APPSCHD_DATA.Scheduler_Reply_Size_T)
      return Int;
   pragma Export (C, POSIX_Appschedattr_Getreplyinfo,
                  "posix_appschedattr_getreplyinfo");

   ----------------------------------------------------------------------------
   -- Release_Event -----------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Used for the POSIX interface to release the event object.
   procedure Release_Event (Event_Ac : in APPSCHD_DATA.AppSched_Event_Ac);
   pragma Inline (Release_Event);

   ----------------------------------------------------------------------------
   --  Get Events  ------------------------------------------------------------
   ----------------------------------------------------------------------------

   procedure Get_Event (Event_Ac : in out APPSCHD_DATA.AppSched_Event_Ac;
                        Set      : in     Signals.Signal_Set_Ac_Const);
   pragma Inline (Get_Event);

   procedure Get_Event (Event_Ac : in out APPSCHD_DATA.AppSched_Event_Ac;
                        Set      : in     Signals.Signal_Set_Ac_Const;
                        Timeout  : in     MaRTE.HAL.HWTime);
   pragma Inline (Get_Event);

   ----------------------------------------------------------------------------
   --  Action sets manipulation  ----------------------------------------------
   ----------------------------------------------------------------------------
   function POSIX_Appsched_Actions_Init
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Init,
                  "posix_appsched_actions_init");
   pragma Inline (POSIX_Appsched_Actions_Init);

   function POSIX_Appsched_Actions_Destroy
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Destroy,
                  "posix_appsched_actions_destroy");
   pragma Inline (POSIX_Appsched_Actions_Destroy);

   function POSIX_Appsched_Actions_Addaccept
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addaccept,
                  "posix_appsched_actions_addaccept");
   pragma Inline (POSIX_Appsched_Actions_Addaccept);

   function POSIX_Appsched_Actions_Addreject
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addreject,
                  "posix_appsched_actions_addreject");
   pragma Inline (POSIX_Appsched_Actions_Addreject);

   function POSIX_Appsched_Actions_Addactivate
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addactivate,
                  "posix_appsched_actions_addactivate");
   pragma Inline (POSIX_Appsched_Actions_Addactivate);

   function POSIX_Appsched_Actions_Addactivateurg
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id;
      Urgency       : in     K.Task_Urgency) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addactivateurg,
                  "posix_appsched_actions_addactivateurg");
   pragma Inline (POSIX_Appsched_Actions_Addactivateurg);

   function POSIX_Appsched_Actions_Addsuspend
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addsuspend,
                  "posix_appsched_actions_addsuspend");
   pragma Inline (POSIX_Appsched_Actions_Addsuspend);

   function POSIX_Appsched_Actions_Addtimedactivation
       (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
        T             : in     K.Task_Id;
        Urgency       : in     K.Task_Urgency;
        HWT           : in     MaRTE.HAL.HWTime) return Int;
   pragma Inline (POSIX_Appsched_Actions_Addtimedactivation);

   function POSIX_Appsched_Actions_Addtimedtasknotification
       (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
        T             : in     K.Task_Id;
        HWT           : in     MaRTE.HAL.HWTime) return Int;
   pragma Inline (POSIX_Appsched_Actions_Addtimedtasknotification);

   function POSIX_Appsched_Actions_Addaccept_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addaccept_Mutex,
                  "posix_appsched_actions_addacceptmutex");
   pragma Inline (POSIX_Appsched_Actions_Addaccept_Mutex);

   function POSIX_Appsched_Actions_Addreject_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addreject_Mutex,
                  "posix_appsched_actions_addrejectmutex");
   pragma Inline (POSIX_Appsched_Actions_Addreject_Mutex);

   function POSIX_Appsched_Actions_Addlock_Mutex
     (Sched_Actions : access APPSCHD_DATA.Sched_Actions_Set;
      T             : in     K.Task_Id;
      M             : in     K.Mutexes.Mutex_Descriptor) return Int;
   pragma Export (C, POSIX_Appsched_Actions_Addlock_Mutex,
                  "posix_appsched_actions_addlockmutex");
   pragma Inline (POSIX_Appsched_Actions_Addlock_Mutex);

   ----------------------------------------------------------------------------
   --  Posix_Appsched_Scheduler_Create  ---------------------------------------
   ----------------------------------------------------------------------------
   function Posix_Appsched_Scheduler_Create
     (Sched_Ops  : access APPSCHD_DATA.Scheduler_Operations;
      Sched_Data : in     System.Address;
      Priority   : in     K.Task_Priority;
      Sched_Id   : access K.Task_Id)
     return Int;
   pragma Export (C, Posix_Appsched_Scheduler_Create,
                    "posix_appsched_scheduler_create");

   ----------------------------------------------------------------------------
   -- Execute Scheduling Actions ----------------------------------------------
   ----------------------------------------------------------------------------
   procedure Execute_Actions (ST    : in  Task_Id; --  Scheduler task
                              SA    : in  APPSCHD_DATA.Sched_Actions_Set;
                              Error : out K.Error_Code);
   pragma Export_Procedure (Internal        => Execute_Actions,
                            Parameter_Types => (Task_Id,
                                                APPSCHD_DATA.Sched_Actions_Set,
                                                K.Error_Code),
                            Mechanism       => (Value, Reference, Reference));
   pragma Inline (Execute_Actions);

   procedure Activate_Appsched_Task (T      : in K.Task_Id;
                                     Action : in APPSCHD_DATA.Actions_T);
   --  Assumes urgency changes has been performed before. Called from
   --  'Execute_Actions' and from 'SCHD.Timer_Interrupt_Handler' for
   --  an 'Activation_Timed_Event'. Performs the actions corresponding
   --  with the activation of an APP_SCHED task.

   ----------------------------------------------------------------------------
   -- Explicit Scheduler Invocation -------------------------------------------
   ----------------------------------------------------------------------------
   function Invoke_Scheduler (User_Event_Code : in Int) return Int;
   pragma Inline (Invoke_Scheduler);
   pragma Export (C, Invoke_Scheduler, "posix_appsched_invoke_scheduler");

   function Invoke_Scheduler_With_Data
     (Info       : in System.Address;
      Info_Size  : in Int;
      Reply      : in APPSCHD_DATA.Scheduler_Reply_Ac;
      Reply_Size : in APPSCHD_DATA.Scheduler_Reply_Size_Ac) return Int;
   --  pragma Inline (Invoke_Scheduler_With_Data); {MAR OJO} para 3.15p
   pragma Export (C, Invoke_Scheduler_With_Data,
                  "posix_appsched_invoke_withdata");

   ----------------------------------------------------------------------------
   -- Dinamically getting the appschedulerstate attribute ---------------------
   ----------------------------------------------------------------------------
   function Pthread_Getappschedulerstate
     (T        : Task_Id;
      Appsched : access Regular_Or_App_Scheduler_T) return Int;
   pragma Export (C, Pthread_Getappschedulerstate,
                  "pthread_getappschedulerstate");

   ----------------------------------------------------------------------------
   -- Dinamically getting and setting the appscheduler attribute --------------
   ----------------------------------------------------------------------------
   function Pthread_Setappscheduler (T            : in Task_Id;
                                     AppScheduler : in Task_Id) return Int;
   pragma Export (C, Pthread_Setappscheduler,
                  "pthread_setappscheduler");

   function Pthread_Getappscheduler (T            : in     Task_Id;
                                     Appscheduler : access Task_Id) return Int;
   pragma Export (C, Pthread_Getappscheduler,
                  "pthread_getappscheduler");

   ----------------------------------------------------------------------------
   -- Dinamically getting and setting the appschedparam attribute--------------
   ----------------------------------------------------------------------------
   function Pthread_Setappschedparam
     (T                   : in Task_Id;
      AppSched_Param      : in K.AppSched_Param_T_Ac;
      AppSched_Param_Size : in K.AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Setappschedparam,
                  "pthread_setappschedparam");

   function Pthread_Getappschedparam
     (T                   : in     Task_Id;
      AppSched_Param      : in     K.AppSched_Param_T_Ac;
      AppSched_Param_Size : access K.AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Getappschedparam,
                  "pthread_getappschedparam");

end MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
