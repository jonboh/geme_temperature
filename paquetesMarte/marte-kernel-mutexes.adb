------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                       'K e r n e l . M u t e x e s'
--
--                                  Body
--
--
--  File 'k-mutexes.adb'                                             By MAR.
--
--
--  POSIX mutexes management.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Task_Suspension_Timed_Events;

--  Debug
with MaRTE.Kernel.Mutexes.Debug;
package body MaRTE.Kernel.Mutexes is

   package SCHD         renames K.Scheduler;
   package APPSCHD      renames K.Application_Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;
   package MLST         renames K.Mutexes_Lists;
   package TSTE         renames K.Task_Suspension_Timed_Events;

   use type Int;

   function UC (Data_Base_Ac : K.Sched_App_Data_Base_Ac)
                return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;

   use K.Tasks_Lists;

   type Attributes_Ac is access all Attributes;
   type Ceiling_Priority_Ac is access all Ceiling_Priority;
   type Int_Ac is access all Int;
   type Mutex_AppSched_Param_Size_Ac is access all Mutex_AppSched_Param_Size_T;

   -------------
   -- Attr_OK --
   -------------
   function Attr_OK (Attr : access Attributes) return Boolean is
   begin
      return Attributes_Ac (Attr) /= null
        and then Attr.Magic = ATTR_INITIALIZED;
   end Attr_OK;

   ----------------------------------------------------------------------------
   -- Attributes management ---------------------------------------------------
   ----------------------------------------------------------------------------

   ----------------------------
   -- Pthread_Mutexattr_Init --
   ----------------------------
   function Pthread_Mutexattr_Init (Attr : access Attributes) return Int is
   begin
      if Attributes_Ac (Attr) = null then
         return INVALID_ARGUMENT;
      end if;

      Attr.all := Attr_Default;
      return 0;
   end Pthread_Mutexattr_Init;

   -------------------------------
   -- Pthread_Mutexattr_Destroy --
   -------------------------------
   function Pthread_Mutexattr_Destroy (Attr : access Attributes)
                                      return Int is
   begin
      --  Error handling
      if not Attr_OK (Attr) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Magic := ATTR_NOT_INITIALIZED;

      return 0;
   end Pthread_Mutexattr_Destroy;

   -----------------------------------
   -- Pthread_Mutexattr_Setprotocol --
   -----------------------------------
   function Pthread_Mutexattr_Setprotocol (Attr    : access Attributes;
                                           Locking : in     Locking_Policy)
                                          return Int is
   begin
      --  Error handling
      if (not Attr_OK (Attr) or
          Locking < Locking_Policy'First or
          Locking > Locking_Policy'Last) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Policy := Locking;

      return 0;
   end Pthread_Mutexattr_Setprotocol;

   -----------------------------------
   -- Pthread_Mutexattr_Getprotocol --
   -----------------------------------
   function Pthread_Mutexattr_Getprotocol (Attr     : access Attributes;
                                           Protocol : access Int)
                                          return Int is
      type Int_Ac is access all Int;
   begin
      --  Error handling
      if not Attr_OK (Attr) or Int_Ac (Protocol) = null then
         return INVALID_ARGUMENT;
      end if;

      Protocol.all := Attr.Policy;

      return 0;
   end Pthread_Mutexattr_Getprotocol;

   --------------------------------------
   -- Pthread_Mutexattr_Setprioceiling --
   --------------------------------------
   function Pthread_Mutexattr_Setprioceiling
     (Attr        : access Attributes;
      New_Ceiling : in     Int)
     return Int is
   begin
      --  Error handling
      if (not Attr_OK (Attr) or
          New_Ceiling < Int (K.Task_Priority'First) or
          New_Ceiling > Int (K.Task_Priority'Last)) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Ceiling_Prio := K.Task_Priority (New_Ceiling);

      return 0;
   end Pthread_Mutexattr_Setprioceiling;

   --------------------------------------
   -- Pthread_Mutexattr_Getprioceiling --
   --------------------------------------
   function Pthread_Mutexattr_Getprioceiling
     (Attr        : access Attributes;
      Prioceiling : access Int)
     return Int is
   begin
      --  Error handling
      if not Attr_OK (Attr) or Int_Ac (Prioceiling) = null then
         return INVALID_ARGUMENT;
      end if;

      Prioceiling.all := Int (Attr.Ceiling_Prio);

      return 0;
   end Pthread_Mutexattr_Getprioceiling;

   ---------------------------------------
   -- Pthread_Mutexattr_Setappscheduler --
   ---------------------------------------
   function Pthread_Mutexattr_Setappscheduler
     (Attr      : access Attributes;
      Scheduler : in     K.Task_Id) return Int is
   begin
      --  Error handling
      if (not Attr_OK (Attr) or else
          not Task_OK (Scheduler) or else
          Scheduler.AppScheduler = null) then
         return INVALID_ARGUMENT;
      end if;

      Attr.App_Scheduler := Scheduler;
      return 0;
   end Pthread_Mutexattr_Setappscheduler;

   ---------------------------------------
   -- Pthread_Mutexattr_Getappscheduler --
   ---------------------------------------
   function Pthread_Mutexattr_Getappscheduler
     (Attr      : access Attributes;
      Scheduler : access K.Task_Id) return Int is
      type Task_Id_Ac is access all K.Task_Id;
   begin
      --  Error handling
      if not Attr_OK (Attr) or Task_Id_Ac (Scheduler) = null then
         return INVALID_ARGUMENT;
      end if;

      Scheduler.all := Attr.App_Scheduler;
      return 0;
   end Pthread_Mutexattr_Getappscheduler;

   ----------------------------------------
   -- Pthread_Mutexattr_SetappSchedparam --
   ----------------------------------------
   function Pthread_Mutexattr_SetappSchedparam
     (Attr       : access Attributes;
      Param      : in     Mutex_AppSched_Param_Ac;
      Param_Size : in     Mutex_AppSched_Param_Size_T) return Int is
   begin
      --  Error handling
      if (not Attr_OK (Attr) or else
          Param_Size < Mutex_AppSched_Param_Size_T'First or else
          Mutex_AppSched_Param_Size_T'Last < Param_Size or else
          (Param_Size /= 0 and Param = null)) then
         return INVALID_ARGUMENT;
      end if;

      if Param_Size > 0 then
         Attr.AppSched_Param (1 .. Param_Size) := Param (1 .. Param_Size);
      end if;
      Attr.AppSched_Param_Size := Param_Size;
      return 0;
   end Pthread_Mutexattr_SetappSchedparam;

   ----------------------------------------
   -- Pthread_Mutexattr_GetappSchedparam --
   ----------------------------------------
   function Pthread_Mutexattr_GetappSchedparam
     (Attr       : access Attributes;
      Param      : in     Mutex_AppSched_Param_Ac;
      Param_Size : access Mutex_AppSched_Param_Size_T) return Int is
   begin
      --  Error handling
      if not Attr_OK (Attr) or else Param = null then
         return INVALID_ARGUMENT;
      end if;

      if Attr.AppSched_Param_Size > 0 then
         Param (1 .. Attr.AppSched_Param_Size) :=
           Attr.AppSched_Param (1 .. Attr.AppSched_Param_Size);
      end if;
      Param_Size.all := Attr.AppSched_Param_Size;
      return 0;
   end Pthread_Mutexattr_GetappSchedparam;

   ------------------------
   -- Pthread_Mutex_Init --
   ------------------------
   Mutexes_Counter : Integer := 0;

   function Pthread_Mutex_Init (M    : access Mutex;
                                Attr : access Attributes) return Int is
      procedure Memory_Copy (From  : access Mutex;
                             To    : access Mutex;
                             Bytes : in     Integer);
      pragma Import (C, Memory_Copy, "memory_copy");
   begin
      --  Error handling
      if ((Attributes_Ac (Attr) /= null and then not Attr_OK (Attr)) or else
          Mutex_Descriptor (M) = null) then
         return INVALID_ARGUMENT;
      end if;
      if M.Magic = INITIALIZED then
         return RESOURCE_BUSY;
      end if;
      if (Attributes_Ac (Attr) /= null      and then
          Attr.Policy = APPSCHED_PROTOCOL   and then
          (not Task_OK (Attr.App_Scheduler) or else
           Attr.App_Scheduler.AppScheduler = null)) then
         --  Invalid scheduler task
         return INVALID_ARGUMENT;
      end if;
      --  Checking if the mutex is already initialized can't be
      --  performed here. This is because for a just created C mutex
      --  variable can't be assured if it's initialized or not due to
      --  its initial random value. This check is only performed for
      --  the POSIX.Ada interface in 'POSIX_Mutexes'.

      --  To set properly the 'tag' in a mutex created from a C
      --  program.
      Memory_Copy (From  => PTHREAD_MUTEX_INITIALIZER'Unrestricted_Access,
                   To    => M,
                   Bytes => Mutex'Size / 8);
      --  'M' is now a valid 'NO_PRIORITY_INHERITANCE' mutex. Fields
      --  as 'Magic', 'Owner', 'Blocked_Tasks' and
      --  'Num_Of_Associated_CVs' are already set to the desired
      --  value. Fields as 'Prio' and 'App_Scheduler' will be valid
      --  for some protocols.

      M.Id := Mutexes_Counter;   Mutexes_Counter := Mutexes_Counter + 1;
      M.SRP_Mutex_Lock.M := Mutex_Descriptor (M);
      if Attributes_Ac (Attr) /= null then
         M.Policy := Attr.Policy;
         M.Preemption_Level := Attr.Preemption_Level;
      end if;
      case M.Policy is
         when HIGHEST_CEILING_PRIORITY =>
            M.Prio := Attr.Ceiling_Prio;
         when HIGHEST_BLOCKED_TASK | NO_PRIORITY_INHERITANCE =>
            null;
            --  Nothing has to be changed since M.Prio is set to
            --  Task_Priority'First in Pthread_Mutex_Initializer.
         when APPSCHED_PROTOCOL =>
            M.App_Scheduler := Attr.App_Scheduler;
            if Attr.AppSched_Param_Size > 0 then
               M.AppSched_Param (1 .. Attr.AppSched_Param_Size) :=
                 Attr.AppSched_Param (1 .. Attr.AppSched_Param_Size);
            end if;
            M.AppSched_Param_Size := Attr.AppSched_Param_Size;
            declare
               Flags : Integer;
            begin
               K.Enter_Critic_Section (Flags);
               M.Magic := NEW_APPSCHED;
               M.Task_Waiting_Appscheduler_Response := SCHD.Self;
               --  Send event to appscheduler and suspend task
               --  waiting for appscheduler response.
               APPSCHD.Send_Event_From_Task
                 (Event_Code => APPSCHD_DATA.APPSCHED_INIT_MUTEX,
                  ST         => M.App_Scheduler,
                  T          => SCHD.Self,
                  M          => Mutex_Descriptor (M));
               SCHD.Do_Scheduling; -- Activate appscheduler task

               if SCHD.Self.Internal_Error_Code = NO_ERROR then
                  --  The appscheduler has accepted the mutex
                  M.Magic := INITIALIZED;
               else
                  --  The appscheduler has the INIT_MUTEX event masked or
                  --  the new mutex has been rejected by its appscheduler
                  pragma Assert
                    (SCHD.Self.Internal_Error_Code = APPSCHED_EVENT_MASKED or
                       SCHD.Self.Internal_Error_Code = APPSCHED_REJECTED);
                  declare
                     Error : K.Error_Code :=
                       SCHD.Self.Internal_Error_Code;
                  begin
                     SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
                     M.Magic := NOT_INITIALIZED;
                     K.Leave_Critic_Section (Flags);
                     return Error;
                  end;
               end if;

               K.Leave_Critic_Section (Flags);
            end;
      end case;

      pragma Debug (Debug.Initialize (M)); -- Trace

      return 0;
   end Pthread_Mutex_Init;

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (M : Mutex) return Mutex_Descriptor is
   begin
      --  Error handling
      if not Internals.Mutex_OK (M) then
         return null;
      end if;

      return M'Unrestricted_Access;
   end Descriptor_Of;
   pragma Inline (Descriptor_Of);

   ---------------------------
   -- Pthread_Mutex_Destroy --
   ---------------------------
   function Pthread_Mutex_Destroy (M : access Mutex) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not Internals.Mutex_OK (Mutex_Descriptor (M)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if M.Owner /= null or M.Num_Of_Associated_CVs > 0 then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_BUSY;
      end if;
      if (M.Policy = APPSCHED_PROTOCOL and then
          M.App_Scheduler /= SCHD.Self and then
          M.App_Scheduler /= UC (SCHD.Self.Sched_App).Scheduler) then
         --  An appmutex only can be finalized from its scheduler task or
         --  other task associated with its scheduler.
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;

      pragma Debug (Debug.Finalize (M)); -- Trace

      M.Magic := FINALIZED;
      if M.Policy = APPSCHED_PROTOCOL and M.App_Scheduler /= SCHD.Self then
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_DESTROY_MUTEX,
            ST         => M.App_Scheduler,
            T          => SCHD.Self,
            M          => Mutex_Descriptor (M));
         SCHD.Do_Scheduling; -- Activate appscheduler task
      end if;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Destroy;

   ----------------------------------
   -- Pthread_Mutex_Setprioceiling --
   ----------------------------------
   function Pthread_Mutex_Setprioceiling
     (M           : in     Mutex_Descriptor;
      New_Ceiling : in     Int;
      Old_Ceiling : access Int) return Int is
      Flags : Integer;
      Ret : Int;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if (not Internals.Mutex_OK (M)                     or else
          M.Policy /= HIGHEST_CEILING_PRIORITY           or else
          New_Ceiling < Int (K.Task_Priority'First) or else
          New_Ceiling > Int (K.Task_Priority'Last)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      Ret := Pthread_Mutex_Lock (M);
      if Ret = 0 then
         if Int_Ac (Old_Ceiling) /= null then
            Old_Ceiling.all := Int (M.Prio);
         end if;
         M.Prio := Task_Priority (New_Ceiling);
         Ret := Pthread_Mutex_Unlock (M);
      end if;

      K.Leave_Critic_Section (Flags);
      return Ret;
   end Pthread_Mutex_Setprioceiling;

   -----------------------------------------
   -- Pthread_Mutex_Setprioceiling_Locked --
   -----------------------------------------
   function Pthread_Mutex_Setprioceiling_Locked
     (M           : in     Mutex_Descriptor;
      New_Ceiling : in     Int) return Int is
      Flags : Integer;
   begin
      if MaRTE.Configuration_Parameters.Not_Ada_User_Code'First then
         return OPERATION_NOT_SUPPORTED;
      end if;
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if (not Internals.Mutex_OK (M)                     or else
          M.Policy /= HIGHEST_CEILING_PRIORITY           or else
          New_Ceiling < Int (K.Task_Priority'First) or else
          New_Ceiling > Int (K.Task_Priority'Last)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if M.Owner /= SCHD.Self then
         --  Mutex have to be looked by task that calls this operation
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      M.Prio := Task_Priority (New_Ceiling);

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Setprioceiling_Locked;

   ----------------------------------
   -- Pthread_Mutex_Getprioceiling --
   ----------------------------------
   function Pthread_Mutex_Getprioceiling
     (M           : in     Mutex_Descriptor;
      Prioceiling : access Int) return Int is
   begin
      --  Error handling
      if not (Internals.Mutex_OK (M)              and then
              M.Policy = HIGHEST_CEILING_PRIORITY and then
              Int_Ac (Prioceiling) /= null) then
         return INVALID_ARGUMENT;
      end if;

      Prioceiling.all := Int (M.Prio);
      return 0;
   end Pthread_Mutex_Getprioceiling;

   ------------------------------------
   -- Pthread_Mutex_Setappschedparam --
   ------------------------------------
   function Pthread_Mutex_Setappschedparam
     (M          : in Mutex_Descriptor;
      Param      : in Mutex_AppSched_Param_Ac;
      Param_Size : in Mutex_AppSched_Param_Size_T)
     return Int is
      Flags : Integer;
      Ret : Int;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not (Internals.Mutex_OK (M) and then
              M.Policy = APPSCHED_PROTOCOL) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if (M.Policy = APPSCHED_PROTOCOL and then
          M.App_Scheduler /= SCHD.Self and then
          M.App_Scheduler /= UC (SCHD.Self.Sched_App).Scheduler) then
         --  The parameters of an appmutex only can be changed from its
         --  scheduler task or other task associated with its scheduler.
         K.Leave_Critic_Section (Flags);
         return APPSCHED_POLICY_ERROR;
      end if;
      if (Param_Size < Mutex_AppSched_Param_Size_T'First or else
          Mutex_AppSched_Param_Size_T'Last < Param_Size or else
          (Param_Size /= 0 and Param = null)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      --  Lock the mutex
      Ret := Pthread_Mutex_Lock (M);
      if Ret = 0 then
         --  Copy new application-scheduling params
         if Param_Size > 0 then
            M.AppSched_Param (1 .. Param_Size) := Param (1 .. Param_Size);
         end if;
         M.AppSched_Param_Size := Param_Size;

         --  Inform to the application-scheduler
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM_MUTEX,
            ST         => M.App_Scheduler,
            T          => SCHD.Self,
            M          => M);

         --  Unlock the mutex
         Ret := Pthread_Mutex_Unlock (M);
      end if;
      K.Leave_Critic_Section (Flags);
      return Ret;
   end Pthread_Mutex_Setappschedparam;

   ---------------------------------------------------------------------
   -- Pthread_Mutex_Getappschedparam (Application-Defined Scheduling) --
   ---------------------------------------------------------------------
   function Pthread_Mutex_Getappschedparam
     (M          : in     Mutex_Descriptor;
      Param      : in     Mutex_AppSched_Param_Ac;
      Param_Size : access Mutex_AppSched_Param_Size_T)
      return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not (Internals.Mutex_OK (M, Extra_Magic => NEW_APPSCHED) and then
              M.Policy = APPSCHED_PROTOCOL and then
              Param /= null and then
              Mutex_AppSched_Param_Size_Ac (Param_Size) /= null) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      if M.AppSched_Param_Size > 0 then
         Param (1 .. M.AppSched_Param_Size) :=
           M.AppSched_Param (1 .. M.AppSched_Param_Size);
      end if;
      Param_Size.all := M.AppSched_Param_Size;

      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Getappschedparam;

   -----------------------------------
   -- Pthread_Mutex_Getappscheduler --
   -----------------------------------
   function Pthread_Mutex_Getappscheduler (M : in     Mutex_Descriptor;
                                           T : access Task_Id) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not (Internals.Mutex_OK (M) and then
              M.Policy = APPSCHED_PROTOCOL) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      T.all := M.App_Scheduler;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Getappscheduler;

   --------------------------------------
   -- Posix_Appsched_Mutex_Setspecific --
   --------------------------------------
   function Posix_Appsched_Mutex_Setspecific
     (M    : in Mutex_Descriptor;
      Data : in System.Address) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      --  Error handling
      if not Internals.Mutex_OK (M, NEW_APPSCHED) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      M.Specific_Data := Data;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Posix_Appsched_Mutex_Setspecific;

   --------------------------------------
   -- Posix_Appsched_Mutex_Getspecific --
   --------------------------------------
   function Posix_Appsched_Mutex_Getspecific
     (M    : in Mutex_Descriptor;
      Data : access System.Address) return Int is
      type Address_Ac is access all System.Address;
   begin
      pragma Assert (Address_Ac (Data) /= null);
      --  Error handling
      if not Internals.Mutex_OK (M, NEW_APPSCHED) then
         return INVALID_ARGUMENT;
      end if;

      Data.all := M.Specific_Data;
      return 0;
   end Posix_Appsched_Mutex_Getspecific;

   ------------------------
   -- Pthread_Mutex_Lock --
   ------------------------
   function Pthread_Mutex_Lock (M : in Mutex_Descriptor) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      if M.Owner = SCHD.Self then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_DEADLOCK_AVOIDED;
      end if;

      loop -- loop until the mutex is locked

         --  Error handling
         if (not Internals.Mutex_OK (M) or else
               (M.Policy = HIGHEST_CEILING_PRIORITY
                and SCHD.Self.Active_Prio > M.Prio)) then
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;
         if (M.Policy = APPSCHED_PROTOCOL and then
               UC (SCHD.Self.Sched_App).Scheduler /= M.App_Scheduler) then
            K.Leave_Critic_Section (Flags);
            return OPERATION_NOT_SUPPORTED;
         end if;

         pragma Assert (SCHD.Self.Internal_Error_Code = NO_ERROR or
                          SCHD.Self.Internal_Error_Code = TIMED_OUT);
         --  This is here to detect the situation when in
         --  Pthread_Mutex_Timedlock the timeout has expired just after testing
         --  the timeout and before calling this function
         if SCHD.Self.Internal_Error_Code = TIMED_OUT then
            --  Timeout has expired
            SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
            K.Leave_Critic_Section (Flags);
            return TIMED_OUT;
         end if;

         if M.Policy = APPSCHED_PROTOCOL then
            --  The task bocks
            UC (SCHD.Self.Sched_App).AppMutex_Where_Waiting := M;
            SCHD.Running_Task_Gets_Blocked (WAITING_TO_LOCK_APPSCHED_MUTEX);
            if M.Owner = null then
               --  Inform to the scheduler the task wants to lock the mutex.
               APPSCHD.Send_Event_From_Task
                 (Event_Code => APPSCHD_DATA.APPSCHED_LOCK_MUTEX,
                  ST         => M.App_Scheduler,
                  T          => SCHD.Self,
                  M          => M);
            else
               --  Inform to the scheduler the task has blocked at the mutex.
               APPSCHD.Send_Event_From_Task
                 (Event_Code => APPSCHD_DATA.APPSCHED_BLOCK_AT_MUTEX,
                  ST         => M.App_Scheduler,
                  T          => SCHD.Self,
                  M          => M);
            end if;
            --  Leave the CPU and let the appscheduler to execute
            SCHD.Do_Scheduling;

            UC (SCHD.Self.Sched_App).AppMutex_Where_Waiting := null;
            --  When the locking task resume execution here is with the
            --  mutex taken (it has been given in 'TO_APPSCHD.Execute_Actions')
            pragma Debug (Debug.Running_Task_Locks_AppMutex (M));
            pragma Assert (M.Owner = SCHD.Self);

            --  Exit the loop with the mutex locked
            exit;

         else -- M.Policy /= APPSCHED_PROTOCOL
            if M.Owner /= null then  --  Mutex locked

               if SCHD.Self.Timed_Waiting_Mutex then
                  --  Get suspended (Wait for unlock or timeout)
                  SCHD.Running_Task_Gets_Suspended
                    (With_Status => TIMED_WAITING_MUTEX,
                     At_Time => HAL.Get_HWTime);
               else
                  --  Get blocked (Wait for unlock)
                  SCHD.Running_Task_Gets_Blocked (BLOCKED);
               end if;
               Internals.Enqueue_Blocked_Task_In_Mutex (SCHD.Self, M);
               SCHD.Do_Scheduling; -- leave CPU

               --  The thread has been put ready by
               --  'Pthread_Mutex_Unlock' or by 'Timeout_Reached'

               --  Loop again to try to lock the mutex

            else  --  Mutex free

               --  Remove suspension event?
               if SCHD.Self.Timed_Waiting_Mutex then
                  SCHD.Self.Timed_Waiting_Mutex := False;
                  --  The task had used 'Pthread_Mutex_Timedlock' to wait on
                  --  the mutex and the timeout hasn't been reached
                  SCHD.Remove_Timed_Event (SCHD.Self.Suspension_Event);
               end if;

               --  lock the mutex
               Internals.Running_Task_Locks_Mutex (M);
               if SCHD.Self.Sched_Policy = SCHED_APP then
                  --  Give a chance to the scheduler task to execute
                  SCHD.Do_Scheduling;
               end if;

               --  Exit the loop with the mutex locked
               exit;
            end if;
         end if;
      end loop;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Lock;

   -----------------------------
   -- Pthread_Mutex_Timedlock --
   -----------------------------
   function Pthread_Mutex_Timedlock
     (M           : in Mutex_Descriptor;
      Abs_Timeout : in MaRTE.Timespec.Timespec_Ac) return Int is
      Flags : Integer;
      Now   : HAL.HWTime;
      use type HAL.HWTime;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if (not Internals.Mutex_OK (M) or else
          (M.Policy = HIGHEST_CEILING_PRIORITY and
           SCHD.Self.Base_Prio > M.Prio)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if M.Policy = APPSCHED_PROTOCOL then
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;
      if M.Owner = SCHD.Self then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_DEADLOCK_AVOIDED;
      end if;

      if M.Owner /= null then
         --  Block in the mutex
         if Abs_Timeout.Tv_Nsec < 0 or Abs_Timeout.Tv_Nsec > 1_000_000_000 then
            K.Leave_Critic_Section (Flags);
            return INVALID_ARGUMENT;
         end if;

         Now := HAL.Get_HWTime;
         SCHD.Self.Suspension_Event.T :=
           MaRTE.Timespec.Timespec_To_HWTime (Abs_Timeout.all);
         if (SCHD.Self.Suspension_Event.T >
               Now + Timers.Realtime_Clock_Offset + Suspension_Time_Minimum)
         then
            --  Configure Task Timed Event
            SCHD.Self.Suspension_Event.T :=
              SCHD.Self.Suspension_Event.T - Timers.Realtime_Clock_Offset;
            TSTE.Task_Suspension_TE_Ac (SCHD.Self.Suspension_Event).Reason :=
              TSTE.MUTEX_TIMEDWAIT;
            TSTE.Task_Suspension_TE_Ac
              (SCHD.Self.Suspension_Event).Mutex_Where_Blocked :=
              Mutex_Base_Ac (M);
            --  Get suspended (Wait for unlock or timeout)
            SCHD.Self.Timed_Waiting_Mutex := True;
            SCHD.Running_Task_Gets_Suspended
              (With_Status => TIMED_WAITING_MUTEX, At_Time => Now);
            Internals.Enqueue_Blocked_Task_In_Mutex (SCHD.Self, M);
            SCHD.Do_Scheduling; -- leave CPU

            --  The thread has been put ready by
            --  'Pthread_Mutex_Unlock' or by 'Timeout_Reached'
            pragma Assert (SCHD.Self.Internal_Error_Code = NO_ERROR or
                             SCHD.Self.Internal_Error_Code = TIMED_OUT);
            if SCHD.Self.Internal_Error_Code = TIMED_OUT then
               --  Timeout has expired
               SCHD.Self.Internal_Error_Code := NO_ERROR;  --  reset flag
               K.Leave_Critic_Section (Flags);
               return TIMED_OUT;
            else
               --  Try to lock the mutex again.
               K.Leave_Critic_Section (Flags);
               return Pthread_Mutex_Lock (M);
            end if;

         else
            --  'Timeout' is too small to be waited for.
            K.Leave_Critic_Section (Flags);
            return TIMED_OUT;
         end if;

      else  --  M.Owner = null
         --  lock the mutex
         Internals.Running_Task_Locks_Mutex (M);
         if SCHD.Self.Sched_Policy = SCHED_APP then
            --  Give a chance to the scheduler task to execute
            SCHD.Do_Scheduling;
         end if;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Timedlock;

   ---------------------------
   -- Pthread_Mutex_Trylock --
   ---------------------------
   function Pthread_Mutex_Trylock (M : in Mutex_Descriptor) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if (not Internals.Mutex_OK (M) or else
          (M.Policy = HIGHEST_CEILING_PRIORITY
           and SCHD.Self.Base_Prio > M.Prio)) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if (M.Policy = APPSCHED_PROTOCOL and then
          UC (SCHD.Self.Sched_App).Scheduler /= M.App_Scheduler) then
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;
      if M.Owner = SCHD.Self then
         K.Leave_Critic_Section (Flags);
         return RESOURCE_DEADLOCK_AVOIDED;
      end if;

      if M.Owner /= null then
         --  Mutex already locked
         K.Leave_Critic_Section (Flags);
         return RESOURCE_BUSY;
      else --  Mutex free
         if M.Policy = APPSCHED_PROTOCOL then
            --  The task bocks
            UC (SCHD.Self.Sched_App).AppMutex_Where_Waiting := M;
            SCHD.Running_Task_Gets_Blocked (TRYING_TO_LOCK_APPSCHED_MUTEX);
            --  Inform to the scheduler the task is trying to lock the mutex.
            APPSCHD.Send_Event_From_Task
              (Event_Code => APPSCHD_DATA.APPSCHED_TRY_LOCK_MUTEX,
               ST         => M.App_Scheduler,
               T          => SCHD.Self,
               M          => M);
            --  Leave the CPU and let the appscheduler to execute
            SCHD.Do_Scheduling;

            UC (SCHD.Self.Sched_App).AppMutex_Where_Waiting := null;
            --  Has the task successfully locked the mutex ?
            if M.Owner = SCHD.Self then
               pragma Debug (Debug.Running_Task_Locks_AppMutex (M));
               null;
            else
               K.Leave_Critic_Section (Flags);
               return RESOURCE_BUSY;
            end if;

         else --  M.Policy /= APPSCHED_PROTOCOL
            --  lock the mutex
            Internals.Running_Task_Locks_Mutex (M);
         end if;
      end if;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Trylock;

   --------------------------
   -- Pthread_Mutex_Unlock --
   --------------------------
   function Pthread_Mutex_Unlock (M : in Mutex_Descriptor) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);

      --  Error handling
      if not Internals.Mutex_OK (M) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;
      if (M.Policy = APPSCHED_PROTOCOL and then
          UC (SCHD.Self.Sched_App).Scheduler /= M.App_Scheduler) then
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_SUPPORTED;
      end if;
      if M.Owner /= SCHD.Self then
         K.Leave_Critic_Section (Flags);
         return OPERATION_NOT_PERMITTED;
      end if;

      ------------
      --  The task gives up ownership of 'M'
      ----
      if M.Policy = APPSCHED_PROTOCOL then
         pragma Debug (Debug.Running_Task_Unlocks_AppMutex (M));
         --  Free the mutex
         M.Owner := null;
         --  Inform to the scheduler the task has unlocked the mutex.
         APPSCHD.Send_Event_From_Task
           (Event_Code => APPSCHD_DATA.APPSCHED_UNLOCK_MUTEX,
            ST         => M.App_Scheduler,
            T          => SCHD.Self,
            M          => M);

      else -- M.Policy /= APPSCHED_PROTOCOL
         Internals.Task_Unlocks_Mutex (SCHD.Self, M);
         --  The schecheduler must always be called after this because the
         --  former owner of the mutex could have changed its priority.
      end if;

      --  If the task is SCHED_RR and its quantum got exhausted while holding
      --  the mutex, now is the time to send the task to the tail of its
      --  priority queue

      if SCHD.Self.Sched_Policy = K.SCHED_RR
        and then SCHD.Self.End_Of_RR_Slice_Pending
        and then SCHD.Self.RR_Quantum_Enabled
        and then MLST.Is_Empty (SCHD.Self.Mutexes_Owned)
      then
         SCHD.Ready_Task_Reduces_Active_Priority (SCHD.Self,
                                                  SCHD.Self.Active_Prio,
                                                  Head_Of_New_Prio_Q => False);
         SCHD.Self.End_Of_RR_Slice_Pending := False;
      end if;

      SCHD.Do_Scheduling;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Mutex_Unlock;

end MaRTE.Kernel.Mutexes;
