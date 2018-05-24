-------------------------------------------------------------------------------
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Signals;

with Interfaces.C;

package body MaRTE.Kernel.Application_Scheduler_Task_Body is

   package TO_APPSCHD   renames K.Tasks_Operations.Application_Scheduler;
   package SCHD         renames K.Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;

   use type Interfaces.C.int;

   -----------------------------------------------
   --  Generic Application Scheduler Task Body  --
   -----------------------------------------------
   function Generic_Body (Sched : System.Address) return System.Address is

      Actions       : aliased APPSCHD_DATA.Sched_Actions_Set;
      Error_Code    : K.Error_Code;
      Event_Ac      : APPSCHD_DATA.AppSched_Event_Ac;
      Flags         : Integer;
      Event_Mask    : aliased APPSCHD_DATA.Event_Set;
      Ret           : Interfaces.C.int;
      use type APPSCHD_DATA.New_Task_Op, APPSCHD_DATA.Ready_Op,
        APPSCHD_DATA.Explicit_Call_Op,
        APPSCHD_DATA.Task_Notification_Op, APPSCHD_DATA.Timeout_Op,
        APPSCHD_DATA.Signal_Op;
   begin
      --  Set event mask
      Ret := TO_APPSCHD.Posix_Appsched_Emptyset (Event_Mask'Access);
      pragma Assert (Ret = 0);
--        Ret := TO_APPSCHD.Posix_Appsched_Fillset (Event_Mask'Access);
--        pragma Assert (Ret /= 0);
--        if Address_To_AppScheduler_Data_Ac (Sched).Ops.New_Task /= null then
--       Ret := TO_APPSCHD.Posix_Appsched_Delset
--         (Event_Mask'Access, APPSCHD_DATA.POSIX_APPSCHED_NEW);
--        end if;
--    if APPSCHD_DATA.AppScheduler_Data_Ac
--      (SCHD.Self.AppScheduler).Ops.Explicit_Call /= null then
--       Ret := TO_APPSCHD.Posix_Appsched_Delset
--         (Event_Mask'Access, APPSCHD_DATA.POSIX_APPSCHED_EXPLICIT_CALL);
--        end if;
--    if APPSCHD_DATA.AppScheduler_Data_Ac
--      (SCHD.Self.AppScheduler).Ops.Task_Notification /= null
--        then
--       Ret := TO_APPSCHD.Posix_Appsched_Delset
--         (Event_Mask'Access, APPSCHD_DATA.POSIX_APPSCHED_TASK_NOTIFICATION);
--        end if;
--        if APPSCHD_DATA.AppScheduler_Data_Ac
--      (SCHD.Self.AppScheduler).Ops.Timeout /= null then
--       Ret := TO_APPSCHD.Posix_Appsched_Delset
--         (Event_Mask'Access, APPSCHD_DATA.POSIX_APPSCHED_TIMEOUT);
--        end if;
      Ret := TO_APPSCHD.Posix_Appschedattr_Seteventmask (Event_Mask);
      pragma Assert (Ret = 0);

      --  Initialize Scheduler
      APPSCHD_DATA.AppScheduler_Data_Ac
        (SCHD.Self.AppScheduler).Ops.Init (Sched);

      --  Initialize actions
      Actions.Initialized := APPSCHD_DATA.INITIALIZED;
      Actions.Last := 0;

      --  Processing events loop
      loop
         --  Execute Scheduling Actions
         K.Enter_Critic_Section (Flags);
         TO_APPSCHD.Execute_Actions (SCHD.Self, Actions, Error_Code);
         K.Leave_Critic_Section (Flags);

         --  Reset actions
         --  Reset_Scheduling_Actions (Actions);
         Actions.Last := 0;

         if Error_Code /= K.NO_ERROR then
            pragma Assert (False);
            null;
            --  Process error
            --
--                         Ada.Application_Scheduling.Invalid_Action_For_Task,
--                           Actions);
--                 else
--                    Error (Sched.all,
--                           Ada.Task_Identification.Null_Task_Id,

--                         Ada.Application_Scheduling.Invalid_Action_For_Task,
--                           Actions);
--                 end if;
         else
            --  Get a Scheduling Event or wait for it
            K.Enter_Critic_Section (Flags);
--                 if Actions.Timeout_Active then
--                    TO_APPSCHD.Get_Event
--                      (Event_Ac,
--                       Signal_Waited'Unrestricted_Access,
--                       HWI.Duration_To_HWTime
--                       (Ada.Real_Time.Delays.To_Duration (Actions.Timeout)));
--                 else
            TO_APPSCHD.Get_Event
              (Event_Ac,
               APPSCHD_DATA.AppScheduler_Data_Ac
               (SCHD.Self.AppScheduler).Wait_Signal_Set'Access);
--                 end if;
            K.Leave_Critic_Section (Flags);

--               Kernel_Console.Put
--                 (" --AppSchd:" &
--                  APPSCHD_DATA.Event_Code_T'Image (Event_Ac.Event_Code) &
--                  "--  ");

            --  Execute scheduler primitive depending on the event's code
            case Event_Ac.Event_Code is

               when APPSCHD_DATA.APPSCHED_NEW =>
                  pragma Assert
                    (APPSCHD_DATA.AppScheduler_Data_Ac
                       (SCHD.Self.AppScheduler).Ops.New_Task /= null);

                  APPSCHD_DATA.AppScheduler_Data_Ac
                    (SCHD.Self.AppScheduler).Ops.New_Task
                    (Sched, Event_Ac.T, Actions'Unrestricted_Access);

               when APPSCHD_DATA.APPSCHED_TERMINATE =>
                  null;
--                       Terminate_Task (Sched.all,
--                                       To_Ada_Task_Id (Event_Ac.T),
--                                       Actions);
               when APPSCHD_DATA.APPSCHED_READY =>
                  pragma Assert
                    (APPSCHD_DATA.AppScheduler_Data_Ac
                       (SCHD.Self.AppScheduler).Ops.Ready /= null);

                  APPSCHD_DATA.AppScheduler_Data_Ac
                    (SCHD.Self.AppScheduler).Ops.Ready
                    (Sched, Event_Ac.T, Actions'Unrestricted_Access);

               when APPSCHD_DATA.APPSCHED_BLOCK =>
                  null;
--                       Block (Sched.all,
--                              To_Ada_Task_Id (Event_Ac.T),
--                              Actions);
               when APPSCHD_DATA.APPSCHED_YIELD =>
                  null;
--                       Yield (Sched.all,
--                              To_Ada_Task_Id (Event_Ac.T),
--                              Actions);
               when APPSCHD_DATA.APPSCHED_SIGNAL =>
                  pragma Assert
                    (APPSCHD_DATA.AppScheduler_Data_Ac
                       (SCHD.Self.AppScheduler).Ops.Signal /= null);

                  APPSCHD_DATA.AppScheduler_Data_Ac
                    (SCHD.Self.AppScheduler).Ops.Signal
                    (Sched, Event_Ac.Siginfo, Actions'Unrestricted_Access);
--                      if Event_Ac.Siginfo.Code = Kernel.Signals.SI_QUEUE then
--                          Abort_Task
--                            (Sched.all,
--                             Int_To_Ada_Task_Id (Event_Ac.Siginfo.Value),
--                             Actions);
--                       else
--                          null;
--                       end if;

               when APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM =>
                  null;
--                       Change_Sched_Param (Sched.all,
--                                           To_Ada_Task_Id (Event_Ac.T),
--                                           Actions);

               when APPSCHD_DATA.APPSCHED_TIMEOUT =>
                  null;
--                       Timeout (Sched.all,
--                                Actions);
               when APPSCHD_DATA.APPSCHED_PRIORITY_INHERIT =>
                  null;
--                       Priority_Inherit
--                         (Sched          => Sched.all,
--                          Tid            => To_Ada_Task_Id (Event_Ac.T),
--                      From_Tid       => Ada.Task_Identification.Null_Task_Id,
--                          Inherited_Prio =>
--                            System.Any_Priority (Event_Ac.Sched_Priority),
--                          Actions        => Actions);
               when APPSCHD_DATA.APPSCHED_PRIORITY_UNINHERIT =>
                  null;
--                       Priority_Uninherit
--                         (Sched            => Sched.all,
--                          Tid              => To_Ada_Task_Id (Event_Ac.T),
--                          From_Tid         =>
--                            Ada.Task_Identification.Null_Task_Id,
--                          Uninherited_Prio =>
--                            System.Any_Priority (Event_Ac.Sched_Priority),
--                          Actions          => Actions);
               when APPSCHD_DATA.APPSCHED_TASK_NOTIFICATION =>
                  pragma Assert
                    (APPSCHD_DATA.AppScheduler_Data_Ac
                       (SCHD.Self.AppScheduler).Ops.Task_Notification /= null);

                  APPSCHD_DATA.AppScheduler_Data_Ac
                    (SCHD.Self.AppScheduler).Ops.Task_Notification
                    (Sched, Event_Ac.T, Actions'Unrestricted_Access);

               when APPSCHD_DATA.APPSCHED_EXPLICIT_CALL_WITH_DATA =>
                  --  Not supported
                  pragma Assert (False);
                  null;

               when  APPSCHD_DATA.APPSCHED_EXPLICIT_CALL =>
                  pragma Assert
                    (APPSCHD_DATA.AppScheduler_Data_Ac
                       (SCHD.Self.AppScheduler).Ops.Explicit_Call /= null);

                  APPSCHD_DATA.AppScheduler_Data_Ac
                    (SCHD.Self.AppScheduler).Ops.Explicit_Call
                    (Sched, Event_Ac.T, Event_Ac.User_Event_Code,
                     Actions'Unrestricted_Access);
               when APPSCHD_DATA.APPSCHED_INIT_MUTEX       |
                 APPSCHD_DATA.APPSCHED_DESTROY_MUTEX    |
                 APPSCHD_DATA.APPSCHED_LOCK_MUTEX       |
                 APPSCHD_DATA.APPSCHED_TRY_LOCK_MUTEX   |
                 APPSCHD_DATA.APPSCHED_UNLOCK_MUTEX     |
                 APPSCHD_DATA.APPSCHED_BLOCK_AT_MUTEX   |
                 APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM_MUTEX =>
                  --  Events not used yet in this implementation
                  null;
            end case;

            --  Event processed, then release it.
            APPSCHD_DATA.Release_AppSched_Event (Event_Ac);
         end if; --  if Error /= Kernel.NO_ERROR

      end loop; --  Processing events loop

      return System.Null_Address;
   end Generic_Body;

end MaRTE.Kernel.Application_Scheduler_Task_Body;
