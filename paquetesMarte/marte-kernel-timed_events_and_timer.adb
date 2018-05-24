------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--         'K e r n e l . T i m e d _ E v e n t s _ A n d _ T i m e r'
--
--                                  Body
--
--
--  File 'k-timed_events_and_timer.adb'                                By MAR.
--
--
--  Timer and Timed Events Management.
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
with MaRTE.Kernel.Timed_Events_Queue;
with MaRTE.Kernel.Debug;

--  Debug
with MaRTE.Kernel.Timed_Events_And_Timer_Debug;

package body MaRTE.Kernel.Timed_Events_And_Timer is

   Initialized : Boolean := False;

   package DBG renames K.Debug;

   package TEQ   renames K.Timed_Events_Queue;
   package Debug renames Timed_Events_And_Timer_Debug;

   use K.CPU_Time_Timed_Events_Lists;
   use K.CPU_Time_Timed_Events_Lists_Order;

   Safe_Longest_Interval : HWI.HWTime;
   --  The system timer shouldn't be programmed for an interval longer
   --  than this value. It is a logical constant, it gets value in
   --  'Initialize' and shouldn't be changed latter.

   Timer_Activation_Time : HWI.HWTime := HWI.HWTime'Last;
   --  Last timer activation time programmed (returned by
   --  'HWI.Program_Timer').

   Time_To_Program : HWI.HWTime := HWI.HWTime'Last;
   --  Global variable used in several functions to stores the event's
   --  activation time the timer is going to be programmed for.

   Compulsory_Timer_Reprogramming : constant Boolean :=
     HWI.Compulsory_Timer_Reprogramming;
   --  It is compulsory reprogramming the timer even if there are no
   --  pending timed events in the queues. This could happen, for example,
   --  when the timer interrupts are used also for the system clock.

   -------------------
   -- Program_Timer --
   -------------------
   --
   --  Program the timer to expire at the instant 'T' or, if it's too
   --  far, in a time equal to 'Safe_Longest_Interval'.
   procedure Program_Timer (T   : in HWI.HWTime;
                            Now : in HWI.HWTime);
   procedure Program_Timer (T   : in HWI.HWTime;
                            Now : in HWI.HWTime) is
      Interval_To_T : constant HWI.HWTime := T - Now;
   begin
      pragma Debug (DBG.Assert (Initialized));

      if T < Now + K.Suspension_Time_Minimum then
         --  Minimum length interval
         HWI.Program_Timer (HWI.HW_Timer_0,
                            K.Suspension_Time_Minimum,
                            Timer_Activation_Time);

         pragma Debug (Debug.Timer_Program_Event (Now,
                                                  Suspension_Time_Minimum,
                                                  T));

      elsif Interval_To_T < Safe_Longest_Interval then
         --  Interval to next timed event
         HWI.Program_Timer (HWI.HW_Timer_0,
                            Interval_To_T,
                            Timer_Activation_Time);

         pragma Debug (Debug.Timer_Program_Event (Now, Interval_To_T, T));
      else
         --  Normal interval
         HWI.Program_Timer (HWI.HW_Timer_0,
                            Safe_Longest_Interval,
                            Timer_Activation_Time);

         pragma Debug (Debug.Timer_Program_Normal (Now,
                                                   Safe_Longest_Interval));
      end if;
   end Program_Timer;
   pragma Inline (Program_Timer);

   --------------------------------------------------------------------------
   -- Interface functions and procedures ------------------------------------
   --------------------------------------------------------------------------

   --------------------------------------
   -- Reprogram_Timer_After_Expiration --
   --------------------------------------
   --
   --  Look for the most urgent timed event and call 'Program_Timer' with
   --  it.  The timer is always programmed except in the case there is no
   --  timed event to be programmed and 'Compulsory_Timer_Reprogramming'
   --  is FALSE.
   procedure Reprogram_Timer_After_Expiration (Running_Task : in Task_Id) is
   begin
      pragma Debug (DBG.Assert (Initialized));

      if TEQ.Is_Empty then
         if Is_Empty (Running_Task.CPU_Time_TEs_Q) then
            --  'TEQ' and 'CPU_Time_TEs_Q' empty
            Time_To_Program := HWI.HWTime'Last;
            if not Compulsory_Timer_Reprogramming then
               Timer_Activation_Time := HWI.HWTime'Last;
               return; -- Do not reprogram the timer
            end if;
         else
            --  'TEQ' empty and 'CPU_Time_TEs_Q' NOT empty
            Time_To_Program := Head (Running_Task.CPU_Time_TEs_Q).T;
         end if;

      elsif Is_Empty (Running_Task.CPU_Time_TEs_Q) then
         --  'TEQ' NOT empty and 'CPU_Time_TEs_Q' empty
         Time_To_Program := TEQ.Head.T;

      elsif Head (Running_Task.CPU_Time_TEs_Q).T < TEQ.Head.T then
         Time_To_Program := Head (Running_Task.CPU_Time_TEs_Q).T;

      else
         Time_To_Program := TEQ.Head.T;

      end if;
      Program_Timer (Time_To_Program, Timer_Activation_Time);
   end Reprogram_Timer_After_Expiration;

   ---------------------------------------
   -- Update_Timer_Due_To_New_CPU_Event --
   ---------------------------------------
   --
   --  If the new CPU timed event 'E' is more urgent than the currently
   --  programmed timer expiration the timer is reporgrammed invoking
   --  'Program_Timer'.
   procedure Update_Timer_Due_To_New_CPU_Event (E   : in Timed_Event_Ac;
                                                Now : in HWTime) is
   begin
      pragma Debug (DBG.Assert (Initialized));

      if E.T < Timer_Activation_Time then
         Time_To_Program := E.T;
         Program_Timer (Time_To_Program, Now);
      end if;
   end Update_Timer_Due_To_New_CPU_Event;

   ------------------------------------------
   -- Update_Timer_Due_To_New_Running_Task --
   ------------------------------------------
   --
   --  If the most urgent CPU timed event of the new running task is more
   --  urgent than the currently programmed timer expiration the timer is
   --  reporgrammed invoking 'Program_Timer'.
   procedure Update_Timer_Due_To_New_Running_Task (Running_Task : in Task_Id;
                                                   Now          : in HWTime) is
   begin
      pragma Debug (DBG.Assert (Initialized));

      if (not Is_Empty (Running_Task.CPU_Time_TEs_Q) and then
          Head (Running_Task.CPU_Time_TEs_Q).T < Timer_Activation_Time) then
         Time_To_Program := Head (Running_Task.CPU_Time_TEs_Q).T;
         Program_Timer (Time_To_Program, Now);
      end if;
   end Update_Timer_Due_To_New_Running_Task;

   ---------------------------------------------
   -- Enqueue_Standard_Event_And_Update_Timer --
   ---------------------------------------------
   --
   --
   --  The new timed event is enqueued and, in the case it is more
   --  urgent than the currently programmed timer expiration the timer is
   --  reporgrammed invoking 'Program_Timer'.
   procedure Enqueue_Standard_Event_And_Update_Timer (E   : in Timed_Event_Ac;
                                                      Now : in HWTime) is
   begin
      pragma Debug (DBG.Assert (Initialized));

      pragma Debug (Debug.New_Standard_Event (At_Time => HWI.Get_HWTime,
                                              T       => E.T));

      TEQ.Enqueue_In_Order (E);
      if E.T < Timer_Activation_Time then
         Time_To_Program := E.T;
         Program_Timer (Time_To_Program, Now);
      end if;
   end Enqueue_Standard_Event_And_Update_Timer;

   ---------------------------------------------------
   -- Enqueue_Standard_Event_Without_Updating_Timer --
   ----------------------------------------------------
   procedure Enqueue_Standard_Event_Without_Updating_Timer
     (E : in Timed_Event_Ac) is
   begin
      pragma Debug (DBG.Assert (Initialized));

      pragma Debug (Debug.New_Standard_Event (At_Time => HWI.Get_HWTime,
                                              T       => E.T));

      TEQ.Enqueue_In_Order (E);
   end Enqueue_Standard_Event_Without_Updating_Timer;

   ---------------------------
   -- Remove_Standard_Event --
   ---------------------------
   procedure Remove_Standard_Event (E : in Timed_Event_Ac)
     renames TEQ.Remove;

   ---------------------------
   -- Extract_Expired_Event --
   ---------------------------
   --
   --  Extract the most urgent timed event, either CPU or standard, from
   --  its queue only when its activation time is before 'Now +
   --  Suspension_Time_Minimum'.
   function Extract_Expired_Event (Running_Task : in Task_Id)
                                   return Timed_Event_Ac is
      Ret : Timed_Event_Ac;
   begin
      pragma Debug (DBG.Assert (Initialized));

      if TEQ.Is_Empty then
         if not Is_Empty (Running_Task.CPU_Time_TEs_Q) then
            --  'TEQ' empty and 'CPU_Time_TEs_Q' NOT empty
            Ret := Timed_Event_Ac (Head (Running_Task.CPU_Time_TEs_Q));
            if Ret.T < Timer_Activation_Time + Suspension_Time_Minimum then
               Dequeue_Head (Running_Task.CPU_Time_TEs_Q);
               return Ret;
            end if;
         end if;

      elsif Is_Empty (Running_Task.CPU_Time_TEs_Q) then
         --  'TEQ' NOT empty and 'CPU_Time_TEs_Q' empty
         if TEQ.Head.T < Timer_Activation_Time + Suspension_Time_Minimum then
            return TEQ.Remove_Head;
         end if;

      elsif Head (Running_Task.CPU_Time_TEs_Q).T < TEQ.Head.T then
         --  Head (Running_Task.CPU_Time_TEs_Q).T < TEQ.Head.T
         Ret := Timed_Event_Ac (Head (Running_Task.CPU_Time_TEs_Q));
         if Ret.T < Timer_Activation_Time + Suspension_Time_Minimum then
            Dequeue_Head (Running_Task.CPU_Time_TEs_Q);
            return Ret;
         end if;

      else
         --  TEQ.Head.T <= Head (Running_Task.CPU_Time_TEs_Q).T
         if TEQ.Head.T < Timer_Activation_Time + Suspension_Time_Minimum then
            return TEQ.Remove_Head;
         end if;

      end if;

      return null;
   end Extract_Expired_Event;

   -------------------------------
   -- Get_Timer_Activation_Time --
   -------------------------------
   function Get_Timer_Activation_Time return HWI.HWTime is
   begin
      return Timer_Activation_Time;
   end Get_Timer_Activation_Time;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      pragma Debug (DBG.Assert (not Initialized));
      Initialized := True;

      Safe_Longest_Interval := HWI.Safe_Longest_Timer_Interval;
   end Initialize;

end MaRTE.Kernel.Timed_Events_And_Timer;
