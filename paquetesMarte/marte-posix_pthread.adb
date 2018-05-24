------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--                              'P T h r e a d'
--
--                                   Body
--
--
--
--  File 'pthread.adb'                                                By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'pthread.h'.
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

with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Pool_TCBs;
with MaRTE.Kernel.Signals.Handler;
with MaRTE.Kernel.Debug;

package body MaRTE.POSIX_Pthread is

   package CP renames MaRTE.Configuration_Parameters; use CP;
   package SCHD renames K.Scheduler;
   package DBG renames K.Debug;

   ----------------------------------------------------------------------------
   -- Errno -------------------------------------------------------------------
   ----------------------------------------------------------------------------

   -------------------
   -- Pthread_Errno --
   -------------------
   function Pthread_Errno return System.Address is
   begin
      return SCHD.Self.Error_Code'Address;
   end Pthread_Errno;

   -----------------------
   -- Set_Pthread_Errno --
   -----------------------
   procedure Set_Pthread_Errno (Errno : in Int) is
   begin
      SCHD.Self.Error_Code  := Errno;
      SCHD.Self.POSIX_Error := True;
   end Set_Pthread_Errno;

   ----------------------------------------------------------------------------
   -- Condition Variables -----------------------------------------------------
   ----------------------------------------------------------------------------

   -------------------------------------------------------
   -- Pthread_Cond_Timedwait (11.4 Condition Variables) --
   -------------------------------------------------------
   function Pthread_Cond_Timedwait (Cond    : CV.Condition_Descriptor;
                                    M       : Mutexes.Mutex_Descriptor;
                                    Abstime : access MaRTE.Timespec.Timespec)
                                    return    Int is
   begin
      return
        Condition_Variables.Timed_Wait_HWTime
        (Cond, M, MaRTE.Timespec.Timespec_To_HWTime (Abstime.all));
   end Pthread_Cond_Timedwait;

   ----------------------------------------------------------------------------
   --  Threads  ---------------------------------------------------------------
   ----------------------------------------------------------------------------

   --------------------------------------------------------------
   -- Pthread_Attr_Setscope (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   --
   --  Returns ENOSYS.
   function Pthread_Attr_Setscope
     (Attr            : access TO.Pthread_Attr_T;
      contentionscope : Int) return Int is
   begin
      pragma Debug (DBG.Assert (False));
      return OPERATION_NOT_IMPLEMENTED;
   end Pthread_Attr_Setscope;

   --------------------------------------------------------------
   -- Pthread_Attr_Getscope (13.5 Thread Scheduling Functions) --
   --------------------------------------------------------------
   --
   --  Returns ENOSYS.
   function Pthread_Attr_Getscope
     (Attr            : access TO.Pthread_Attr_T;
      contentionscope : access Int) return Int is
   begin
      pragma Debug (DBG.Assert (False));
      return OPERATION_NOT_IMPLEMENTED;
   end Pthread_Attr_Getscope;

   -------------------------------------------------------------------
   -- Pthread_Setschedparam_Gnat (13.5 Thread Scheduling Functions) --
   -------------------------------------------------------------------
   --
   --  This function is necessary because Gnat only knows about the
   --  'sched_priority' param. So the others must be set "by hand"
   --  internally in this function.
   function Pthread_Setschedparam_Gnat (T              : Task_Id;
                                        Policy         : Scheduling_Policies;
                                        Sched_Priority : Task_Priority)
                                        return Int is
      Param_Tmp : aliased TO.Sched_Param :=
        (Sched_Priority        => Sched_Priority,
         Sched_SS_Low_Priority => Task_Priority'First,
         Sched_SS_Repl_Period  => (0, 0),
         Sched_SS_Init_Budget  => (0, 0),
         Sched_SS_Max_Repl     => 0);
   begin
      return Pthread_Setschedparam (T, Policy, Param_Tmp'Access);
   end Pthread_Setschedparam_Gnat;

   --------------------------------
   -- Pthread_Equal (16 Threads) --
   --------------------------------
   function Pthread_Equal (T1 : K.Task_Id;
                           T2 : K.Task_Id) return Int is
   begin
      if K.Tasks_Lists."=" (T1, T2) then
         return 1;
      else
         return 0;
      end if;
   end Pthread_Equal;

   ----------------------------------------------------------------------------
   -- Attributes Stack Management (16 threads) --------------------------------
   ----------------------------------------------------------------------------

   function Pthread_Attr_Setstackaddr (Attr      : access Pthread_Attr_T;
                                       Stackaddr : System.Address)
                                  return Int is
   begin
      return OPERATION_NOT_IMPLEMENTED;
   end Pthread_Attr_Setstackaddr;

   function Pthread_Attr_Getstackaddr (Attr      : access Pthread_Attr_T;
                                       Stackaddr : access System.Address)
                                  return Int is
   begin
      return OPERATION_NOT_IMPLEMENTED;
   end Pthread_Attr_Getstackaddr;

   ----------------------------------------------------------------------------
   --  POSIX.1c  Section 17  --------------------------------------------------
   ----------------------------------------------------------------------------

   type Key_Used_Element is record
      Used      : Boolean;
      Use_Count : Natural;
   end record;
   Key_Used : array (CP.Keys_Range) of Key_Used_Element;
   First_Time_In_Pthread_Key_Create : Boolean := True;
   pragma Volatile (First_Time_In_Pthread_Key_Create);

   ------------------------
   -- Pthread_Key_Create --
   ------------------------
   --
   --  'Destructor' must be null (functionality not implemented yet).
   function Pthread_Key_Create (Key        : Pthread_Key_T_Ac;
                                Destructor : System.Address) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if First_Time_In_Pthread_Key_Create then
         Key_Used := (others => (False, 0));
         First_Time_In_Pthread_Key_Create := False;
      end if;
      pragma Debug
        (DBG.Assert (System."=" (Destructor, System.Null_Address),
                 "Called Pthread_Key_Create with Destructor /= null !!"));

      --  Try to find an unused specific data item
      for K in Pthread_Key_T'Range loop
         if not Key_Used (K).Used then
            --  Found
            Key_Used (K).Used := True;
            Key_Used (K).Use_Count := Key_Used (K).Use_Count + 1;
            Key.all := K;
            MaRTE.Kernel.Leave_Critic_Section (Flags);
            return 0;
         end if;
      end loop;

      --  Not found any specific data item unused
      K.Leave_Critic_Section (Flags);
      return RESOURCE_TEMPORARILY_UNAVAILABLE;
   end Pthread_Key_Create;

   -------------------------
   -- Pthread_Setspecific --
   -------------------------
   function Pthread_Setspecific (Key   : Pthread_Key_T;
                                 Value : System.Address) return Int is
   begin
      if SCHD.Self.Task_Type = K.SIGNAL_HANDLER_TASK then
         return Pthread_Setspecific_For
           (Key,
            K.Signals.Handler.Current_Siginst_Ac.To_Task,
            Value);
      else
         return Pthread_Setspecific_For (Key, SCHD.Self, Value);
      end if;
   end Pthread_Setspecific;

   -----------------------------
   -- Pthread_Setspecific_For --
   -----------------------------
   function Pthread_Setspecific_For (Key   : Pthread_Key_T;
                                     T     : K.Task_Id;
                                     Value : System.Address) return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (T.Task_Type /= SIGNAL_HANDLER_TASK));

      K.Enter_Critic_Section (Flags);
      if not K.Task_OK (T, NEW_APPSCHED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if ((Key < Pthread_Key_T'First or Pthread_Key_T'Last < Key)
          or else not Key_Used (Key).Used) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      T.Specific_Data (Key).Data := Value;
      pragma Debug
        (DBG.Assert (T.Specific_Data (Key).Use_Count <=
                     Key_Used (Key).Use_Count));
      T.Specific_Data (Key).Use_Count := Key_Used (Key).Use_Count;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Setspecific_For;
   pragma Inline (Pthread_Setspecific_For);

   -------------------------
   -- Pthread_Getspecific --
   -------------------------
   function Pthread_Getspecific (Key : Pthread_Key_T) return System.Address is
      Flags : Integer;
      Spec_Data : K.Specific_Data_Element;
   begin
      K.Enter_Critic_Section (Flags);
      if ((Key < Pthread_Key_T'First or Pthread_Key_T'Last < Key)
          or else not Key_Used (Key).Used) then
         K.Leave_Critic_Section (Flags);
         return System.Null_Address;
      end if;

      K.Leave_Critic_Section (Flags);

      --  Special case for the signal handler task
      if SCHD.Self.Task_Type = K.SIGNAL_HANDLER_TASK then
         Spec_Data :=
           K.Signals.Handler.Current_Siginst_Ac.To_Task.Specific_Data
           (Key);
      else
         Spec_Data := SCHD.Self.Specific_Data (Key);
      end if;

      pragma Debug (DBG.Assert (Spec_Data.Use_Count
                                <= Key_Used (Key).Use_Count));
      --  If the key is not set, set it to null now
      if Spec_Data.Use_Count < Key_Used (Key).Use_Count then
         --  Set key to null
         Spec_Data.Use_Count := Key_Used (Key).Use_Count;
         Spec_Data.Data := System.Null_Address;
      end if;

      return Spec_Data.Data;
   end Pthread_Getspecific;

   ------------------------------
   -- Pthread_Getspecific_From --
   ------------------------------
   function Pthread_Getspecific_From (Key   : in     Pthread_Key_T;
                                      T     : in     K.Task_Id;
                                      Value : access System.Address)
                                      return Int is
      Flags : Integer;
   begin
      pragma Debug (DBG.Assert (T.Task_Type /= SIGNAL_HANDLER_TASK));
      K.Enter_Critic_Section (Flags);
      if not K.Task_OK (T, NEW_APPSCHED) then
         K.Leave_Critic_Section (Flags);
         return NO_SUCH_PROCESS;
      end if;
      if ((Key < Pthread_Key_T'First or Pthread_Key_T'Last < Key)
          or else not Key_Used (Key).Used) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      pragma Debug
        (DBG.Assert (T.Specific_Data (Key).Use_Count
                     <= Key_Used (Key).Use_Count));
      --  If the key is not set, set it to null now
      if T.Specific_Data (Key).Use_Count < Key_Used (Key).Use_Count then
         --  Set key to null
         T.Specific_Data (Key).Use_Count := Key_Used (Key).Use_Count;
         T.Specific_Data (Key).Data := System.Null_Address;
      end if;

      Value.all := T.Specific_Data (Key).Data;
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Getspecific_From;
   pragma Inline (Pthread_Getspecific_From);

   ------------------------
   -- Pthread_Key_Delete --
   ------------------------
   function Pthread_Key_Delete (Key : Pthread_Key_T) return Int is
      Flags : Integer;
   begin
      K.Enter_Critic_Section (Flags);
      if ((Key < Pthread_Key_T'First or Pthread_Key_T'Last < Key)
          or else not Key_Used (Key).Used) then
         K.Leave_Critic_Section (Flags);
         return INVALID_ARGUMENT;
      end if;

      Key_Used (Key).Used := False;
      --  Do NOT modify 'Key_Used (Key).Use_Count'
      K.Leave_Critic_Section (Flags);
      return 0;
   end Pthread_Key_Delete;

end MaRTE.POSIX_Pthread;
