------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . S i g n a l s . I n t e r n a l s'
--
--                                 Body
--
--
--  File 'k-signals-internals.adb'                                    By MAR.
--
--
--  Signal related types and procedures not included in the POSIX
--  standard. Only for internal use inside the herarchy of 'Scheduler'.
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
with Ada.Unchecked_Conversion;

with MaRTE.Kernel.Signals.Global;
use MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Scheduler;
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);
with MaRTE.Kernel.Signals.Pending;
with MaRTE.Kernel.Signals.Handler; use MaRTE.Kernel.Signals.Handler;
with MaRTE.Kernel.Application_Scheduler;

--  Debug
with MaRTE.Direct_IO;
with MaRTE.Kernel.Signals.Debug;

package body MaRTE.Kernel.Signals.Internals is

   package SCHD         renames K.Scheduler;
   package SP           renames K.Signals.Pending;
   package APPSCHD      renames K.Application_Scheduler;

   use K.Tasks_Lists, MaRTE.Kernel.Signals.Global.Sig_Data_Lists,
       K.Signals.Global.Signal_Instances_Lists;

   Initialized : Boolean := False;

   --------------------------------------------------------------------------
   -- Reserve, request and release Signal Instances -------------------------
   --------------------------------------------------------------------------

   ---------------------------
   -- Signal Instances Pool --
   ---------------------------
   Free_Signal_Instances :
     array (1 .. MaRTE.Configuration_Parameters.Queued_Signals_Maximum)
     of aliased Global.Signal_Instances_Lists.Element;
   package Signal_Instances_Pool is
      new Global.Signal_Instances_Lists.Resources
     (Global.Signal_Instances_Lists.Element);
   Default_Free_Signal_Instance : aliased constant Signal_Instance :=
     (Signo      => Signal_Null,
      Code       => SI_USER,
      Value      => 0,
      To_Task    => null,
      Reserved   => False,
      Pending    => False);

   -----------------------------
   -- Request_Signal_Instance --
   -----------------------------
   function Request_Signal_Instance return Signal_Instance_Ac is
      Ac : Signal_Instance_Ac := Signal_Instances_Pool.Request;
   begin
      pragma Assert (Initialized);
      if Ac /= null then
         Signal_Instance (Ac.all) := Default_Free_Signal_Instance;
      end if;
      return Ac;
   end Request_Signal_Instance;

   -----------------------------------------
   -- Request_And_Reserve_Signal_Instance --
   -----------------------------------------
   function Request_And_Reserve_Signal_Instance return Signal_Instance_Ac is
      Ac : Signal_Instance_Ac := Signal_Instances_Pool.Request;
   begin
      pragma Assert (Initialized);
      if Ac /= null then
         Signal_Instance (Ac.all) := Default_Free_Signal_Instance;
         Ac.Reserved := True;
         Ac.Pending := False;
      end if;
      return Ac;
   end Request_And_Reserve_Signal_Instance;

   -----------------------------
   -- Release_Signal_Instance --
   -----------------------------
   procedure Release_Signal_Instance (Siginst_Ac : in Signal_Instance_Ac) is
   begin
      pragma Assert (Initialized);
      if Siginst_Ac.Reserved then
         Siginst_Ac.Pending := False;
      else
         Signal_Instances_Pool.Release (Siginst_Ac);
      end if;
   end Release_Signal_Instance;

   ------------------------------------------------
   -- Cancel_Reserve_And_Release_Signal_Instance --
   ------------------------------------------------
   procedure Cancel_Reserve_And_Release_Signal_Instance
     (Siginst_Ac : in Signal_Instance_Ac) is
   begin
      pragma Assert (Initialized);
      pragma Assert (Siginst_Ac.Reserved,
                     "Release_Signal_Instance: not reserved signal");
      Siginst_Ac.Reserved := False;
      Signal_Instances_Pool.Release (Siginst_Ac);
   end Cancel_Reserve_And_Release_Signal_Instance;

   -----------------------------------------
   -- Is_Reserved_Signal_Instance_Pending --
   -----------------------------------------
   function Is_Reserved_Signal_Instance_Pending
     (Siginst_Ac : in Signal_Instance_Ac) return Boolean is
   begin
      pragma Assert (Initialized);
      pragma Assert
        (Siginst_Ac.Reserved,
         "Is_Reserved_Signal_Instance_Pending: not reserved signal");
      return Siginst_Ac.Pending;
   end Is_Reserved_Signal_Instance_Pending;

   --------------------------------------------------------------------------
   -- Operations over 'Sig_Data_Blocks_Used' list ---------------------------
   --------------------------------------------------------------------------

   -------------------------------------
   -- Find_Task_With_Signal_Unblocked --
   -------------------------------------
   function Find_Task_With_Signal_Unblocked (Sig : Signal) return Sig_Data_Ac;
   function Find_Task_With_Signal_Unblocked (Sig : Signal)
                                             return Sig_Data_Ac is
      SDB_Ac : Sig_Data_Ac :=
        Sig_Data_Lists.Head (Sig_Data_Blocks_Used);
   begin
      while SDB_Ac /= null loop
         if not SDB_Ac.Mask (Sig) then
            return SDB_Ac;
         end if;
         SDB_Ac := Sig_Data_Lists.Next (SDB_Ac);
      end loop;
      return null;
   end Find_Task_With_Signal_Unblocked;

   ----------------------------------
   -- Find_Task_Waiting_For_Signal --
   ----------------------------------
   function Find_Task_Waiting_For_Signal (Sig : Signal) return Sig_Data_Ac;
   function Find_Task_Waiting_For_Signal (Sig : Signal) return Sig_Data_Ac is
      Sigdata_Ac : Global.Sig_Data_Ac :=
        Global.Sig_Data_Lists.Head (Sig_Data_Blocks_Used);
   begin
      while Sigdata_Ac /= null loop
         if (Sigdata_Ac.Is_Waiting_For_Signals and then
             Sigdata_Ac.Waited_Signals (Sig)) then
            return Sigdata_Ac;
         end if;
         Sigdata_Ac := Sig_Data_Lists.Next (Sigdata_Ac);
      end loop;
      return null;
   end Find_Task_Waiting_For_Signal;

   --------------------------------------------------------------------------
   -- Start and finish waiting for signal -----------------------------------
   --------------------------------------------------------------------------

   ------------------------
   -- Awake_Task_Waiting --
   ------------------------
   procedure Awake_Task_Waiting (Siginst_Ac : in Signal_Instance_Ac);
   procedure Awake_Task_Waiting (Siginst_Ac : in Signal_Instance_Ac) is
   begin
      pragma Assert (K.Task_OK (Siginst_Ac.To_Task),
                     "Awake_Task_Waiting: invalid task");
      pragma Assert (Global.Sig_Data_Ac
                       (Siginst_Ac.To_Task.Sig_Data).Is_Waiting_For_Signals);
      Global.Sig_Data_Ac
        (Siginst_Ac.To_Task.Sig_Data).Is_Waiting_For_Signals := False;
      Global.Sig_Data_Ac (Siginst_Ac.To_Task.Sig_Data).Accepted_Siginst_Ac
        := Siginst_Ac;
      if (Siginst_Ac.To_Task.Status = WAITING_APPSCHED_EVENT or
          Siginst_Ac.To_Task.Status = TIMED_WAITING_APPSCHED_EVENT) then
         --  Application Scheduler task waiting for events (and signals).
         APPSCHD.Send_Signal_Event (ST      => Siginst_Ac.To_Task,
                                    Siginfo => (Signo => Siginst_Ac.Signo,
                                                Code => Siginst_Ac.Code,
                                                Value => Siginst_Ac.Value));

         Release_Signal_Instance (Siginst_Ac);
      else
         --  Normal task waiting for signals
         SCHD.Task_Gets_Ready (Siginst_Ac.To_Task);
         SCHD.Do_Scheduling;
      end if;
   end Awake_Task_Waiting;

   ------------------------------------------
   -- Remove_Task_From_Waiting_For_Signals --
   ------------------------------------------
   procedure Remove_Task_From_Waiting_For_Signals (T : in K.Task_Id) is
   begin
      pragma Assert (Initialized);
      pragma Assert (Global.Sig_Data_Ac (T.Sig_Data).Is_Waiting_For_Signals);
      Global.Sig_Data_Ac (T.Sig_Data).Is_Waiting_For_Signals := False;
   end Remove_Task_From_Waiting_For_Signals;

   ------------------------------------------
   -- Register_Self_As_Waiting_For_Signals --
   ------------------------------------------
   procedure Register_Self_As_Waiting_For_Signals
     (Set : in Signal_Set_Ac_Const) is
      Self_Sigdata_Ac : Global.Sig_Data_Ac := Sig_Data_Ac (SCHD.Self.Sig_Data);
   begin
      pragma Assert (Initialized);
      pragma Assert (not Self_Sigdata_Ac.Is_Waiting_For_Signals);
      Self_Sigdata_Ac.Waited_Signals := Set.all;
      Self_Sigdata_Ac.Is_Waiting_For_Signals := True;
      pragma Debug (Debug.Add_Self_To_The_Waiting_For_Signals_List);
   end Register_Self_As_Waiting_For_Signals;

   -----------------------------------------
   -- Abnormal_Termination_Of_The_Process --
   -----------------------------------------
   --
   --  Used in default actions
   procedure Exit_Process (Status : in Int);
   pragma Import (C, Exit_Process, "exit");
   procedure Abnormal_Termination_Of_The_Process (Sig : in Signal);
   procedure Abnormal_Termination_Of_The_Process (Sig : in Signal) is
   begin
      MaRTE.Direct_IO.Put ("Signal " & Signal'Image (Sig) & " Delivered:");
      MaRTE.Direct_IO.Put
        ("Taking Default Action: Process Abnormal Termination");
      Exit_Process (1);
   end Abnormal_Termination_Of_The_Process;

   ------------------------------
   -- Deliver_Signal_Instances --
   ------------------------------
   procedure Deliver_Signal_Instances (Sig        : in Signal;
                                       To_Sigdata : in Sig_Data_Ac) is
   begin
      pragma Assert (Initialized);
      if SP.Is_Signal_Pending (Sig, To_Sigdata) then
         if Actions (Sig).Action = SIGNAL_DEFAULT then
            Abnormal_Termination_Of_The_Process (Sig);
            pragma Debug
              (MaRTE.Direct_IO.Error
               ("Delivery_Signal: This point shouldn't have been reached..."));
         elsif Actions (Sig).Action = SIGNAL_IGNORE then
            SP.Discard_Pending_Instances (Sig);
         else
            SP.Add_Signal_Instaces_To_Handler_Q (Sig, To_Sigdata);
            if Handler.Handler_Task.Status /= READY then
               SCHD.Task_Gets_Ready (Handler.Handler_Task);
               SCHD.Do_Scheduling;
            end if;
         end if;
      end if;
   end Deliver_Signal_Instances;

   ----------------------------
   -- Send_Signal_To_Process --
   ----------------------------
   procedure Send_Signal_To_Process (Siginst_Ac : in Signal_Instance_Ac) is
      Sigdata_Ac : Global.Sig_Data_Ac;
   begin
      pragma Assert (Initialized);
      --  First of all, if the associated action is 'SIG_IGN' or is the null
      --  signal isn't necessary do anything.
      if not (Actions (Siginst_Ac.Signo).Action = SIGNAL_IGNORE or
              Siginst_Ac.Signo = SIGNULL) then

         Siginst_Ac.Pending := True;

         --  Look for a task waiting for 'Sig'
         Sigdata_Ac := Find_Task_Waiting_For_Signal (Siginst_Ac.Signo);
         if Sigdata_Ac /= null then
            Siginst_Ac.To_Task := Sigdata_Ac.Owner;
            Awake_Task_Waiting (Siginst_Ac);
            return;
         end if;

         SP.Signal_Instance_Generated_For_Process (Siginst_Ac);
         --  If no task waiting look if 'Sig' is unblocked in some task
         Sigdata_Ac := Find_Task_With_Signal_Unblocked (Siginst_Ac.Signo);
         if Sigdata_Ac /= null then
            Siginst_Ac.To_Task := Sigdata_Ac.Owner;
            Deliver_Signal_Instances (Siginst_Ac.Signo, Sigdata_Ac);
            return;
         end if;
      end if;
   end Send_Signal_To_Process;

   -------------------------
   -- Send_Signal_To_Task --
   -------------------------
   procedure Send_Signal_To_Task (Siginst_Ac : in Signal_Instance_Ac) is
      Sigdata_Ac : Global.Sig_Data_Ac;
   begin
      pragma Assert (Initialized);
      pragma Assert (K.Task_OK (Siginst_Ac.To_Task),
                     "Send_Signal_To_Task: invalid task");
      --  First of all, if the associated action is 'SIG_IGN' or it is the null
      --  signal it isn't necessary to do anything.
      if not (Actions (Siginst_Ac.Signo).Action = SIGNAL_IGNORE or
              Siginst_Ac.Signo = SIGNULL) then

         Siginst_Ac.Pending := True;
         Sigdata_Ac := Global.Sig_Data_Ac (Siginst_Ac.To_Task.Sig_Data);
         if (Sigdata_Ac.Is_Waiting_For_Signals and then
             Sigdata_Ac.Waited_Signals (Siginst_Ac.Signo)) then
            --  Task was waiting for 'Sig'
            Awake_Task_Waiting (Siginst_Ac);

         else
            SP.Signal_Instance_Generated_For_Task (Siginst_Ac);
            if not Sigdata_Ac.Mask (Siginst_Ac.Signo) then
               --  Deliver signals
               Deliver_Signal_Instances (Siginst_Ac.Signo, Sigdata_Ac);
            end if;
         end if;
      end if;
   end Send_Signal_To_Task;

   ---------------------------------
   --  Task_Stops_Getting_Signals --
   ---------------------------------
   procedure Task_Stops_Getting_Signals (T : Task_Id) is
   begin
      pragma Assert (Initialized);
      pragma Assert (K.Task_Terminated (T));
      pragma Assert
        (Sig_Data_Lists.Is_In_The_List
           (Sig_Data_Ac (T.Sig_Data), Sig_Data_Blocks_Used));
      --  Remove sig_data from the sig_data blocks used list
      Sig_Data_Lists.Dequeue (Sig_Data_Ac (T.Sig_Data),
                              Sig_Data_Blocks_Used);
   end Task_Stops_Getting_Signals;

   function To_Addr is new Ada.Unchecked_Conversion (Int, System.Address);

   -----------------
   --  Initialize --
   -----------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      --  Initialize 'Actions'
      for S in Signal_Set_Range loop
         Actions (S) := (Sa_Handler => To_Addr (SIG_DFL),
                         Action     => Global.SIGNAL_DEFAULT,
                         Mask       => Blockeable_Signals,
                         Flags      => Sa_Flags_T'(0),
                         Handler    => null,
                         Sigaction  => null);
      end loop;

      if CP.Preallocated_Resources'First then
         --  Initialize 'Free_Signal_Instances'
         for I in 1 .. MaRTE.Configuration_Parameters.Queued_Signals_Maximum loop
            Signal_Instances_Pool.Release (Free_Signal_Instances (I)'Access);
         end loop;
      end if;

      Initialized := True;
   end Initialize;

end MaRTE.Kernel.Signals.Internals;
