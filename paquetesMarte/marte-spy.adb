------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                      'T a s k s _ I n s p e c t o r'
--
--                                 Body
--
--
--  File 'tasks_inspector.adb'                                        By Mar.
--
--
--  Operantions to send scheduling events to the host using a serial
--  port. The events sent using 'Send_Event' can be interpretated by the
--  "Tasks Inspector" tool to display the tasks activity graphically.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Debug_Messages;
with MaRTE.Configuration_Parameters;
with MaRTE.Kernel.Debug;
with MaRTE.Direct_IO;

package body MaRTE.Spy is

   package K   renames MaRTE.Kernel;
   package CP  renames MaRTE.Configuration_Parameters;
   package DBG renames MaRTE.Kernel.Debug;
   package DIO renames MaRTE.Direct_IO;

   use type CP.Supported_Architectures;

   ----------------------------------
   -- Imports from 'marte-spy_c.c' --
   ----------------------------------
   procedure Spy_Init
     (Log_Device : MaRTE.Debug_Messages.Log_Device;
      Use_Logger_Task : MaRTE.Debug_Messages.Logger_Mechanism);
   pragma Import (C, Spy_Init, "_marte_spy_init");

   procedure Spy_Send_Event_One_Object (Timestamp : in HAL.HWTime;
                                        C_Op1     : in Character;
                                        C_Op2     : in Character;
                                        Id1       : in Int;
                                        Prio1     : in Int);
   pragma Import (C, Spy_Send_Event_One_Object,
                  "_marte_spy_send_event_one_object");

   procedure Spy_Send_Event_Two_Objects (Timestamp : in HAL.HWTime;
                                         C_Op1     : in Character;
                                         C_Op2     : in Character;
                                         Id1       : in Int;
                                         Prio1     : in Int;
                                         Id2       : in Int;
                                         Prio2     : in Int);
   pragma Import (C, Spy_Send_Event_Two_Objects,
                  "_marte_spy_send_event_two_objects");

   procedure Spy_Send_Event_Time (Timestamp : in HAL.HWTime;
                                  C_Op1     : in Character;
                                  C_Op2     : in Character;
                                  T         : in HAL.HWTime);
   pragma Import (C, Spy_Send_Event_Time, "_marte_spy_send_event_time");

   -----------------
   -- Initialized --
   -----------------

   Initialized : Boolean := False;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Initital_Prio_Of_Main_Task : Kernel.Task_Priority) is
      Num_Tasks_Mx : Integer;
      Interrupts_Enabled : Boolean := False;
      Flags : Integer;
      use type Int;
   begin
      if not MaRTE.Debug_Messages.Tasks_Inspector_Messages then
         return;
      end if;

      if CP.MaRTE_Architecture'First = CP.ARCH_LINUX then
         --  tracer not supported for ARCH_LINUX yet
         pragma Debug (DBG.Assert (False));
         return;
      end if;

      DIO.Put ("Initializing MaRTE tracer...");

      if HAL.Are_Interrupts_Enabled then
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         Interrupts_Enabled := True;
      end if;

      --  Set maximum number of tasks

      if CP.Preallocated_Resources'First then
         Num_Tasks_Mx := CP.Num_MaRTE_Tasks_Mx;
      else
         Num_Tasks_Mx := 50;
         --  ??? When resources are not preallocated the max number of tasks is
         --  only limited by the amount of free memory. We put a "long" number:
         --  50
      end if;

      --  Initialice the tracer mechanism

      Spy_Init (MaRTE.Debug_Messages.Tasks_Inspector_Log_Device,
                MaRTE.Debug_Messages.Task_Inspector_Logger_Mechanism);

      --  Set 'Initialized'

      pragma Debug (DBG.Assert (not Initialized));
      Initialized := True;

      --  Send Initial data

      --     #0#ID(Num_Tasks,Num_Prios)#

      Spy_Send_Event_One_Object
        (0, 'I', 'D',
         Int (Num_Tasks_Mx), CP.Task_Priority_Mx + 1);

      --     #0#CF(Clock_freq,0)#

      Spy_Send_Event_One_Object
        (0, 'C', 'F',
         Int (HAL.Get_HWClock_Frequency), 0);

      --     #init_timestamp#TR(0,Prio_Of_Main)#

      Spy_Send_Event_One_Object
        (HAL.Get_HWTime_Slow, 'T', 'R',
         0, Int (Initital_Prio_Of_Main_Task));

      DIO.Put (" OK"); DIO.New_Line;

      if Interrupts_Enabled then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end if;
   end Initialize;

   ----------------
   -- Send_Event --
   ----------------
   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         T1        : in MaRTE.Kernel.Task_Id;
                         T2        : in MaRTE.Kernel.Task_Id) is
      use HAL;
      Interrupts_Enabled : Boolean := False;
      Flags : Integer;
      use type K.Task_Types, K.Task_Id;
   begin
      if not MaRTE.Debug_Messages.Tasks_Inspector_Messages then
         return;
      end if;

      if CP.MaRTE_Architecture'First = CP.ARCH_LINUX then
         --  tracer not supported for ARCH_LINUX yet
         pragma Debug (DBG.Assert (False));
         return;
      end if;

      --  The first notification of the running task gets ready should be
      --  discarded because it happens at the very beginning of kernel
      --  initialization before 'Spy.Initialize' has been called.
      --  The event of main task ready will be sent latter by 'Spy.Initialize'

      if not Initialized
        and then Operation (Operation'First) = 'T'
        and then Operation (Operation'First + 1) = 'R'
        and then T1.Task_Type = K.MAIN_TASK
        and then T2 = null
      then
         return;  --  Event discarded
      end if;

      pragma Debug (DBG.Assert (Initialized));

      if HAL.Are_Interrupts_Enabled then
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         Interrupts_Enabled := True;
      end if;

      if MaRTE.Kernel.Tasks_Lists."=" (T2, null) then
         Spy_Send_Event_One_Object
           (Timestamp,
            Operation (Operation'First), Operation (Operation'First + 1),
            Int (T1.Id), Int (T1.Active_Prio));
      else
         Spy_Send_Event_Two_Objects
           (Timestamp,
            Operation (Operation'First), Operation (Operation'First + 1),
            Int (T1.Id), Int (T1.Active_Prio),
            Int (T2.Id), Int (T2.Active_Prio));
      end if;

      if Interrupts_Enabled then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end if;
   end Send_Event;

   ----------------
   -- Send_Event --
   ----------------
   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         Id        : in Integer;
                         Prio      : in MaRTE.Kernel.Task_Priority) is
      use HAL;
      Interrupts_Enabled : Boolean := False;
      Flags : Integer;
   begin
      if not MaRTE.Debug_Messages.Tasks_Inspector_Messages then
         return;
      end if;

      if CP.MaRTE_Architecture'First = CP.ARCH_LINUX then
         --  tracer not supported for ARCH_LINUX yet
         pragma Debug (DBG.Assert (False));
         return;
      end if;

      pragma Debug (DBG.Assert (Initialized));

      if HAL.Are_Interrupts_Enabled then
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         Interrupts_Enabled := True;
      end if;

      Spy_Send_Event_One_Object
        (Timestamp,
         Operation (Operation'First), Operation (Operation'First + 1),
         Int (Id), Int (Prio));

      if Interrupts_Enabled then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end if;
   end Send_Event;

   ----------------
   -- Send_Event --
   ----------------
   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         T         : in HAL.HWTime) is
      use HAL;
      Interrupts_Enabled : Boolean := False;
      Flags : Integer;
   begin
      if not MaRTE.Debug_Messages.Tasks_Inspector_Messages then
         return;
      end if;

      if CP.MaRTE_Architecture'First = CP.ARCH_LINUX then
         --  tracer not supported for ARCH_LINUX yet
         pragma Debug (DBG.Assert (False));
         return;
      end if;

      pragma Debug (DBG.Assert (Initialized));

      if HAL.Are_Interrupts_Enabled then
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         Interrupts_Enabled := True;
      end if;

      Spy_Send_Event_Time
        (Timestamp,
         Operation (Operation'First), Operation (Operation'First + 1),
         T);

      if Interrupts_Enabled then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end if;
   end Send_Event;

end MaRTE.Spy;
