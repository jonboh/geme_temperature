------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . H a r d w a r e _ I n t e r r u p t s'
--
--                                 Body
--
--
--  File 'k-hardware_interrupts.adb'                                   By MAR.
--
--
--  Internal hardware interrupts management.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Configuration_Parameters;
package body MaRTE.Kernel.Hardware_Interrupts is

   use K.Tasks_Lists;
   package SCHD renames K.Scheduler;
   package CP   renames MaRTE.Configuration_Parameters;

   use type CP.Use_HW_Interrupts_Control;
   use type Int;

   Initialized : Boolean := False;

   -----------------------------
   -- Interrupt_Nesting_Count --
   -----------------------------
   Interrupt_Nesting_Count : Unsigned_8;
   pragma Import (C, Interrupt_Nesting_Count, "base_irq_nest");
   --  Hardware interrupt nesting counter (modified in the interrupt
   --  handlers in 'x86/boot/base_irq_inittab.S'), used to ensure that the
   --  software interrupt handler ('Do_Scheduling') isn't called until all
   --  outstanding hardware interrupts have been processed. In addition,
   --  this byte also acts as the software interrupt pending flag: if the
   --  high bit is set, no software interrupt is pending; if the high bit
   --  is clear, a software interrupt is pending. This design allows the
   --  interrupt path to decide whether to call the software interrupt
   --  handler simply by testing the word for zero.
   --  'base_irq_nest' is defined in 'x86/boot/base_irq.c'.
   Outside_Of_Interrupts : constant := 16#80#;
   Invoke_Do_Scheduling_Mask : constant := 16#7F#; -- not 16#80#;

   ---------------------------------
   -- Interrupt associations pool --
   ---------------------------------
   All_Associations : array (1 .. CP.Mx_Number_Of_Interrupt_Associations)
     of aliased Associations_Lists.Element;

   ---------------------
   -- Wrapper_Handler --
   ---------------------
   Intr  : Intr_T;
   Assoc : Association;
   Handler_Ret : Handler_Return_Code;
   procedure Wrapper_Handler (State : in HAL.Trap_State_Ac);
   procedure Wrapper_Handler (State : in HAL.Trap_State_Ac) is
      use type Association;
   begin
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return;
      end if;

      Intr := Intr_T (State.ERR);

      --  Execute interrupt handlers (if any)
      Assoc := Associations_Lists.Head (Associations (Intr));
      if Assoc = null then
         --  No user's interrupt handler => executes default interrupt
         --  handler and leave
         HAL.Default_HW_Interrupt_Handler (State);
      else
         Handler_Ret := POSIX_INTR_NOT_HANDLED;
         loop
            --  Executes handler
            Handler_Ret := Assoc.Handler.all (Assoc.Area, Intr);
            exit when (Handler_Ret = POSIX_INTR_HANDLED_NOTIFY or
                       Handler_Ret = POSIX_INTR_HANDLED_DO_NOT_NOTIFY);
            Assoc := Associations_Lists.Next (Assoc);
            exit when Assoc = null;
         end loop;

         --  Must the waiting task be activated ??
         if Handler_Ret = POSIX_INTR_HANDLED_NOTIFY then

            pragma Assert (K.Task_Ok (Assoc.T));

            if (Assoc.T.Status = TIMED_WAITING_HW_INTERRUPT or
                Assoc.T.Status = WAITING_HW_INTERRUPT) then
               --  The taks was waiting the interrupt

               --  Remove timeout?
               if Assoc.T.Status = TIMED_WAITING_HW_INTERRUPT then
                  SCHD.Remove_Timed_Event (Assoc.T.Suspension_Event);
               end if;

               --  Put task ready
               SCHD.Task_Gets_Ready (Assoc.T);

               --  Annotate interrupt association
               Assoc.T.Last_Deliver_Intr          := Intr;
               Assoc.T.Last_Executed_Intr_Handler := Assoc.Handler;

               --  The "software interrupt handler" ('Do_Scheduling') is put
               --  pending. It will be called when all outstanding hardware
               --  interrupts have been processed.
               Interrupt_Nesting_Count :=
                 Interrupt_Nesting_Count and Invoke_Do_Scheduling_Mask;
            else
               --  Task isn't waiting the interrupt, then just mark it as
               --  "pending"
               Assoc.Pending := True;
            end if;
         end if;
      end if;
   end Wrapper_Handler;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached (T : in K.Task_Id) is
   begin
      pragma Assert (Initialized);
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return;
      end if;

      --  Set the internal notification to record the task is
      --  activated due to a timeout (it will be checked in
      --  'Operations.Posix_Intr_Timedwait_HWTime')
      pragma Assert (T.Internal_Error_Code = NO_ERROR);
      T.Internal_Error_Code := TIMED_OUT;

      --  Now suspended task gets ready
      SCHD.Task_Gets_Ready (T);

      --  When 'T' is scheduled again it will execute the final part of
      --  'Posix_Intr_Timedwait_HWTime'
   end Timeout_Reached;

   ----------------------------------
   -- Reserved hardware interrupts --
   ----------------------------------
   Reserved : array (Intr_T) of Boolean := (others => False);
   procedure Reserve_Interrupt (Intr : in Intr_T) is
   begin
      pragma Assert (Initialized);
      Reserved (Intr) := True;
   end Reserve_Interrupt;

   function Is_Reserved_Interrupt (Intr : in Intr_T) return Boolean is
   begin
      pragma Assert (Initialized);
      return Reserved (Intr);
   end Is_Reserved_Interrupt;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      Initialized := True;
      if not MaRTE.Configuration_Parameters.Use_HW_Interrupts_Control'First then
         return;
      end if;

      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         ------------
         ----  Set free all the associations
         ----
         for I in All_Associations'Range loop
            Associations_Pool.Release (All_Associations (I)'Access);
         end loop;
      end if;

      --  Initialize User Interrupt Handlers
      for I in Intr_T loop
         HAL.Install_HW_Interrupt_Handler (I, Wrapper_Handler'Access);
      end loop;
   end Initialize;

end MaRTE.Kernel.Hardware_Interrupts;
