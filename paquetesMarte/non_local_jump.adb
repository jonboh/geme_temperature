------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                      'N o n _ L o c a l _ J u m p'
--
--                                  Body
--
--  File 'non_local_jump.adb'                                          By MAR
--
--  Non-local jumps for preempted tasks.
--
--  IMPORTANT: it must be compiled without any optimization!!
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
with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;
with MaRTE.HAL;

package body Non_Local_Jump is
   use MaRTE.Integer_Types;
   use System;

   pragma Optimize (Off);

   -------------------
   -- Save_Context --
   -------------------
   --
   --  Context stores the information required to modify the stack of a
   --  preempted task with 'Change_Return_Context_Of_Preempted_Task'.
   --
   --
   procedure Save_Context (Context : access Jmp_Context) is
      pragma Optimize (Off);

      function To_Save_Context_Stack_Ac is
        new Ada.Unchecked_Conversion (Unsigned_32, Save_Context_Stack_Ac);
   begin
      --  Store registers
      Asm ("pushl %%eax;" &
           "pushl %%ebp;" &
           "pushl %%edi;" &
           "pushl %%esi;" &
           "pushl %%edx;" &
           "pushl %%ecx;" &
           "pushl %%ebx;"  &
           "pushl $1f;",
           No_Output_Operands, No_Input_Operands, "", True);

      Asm ("popl %0 ;" &
        "popl %1 ;" &
        "popl %2 ;" &
        "popl %3 ;" &
        "popl %4 ;" &
        "popl %5 ;" &
        "popl %6 ;" &
        "popl %7 ;",
        (Address'Asm_Output ("=g", Context.Return_Address),
         Unsigned_32'Asm_Output ("=g", Context.Ebx),
         Unsigned_32'Asm_Output ("=g", Context.Ecx),
         Unsigned_32'Asm_Output ("=g", Context.Edx),
         Unsigned_32'Asm_Output ("=g", Context.Esi),
         Unsigned_32'Asm_Output ("=g", Context.Edi),
         Unsigned_32'Asm_Output ("=g", Context.Ebp),
         Unsigned_32'Asm_Output ("=g", Context.Eax)),
        No_Input_Operands, "", True);

      --  Mark context has been used in a direct invocation of 'Save_Context'
      pragma Validity_Checks (Off);
      Context.After_Jmp := 0;
      pragma Validity_Checks (On);

      --  Store esp
      Asm ("movl %%esp, %0;",
           Unsigned_32'Asm_Output ("=g", Context.Esp),
           No_Input_Operands, "", True);

      --  Save stack
      pragma Validity_Checks (Off);
      Context.Context_Stack := To_Save_Context_Stack_Ac (Context.Esp).all;
      pragma Validity_Checks (On);

      --  Push ebp to be restored after label "1:"
      Asm ("pushl %%ebp;",
           No_Output_Operands, No_Input_Operands, "", True);

      --  return address where the task returns after it is scheduled again. It
      --  jumps here from the 'ret' instruction in
      --  'Processor_Registers.Context_Switch'.
      Asm ("1: popl %%ebp;",
           No_Output_Operands,
           No_Input_Operands, "", True);

      --  Enable interrupts. In a standard context switch, interrupts
      --  are re-enabled when Kernel.Leave_Critic_Section is
      --  called. In this case they must be re-enabled here. This is a
      --  not the perfect solution since the task could have disabled
      --  interrupts for any reason before calling this function, but
      --  it would be a very strange situation.
      MaRTE.HAL.Enable_Interrupts;
   end Save_Context;

   ------------------
   --  After_Jump  --
   ------------------
   --
   --  To be invoked after 'Save_Context'. If invoked after a direct invocation
   --  to 'Save_Context', 'After_Jump' shall return False. If invoked after
   --  returning from 'Save_Context' due to a call to 'Restore_Context',
   --  'After_Jump' shall return True.
   function After_Jump (Context : access Jmp_Context) return Unsigned_32 is
   begin
      return Context.After_Jmp;
   end After_Jump;

   ---------------------
   -- Restore_Context --
   ---------------------
   --
   --  This procedure changes the return context of a preempted task.
   --
   --                    |        |               |       |
   --  TCB_Ac.Stack_Ptr->|  Regs  |               |       |
   --                    |        |               |       |
   --                       ...                      ...
   --                    |        |               |       |
   --                    |        |               |Context|<-TCB_Ac.Stack_Ptr
   --                    |        |<-Context.Esp->|       |
   --                    |        |               |       |
   --                    |        |               |       |
   --                    |        |               |       |
   --                      before                   after
   --  The next time the task is scheduled will execute the final part of
   --  Save_Context.
   procedure Restore_Context (TCB_Ac  : System.Address;
                              Context : access Jmp_Context) is

      --  TCB_Ac points to the TCB of preempted task (a Kernel.TCB). The first
      --  8 bytes are the tag and the top of the stack of preempted task.
      type Beginning_Of_TCB is record
         Tag : Unsigned_32;
         Top_Of_Stack : Preempted_Task_Restore_Context_Stack_Ac;
      end record;
      type Beginning_Of_TCB_Ac is access Beginning_Of_TCB;

      function UC  is
        new Ada.Unchecked_Conversion (System.Address, Beginning_Of_TCB_Ac);
      function UC  is
        new Ada.Unchecked_Conversion (Unsigned_32,
                                      Preempted_Task_Restore_Context_Stack_Ac);
   begin

      --  Set top of stack of the task to its new value
      UC (TCB_Ac).Top_Of_Stack :=
        UC (Context.Esp - Context.Return_Address'Size / 8
           - Context.Ebp'Size / 8);

      --  Create new return context
      UC (TCB_Ac).Top_Of_Stack.all :=
        (Return_Address => Context.Return_Address,
         Ebp            => Context.Ebp,
         Context_Stack  => Context.Context_Stack);

      --  The context has been used to perform a non-local jump
      Context.After_Jmp := 1;
   end Restore_Context;

end Non_Local_Jump;
