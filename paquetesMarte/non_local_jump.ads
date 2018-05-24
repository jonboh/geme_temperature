------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                      'N o n _ L o c a l _ J u m p'
--
--                                  Spec
--
--  File 'non_local_jump.ads'                                          By MAR
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
with System;
with MaRTE.Integer_Types;

package Non_Local_Jump is
   pragma Optimize (Off);

   type Jmp_Context is private;

   -------------------
   -- Save_Context --
   -------------------
   --
   --  Context stores the information required to modify the stack of a
   --  preempted task with 'Change_Return_Context_Of_Preempted_Task'.
   --
   --  This function stores in 'Context' the registers and its stack
   --  frame. This information can be used for 'Restore_Context' to
   --  change the stack of the task (when it is preempted) so that
   --  when it is scheduled again it returns to the end of this
   --  function
   --
   procedure Save_Context (Context : access Jmp_Context);
   pragma Export (C, Save_Context, "marte_nonlocaljmp_savecontext");

   ------------------
   --  After_Jump  --
   ------------------
   --
   --  To be invoked after 'Save_Context'. If invoked after a direct invocation
   --  to 'Save_Context', 'After_Jump' shall return 0 (False). If invoked after
   --  returning from 'Save_Context' due to a call to 'Restore_Context',
   --  'After_Jump' shall return 1 (True).
   function After_Jump (Context : access Jmp_Context)
                        return MaRTE.Integer_Types.Unsigned_32;
   pragma Export (C, After_Jump, "marte_nonlocaljmp_afterjmp");

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
   --                    |        |               |  ret  |<-TCB_Ac.Stack_Ptr
   --                    |        |               |  ebp  |
   --                    |        |<-Context.Esp->|  SC*  |
   --                    |        |               |  SC*  |
   --                    |        |               |  SC*  |
   --                    |        |               |       |
   --                      before                   after
   --  SC*: stack frame of 'Save_Context'.
   --
   --  The next time the task is scheduled will execute the final part of
   --  Save_Context.
   --
   --  The full process is the following: 'Save_Context' stores the
   --  registers and its stack frame in
   --  'Jmp_Context'. 'Restore_Context' restores that context placing
   --  the stored frame in the same position of the stack it was
   --  originally. Over this frame it is placed the value of the ebp
   --  register and the address of label "1:" in 'Save_Context'.  When
   --  the task is scheduled again, the first instruction it executes
   --  is "ret" in 'Processor_Registers.Context_Switch' and then the
   --  address of label "1:" in 'Save_Context' is popped from the
   --  stack. Once in 'Save_Context', the ebp register is also popped
   --  and, with this instruction, the registers and stack are in the
   --  same situation it was the time that the contexts was stored,
   --  and then the final part of 'Save_Context' can be executed
   --  successfully.
   procedure Restore_Context (TCB_Ac  : System.Address;
                              Context : access Jmp_Context);
   pragma Export (C, Restore_Context, "marte_nonlocaljmp_restorecontext");

private

   type Save_Context_Stack is array (1 .. 29) of  --  17->GPL2005, 29->GPL2007
     MaRTE.Integer_Types.Unsigned_32;
   type Save_Context_Stack_Ac is access Save_Context_Stack;
   --  Stack of 'Save_Context' funcion. The frame created by
   --  'Save_Context' is 16 integers long when no optimization is used
   --  for this function. The total size is 17 because ebp register is
   --  also stored in the stack


   --  A change in this record could affect the type
   --  'marte_nonlocaljmp_context_t' defined in
   --  'x86_arch/include/misc/marte_non_local_jmp.h'
   type Jmp_Context is record
      Return_Address     : System.Address;
      --  The address of label "1:" inside 'Save_Context'. The task jumps to
      --  this instruction from the 'ret' instruction in
      --  'Processor_Registers.Context_Switch'.

      Ebx                : MaRTE.Integer_Types.Unsigned_32;
      Ecx                : MaRTE.Integer_Types.Unsigned_32;
      Edx                : MaRTE.Integer_Types.Unsigned_32;
      Esi                : MaRTE.Integer_Types.Unsigned_32;
      Edi                : MaRTE.Integer_Types.Unsigned_32;
      Ebp                : MaRTE.Integer_Types.Unsigned_32;
      Eax                : MaRTE.Integer_Types.Unsigned_32;

      Esp                : MaRTE.Integer_Types.Unsigned_32;  -- top of stack

      After_Jmp          : MaRTE.Integer_Types.Unsigned_32;
      -- 0 (False) after a direct invocation to 'Save_Context'. 1 (True) if
      -- 'Restore_Context' has been invoked.

      Context_Stack  : Save_Context_Stack;
      --  Stack frame of 'Save_Context' funcion

   end record;
   pragma Volatile (Jmp_Context);
   type Jmp_Context_Ac is access Jmp_Context;

   type Preempted_Task_Restore_Context_Stack is record
      Return_Address : System.Address;
      Ebp            : MaRTE.Integer_Types.Unsigned_32;
      Context_Stack  : Save_Context_Stack;
   end record;
   type Preempted_Task_Restore_Context_Stack_Ac is
     access Preempted_Task_Restore_Context_Stack;

end Non_Local_Jump;
