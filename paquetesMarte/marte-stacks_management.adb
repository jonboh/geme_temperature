------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'S t a c k s _ M a n a g e m e n t'
--
--                                 Body
--
--
--  File 'stacks_management.adb'                                      By MAR.
--
--
--  Task stacks management.
--
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
--
--  When using preallocated resources two arrays are created:
--  'Stack_Data_Pool' to store the data associated with each stack and
--  'Stack_Pool' to create the stacks themselves. Every stack in
--  'Stack_Pool' has the same size
--  ('Configuration_Parameters.Stack_Size_In_Bytes'):
--
--                                    Stack_Pool
--                               +-----------------+                       |
--  Stack_Data_Pool(1).Stack --> | MAGIC_LABEL_EOS | <-- Top of stack 1    | m
--                               | MAGIC_LABEL_EOS |                       | e
--                               |                 |                       | m
--                                      ...                                | .
--                               |                 |                       |
--                               |                 | <-- Base of stack 1   | a
--                               +-----------------+                       | d
--  Stack_Data_Pool(2).Stack --> | MAGIC_LABEL_EOS | <-- Top of stack 2    | d
--                               | MAGIC_LABEL_EOS |                       | r
--                                      ...                                | e
--                               |                 |                       | s
--                               |                 | <-- Base of stack 2   | s
--                               +-----------------+                       | e
--  Stack_Data_Pool(3).Stack --> | MAGIC_LABEL_EOS | <-- Top of stack 3    | s
--                               | MAGIC_LABEL_EOS |                       |
--                                      ...                               \|/
--
--  When NOT using preallocated resources, both the stack data and the
--  stack memory area are created in dynamic memory. In this case
--  every stack can have a different size:
--
--                                 +-----------------+
--          T1.Stack.Stack ------> | MAGIC_LABEL_EOS | <--- Top of stack of T1
--                                 | MAGIC_LABEL_EOS |
--                                 |                 |
--                                        ...
--                                 |                 |
--                                 |                 |
--                                 |                 |
--                                 |                 | <--- Base of stack of T1
--                                 +-----------------+
--
--                                 +-----------------+
--          T2.Stack.Stack ------> | MAGIC_LABEL_EOS | <--- Top of stack of T2
--                                 | MAGIC_LABEL_EOS |
--                                 |                 |
--                                        ...
--                                 |                 |
--                                 |                 | <--- Base of stack of T2
--                                 +-----------------+
--
--
--  Either using preallocated or non-preallocated resources the
--  structure of every stack is the following:
--
--        Array elemens           Stack             System.Address
--                           +==============+ <- Top and DWord "Size_In_DWords"
--             1             |    4 bytes   |
--                           +--------------+ <- DWord "Size_In_DWords" - 1
--             2             |    4 bytes   |
--                           +--------------+ <- DWord "Size_In_DWords" - 2
--                                 ...
--                           +--------------+ <- DWord 3
--  Stack.Size_In_DWords - 2 |    4 bytes   |
--                           +--------------+ <- DWord 2
--  Stack.Size_In_DWords - 1 |    4 bytes   |
--                           +--------------+ <- Base and DWord 1
--  Stack.Size_In_DWords     |    4 bytes   |
--                           +==============+
--
with System; -- for  'Address'
with Ada.Unchecked_Conversion;

with MaRTE.Configuration_Parameters; -- for 'Num_Tasks_Mx'

with MaRTE.Kernel.Debug;
--  with MaRTE.Direct_IO;

package body MaRTE.Stacks_Management is

   use MaRTE.Integer_Types;
   package CP  renames MaRTE.Configuration_Parameters;
   use type CP.Supported_Architectures;
   package DBG renames MaRTE.Kernel.Debug;

   Pool_Initialized : Boolean := False;

   Id_Count : Natural := 0;

   ---------------------------------
   -- Magic Label at end of stack --
   ---------------------------------
   --
   --  This "Magic" number is writen at the end of every stack. A "Stack
   --  Overflow" error can be detected when this number is overwriten (with
   --  a different value of course ;-)
   MAGIC_LABEL_EOS : constant := 16#60221e19#;
   --  Avogadro's number written with hexadecimal digits ;-)

   ------------------------------------
   --  Pool of 'Stack_Data' objects  --
   ------------------------------------
   Stack_Data_Pool : array (1 .. CP.Num_MaRTE_Tasks_Mx - 1)
     of aliased Stack_Data;
   --  Stack data for all the tasks but the main task (Only used when
   --  using preallocated resources)

   ------------------
   -- To_Stack_Pos --
   ------------------
   function To_Stack_Pos (P : in Positive;
                          S : in Stack_Id) return Stack_Pos;
   function To_Stack_Pos (P : in Positive;
                          S : in Stack_Id) return Stack_Pos is
   begin
      --  P=1                <=> base of stack <=> S.Stack (S.Size_In_DWords)
      --  P=S.Size_In_DWords <=> top os stack  <=> S.Stack (1)
      return (S.Size_In_DWords - Stack_Pos (P)) + 1;
   end To_Stack_Pos;

   --------------------
   -- Pool of stacks --
   --------------------
   --
   --  Stacks for all the tasks but the main task (Only used when
   --  using preallocated resources)
   type Stack_Fix_Size is
     array (1 .. CP.Stack_Size_In_Bytes / 4) of Unsigned_32;
   pragma Pack (Stack_Fix_Size);
   type Stack_Fix_Size_Ac is access all Stack_Fix_Size;
   Stack_Pool : array (Stack_Data_Pool'Range) of aliased Stack_Fix_Size;

   ---------------------
   -- Main_Task_Stack --
   ---------------------
   Main_Task_Stack : aliased Stack_Data;
   --  Initialized in 'Configure_Main_Task_Stack'

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------
   --  Alguna de estas sobra:
   function UC is new Ada.Unchecked_Conversion (System.Address, Unsigned_32);

   ------------------------
   --  Initialize_Stack  --
   ------------------------
   --  To be used internally in this package.
   procedure Initialize_Stack (S              : out Stack_Data;
                               Status         : in  Magic_Check;
                               Stack_Top      : in  Stack_Memory_Area_Ac;
                               Size_In_DWords : in  Stack_Pos);
   procedure Initialize_Stack (S              : out Stack_Data;
                               Status         : in  Magic_Check;
                               Stack_Top      : in Stack_Memory_Area_Ac;
                               Size_In_DWords : in Stack_Pos) is
      use type Stack_Data;
   begin
      Stack_Data_Base (S) := (Magic          => Status,
                              Stack          => Stack_Top,
                              Size_In_Dwords => Size_In_DWords,
                              Id             => Id_Count);
      Id_Count := Id_Count + 1;
      if S = Main_Task_Stack then
         --  For the main task's stack, it is only possible to know its
         --  limits in the X86 architecture.
         case CP.MaRTE_Architecture'First is
            when CP.ARCH_X86 =>
               S.Stack (Stack_Memory_Area'First)     := MAGIC_LABEL_EOS;
               S.Stack (Stack_Memory_Area'First + 1) := MAGIC_LABEL_EOS;
            when CP.ARCH_LINUX | CP.ARCH_LINUX_LIB =>
               null;
         end case;
      else
         S.Stack (Stack_Memory_Area'First)     := MAGIC_LABEL_EOS;
         S.Stack (Stack_Memory_Area'First + 1) := MAGIC_LABEL_EOS;
      end if;
   end Initialize_Stack;

   -------------------
   -- Request_Stack --
   -------------------
   function Request_Stack (Size : in Stack_Size) return Stack_Id is
      function Malloc (Size : Size_T) return Stack_Memory_Area_Ac;
      pragma Import (C, Malloc, "malloc");

      S : Stack_Id := Stack_Id (Stack_Data_Pool_Management.Request);
   begin
      pragma Debug (DBG.Assert (Pool_Initialized));
      pragma Debug (DBG.Assert (S.Magic /= STACK_USED));
      if S = null then
         return null;
      end if;
      if not CP.Preallocated_Resources'First then
         declare
            Size : Stack_Size := Request_Stack.Size;
         begin
            pragma Debug (DBG.Assert (Size > 100));  --  Is "Size" too small?
            if Size mod 4 /= 0 then
               --  Make size a multiple of 4
               Size := (Size - Size mod 4) + 4;
            end if;
            S.Stack := Malloc (Size_T (Size));
            if S.Stack = null then
               Stack_Data_Pool_Management.Release
                 (Stack_Data_Lists.Element_Ac (S));
               return null;
            end if;
            Initialize_Stack (S.all,
                              Status         => STACK_USED,
                              Stack_Top      => S.Stack,
                              Size_In_DWords => Stack_Pos (Size) / 4);
         end;
      else
         --  When using preallocated resources stacks are pre-initialized
         pragma Debug
           (DBG.Assert (S.Stack (Stack_Memory_Area'First) = MAGIC_LABEL_EOS and
                    S.Stack (Stack_Memory_Area'First + 1) = MAGIC_LABEL_EOS));
         pragma Debug
           (DBG.Assert (S.Size_In_DWords = CP.Stack_Size_In_Bytes / 4));
         pragma Debug
           (DBG.Assert (Unsigned_32 (S.Size_In_DWords - 1) * 4 =
                    Get_Stack_Base_Address (S) - Get_Stack_Top_Address (S)));
         pragma Debug (DBG.Assert (S.Magic = STACK_FREE));
         S.Magic := STACK_USED;
      end if;
      return S;
   end Request_Stack;

   -------------------------
   -- Get_Main_Task_Stack --
   -------------------------
   function Get_Main_Task_Stack return Stack_Id is
   begin
      return Main_Task_Stack'Access;
   end Get_Main_Task_Stack;

   -------------------
   -- Release_Stack --
   -------------------
   procedure Release_Stack (S : in Stack_Id) is
      procedure Free (E : Stack_Memory_Area_Ac);
      pragma Import (C, Free, "free");
   begin
      pragma Debug (DBG.Assert (Pool_Initialized));
      pragma Debug (DBG.Assert (S /= Main_Task_Stack'Access));
      pragma Debug (DBG.Assert (S.Magic = STACK_USED));
      if not CP.Preallocated_Resources'First then
         Free (S.Stack);
      end if;
      S.Magic := STACK_FREE;
      Stack_Data_Pool_Management.Release
        (Stack_Data_Lists.Element_Ac (S));
   end Release_Stack;

   ----------------------------
   -- Get_Stack_Base_Address --
   ----------------------------
   function Get_Stack_Base_Address (S : in Stack_Id) return Unsigned_32 is
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First = CP.ARCH_X86
                                or else S /= Main_Task_Stack'Access));
      return UC (S.Stack (S.Size_In_DWords)'Address);
   end Get_Stack_Base_Address;

   ---------------------------
   -- Get_Stack_Top_Address --
   ---------------------------
   function Get_Stack_Top_Address (S : in Stack_Id) return Unsigned_32 is
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First = CP.ARCH_X86
                                or else S /= Main_Task_Stack'Access));
      return UC (S.Stack (Stack_Memory_Area'First)'Address);
   end Get_Stack_Top_Address;

   ----------------------------
   -- DWord_In_Stack_Address --
   ----------------------------
   function DWord_In_Stack_Address (P : in Positive;
                                    S : in Stack_Id) return Unsigned_32 is
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First = CP.ARCH_X86
                                or else S /= Main_Task_Stack'Access));
      return UC (S.Stack (To_Stack_Pos (P, S))'Address);
   end DWord_In_Stack_Address;

   -------------------------------------
   -- Write_In_Stack (System.Address) --
   -------------------------------------
   procedure Write_In_Stack (Addr : in System.Address;
                             P    : in Positive;
                             S    : in Stack_Id) is
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First = CP.ARCH_X86
                                or else S /= Main_Task_Stack'Access));
      S.Stack (To_Stack_Pos (P, S)) := UC (Addr);
   end Write_In_Stack;

   ------------------------------
   -- Write_In_Stack (Integer) --
   ------------------------------
   procedure Write_In_Stack (Int : in Unsigned_32;
                             P   : in Positive;
                             S   : in Stack_Id) is
   begin
      pragma Debug (DBG.Assert (CP.MaRTE_Architecture'First = CP.ARCH_X86
                                or else S /= Main_Task_Stack'Access));
      S.Stack (To_Stack_Pos (P, S)) := Int;
   end Write_In_Stack;

   --------------
   -- Stack_OK --
   --------------
   function Stack_OK (Task_Stack_Top : in Unsigned_32;
                      S              : in Stack_Id) return Boolean is
   begin
      case CP.MaRTE_Architecture'First is
         when CP.ARCH_X86 =>
            null;

         when CP.ARCH_LINUX | CP.ARCH_LINUX_LIB =>
            if S = Main_Task_Stack'Access then
               --  For these architectures, the only check that can be
               --  preformed on the main task's stack is if it is used
               --  or not
               return S.Magic = STACK_USED;
            end if;
      end case;

      if S /= Main_Task_Stack'Access and CP.Preallocated_Resources'First then
         --  Debug assertions
         pragma Warnings (Off); -- to avoid warning when not using Preallocated
         --  Resources since, in that case, the Stack_Pool range is 0
         pragma Debug (DBG.Assert (Get_Stack_Top_Address (S) >=
                               UC (Stack_Pool (Stack_Pool'First)'Address)));
         pragma Debug (DBG.Assert (Get_Stack_Top_Address (S) <=
                               UC (Stack_Pool (Stack_Pool'Last)'Address)));
         pragma Debug (DBG.Assert (Get_Stack_Base_Address (S) >
                               UC (Stack_Pool (Stack_Pool'First)'Address)));
         null;
         pragma Warnings (On);
      end if;

      pragma Debug
        (DBG.Assert (S.Magic = STACK_USED));
      pragma Debug
        (DBG.Assert (S.Stack (Stack_Memory_Area'First) = MAGIC_LABEL_EOS));
      pragma Debug
        (DBG.Assert (S.Stack (Stack_Memory_Area'First + 1) = MAGIC_LABEL_EOS));
      pragma Debug
        (DBG.Assert (Task_Stack_Top >= Get_Stack_Top_Address (S)));
      pragma Debug
        (DBG.Assert (Task_Stack_Top <= Get_Stack_Base_Address (S)));
      pragma Debug
        (DBG.Assert (Unsigned_32 (S.Size_In_DWords - 1) * 4 =
                 Get_Stack_Base_Address (S) - Get_Stack_Top_Address (S)));

      return
        (S.Magic = STACK_USED                                    and
         S.Stack (Stack_Memory_Area'First)     = MAGIC_LABEL_EOS and
         S.Stack (Stack_Memory_Area'First + 1) = MAGIC_LABEL_EOS and
         Task_Stack_Top >= Get_Stack_Top_Address (S)             and
         Task_Stack_Top <= Get_Stack_Base_Address (S)            and
         Unsigned_32 (S.Size_In_DWords - 1) * 4 =
         Get_Stack_Base_Address (S) - Get_Stack_Top_Address (S));
   end Stack_OK;

   -------------------------------
   -- Configure_Main_Task_Stack --
   -------------------------------
   procedure Configure_Main_Task_Stack is
      function UC is new Ada.Unchecked_Conversion (Stack_Pos,
                                                   Stack_Memory_Area_Ac);
      function UC is new Ada.Unchecked_Conversion (System.Address, Stack_Pos);
      use type System.Address;
      Base : Stack_Pos;
      Top  : Stack_Pos;
   begin
      case CP.MaRTE_Architecture'First is
         when CP.ARCH_X86 =>
            Base := UC (CP.Base_Main_Task_Stack'Address);
            Top  := UC (CP.Top_Main_Task_Stack'Address);
            pragma Debug (DBG.Assert (Base > Top));
            pragma Debug
              (DBG.Assert (Base - Top = CP.Main_Stack_Size_In_Bytes));

         when CP.ARCH_LINUX | CP.ARCH_LINUX_LIB =>
            Base := 20*1024*4 + 1;  --  No-sense
            Top  := 1;              --          values (20Kb)
      end case;
      Initialize_Stack (Main_Task_Stack,
                        Status         => STACK_USED,
                        Stack_Top      => UC (Top),
                        Size_In_DWords => (Base - Top) / 4);
   end Configure_Main_Task_Stack;

   -----------------------------
   -- Get_Stack_Size_In_Bytes --
   -----------------------------
   function Get_Stack_Size_In_Bytes (S : in Stack_Id)
                                     return Stack_Size is
   begin
      return Stack_Size (S.Size_In_DWords) * 4;
   end Get_Stack_Size_In_Bytes;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize_Pool is
      function UC is
        new Ada.Unchecked_Conversion (Stack_Fix_Size_Ac, Stack_Memory_Area_Ac);
   begin
      pragma Debug (DBG.Assert (not Pool_Initialized));
      if CP.Preallocated_Resources'First then
         --  Stacks of rest of tasks
         for I in Stack_Data_Pool'Range loop
            Initialize_Stack
              (Stack_Data_Pool (I),
               Status         => STACK_FREE,
               Stack_Top      => UC (Stack_Pool (I)'Access),
               Size_In_DWords => Stack_Pos (Stack_Pool (I)'Last));

            Stack_Data_Pool_Management.Release (Stack_Data_Pool (I)'Access);
         end loop;
      end if;

      Pool_Initialized := True;
   end Initialize_Pool;

end MaRTE.Stacks_Management;
