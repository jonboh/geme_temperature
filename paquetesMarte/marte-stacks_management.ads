------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                   'S t a c k s _ M a n a g e m e n t'
--
--                                 Spec
--
--
--  File 'stacks_management.ads'                                      By MAR.
--
--
--  Task stacks management.
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
with System; --  for 'Address'
with MaRTE.Integer_Types;
with MaRTE.Configuration_Parameters;

with MaRTE.SLL;
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

package MaRTE.Stacks_Management is

   type Stack_Id is private;

   Null_Stack_Id : constant Stack_Id;

   --  Stack positions are 'Positive' numbers, been P=1 the lower
   --  position of the stack (i.e. its base).

   type Stack_Size is new Positive;
   function Request_Stack (Size : Stack_Size) return Stack_Id;
   function Get_Main_Task_Stack return Stack_Id;

   procedure Release_Stack (S : Stack_Id);

   function Get_Stack_Base_Address (S : Stack_Id)
                                    return MaRTE.Integer_Types.Unsigned_32;

   function Get_Stack_Top_Address (S : Stack_Id)
                                  return MaRTE.Integer_Types.Unsigned_32;

   function DWord_In_Stack_Address (P : Positive;
                                    S : Stack_Id)
                                    return MaRTE.Integer_Types.Unsigned_32;

   procedure Write_In_Stack (Addr : System.Address;
                             P    : Positive;
                             S    : Stack_Id);

   procedure Write_In_Stack (Int : MaRTE.Integer_Types.Unsigned_32;
                             P   : Positive;
                             S   : Stack_Id);

   function Stack_OK (Task_Stack_Top : MaRTE.Integer_Types.Unsigned_32;
                      S              : Stack_Id) return Boolean;

   procedure Configure_Main_Task_Stack;

   function Get_Stack_Size_In_Bytes (S : Stack_Id)
                                     return Stack_Size;

   -----------------------
   --  Initialize_Pool  --
   -----------------------
   procedure Initialize_Pool;

private

   --  Stack magic number. Written as a array of chars for easier
   --  debugging.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   STACK_FREE : constant Magic_Check := ('s', 't', 'k', 'F');
   STACK_USED : constant Magic_Check := ('S', 'T', 'K', 'U');

   -----------------------------------------
   --  Types 'Stack_Data' and 'Stack_Id'  --
   -----------------------------------------
   type Stack_Pos is new Positive;
   for Stack_Pos'Size use 32;

   type Stack_Memory_Area is array (Stack_Pos) of
     MaRTE.Integer_Types.Unsigned_32;
   pragma Pack (Stack_Memory_Area);
   type Stack_Memory_Area_Ac is access all Stack_Memory_Area;
   type Stack_Data_Base is tagged record
      Magic          : Magic_Check;
      Stack          : Stack_Memory_Area_Ac;
      Size_In_DWords : Stack_Pos;
      Id             : Natural;
   end record;
   package Stack_Data_Lists is new MaRTE.SLL (Stack_Data_Base);
   package Stack_Data_Pool_Management is
      new Stack_Data_Lists.Resources (Stack_Data_Lists.Element);
   subtype Stack_Data is Stack_Data_Lists.Element;

   type Stack_Id is access all Stack_Data;

   Null_Stack_Id : constant Stack_Id := null;

end MaRTE.Stacks_Management;
