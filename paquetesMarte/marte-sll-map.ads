------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'S i n g l y _ L i n k e d _ L i s t s . M a p'
--
--                                Spec
--
--
--  File 'sll-map.ads'                                                by MAR.
--
--
--  Array of ordered singly linked lists, one for each priority level,
--  among with a map of bits that indicates if there is elements at any
--  given priority.
--
--  This lists also has an especial element (called 'Last_Element') what is
--  the one returned by 'Head' when all the priority queues are empty.
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
with MaRTE.Integer_Types;

generic
   type Priority is range <>;
   with function ">" (Left, Right : in Element_Ac) return Boolean;
   with function Get_Priority (E : in Element_Ac) return Priority;
package MaRTE.SLL.Map is

   type Map_List is private;

   procedure Initialize (ML : in out Map_List);

   procedure Set_Last_Element (E  : in Element_Ac;
                               ML : in out Map_List);

   procedure Enqueue_In_Order (E  : in Element_Ac;
                               ML : in out Map_List);
   pragma Inline (Enqueue_In_Order);
   --  Enqueue at the tail of its priority queue.

   procedure Enqueue_First_In_Priority (E  : in Element_Ac;
                                        ML : in out Map_List);
   pragma Inline (Enqueue_First_In_Priority);
   --  Enqueue at the head of its priority queue.
   procedure Dequeue_Head (E  : in Element_Ac;
                           ML : in out Map_List);
   pragma Inline (Dequeue_Head);

   procedure Dequeue (E  : in Element_Ac;
                      ML : in out Map_List);
   pragma Inline (Dequeue);

   procedure Dequeue_First_In_Priority (E  : in Element_Ac;
                                        ML : in out Map_List);
   pragma Inline (Dequeue_First_In_Priority);

   function Head (ML : in Map_List) return Element_Ac;
   pragma Inline (Head);
   --  Returns null if the list is empty

   procedure Reenqueue_Head_In_Order (E  : in Element_Ac;
                                      ML : in out Map_List);
   pragma Inline (Reenqueue_Head_In_Order);
   --  Use Only when the priority of the task hasn't been changed !!

   function Find_Mx_Priority (ML : in Map_List) return Priority;
   pragma Inline (Find_Mx_Priority);

   function Is_Empty (ML : in Map_List) return Boolean;
   pragma Inline (Is_Empty);
   --  Never returns TRUE if 'Set_Last_Element' has been called with 'E'
   --  different than null.

private
   type Priority_Lists is array (Priority) of List;

   subtype Map is MaRTE.Integer_Types.Unsigned_32;
   subtype Index_Map is Map range 0 .. Map (Priority'Last / 32);

   type Map_Prio_T is array (Index_Map) of Map;

   type Map_List is record
      Map_Prio : Map_Prio_T;
      PL       : Priority_Lists;
      Last     : Element_Ac;
   end record;
end MaRTE.SLL.Map;
