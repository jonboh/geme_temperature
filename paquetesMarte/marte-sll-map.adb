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
--  File 'sll-map.ads'                                             by Mar.
--
--
--  Array of ordered singly linked lists, one for each priority level,
--  among with a map of bits that indicates whether there is elements
--  at any given priority.
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

with MaRTE.HAL;
with MaRTE.Configuration_Parameters;

package body MaRTE.SLL.Map is

   package CP renames MaRTE.Configuration_Parameters;

   use MaRTE.Integer_Types;
   --  package HAL renames MaRTE.HAL;

   function To_Map is new Ada.Unchecked_Conversion (Priority, Map);

   ------------------------------
   -- Enqueue_In_Urgency_Order --
   ------------------------------

   --  Go from tail to head. Place E after the first P such as:
   --     - P >= E (if FIFO_Order is set)
   --     - P > E  (if FIFO_Order is NOT set)
   --  In other words: if FIFO_Order is NOT set, the newcomer element will
   --  overtake elements equals to it.
   --
   --  ??? O(n2)!!! Use a double linked to reduce the complexity to O(n)
   --  (better with pointer to the tail)

   procedure Enqueue_In_Urgency_Order (E : in Element_Ac;
                                       L : in out List;
                                       FIFO_Order : Boolean) is

      function Tail (L : List) return Element_Ac is
         P : Element_Ac := Element_Ac (L);
      begin
         if L = null then
            return null; --  Empty List
         else
            loop
               if P.Next = null then
                  return P;
               end if;
               P := P.Next;
            end loop;
         end if;
      end Tail;
      pragma Inline (Tail);

      function Previous (L : List; E : Element_Ac) return Element_Ac is
         P : Element_Ac := Element_Ac (L);
      begin
         if P = E then
            return null;  --  E is the head of the list
         end if;

         loop
            exit when P = null;  --  never should happen

            if P.Next = E then
               return P;
            end if;
            P := P.Next;
         end loop;

         pragma Assert (False);
         return null;  --  Never should be reached
      end Previous;
      pragma Inline (Previous);

      P : Element_Ac := Tail (L);
   begin
      loop
         if P = null then --  reached the head of the queue
            E.Next := Element_Ac (L);
            L := List (E);
            return;
         end if;

         if (FIFO_Order and not (E > P))
           or else (not FIFO_Order and (P > E))
         then  --  found the place in the queue: after P
            E.Next := P.Next;
            P.Next := E;
            return;
         end if;

         P := Previous (L, P);  --  One element back
      end loop;
   end Enqueue_In_Urgency_Order;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (ML : in out Map_List) is
   begin
      for I in Index_Map loop
         ML.Map_Prio (I) := 0;
      end loop;
      for I in Priority loop
         Initialize (ML.PL (I));
      end loop;
      ML.Last := null;
   end Initialize;

   ----------------------
   -- Set_Last_Element --
   ----------------------
   procedure Set_Last_Element (E  : in Element_Ac;
                               ML : in out Map_List) is
   begin
      ML.Last := E;
   end Set_Last_Element;

   ----------------------
   -- Enqueue_In_Order --
   ----------------------
   procedure Enqueue_In_Order (E  : in Element_Ac;
                               ML : in out Map_List) is
      P_Major, P_Minor : Map;
   begin
      if Is_Empty (ML.PL (Get_Priority (E))) then
         P_Major := Shift_Right (To_Map (Get_Priority (E)), 5);
         P_Minor := To_Map (Get_Priority (E)) and 31;
         HAL.Bit_Set (Bit_Field => ML.Map_Prio (P_Major),
                      Bit       => P_Minor);
         Enqueue_Head (E, ML.PL (Get_Priority (E)));
      else
         if not CP.Use_EDF_Scheduling_Policy'First
           and then not CP.Use_Application_Defined_Scheduling
         then
            Enqueue_Tail (E, ML.PL (Get_Priority (E)));
         else
            Enqueue_In_Urgency_Order (E, ML.PL (Get_Priority (E)),
                                      FIFO_Order => True);
         end if;
      end if;
   end Enqueue_In_Order;

   -------------------------------
   -- Enqueue_First_In_Priority --
   -------------------------------
   procedure Enqueue_First_In_Priority (E  : in Element_Ac;
                                        ML : in out Map_List) is
      P_Major, P_Minor : Map;
   begin
      if Is_Empty (ML.PL (Get_Priority (E))) then
         P_Major := Shift_Right (To_Map (Get_Priority (E)), 5);
         P_Minor := To_Map (Get_Priority (E)) and 31;
         HAL.Bit_Set (Bit_Field => ML.Map_Prio (P_Major),
                      Bit       => P_Minor);
      end if;

      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         Enqueue_Head (E, ML.PL (Get_Priority (E)));
      else
         Enqueue_In_Urgency_Order (E, ML.PL (Get_Priority (E)),
                                   FIFO_Order => False);
      end if;
   end Enqueue_First_In_Priority;

   -------------------------------
   -- Dequeue_First_In_Priority --
   -------------------------------
   procedure Dequeue_First_In_Priority (E  : in Element_Ac;
                                        ML : in out Map_List) is
      P_Major, P_Minor : Map;
   begin
      pragma Assert (Is_The_Head (E, ML.PL (Get_Priority (E))),
                     "Dequeue_First_In_Priority: non-head task");
      if Is_The_Tail (E) then
         --  'T' is the only active task in its priority
         P_Major := Shift_Right (Map (Get_Priority (E)), 5);
         P_Minor := Map (Get_Priority (E)) and 31;
         HAL.Bit_Reset (Bit_Field => ML.Map_Prio (P_Major),
                        Bit       => P_Minor);
      end if;
      Dequeue_Head (ML.PL (Get_Priority (E)));
   end Dequeue_First_In_Priority;

   ------------------
   -- Dequeue_Head --
   ------------------
   procedure Dequeue_Head (E  : in Element_Ac;
                           ML : in out Map_List)
     renames Dequeue_First_In_Priority;

   -------------
   -- Dequeue --
   -------------
   procedure Dequeue (E  : in Element_Ac;
                      ML : in out Map_List) is
      P_Major, P_Minor : Map;
   begin
      pragma Assert (Is_In_The_List (E, ML.PL (Get_Priority (E))),
                     "Dequeue: task is not in RQ");

      Dequeue (E, ML.PL (Get_Priority (E)));

      --  Was 'T' the last tasks in its priority ??
      if Is_Empty (ML.PL (Get_Priority (E))) then
         P_Major := Shift_Right (Map (Get_Priority (E)), 5);
         P_Minor := Map (Get_Priority (E)) and 31;
         HAL.Bit_Reset (Bit_Field => ML.Map_Prio (P_Major),
                        Bit       => P_Minor);
      end if;
   end Dequeue;

   ----------
   -- Head --
   ----------
   function Head (ML : in Map_List) return Element_Ac is
      P : Map;
   begin
      for I in reverse Index_Map loop
         if ML.Map_Prio (I) > 0 then
            HAL.Bit_Scan_Reverse (Bit_Field => ML.Map_Prio (I),
                                  Bit       => P);

            return Head (ML.PL (Priority (P + Shift_Left (Map (I), 5))));
         end if;
      end loop;
      return ML.Last;
   end Head;

   -----------------------------
   -- Reenqueue_Head_In_Order --
   -----------------------------
   --
   --  Use Only when the priority of the task hasn't been changed !!
   procedure Reenqueue_Head_In_Order (E  : in Element_Ac;
                                      ML : in out Map_List) is
   begin
      pragma Assert (Is_The_Head (E, ML.PL (Get_Priority (E))),
                     "Reenqueue_Head_In_Order: non-head task");
      --  If 'T' is the only task in the queue do nothing
      if not Is_The_Tail (E) then
         Dequeue_Head (ML.PL (Get_Priority (E)));
         if not CP.Use_EDF_Scheduling_Policy'First
           and then not CP.Use_Application_Defined_Scheduling
         then
            Enqueue_Tail (E, ML.PL (Get_Priority (E)));
         else
            Enqueue_In_Urgency_Order (E, ML.PL (Get_Priority (E)),
                                     FIFO_Order => True);
         end if;
      end if;
   end Reenqueue_Head_In_Order;

   ----------------------
   -- Find_Mx_Priority --
   ----------------------
   function Find_Mx_Priority (ML : in Map_List) return Priority is
      P : Map;
   begin
      for I in reverse Index_Map loop
         if ML.Map_Prio (I) > 0 then
            HAL.Bit_Scan_Reverse (Bit_Field => ML.Map_Prio (I),
                                  Bit       => P);

            return Get_Priority
              (Head (ML.PL (Priority (P + Shift_Left (Map (I), 5)))));
         end if;
      end loop;
      return Priority'First;
   end Find_Mx_Priority;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (ML : in Map_List) return Boolean is
   begin
      if ML.Last /= null then
         return False;
      end if;
      for I in Index_Map loop
         if ML.Map_Prio (I) > 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;

end MaRTE.SLL.Map;
