------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'S i n g l y _ L i n k e d _ L i s t s'
--
--                                  Body
--
--
--  File 'singly_linked_lists.ads'                                     by MAR.
--
--  Singly linked lists management.
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
package body MaRTE.SLL is

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (L : in out List) is
   begin
      L := null;
   end Initialize;

   ------------------
   -- Enqueue_Head --
   ------------------
   procedure Enqueue_Head (E : in Element_Ac;
                           L : in out List) is
   begin
      pragma Assert (E /= null, "Enqueue_Head: Null element");
      E.Next := To_Element_Ac (L);
      L := To_List (E);
   end Enqueue_Head;

   ------------------
   -- Enqueue_Tail --
   ------------------
   procedure Enqueue_Tail (E : in Element_Ac;
                           L : in out List) is
      P : Element_Ac := To_Element_Ac (L);
   begin
      pragma Assert (E /= null, "Enqueue_Tail: Null element");
      if L = null then
         --  Empty List
         L := To_List (E);
         E.Next := null;
      else
         loop
            if P.Next = null then
               P.Next := E;
               E.Next := null;
               return;
            end if;
            P := P.Next;
         end loop;
      end if;
   end Enqueue_Tail;

   ------------------
   -- Dequeue_Head --
   ------------------
   procedure Dequeue_Head (L : in out List) is
   begin
      pragma Assert (L /= null, "Dequeue_Head: Empty list");
      L := To_List (L.Next);
   end Dequeue_Head;

   ----------
   -- Head --
   ----------
   function Head (L : in List) return Element_Ac is
   begin
      return To_Element_Ac (L);
   end Head;

   -------------
   -- Dequeue --
   -------------
   procedure Dequeue (E : in Element_Ac;
                      L : in out List) is
      P : Element_Ac := To_Element_Ac (L);
   begin
      pragma Assert (E /= null, "Dequeue: Null element");
      if L = null then
         --  Empty List
         return;
      end if;
      if L = To_List (E) then
         --  It is in the first position
         L := To_List (E.Next);
      else
         loop
            if P.Next = null then
               return; -- Not found
            elsif P.Next = E then
               P.Next := E.Next;
               return; -- Found
            end if;
            P := P.Next;
         end loop;
      end if;
   end Dequeue;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (L : in List) return Boolean is
   begin
      return L = null;
   end Is_Empty;

   ----------
   -- Next --
   ----------
   function Next (E : in Element_Ac) return Element_Ac is
   begin
      pragma Assert (E /= null, "Next: Null element");
      return E.Next;
   end Next;

   -----------------
   -- Is_The_Head --
   -----------------
   function Is_The_Head (E : in Element_Ac; L : in List) return Boolean is
   begin
      return To_Element_Ac (L) = E and then E /= null;
   end Is_The_Head;

   -----------------
   -- Is_The_Tail --
   -----------------
   function Is_The_Tail (E : in Element_Ac) return Boolean is
   begin
      pragma Assert (E /= null, "Is_The_Tail: Null element");
      return E.Next = null;
   end Is_The_Tail;

   --------------------
   -- Is_In_The_List --
   --------------------
   function Is_In_The_List (E : in Element_Ac;
                            L : in List) return Boolean is
      P : Element_Ac := To_Element_Ac (L);
   begin
      pragma Assert (E /= null, "Is_In_The_list: Null element");
      loop
         exit when P = null;
         if P = E then
            return True; -- Found
         end if;
         P := P.Next;
      end loop;
      return False; -- Not found
   end Is_In_The_List;

end MaRTE.SLL;
