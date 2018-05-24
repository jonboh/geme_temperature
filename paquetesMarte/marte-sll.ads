------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                  'S i n g l y _ L i n k e d _ L i s t s'
--
--                                  Spec
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

with Ada.Unchecked_Conversion;

generic
   type Basic is tagged private;
package MaRTE.SLL is

   pragma Elaborate_Body;

   type Element is new Basic with private;

   type Element_Ac is access all Element'Class;

   type List is private;

   Null_List : constant List;

   procedure Initialize (L : in out List);
   pragma Inline (Initialize);

   procedure Enqueue_Head (E : in Element_Ac;
                           L : in out List);
   pragma Inline (Enqueue_Head);

   procedure Enqueue_Tail (E : in Element_Ac;
                           L : in out List);
   pragma Inline (Enqueue_Tail);

   procedure Dequeue_Head (L : in out List);
   pragma Inline (Dequeue_Head);

   function Head (L : in List) return Element_Ac;
   pragma Inline (Head);

   procedure Dequeue (E : in Element_Ac;
                      L : in out List);
   pragma Inline (Dequeue);
   --  Does nothing if 'E' isn't in 'L'

   function Is_Empty (L : in List) return Boolean;
   pragma Inline (Is_Empty);

   function Next (E : in Element_Ac) return Element_Ac;
   pragma Inline (Next);

   function Is_The_Head (E : in Element_Ac; L : in List) return Boolean;
   pragma Inline (Is_The_Head);

   function Is_The_Tail (E : in Element_Ac) return Boolean;
   pragma Inline (Is_The_Tail);

   function Is_In_The_List (E : in Element_Ac;
                            L : in List) return Boolean;
   pragma Inline (Is_In_The_List);

private

   type List is access all Element'Class;

   Null_List : constant List := null;

   type Element is new Basic with record
      Freed : Boolean;
      Next : Element_Ac;
   end record;

   --  Unchecked Conversions in order to avoid innecesary and slow type
   --  conversions
   function To_Element_Ac is
     new Ada.Unchecked_Conversion (List, Element_Ac);
   function To_List is
     new Ada.Unchecked_Conversion (Element_Ac, List);
end MaRTE.SLL;
