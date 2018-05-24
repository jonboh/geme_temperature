------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'S i n g l y _ L i n k e d _ L i s t s . O r d e r'
--
--                                Body
--
--
--  File 'sll-order.adb'                                              by MAR.
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
package body MaRTE.SLL.Order is

   ----------------------
   -- Enqueue_In_Order --
   ----------------------
   procedure Enqueue_In_Order (E : in Element_Ac;
                               L : in out List) is
      P : Element_Ac := Element_Ac (L);
   begin
      if Element_Ac (L) = null or else E > Element_Ac (L) then
         E.Next := Element_Ac (L);
         L := List (E);
      else
         loop
            if (P.Next = null or else E > P.Next) then
               E.Next := P.Next;
               P.Next := E;
               exit;
            end if;
            P := P.Next;
         end loop;
      end if;
   end Enqueue_In_Order;

   -----------------------------
   -- Reenqueue_Head_In_Order --
   -----------------------------
   procedure Reenqueue_Head_In_Order (L : in out List) is
      P, Tmp : Element_Ac := Element_Ac (L);
   begin
      if (L /= null and then P.Next /= null and then
           not (P > P.Next)) then
         loop
            P := P.Next;
            exit when (P.Next = null or else
                       Element_Ac (L) > P.Next);
         end loop;
         L := List (Tmp.Next);
         Tmp.Next := P.Next;
         P := Tmp;
      end if;
   end Reenqueue_Head_In_Order;

   ------------------
   -- Find_Mx_Prio --
   ------------------
   function Find_Mx_Prio (L : in List) return Element_Ac is
      Mx_Prio : Element_Ac := Element_Ac (L);
      P : Element_Ac := Element_Ac (L);
   begin
      while (P /= null) loop
         if P > Mx_Prio then
            Mx_Prio := P;
         end if;
         P := P.Next;
      end loop;
      return Mx_Prio;
   end Find_Mx_Prio;

end MaRTE.SLL.Order;
