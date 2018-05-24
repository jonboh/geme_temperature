------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'S i n g l y _ L i n k e d _ L i s t s . P r i o'
--
--                                 Spec
--
--
--  File 'sll-prio.ads'                                                by Mar.
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

generic
   type Priority is range <>;
   with function ">" (Left, Right : in Element_Ac) return Boolean;
   with function Get_Priority (E : in Element_Ac) return Priority;
package MaRTE.SLL.Prio is

   procedure Dequeue_Element_Mx_Prio (L : in out List; E : out Element_Ac);
   pragma Inline (Dequeue_Element_Mx_Prio);

   function Find_Element_Mx_Prio (L : in List) return Element_Ac;
   pragma Inline (Find_Element_Mx_Prio);

   function Find_Mx_Prio (L : in List) return Priority;
   pragma Inline (Find_Mx_Prio);

end MaRTE.SLL.Prio;
