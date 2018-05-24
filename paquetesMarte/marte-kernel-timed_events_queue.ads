------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . T i m e d _ E v e n t s _ Q u e u e'
--
--                                  Spec
--
--
--  File 'k-timed_events_queue.ads'                                    By MAR.
--
--
--  Timed Events Queue Management. This package keeps a ordered binary
--  tree of timed events. This tree is used by 'Kernel.Timed_Events' to
--  store all the timed events but the CPU time events (including the
--  round-robin and sporadic server ones).
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

package MaRTE.Kernel.Timed_Events_Queue is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   procedure Enqueue_In_Order (E : in Timed_Event_Ac);
   pragma Inline (Enqueue_In_Order);

   function Remove_Head return Timed_Event_Ac;
   pragma Inline (Remove_Head);

   procedure Remove (E : in Timed_Event_Ac);
   pragma Inline (Remove);

   function Head return Timed_Event_Ac;
   pragma Inline (Head);

   function Is_Empty return Boolean;
   pragma Inline (Is_Empty);

   subtype Index is Integer range 1 .. Integer'Last;

   function Timed_Event_In_Pos (Pos : in Index) return Timed_Event_Ac;
   pragma Inline (Timed_Event_In_Pos);
   --  This function can be used to get all the elements in the queue just
   --  incrementing 'Pos' form 1. The last element in the queue is detected
   --  because either 'Pos' reach Intex'Last or 'null' is returned. The
   --  order in which the elements are got when varying 'Pos' form 1 to
   --  Index'Last is not preorder, nor inorder, nor postorder.

   ------------------
   --  Initialize  --
   ------------------
   --
   --  Initialize Events Queue
   procedure Initialize;

end MaRTE.Kernel.Timed_Events_Queue;
