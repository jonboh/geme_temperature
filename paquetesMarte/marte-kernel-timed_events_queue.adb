------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . T i m e d _ E v e n t s _ Q u e u e'
--
--                                  Body
--
--
--  File 'k-timed_events_queue.adb'                                    By MAR.
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
with MaRTE.HAL; use MaRTE.HAL;
pragma Elaborate_All (MaRTE.HAL);
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Configuration_Parameters;

--  Debug
with MaRTE.Direct_IO;
with MaRTE.Debug_Messages;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Timed_Events_Queue is

   use K.Timed_Events_Lists;

   package CP renames MaRTE.Configuration_Parameters;

   procedure Show_TEQ_On_Console (Msg : in String);

   Initialized : Boolean := False;

   --------------------------
   -- Initial Events Queue --
   --------------------------
   --
   --  Only used when 'CP.Preallocated_Resources' is "True"
   type Initial_Events_Queue is
     array (0 .. CP.Num_Timed_Events_Mx) of Timed_Event_Ac;
   pragma Pack (Initial_Events_Queue);
   type Initial_Events_Queue_Ac is access all Initial_Events_Queue;
   Initial_Q : aliased Initial_Events_Queue;

   ------------------
   -- Events Queue --
   ------------------
   subtype Q_Size_T is Integer range 0 .. Integer'Last;
   type Generic_Events_Queue is array (Q_Size_T) of Timed_Event_Ac;
   type Generic_Events_Queue_Ac is access all Generic_Events_Queue;
   pragma Pack (Generic_Events_Queue);

   Q         : Generic_Events_Queue_Ac;
   Q_Mx_Size : Q_Size_T;  --  Queue maximum size
   Q_Size    : Q_Size_T;  --  Queue current size

   -------------------------
   -- Auxiliary functions --
   -------------------------
   function Malloc (Size : MaRTE.Integer_Types.Size_T)
                   return Generic_Events_Queue_Ac;
   pragma Import (C, Malloc, "malloc");
   procedure Free (E : Generic_Events_Queue_Ac);
   pragma Import (C, Free, "free");
   function UC is new Ada.Unchecked_Conversion (Initial_Events_Queue_Ac,
                                                Generic_Events_Queue_Ac);

   ------------
   -- UpHeap --
   ------------
   procedure UpHeap (N : in Index);
   procedure UpHeap (N : in Index) is
      P : Index;
      K : Index := N;
      Temp : Timed_Event_Ac;
   begin
      pragma Assert (N > 0 and N <= Q_Mx_Size);
      while K >= 2 loop
         P := K / 2;
         if Q (K).T < Q (P).T then
            Temp := Q (K);
            Q (K) := Q (P);
            Q (P) := Temp;
            K := P;
         else
            exit;
         end if;
      end loop;
   end UpHeap;
   pragma Inline (UpHeap);

   ----------------------
   -- Enqueue_In_Order --
   ----------------------
   procedure Enqueue_In_Order (E : in Timed_Event_Ac) is
   begin
      pragma Assert (Initialized);

      if Q_Size >= Q_Mx_Size then
         if CP.Preallocated_Resources'First then
            MaRTE.Direct_IO.Error ("Reached max. number of timed events",
                                   Fatal => True);
         else
            --  Create a larger list
            declare
               New_Q : Generic_Events_Queue_Ac :=
                 Malloc (Size_T (Q_Mx_Size * 2 * Timed_Event_Ac'Size));
            begin
               if New_Q = null then
                  MaRTE.Direct_IO.Error
                    ("timed events queue: mem pool exhausted",
                     Fatal => True);
               end if;
               for I in 0 .. Q_Mx_Size loop
                  New_Q (I) := Q (I);
               end loop;
               Q_Mx_Size := Q_Mx_Size * 2;
               Free (Q);
               Q := New_Q;
            end;
         end if;
      end if;
      Q_Size := Q_Size + 1;
      Q (Q_Size) := E;
      UpHeap (Q_Size);
      pragma Debug (Show_TEQ_On_Console ("Enq. Event EQ:"));
   end Enqueue_In_Order;

   --------------
   -- DownHeap --
   --------------
   procedure DownHeap (K : in Index; N : in Q_Size_T);
   procedure DownHeap (K : in Index; N : in Q_Size_T) is
      J : Integer := 2 * K;
      Half : Index := K;
      Temp : Timed_Event_Ac := Q (K);
   begin
      pragma Assert (K > 0 and K <= Q_Mx_Size);
      pragma Assert (N >= 0 and N <= Q_Mx_Size);
      while J <= N loop
         if (J < N) and then (Q (J + 1).T < Q (J).T) then
            J := J + 1;
         end if;
         if not (Q (J).T < Temp.T) then
            Q (Half) := Temp;
            return;
         end if;
         Q (Half) := Q (J);
         Half := J;
         J := 2 * J;
      end loop;
      Q (Half) := Temp;
   end DownHeap;
   pragma Inline (DownHeap);

   -----------------
   -- Remove_Head --
   -----------------
   function Remove_Head return Timed_Event_Ac is
      E : Timed_Event_Ac;
   begin
      pragma Assert (Initialized);
      if Q_Size = 0 then
         return null;
      end if;
      E := Q (1);
      Q (1) := Q (Q_Size);
      Q_Size := Q_Size - 1;
      DownHeap (1, Q_Size);
      pragma Debug (Show_TEQ_On_Console ("RM head EQ:"));
      return E;
   end Remove_Head;

   ------------
   -- Remove --
   ------------
   procedure Remove (E : in Timed_Event_Ac) is
      I : Integer := 0;
   begin
      pragma Assert (Initialized);
      while I < Q_Size loop
         I := I + 1;
         if E = Q (I) then
            Q (I) := Q (Q_Size);
            Q_Size := Q_Size - 1;
            DownHeap (I, Q_Size);
            pragma Debug (Show_TEQ_On_Console ("RM EQ:"));
            return;
         end if;
      end loop;
      pragma Assert (False, "Remove: event is not in The queue");
   end Remove;

   ----------
   -- Head --
   ----------
   function Head return Timed_Event_Ac is

   begin
      pragma Assert (Initialized);
      if Q_Size = 0 then
         return null;
      else
         return Q (1);
      end if;
   end Head;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty return Boolean is

   begin
      pragma Assert (Initialized);
      return Q_Size = 0;
   end Is_Empty;

   ------------------------
   -- Timed_Event_In_Pos --
   ------------------------
   --
   --  This function can be used to get all the elements in the queue just
   --  incrementing 'Pos' form 1. The last element in the queue is detected
   --  because either 'Pos' reach Intex'Last or 'null' is returned. The
   --  order in which the elements are got when varying 'Pos' form 1 to
   --  Index'Last is not preorder, nor inorder, nor postorder.
   function Timed_Event_In_Pos (Pos : in Index) return Timed_Event_Ac is
   begin
      pragma Assert (Initialized);
      if Pos > Q_Size then
         return null;
      else
         return Q (Pos);
      end if;
   end Timed_Event_In_Pos;
   pragma Inline (Timed_Event_In_Pos);

   -------------------------
   -- Show_TEQ_On_Console --
   -------------------------
   --
   --  Used when debugging
   procedure Show_TEQ_On_Console (Msg : in String) is
      use MaRTE.Direct_IO;
   begin
      if MaRTE.Debug_Messages.Timed_Events_Queue_Debug_Messages then
         pragma Assert (Initialized);
         Put ("  |  ");
         Put (Msg);
         if Q_Size = 0 then
            Put ("empty");
         else
            for I in 1 .. Q_Size loop
               Put ("["); K.Debug.Show_In_Secs (Q (I).T); Put ("]");
            end loop;
         end if;
      end if;
   end Show_TEQ_On_Console;

   ------------------
   --  Initialize  --
   ------------------
   --
   --  Initialize Events Queue
   procedure Initialize is
      use type MaRTE.Integer_Types.Size_T;
   begin
      pragma Assert (not Initialized);
      Q_Size := 0;
      if CP.Preallocated_Resources'First then
         Q := UC (Initial_Q'Access);
         Q_Mx_Size := Initial_Q'Last;
      else  --  Not preallocated resources
         Q := Malloc (CP.Num_Timed_Events_Initial * Timed_Event_Ac'Size);
         Q_Mx_Size := CP.Num_Timed_Events_Initial;
      end if;
      for I in 1 .. Q_Mx_Size loop
         Q (I) := null;
      end loop;

      Initialized := True;
   end Initialize;

end MaRTE.Kernel.Timed_Events_Queue;
