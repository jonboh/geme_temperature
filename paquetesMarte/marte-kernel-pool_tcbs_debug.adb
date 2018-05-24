------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . P o o l _ T C B s _ D e b u g'
--
--                                 Body
--
--
--  File 'k-pool_tcbs_debug.adb'                                      By MAR.
--
--
--  Debug of Free Task Control Blocks management. Keep a list of all
--  TCBs used.
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
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.Kernel.Pool_TCBs;

package body MaRTE.Kernel.Pool_TCBs_Debug is

   Initialized : Boolean := False;

   ---------------------------------------------------------------------------
   --  Active TCBs List  -----------------------------------------------------
   ---------------------------------------------------------------------------
   Active_TCBs : K.Task_Containers_Lists.List :=
     K.Task_Containers_Lists.Null_List;

   Free_TCs : array (K.Pool_TCBs.TCBs_Range)
     of aliased K.Task_Containers_Lists.Element;
   package TCs_Pool_Mangement is new
     Task_Containers_Lists.Resources (K.Task_Containers_Lists.Element);

   ---------------------------
   --  Register_Active_TCB  --
   ---------------------------
   procedure Register_Active_TCB (T : in K.Task_Id) is
      use type K.Task_Container_Ac;
   begin
      pragma Assert (Initialized);
      if MaRTE.Debug_Messages.Register_Active_TCBs'First then
         declare
            TC : K.Task_Container_Ac := TCs_Pool_Mangement.Request;
         begin
            pragma Assert (TC /= null);
            K.Task_Containers_Lists.Enqueue_Head (TC, Active_TCBs);
         end;
      end if;
   end Register_Active_TCB;

   -------------------------
   --  Remove_Active_TCB  --
   -------------------------
   procedure Remove_Active_TCB (T : in K.Task_Id) is
      use type K.Task_Container_Ac;
      use type K.Task_Id;
   begin
      pragma Assert (Initialized);
      if MaRTE.Debug_Messages.Register_Active_TCBs'First then
         declare
            TC : K.Task_Container_Ac :=
              K.Task_Containers_Lists.Head (Active_TCBs);
         begin
            loop
               pragma Assert (TC /= null);
               exit when TC.T = T;
               TC := K.Task_Containers_Lists.Next (TC);
            end loop;
            K.Task_Containers_Lists.Dequeue (TC, Active_TCBs);
            Tcs_Pool_Mangement.Release (TC);
         end;
      end if;
   end Remove_Active_TCB;

   -----------------------------
   --  Show_All_Active_Tasks  --
   -----------------------------
   procedure Show_All_Active_Tasks is
      use type K.Task_Container_Ac;
   begin
      pragma Assert (Initialized);
      if MaRTE.Debug_Messages.Register_Active_TCBs'First then
        declare
           TC : K.Task_Container_Ac :=
             K.Task_Containers_Lists.Head (Active_TCBs);
        begin
           while TC /= null loop
              Put ("  |  ("); Put (TC.T.Id);
              Put (","); Put (Integer (TC.T.Active_Prio));
              Put (")");
              TC := K.Task_Containers_Lists.Next (TC);
           end loop;
        end;
      end if;
   end Show_All_Active_Tasks;

   -----------------------
   --  Initialize_Pool  --
   -----------------------
   procedure Initialize_Pool is
   begin
      pragma Assert (not Initialized);
      if MaRTE.Debug_Messages.Register_Active_TCBs'First then
         --  Initialize 'Free_TCs'
         for I in Free_TCs'Range loop
            TCs_Pool_Mangement.Release (Free_TCs (I)'Access);
         end loop;
      end if;

      Initialized := True;
   end Initialize_Pool;

end MaRTE.Kernel.Pool_TCBs_Debug;
