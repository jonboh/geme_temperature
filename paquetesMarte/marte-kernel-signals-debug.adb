------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . S i g n a l s . D e b u g'
--
--                                 Body
--
--
--  File 'k-signals-debug.adb'                                         By Mar.
--
--
--  Signals debugging package.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Signals.Global;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;

package body MaRTE.Kernel.Signals.Debug is

   package SCHD renames K.Scheduler;

   --------------
   -- Show_Set --
   --------------
   procedure Show_Set (Set : in Signal_Set);
   procedure Show_Set (Set : in Signal_Set) is
   begin
      if Signals_Debug_Messages then
         Put ("/");
         for S in Signal_Set_Range'Range loop
            if Set (S) then
               Put (" "); Put (Integer (S));
            end if;
         end loop;
         Put ("/");
      end if;
   end Show_Set;

   ----------------------
   -- Pthread_Sig_Mask --
   ----------------------
   procedure Pthread_Sig_Mask (How : in Int) is
   begin
      if Signals_Debug_Messages then
         Put ("  |  ("); Put (Integer (SCHD.Self.Id));
         --       Put (","); Show_Set (Self_Sig_Data_Ac.Mask);
         Put (") changes mask: How:"); Put (Integer (How));
         --       if Set /= null then
         --          Put (" Set:"); Show_Set (Set.all);
         --       else
         --          Put (" Set:null");
         --       end if;
      end if;
   end Pthread_Sig_Mask;

   -----------------
   -- Sig_Action1 --
   -----------------
   procedure Sig_Action1 (Act : in Struct_Sig_Action_Ac_Const;
                          Sig : in Signal) is
   begin
      if Signals_Debug_Messages then
         Put ("  |  Assigning Act.Sa_Handler:"); Put (Act.Sa_Handler);
         Put (" to Sig:"); Put (Integer (Sig));
      end if;
   end Sig_Action1;

   --------------
   -- Sig_Wait --
   --------------
   procedure Sig_Wait (Set : in Signal_Set_Ac_Const) is
   begin
      if Signals_Debug_Messages then
         Put ("  |  ("); Put (Integer (SCHD.Self.Id));
         Put (") wait for sigs:");
         Show_Set (Set.all);
      end if;
   end Sig_Wait;

   ------------------
   -- Pthread_Kill --
   ------------------
   procedure Pthread_Kill (T : in Task_Id; Sig : in Signal) is
   begin
      if Signals_Debug_Messages then
         Put (" |  ("); Put (Integer (SCHD.Self.Id));
         Put (") sends Sig:"); Put (Integer (Sig));
         Put (" to ("); Put (Integer (T.Id));
         Put (")");
      end if;
   end Pthread_Kill;

   ----------
   -- Kill --
   ----------
   procedure Kill (Sig : in Signal) is
   begin
      if Signals_Debug_Messages then
         Put ("  |  ("); Put (Integer (SCHD.Self.Id));
         Put (") sends Sig:"); Put (Integer (Sig));
         Put (" to process");
      end if;
   end Kill;

   ---------------------
   -- Signal_Catching --
   ---------------------
   procedure Signal_Catching1 (T : in Task_Id; Sig : in Signal) is
   begin
      if Signals_Debug_Messages then
         Put ("  |  Catched Sig:"); Put (Integer (Sig));
         Put (" Delivered_To: ("); Put (Integer (T.Id));
         Put (")");
      end if;
   end Signal_Catching1;

   ----------------------------------------------
   -- Add_Self_To_The_Waiting_For_Signals_List --
   ----------------------------------------------
   procedure Add_Self_To_The_Waiting_For_Signals_List is
      use K.Signals.Global;
   begin
      if Signals_Debug_Messages then
         Put ("  |  ("); Put (Integer (SCHD.Self.Id));
         Put (")WaitSig");
         Show_Set (Sig_Data_Ac (SCHD.Self.Sig_Data).Waited_Signals);
      end if;
   end Add_Self_To_The_Waiting_For_Signals_List;

end MaRTE.Kernel.Signals.Debug;
