------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . S i g n a l s . P o o l _ S D B s'
--
--                                 Body
--
--
--  File 'k-signals-pool_sdbs.adb'                                     By Mar.
--
--
--  Free Signal Data Blocks (SDB) management.
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
with MaRTE.Configuration_Parameters;
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

with MaRTE.Kernel.Signals.Global;
use MaRTE.Kernel.Signals.Global;
with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Signals.Pool_SDBs is

   package DBG renames K.Debug;

   Initialized : Boolean := False;

   --------------------------------------------------------------------------
   -- Pool of Signal Data Blocks (SDB) --------------------------------------
   --------------------------------------------------------------------------
   use K.Signals.Global.Sig_Data_Lists;
   Free_SDBs : array (1 .. MaRTE.Configuration_Parameters.Num_User_Tasks_Mx)
     of aliased Sig_Data_Lists.Element;
   package SDBs_Mangement is
      new Signals.Global.Sig_Data_Lists.Resources (Sig_Data_Lists.Element);

   Sig_Data_Default_Value : constant Sig_Data :=
     (Mask                => Blockeable_Signals,
      Pending_Signals  => (others => Global.Signal_Instances_Lists.Null_List),
      Waited_Signals => (others => False),
      Is_Waiting_For_Signals => False,
      Accepted_Siginst_Ac => null,
      Owner               => null);

   -----------------
   -- Request_SDB --
   -----------------
   function Request_SDB return K.Signals.Global.Sig_Data_Ac is
      Ac : K.Signals.Global.Sig_Data_Ac := SDBs_Mangement.Request;
   begin
      pragma Debug (DBG.Assert (Initialized));
      if Ac /= null  then
         Sig_Data (Ac.all) := Sig_Data_Default_Value;
      end if;
      return Ac;
   end Request_SDB;

   -----------------
   -- Release_SDB --
   -----------------
   procedure Release_SDB (SDB : K.Signals.Global.Sig_Data_Ac) is
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (not Sig_Data_Lists.Is_In_The_List
                            (SDB, Sig_Data_Blocks_Used)));
      SDBs_Mangement.Release (SDB);
   end Release_SDB;

   -------------------------
   --  Register_Used_SDB  --
   -------------------------
   procedure Register_Used_SDB (T : in K.Task_Id) is
      use type K.Task_Id;
   begin
      pragma Debug (DBG.Assert (Initialized));
      pragma Debug (DBG.Assert (T.Sig_Data /= null        and then
                            Sig_Data_Ac (T.Sig_Data).Owner = T));
      pragma Debug (DBG.Assert (K.Task_OK (T, MaRTE.Kernel.NEW_APPSCHED)));
      pragma Debug (DBG.Assert (not Sig_Data_Lists.Is_In_The_List
                            (Sig_Data_Ac (T.Sig_Data),
                             Sig_Data_Blocks_Used)));
      Sig_Data_Lists.Enqueue_Head (Sig_Data_Ac (T.Sig_Data),
                                   Sig_Data_Blocks_Used);
   end Register_Used_SDB;

   -----------------------
   --  Initialize_Pool  --
   -----------------------
   procedure Initialize_Pool is
   begin
      pragma Debug (DBG.Assert (not Initialized));
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         --  Initialize 'Free_SDBs'
         for I in Free_SDBs'Range loop
            SDBs_Mangement.Release (Free_SDBs (I)'Access);
         end loop;
      end if;

      Initialized := True;
   end Initialize_Pool;

end MaRTE.Kernel.Signals.Pool_SDBs;
