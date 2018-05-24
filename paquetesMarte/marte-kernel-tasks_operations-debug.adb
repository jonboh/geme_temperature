------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--        'K e r n e l . T a s k s _ O p e r a t i o n s . D e b u g'
--
--                                 Body
--
--
--  File 'k-to-debug.adb'                                              By MAR.
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
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.HAL;
with MaRTE.Kernel.Debug;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Spy;

package body MaRTE.Kernel.Tasks_Operations.Debug is

   package DBG  renames K.Debug;
   -------------
   -- Suspend --
   -------------
   procedure Suspend1 (T : in Task_Id; Until_Time : in HAL.HWTime) is
   begin
      if Tasks_Operations_Debug_Messages then
         Put ("  |  ("); Put (Integer (T.Id));
         Put (") SleepUntil:"); DBG.Show_In_Secs (Until_Time, DBG.ABSO);
      end if;
   end Suspend1;

   -----------------
   -- Create_Task --
   -----------------
   procedure Create_Task1 (Id : in Integer; Attr : in Pthread_Attr_T) is
   begin
      MaRTE.Spy.Send_Event (HAL.Get_HWTime,
                            "NT", Id, Attr.Param.Sched_Priority);
      if Tasks_Operations_Debug_Messages then
         Put ("  |  NT("); Put (Id);
         Put (",");        Put (Integer (Attr.Param.Sched_Priority));
         Put (",SP:");     Put (Integer (Attr.Sched_Policy));
         Put (")");
      end if;
   end Create_Task1;

   --------------------------
   -- Initialize_Main_Task --
   --------------------------
   procedure Initialize_Main_Task1 (Prio : in Task_Priority;
                                    Sched_Policy : in Scheduling_Policies) is
   begin
      if Tasks_Operations_Debug_Messages then
         Put ("  |  NEW_Main(0,"); Put (Integer (Prio));
         Put (", SP:");            Put (Integer (Sched_Policy));
         Put (")");
      end if;
   end Initialize_Main_Task1;

   --------------------------
   -- Initialize_Idle_Task --
   --------------------------
   procedure Initialize_Idle_Task1 (Id : in Integer) is
   begin
      if Tasks_Operations_Debug_Messages then
         Put ("  |  NEW_Idle("); Put (Id);
         Put (")");
      end if;
   end Initialize_Idle_Task1;

   -----------------
   -- Remove_Task --
   -----------------
   procedure Remove_Task1 (T : in Task_Id) is
   begin
      if Tasks_Operations_Debug_Messages then
         Put ("  |  RM("); Put (T.Id);
         Put (","); Put (Integer (T.Active_Prio));
         Put (")");
      end if;
   end Remove_Task1;

end MaRTE.Kernel.Tasks_Operations.Debug;
