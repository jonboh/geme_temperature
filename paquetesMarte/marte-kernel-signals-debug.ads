------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . S i g n a l s . D e b u g'
--
--                                 Spec
--
--
--  File 'k-signals-debug.ads'                                         By Mar.
--
--
--  Signal package debugging.
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

package MaRTE.Kernel.Signals.Debug is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   ----------------------
   -- Pthread_Sig_Mask --
   ----------------------
   procedure Pthread_Sig_Mask (How : in Int);

   -----------------
   -- Sig_Action1 --
   -----------------
   procedure Sig_Action1 (Act : in Struct_Sig_Action_Ac_Const;
                          Sig : in Signal);

   --------------
   -- Sig_Wait --
   --------------
   procedure Sig_Wait (Set : in Signal_Set_Ac_Const);

   ------------------
   -- Pthread_Kill --
   ------------------
   procedure Pthread_Kill (T : in Task_Id; Sig : in Signal);

   ----------
   -- Kill --
   ----------
   procedure Kill (Sig : in Signal);

   ---------------------
   -- Signal_Catching --
   ---------------------
   procedure Signal_Catching1 (T : in Task_Id; Sig : in Signal);

   ----------------------------------------------
   -- Add_Self_To_The_Waiting_For_Signals_List --
   ----------------------------------------------
   procedure Add_Self_To_The_Waiting_For_Signals_List;

end MaRTE.Kernel.Signals.Debug;
