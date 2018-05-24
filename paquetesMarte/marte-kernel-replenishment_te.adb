------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . R e p l e n i s h m e n t _ T E'
--
--                                   Body
--
--
--
--  File 'k-replenishment_te.adb'                                      By MAR.
--
--  Replenishment timed events management. This events are used by the
--  Sporadic Server Policy tasks to schedule a replenishment operation.
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

with MaRTE.Configuration_Parameters;
with MaRTE.SLL.Resources;
pragma Elaborate_All (MaRTE.SLL.Resources);

package body MaRTE.Kernel.Replenishment_TE is

   package CP renames MaRTE.Configuration_Parameters;

   ----------------------------------------------------------------------------
   -- Application scheduling events pool --------------------------------------
   ----------------------------------------------------------------------------

   package Replenishment_Timed_Events_Resources is
      new Replenishment_Timed_Events_Lists.Resources
     (Replenishment_Timed_Events_Lists.Element);

   Free_Replenishment_Timed_Events :
     array (1 ..
            CP.Sporadic_Server_Tasks_Mx * CP.Sporadic_Server_Replenishments_Mx)
     of aliased Replenishment_Timed_Events_Lists.Element;


   -------------
   -- Request --
   -------------
   function Request return Replenishment_Timed_Event_Ac
     renames Replenishment_Timed_Events_Resources.Request;

   -------------
   -- Release --
   -------------
   procedure Release (Repl : in Replenishment_Timed_Event_Ac)
     renames Replenishment_Timed_Events_Resources.Release;

   --------------------------------------
   -- Initialize_Pool --
   --------------------------------------
   procedure Initialize_Pool is
   begin
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         --  Initialize 'Free_Replenishment_Timed_Events' pool
         for I in 1 .. Free_Replenishment_Timed_Events'Last loop
            Replenishment_Timed_Events_Resources.Release
              (Free_Replenishment_Timed_Events (I)'Access);
         end loop;
      end if;
   end Initialize_Pool;

end MaRTE.Kernel.Replenishment_TE;
