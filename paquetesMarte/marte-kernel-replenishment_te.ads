------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . R e p l e n i s h m e n t _ T E'
--
--                                   Spec
--
--
--
--  File 'k-replenishment_te.ads'                                      By MAR.
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
with MaRTE.HAL;
with MaRTE.SLL;
pragma Elaborate_All (MaRTE.SLL);

package MaRTE.Kernel.Replenishment_TE is

   package K renames MaRTE.Kernel;

   ----------------------------------------------------------------------------
   -- Type 'Replenishment_Timed_Event' ----------------------------------------
   ----------------------------------------------------------------------------
   type Replenishment_Timed_Event is new Timed_Events_Lists.Element with record
      SS_Task          : Task_Id;
      Replenish_Amount : MaRTE.HAL.HWTime;
   end record;
   package Replenishment_Timed_Events_Lists is
     new MaRTE.SLL (Replenishment_Timed_Event);
   subtype Replenishment_Timed_Event_Ac is
     Replenishment_Timed_Events_Lists.Element_Ac;

   ----------------------------------------------------------------------------
   -- Application scheduling events pool --------------------------------------
   ----------------------------------------------------------------------------

   -------------
   -- Request --
   -------------
   function Request return Replenishment_Timed_Event_Ac;

   -------------
   -- Release --
   -------------
   procedure Release (Repl : in Replenishment_Timed_Event_Ac);

   ---------------------
   -- Initialize_Pool --
   ---------------------
   procedure Initialize_Pool;

end MaRTE.Kernel.Replenishment_TE;
