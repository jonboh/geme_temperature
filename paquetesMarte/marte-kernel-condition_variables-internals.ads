------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--  'K e r n e l . C o n d i t i o n _ V a r i a b l e s . I n t e r n a l s'
--
--                                  Spec
--
--
--  File 'k-cv-internas.ads'                                           By MAR.
--
--
--  Condition Variables related types and procedures not included in the
--  POSIX standard. Only for internal use inside the herarchy of 'Kernel'.
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

package MaRTE.Kernel.Condition_Variables.Internals is

   package K renames MaRTE.Kernel;

   --------------
   -- Cond_OK --
   --------------
   function Cond_OK (Cond : in Condition_Descriptor) return Boolean;
   function Cond_OK (Cond : in Condition) return Boolean;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached (T : in Task_Id; CV : in Condition_Base_Ac);
   ---------------------------
   -- Reorder_Task_In_Queue --
   ---------------------------
   --
   --  To be called when priority of a task blocked in a CV
   --  changes. This procedure reorders the task in the CV's blocked
   --  tasks queue. The CV is identified using the field
   --  'T.CV_Where_Blocked'.
   --
   --  Field 'T.Active_Prio' must keep the old value in order to find
   --  the task in the queue. After execution 'T.Active_Prio' is set
   --  to 'New_Prio'.
   procedure Reorder_Task_In_Queue (T        : in Task_Id;
                                    New_Prio : in Task_Priority);
   pragma Inline (Reorder_Task_In_Queue);

end MaRTE.Kernel.Condition_Variables.Internals;
