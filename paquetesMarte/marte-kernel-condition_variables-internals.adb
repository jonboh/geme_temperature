------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--  'K e r n e l . C o n d i t i o n _ V a r i a b l e s . I n t e r n a l s'
--
--                                  Body
--
--
--  File 'k-cv-internas.adb'                                            By MAR.
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

with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Condition_Variables.Debug;
with MaRTE.Kernel.Mutexes.Internals; use MaRTE.Kernel.Mutexes.Internals;

with MaRTE.Kernel.Debug;

package body MaRTE.Kernel.Condition_Variables.Internals is

   package ML   renames K.Tasks_Map_Lists;
   package DBG  renames K.Debug;
   use Tasks_Lists;

   use type Int;

   --------------
   -- Cond_OK --
   --------------
   function Cond_OK (Cond : in Condition_Descriptor) return Boolean is
   begin
      return Cond /= null and then Cond.Magic = INITIALIZED;
   end Cond_OK;
   function Cond_OK (Cond : in Condition) return Boolean is
   begin
      return Cond.Magic = INITIALIZED;
   end Cond_OK;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached (T : in Task_Id; CV : in Condition_Base_Ac) is
      CVd : Condition_Descriptor := Condition (CV.all)'Access;
   begin

      pragma Debug (Debug.Timeout_Reached); -- Trace;

      ML.Dequeue (T, CVd.Blocked_Tasks);

      --  CV no associated with mutex or invalid associated mutex
      pragma Debug (DBG.Assert (CVd.Is_Associated_With_Mutex_Now and
                                Mutexes.Internals.Mutex_OK (CVd.M)));

      if ML.Is_Empty (CVd.Blocked_Tasks) then
         Decrement_Num_Of_Associated_CVs (CVd.M);
         CVd.Is_Associated_With_Mutex_Now := False;
      end if;

      --  Set the internal notification to record the task is
      --  activated due to a timeout (it will be checked in
      --  'Condition_Variables.Timed_Wait_HWTime' when 'T' is
      --  scheduled again)
      pragma Assert (T.Internal_Error_Code = NO_ERROR);
      T.Internal_Error_Code := TIMED_OUT;

      if Mutexes.Internals.Get_Owner (CVd.M) = null then
         --  Mutex unlocked
         Mutexes.Internals.Task_Gets_Ready_Owning_Mutex (T, CVd.M);
      else
         --  Mutex Locked
         T.Status := BLOCKED;
         Mutexes.Internals.Enqueue_Blocked_Task_In_Mutex (T, CVd.M);
      end if;
   end Timeout_Reached;

   ---------------------------
   -- Reorder_Task_In_Queue --
   ---------------------------
   procedure Reorder_Task_In_Queue (T        : in Task_Id;
                                    New_Prio : in Task_Priority) is
   begin
      pragma Debug
        (DBG.Assert (K.Task_OK (T) and then
                     Cond_OK (Condition_Descriptor (T.CV_Where_Blocked))));

      ML.Dequeue (T, Condition_Descriptor (T.CV_Where_Blocked).Blocked_Tasks);
      T.Active_Prio := New_Prio;
      ML.Enqueue_In_Order
        (T, Condition_Descriptor (T.CV_Where_Blocked).Blocked_Tasks);
   end Reorder_Task_In_Queue;

end MaRTE.Kernel.Condition_Variables.Internals;
