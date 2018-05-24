------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--          'K e r n e l . S e m a p h o r e s . I n t e r n a l s'
--
--                                  Spec
--
--
--
--  File 'k-semaphores-internals.adb'                                  By MAR.
--
--
--  Internal Semaphore operations.
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
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body MaRTE.Kernel.Semaphores.Internals is

   package SCHD renames K.Scheduler;
   package TOI  renames K.Tasks_Operations.Internals;
   package ML   renames K.Tasks_Map_Lists;

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached (T   : in Task_Id;
                              Sem : in K.Semaphore_Base_Ac) is
   begin
      if not MaRTE.Configuration_Parameters.Use_Semaphores then
         return;
      end if;

      --  Dequeue from semaphore blocked tasks list
      ML.Dequeue (T, To_Semaphore_Ac (Sem).Blocked_Tasks);

      --  Set timeout error
      TOI.Set_POSIX_Error (T, TIMED_OUT);

      --  Now 'T' is ready
      SCHD.Task_Gets_Ready (T);

      --  When 'T' is scheduled again it will execute the final part of
      --  'Sem_Timedwait_HWTime'
   end Timeout_Reached;

end MaRTE.Kernel.Semaphores.Internals;

