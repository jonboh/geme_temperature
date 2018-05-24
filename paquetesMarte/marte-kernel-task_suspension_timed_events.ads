------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--   'K e r n e l . T a s k _ S u s p e n s i o n _ T i m e d _ E v e n t s'
--
--                                  Spec
--
--
--  File 'k-task_suspension_te.ads'                                    By MAR.
--
--
--  This package defines the type 'Task_Suspension_Timed_Event',
--  which is the temporal events queued when a tasks gets suspended,
--  and the pool of objects of this type.
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
package MaRTE.Kernel.Task_Suspension_Timed_Events is

   package K renames MaRTE.Kernel;

   ---------------------------------
   -- Task_Suspension_Timed_Event --
   ---------------------------------
   type Suspension_Reason is (SUSPENSION,
                              CV_TIMEDWAIT,
                              MUTEX_TIMEDWAIT,
                              SEM_TIMEDWAIT,
                              HWINTR_TIMEDWAIT,
                              APPSCHED_EVENT_TIMEDWAIT);
   type Task_Suspension_Timed_Event is new Timed_Events_Lists.Element
     with record
        Reason              : Suspension_Reason;
        Suspended_Task      : Task_Id;
        CV_Where_Blocked    : Condition_Base_Ac;
        Mutex_Where_Blocked : Mutex_Base_Ac;
        Sem_Where_Blocked   : Semaphore_Base_Ac;
     end record;
   type Task_Suspension_TE_Ac is access all Task_Suspension_Timed_Event;

   function Request_Task_Suspension_TE return K.Timed_Event_Ac;
   pragma Inline (Request_Task_Suspension_TE);

   procedure Release_Task_Suspension_TE (TE : K.Timed_Event_Ac);
   pragma Inline (Release_Task_Suspension_TE);

   procedure Initialize_Pool;

end MaRTE.Kernel.Task_Suspension_Timed_Events;
