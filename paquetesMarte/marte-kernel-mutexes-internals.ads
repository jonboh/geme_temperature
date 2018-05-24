------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--             'K e r n e l . M u t e x e s . I n t e r n a l s'
--
--                                  Spec
--
--
--  File 'k-mutexes-internals.ads'                                     By MAR.
--
--
--  Mutexes related procedures not included in the POSIX standard. Only for
--  internal use inside the herarchy of 'Kernel'.
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

package MaRTE.Kernel.Mutexes.Internals is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   --------------
   -- Mutex_OK --
   --------------
   function Mutex_OK (M : in Mutex_Descriptor) return Boolean;
   function Mutex_OK (M           : in Mutex_Descriptor;
                      Extra_Magic : in Mutexes.Magic_Check) return Boolean;
   function Mutex_OK (M : in Mutex) return Boolean;

   ---------------------------
   -- Null_Mutex_Descriptor --
   ---------------------------
   Null_Mutex_Descriptor : constant Mutex_Descriptor;

   ------------------------------
   -- Running_Task_Locks_Mutex --
   ------------------------------
   procedure Running_Task_Locks_Mutex (M : in Mutex_Descriptor);
   pragma Inline (Running_Task_Locks_Mutex);

   ------------------------------
   -- Raise_HBT_Mutex_Priority --
   ------------------------------
   --
   --  Called when the priority of a task blocked in a
   --  'HIGHEST_BLOCKED_TASK' mutex is dinamicaly raised or when a higher
   --  priority task gets blocked in a 'HIGHEST_BLOCKED_TASK' mutex.
   --
   --  If necessary it calls 'Tasks_Operations.Raise_Active_Prio' for the
   --  task owning the mutex. That procedure raises the active priority of
   --  the task and if this task is blocked in another mutex
   --  'Raise_Mutex_Priority' is called again for this new mutex, going on
   --  recursively until founding a task that isn't blocked in a
   --  'HIGHEST_BLOCKED_TASK' mutex.
   --
   --  The scheduler must be called after it.
   procedure Raise_HBT_Mutex_Priority (M : in Mutexes_Lists.Element_Ac;
                                       New_Prio : in Task_Priority);
   --  pragma Inline (Raise_HBT_Mutex_Priority); {MAR OJO} para 3.15p

   -------------------------------
   -- Reduce_HBT_Mutex_Priority --
   -------------------------------
   --
   --  Called when the priority of a task blocked in a
   --  'HIGHEST_BLOCKED_TASK' mutex is dinamicaly reduced, well after a call
   --  to 'Set_Base_Prio' or after the task has unlocked a mutex. In the
   --  last case the 'Head_Of_New_Prio_Q' parameter must be True, because we
   --  want the task continues executing even if there are some other tasks
   --  in the new priority queue.
   --
   --  If necessary it calls 'Tasks_Operations.Reduce_Active_Prio' for the
   --  task owning the mutex. That procedure reduces the active priority of
   --  the task and if this task is blocked in another mutex
   --  'Reduce_Mutex_Priority' is called again for this new mutex, going on
   --  recursively until founding a task that isn't blocked in a
   --  'HIGHEST_BLOCKED_TASK' mutex.
   --
   --  The scheduler must be called after it.
   procedure Reduce_HBT_Mutex_Priority (M                  : in Mutex_Base_Ac;
                                        New_Prio           : in Task_Priority;
                                        Head_Of_New_Prio_Q : in Boolean);
   --  pragma Inline (Reduce_HBT_Mutex_Priority); {MAR OJO} para 3.15p

   ----------------------------------
   -- Task_Gets_Ready_Owning_Mutex --
   ----------------------------------
   procedure Task_Gets_Ready_Owning_Mutex (T : in Task_Id;
                                           M : in Mutex_Descriptor);
   pragma Inline (Task_Gets_Ready_Owning_Mutex);

   -----------------------------------
   -- Enqueue_Blocked_Task_In_Mutex --
   -----------------------------------
   procedure Enqueue_Blocked_Task_In_Mutex (T : in Task_Id;
                                            M : in Mutex_Descriptor);
   pragma Inline (Enqueue_Blocked_Task_In_Mutex);

   ------------------------
   -- Task_Unlocks_Mutex --
   ------------------------
   --  Always call Do_Scheduling after this procedure since it activates the
   --  most prioritary task blocked in mutex (if any)
   procedure Task_Unlocks_Mutex (T : in Task_Id;
                                 M : in Mutex_Descriptor);
   pragma Inline (Task_Unlocks_Mutex);

   ---------------
   -- Set_Owner --
   ---------------
   procedure Set_Owner (M : in Mutex_Descriptor;
                        T : in Task_Id);
   pragma Inline (Set_Owner);

   ---------------
   -- Get_Owner --
   ---------------
   function Get_Owner (M : in Mutex_Descriptor) return Task_Id;
   pragma Inline (Get_Owner);

   -------------------------------------
   -- Decrement_Num_Of_Associated_CVs --
   -------------------------------------
   procedure Decrement_Num_Of_Associated_CVs (M : in Mutex_Descriptor);

   -------------------------------------
   -- Increment_Num_Of_Associated_CVs --
   -------------------------------------
   procedure Increment_Num_Of_Associated_CVs (M : in Mutex_Descriptor);

   ---------------------
   -- Timeout_Reached --
   ---------------------
   procedure Timeout_Reached  (T : in Task_Id; Mb : in Mutex_Base_Ac);

   ----------------------------
   --  Initialize_HCP_Mutex  --
   ----------------------------
   --
   --  Used internally in the kernel. Currently only from
   --  'k-file_system.Open'.
   procedure Initialize_HCP_Mutex (M       : access Mutex;
                                   Ceiling : in     Ceiling_Priority);

private
   Null_Mutex_Descriptor : constant Mutex_Descriptor := null;
end MaRTE.Kernel.Mutexes.Internals;
