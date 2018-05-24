------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--     'K e r n e l . T a s k s _ O p e r a t i o n s . I n t e r n a l s'
--
--                                 Spec
--
--
--  File 'k-to-internals.ads'                                          By MAR.
--
--
--  Tasks operations procedures needed by the rest of 'Kernel'. Only for
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
-------------------------------------------------------------------------------

package MaRTE.Kernel.Tasks_Operations.Internals is

   package K renames MaRTE.Kernel;

   -----------------------------
   -- Tasks Wrapper procedure --
   -----------------------------
   procedure Task_Wrapper (Task_Body : in Task_Body_Function);

   -------------
   -- Attr_OK --
   -------------
   function Attr_OK (Attr : in Pthread_Attr_T_Ac_Const) return Boolean;
   pragma Inline (Attr_OK);

   ---------------------------------------------------------------------------
   -- Basic Task Operations --------------------------------------------------
   ---------------------------------------------------------------------------

   -------------------------
   -- Get_Self_Attributes --
   -------------------------
   function Get_Self_Attributes return Pthread_Attr_T;
   pragma Inline (Get_Self_Attributes);

   -----------------
   -- Create_Task --
   -----------------
   function Create_Task (Task_Body   : in Task_Body_Function;
                         Arg         : in System.Address;
                         Attr        : in Pthread_Attr_T)  return Task_Id;
   pragma Inline (Create_Task);

   ----------------------------
   -- Terminate_Running_Task --
   ----------------------------
   --  Manages the "joinable or detached" related actions, call 'Remove_Task'
   --  and finally calls 'Do_Scheduling'.
   procedure Terminate_Running_Task (Value_Ptr : in System.Address);
   pragma Inline (Terminate_Running_Task);

   -------------------
   -- Set_Base_Prio --
   -------------------
   procedure Set_Base_Prio (T                  : in Task_Id;
                            New_Base_Prio      : in Task_Priority;
                            Head_Of_New_Prio_Q : in Boolean := False);
   pragma Inline (Set_Base_Prio);

   ------------------------
   -- Reduce_Active_Prio --
   ------------------------
   --
   --  'Head_Of_New_Prio_Q' True means that the task shall become the head
   --  of its new priority queue (not the tail like ususal). This is useful
   --  in the case the priority of a task is reduced because it has unlocked
   --  a mutex. In this case the task must continue executing even if there
   --  are some other tasks in the new priority queue.
   procedure Reduce_Active_Prio (T                  : in Task_Id;
                                 New_Prio           : in Task_Priority;
                                 Head_Of_New_Prio_Q : in Boolean);
   --  pragma Inline (Reduce_Active_Prio); {MAR OJO} para 3.15p

   -----------------------
   -- Raise_Active_Prio --
   -----------------------
   procedure Raise_Active_Prio (T        : in Task_Id;
                                New_Prio : in Task_Priority);
   --  pragma Inline (Raise_Active_Prio); {MAR OJO} para 3.15p

   ----------------------------------------------
   -- Set_POSIX_Error_And_Leave_Critic_Section --
   ----------------------------------------------
   procedure Set_POSIX_Error_And_Leave_Critic_Section (Error : in Error_Code;
                                                       Flags : in Integer);
   pragma Inline (Set_POSIX_Error_And_Leave_Critic_Section);

   ---------------------
   -- Set_POSIX_Error --
   ---------------------
   procedure Set_POSIX_Error (Error : in Error_Code);
   pragma Inline (Set_POSIX_Error);
   procedure Set_POSIX_Error (T : in Task_Id; Error : in Error_Code);
   pragma Inline (Set_POSIX_Error);

   -----------------------
   -- Reset_POSIX_Error --
   -----------------------
   procedure Reset_POSIX_Error;
   pragma Inline (Reset_POSIX_Error);

   ----------------------------------
   -- There_Has_Been_A_POSIX_Error --
   ----------------------------------
   function There_Has_Been_A_POSIX_Error return Boolean;
   pragma Inline (There_Has_Been_A_POSIX_Error);

   -------------------------------
   -- Get_Last_POSIX_Error_Code --
   -------------------------------
   function Get_Last_POSIX_Error_Code return Error_Code;
   pragma Inline (Get_Last_POSIX_Error_Code);

end MaRTE.Kernel.Tasks_Operations.Internals;
