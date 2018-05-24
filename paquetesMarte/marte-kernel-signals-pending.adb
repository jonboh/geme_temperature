------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . S i g n a l s . P e n d i n g'
--
--                                 Body
--
--
--  File 'k-signals-pending.adb'                                       By MAR.
--
--
--  Pending signal instances management.
--  Queue of signal instances to execute signal handlers.
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


with MaRTE.Kernel.Signals.Internals;
with MaRTE.SLL.Order_Advanced;
pragma Elaborate_All (MaRTE.SLL.Order_Advanced);

package body MaRTE.Kernel.Signals.Pending is

   package SILST  renames Global.Signal_Instances_Lists;
   package SILSTA  renames Global.Signal_Instances_Lists_Advanced;
   use Global.Signal_Instances_Lists;
   use Global.Sig_Data_Lists;

   --------------------------------
   -- Signals_Pending_On_Process --
   --------------------------------
   Signals_Pending_On_Process : Global.Pending_Signals_List :=
     (others => Global.Signal_Instances_Lists.Null_List);

   -------------------------------
   -- Discard_Pending_Instances --
   -------------------------------
   --
   --   Release the resources of all the pending instances with signo 'Sig'
   --  on all the task and on the process.
   procedure Discard_Pending_Instances (Sig : in Signal) is
      Siginst_Ac : Global.Signal_Instance_Ac;
      Sigdata : Sig_Data_Ac :=
        Global.Sig_Data_Lists.Head (Global.Sig_Data_Blocks_Used);
   begin
      --  Release the signal instances pending on process.
      loop
         Siginst_Ac := SILST.Head (Signals_Pending_On_Process (Sig));
         exit when Siginst_Ac = null;
         SILST.Dequeue_Head (Signals_Pending_On_Process (Sig));
         K.Signals.Internals.Release_Signal_Instance (Siginst_Ac);
      end loop;

      --  Pending on every task
      while Sigdata /= null loop
         --  Release the signal instances pending on 'Sigdata'.
         loop
            Siginst_Ac := SILST.Head (Sigdata.Pending_Signals (Sig));
            exit when Siginst_Ac = null;
            SILST.Dequeue_Head (Sigdata.Pending_Signals (Sig));
            K.Signals.Internals.Release_Signal_Instance (Siginst_Ac);
         end loop;
         Sigdata := Next (Sigdata);
      end loop;
   end Discard_Pending_Instances;

   -----------------------
   -- Is_Signal_Pending --
   -----------------------
   --
   --  Some signal instance with signo 'Sig' is pending either on the
   --  process or on the task owning 'Sigdata'.
   function Is_Signal_Pending (Sig     : in Signal;
                               Sigdata : in Sig_Data_Ac) return Boolean is
   begin
      return (not SILST.Is_Empty (Signals_Pending_On_Process (Sig)) or else
              not SILST.Is_Empty (Sigdata.Pending_Signals (Sig)));
   end Is_Signal_Pending;

   ------------------------------------
   -- First_Signal_Instance_Accepted --
   ------------------------------------
   --
   --  Returns the first signal instance with signo 'Sig' pending on the
   --  task or else on the process. Returns null if no signal instance
   --  pending.
   function First_Signal_Instance_Accepted (Sig     : in Signal;
                                            Sigdata : in Sig_Data_Ac)
                                            return Global.Signal_Instance_Ac is
      Siginst_Ac : Global.Signal_Instance_Ac := null;
   begin
      if not SILST.Is_Empty (Sigdata.Pending_Signals (Sig)) then
         --  Return first signal instance queued on task
         Siginst_Ac := SILST.Head (Sigdata.Pending_Signals (Sig));
         SILST.Dequeue_Head (Sigdata.Pending_Signals (Sig));

      elsif not SILST.Is_Empty (Signals_Pending_On_Process (Sig)) then
         --  Return first signal instance queued on process
         Siginst_Ac := SILST.Head (Signals_Pending_On_Process (Sig));
         SILST.Dequeue_Head (Signals_Pending_On_Process (Sig));

      end if;
      pragma Assert (Siginst_Ac /= null,
                     "First_Signal_Instance_Accepted: instance not found");
      return Siginst_Ac;
   end First_Signal_Instance_Accepted;

   -----------------------------------------
   -- Accept_All_Pending_Signal_Instances --
   -----------------------------------------
   function Accept_All_Pending_Signal_Instances
     (Sigdata_Ac : in Sig_Data_Ac;
      Set        : in Signal_Set_Ac_Const) return SILST.List is
      Tmp_List : SILST.List := SILST.Null_List;
   begin
      for S in Signal_Set_Range loop
         if Set (S) then
            --  Add signals pending on task
            SILSTA.Append_List_To_Tail (From => Sigdata_Ac.Pending_Signals (S),
                                        To   => Tmp_List);
            --  Add signals pending on process
            SILSTA.Append_List_To_Tail (From => Signals_Pending_On_Process (S),
                                        To   => Tmp_List);
         end if;
      end loop;
      return Tmp_List;
   end Accept_All_Pending_Signal_Instances;

   -------------------------------------------
   -- Signal_Instance_Generated_For_Process --
   -------------------------------------------
   --
   --  'Siginst_Ac' is queued as pending on the process.
   procedure Signal_Instance_Generated_For_Process
     (Siginst_Ac : Global.Signal_Instance_Ac) is
   begin
      SILST.Enqueue_Tail (Siginst_Ac,
                          Signals_Pending_On_Process (Siginst_Ac.Signo));
   end Signal_Instance_Generated_For_Process;

   ----------------------------------------
   -- Signal_Instance_Generated_For_Task --
   ----------------------------------------
   --
   --  'Siginst_Ac' is queued as pending on the task.
   procedure Signal_Instance_Generated_For_Task
     (Siginst_Ac : in Signal_Instance_Ac) is
   begin
      SILST.Enqueue_Tail
        (Siginst_Ac,
         Global.Sig_Data_Ac
         (Siginst_Ac.To_Task.Sig_Data).Pending_Signals (Siginst_Ac.Signo));
   end Signal_Instance_Generated_For_Task;

   ---------------------------------------------------------------------------
   -- Signal Instances to execute its handler --------------------------------
   ---------------------------------------------------------------------------

   -------------------
   -- Handler queue --
   -------------------
   Signal_Instances_Handler_Q : SILST.List := SILST.Null_List;
   function "<" (Left, Right : in SILST.Element_Ac)
                 return Boolean;
   function "<" (Left, Right : in SILST.Element_Ac)
                 return Boolean is
   begin
      return Left.Signo < Right.Signo;
   end "<";
   package Signal_Instances_Lists_Order_Advanced is
     new Signal_Instances_Lists.Order_Advanced (">" => "<");

   -----------------------------------------
   -- Extract_Head_Instace_From_Handler_Q --
   -----------------------------------------
   --
   --  Returns the first instance from the queue of instances to execute its
   --  handler. In this queue the instances are ordered by signo (lower
   --  signo first) and FIFO for instances with the same
   --  signo. 'Release_Signal_Instace' should be called after this function
   --  to release the resources taken by the instance.
   function Extract_Head_Instace_From_Handler_Q return Signal_Instance_Ac is
      Siginst_Ac : Global.Signal_Instance_Ac;
   begin
      Siginst_Ac := SILST.Head (Signal_Instances_Handler_Q);
      if Siginst_Ac /= null then
         SILST.Dequeue_Head (Signal_Instances_Handler_Q);
      end if;
      return Siginst_Ac;
   end Extract_Head_Instace_From_Handler_Q;

   --------------------------------------
   -- Add_Signal_Instaces_To_Handler_Q --
   --------------------------------------
   --
   --  Enqueue in the handler queue all the signal instances with signo
   --  'Sig' pending either on the process or on the task owning 'Sigdata'.
   procedure Add_Signal_Instaces_To_Handler_Q (Sig        : in Signal;
                                               Sigdata_Ac : in Sig_Data_Ac) is
      Siginst_Ac : Global.Signal_Instance_Ac;
   begin
      --  Set 'To_Sigdata' in pending on process signals.
      Siginst_Ac :=
        SILST.Head (Signals_Pending_On_Process (Sig));
      while Siginst_Ac /= null loop
         Siginst_Ac.To_Task := Sigdata_Ac.Owner;
         Siginst_Ac := SILST.Next (Siginst_Ac);
      end loop;

      --  Move signal instances to the execute handler queue
      Signal_Instances_Lists_Order_Advanced.Insert_Monopriority_List
        (Org  => Signals_Pending_On_Process (Sig),
         Dest => Signal_Instances_Handler_Q);
      Signal_Instances_Lists_Order_Advanced.Insert_Monopriority_List
        (Org  => Sigdata_Ac.Pending_Signals (Sig),
         Dest => Signal_Instances_Handler_Q);
   end Add_Signal_Instaces_To_Handler_Q;

end MaRTE.Kernel.Signals.Pending;
