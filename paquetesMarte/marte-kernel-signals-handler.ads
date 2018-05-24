------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . S i g n a l s . H a n d l e r'
--
--                                 Spec
--
--
--  File 'k-signals-handler.ads'                                       By Mar.
--
--
--  General signal catching function.
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
with System;
with MaRTE.Kernel.Signals.Global;
with MaRTE.Integer_Types;

package MaRTE.Kernel.Signals.Handler is

   package K renames MaRTE.Kernel;

   Handler_Task : K.Task_Id;
   --  Task in charge of executing the signal handler
   --  procedures. Initialized in 'Kernel.Tasks_Operations.Internals'.

   Current_Siginst_Ac : Global.Signal_Instance_Ac := null;
   --  Signal Instance currently delivered

   -------------------------------------------
   -- Funtions to be used from the GNAT RTS --
   -------------------------------------------
   function Top_Of_Stack_Of_Task_Receiving_Signal
     return MaRTE.Integer_Types.Unsigned_32;
   pragma Export (C, Top_Of_Stack_Of_Task_Receiving_Signal,
                    "signals__handler__top_of_stack_of_task_receiving_signal");

   --------------------------------------
   --  Initialize_Signal_Handler_Task  --
   --------------------------------------
   procedure Initialize_Signal_Handler_Task;

end MaRTE.Kernel.Signals.Handler;
