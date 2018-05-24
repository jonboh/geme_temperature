------------------------------------------------------------------------------
-- --------------------         M a R T E   O S         ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'S c h e d'
--
--                                   Spec
--
--
--
--  File 'sched.ads'                                                   By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'sched.h'.
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

with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Timespec;
with MaRTE.Kernel;
with MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Signals;

package MaRTE.POSIX_Sched is

   package K renames MaRTE.Kernel;

   package TO_APPSCHD   renames K.Tasks_Operations.Application_Scheduler;
   package APPSCHD_DATA renames K.Application_Scheduling_Data;

   -----------------------------------------------------
   -- Sched_Yield (13.3 Process Scheduling Functions) --
   -----------------------------------------------------
   function Sched_Yield return Int
     renames K.Tasks_Operations.Sched_Yield;

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Max (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_Get_Priority_Max (Policy : in Int) return Int;
   pragma Export (C, Sched_Get_Priority_Max, "sched_get_priority_max");

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Min (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_Get_Priority_Min (Policy : in Int) return Int;
   pragma Export (C, Sched_Get_Priority_Min, "sched_get_priority_min");

   ----------------------------------------------------------------
   -- Sched_Get_Priority_Min (13.3 Process Scheduling Functions) --
   ----------------------------------------------------------------
   function Sched_RR_Get_Interval
     (Pid : in Int; Interval : in MaRTE.Timespec.Timespec_Ac) return Int;
   pragma Export (C, Sched_RR_Get_Interval, "sched_rr_get_interval");

   ----------------------------------------------------------------------------
   -- Application-defined Scheduling ------------------------------------------
   ----------------------------------------------------------------------------

   -----------------------
   -- Scheduling Events --
   -----------------------
   --
   --  In the C equivalent type one field is a union of 2 ints size. It is
   --  emulated with the fields 'Union_Int0' and 'Union_Int1'.
   type Posix_Appsched_Event is record
      Event_Code           : APPSCHD_DATA.Event_Code_T;
      T                    : K.Task_Id;
      Union_Int0 : Int; -- 'Sched_Priority', 'Siginfo.Signo', 'Mutex' or 'Info'
      Union_Int1 : Int; -- 'Siginfo.Code' or not used.
      Union_Int2 : Int; -- 'Siginfo.Value' or not used.
      Info_Size            : Int;
   end record;
   pragma Convention (C, Posix_Appsched_Event);
   type Posix_Appsched_Event_Ac is access all Posix_Appsched_Event;
   --  Changes in the 'Sched.Posix_Appsched_Event' type must be
   --  reflected in its C equivalent 'posix_appsched_event' in
   --  'sched.h'.

   --------------------------------
   -- Execute Scheduling Actions --
   --------------------------------
   function POSIX_Appsched_Execute_Actions
     (Sched_Actions  : in APPSCHD_DATA.Sched_Actions_Set_Ac;
      Set            : in K.Signals.Signal_Set_Ac_Const;
      Timeout        : in MaRTE.Timespec.Timespec_Ac;
      Current_Time   : in MaRTE.Timespec.Timespec_Ac;
      POSIX_Event_Ac : in Posix_Appsched_Event_Ac) return Int;
   pragma Export (C, POSIX_Appsched_Execute_Actions,
                  "posix_appsched_execute_actions");
   pragma Inline (POSIX_Appsched_Execute_Actions);

end MaRTE.POSIX_Sched;
