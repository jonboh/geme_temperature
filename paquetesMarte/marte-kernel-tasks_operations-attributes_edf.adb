------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . T a s k s _ O p e r a t i o n s .
--                     A t t r i b u t e s _ E D F'
--
--                                 Body
--
--
--  File 'k-to-attributes_edf.adb'                                     By MAR.
--
--
--  Management of the "preemption level" and the "relative deadline" attributes
--  in a pthread attributes object ('pthread_attr_t').
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body MaRTE.Kernel.Tasks_Operations.Attributes_EDF is

   package TOI  renames K.Tasks_Operations.Internals;
   use type TSPC.Timespec_Ac;

   ---------------------------------
   -- Pthread_Attr_Setreldeadline --
   ---------------------------------

   function Pthread_Attr_Setreldeadline
     (Attr         : access Pthread_Attr_T;
      Rel_Deadline : TSPC.Timespec_Ac) return Int is
   begin
      if not CP.Use_EDF_Scheduling_Policy'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      pragma Assert (Rel_Deadline /= null);
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Attr.Rel_Deadline := TSPC.Timespec_To_HWTime (Rel_Deadline.all);
      return 0;
   end Pthread_Attr_Setreldeadline;

   ---------------------------------
   -- Pthread_Attr_Getreldeadline --
   ---------------------------------

   function Pthread_Attr_Getreldeadline
     (Attr         : access Pthread_Attr_T;
      Rel_Deadline : TSPC.Timespec_Ac) return Int is
   begin
      if not CP.Use_EDF_Scheduling_Policy'First then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      pragma Assert (Rel_Deadline /= null);
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Rel_Deadline.all := TSPC.HWTime_To_Timespec (Attr.Rel_Deadline);
      return 0;
   end Pthread_Attr_Getreldeadline;

   ---------------------------------------
   --  Pthread_Attr_Setpreemptionlevel  --
   ---------------------------------------

   function Pthread_Attr_Setpreemptionlevel
     (Attr  : access Pthread_Attr_T;
      Level : in     K.Task_Preemption_Level) return Int is
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Attr.Preemption_Level := Level;
      return 0;
   end Pthread_Attr_Setpreemptionlevel;

   ---------------------------------------
   --  Pthread_Attr_Getpreemptionlevel  --
   ---------------------------------------

   function Pthread_Attr_Getpreemptionlevel
     (Attr  : access Pthread_Attr_T;
      Level : access K.Task_Preemption_Level) return Int is
      type Preemption_Level_Ac is
        access all K.Task_Preemption_Level; -- To compare with null
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Preemption_Level_Ac (Level) /= null);

      Level.all := Attr.Preemption_Level;
      return 0;
   end Pthread_Attr_Getpreemptionlevel;

end MaRTE.Kernel.Tasks_Operations.Attributes_EDF;
