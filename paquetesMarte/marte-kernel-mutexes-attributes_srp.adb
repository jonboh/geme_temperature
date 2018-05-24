------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--         'K e r n e l . M u t e x e s . A t t r i b u t e s _ S R P'
--
--                                  Body
--
--
--
--  File 'k-mutexes-attributes_srp.adb'                                By MAR.
--
--
--  Management of the "preemption level" attribute in a mutex
--  attributes object.
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

package body MaRTE.Kernel.Mutexes.Attributes_SRP is

   --------------------------------------------
   --  Pthread_Mutexattr_Setpreemptionlevel  --
   --------------------------------------------
   function Pthread_Mutexattr_Setpreemptionlevel
     (Attr  : access K.Mutexes.Attributes;
      Level : in     K.Task_Preemption_Level) return Int is
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      if not Attr_OK (Attr) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Preemption_Level := Level;
      return 0;
   end Pthread_Mutexattr_Setpreemptionlevel;

   --------------------------------------------
   --  Pthread_Mutexattr_Getpreemptionlevel  --
   --------------------------------------------
   function Pthread_Mutexattr_Getpreemptionlevel
     (Attr  : access K.Mutexes.Attributes;
      Level : access K.Task_Preemption_Level) return Int is
      type Preemption_Level_Ac is
        access all K.Task_Preemption_Level; -- To compare with null
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         return OPERATION_NOT_IMPLEMENTED;
      end if;

      if not Attr_OK (Attr) or Preemption_Level_Ac (Level) = null then
         return INVALID_ARGUMENT;
      end if;

      Level.all := Attr.Preemption_Level;
      return 0;
   end Pthread_Mutexattr_Getpreemptionlevel;

end MaRTE.Kernel.Mutexes.Attributes_SRP;
