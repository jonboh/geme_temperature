------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . T a s k s _ O p e r a t i o n s .
--                     A t t r i b u t e s _ E D F'
--
--                                 Spec
--
--
--  File 'k-to-attributes_edf.ads'                                     By MAR.
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
with MaRTE.Timespec;

package MaRTE.Kernel.Tasks_Operations.Attributes_EDF is

   package K    renames MaRTE.Kernel;
   package TSPC renames MaRTE.Timespec;

   function Pthread_Attr_Setreldeadline
     (Attr         : access Pthread_Attr_T;
      Rel_Deadline : TSPC.Timespec_Ac) return Int;
   pragma Export (C, Pthread_Attr_Setreldeadline,
                  "pthread_attr_setreldeadline");
   pragma Export_Function (Pthread_Attr_Setreldeadline,
                           "pthread_attr_setreldeadline",
                           Mechanism => (Attr => Value,
                                         Rel_Deadline => Value));

   function Pthread_Attr_Getreldeadline
     (Attr         : access Pthread_Attr_T;
      Rel_Deadline : TSPC.Timespec_Ac) return Int;
   pragma Export (C, Pthread_Attr_Getreldeadline,
                  "pthread_attr_getreldeadline");
   pragma Export_Function (Pthread_Attr_Getreldeadline,
                           "pthread_attr_getreldeadline",
                           Mechanism => (Attr => Value,
                                         Rel_Deadline => Value));

   function Pthread_Attr_Setpreemptionlevel
     (Attr  : access Pthread_Attr_T;
      Level : in     K.Task_Preemption_Level) return Int;
   pragma Export (C, Pthread_Attr_Setpreemptionlevel,
                  "pthread_attr_setpreemptionlevel");

   function Pthread_Attr_Getpreemptionlevel
     (Attr  : access Pthread_Attr_T;
      Level : access K.Task_Preemption_Level) return Int;
   pragma Export (C, Pthread_Attr_Getpreemptionlevel,
                  "pthread_attr_getpreemptionlevel");

end MaRTE.Kernel.Tasks_Operations.Attributes_EDF;
