------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--            'K e r n e l . C o n d i t i o n _ V a r i a b l e s'
--
--                                  Spec
--
--
--  File 'k-condition_variables.ads'                                   By Mar.
--
--
--  Condition Variables management.
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
with MaRTE.HAL;
with MaRTE.Kernel.Mutexes;

package MaRTE.Kernel.Condition_Variables is

   package K renames MaRTE.Kernel;

   type Condition is limited private;
   type Condition_Descriptor is access all Condition;

   type Attributes is private;
   Default_Attributes : constant Attributes;
   function Pthread_Condattr_Init (Attr : access Attributes)
                                  return Int;
   pragma Export (C, Pthread_Condattr_Init, "pthread_condattr_init");

   function Pthread_Condattr_Destroy (Attr : access Attributes)
                                      return Int;
   pragma Export (C, Pthread_Condattr_Destroy, "pthread_condattr_destroy");

   function Pthread_Cond_Init (Cond : access Condition;
                               Attr : access Attributes)
                               return Int;
   pragma Export (C, Pthread_Cond_Init, "pthread_cond_init");

   function Pthread_Cond_Destroy (Cond : access Condition)
                                  return Int;
   pragma Export (C, Pthread_Cond_Destroy, "pthread_cond_destroy");

   function Descriptor_Of (Cond : Condition) return Condition_Descriptor;

   function Pthread_Cond_Signal (Cond : access Condition)
                                 return Int;
   pragma Export (C, Pthread_Cond_Signal, "pthread_cond_signal");

   function Pthread_Cond_Broadcast (Cond : in Condition_Descriptor)
                                    return Int;
   pragma Export (C, Pthread_Cond_Broadcast, "pthread_cond_broadcast");

   function Pthread_Cond_Wait (Cond : in Condition_Descriptor;
                               M    : in K.Mutexes.Mutex_Descriptor)
                               return Int;
   pragma Export (C, Pthread_Cond_Wait, "pthread_cond_wait");
   function Timed_Wait_HWTime
     (Cond    : in Condition_Descriptor;
      M       : in K.Mutexes.Mutex_Descriptor;
      Timeout : in MaRTE.HAL.HWTime) return Int;
   pragma Inline (Timed_Wait_HWTime);

private
   --  Magic values to detect invalid attributes objects. It is an
   --  array of characters for easier reading when debugging.
   type Attr_Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Attr_Magic_Check);
   ATTR_NOT_INITIALIZED : constant Attr_Magic_Check := ('c', 'v', 'a', 'N');
   ATTR_INITIALIZED     : constant Attr_Magic_Check := ('C', 'V', 'A', 'I');

   type Attributes is record
      Magic : Attr_Magic_Check := ATTR_NOT_INITIALIZED;
   end record;

   Default_Attributes : constant Attributes := (Magic => ATTR_INITIALIZED);

   --  Magic values
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   NOT_INITIALIZED : constant Magic_Check := ('C', 'N', '_', 'I');
   INITIALIZED     : constant Magic_Check := ('C', 'I', 'N', 'I');
   FINALIZED       : constant Magic_Check := ('C', 'F', 'I', 'N');

   type Condition is new Condition_Base with record
      Magic : Magic_Check := NOT_INITIALIZED;
      --  To detect invalid CVs when debugging

      Id : Integer := -1;
      --  For easier identification when debugging

      Blocked_Tasks : aliased K.Tasks_Map_Lists.Map_List;
      --  List of tasks blocked in the CV

      M : K.Mutexes.Mutex_Descriptor;
      --  Mutex specified in the last succesful call to 'Wait' or 'Timed_Wait'

      Is_Associated_With_Mutex_Now : Boolean := False;
      --  Indicates whether there is some task waiting on the condition (and
      --  then using 'M') or not. If false a new call to 'Wait' or
      --  'Timed_Wait' is allowed to change 'M'.

      Descriptor : Condition_Descriptor := null;
      --  Used by 'Descriptor_Of'
   end record;

end MaRTE.Kernel.Condition_Variables;
