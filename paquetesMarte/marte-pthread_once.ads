------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                         'P t h r e a d _ O n c e'
--
--                                  Spec
--
--
--
--  File 'pthread_once.ads'                                           By MAR.
--
--
--  Implementation of the "Dynamic Package Initialization"
--  (pthread_once) POSIX funcionality.
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
with MaRTE.Integer_Types;
with MaRTE.Kernel.Mutexes;

package MaRTE.Pthread_Once is

   ---------------------------
   -- Type 'Pthread_Once_T' --
   ---------------------------
   type Pthread_Once_T is limited private;
   type Pthread_Once_Ac is access all Pthread_Once_T;

   -----------------------------
   -- Function 'Pthread_Once' --
   -----------------------------
   type Init_Routine is access procedure;
   function Pthread_Once (Once_Control : in Pthread_Once_Ac;
                          Routine      : in Init_Routine)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Pthread_Once, "pthread_once");


private

   --  Magic values to detect invalid Mutexes. It is an array of characters
   --  for easier reading when debugging.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   INITIALIZED     : constant Magic_Check := ('P', 'o', '_', 'I');
   NOT_INITIALIZED : constant Magic_Check := ('p', 'O', 'N', 'i');
   --  If value of 'NOT_INITIALIZED' is changed it should also be
   --  changed in 'write_marte_c_headers.adb'


   ---------------------------
   -- Type 'Pthread_Once_T' --
   ---------------------------
   type Pthread_Once_T is record
      Magic : Magic_Check;
      Mutex : aliased MaRTE.Kernel.Mutexes.Mutex;
      Done  : Boolean;
   end record;
   pragma Convention (C, Pthread_Once_T);

end MaRTE.Pthread_Once;


