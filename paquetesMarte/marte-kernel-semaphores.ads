------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                    'K e r n e l . S e m a p h o r e s'
--
--                                  Spec
--
--
--
--  File 'k-semaphores.ads'                                          By MAR.
--
--
--  Semaphore type and operations.
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
with Ada.Unchecked_Conversion;

package MaRTE.Kernel.Semaphores is

   package K renames MaRTE.Kernel;

   ----------------------------
   -- Type 'Semaphore_Value' --
   ----------------------------
   type Semaphore_Value is new MaRTE.Integer_Types.Unsigned;

   ----------------------
   -- Type 'Semaphore' --
   ----------------------
   type Semaphore is private;
   type Semaphore_Ac is access all Semaphore;

   ------------------
   -- Semaphore_OK --
   ------------------
   function Semaphore_OK (Sem : in Semaphore_Ac) return Boolean;

   ----------------------------------------------------------------
   -- Conversions between 'Semaphore_Base_Ac' and 'Semaphore_Ac' --
   ----------------------------------------------------------------
   function To_Semaphore_Base_Ac is
      new Ada.Unchecked_Conversion (Semaphore_Ac, K.Semaphore_Base_Ac);
   function To_Semaphore_Ac is
      new Ada.Unchecked_Conversion (K.Semaphore_Base_Ac, Semaphore_Ac);

private
   --  Magic values to detect invalid Semaphores. It is an array of characters
   --  for easier reading when debugging.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   INITIALIZED : constant Magic_Check := ('S', 'I', 'n', 't');
   DESTROYED   : constant Magic_Check := ('s', 'D', 't', 'y');

   ----------------------
   -- Type 'Semaphore' --
   ----------------------
   type Semaphore is record
      Value         : Semaphore_Value;
      Blocked_Tasks : K.Tasks_Map_Lists.Map_List;
      Magic         : Magic_Check;
   end record;

end MaRTE.Kernel.Semaphores;
