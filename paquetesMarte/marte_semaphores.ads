------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                     'M a R T E _ S e m a p h o r e s'
--
--                                 Spec
--
--
--  File 'marte_semaphores.ads'                                        By MAR.
--
--
--  Direct mapping of semaphores internal functions defined inside
--  MaRTE OS kernel.
--
--  It is mainly intended for Ada drivers that need to use semaphores
--  to synchronize with an interrupt handler. Using this interface
--  (instead 'POSIX_Semaphores') leads to a smaller MaRTE footprint
--  since dependency with the POSIX-Ada binding is not included.
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
with MaRTE.Kernel.Semaphores;
with MaRTE.Kernel.Semaphores.Operations;
with MaRTE.POSIX_Semaphore;
with Ada.Real_Time;

package MaRTE_Semaphores is

   package K renames MaRTE.Kernel;

   subtype Semaphore_Value is K.Semaphores.Semaphore_Value;
   subtype Semaphore       is K.Semaphores.Semaphore;
   subtype Semaphore_Ac    is K.Semaphores.Semaphore_Ac;
   --  type Semaphore_Ac is access all Semaphore;

   ----------------
   -- Initialize --
   ----------------
   function Initialize (Sem     : in Semaphore_Ac;
                        Pshared : in Int;
                        Value   : in Semaphore_Value) return Int
     renames K.Semaphores.Operations.Sem_Init;

   -----------------
   -- Sem_Destroy --
   -----------------
   function Destroy (Sem : in Semaphore_Ac) return Int
     renames K.Semaphores.Operations.Sem_Destroy;

   ----------
   -- Wait --
   ----------
   function Wait (Sem : in Semaphore_Ac) return Int
     renames K.Semaphores.Operations.Sem_Wait;

   -------------
   -- Trywait --
   -------------
   function Trywait (Sem : in Semaphore_Ac) return Int
     renames K.Semaphores.Operations.Sem_Trywait;

   ---------------
   -- Timedwait --
   ---------------
   function Timedwait (Sem         : in Semaphore_Ac;
                       Abs_Timeout : in Ada.Real_Time.Time)
                      return Int;

   ----------
   -- Post --
   ----------
   function Post (Sem : in Semaphore_Ac) return Int
     renames K.Semaphores.Operations.Sem_Post;

   --------------
   -- Getvalue --
   --------------
   function Getvalue (Sem   : in     Semaphore_Ac;
                      Value : access Semaphore_Value) return Int
     renames K.Semaphores.Operations.Sem_Getvalue;

end MaRTE_Semaphores;
