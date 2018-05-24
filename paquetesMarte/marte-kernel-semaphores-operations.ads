------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--          'K e r n e l . S e m a p h o r e s . O p e r a t i o n s'
--
--                                  Spec
--
--
--
--  File 'k-semaphores-operations.ads'                                 By MAR.
--
--
--  POSIX semaphores implementation.
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
with MaRTE.HAL;
with MaRTE.Timespec;

package MaRTE.Kernel.Semaphores.Operations is

   package K renames MaRTE.Kernel;

   --------------
   -- Sem_Init --
   --------------
   function Sem_Init (Sem     : in Semaphore_Ac;
                      Pshared : in Int;
                      Value   : in Semaphore_Value) return Int;
   pragma Export (C, Sem_Init, "sem_init");

   -----------------
   -- Sem_Destroy --
   -----------------
   function Sem_Destroy (Sem : in Semaphore_Ac) return Int;
   pragma Export (C, Sem_Destroy, "sem_destroy");

   --------------
   -- Sem_Wait --
   --------------
   function Sem_Wait (Sem : in Semaphore_Ac) return Int;
   pragma Export (C, Sem_Wait, "sem_wait");

   -----------------
   -- Sem_Trywait --
   -----------------
   function Sem_Trywait (Sem : in Semaphore_Ac) return Int;
   pragma Export (C, Sem_Trywait, "sem_trywait");

   --------------------------
   -- Sem_Timedwait_HWTime --
   --------------------------
   function Sem_Timedwait_HWTime (Sem          : in Semaphore_Ac;
                                  Abs_Timeout  : in MaRTE.HAL.HWTime;
                                  Abs_Timespec : in MaRTE.Timespec.Timespec)
                                 return Int;
   pragma Inline (Sem_Timedwait_HWTime);

   --------------
   -- Sem_Post --
   --------------
   function Sem_Post (Sem : in Semaphore_Ac) return Int;
   pragma Export (C, Sem_Post, "sem_post");

   ------------------
   -- Sem_Getvalue --
   ------------------
   function Sem_Getvalue (Sem   : in     Semaphore_Ac;
                          Value : access Semaphore_Value) return Int;
   pragma Export (C, Sem_Getvalue, "sem_getvalue");

end MaRTE.Kernel.Semaphores.Operations;
