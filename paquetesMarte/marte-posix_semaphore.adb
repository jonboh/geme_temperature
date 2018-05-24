------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                          'S e m a p h o r e s'
--
--                                  Body
--
--
--
--  File 'semaphores.adb'                                              By MAR.
--
--
--  Part of the POSIX semaphores interface not directly implemented in
--  package 'Kernel.Semaphores.Operations'.
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
with MaRTE.Kernel.Semaphores.Operations;

package body MaRTE.POSIX_Semaphore is

   -------------------
   -- Sem_Timedwait --
   -------------------
   function Sem_Timedwait (Sem         : in K.Semaphores.Semaphore_Ac;
                           Abs_Timeout : in MaRTE.Timespec.Timespec_Ac_Const)
                          return MaRTE.Integer_Types.Int is
   begin
      if MaRTE.Timespec."=" (Abs_Timeout, null) then
         return K.Semaphores.Operations.Sem_Wait (Sem);
      else
         return K.Semaphores.Operations.Sem_Timedwait_HWTime
           (Sem,
            MaRTE.Timespec.Timespec_To_HWTime (Abs_Timeout.all),
            Abs_Timeout.all);
      end if;
   end Sem_Timedwait;

end MaRTE.POSIX_Semaphore;
