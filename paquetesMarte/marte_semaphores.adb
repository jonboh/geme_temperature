------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                     'M a R T E _ S e m a p h o r e s'
--
--                                 Body
--
--
--  File 'marte_semaphores.adb'                          By Daniel Sangorrín.
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
with Ada.Unchecked_Conversion;
with MaRTE.Kernel.Semaphores.Operations;
with MaRTE.HAL;
with MaRTE.Timespec;
pragma Elaborate_All (MaRTE.Timespec);

package body MaRTE_Semaphores is

   package HWI  renames MaRTE.HAL;

   Fake_Timespec : constant MaRTE.Timespec.Timespec :=
     MaRTE.Timespec.To_Timespec (0.0);

   ---------------
   -- Timedwait --
   ---------------
   function Timedwait (Sem         : in Semaphore_Ac;
                       Abs_Timeout : in Ada.Real_Time.Time)
                      return Int is
      function Time_To_Duration is new Ada.Unchecked_Conversion
         (Ada.Real_Time.Time, Duration);
   begin
      return K.Semaphores.Operations.Sem_Timedwait_HWTime
        (Sem,
         HWI.Duration_To_HWTime (Time_To_Duration (Abs_Timeout)),
         Fake_Timespec);
   end Timedwait;


end MaRTE_Semaphores;
