------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                               'S i g n a l'
--
--                                   Body
--
--
--  File 'signal.adb'                                                  By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over the MaRTE OS. In particular this package defines
--  the functions in the C header file 'signal.h'.
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
with MaRTE.Kernel.Scheduler;

package body MaRTE.POSIX_Signal is

   use type Int;

   -------------
   -- Sigwait --
   -------------
   function Sigwait (Set : in Signal_Set_Ac_Const;
                     Sig : access Signal) return Int is
      Siginf : aliased K.Signals.Siginfo_T;
   begin
      if Sigwaitinfo (Set, Siginf'Access) < 0 then
         Sig.all := Signals.Signal_Null;
         return K.Scheduler.Self.Error_Code;
      else
         Sig.all := Siginf.Signo;
         return 0;
      end if;
   end Sigwait;

end MaRTE.POSIX_Signal;
