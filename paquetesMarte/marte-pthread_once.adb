------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                         'P t h r e a d _ O n c e'
--
--                                  Body
--
--
--
--  File 'pthread_once.adb'                                           By MAR.
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
with MaRTE.Kernel.Mutexes.Internals;

package body MaRTE.Pthread_Once is

   -----------------------------
   -- Function 'Pthread_Once' --
   -----------------------------
   function Pthread_Once (Once_Control : in Pthread_Once_Ac;
                          Routine      : in Init_Routine)
                         return MaRTE.Integer_Types.Int is
      use MaRTE.Kernel;
      procedure Memory_Copy (From  : access Mutexes.Mutex;
                             To    : access Mutexes.Mutex;
                             Bytes : in     Integer);
      pragma Import (C, Memory_Copy, "memory_copy");
      use type MaRTE.Integer_Types.Int;
      Ret : MaRTE.Integer_Types.Int;
      Flags : Integer;
   begin
      if (Once_Control = null    or else
          Routine = null                 ) then
         return INVALID_ARGUMENT;
      end if;

      MaRTE.Kernel.Enter_Critic_Section (Flags);
      if Once_Control.Magic = NOT_INITIALIZED then
         --  To set properly the 'tag' of the mutex object. It is not
         --  properly set since the 'Pthread_Once_T' object has been
         --  created from a C program.
         Memory_Copy
           (From  => Mutexes.Pthread_Mutex_Initializer'Unrestricted_Access,
            To    => Once_Control.Mutex'Access,
            Bytes => Mutexes.Mutex'Size / 8);
         Once_Control.Done  := False;
         Once_Control.Magic := INITIALIZED;
      end if;
      MaRTE.Kernel.Leave_Critic_Section (Flags);

      if (Once_Control.Magic /= INITIALIZED or else
          not Mutexes.Internals.Mutex_OK (Once_Control.Mutex)) then
         return INVALID_ARGUMENT;
      end if;

      Ret := Mutexes.Pthread_Mutex_Lock (Once_Control.Mutex'Access);
      pragma Assert (Ret = 0);
      if not Once_Control.Done then
         Routine.all;
         Once_Control.Done := True;
      end if;
      Ret := Mutexes.Pthread_Mutex_Unlock (Once_Control.Mutex'Access);
      pragma Assert (Ret = 0);

      return 0;
   end Pthread_Once;

end MaRTE.Pthread_Once;
