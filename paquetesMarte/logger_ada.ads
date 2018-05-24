------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                            'Logger_Ada'
--
--                                 Spec
--
--
--  File 'logger_ada.ads'                                   By Sangorrin
--
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
with POSIX_Timers;
with MaRTE.Timespec;
with Interfaces.C;
with System;

package Logger_Ada is

   package C renames Interfaces.C;

   -----------
   -- Types --
   -----------

   type Log_Device is (Log_Console, Log_Ethernet, Log_Disk);
   for Log_Device use (Log_Console => 0, Log_Ethernet => 1, Log_Disk => 2);

   -----------------
   -- Logger_Init --
   -----------------

   function Logger_Init (Dev : in Log_Device) return C.Int;
   pragma Import (C, Logger_Init, "logger_init");

   --------------------------
   -- Logger_Thread_Create --
   --------------------------

   function Logger_Thread_Create
      (Period : access MaRTE.Timespec.Timespec) return C.Int;
   pragma Import (C, Logger_Thread_Create, "logger_thread_create");

   ------------------------
   -- Logger_Manual_Call --
   ------------------------

   function Logger_Manual_Call return C.Int;
   pragma Import (C, Logger_Manual_Call, "logger_manual_call");

   ------------------------
   -- Logger_Direct_Call --
   ------------------------

   function Logger_Direct_Call (Logger_Buffer : access System.Address;
                                Nbytes        : in C.Int) return C.Int;
   pragma Import (C, Logger_Direct_Call, "logger_direct_call");

end Logger_Ada;
