------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'D e b u g _ M e s s a g e s'
--
--                                  Spec
--
--
--  File 'debug_messages.ads'                                         By MAR.
--
--
--  Configuration of the debug messages written for the kernel.
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

package MaRTE.Debug_Messages is
   pragma Preelaborate;

   ----------------------
   -- General Messages --
   ----------------------
   Scheduler_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Scheduler' messages.

   Condition_Variables_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Condition_Variables' messages.

   Mutexes_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Mutexes' messages.

   Signals_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Signals' messages.

   Timed_Events_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Timed_Events_And_Timer' messages.

   Timed_Events_Queue_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Timed_Events_Queue' messages.

   Tasks_Operations_Debug_Messages : constant Boolean := False;
   --  Enable or disable the 'Tasks_Operations' messages.

   subtype TLSF_Dynamic_Memory_Debug_Messages is Boolean range False .. False;
   --  Enable or disable the debug messages produced by the TLSF
   --  dynamic memory allocator.

   subtype Register_Active_TCBs is Boolean range False .. False;
   --  Enable Active TCBs register in 'Kernel.Pool_TCBs.Debug'.

   ---------------------
   --  Task Inspector --
   ---------------------

   --  By now it can only be used in architectures x86 and Linux_Lib

   Tasks_Inspector_Messages : constant Boolean := False;
   --  Enable or disable the messages for the task inspector tool.

   type Log_Device is (LOG_CONSOLE, LOG_ETHERNET, LOG_DISK);
   pragma Convention (C, Log_Device);

   Tasks_Inspector_Log_Device : constant Log_Device := LOG_CONSOLE;
   --  Only relevant in x86 architecture.
   --  Device where the trace data is sent:
   --    - LOG_CONSOLE: trace data is written in the standard console
   --    - LOG_ETHERNET: trace data is sent as a set of Ethernet frames with a
   --      broadcast address and a certain protocol number not allocated for
   --      other protocols. This data can be read with a Linux program that
   --      uses raw sockets to receive those frames and write them to a log
   --      file (examples/logger/linux_eth_receive_log.c).
   --    - LOG_DISK: trace data is written in a file. The name of the file is
   --      '/fat/log' in architecture x86 or 'trace_marte.dat' (created in the
   --      working directory) in the architecture Linux_Lib

   type Logger_Mechanism is (DIRECT, USE_LOGGER_TASK);
   pragma Convention (C, Logger_Mechanism);

   Task_Inspector_Logger_Mechanism : Logger_Mechanism := DIRECT;
   --  Only relevant in x86 architecture.
   --  When USE_LOGGER_TASK data is stored in a memory buffer and is written in
   --  the log device by an auxiliary task. When DIRECT, data is directly
   --  written on the log device.
   --  TODO: configurable priority and period of auxiliary task.

end MaRTE.Debug_Messages;
