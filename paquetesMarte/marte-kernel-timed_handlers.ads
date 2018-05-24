------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                'K e r n e l . T i m e d _ H a n d l e r s'
--
--                                 Spec
--
--
--  File 'kernel-timed_handlers.ads'                                    By MAR.
--
--
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
-------------------------------------------------------------------------------
with MaRTE.HAL;
with MaRTE.Integer_Types;
with MaRTE.POSIX_Constants;
--  with Kernel.Timers;

with System;

package MaRTE.Kernel.Timed_Handlers is

   package K renames MaRTE.Kernel;

   package CPU_TE_Lists renames K.CPU_Time_Timed_Events_Lists;

   type Timed_Handler_TE_Ac;

   subtype Timed_Handler_Options is MaRTE.Integer_Types.Unsigned_32;
   PERIODIC_HANDLER : constant := MaRTE.POSIX_Constants.PERIODIC_HANDLER;
   ABSOLUTE_TIMER   : constant := MaRTE.POSIX_Constants.ABSOLUTE_TIMER;

   ---------------
   --  Handler  --
   ---------------
   type Handler_Procedure is access
     procedure (Area : in System.Address;
                TE   : in Timed_Handler_TE_Ac);
   pragma Convention (C, Handler_Procedure);

   ---------------------------------
   --  Timed_Handler_Timed_Event  --
   ---------------------------------

   --  Magic values
   --  To detect invalid 'Timed_Handler_Timed_Event'. It is an array of
   --  characters for easier reading when debugging.
   type Magic_Check is array (1 .. 4) of Character;
   pragma Pack (Magic_Check);
   NOT_INITIALIZED : constant Magic_Check := ('t', 'H', 'e', 'N');
   INITIALIZED     : constant Magic_Check := ('T', 'h', 'E', 'I');

   --  Overrun Count
   type Overrun_Count_T is new Natural;
   for Overrun_Count_T'Size use MaRTE.Integer_Types.Int'Size;

   type Timed_Handler_Timed_Event is new CPU_TE_Lists.Element with record
      Handler  : Handler_Procedure;

      Area     : System.Address;

      Options  : Timed_Handler_Options;

      Period : MaRTE.HAL.HWTime;

      Overrun_Count : Overrun_Count_T;

      --  Armed : Boolean; Inherit from CPU_TE_Lists.Element
      --  True for valid events that haven't expired yet (always
      --  true for periodic events)

      Enabled  : Boolean;
      --  Set to false when the event is disabled using
      --  'MaRTE_Timed_Handler_Disable'.

      Queued : Boolean;
      --  True if the event is enqueued in the scheduler queue, CPU time queue
      --  or pending timed handlers queue (Internals.Pending_Handlers_List)

      Pending_Enable : Boolean;
      --  True if the event has expired but it is disabled at the moment. The
      --  handler will be executed as soon as the handler execution is
      --  re-enabled using 'MaRTE_Timed_Handler_Enable' (provided handlers are
      --  not globally disabled).

      Pending_Global : Boolean;
      --  True if the event has expired but the handlers execution is globally
      --  disabled at the moment. The handler will be executed as soon as the
      --  'MaRTE_Timed_Handler_Global_Enable' is called (provided this
      --  particular handler is individualy enabled).

      Magic    : Magic_Check;
      --  Allows detecting invalid events.
   end record;

   type Timed_Handler_TE_Ac is access all Timed_Handler_Timed_Event;

end MaRTE.Kernel.Timed_Handlers;
