------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--      'K e r n e l . T i m e d _ H a n d l e r s . O p e r a t i o n s'
--
--                                 Spec
--
--
--  File 'kernel-timed_handlers-operations.ads'                        By MAR.
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
with MaRTE.Kernel.Timers;
with MaRTE.Timespec;
with MaRTE.Integer_Types;
with MaRTE.HAL;

package MaRTE.Kernel.Timed_Handlers.Operations is

   package K renames MaRTE.Kernel;

   --------------------------------
   --  MaRTE_Timed_Handler_Init  --
   --------------------------------
   function MaRTE_Timed_Handler_Init
     (TE_Ac   : Timed_Handler_TE_Ac;
      Clock   : K.Timers.Clock_Id;
      Handler : Handler_Procedure;
      Area    : System.Address;
      Size    : Size_T) return Int;
   pragma Export (C, MaRTE_Timed_Handler_Init, "marte_timed_handler_init");

   -----------------------------------
   --  MaRTE_Timed_Handler_Destroy  --
   -----------------------------------
   function MaRTE_Timed_Handler_Destroy (TE_Ac   : Timed_Handler_TE_Ac)
                                        return Int;
   pragma Export (C, MaRTE_Timed_Handler_Destroy,
                    "marte_timed_handler_destroy");

   -------------------------------
   --  MaRTE_Timed_Handler_Set  --
   -------------------------------
   --  Options: 0, TIMER_ABSTIME or PERIODIC_HANDLER
   --  Time: If null, the expiration time of the timed handler remains
   --  unchanged
   function MaRTE_Timed_Handler_Set
     (TE_Ac   : Timed_Handler_TE_Ac;
      Options : MaRTE.Integer_Types.Unsigned_32;
      Time    : MaRTE.Timespec.Timespec_Ac) return Int;
   pragma Export (C, MaRTE_Timed_Handler_Set, "marte_timed_handler_set");

   ----------------------------------------
   --  MaRTE_Timed_Handler_Set_Interval  --
   ----------------------------------------
   --  Add the interval (positive or negative) to the current expiration time.
   --  If interval is null, the expiration time of the timed handler remains
   --  unchanged
   --  Not for periodic timed handlers
   function MaRTE_Timed_Handler_Set_Interval
     (TE_Ac    : Timed_Handler_TE_Ac;
      Interval : MaRTE.Timespec.Timespec_Ac) return Int;
   pragma Export (C, MaRTE_Timed_Handler_Set_Interval,
                  "marte_timed_handler_set_interval");

   ---------------------------------------
   --  MaRTE_Timed_Handler_Set_Handler  --
   ---------------------------------------
   --  Not used, can be removed
   --  Handler: if null, the handler functions remains unchanged
   --  Options: 0, TIMER_ABSTIME or PERIODIC_HANDLER
   --  Time: If null, the expiration time of the timed handler remains
   --  unchanged
   function MaRTE_Timed_Handler_Set_Handler
     (TE_Ac   : Timed_Handler_TE_Ac;
      Handler : Handler_Procedure;
      Options : MaRTE.Integer_Types.Unsigned_32;
      Time    : MaRTE.Timespec.Timespec_Ac) return Int;
   pragma Export (C, MaRTE_Timed_Handler_Set_Handler,
                  "marte_timed_handler_set_handler");

   -----------------------------------
   --  MaRTE_Timed_Handler_Disable  --
   -----------------------------------
   function MaRTE_Timed_Handler_Disable (TE_Ac : Timed_Handler_TE_Ac)
                                        return Int;
   pragma Export (C, MaRTE_Timed_Handler_Disable,
                    "marte_timed_handler_disable");

   ----------------------------------
   --  MaRTE_Timed_Handler_Enable  --
   ----------------------------------
   function MaRTE_Timed_Handler_Enable (TE_Ac : Timed_Handler_TE_Ac)
                                       return Int;
   pragma Export (C, MaRTE_Timed_Handler_Enable, "marte_timed_handler_enable");

   -----------------------------------------
   --  MaRTE_Timed_Handler_Global_Enable  --
   -----------------------------------------
   function MaRTE_Timed_Handler_Global_Enable return Int;
   pragma Export (C, MaRTE_Timed_Handler_Global_Enable,
                    "marte_timed_handler_global_enable");

   ------------------------------------------
   --  MaRTE_Timed_Handler_Global_Disable  --
   ------------------------------------------
   function MaRTE_Timed_Handler_Global_Disable return Int;
   pragma Export (C, MaRTE_Timed_Handler_Global_Disable,
                    "marte_timed_handler_global_disable");

   ---------------------------------------
   --  MaRTE_Timed_Handler_Get_Overrun  --
   ---------------------------------------
   function MaRTE_Timed_Handler_Get_Overrun (TE_Ac : Timed_Handler_TE_Ac)
                                            return Int;
   pragma Export (C, MaRTE_Timed_Handler_Get_Overrun,
                  "marte_timed_handler_get_overrun");

   ---------------------------------
   -- MaRTE_Timed_Handler_Expired --
   ---------------------------------

   function MaRTE_Timed_Handler_Expired (TE_Ac   : Timed_Handler_TE_Ac;
                                         Expired : access Int)
                                         return Int;
   pragma Export (C, MaRTE_Timed_Handler_Expired,
                  "marte_timed_handler_expired");

   --------------------------------------------
   -- MaRTE_Timed_Handler_Time_To_Expiration --
   --------------------------------------------
   --  TS: 0 if the timed handler is expired or not set
   function MaRTE_Timed_Handler_Time_To_Expiration
     (TE_Ac : Timed_Handler_TE_Ac;
      TS    : MaRTE.Timespec.Timespec_Ac)
      return Int;
   pragma Export (C, MaRTE_Timed_Handler_Time_To_Expiration,
                  "marte_timed_handler_time_to_expiration");

   -----------------------------------------
   -- MaRTE_Timed_Handler_Expiration_Time --
   -----------------------------------------
   --  TS: absolute expiration time. 0 if the timed handler is expired or not
   --  set
   function MaRTE_Timed_Handler_Expiration_Time
     (TE_Ac : Timed_Handler_TE_Ac;
      TS    : MaRTE.Timespec.Timespec_Ac)
      return Int;
   pragma Export (C, MaRTE_Timed_Handler_Expiration_Time,
                  "marte_timed_handler_expiration_time");

   -----------
   --  Set  --
   -----------
   --  Not in the POSIX-like interface, only to be used internally. Assumes
   --  'TE.T' is set to the expiration time of the event
   procedure Set (TE  : in out Timed_Handler_Timed_Event;
                  Now :      MaRTE.HAL.HWTime);

end MaRTE.Kernel.Timed_Handlers.Operations;
