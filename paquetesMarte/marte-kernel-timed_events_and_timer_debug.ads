------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--   'K e r n e l . T i m e d _ E v e n t s _ A n d _ T i m e r _ D e b u g'
--
--                                 Spec
--
--
--  File 'k-timed_events_and_timer_debug.ads'                          By MAR.
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

package MaRTE.Kernel.Timed_Events_And_Timer_Debug is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   procedure Timer_Program_Event (At_Time  : in HAL.HWTime;
                                  Interval : in HAL.HWTime;
                                  T        : in HAL.HWTime);

   procedure Timer_Program_Normal (At_Time  : in HAL.HWTime;
                                   Interval : in HAL.HWTime);

   procedure New_Standard_Event (At_Time : HAL.HWTime;
                                 T       : HAL.HWTime);

end MaRTE.Kernel.Timed_Events_And_Timer_Debug;
