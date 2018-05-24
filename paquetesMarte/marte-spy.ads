------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                      'T a s k s _ I n s p e c t o r'
--
--                                 Spec
--
--
--  File 'tasks_inspector.ads'                                        By MAR.
--
--
--  Operantions to send scheduling events to the host using a serial
--  port. The events sent using 'Send_Event' can be interpretated by the
--  "Tasks Inspector" tool to display the tasks activity graphically.
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
with MaRTE.Kernel;

package MaRTE.Spy is

   procedure Initialize (Initital_Prio_Of_Main_Task : Kernel.Task_Priority);
   --  Initialize the tracer mechanism and send the initial trace data.
   pragma Inline (Initialize);

   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         T1        : in MaRTE.Kernel.Task_Id;
                         T2        : in MaRTE.Kernel.Task_Id);

   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         Id        : in Integer;
                         Prio      : in MaRTE.Kernel.Task_Priority);

   procedure Send_Event (Timestamp : in HAL.HWTime;
                         Operation : in String;
                         T         : in HAL.HWTime);

   pragma Inline (Send_Event);
end MaRTE.Spy;

