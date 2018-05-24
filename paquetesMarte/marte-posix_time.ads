------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                          'P O S I X _ T i m e'
--
--                                   Spec
--
--
--
--  File 'time.ads'                                                   By MAR.
--
--  This package is a part of the layer that implements the POSIX.C
--  funcionalty over MaRTE OS. In particular this package defines
--  the functions in the C header file 'time.h'.
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

with MaRTE.Integer_Types;
with MaRTE.Kernel;
with MaRTE.Kernel.Tasks_Operations;
with MaRTE.Kernel.Timers;
with MaRTE.Kernel.Signals;
with MaRTE.HAL;
pragma Elaborate_All ((MaRTE.HAL));
with MaRTE.Timespec;

package MaRTE.POSIX_Time is

   package K renames MaRTE.Kernel;

   ----------------------------------------------------------------------------
   -- Types 'Timeval' ---------------------------------------------------------
   ----------------------------------------------------------------------------
   type Timeval is record
      Tv_Sec  : MaRTE.Integer_Types.Int;
      Tv_Usec : MaRTE.Integer_Types.Int;
   end record;
   pragma Convention (C, Timeval);
   type Timezone is record
      Tz_Minuteswest : MaRTE.Integer_Types.Long; --  minutes W of Greenwich
      Tz_Dsttime     : MaRTE.Integer_Types.Long; --  type of dst correction
   end record;
   pragma Convention (C, Timezone);

   function To_Duration (TV : Timeval) return Duration;
   pragma Inline (To_Duration);

   function To_Timeval (D : Duration) return Timeval;
   pragma Inline (To_Timeval);

   ---------------------------------------------------------------------------
   -- Clocks and Timers ------------------------------------------------------
   ---------------------------------------------------------------------------
   subtype Clockid_T is K.Timers.Clock_Id;
   subtype Timer_T   is K.Timers.Timer_Id;

   CLOCK_REALTIME          : constant Clockid_T :=
     K.Timers.CLOCK_REALTIME;
   CLOCK_THREAD_CPUTIME_ID : constant Clockid_T :=
     K.Timers.CLOCK_THREAD_CPUTIME_ID;

   type Itimerspec is record
      It_Interval : MaRTE.Timespec.Timespec;
      It_Value    : MaRTE.Timespec.Timespec;
   end record;
   pragma Convention (C, Itimerspec);
   type Itimerspec_Ac is access all Itimerspec;
   type Itimerspec_Ac_Const is access constant Itimerspec;

   function Clock_Settime (Clock_Id : Clockid_T;
                           Tp       : MaRTE.Timespec.Timespec_Ac_Const)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Clock_Settime, "clock_settime");

   function Clock_Gettime (Clock_Id : Clockid_T;
                           Tp       : access MaRTE.Timespec.Timespec)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Clock_Gettime, "clock_gettime");

   function Clock_Getres (Clock_Id : Clockid_T;
                          Tp       : access MaRTE.Timespec.Timespec)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Clock_Getres, "clock_getres");

   function Pthread_Getcpuclockid (Thread_Id : K.Task_Id;
                                   Clock_Id  : access Clockid_T)
                                  return MaRTE.Integer_Types.Int
     renames K.Timers.Pthread_Getcpuclockid;
   type Signal_Event_Ac_Const is access constant K.Signals.Signal_Event;
   function Timer_Create (Clock_Id   : Clockid_T;
                          Evp        : Signal_Event_Ac_Const;
                          Timerid    : access Timer_T)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Timer_Create, "timer_create");

   function Timer_Delete (Timerid : Timer_T) return MaRTE.Integer_Types.Int
     renames K.Timers.Timer_Delete;
   function Timer_Settime (Timerid : Timer_T;
                           Flags   : K.Timers.Timer_Options;
                           Value   : Itimerspec_Ac_Const;
                           Ovalue  : Itimerspec_Ac)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Timer_Settime, "timer_settime");

   function Timer_Gettime (Timerid : Timer_T;
                           Value   : Itimerspec_Ac)
                          return MaRTE.Integer_Types.Int;
   pragma Export (C, Timer_Gettime, "timer_gettime");

   function Timer_Getoverrun (Timerid : Timer_T)
                             return MaRTE.Integer_Types.Int
     renames K.Timers.Timer_Getoverrun;
   ------------
   --  Time  --
   ------------
   --  'mkkernel' changes the name of this symbol to "time" in "x86"
   --  and "linux" archs. In "linux_lib" the name is not changed in
   --  order to use the standard function provided by Linux.
   type Int_Ac is access MaRTE.Integer_Types.Int;
   function Time (Tloc : in Int_Ac)
                 return MaRTE.Integer_Types.Int;
   pragma Export (C, Time, "marte_time");

   --------------------
   --  Gettimeofday  --
   --------------------
   --  'mkkernel' changes the name of this symbol to "gettimeofday" in
   --  "x86" and "linux" archs. In "linux_lib" the name is not changed
   --  in order to use the standard function provided by Linux.
   function Gettimeofday (Tv : access Timeval;
                          Tz : access Timezone)
                         return MaRTE.Integer_Types.Int;
   pragma Export (C, Gettimeofday, "marte_gettimeofday");

end MaRTE.POSIX_Time;
