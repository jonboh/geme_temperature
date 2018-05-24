------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--    'K e r n e l . T a s k s _ O p e r a t i o n s . A t t r i b u t e s'
--
--                                 Spec
--
--
--  File 'k-to-attributes.ads'                                        By Mar.
--
--
--  Task attributes management.
--
--  The functions in this package are according with the POSIX.C functions
--  used to manage the thread creation attributes ('pthread_attr_t').
--  POSIX sections 16 "Threads" and 13.5 "Thread Scheduling Functions".
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

package MaRTE.Kernel.Tasks_Operations.Attributes is

   package K renames MaRTE.Kernel;

   function Pthread_Attr_Init (Attr : access Pthread_Attr_T) return Int;
   pragma Export (C, Pthread_Attr_Init, "pthread_attr_init");

   function Pthread_Attr_Destroy (Attr : access Pthread_Attr_T)
                                  return Int;
   pragma Export (C, Pthread_Attr_Destroy, "pthread_attr_destroy");

   function Pthread_Attr_Setdetachstate (Attr        : access Pthread_Attr_T;
                                         Detachstate :        Detach_State_T)
                                         return Int;
   pragma Export (C, Pthread_Attr_Setdetachstate,
                  "pthread_attr_setdetachstate");

   function Pthread_Attr_Getdetachstate (Attr        : access Pthread_Attr_T;
                                         Detachstate : access Int)
                                         return Int;
   pragma Export (C, Pthread_Attr_Getdetachstate,
                  "pthread_attr_getdetachstate");

   function Pthread_Attr_Setstacksize (Attr      : access Pthread_Attr_T;
                                       Stacksize : Size_T)
                                      return Int;
   pragma Export (C, Pthread_Attr_Setstacksize,
                  "pthread_attr_setstacksize");

   function Pthread_Attr_Getstacksize (Attr      : access Pthread_Attr_T;
                                       Stacksize : access Size_T)
                                       return Int;
   pragma Export (C, Pthread_Attr_Getstacksize,
                  "pthread_attr_getstacksize");

   function Pthread_Attr_Setinheritsched (Attr         : access Pthread_Attr_T;
                                          Inheritsched : Int)
                                          return Int;
   pragma Export (C, Pthread_Attr_Setinheritsched,
                  "pthread_attr_setinheritsched");

   function Pthread_Attr_Getinheritsched (Attr         : access Pthread_Attr_T;
                                          Inheritsched : access Int)
                                          return Int;
   pragma Export (C, Pthread_Attr_Getinheritsched,
                  "pthread_attr_getinheritsched");

   function Pthread_Attr_Setschedparam (Attr   : access Pthread_Attr_T;
                                        Param  : access Sched_Param)
                                        return Int;
   pragma Export (C, Pthread_Attr_Setschedparam,
              "pthread_attr_setschedparam");

   function Pthread_Attr_Getschedparam (Attr   : access Pthread_Attr_T;
                                        Param  : access Sched_Param)
                                        return Int;
   pragma Export (C, Pthread_Attr_Getschedparam,
                  "pthread_attr_getschedparam");

   function Pthread_Attr_Setschedpolicy (Attr   : access Pthread_Attr_T;
                                         Policy : Scheduling_Policies)
                                         return Int;
   pragma Export (C, Pthread_Attr_Setschedpolicy,
                  "pthread_attr_setschedpolicy");

   function Pthread_Attr_Getschedpolicy (Attr   : access Pthread_Attr_T;
                                         Policy : access Int)
                                         return Int;
   pragma Export (C, Pthread_Attr_Getschedpolicy,
                  "pthread_attr_getschedpolicy");

   --  SCHED_APP
   function Pthread_Attr_Setappschedulerstate
     (Attr     : access Pthread_Attr_T;
      Appsched : in     Regular_Or_App_Scheduler_T) return Int;
   pragma Export (C, Pthread_Attr_Setappschedulerstate,
                  "pthread_attr_setappschedulerstate");

   function Pthread_Attr_Getappschedulerstate
     (Attr     : access Pthread_Attr_T;
      Appsched : access Regular_Or_App_Scheduler_T) return Int;
   pragma Export (C, Pthread_Attr_Getappschedulerstate,
                  "pthread_attr_getappschedulerstate");

   function Pthread_Attr_Setappscheduler
     (Attr         : access Pthread_Attr_T;
      Appscheduler : in     Task_Id) return Int;
   pragma Export (C, Pthread_Attr_Setappscheduler,
                  "pthread_attr_setappscheduler");

   function Pthread_Attr_Getappscheduler
     (Attr         : access Pthread_Attr_T;
      Appscheduler : access Task_Id) return Int;
   pragma Export (C, Pthread_Attr_Getappscheduler,
                  "pthread_attr_getappscheduler");

   function Pthread_Attr_Setappschedparam
     (Attr                : access Pthread_Attr_T;
      AppSched_Param      : K.AppSched_Param_T_Ac;
      AppSched_Param_Size : K.AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Attr_Setappschedparam,
                  "pthread_attr_setappschedparam");

   function Pthread_Attr_Getappschedparam
     (Attr                : access Pthread_Attr_T;
      AppSched_Param      : K.AppSched_Param_T_Ac;
      AppSched_Param_Size : access K.AppSched_Param_Size_T) return Int;
   pragma Export (C, Pthread_Attr_Getappschedparam,
                  "pthread_attr_getappschedparam");

end MaRTE.Kernel.Tasks_Operations.Attributes;
