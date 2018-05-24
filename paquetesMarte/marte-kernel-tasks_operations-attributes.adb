------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--    'K e r n e l . T a s k s _ O p e r a t i o n s . A t t r i b u t e s'
--
--                                 Body
--
--
--  File 'k-to-attributes.adb'                                        By MAR.
--
--
--  Task attributes management.
--
--  The functions in this package are according with the POSIX.C functions
--  used to manage the thread creation attributes ('pthread_attr_t').
--  POSIX section 16 "Threads" and 13.5 "Thread Scheduling Functions".
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
with MaRTE.Kernel.Tasks_Operations.Internals;

package body MaRTE.Kernel.Tasks_Operations.Attributes is

   package TOI  renames K.Tasks_Operations.Internals;

   use type Int;

   -------------------------------------
   --  Types for comparison with null --
   -------------------------------------
   type Sched_Param_Ac is access all Sched_Param;
   type Int_Ac is access all Int;
   type Regular_Or_App_Scheduler_Ac is access all Regular_Or_App_Scheduler_T;
   type Task_Id_Ac is access all Task_Id;
   type AppSched_Param_Size_Ac is access all K.AppSched_Param_Size_T;

   -----------------------
   -- Pthread_Attr_Init --
   -----------------------
   function Pthread_Attr_Init (Attr : access Pthread_Attr_T) return Int is
   begin
      pragma Assert (Pthread_Attr_T_Ac_Const (Attr) /= null);

      Attr.all := Attr_Deffault;
      return 0;
   end Pthread_Attr_Init;

   --------------------------
   -- Pthread_Attr_Destroy --
   --------------------------
   function Pthread_Attr_Destroy (Attr : access Pthread_Attr_T) return Int is
   begin
      pragma Assert (Pthread_Attr_T_Ac_Const (Attr) /= null);

      Attr.Magic := ATTR_NOT_INITIALIZED;
      return 0;
   end Pthread_Attr_Destroy;

   ---------------------------------
   -- Pthread_Attr_Setdetachstate --
   ---------------------------------
   function Pthread_Attr_Setdetachstate (Attr        : access Pthread_Attr_T;
                                         Detachstate :        Detach_State_T)
                                         return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Attr.Detach_State := Detachstate;
      return 0;
   end Pthread_Attr_Setdetachstate;

   ---------------------------------
   -- Pthread_Attr_Getdetachstate --
   ---------------------------------
   function Pthread_Attr_Getdetachstate (Attr        : access Pthread_Attr_T;
                                         Detachstate : access Int)
                                         return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Int_Ac (Detachstate) /= null);

      Detachstate.all := Attr.Detach_State;
      return 0;
   end Pthread_Attr_Getdetachstate;

   ---------------------------------
   --  Pthread_Attr_Setstacksize  --
   ---------------------------------
   function Pthread_Attr_Setstacksize (Attr      : access Pthread_Attr_T;
                                       Stacksize : Size_T)
                                      return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Attr.Stack_Size := Stacksize;
      return 0;
   end Pthread_Attr_Setstacksize;

   ---------------------------------
   --  Pthread_Attr_Getstacksize  --
   ---------------------------------
   function Pthread_Attr_Getstacksize (Attr      : access Pthread_Attr_T;
                                       Stacksize : access Size_T)
                                       return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));

      Stacksize.all := Attr.Stack_Size;
      return 0;
   end Pthread_Attr_Getstacksize;

   ----------------------------------
   -- Pthread_Attr_Setinheritsched --
   ----------------------------------
   function Pthread_Attr_Setinheritsched (Attr         : access Pthread_Attr_T;
                                          Inheritsched : Int)
                                          return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));
      if (Inheritsched /= Inherit_Or_Explicit_Sched'First and
          Inheritsched /= Inherit_Or_Explicit_Sched'Last) then
         return INVALID_ARGUMENT;
      end if;
      Attr.Inherit_Sched := Inheritsched;
      return 0;
   end Pthread_Attr_Setinheritsched;

   ----------------------------------
   -- Pthread_Attr_Getinheritsched --
   ----------------------------------
   function Pthread_Attr_Getinheritsched (Attr         : access Pthread_Attr_T;
                                          Inheritsched : access Int)
                                          return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Int_Ac (Inheritsched) /= null);
      Inheritsched.all := Attr.Inherit_Sched;
      return 0;
   end Pthread_Attr_Getinheritsched;

   --------------------------------
   -- Pthread_Attr_Setschedparam --
   --------------------------------
   function Pthread_Attr_Setschedparam (Attr   : access Pthread_Attr_T;
                                        Param  : access Sched_Param)
                                       return Int is
      Error : K.Error_Code;
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Sched_Param_Ac (Param) /= null);
      Error :=  Error_In_Scheduling_Parameters (Param.all, Attr.Sched_Policy);
      if Error /= NO_ERROR then
         return Error;
      end if;

      Attr.Param := Param.all;
      return 0;
   end Pthread_Attr_Setschedparam;

   --------------------------------
   -- Pthread_Attr_Getschedparam --
   --------------------------------
   function Pthread_Attr_Getschedparam (Attr   : access Pthread_Attr_T;
                                        Param  : access Sched_Param)
                                        return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Sched_Param_Ac (Param) /= null);

      Param.all := Attr.Param;
      return 0;
   end Pthread_Attr_Getschedparam;

   ---------------------------------
   -- Pthread_Attr_Setschedpolicy --
   ---------------------------------
   function Pthread_Attr_Setschedpolicy (Attr   : access Pthread_Attr_T;
                                         Policy : in Scheduling_Policies)
                                         return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));
      --  Error handling
      if (Policy < Scheduling_Policies'First or
          Policy > Scheduling_Policies'Last) then
         return INVALID_ARGUMENT;
      end if;

      Attr.Sched_Policy := Policy;
      return 0;
   end Pthread_Attr_Setschedpolicy;

   ---------------------------------
   -- Pthread_Attr_Getschedpolicy --
   ---------------------------------
   function Pthread_Attr_Getschedpolicy (Attr   : access Pthread_Attr_T;
                                         Policy : access Int)
                                         return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Int_Ac (Policy) /= null);

      Policy.all := Attr.Sched_Policy;
      return 0;
   end Pthread_Attr_Getschedpolicy;

   ----------------------------------------------------------------------------
   -- Application-Defined Scheduling ------------------------------------------
   ----------------------------------------------------------------------------

   ---------------------------------------
   -- Pthread_Attr_Setappschedulerstate --
   ---------------------------------------
   function Pthread_Attr_Setappschedulerstate
     (Attr     : access Pthread_Attr_T;
      Appsched : in     Regular_Or_App_Scheduler_T) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));
      --  Error handling
      if (Appsched < Regular_Or_App_Scheduler_T'First or
          Appsched > Regular_Or_App_Scheduler_T'Last) then
         return INVALID_ARGUMENT;
      end if;

      Attr.App_Scheduler_State := Appsched;
      return 0;
   end Pthread_Attr_Setappschedulerstate;

   ---------------------------------------
   -- Pthread_Attr_Getappschedulerstate --
   ---------------------------------------
   function Pthread_Attr_Getappschedulerstate
     (Attr     : access Pthread_Attr_T;
      Appsched : access Regular_Or_App_Scheduler_T) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Regular_Or_App_Scheduler_Ac (Appsched) /= null);

      Appsched.all := Attr.App_Scheduler_State;
      return 0;
   end Pthread_Attr_Getappschedulerstate;

   ----------------------------------
   -- Pthread_Attr_Setappscheduler --
   ----------------------------------
   function Pthread_Attr_Setappscheduler
     (Attr         : access Pthread_Attr_T;
      Appscheduler : in     Task_Id) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));
      --  Error handling
      if (not Task_OK (Appscheduler) or else
          Appscheduler.AppScheduler = null) then
         --  Invalid scheduler task
         return INVALID_ARGUMENT;
      end if;

      Attr.App_Scheduler := Appscheduler;
      return 0;
   end Pthread_Attr_Setappscheduler;

   ----------------------------------
   -- Pthread_Attr_Getappscheduler --
   ----------------------------------
   function Pthread_Attr_Getappscheduler
     (Attr         : access Pthread_Attr_T;
      Appscheduler : access Task_Id) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     Task_Id_Ac (Appscheduler) /= null);

      Appscheduler.all := Attr.App_Scheduler;
      return 0;
   end Pthread_Attr_Getappscheduler;

   -----------------------------------
   -- Pthread_Attr_Setappschedparam --
   -----------------------------------
   function Pthread_Attr_Setappschedparam
     (Attr                : access Pthread_Attr_T;
      AppSched_Param      : K.AppSched_Param_T_Ac;
      AppSched_Param_Size : K.AppSched_Param_Size_T) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)));
      --  Error handling
      if (AppSched_Param_Size < AppSched_Param_Size_T'First or else
          AppSched_Param_Size_T'Last < AppSched_Param_Size  or else
          (AppSched_Param_Size /= 0 and AppSched_Param = null))  then
         return INVALID_ARGUMENT;
      end if;

      if AppSched_Param_Size /= 0 then
         Attr.AppSched_Param (1 .. AppSched_Param_Size) :=
           AppSched_Param (1 .. AppSched_Param_Size);
      end if;
      Attr.AppSched_Param_Size := AppSched_Param_Size;
      return 0;
   end Pthread_Attr_Setappschedparam;

   -----------------------------------
   -- Pthread_Attr_Getappschedparam --
   -----------------------------------
   function Pthread_Attr_Getappschedparam
     (Attr                : access Pthread_Attr_T;
      AppSched_Param      : K.AppSched_Param_T_Ac;
      AppSched_Param_Size : access K.AppSched_Param_Size_T) return Int is
   begin
      pragma Assert (TOI.Attr_OK (Pthread_Attr_T_Ac_Const (Attr)) and
                     AppSched_Param /= null and
                     AppSched_Param_Size_Ac (AppSched_Param_Size) /= null);

      AppSched_Param (1 .. Attr.AppSched_Param_Size) :=
        Attr.AppSched_Param (1 .. Attr.AppSched_Param_Size);
      AppSched_Param_Size.all := Attr.AppSched_Param_Size;
      return 0;
   end Pthread_Attr_Getappschedparam;

end MaRTE.Kernel.Tasks_Operations.Attributes;
