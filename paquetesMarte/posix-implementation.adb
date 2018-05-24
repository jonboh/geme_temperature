------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . I M P L E M E N T A T I O N                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996-1999 Florida State University (FSU).     All Rights  --
--  Reserved.                                                               --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision: 1292 $]
------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                 'P O S I X - I m p l e m e n t a t i o n'
--
--                                  Body
--
--
--  File 'posix-implementation.adb'                                    By MAR.
--
--
--  This file is based on the Florist implementation.
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

with Ada.Exceptions;
--  with POSIX.C;
--  with POSIX.Error_Codes;
--  with System.Interrupt_Management;
--  with System.Interrupt_Management.Operations;
with GNAT.Task_Lock;
with System.Soft_Links;
with System.Tasking;
with System.OS_Interface;
with System.Task_Primitives.Operations;
with Unchecked_Conversion;
with MaRTE.Kernel;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body POSIX.Implementation is

   -- use POSIX.C;

   package TOI renames MaRTE.Kernel.Tasks_Operations.Internals;

   use type Error_Code;


   --  package SIM renames System.Interrupt_Management;
   --  package SIMO renames System.Interrupt_Management.Operations;

   package System_OSI renames System.OS_Interface;
   package System_TPO renames System.Task_Primitives.Operations;


   ----------------------------------------------
   -- Conversions between 'MaRTE.Kernel.Task_Id' and --
   -- 'Ada.Task_Identification.Task_Id'        --
   ----------------------------------------------

   function To_Kernel_Task_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
        return MaRTE.Kernel.Task_Id is

      function Pthread_T_To_MaRTE_Task_Id is new Unchecked_Conversion
        (System_OSI.pthread_t, MaRTE.Kernel.Task_Id);
      function Convert_Ids is new Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_ID);
   begin
      return
        Pthread_T_To_MaRTE_Task_Id
        (System_TPO.Get_Thread_Id (Convert_Ids (Ada_Task_Id)));
   end To_Kernel_Task_Id;

   function To_Ada_Task_Id
     (Kernel_Task_Id : MaRTE.Kernel.Task_Id)
      return Ada.Task_Identification.Task_Id is

      function Convert_Ids is new Unchecked_Conversion
        (System.Tasking.Task_ID, Ada.Task_Identification.Task_Id);
      function To_Pthread is new Unchecked_Conversion
        (MaRTE.Kernel.Task_Id, System_OSI.pthread_t);

   begin
      return Convert_Ids (System_TPO.Pthread_T_To_Task_Id
                                (To_Pthread(Kernel_Task_Id)));
   end To_Ada_Task_Id;




   --  .... It would be nice if we had a way to check whether we
   --  are in a critical section, at the points (below) where we are
   --  about to raise an exception.  These routines should never be
   --  called from inside a critical section, but that is an easy
   --  mistake to make.

   ------------------------------
   --  Begin_Critical_Section  --
   ------------------------------

   procedure Begin_Critical_Section is
   begin
      GNAT.Task_Lock.Lock;
   end Begin_Critical_Section;

   ----------------------------
   --  End_Critical_Section  --
   ----------------------------

   procedure End_Critical_Section is
   begin
      GNAT.Task_Lock.Unlock;
   end End_Critical_Section;


   ----------------------
   --  Defer_Abortion  --
   ----------------------

   procedure Defer_Abortion is
   begin
      System.Soft_Links.Abort_Defer.all;
   end Defer_Abortion;


   ------------------------
   --  Undefer_Abortion  --
   ------------------------

   procedure Undefer_Abortion is
   begin
      System.Soft_Links.Abort_Undefer.all;
   end Undefer_Abortion;

   -------------------------
   --  Raise_POSIX_Error  --
   -------------------------

   procedure Raise_POSIX_Error (Error : Error_Code := No_Error) is
      Tmp : Error_Code := Error;
   begin
      --  .... see note on critical sections above
      if Error = No_Error then Tmp := Fetch_Errno; end if;
      POSIX.Set_Error_Code (Tmp);
      Ada.Exceptions.Raise_Exception
        (POSIX_Error'Identity, Image (Tmp));
   end Raise_POSIX_Error;

   procedure Raise_POSIX_Error_On_Error is
   begin
      if TOI.There_Has_Been_A_POSIX_Error then
         declare
         begin
            Ada.Exceptions.Raise_Exception
              (POSIX_Error'Identity, Image (POSIX.Get_Error_Code));
         end;
      end if;
   end Raise_POSIX_Error_On_Error;

   -------------
   --  Check  --
   -------------

   procedure Check (Condition : Boolean; Error : Error_Code) is
   begin
      --  .... see note on critical sections above
      if not Condition then Raise_POSIX_Error (Error);
      end if;
   end Check;

   procedure Check (Result : int) is
   begin
      --  .... see note on critical sections above
      if Result = -1 then Raise_POSIX_Error (Fetch_Errno);
      end if;
   end Check;

   function Check (Result : int) return int is
   begin
      --  .... see note on critical sections above
      if Result = -1 then Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Result;
   end Check;

   --  ....is there a better work-around????
   --  Provenzano's threads seem to
   --  return nonstandard negative values for some calls,
   --  like "close".

   procedure Check_NNeg (Result : int) is
   begin
      --  .... see note on critical sections above
      if Result < 0 then Raise_POSIX_Error (Fetch_Errno);
      end if;
   end Check_NNeg;

   --  ....is there a better work-around????
   --  Provenzano's threads seem to
   --  return nonstandard negative values for some calls,
   --  like "close".

   function Check_NNeg (Result : int) return int is
   begin
      --  .... see note on critical sections above.
      if Result < 0 then Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Result;
   end Check_NNeg;

   procedure Check_NZ (Result : int) is
   begin
      --  .... see note on critical sections above.
      if Result /= 0 then Raise_POSIX_Error (Error_Code (Result));
      end if;
   end Check_NZ;

   procedure Check_NZ (Result : in int; Flags : in Integer) is
   begin
      if Result /= 0 then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
         Raise_POSIX_Error (Error_Code (Result));
      end if;
   end Check_NZ;

end POSIX.Implementation;
