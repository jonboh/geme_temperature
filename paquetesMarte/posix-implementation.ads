------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . I M P L E M E N T A T I O N                 --
--                                                                          --
--                                  S p e c                                 --
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
--  [$Revision: 849 $]
------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                 'P O S I X - I m p l e m e n t a t i o n'
--
--                                  Spec
--
--
--  File 'posix-implementation.ads'                                    By MAR.
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


--  with System.Interrupt_Management;
--  with Unchecked_Conversion;
with MaRTE.Kernel;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with Ada.Task_Identification;

package POSIX.Implementation is
   pragma Elaborate_Body;

   --  =========  --
   --   WARNING   --
   --  =========  --

   --  This package should NOT be used directly by an application.
   --  It is internal to the FLORIST implementation of the POSIX.5 API,
   --  and may be changed or replaced in future versions of FLORIST.

   --  The following is used in POSIX.Unsafe_Process_Primitives.Fork and
   --  POSIX.Process_Identification.Get_Process_ID to cache the ID of the
   --  current process.  It is present only because  Xavier Leroy's
   --  Linux threads do not conform to the POSIX C interface standard.
   --  In particular, they return different values from getpid()
   --  for each thread.

   ----------------------------------------------
   -- Conversions between 'MaRTE.Kernel.Task_Id' and --
   -- 'Ada.Task_Identification.Task_Id'        --
   ----------------------------------------------

   function To_Kernel_Task_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
        return MaRTE.Kernel.Task_Id;
   pragma Inline (To_Kernel_Task_Id);

   function To_Ada_Task_Id
     (Kernel_Task_Id : MaRTE.Kernel.Task_Id)
        return Ada.Task_Identification.Task_Id;
   pragma Inline (To_Ada_Task_Id);

   -------------------------
   --  Critical Sections  --
   -------------------------

   --  NEVER raise an exception within a critical section
   --  or abort-deferred section!
   --  Not even indirectly, by calling a subprogram
   --  that might raise an exception.
   --  Always exit the section, then raise the exception.

   --  ALWAYS enclose critical sections in a block with an
   --  exception handler that will call End_Critical_Section
   --  before allowing the exception to propagate, unless you
   --  can prove that no exception will be raised in the code.
   --  (How about Storage_Error, due to stack overflow?)

   --  Try to avoid nesting critical sections,
   --  as it means extra overhead.

   procedure Defer_Abortion;
   procedure Undefer_Abortion;

   --  The following two also defer/undefer abort, as side-effects.

   procedure Begin_Critical_Section;
   procedure End_Critical_Section;

   --------------
   --  Checks  --
   --------------

   --  Don't ever call any of these two within a critical section,
   --  or within an abort-deferred section!
   procedure Raise_POSIX_Error (Error : Error_Code := No_Error);
   pragma Inline (Raise_POSIX_Error);
   --  Calls to POSIX.Set_Error_Code
   procedure Raise_POSIX_Error_On_Error;
   pragma Inline (Raise_POSIX_Error_On_Error);

   procedure Check (Condition : Boolean; Error : Error_Code);
   --  if Condition is false, raise POSIX_Error with
   --  specified error code, else just return

   procedure Check (Result : Int);
   function Check (Result : Int) return Int;
   --  if Result is -1
   --  raise POSIX_Error with current error code
   --  else just return
   --  function returns Result if it does not raise POSIX_Error
   pragma Inline (Check);

   procedure Check_NNeg (Result : Int);
   function Check_NNeg (Result : Int) return Int;
   --  same as Check, except any negative value is treated
   --  as a failure

   procedure Check_NZ (Result : Int);
   pragma Inline (Check_NZ);
   --  same as Check, except any nonzero value is an error code

   procedure Check_NZ (Result : in Int; Flags : in Integer);
   --  To be used inside MaRTE Critical Sections
   --  ('MaRTE.Kernel.Leave_Critic_Section' is called before raise the exception).


--    --------------------
--    --  String Lists  --
--    --------------------

   type POSIX_String_Ptr is access all POSIX_String;


--    -------------------
--    --  Error Codes  --
--    -------------------

--    --  The following operate on the raw Pthread errno value,
--    --  and must be written in C since errno may be accessed via
--    --  a macro.

   function Fetch_Errno return Error_Code
     renames Get_Error_Code;


end POSIX.Implementation;
