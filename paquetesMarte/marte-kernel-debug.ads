------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'K e r n e l . D e b u g'
--
--                                 Spec
--
--
--  File 'k-debug.ads'                                                 By MAR.
--
--
--  General debugging functions.
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
with GNAT.Source_Info;

package MaRTE.Kernel.Debug is

   package K renames MaRTE.Kernel;

   --------------------------------
   -- Short_Show_Task_On_Console --
   --------------------------------
   procedure Short_Show_Task_On_Console (T : in Tasks_Lists.Element_Ac);

   ------------------
   -- Show_In_Secs --
   ------------------
   type Relative_Absolute_T is (REL, ABSO);
   for Relative_Absolute_T'Size use Integer'Size;
   procedure Show_In_Secs (Hwt : in MaRTE.HAL.HWTime;
                           Ref : in Relative_Absolute_T := REL);

   -------------
   -- Task_OK --
   -------------
   function Task_OK (T : Task_Id) return Boolean renames K.Task_OK;
   function Task_OK (T           : Task_Id;
                     Extra_Magic : K.Magic_Check)
                     return Boolean renames K.Task_OK;
   function Task_OK (T : TCB) return Boolean renames K.Task_OK;

   --------------
   --  Assert  --
   --------------
   --  To be used instead of pragma assert:
   --  pragma Debug (Kernel.Debug.Assert (..., ...));
   procedure Assert
     (Condition : in Boolean;
      Msg       : in String := GNAT.Source_Info.Source_Location);

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize;

end MaRTE.Kernel.Debug;
