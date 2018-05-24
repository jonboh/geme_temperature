------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--    'K e r n e l . M u t e x e s .  I n t e r n a l s _ A p p s c h e d'
--
--                                  Body
--
--
--  File 'k-mutexes-internals_appsched.adb'                           By MAR.
--
--
--  Mutexes related operations used only for the "application-defined
--  scheduling" functionality.
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

package body MaRTE.Kernel.Mutexes.Internals_Appsched is

   ---------------------------
   -- Task_Waiting_Response --
   ---------------------------
   function Task_Waiting_Response (M : in Mutex_Descriptor)
                                  return Task_Id is
   begin
      pragma Assert (M /= null and then M.Magic = NEW_APPSCHED,
                     "Task_Waiting_Response: invalid Mutex");
      return M.Task_Waiting_Appscheduler_Response;
   end Task_Waiting_Response;

   -------------------------
   -- To_Accept_Or_Reject --
   -------------------------
   --  Returns true when 'M' is a just "created" mutex waiting for its
   --  appscheduler acceptation or rejection
   function To_Accept_Or_Reject (M : in Mutex_Descriptor) return Boolean is
   begin
      pragma Assert
        (M /= null and then
         (M.Magic = INITIALIZED or
          (M.Magic = NEW_APPSCHED and then
           K.Task_OK (M.Task_Waiting_Appscheduler_Response))));
      return M /= null and then M.Magic = NEW_APPSCHED;
   end To_Accept_Or_Reject;

end MaRTE.Kernel.Mutexes.Internals_Appsched;
