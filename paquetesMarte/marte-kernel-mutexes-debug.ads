------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--               'K e r n e l . M u t e x e s . D e b u g'
--
--                                 Spec
--
--
--  File 'k-mutexes-debug.ads'                                         By MAR.
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

package MaRTE.Kernel.Mutexes.Debug is

   package K renames MaRTE.Kernel;

   procedure Initialize (M : access Mutex);
   procedure Finalize (M : access Mutex);

   procedure Running_Task_Locks_Mutex (M : in Mutex_Descriptor);

   procedure Task_Gets_Ready_Owning_Mutex (T : in Task_Id;
                                           M : in Mutex_Descriptor);

   procedure Enqueue_Blocked_Task_In_Mutex (T : in Task_Id;
                                            M : in Mutex_Descriptor);

   procedure Task_Unlocks_Mutex1 (T : in Task_Id;
                                  M : in Mutex_Descriptor);

   procedure Task_Unlocks_Mutex2;

   ------------------------------------------
   -- Application-defined protocol mutexes --
   ------------------------------------------
   procedure Running_Task_Locks_AppMutex (M : in Mutex_Descriptor);

   procedure Running_Task_Unlocks_AppMutex (M : in Mutex_Descriptor);

end MaRTE.Kernel.Mutexes.Debug;
