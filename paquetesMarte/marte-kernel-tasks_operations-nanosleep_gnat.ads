------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--              'K e r n e l . T a s k s _ O p e r a t i o n s .
--                       N a n o s l e e p _ G n a t'
--
--                                 Spec
--
--
--  File 'k-to-nanosleep_gnat.ads'                                     By MAR.
--
--
--  Function 'nanosleep_gnat'. This function has the same functionality
--  than the POSIX 'nanosleep' function but with parameters of type 'access
--  Duration' instead of 'struct timespec *'.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat is

   package K renames MaRTE.Kernel;

   --------------------
   -- Nanosleep_Gnat --
   --------------------
   function Nanosleep_Gnat (Interval  : access Duration;
                            Remaining : access Duration) return Int;
   pragma Inline (Nanosleep_Gnat);
   --  Inside GNARL the type 'Duration' is used and the same happens
   --  in MaRTE OS. A more efficient implementation is got by using
   --  this function in the package 'System.OS_Interface' instead of
   --  the normal 'Nanosleep' declared below (avoid conversions from
   --  'Duration' to 'Timespec' and then from 'Timespec' to
   --  'Duration').

end MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
