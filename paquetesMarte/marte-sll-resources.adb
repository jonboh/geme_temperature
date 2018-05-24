------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--         'S i n g l y _ L i n k e d _ L i s t s . R e s o u r c e s'
--
--                                  Body
--
--
--  File 'sll-resources.adb'                                           by MAR.
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
with MaRTE.Configuration_Parameters;
--  with Unchecked_Deallocation;
package body MaRTE.SLL.Resources is

   Free_Resources : List := null;

   type Resource_Ac is access all Resource_T;

   function Malloc (Size : Integer) return Resource_Ac;
   pragma Import (C, Malloc, "malloc");
   procedure Free (E : Resource_Ac);
   pragma Import (C, Free, "free");

   --  type Dealloc_Element_Ac is access all Element;

   --  procedure Deallocate_Element is
   --     new Unchecked_Deallocation (Element, Dealloc_Element_Ac);

   -------------
   -- Release --
   -------------
   procedure Release (R : in Element_Ac) is
   begin
      pragma Assert (not R.Freed);
      R.Freed := True;
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         Enqueue_Head (R, Free_Resources);

      else  --  Resources allocated in dynamic memory
         Free (Resource_Ac (R));

      end if;
   end Release;

   -------------
   -- Request --
   -------------
   function Request return Element_Ac is
      Ret : Resource_Ac;
      procedure Memory_Copy (From  : in Resource_Ac;
                             To    : in Resource_Ac;
                             Bytes : in Integer);
      pragma Import (C, Memory_Copy, "memory_copy");
   begin
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         Ret := Resource_Ac (Free_Resources);
         if Ret = null then
            --  Resources exhausted
            return null;
         end if;

         Dequeue_Head (Free_Resources);

      else  --  Resources allocated in dynamic memory
         Ret := Malloc (Resource_T'Size);
         if Ret /= null then
            declare
               Foo : aliased Resource_T;
            begin
               --  To set properly the 'tag'
               Memory_Copy (From  => Foo'Unrestricted_Access,
                            To    => Ret,
                            Bytes => 4); --  I trust tag size is 4 bytes
            end;
         end if;
      end if;

      Element_Ac (Ret).Freed := False;
      return Element_Ac (Ret);
   end Request;

   --------------------
   -- Resources_Left --
   --------------------
   function Resources_Left return Boolean is
   begin
      if MaRTE.Configuration_Parameters.Preallocated_Resources'First then
         return Free_Resources /= null;

      else  --  Resources allocated in dynamic memory
         declare
            R : Resource_Ac;
         begin
            R := Malloc (Resource_T'Size);
            if R = null then
               return False;
            else
               Free (R);
               return True;
            end if;
         end;
      end if;
   end Resources_Left;

end MaRTE.SLL.Resources;
