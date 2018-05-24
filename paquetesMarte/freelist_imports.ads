------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                              'Freelist'
--
--                                 Spec
--
--
--  File 'freelist.ads'                                         By Sangorrin
--
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

with Interfaces.C;

package Freelist_Imports is

    package IC renames Interfaces.C;

    type Freelist_Type is record
        Freelist   : access IC.Int;
        First_Free : IC.Int;
    end record;
    pragma Convention (C, Freelist_Type);

    -------------------
    -- Freelist_Init --
    -------------------
    --  Initialize a free list adding all the cells to the list of free cells
    --  The size of the list is specified by size. Returns 0 if successful, or
    --  -1 if not enough memory

    function Freelist_Init
                (List : access Freelist_Type;
                 Size : in IC.Int) return IC.Int;

    pragma Import (C, Freelist_Init, "freelist_init");

    --------------------
    -- Freelist_Alloc --
    --------------------
    --  Obtain the index of a free cell from the free list specified by list
    --  the cell is removed from the list
    --  Returns the index to the requested cell,
    --  or -1 if there are no free cells

    function Freelist_Alloc
                (List : access Freelist_Type) return IC.Int;

    pragma Import (C, Freelist_Alloc, "freelist_alloc");

    -------------------
    -- Freelist_Free --
    -------------------
    -- Deallocate the cell specified by index cell adding it to the list
    -- of free cells specified by list
    -- Returns 0 if successful, or -1 if the cell was already free

    function Freelist_Free
                (List : access Freelist_Type;
                 Cell : in IC.Int) return IC.Int;

    pragma Import (C, Freelist_Free, "freelist_free");

end Freelist_Imports;
