------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                         'I O _ I n t e r f a c e'
--
--                                   Spec
--
--
--  File 'io_interface.ads'                                           By Mar.
--
--
--  Operations to access the PC I/O address space.
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
with System;

package MaRTE.HAL.IO is

   pragma Preelaborate;
   --  pragma Elaborate_Body;

   --  IO_Port
   type IO_Port is range 0 .. 16#FFFF#; -- I/O directions
   for  IO_Port'Size use 16;

   --  Read byte from port
   function Inb (Port : in IO_Port) return Unsigned_8;
   pragma Inline (Inb);

   --  Read byte from port with delay
   function Inb_P (Port : in IO_Port) return Unsigned_8;
   pragma Inline (Inb_P);

   --  Read word (2 bytes) from port
   function Inw (Port : in IO_Port) return Unsigned_16;
   pragma Inline (Inw);

   --  Read word (2 bytes) from port with delay
   function Inw_P (Port : in IO_Port) return Unsigned_16;
   pragma Inline (Inw_P);

   --  Read long word (4 bytes) from port
   function Inl (Port : in IO_Port) return Unsigned_32;
   pragma Inline (Inl);

   --  Read long word (4 bytes) from port with delay
   function Inl_P (Port : in IO_Port) return Unsigned_32;
   pragma Inline (Inl_P);

   --  Write in port
   procedure Outb (Port : in IO_Port; Val : in Unsigned_8);
   pragma Inline (Outb);

   --  Write in port with delay
   procedure Outb_P (Port : in IO_Port; Val : in Unsigned_8);
   pragma Inline (Outb_P);

   --  Write word (2 bytes) in port
   procedure Outw (Port : in IO_Port; Val : in Unsigned_16);
   pragma Inline (Outw);

   --  Write word (2 bytes) in port with delay
   procedure Outw_P (Port : in IO_Port; Val : in Unsigned_16);
   pragma Inline (Outw_P);

   --  Write long word (4 bytes) in port
   procedure Outl (Port : in IO_Port; Val : in Unsigned_32);
   pragma Inline (Outl);

   --  Write long word (4 bytes) in port with delay
   procedure Outl_P (Port : in IO_Port; Val : in Unsigned_32);
   pragma Inline (Outl_P);

   --  Read string of Count bytes in port
   procedure Insb
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insb);

   --  Read string of Count bytes in port with Delay
   procedure Insb_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insb_P);

   --  Read string of Count words in port
   procedure Insw
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insw);

   --  Read string of Count words in port with Delay
   procedure Insw_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insw_P);

   --  Read string of Count long words in port with Delay
   procedure Insl
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insl);

   --  Read string of Count long words in port with Delay
   procedure Insl_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Insl_P);

   --  Write string of Count bytes in port
   procedure Outsb
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsb);

   --  Write string of Count bytes in port with Delay
   procedure Outsb_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsb_P);

   --  Write string of Count words in port
   procedure Outsw
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsw);

   --  Write string of Count words in port
   procedure Outsw_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsw_P);

   --  Write string of Count long words in port
   procedure Outsl
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsl);

   --  Write string of Count long words in port with delay
   procedure Outsl_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32);
   pragma Inline (Outsl_P);

end MaRTE.HAL.IO;
