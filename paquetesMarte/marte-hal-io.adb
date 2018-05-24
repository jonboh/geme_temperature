------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'I O _ I n t e r f a c e'
--
--                                 Body
--
--
--  File 'io_interface.adb'                                            By Mar.
--
--
--  Operations for accessing I/O PC space.
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

with System.Machine_Code; use System.Machine_Code;

package body MaRTE.HAL.IO is

   procedure IOdelay;
   procedure IOdelay is
   begin
      --  Using outb's to a nonexistent port seems to guarantee better
      --  timings even on fast machines. (Linux).

      Asm ("outb %%al,$0x80", No_Output_Operands, No_Input_Operands,
           Volatile => True);
   end IOdelay;
   pragma Inline (IOdelay);

   ---------
   -- Inb --
   ---------
   function Inb (Port : in IO_Port) return Unsigned_8 is
      Tmp : Unsigned_8;
   begin
      Asm ("inb %%dx, %0",
           Unsigned_8'Asm_Output ("=a", Tmp),
           IO_Port'Asm_Input  ("d",  Port),
           "",
           True);
      return Tmp;
   end Inb;
   pragma Inline (Inb);

   -----------
   -- Inb_P --
   -----------
   function Inb_P (Port : in IO_Port) return Unsigned_8 is
      Tmp : Unsigned_8;
   begin
      Tmp := Inb (Port);
      IOdelay;
      return Tmp;
   end Inb_P;
   pragma Inline (Inb_P);

   ---------
   -- Inw --
   ---------
   function Inw (Port : in IO_Port) return Unsigned_16 is
      Tmp : Unsigned_16;
   begin
      Asm ("inw %%dx, %0",
           Unsigned_16'Asm_Output ("=a", Tmp),
           IO_Port'Asm_Input  ("d",  Port),
           "",
           True);
      return Tmp;
   end Inw;
   pragma Inline (Inw);

   -----------
   -- Inw_P --
   -----------
   function Inw_P (Port : in IO_Port) return Unsigned_16 is
      Tmp : Unsigned_16;
   begin
      Tmp := Inw (Port);
      IOdelay;
      return Tmp;
   end Inw_P;
   pragma Inline (Inw_P);

   ---------
   -- Inl --
   ---------
   function Inl (Port : in IO_Port) return Unsigned_32 is
      Tmp : Unsigned_32;
   begin
      Asm ("inl %%dx, %0",
           Unsigned_32'Asm_Output ("=a", Tmp),
           IO_Port'Asm_Input  ("d",  Port),
           "",
           True);
      return Tmp;
   end Inl;
   pragma Inline (Inl);

   -----------
   -- Inl_P --
   -----------
   function Inl_P (Port : in IO_Port) return Unsigned_32 is
      Tmp : Unsigned_32;
   begin
      Tmp := Inl (Port);
      IOdelay;
      return Tmp;
   end Inl_P;
   pragma Inline (Inl_P);

   ----------
   -- Outb --
   ----------
   procedure Outb (Port : in IO_Port; Val : in Unsigned_8) is
   begin
      Asm ("outb %0, %%dx",
           No_Output_Operands,
           (Unsigned_8'Asm_Input ("a", Val),
            IO_Port'Asm_Input ("d", Port)),
           "", True);
   end Outb;
   pragma Inline (Outb);

   ------------
   -- Outb_P --
   ------------
   procedure Outb_P (Port : in IO_Port; Val : in Unsigned_8) is
   begin
      Outb (Port, Val);
      IOdelay;
   end Outb_P;
   pragma Inline (Outb_P);

   ----------
   -- Outw --
   ----------
   procedure Outw (Port : in IO_Port; Val : in Unsigned_16) is
   begin
      Asm (".byte 0x66; outl %0, %%dx",
           No_Output_Operands,
           (Unsigned_16'Asm_Input ("a", Val),
            IO_Port'Asm_Input ("d", Port)),
           "", True);
   end Outw;
   pragma Inline (Outw);

   ------------
   -- Outw_P --
   ------------
   procedure Outw_P (Port : in IO_Port; Val : in Unsigned_16) is
   begin
      Outw (Port, Val);
      IOdelay;
   end Outw_P;
   pragma Inline (Outw_P);

   ----------
   -- Outl --
   ----------
   procedure Outl (Port : in IO_Port; Val : in Unsigned_32) is
   begin
      Asm ("outl %0, %%dx",
           No_Output_Operands,
           (Unsigned_32'Asm_Input ("a", Val),
            IO_Port'Asm_Input ("d", Port)),
           "", True);
   end Outl;
   pragma Inline (Outl);

   ------------
   -- Outl_P --
   ------------
   procedure Outl_P (Port : in IO_Port; Val : in Unsigned_32) is
   begin
      Outl (Port, Val);
      IOdelay;
   end Outl_P;
   pragma Inline (Outl_P);

   ----------------------------------------
   -- String versions of IO instructions --
   ----------------------------------------
   --   Taken from Linux 2.4.28 (include/asm-i386/io.h):
   --   398 #define __INS(s) \
   --   399 static inline void ins##s(unsigned short port, void * addr,
   --                                                    unsigned long count) \
   --   400 { __asm__ __volatile__ ("rep ; ins" #s \
   --   401 : "=D" (addr), "=c" (count) : "d" (port),"" (addr),"1" (count)); }
   --   402
   --   403 #define __OUTS(s) \
   --   404 static inline void outs##s(unsigned short port, const void * addr,
   --                                                    unsigned long count) \
   --   405 { __asm__ __volatile__ ("rep ; outs" #s \
   --   406 : "=S" (addr), "=c" (count) : "d" (port),"" (addr),"1" (count)); }

   ----------
   -- Insb --
   ----------
   procedure Insb
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "insb",
           (System.Address'Asm_Output ("=D", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Insb;
   pragma Inline (Insb);

   ------------
   -- Insb_P --
   ------------
   procedure Insb_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Insb (Port, Buff, Count);
      IOdelay;
   end Insb_P;
   pragma Inline (Insb_P);

   ----------
   -- Insw --
   ----------
   procedure Insw
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "insw",
           (System.Address'Asm_Output ("=D", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Insw;
   pragma Inline (Insw);

   ------------
   -- Insw_P --
   ------------
   procedure Insw_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Insw (Port, Buff, Count);
      IOdelay;
   end Insw_P;
   pragma Inline (Insw_P);

   ----------
   -- Insl --
   ----------
   procedure Insl
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "insl",
           (System.Address'Asm_Output ("=D", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Insl;
   pragma Inline (Insl);

   ------------
   -- Insl_P --
   ------------
   procedure Insl_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Insl (Port, Buff, Count);
      IOdelay;
   end Insl_P;
   pragma Inline (Insl_P);

   -----------
   -- Outsb --
   -----------
   procedure Outsb
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "outsb",
           (System.Address'Asm_Output ("=S", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Outsb;
   pragma Inline (Outsb);

   -------------
   -- Outsb_P --
   -------------
   procedure Outsb_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Outsb (Port, Buff, Count);
      IOdelay;
   end Outsb_P;
   pragma Inline (Outsb_P);

   -----------
   -- Outsw --
   -----------
   procedure Outsw
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "outsw",
           (System.Address'Asm_Output ("=S", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Outsw;
   pragma Inline (Outsw);

   -------------
   -- Outsw_P --
   -------------
   procedure Outsw_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Outsw (Port, Buff, Count);
      IOdelay;
   end Outsw_P;
   pragma Inline (Outsw_P);

   -----------
   -- Outsl --
   -----------
   procedure Outsl
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Asm ("rep;" &
           "outsl",
           (System.Address'Asm_Output ("=S", Buff),
            Unsigned_32'Asm_Output ("=c", Count)),
           (IO_Port'Asm_Input ("d",  Port),
            System.Address'Asm_Input ("0", Buff),
            Unsigned_32'Asm_Input ("1", Count)),
           "",
           True);
   end Outsl;
   pragma Inline (Outsl);

   -------------
   -- Outsl_P --
   -------------
   procedure Outsl_P
      (Port  : in IO_Port;
       Buff  : in out System.Address;
       Count : in out Unsigned_32) is
   begin
      Outsl (Port, Buff, Count);
      IOdelay;
   end Outsl_P;
   pragma Inline (Outsl_P);

end MaRTE.HAL.IO;
