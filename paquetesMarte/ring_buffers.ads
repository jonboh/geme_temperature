------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'R i n g _ B u f f e r s'
--
--                                   Spec
--
--
--  File 'ring_buffers.ads'                                       By MAR and
--                                                                   MGH.
--
--
--  Ring buffers intended to share data between the interrupt handler
--  and the driver functions (read and write).
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Timespec;
with MaRTE_Semaphores; use MaRTE_Semaphores;
with MaRTE.Kernel.File_System_Data_Types;

package Ring_Buffers is

   package FSDT renames MaRTE.Kernel.File_System_Data_Types;

   type Boolean_32 is new Boolean range False .. True;
   for Boolean_32'Size use MaRTE.Integer_Types.Int'Size;

   ----------------------------------------------------------------------------
   --  Input Buffers  ---------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Interrupt handler => puts
   --  User tasks        => get
   --
   --  'Get_From_Input_Buffer' blocks the calling task if there is no
   --  characters in buffer until a new character is put using
   --  'Put_In_Input_Buffer'.

   type Input_Buffer (Size_Mx : Positive) is private;

   -------------------------
   --  Init_Input_Buffer  --
   -------------------------
   procedure Init_Input_Buffer (Buf : in out Input_Buffer);
   pragma Export (C, Init_Input_Buffer, "init_input_buffer");
   pragma Export_Procedure (Init_Input_Buffer,
                              "init_input_buffer",
                              Mechanism => (Buf => Reference));

   -----------------------------
   --  Get_From_Input_Buffer  --
   -----------------------------
   --
   --  To be called from a 'read' driver function.
   procedure Get_From_Input_Buffer
     (Buf         : in out Input_Buffer;
      Blocking    : in     Boolean_32;
      C           : out    Unsigned_8;
      Empty       : out    Boolean_32;
      Rel_Timeout : in     MaRTE.Timespec.Timespec_Ac_Const;
      Timedout    : out    Boolean_32);
   pragma Export (C, Get_From_Input_Buffer, "get_from_input_buffer");
   pragma Export_Procedure (Get_From_Input_Buffer,
                              "get_from_input_buffer",
                              Mechanism => (Buf         => Reference,
                                            Blocking    => Value,
                                            C           => Reference,
                                            Empty       => Reference,
                                            Rel_Timeout => Value,
                                            Timedout    => Reference));
   pragma Inline (Get_From_Input_Buffer);

   ---------------------------
   --  Put_In_Input_Buffer  --
   ---------------------------
   --
   --  To be called from an interrupt handler
   procedure Put_In_Input_Buffer (Buf          : in out Input_Buffer;
                                  C            : in     Unsigned_8;
                                  Replace_Last : in     Boolean_32);
   pragma Export (C, Put_In_Input_Buffer, "put_in_input_buffer");
   pragma Export_Procedure (Put_In_Input_Buffer,
                              "put_in_input_buffer",
                              Mechanism => (Buf          => Reference,
                                            C            => Value,
                                            Replace_Last => Value));
   pragma Inline (Put_In_Input_Buffer);

   ------------------------------
   --  Read_From_Input_Buffer  --
   ------------------------------
   --
   --  To be called from a 'read' driver function.
   --
   --  This function shall attempt to read 'Nbytes' bytes from the
   --  ring buffer pointed to by 'Buf', into the buffer pointed to by
   --  'Buffer_Ptr'.
   --
   --  Correct behaviour for blocking and not blocking devices. POSIX
   --  says: When attempting to read a file (other than a pipe or
   --  FIFO) that supports non-blocking reads and has no data
   --  currently available:
   --
   --  * If O_NONBLOCK is set, read() shall return -1 and set errno to
   --  [EAGAIN].
   --
   --  * If O_NONBLOCK is clear, read() shall block the calling thread
   --  until some data becomes available.
   --
   --  * The use of the O_NONBLOCK flag has no effect if there is some
   --  data available.
   function Read_From_Input_Buffer
     (Buf         : access Input_Buffer;
      Buffer_Ptr  : in     FSDT.Buffer_Ac;
      Nbytes      : in     FSDT.Buffer_Length;
      Blocking    : in     Boolean_32;
      Rel_Timeout : in     MaRTE.Timespec.Timespec_Ac_Const)
     return Ssize_T;
   pragma Export (C, Read_From_Input_Buffer, "read_from_input_buffer");
   pragma Inline (Read_From_Input_Buffer);

   ----------------------------------------------------------------------------
   --  Output Buffers  --------------------------------------------------------
   ----------------------------------------------------------------------------
   --
   --  Interrupt handler => gets
   --  User tasks        => put
   --
   --  'Put_In_Output_Buffer' blocks the calling task if there is no
   --  free room in buffer until a character is get using
   --  'Get_From_Output_Buffer'.
   type Output_Buffer (Size_Mx : Positive) is private;

   --------------------------
   --  Init_Output_Buffer  --
   --------------------------
   procedure Init_Output_Buffer (Buf : in out Output_Buffer);
   pragma Export (C, Init_Output_Buffer, "init_output_buffer");
   pragma Export_Procedure (Init_Output_Buffer,
                              "init_output_buffer",
                              Mechanism => (Buf => Reference));

   ------------------------------
   --  Get_From_Output_Buffer  --
   ------------------------------
   --
   --  To be called from an interrupt handler
   procedure Get_From_Output_Buffer (Buf      : in out Output_Buffer;
                                     C        : out    Unsigned_8;
                                     Empty    : out    Boolean_32);
   pragma Export (C, Get_From_Output_Buffer, "get_from_output_buffer");
   pragma Export_Procedure (Get_From_Output_Buffer,
                              "get_from_output_buffer",
                              Mechanism => (Buf   => Reference,
                                            C     => Reference,
                                            Empty => Reference));
   pragma Inline (Get_From_Output_Buffer);

   type Send_First_Byte_Ac is access procedure (C : in Unsigned_8);

   ----------------------------
   --  Put_In_Output_Buffer  --
   ----------------------------
   --
   --  To be called from a 'write' driver function.
   procedure Put_In_Output_Buffer (Buf             : in out Output_Buffer;
                                   C               : in     Unsigned_8;
                                   Blocking        : in     Boolean_32;
                                   Send_First_Byte : in     Send_First_Byte_Ac;
                                   Full            : out    Boolean_32);
   pragma Export (C, Put_In_Output_Buffer, "put_in_output_buffer");
   pragma Export_Procedure (Put_In_Output_Buffer,
                              "put_in_output_buffer",
                              Mechanism => (Buf             => Reference,
                                            C               => Value,
                                            Blocking        => Value,
                                            Send_First_Byte => Value,
                                            Full            => Reference));
   pragma Inline (Put_In_Output_Buffer);

   ------------------------------
   --  Write_To_Output_Buffer  --
   ------------------------------
   --
   --  To be called from a 'write' driver function.
   --
   --  This function shall attempt to write 'Nbytes' bytes from the
   --  buffer pointed to by 'Buffer_Ptr' to the ring buffer pointed to
   --  by 'Buf'.
   --
   --  POSIX says: When attempting to write to a file descriptor
   --  (other than a pipe or FIFO) that supports non-blocking writes
   --  and cannot accept the data immediately:
   --
   --  - If the O_NONBLOCK flag is clear, write() shall block the
   --  calling thread until the data can be accepted.
   --
   --  - If the O_NONBLOCK flag is set, write() shall not block the
   --  thread. If some data can be written without blocking the
   --  thread, write() shall write what it can and return the number
   --  of bytes written. Otherwise, it shall return -1 and set errno
   --  to [EAGAIN].
   function Write_To_Output_Buffer (Buf        : access Output_Buffer;
                                    Buffer_Ptr : in     FSDT.Buffer_Ac;
                                    Nbytes     : in     FSDT.Buffer_Length;
                                    Blocking   : in     Boolean_32;
                                    Send_First_Byte : in Send_First_Byte_Ac)
                                   return Ssize_T;
   pragma Export (C, Write_To_Output_Buffer, "write_to_output_buffer");
   pragma Inline (Write_To_Output_Buffer);

   ------------------------------------
   --  Reset_Last_Interrupt_Pending  --
   ------------------------------------
   --
   procedure Reset_Last_Interrupt_Pending (Buf : access Output_Buffer);
   pragma Export (C, Reset_Last_Interrupt_Pending,
                    "reset_last_interrupt_pending");
   pragma Inline (Reset_Last_Interrupt_Pending);

private
   type Buffer_Array is array (Integer range <>) of Unsigned_8;

   type Buffer_Magic is (INPUT, OUTPUT);
   for Buffer_Magic use (INPUT => 16#81#, OUTPUT => 16#E7#);
   --  "Magic" values to perform checks on the buffer type (input or
   --  output). This is useful when the functions defined in this
   --  package are used from a C driver.

   type Ring_Buffer (Size_Mx : Positive) is record
      Buffer : Buffer_Array (0 .. Size_Mx);
      Head   : Natural;  --  First occupied position
      Tail   : Natural;  --  Last occupied position
      --  When empty and full Head=Tail
      Size   : Natural;  --  Number of elements in buffer
      Sem    : aliased MaRTE_Semaphores.Semaphore;
      Last_Interrupt_Pending : Boolean; -- Only for output buffers
      Magic  : Buffer_Magic;
   end record;

   type Input_Buffer (Size_Mx : Positive) is new Ring_Buffer (Size_Mx);
   type Output_Buffer (Size_Mx : Positive) is new Ring_Buffer (Size_Mx);

end Ring_Buffers;
