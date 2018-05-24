------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                        'R i n g _ B u f f e r s'
--
--                                   Body
--
--
--  File 'ring_buffers.adb'                                        By MAR and
--                                                                    MGH.
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

with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.HAL;
with MaRTE.Kernel.Timers;
with MaRTE.Timespec;
pragma Elaborate_All (MaRTE.Timespec);
with MaRTE.Kernel.Semaphores.Operations;
with MaRTE.Integer_Types;

package body Ring_Buffers is

   package TOI  renames MaRTE.Kernel.Tasks_Operations.Internals;
   package HAL  renames MaRTE.HAL;
   use type MaRTE.Integer_Types.Int;

   ---------------------------------------------------------------------------
   --  Input Buffers  --------------------------------------------------------
   ---------------------------------------------------------------------------

   -------------------------
   --  Init_Input_Buffer  --
   -------------------------
   procedure Init_Input_Buffer (Buf : in out Input_Buffer) is
      Ret : MaRTE.Integer_Types.Int;
   begin
      Buf.Head  := 0;
      Buf.Tail  := Buf.Size_Mx - 1;
      Buf.Size  := 0;
      Buf.Magic := INPUT; --  Extra check for C drivers
      Ret := Marte_Semaphores.Initialize (Buf.Sem'Unrestricted_Access,
                                          Pshared => 0,
                                          Value   => 0);
      pragma Assert (Ret = 0);
   end Init_Input_Buffer;

   ---------------------------
   --  Put_In_Input_Buffer  --
   ---------------------------
   --
   --  To be called from an interrupt handler
   procedure Put_In_Input_Buffer (Buf          : in out Input_Buffer;
                                  C            : in     Unsigned_8;
                                  Replace_Last : in     Boolean_32) is
      Ret : MaRTE.Integer_Types.Int;
   begin
      pragma Assert (not HAL.Are_Interrupts_Enabled);
      pragma Assert (Buf.Magic = INPUT); --  Extra check for C drivers
      if Buf.Size < Buf.Size_Mx then
         Buf.Tail := (Buf.Tail + 1) mod Buf.Size_Mx;
         Buf.Size := Buf.Size + 1;
         Buf.Buffer (Buf.Tail) := C;
         Ret := MaRTE_Semaphores.Post (Buf.Sem'Unrestricted_Access);
         pragma Assert (Ret = 0);
      elsif Replace_Last then
         --  If buffer is full the last character is replaced.
         Buf.Buffer (Buf.Tail) := C;
      end if;
   end Put_In_Input_Buffer;
   pragma Inline (Put_In_Input_Buffer);

   -----------------------------
   --  Get_From_Input_Buffer  --
   -----------------------------
   --
   --  To be called from a 'read' driver function.
   Fake_Timespec : constant MaRTE.Timespec.Timespec :=
     MaRTE.Timespec.To_Timespec (0.0);

   procedure Get_From_Input_Buffer
     (Buf         : in out Input_Buffer;
      Blocking    : in     Boolean_32;
      C           : out    Unsigned_8;
      Empty       : out    Boolean_32;
      Rel_Timeout : in     MaRTE.Timespec.Timespec_Ac_Const;
      Timedout    : out    Boolean_32) is
      Ret : MaRTE.Integer_Types.Int;
      Flags : Integer;
      Abs_Timeout : HAL.HWTime;
      use type HAL.HWTime;
      use type MaRTE.Timespec.Timespec_Ac_Const;
   begin
      Timedout:=False;
      pragma Assert (Int'Size = Boolean_32'Size);
      pragma Assert (Buf.Magic = INPUT); --  Extra check for C drivers
      if Blocking then
         if Rel_Timeout=null then
            Ret := MaRTE_Semaphores.Wait (Buf.Sem'Unrestricted_Access);
            pragma Assert (Ret = 0);
         else
            --  wait with timeout
            Abs_Timeout :=
              MaRTE.Kernel.Timers.Get_Time (K.Timers.Clock_Realtime) +
              MaRTE.Timespec.Timespec_To_HWTime(Rel_Timeout.all);
            Ret := K.Semaphores.Operations.Sem_Timedwait_HWTime
              (Buf.Sem'Unrestricted_Access, Abs_Timeout, Fake_Timespec);
            if Ret=-1 and then
              TOI.Get_Last_POSIX_Error_Code = K.TIMED_OUT
            then
               Empty:=True;
               Timedout:=True;
               return;
            end if;
            pragma Assert (Ret = 0);
         end if;
      else  --  not Blocking Mode
         if MaRTE_Semaphores.Trywait (Buf.Sem'Unrestricted_Access) /= 0 then
            pragma Assert (TOI.Get_Last_POSIX_Error_Code =
                             K.RESOURCE_TEMPORARILY_UNAVAILABLE);
            Empty := True;
            return;
         end if;
      end if;
      pragma Assert (Buf.Size > 0);

      --  Get data and update buffer
      HAL.Save_Flags_And_Disable_Interrupts (Flags);
      C := Buf.Buffer (Buf.Head);
      Buf.Head := (Buf.Head + 1) mod Buf.Size_Mx;
      Buf.Size := Buf.Size - 1;
      HAL.Restore_Flags (Flags);
      Empty := False;
   end Get_From_Input_Buffer;
   pragma Inline (Get_From_Input_Buffer);

   ------------------------------
   --  Read_From_Input_Buffer  --
   ------------------------------
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
     return Ssize_T is
      use type FSDT.Buffer_Length;
      Count : FSDT.Buffer_Length := 0;
      C : MaRTE.Integer_Types.Unsigned_8;
      Empty, Timedout : Boolean_32;
   begin
      pragma Assert (Buf.Magic = INPUT); --  Extra check for C drivers
      if Nbytes = 0 then
         return 0;
      end if;

      --  Get first byte
      Get_From_Input_Buffer
        (Buf.all, Blocking, C, Empty, Rel_Timeout, Timedout);
      pragma Assert (Boolean(not (Empty and Blocking)));
      if Empty then
         if Timedout then
            --  A timeout occurred
            TOI.Set_POSIX_Error
              (K.TIMED_OUT);
            return -1;
         else
            --  Not blocking mode and no data available
            TOI.Set_POSIX_Error
              (K.RESOURCE_TEMPORARILY_UNAVAILABLE);
            return -1;
         end if;
      end if;

      --  Get following bytes
      loop
         Count := Count + 1;
         Buffer_Ptr.all (Count) := C;
         exit when Count = Nbytes;
         Get_From_Input_Buffer (Buf.all, False, -- Not blocking mode
                                C, Empty, Rel_Timeout, Timedout);
         exit when Boolean(Empty);
      end loop;
      TOI.Reset_POSIX_Error;
      return Ssize_T (Count);
   end Read_From_Input_Buffer;


   ---------------------------------------------------------------------------
   --  Output Buffers  -------------------------------------------------------
   ---------------------------------------------------------------------------

   --------------------------
   --  Init_Output_Buffer  --
   --------------------------
   procedure Init_Output_Buffer (Buf : in out Output_Buffer) is
      Ret : MaRTE.Integer_Types.Int;
   begin
      Buf.Head  := 0;
      Buf.Tail  := Buf.Size_Mx - 1;
      Buf.Size  := 0;
      Buf.Last_Interrupt_Pending := False;
      Buf.Magic := OUTPUT; --  Extra check for C drivers
      --  Semaphore in an output buffer starts with a value equal to
      --  'Buf.Size_Mx' and it is posted in get and waited in put.
      Ret :=
        Marte_Semaphores.Initialize (Buf.Sem'Unrestricted_Access,
                                     Pshared => 0,
                                     Value   => Semaphore_Value (Buf.Size_Mx));
      pragma Assert (Ret = 0);
   end Init_Output_Buffer;


   ------------------------------
   --  Get_From_Output_Buffer  --
   ------------------------------
   --
   --  To be called from an interrupt handler
   procedure Get_From_Output_Buffer (Buf      : in out Output_Buffer;
                                     C        : out    Unsigned_8;
                                     Empty    : out    Boolean_32) is
      Ret : MaRTE.Integer_Types.Int;
   begin
      pragma Assert (not HAL.Are_Interrupts_Enabled);
      pragma Assert (Buf.Magic = OUTPUT); --  Extra check for C drivers

      if Buf.Size > 0 then
         --  There is data in buffer
         Empty := False;

         --  Get data and update buffer
         C := Buf.Buffer (Buf.Head);
         Buf.Head := (Buf.Head + 1) mod Buf.Size_Mx;
         Buf.Size := Buf.Size - 1;
         --  Post semaphore
         Ret := MaRTE_Semaphores.Post (Buf.Sem'Unrestricted_Access);
         pragma Assert (Ret = 0);
      else
         --  Buffer empty
         Empty := True;
      end if;
   end Get_From_Output_Buffer;
   pragma Inline (Get_From_Output_Buffer);

   ----------------------------
   --  Put_In_Output_Buffer  --
   ----------------------------
   --
   --  To be called from a 'write' driver function.
   procedure Put_In_Output_Buffer (Buf             : in out Output_Buffer;
                                   C               : in     Unsigned_8;
                                   Blocking        : in     Boolean_32;
                                   Send_First_Byte : in     Send_First_Byte_Ac;
                                   Full            : out    Boolean_32) is
      Ret : MaRTE.Integer_Types.Int;
      Flags : Integer;
   begin
      pragma Assert (Buf.Magic = OUTPUT); --  Extra check for C drivers
      if Blocking then
         Ret := MaRTE_Semaphores.Wait (Buf.Sem'Unrestricted_Access);
         pragma Assert (Ret = 0);
      else  --  not Blocking Mode
         if MaRTE_Semaphores.Trywait (Buf.Sem'Unrestricted_Access) /= 0 then
            pragma Assert (TOI.Get_Last_POSIX_Error_Code =
                             K.RESOURCE_TEMPORARILY_UNAVAILABLE);
            Full := True;
            return;
         end if;
      end if;
      pragma Assert (Buf.Size < Buf.Size_Mx);

      --  Put data and update buffer
      HAL.Save_Flags_And_Disable_Interrupts (Flags);
      if (Buf.Size = 0 and not Buf.Last_Interrupt_Pending and
          Send_First_Byte /= null) then
         Send_First_Byte.all (C);
         Buf.Last_Interrupt_Pending := True;
         --  Post semaphore since the byte hasn't been stored in the
         --  buffer.
         Ret := MaRTE_Semaphores.Post (Buf.Sem'Unrestricted_Access);
         pragma Assert (Ret = 0);
      else
         Buf.Tail := (Buf.Tail + 1) mod Buf.Size_Mx;
         Buf.Size := Buf.Size + 1;
         Buf.Buffer (Buf.Tail) := C;
      end if;
      HAL.Restore_Flags (Flags);
      Full := False;
   end Put_In_Output_Buffer;
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
                                   return Ssize_T is
      use type FSDT.Buffer_Length;
      Count : FSDT.Buffer_Length := 1;
      Full : Boolean_32;
   begin
      pragma Assert (Buf.Magic = OUTPUT); --  Extra check for C drivers
      if Nbytes = 0 then
         return 0;
      end if;

      --  Write first byte
      Put_In_Output_Buffer (Buf.all, Buffer_Ptr.all (1),
                            Blocking, Send_First_Byte, Full);
      pragma Assert (not Boolean (Full and Blocking));
      if Full then
         --  Not blocking mode and no space in buffer
         TOI.Set_POSIX_Error
           (K.RESOURCE_TEMPORARILY_UNAVAILABLE);
         return -1;
      end if;

      --  Write following bytes
      loop
         exit when Count = Nbytes;
         Put_In_Output_Buffer (Buf.all, Buffer_Ptr.all (Count+1),
                               False, -- Not blocking mode
                               Send_First_Byte, Full);
         exit when Boolean(Full);
         Count := Count + 1;
      end loop;
      TOI.Reset_POSIX_Error;
      return Ssize_T (Count);
   end Write_To_Output_Buffer;

   ------------------------------------
   --  Reset_Last_Interrupt_Pending  --
   ------------------------------------
   --
   procedure Reset_Last_Interrupt_Pending (Buf : access Output_Buffer) is
   begin
      pragma Assert (not HAL.Are_Interrupts_Enabled);
      pragma Assert (Buf.Magic = OUTPUT); --  Extra check for C drivers
      Buf.Last_Interrupt_Pending := False;
   end Reset_Last_Interrupt_Pending;

end Ring_Buffers;
