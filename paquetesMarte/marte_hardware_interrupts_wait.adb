with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.Kernel.Hardware_Interrupts.Operations;

package body MaRTE_Hardware_Interrupts_Wait is
   package KHInt renames K.Hardware_Interrupts;

   procedure Wait_Associated_Interrupt is
      Intr         : aliased KHInt.Intr_T;
      Handler      : aliased KHInt.Interrupt_Handler_Function;
   begin
      if KHInt.Operations.Posix_Intr_Timedwait_HWTime
        (OS_Flags     => 0,
         Abs_Timespec => null,
         Abs_Timeout  => 0,
         Intr         => Intr'Access,
         Handler      => Handler'Access) /= 0 then
         raise NO_ASSOCIATED_INTERRUPTS;
      end if;
   end Wait_Associated_Interrupt;

end MaRTE_Hardware_Interrupts_Wait;
