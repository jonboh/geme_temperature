package MaRTE.X86_Processor_Type is
   pragma Pure;
   type Processor_Type is (I386, PENTIUM_I, PENTIUM_II);
   for Processor_Type'Size use 32;
   Processor : constant Processor_Type := PENTIUM_II;
end MaRTE.X86_Processor_Type;
