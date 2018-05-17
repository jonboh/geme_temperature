package body manejo_buffer is

   procedure guardar_dato (elem_buffer: tipo_generico; array_buffer: out t_array_buffer) is
   begin
      long_buffer:=long_buffer+1;
      array_buffer(long_buffer):=elem_buffer;

   end guardar_dato;

   procedure resetear_buffer is
   begin
      long_buffer:=0;
   end resetear_buffer;

begin

   null;

end manejo_buffer;
