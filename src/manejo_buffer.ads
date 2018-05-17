generic

   type tipo_generico is private;
   type t_array_buffer is array (Integer range <>) of tipo_generico;
package manejo_buffer is

   long_buffer:Natural:=0;
   procedure guardar_dato (elem_buffer: tipo_generico; array_buffer: out t_array_buffer);
   procedure resetear_buffer;
end manejo_buffer;
