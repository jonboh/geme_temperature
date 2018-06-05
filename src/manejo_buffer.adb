with Ada.Text_IO;

package body manejo_buffer is
    max_length: Integer:= -1;

    procedure Initialize(max_len:Integer) is
    begin
        max_length := max_len;
    end Initialize;


   procedure guardar_dato (elem_buffer: tipo_generico; array_buffer: out t_array_buffer) is
   begin
        if max_length=-1 then
            Ada.Text_IO.Put_Line("Buffer no inicializado!");
            raise Constraint_Error;
        end if;
        if long_buffer<max_length then
            long_buffer:=long_buffer+1;
        else
         Ada.Text_IO.Put_Line("Atencion! El buffer esta lleno!");
         Ada.Text_IO.Put_Line("Reseteando Buffer...");
         resetear_buffer;
        end if;
        array_buffer(long_buffer):=elem_buffer;

   end guardar_dato;

   procedure resetear_buffer is
   begin
      long_buffer:=0;
   end resetear_buffer;

begin

   null;

end manejo_buffer;
