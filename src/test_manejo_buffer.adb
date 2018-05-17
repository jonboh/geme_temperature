with manejo_buffer;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
procedure test_manejo_buffer is

   i_opcion:Integer;

   type t_array_buffer is array (Integer range <>) of Float;
   package mi_manejo_buffer is new manejo_buffer(Float,t_array_buffer);

   elem_buffer: Float;
   array_buffer:t_array_buffer(1..25);
   long_buffer: Natural:=0;

   function generar_random_float return Float is
      G:Generator;
      f_valor_aleatorio_aux,f_valor_aleatorio: Float;
   begin
      Reset(G);
      f_valor_aleatorio_aux:=Random(G);
      f_valor_aleatorio:=f_valor_aleatorio_aux*5.0;
      return f_valor_aleatorio;
   end generar_random_float;


begin

   loop
      Put("elige lo que quieras hacer");Get(i_opcion);
      case i_opcion is
         when 1 =>
            elem_buffer:=generar_random_float;
            mi_manejo_buffer.guardar_dato (elem_buffer, array_buffer,long_buffer);

            for i in 1..long_buffer loop
               Put(array_buffer(i),0,0,0);New_Line;
            end loop;

            if long_buffer=25 then
               mi_manejo_buffer.resetear_buffer(long_buffer);
            end if;

         when 2 =>
            exit;
         when others =>
            null;
      end case;
   end loop;



end test_manejo_buffer;
