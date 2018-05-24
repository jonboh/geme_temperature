with Ada.Text_IO;
with Ada.Float_Text_IO;
with mockup_control;
with manejo_buffer;

package body mockup_interface is
    max_length_buffer: constant Integer:= 25;
    type temp_array_type is array(Integer range <>) of Float;
    temps : temp_array_type(0..max_length_buffer);
    package buffer_temp is new manejo_buffer(Float,temp_array_type);

    procedure show_temp is
    temp: Float;
    begin
        Ada.Text_IO.Put("Temperatura: ");
        temp := mockup_control.read_temp;
      Ada.Float_Text_IO.Put(temp,0,2,0);
    end show_temp;

    procedure read_save_temp is
      temp: Float;
    begin
      temp := mockup_control.read_temp;
      buffer_temp.guardar_dato(temp, temps);
    end read_save_temp;

    procedure show_array is
    length: Integer;
    begin
        Ada.Text_IO.Put_line("Array Temperatura: ");
        length := buffer_temp.long_buffer;
        for i in 1..length loop
            Ada.Float_Text_IO.Put(temps(i),0,2,0);Ada.Text_IO.New_Line;
        end loop;
    end show_array;

    procedure show_avg_temp is
      F_length,F_temp_aux,F_temp_media:Float;
      length:Integer;
   begin
      F_temp_aux:=0.0;
      length := buffer_temp.long_buffer;
      F_length:= Float(length);
      for i in 1..length loop
            F_temp_aux:=temps(i)+F_temp_aux;
      end loop;
      F_temp_media:=F_temp_aux/F_length;
      Ada.Text_IO.Put_line("Temperatura media: ");
      Ada.Float_Text_IO.Put(F_temp_media,0,2,0);Ada.Text_IO.New_Line;
        null;
   end show_avg_temp;

   procedure show_overlevel(F_umbral: in Float) is
      length:Integer;
      F_count,F_length,F_percen:Float;
   begin
      length:= buffer_temp.long_buffer;
      F_length:= Float(length);
      F_count:=0.0;
      Ada.Text_IO.Put_line("Las temperaturas que superan el umbral son: ");
      for i in 1..length loop
         if temps(i)>F_umbral then
            Ada.Text_IO.Put_line(" ");
            Ada.Float_Text_IO.Put(temps(i),0,2,0);
            F_count:=F_count+1.0;
         else
            null;
         end if;

      end loop;
      Ada.Text_IO.New_Line;
      F_percen:=(F_count*100.0)/F_length;
      Ada.Text_IO.Put_line("El porcentaje de temperaturas que superan el umbral es: ");
      Ada.Float_Text_IO.Put(F_percen,0,2,0);
      Ada.Text_IO.Put(" 0/0");
      Ada.Text_IO.New_Line;
      null;
      end show_overlevel;

begin
Ada.Text_IO.Put_line("Initialize Buffer");
buffer_temp.initialize(max_length_buffer);

end mockup_interface;
