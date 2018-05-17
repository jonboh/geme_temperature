with Ada.Text_IO; 
with Ada.Float_Text_IO;
with mockup_control;
with manejo_buffer;

package body mockup_interface is 

    type temp_array_type is array(Integer range <>) of Float;
    temps : temp_array_type(0..24);
    package buffer_temp is new manejo_buffer(Float,temp_array_type);

    procedure show_temp is
    temp: Float;
    begin
        Ada.Text_IO.Put("Temperatura: ");
        temp := mockup_control.read_temp;
        Ada.Float_Text_IO.Put(temp);
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
        for i in 0..length loop
            Ada.Float_Text_IO.Put(temps(i)); 
        end loop;
    end show_array;

    procedure show_avg_temp is
    length : Integer;
    begin
        --length := buffer.leb()
        null;
    end show_avg_temp;
    
end mockup_interface;
