with Ada.Text_IO; use Ada.Text_IO;
with PCM3712_mock;
with PCM3718_mock;


package body mockup_interface is 

   type temp_array is array(0 .. 99) of Float;
    temps : temp_array;
    n_temps: Integer;

    procedure heat(temp: Float) is
    begin
        null;
    end heat;

    procedure read_temp is
    begin
        null;
    end read_temp;

    procedure show_temp is
    begin
        --retrieve_temp()
        put_line("Temperatura: ");
    end show_temp;

    procedure read_save_temp is
    begin
        null;
    end read_save_temp;
   
begin
    --ARRAY TEMPERATURA
    
   null;

end mockup_interface;
