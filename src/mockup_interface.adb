with Ada.Text_IO; 
with Ada.Float_Text_IO;
with PCM3712_mock;
with PCM3718_mock;


package body mockup_interface is 

    gain_wats_volt: constant Float := 80.0;
    type temp_array is array(0 .. 99) of Float;
    temps : temp_array;

    procedure heat(power: Float) is
    volts: Float;
    begin
        volts := power * gain_wats_volt;
        PCM3712_mock.Write_ao_PCM3712(volts);
    end heat;

    function read_temp return Float is
    temp: Float;
    begin
        temp := PCM3718_mock.Adquirir;
    return temp;
    end read_temp;

    procedure show_temp is
    begin
        Ada.Text_IO.Put("Temperatura: ");
        Ada.Float_Text_IO.Put(read_temp);
    end show_temp;

    procedure read_save_temp is
    begin
        null;
        -- buffer.add_data(read_temp)
    end read_save_temp;
    
    procedure show_array is
    length: Integer;
    begin
        Ada.Text_IO.Put_line("Array Temperatura: ");
        --length = buffer.len()
        --for i in 0..length loop
        --    Ada.Float_Text_IO.Put(buffer(i)); 
    end show_array;

    procedure show_avg_temp is
    length : Integer;
    begin
        --length := buffer.leb()
        null;
    end show_avg_temp;

    procedure close is
    begin
        PCM3718_mock.Fin_Adquisicion;
    end close;

begin
    --ARRAY TEMPERATURA
    PCM3712_mock.Initialize_PCM3712;
    --PCM3718_mock.Configuracion_Inicial()

end mockup_interface;
