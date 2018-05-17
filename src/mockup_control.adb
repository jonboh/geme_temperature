with PCM3712_mock;
with PCM3718_mock;


package body mockup_control is

    gain_wats_volt: constant Float := 80.0;

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

    procedure close is
    begin
        PCM3718_mock.Fin_Adquisicion;
    end close;
    
begin
    PCM3712_mock.Initialize_PCM3712;
    --PCM3718_mock.Configuracion_Inicial()
end mockup_control;