with Ada.Text_IO; use Ada.Text_IO;

package body PCM3718_mock is
    procedure Configuracion_Inicial(Conexion: String; Canal_Primero: Integer; Canal_Ultimo: Integer; Disparo: String; Numero_Muestras: Integer; Rango: String) is
    begin
        put_line("PCM3718 MOCK configured");
    end Configuracion_Inicial;
    
    function Adquirir return Float is
    rand: Float;
    begin
        put_line("Polling PCM3718...");
        rand := 23.0;
        return rand;-- random generator here
    end Adquirir;

    procedure Fin_Adquisicion is
    begin
        put_line("Connection with PCM3718 closed.");
    end Fin_Adquisicion;
end PCM3718_mock;