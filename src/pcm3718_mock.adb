with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

package body PCM3718_mock is
    procedure Configuracion_Inicial(Conexion: String; Canal_Primero: Integer; Canal_Ultimo: Integer; Disparo: String; Numero_Muestras: Integer; Rango: String) is
    begin
        put_line("PCM3718 MOCK configured");
    end Configuracion_Inicial;
    
    function Adquirir return Float is
    rand: Float;
    
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
        put_line("Polling PCM3718...");
        rand := generar_random_float;
        return rand;-- random generator here
    end Adquirir;

    procedure Fin_Adquisicion is
    begin
        put_line("Connection with PCM3718 closed.");
    end Fin_Adquisicion;
end PCM3718_mock;