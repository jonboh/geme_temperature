package PCM3718_mock is
    procedure Configuracion_Inicial(Conexion: String; Canal_Primero: Integer; Canal_Ultimo: Integer; Disparo: String; Numero_Muestras: Integer; Rango: String);
    function Adquirir return Float;
    procedure Fin_Adquisicion;
end PCM3718_mock;