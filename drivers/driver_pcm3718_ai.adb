package body Driver_Pcm3718_Ai is

   -----------------------------------------------------------------------
   -- 1.- Useful Functions and Procedures
   -----------------------------------------------------------------------

   procedure Pause is
      H : Character;
   begin
      Put(" Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   -----------------------------------------------------------------------

   procedure Message (Str : String) is
   begin
      Put(Str);
      Pause;
   end Message;

   -----------------------------------------------------------------------

   function Sample2volt (
         The_Sample : Sample_Type;
         Range_P    : Float;
         Bipolar    : Boolean)
     return Float is
      Range_N : Float := 0.0;
   begin
      if Bipolar then
         Range_N := Range_P;
      end if;
      return (Range_P-(-Range_N))*Float(The_Sample)/4096.0-Range_N;
   end Sample2volt;

   -----------------------------------------------------------------------

   procedure Configuracion_Inicial (
               Conexion: in Tipo_Conexion:=Differential_Ended;
               Canal_Primero: in Tipo_Numero_Canal:=0;
               Canal_Ultimo: in Tipo_Numero_Canal:=0;
               Disparo: in Tipo_Disparo:=Software;
               Numero_Muestras: in Positive:=1;
               Rango: in Tipo_Rango:=Unipolar_5
               -- Modo: in Mode_Type; -- NO UTILIZADO!!!!
               -- Velocidad_muestreo: in Scan_Rate_Type; -- NO UTILIZADO!!!!
               ) is
   begin
      New_Line;
      Put("This is the Ada driver program for Analog Input of PCM-3718H.");
      New_Line;
      Fd := Posix_Io.Open ("/dev/daq", Posix_Io.Read_Only);

      if Canal_Ultimo<Canal_Primero then
         loop
            Message("Error: Canal_Primero es mayor que Canal_Ultimo");
         end loop;
      end if;

      if Conexion=Differential_Ended and then
        (Canal_Primero>7 or Canal_Ultimo>7) then
         loop
            Message("Error: Canales fuera de rango en Diffential_Ended (0..7)");
         end loop;
      end if;

      N:=Numero_muestras;--N indica un numero fijo de muestras
      Ai_Arg.Start_Ch   := Channel_Type(Canal_Primero);
      --Todos los canales con el mismo rango
      Ranges := (others => Range_Type(Rango));

      -- Create the buffer to hold the samples
      Buffer := new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset(N*Data'Size/8));

      -- Set the Range of each channel we want to use
      Ai_Command := Set_Range_Of_Channel;
      --Configurar los canales con el mismo rango
      for I in Channel_Type range Ai_Arg.Start_Ch .. Ai_Arg.Stop_Ch loop
         Ai_Arg.Start_Ch := I;
         Ai_Arg.Input_Range := Ranges(I);
         Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
      end loop;

      -- Set the parameters for software trigger
      Ai_Command := Set_Parameters;
      Ai_Arg.Start_Ch     := Channel_Type(Canal_Primero);
      Ai_Arg.Stop_Ch      := Channel_Type(Canal_Ultimo);
      Ai_Arg.Trigger      := Software;
      --Num_Conv_Type de 0..256, y N de 1..infinito
      Ai_Arg.Count        := Num_Conv_Type(N);
      Ai_Ioctl(Fd,Ai_Command,Ai_Arg);

      case Rango is
         when Bipolar_5=>
            Bipolar:=True;
            Vref:=5.0;
         when Bipolar_2_5=>
            Bipolar:=True;
            Vref:=2.5;
         when Bipolar_1_25=>
            Bipolar:=True;
            Vref:=1.25;
         when Bipolar_0_625=>
            Bipolar:=True;
            Vref:=0.625;
         when Unipolar_10=>
            Bipolar:=False;
            Vref:=10.0;
         when Unipolar_5=>
            Bipolar:=False;
            Vref:=5.0;
         when Unipolar_2_5=>
            Bipolar:=False;
            Vref:=2.5;
         when Unipolar_1_25=>
            Bipolar:=False;
            Vref:=1.25;
         when Bipolar_10=>
            Bipolar:=True;
            Vref:=10.0;
      end case;
   end Configuracion_Inicial;

   -----------------------------------------------------------------------

   function Adquirir return Float is
      Voltios: Float;
   begin
      Posix_Io.Read(Fd, Buffer.all, Position);
      Data:=To_Data(Buffer).all(1);
      Voltios:=Sample2volt(Data.The_Sample, Vref, Bipolar);
      return Voltios;
   end Adquirir;

   -----------------------------------------------------------------------

   procedure Fin_Adquisicion is
   begin
      Posix_Io.Close (Fd);
   end Fin_Adquisicion;


begin
   null;

exception
   when Except:others =>
      Ada.Text_Io.Put_Line ("Unexpected exception raised " &
         Ada.Exceptions.Exception_Name (Except));
      Ada.Text_Io.Put_Line(Exception_Information(Except));
      Ada.Text_Io.Put_Line(Exception_Message (Except));

end Driver_Pcm3718_Ai;
