with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_IO; use Ada.Text_IO;
with Pcm3712; use Pcm3712;
with Pcm3712_Functions; use Pcm3712_Functions;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;


package body driver_pcm3712_ao_ehu is

   procedure Initialize_PCM3712 is
   begin
      Outb_P (Pcm3712_Base + 5, 16#FF#);
   end Initialize_PCM3712;

   procedure Write_ao_PCM3712(fValor: Float) is
      fAux: Float;
      yValor: Integer;
      u16_Valor, u16_Valor_Low, u16_Valor_High: Unsigned_16; --
      u8_Valor_Low, u8_Valor_high: Unsigned_8;
   begin
      fAux:= fValor;
      if fAux > 5.0 then
         fAux:=5.0;
      elsif fAux <0.0 then
         fAux:=0.0;
      end if;


      --code_low:=Sample_Byte_Type(Unsigned_16(code) AND Unsigned_16(16#00FF#));
      --code_high:=Sample_Byte_Type(MaRTE.Integer_Types.Shift_Right(Unsigned_16(code),8));
      yValor:=Integer(fAux*13107.0); -- y=13107x (para 2¹6-1) Ejemplo: 3V -> 39321-> 1001100110011001 --> 153  153
      -- Put(yValor); New_Line;
      u16_Valor := Unsigned_16(yValor);
      u16_Valor_Low:= u16_Valor AND 2#0000000011111111#;
      -- Put(Integer(u16_Valor_Low));New_Line;
      u8_Valor_Low:= Unsigned_8(u16_Valor_Low);
      --u8_Valor_Low:= Unsigned_8(u16_Valor); --range check failed

      --Put(Integer(u8_Valor_Low));New_Line;

      u16_Valor_High:= u16_Valor AND 2#1111111100000000#;
      u16_Valor_High:= Shift_Right(u16_Valor,8);
      u8_Valor_High:= Unsigned_8(u16_Valor_High);
      --Put(Integer(u8_Valor_High)); New_Line;

      Outb_P (Pcm3712_Base + 0, u8_Valor_Low); --Low
      Outb_P (Pcm3712_Base + 1, u8_Valor_High); --High
   end Write_ao_PCM3712;


end driver_pcm3712_ao_ehu;
