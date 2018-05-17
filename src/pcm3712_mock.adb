with Ada.Text_IO;
with Ada.Float_Text_IO;

package body PCM3712_mock is
    procedure Initialize_PCM3712 is
    begin
        Ada.Text_IO.Put_Line("Initializing PCM3712");
    end Initialize_PCM3712;

    procedure Write_ao_PCM3712(Volts: Float) is
    begin
        Ada.Text_IO.Put("Heating with ");
        Ada.Float_Text_IO.Put(Volts);
    end Write_ao_PCM3712;    
end PCM3712_mock;