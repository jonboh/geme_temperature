with Ada.Text_IO;
with Ada.Float_Text_IO;
with menu;
with mockup_interface;


procedure geme is
      opcion: Character;
begin

   opcion := menu.menu_impl;

   case opcion is
      when '1' =>
            Ada.Text_IO.Put("Introduzca potencia de calentado (Wats):  ");
            --mockup_interface.heat(Ada.Float_Text_IO.Get); -- feed user input to heat()
      when '2' =>
            mockup_interface.show_temp;
         null;
      when '3' =>
            mockup_interface.read_save_temp;
         null;
      when '4' =>
            mockup_interface.show_array;
         null;
      when '5' =>
            mockup_interface.show_avg_temp;
      when '6' =>
            --mockup_interface.show_overlevel
         null;
      when '7' =>
            mockup_interface.close_mockup;
      when others =>
            null;
      end case;

end geme;
