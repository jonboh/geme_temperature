with Ada.Text_IO;
with Ada.Float_Text_IO;
with menu;
with mockup_interface;
with mockup_control;


procedure geme is
      opcion: Character;
      power,F_umbral: Float;
begin

      loop--
      opcion := menu.menu_impl;
      case opcion is
            when '1' =>
                  Ada.Text_IO.Put("Introduzca potencia de calentado (Wats):  ");
                  Ada.Float_Text_IO.Get(power);
                  mockup_control.heat(power); -- feed user input to heat()
            when '2' =>
                  mockup_interface.show_temp;
            when '3' =>
                  mockup_interface.read_save_temp;
            null;
            when '4' =>
                  mockup_interface.show_array;
            null;
            when '5' =>
                  mockup_interface.show_avg_temp;
         when '6' =>
                  Ada.Text_IO.Put_line("Introduzca valor umbral: ");
                  Ada.Float_Text_IO.Get(F_umbral);
                  mockup_interface.show_overlevel(F_umbral);

            when '7' =>
                  mockup_control.close;
            when others =>
                  null;
            end case;
      exit when opcion='7';
      end loop;

end geme;
