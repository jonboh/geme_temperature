with menu;
with mockup;


procedure geme is
   opcion: Character;

   procedure end_geme is
   begin
      null;
      --Fin_Adquisicion
   end end_geme;

begin

   --initialize()

   opcion := menu.menu_impl;

   case opcion is
      when '1' =>
         -- heat(temp)
         null;
      when '2' =>
         -- show_temp()
         null;
      when '3' =>
         -- read_save_temp()
         null;
      when '4' =>
         -- show_array()
         null;
      when '5' =>
         -- show_avg_temp()
         null;
      when '6' =>
         -- show_overlevel()
         null;
      when '7' =>
            end_geme;
      when others =>
            null;
      end case;

end geme;
