with Ada.Text_IO; use Ada.Text_IO;

package body menu is
   function menu_impl return character is
      cOpcion:character;
   begin

      new_line;
      Put_Line("----------------------MENU---------------------------------------");
      put_line("1- Calentar");
      put_line("2- Mostrar temperatura actual");
      put_line("3- Leer temperatura");
      put_line("4- Mostrar temperatura");
      put_line("5- Temperatura media");
      put_line("6- Porcentaje sobre umbral");
      put_line("7- Salir");

      Put("Pulsar el n de la opcion: ");
      new_line;

      loop--
         Get(cOpcion);

         New_line;
         exit when cOpcion in '1' .. '7';
         Put_Line ("Introduzca un valor entre 1 y 7");
      end loop;

      return copcion;
   end menu_impl;
   -------------
end menu;
