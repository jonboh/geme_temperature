with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.Clock_Modulation is

   pragma Preelaborate;

   type CM_Dutycyclelevels is (
      CLOCK_MODULATION_DUTYCYCLE_100,
      CLOCK_MODULATION_DUTYCYCLE_012,
      CLOCK_MODULATION_DUTYCYCLE_025,
      CLOCK_MODULATION_DUTYCYCLE_037,
      CLOCK_MODULATION_DUTYCYCLE_050,
      CLOCK_MODULATION_DUTYCYCLE_063,
      CLOCK_MODULATION_DUTYCYCLE_075,
      CLOCK_MODULATION_DUTYCYCLE_086
      );
   for CM_Dutycyclelevels use (
      CLOCK_MODULATION_DUTYCYCLE_100 => 16#06#,
      CLOCK_MODULATION_DUTYCYCLE_086 => 16#1E#,
      CLOCK_MODULATION_DUTYCYCLE_075 => 16#1C#,
      CLOCK_MODULATION_DUTYCYCLE_063 => 16#1A#,
      CLOCK_MODULATION_DUTYCYCLE_050 => 16#18#,
      CLOCK_MODULATION_DUTYCYCLE_037 => 16#16#,
      CLOCK_MODULATION_DUTYCYCLE_025 => 16#14#,
      CLOCK_MODULATION_DUTYCYCLE_012 => 16#12#
      );
   for CM_Dutycyclelevels'size use 8;


   function set_Dutycyclelevel (lvl : in CM_Dutycyclelevels) return Int;
   pragma Import (C, set_dutycyclelevel, "clock_modulation_set_dutycyclelevel");
      -- clock_modulation.c

   function get_dutycyclelevel return CM_Dutycyclelevels;
   pragma Import (C, get_dutycyclelevel, "clock_modulation_get_dutycyclelevel");
      -- clock_modulation.c

   function get_dutycycle (lvl : in CM_Dutycyclelevels) return Float;
   pragma Import (C, get_dutycycle, "clock_modulation_get_dutycycle");
      -- clock_modulation.c

   function is_supported return Int;
   pragma Import (C, is_supported, "clock_modulation_is_supported");
      -- clock_modulation.c

   function calibrate return Int;
   pragma Import (C, calibrate, "clock_modulation_calibrate");
      -- clock_modulation.c

end MaRTE.Clock_Modulation;