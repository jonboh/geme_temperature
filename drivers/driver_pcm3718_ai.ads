------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--
--                              'test_pcm3718_ai'
--
--                                   Body
--
--
--  File 'test_pcm3718_ai.adb'                                   By Sangorrin
--
--  This program uses the Analog Input of the PCM-3718H Driver.
--  In order to run the program you should install that driver in the
--  system. For more information about installing drivers in MaRTE OS
--  refer to the "MaRTE OS User's Guide" (marte_ug.html).
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--
--  Modificado por:
--      Víctor M. López Senderos
--      Rafael Priego Rementería
-------------------------------------------------------------------------------

-- POSIX IO --------------------------------------------------------------
with Posix_Io;
with Ada.Streams;
with Ada.Unchecked_Conversion;
-- TEXT IO ---------------------------------------------------------------
with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
-- TYPES -----------------------------------------------------------------
with Pcm3718;
use Pcm3718;
with MaRTE.Integer_Types;
--------------------------------------------------------------------------
with Ada.Exceptions;
use Ada.Exceptions;
package Driver_Pcm3718_Ai is

   package Bit renames MaRTE.Integer_Types;
   use type Bit.Unsigned_16;

   --pragma Priority(4); -- ¡¡mejor poner prioridad en la que está en métodos!!

   -----------------------------------------------------------------------
   -- 0. Types
   -----------------------------------------------------------------------

   type Tipo_Conexion is (Single_Ended, Differential_Ended);

   type Tipo_Numero_Canal is new Integer range 0..15;

   type Tipo_Disparo is new Trigger_Type;-- (Software, External, Pacer)

   type Tipo_Rango is new Range_Type;-- (Bipolar_5,      --     -5 < Vin < 5
                                     --  Bipolar_2_5,    --   -2.5 < Vin < 2.5
                                     --  Bipolar_1_25,   --  -1.25 < Vin < 1.25
                                     --  Bipolar_0_625,  -- -0.625 < Vin < 0.625
                                     --  Unipolar_10,    --      0 < Vin < 10
                                     --  Unipolar_5,     --      0 < Vin < 5
                                     --  Unipolar_2_5,   --      0 < Vin < 2.5
                                     --  Unipolar_1_25,  --      0 < Vin < 1.25
                                     --  Bipolar_10);    --    -10 < Vin < 10

   -----------------------------------------------------------------------
   -- 1.- Useful Functions and Procedures
   -----------------------------------------------------------------------

   procedure Pause ;

   procedure Message (
         Str : String) ;

   function Sample2volt (
         The_Sample : Sample_Type;
         Range_P    : Float;
         Bipolar    : Boolean)
     return Float ;

   --ONLY SOFTWARE TRIGGER IS AVAILABLE IN 'Driver_Pcm3718_Ai'
   --Si se lee de varios canales es obligatorio que sean consecutivos
   procedure Configuracion_Inicial (
                Conexion: in Tipo_Conexion:=Differential_Ended;
                	-- (Single_Ended, Differential_Ended)
                Canal_Primero: in Tipo_Numero_Canal:=0;
                        -- (0..15)
                        --En Differential_Ended solo se pueden usar 0..7
                Canal_Ultimo: in Tipo_Numero_Canal:=0;
                	-- (0..15)
                Disparo: in Tipo_Disparo:=Software;
                	-- (Software, External, Pacer)
                Numero_Muestras: in Positive:=1;
               		-- (1..256) Por defecto 1 para recoger 1
                Rango: in Tipo_Rango:=Unipolar_5
                	-- (Bipolar_5,      --            -5 < Vin < 5
                	--  Bipolar_2_5,    --          -2.5 < Vin < 2.5
                	--  Bipolar_1_25,   --         -1.25 < Vin < 1.25
                	--  Bipolar_0_625,  --        -0.625 < Vin < 0.625
                	--  Unipolar_10,    --             0 < Vin < 10
                	--  Unipolar_5,     --             0 < Vin < 5
                	--  Unipolar_2_5,   --             0 < Vin < 2.5
                	--  Unipolar_1_25,  --             0 < Vin < 1.25
                	--  Bipolar_10);    --           -10 < Vin < 10
                -- Modo: in Mode_Type; -- NO UTILIZADO!!!!
                        -- (Fixed, Scan)
                	-- When Fixed Mode is set the driver adquires "Count"
               		-- samples each time you call READ.
                	-- When Scan Mode is set the driver is continuosly
                	-- storing samples in a internal Buffer (It can
                        -- overwrite the old ones), and when you call READ you
                        -- read the samples in the Buffer.
                -- Velocidad_muestreo: in Scan_Rate_Type; -- NO UTILIZADO!!!!
                	-- (4 .. 4_294_836_225)
                        -- SCAN_RATE_TYPE
                        -- This is the rate used to acquire data when using
                        -- PACER TRIGGER.
                        -- If F_Clock_Pacer = 1E6Hz =>
                        --    Scan_Rate is in seconds*10e-6 (uS)
                        -- If F_Clock_Pacer = 10E6Hz =>
                        --    Scan_Rate is in seconds*10e-7
			-- The driver will try to aproximate this rate and
                        -- return this aproximation on the same variable of
                        -- the Ioctl Argument.
                );

   function Adquirir return Float;

   procedure Fin_Adquisicion;

   -----------------------------------------------------------------------
   -- 2.- GENERIC POSIX_IO INSTANCE
   -----------------------------------------------------------------------
   procedure Ai_Ioctl is
   new Posix_Io.Generic_Ioctl (Pcm3718.Ai_Ioctl_Cmd,Pcm3718.Ai_Ioctl_Arg);
   -----------------------------------------------------------------------
   -- 3.- UNCHECKED OPERATIONS FOR POSIX_IO ADA INTERFACE
   -----------------------------------------------------------------------
   type Sea_Ptr is access all Ada.Streams.Stream_Element_Array;
   type Data_Ptr is access all Analog_Data;
   function To_Data is
   new Ada.Unchecked_Conversion (Sea_Ptr,Data_Ptr);
   -----------------------------------------------------------------------
   -- 4.- VARIABLES
   -----------------------------------------------------------------------
   Fd         : Posix_Io.File_Descriptor;
   Buffer     : Sea_Ptr;
   Position   : Ada.Streams.Stream_Element_Offset;
   Ai_Command : Pcm3718.Ai_Ioctl_Cmd;
   Ai_Arg     : Pcm3718.Ai_Ioctl_Arg;
   Data       : Analog_Data_Type;
   Ranges     : array (Channel_Type) of Range_Type;
   N,
   R	      : Positive;
   Bipolar: Boolean;
   Vref: Float;
   -----------------------------------------------------------------------
end Driver_Pcm3718_Ai;
