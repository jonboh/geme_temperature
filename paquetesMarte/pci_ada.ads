------------------------------------------------------------------------------
---------------------------      P C I     L I B      ------------------------
------------------------------------------------------------------------------
--
--                                 'p c i'
--
--                                  Spec
--
--
--  File 'pci.ads'                                                   By Chema.
--                                                        Jose Mar�a Martinez
--                                                       <martinjm@unican.es>
--
--  PCI bus management. Import declarations of pci.h / pci.c
--
--
--
-----------------------------------------------------------------------------

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package PCI_Ada is

   type Char_Ptr is access String;
   type Region_Map is record
      Base_Address : Unsigned_32;
      --  Space_Mapped: If 'M' the region maps to memory address space.
      --                If 'I' the region maps to the I/O address space.
      --                If 'N' the region is Not mapped
      Space_Mapped : Character;
      Region_Size  : Unsigned_32;
   end record;

   type PCI_Region_Map is array (1 .. 6) of Region_Map;

   type Pci_Device is record
      Class        : Unsigned_32; -- 3 bytes: (base,sub,prog-if)
      Vendor       : Unsigned_16;
      Device_ID    : Unsigned_16;
      --  membase and ioaddr are depricated, not used. Backward
      --  compativility.
      Membase      : Unsigned_32;
      Ioaddr       : Unsigned_32;
      --  Membase and ioaddr are replaced by proper memory identification.
      --  structure Region_Map.
      Pci_Region   : PCI_Region_Map;
      Rom_Address  : Unsigned_32; -- which config register controls the ROM
      Device_Fn    : Unsigned_8;
      Pci_Bus      : Unsigned_8;
      Pci_Irq      : Unsigned_8; -- Assigned IRQ values (0 - 15)
      Pci_Pin      : Unsigned_8; -- Assigned PIN values (0x01,0x02,0x03,0x04)
                                 --   0x00 -> Means that the device doesn't
                                 --           support interrupts.
      Name         : Char_Ptr;   -- Device name (not used.)
   end record;

   --  The from argument is used to get hold of multiple devices with
   --  the same signature (vendor + device); the argument should point
   --  to the last device that has been found, so the search can be
   --  continued. To find the first device, from must be specified
   --  with Pci_Bus and Device_Fn zeroed. The function stores in found
   --  the devices information, if no device found with the required
   --  vendor + device -1 is stored in Result. If OK returns 0.  If
   --  vendor == 0 && device == 0 it will find the first PCI device,
   --  in conjunction with from you can scan all PCIs

   procedure Pci_Find_Device( Result : out Integer;
                              Vendor : in Unsigned_16;
                              Device : in Unsigned_16;
                              From   : in out Pci_Device;
                              Found  : in out Pci_Device);
   pragma Import (C, Pci_Find_Device, "pci_find_device");
   pragma Import_Valued_Procedure(Pci_Find_Device,"pci_find_device");


   procedure Pci_Read_Config_Byte( Result : out Integer;
                                   Dev    : in out Pci_Device;
                                   Where  : in Unsigned_32;
                                   Value  : out Unsigned_8);
   pragma Import (C, Pci_Read_Config_Byte, "pci_read_config_byte");
   pragma Import_Valued_Procedure(Pci_Read_Config_Byte,"pci_read_config_byte");


   procedure Pci_Read_Config_Word( Result : out Integer;
                                   Dev    : in out Pci_Device;
                                   Where  : in Unsigned_32;
                                   Value  : out Unsigned_16);
   pragma Import (C, Pci_Read_Config_Word, "pci_read_config_word");
   pragma Import_Valued_Procedure(Pci_Read_Config_word,"pci_read_config_word");


   procedure Pci_Read_Config_DWord( Result : out Integer;
                                   Dev    : in out Pci_Device;
                                   Where  : in Unsigned_32;
                                   Value  : out Unsigned_32);
   pragma Import (C, Pci_Read_Config_DWord, "pci_read_config_dword");
   pragma Import_Valued_Procedure(Pci_Read_Config_DWord,
                                    "pci_read_config_dword");

   procedure Pci_Write_Config_Byte( Result : out Integer;
                                   Dev    : in out Pci_Device;
                                   Where  : in Unsigned_32;
                                   Value  : out Unsigned_8);
   pragma Import (C, Pci_Write_Config_Byte, "pci_write_config_byte");
   pragma Import_Valued_Procedure(Pci_write_Config_Byte,
                                    "pci_write_config_byte");
   procedure Pci_Write_Config_Word( Result : out Integer;
                                    Dev    : in out Pci_Device;
                                    Where  : in Unsigned_32;
                                    Value  : out Unsigned_16);
   pragma Import (C, Pci_Write_Config_Word, "pci_write_config_word");
   pragma Import_Valued_Procedure(Pci_write_Config_Word,
                                    "pci_write_config_word");
   procedure Pci_Write_Config_DWord( Result : out Integer;
                                     Dev    : in out Pci_Device;
                                     Where  : in Unsigned_32;
                                     Value  : out Unsigned_32);
   pragma Import (C, Pci_Write_Config_DWord, "pci_write_config_dword");
   pragma Import_Valued_Procedure(Pci_write_Config_DWord,
                                    "pci_write_config_dword");


end PCI_Ada;
