------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T Y P E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package System.Garlic.Types is

   pragma Preelaborate;

   type Partition_ID is range 0 .. 63;

   Communication_Error : exception;

   type Shutdown_Access is access procedure;

   --  This package defines basic types that are used throughout Garlic
   --  as well as commonly used deallocation and conversion subprograms.

   type String_Access is access String;
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);
   --  Access on string and deallocation procedure

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Portable_Address is mod 2 ** 64;
   --  This type can contain an object of type System.Address on any platform
   --  where GNAT is supported. It is made public on purpose so that it is
   --  possible to take a 'Image of it.

   function To_Address (Addr : Portable_Address) return Address;
   function To_Portable_Address (Addr : Address) return Portable_Address;
   --  Conversion routines

private

   pragma Inline (To_Address);
   pragma Inline (To_Portable_Address);

end System.Garlic.Types;
