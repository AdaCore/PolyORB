------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . P O I N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Conversion;
with System.Garlic.Addresses;  use System.Garlic.Addresses;

package body System.Garlic.Pointers is

   use System.Garlic.Addresses;

   function Convert is
      new Ada.Unchecked_Conversion (Pointer, Address);
   function Convert is
      new Ada.Unchecked_Conversion (Address, Pointer);

   -------------------
   -- Create_Handle --
   -------------------

   function Create_Handle (P : Pointer) return Handle is
   begin
      return Create_Handle (Convert (P));
   end Create_Handle;

   ----------------------
   -- Retrieve_Pointer --
   ----------------------

   function Retrieve_Pointer (H : Handle) return Pointer is
   begin
      return Convert (Retrieve_Address (H));
   end Retrieve_Pointer;

end System.Garlic.Pointers;
