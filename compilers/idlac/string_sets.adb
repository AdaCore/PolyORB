------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S T R I N G _ S E T S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.HTable;

package body String_Sets is

   function Hash_String is new GNAT.HTable.Hash (Header_Num);

   function Hash (F : String_Ptr) return Header_Num is
   begin
      return Hash_String (F.all);
   end Hash;

   function Equal (F1, F2 : String_Ptr) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   function Contains (Container : Set; Element : String) return Boolean is
      Element_Copy : aliased constant String := Element;
   begin
      return Tables.Get (Container.Set, Element_Copy'Unchecked_Access);
   end Contains;

   procedure Insert (Container : in out Set; Element : String) is
   begin
      --  Avoid heap allocation if the string is already in the set

      if not Contains (Container, Element) then
         Tables.Set (Container.Set, new String'(Element), True);
      end if;
   end Insert;

end String_Sets;
