------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S T R I N G _ S E T S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
