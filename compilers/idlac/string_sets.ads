------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S T R I N G _ S E T S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2007, Free Software Foundation, Inc.          --
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

--  Simple sets of Strings. Note that storage is never reclaimed; objects of
--  type Set are typically global.

with GNAT.Dynamic_HTables;

package String_Sets is

   type Set is limited private;

   function Contains (Container : Set; Element : String) return Boolean;
   --  Determine whether Element is in Container

   procedure Insert (Container : in out Set; Element : String);
   --  Insert Element into Container

private

   --  WAG:5.03a1 : Ideally, this would be implemented in terms of
   --  Ada.Containers.Indefinite_Hashed_Sets, but we wish to avoid
   --  dependence on Ada 2005 features, so this can be compiled with
   --  older compilers.

   --  The implementation is a hash table mapping strings to True; False means
   --  "not present". This implementation is probably not as efficient as the
   --  Indefinite_Hashed_Sets would be.

   type Header_Num is range 0 .. 2**14 - 1;
   --  arbitrary number; seems big enough

   type String_Ptr is access constant String;
   --  We have to use a pointer, because Simple_HTable requires a definite
   --  subtype.

   function Hash (F : String_Ptr) return Header_Num;
   function Equal (F1, F2 : String_Ptr) return Boolean;

   package Tables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   type Set is limited record
      Set : Tables.Instance;
   end record;

end String_Sets;
