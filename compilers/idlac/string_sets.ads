------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          S T R I N G _ S E T S                           --
--                                                                          --
--                                 S p e c                                  --
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
