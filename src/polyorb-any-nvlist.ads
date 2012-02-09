------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . N V L I S T                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

pragma Ada_2005;

with PolyORB.Any;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Any.NVList is

   type Ref is new PolyORB.Smart_Pointers.Ref with private;

   procedure Add_Item
     (Self       : Ref;
      Item_Name  : Types.Identifier;
      Item       : Any;
      Item_Flags : Flags);
   --  Create a NamedValue and add it to this NVList

   procedure Add_Item
     (Self : Ref;
      Item : NamedValue);
   --  Add a NamedValue to this NVList

   function Get_Count (Self : Ref) return PolyORB.Types.Long;
   --  Return the number of items in this NVList

   ------------------------------------------
   -- The following is specific to PolyORB --
   ------------------------------------------

   procedure Create (NVList : out Ref);
   --  Create a new NVList object and return a reference to it

   function Image (NVList : Ref) return Standard.String;
   --  For debugging purposes

   package Internals is

      --  The actual implementation of an NVList: a chained list of
      --  NamedValues.

      package NV_Lists is new PolyORB.Utils.Chained_Lists (NamedValue);

      type NV_List_Access is access all NV_Lists.List;

      function List_Of
        (NVList : Ref)
        return NV_List_Access;

   end Internals;

private

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity
     with record
        List : aliased Internals.NV_Lists.List;
     end record;

   type Object_Ptr is access all Object;

   overriding procedure Finalize
     (X : in out Object);

end PolyORB.Any.NVList;
