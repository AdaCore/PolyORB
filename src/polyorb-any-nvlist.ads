------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . N V L I S T                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Any;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Any.NVList is

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Types.Identifier;
      Item       : in Any;
      Item_Flags : in Flags);
   --  Create a NamedValue and add it to this NVList.

   procedure Add_Item
     (Self : Ref;
      Item : in NamedValue);
   --  Add a NamedValue to this NVList.

   function Get_Count (Self : Ref) return PolyORB.Types.Long;
   --  Return the number of items in this NVList.

   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref) renames Free;
   --  Free and Free_Memory are no-ops in Ada.

   ------------------------------------------
   -- The following is specific to PolyORB --
   ------------------------------------------

   procedure Create (NVList : out Ref);
   --  Create a new NVList object and return a reference to it.

   function Image (NVList : Ref) return Standard.String;
   --  For debugging purposes.

   package Internals is

      --  The actual implementation of an NVList: a chained list of
      --  NamedValues.

      package NV_Lists is new PolyORB.Utils.Chained_Lists (NamedValue);
      type NV_List_Access is access all NV_Lists.List;
      function List_Of (NVList : Ref) return NV_List_Access;

   end Internals;

private

   type Object is
     new PolyORB.Smart_Pointers.Non_Controlled_Entity
     with record
         List : aliased Internals.NV_Lists.List;
     end record;
   procedure Finalize (X : in out Object);

   type Object_Ptr is access all Object;

end PolyORB.Any.NVList;
