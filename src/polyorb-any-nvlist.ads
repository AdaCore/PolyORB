--  $Id$

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Smart_Pointers;

package PolyORB.Any.NVList is

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
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

   -----------------------------------------
   -- The following is specific to PolyORB --
   -----------------------------------------

   procedure Create (NVList : out Ref);
   --  Create a new NVList object and return a reference to it.

   function Image (NVList : Ref) return Standard.String;
   --  For debugging purposes.

   package Internals is

      --  The actual implementation of an NVList:
      --  a sequence of NamedValues.

      package NV_Sequence is new Sequences.Unbounded (NamedValue);
      type NV_Sequence_Access is access all NV_Sequence.Sequence;

      function List_Of (NVList : Ref) return NV_Sequence_Access;

   end Internals;

private

   package NV_Sequence renames Internals.NV_Sequence;

   type Object is new PolyORB.Smart_Pointers.Entity with record
      List : aliased NV_Sequence.Sequence := NV_Sequence.Null_Sequence;
   end record;

   type Object_Ptr is access all Object;

end PolyORB.Any.NVList;
