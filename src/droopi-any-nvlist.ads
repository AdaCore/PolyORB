--  $Id$

with Ada.Finalization;

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

with Droopi.Any;
with Droopi.Types;
with Droopi.Smart_Pointers;

package Droopi.Any.NVList is

   type Ref is new Droopi.Smart_Pointers.Ref with null record;

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

   function Get_Count (Self : Ref) return Droopi.Types.Long;
   --  Return the number of items in this NVList.

   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref) renames Free;
   --  Free and Free_Memory are no-ops in Ada.

   -----------------------------------------
   -- The following is specific to DROOPI --
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

   type Object is new Droopi.Smart_Pointers.Entity with record
      List : aliased NV_Sequence.Sequence := NV_Sequence.Null_Sequence;
   end record;

   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);


   function Create_Object return Object_Ptr;
   --  Create a new and empty Object

end Droopi.Any.NVList;
