with Ada.Finalization;

generic

   type T is new Broca.Refs.Entity
     with private;

package Broca.Refs.Smart_Pointers is

   pragma Elaborate_Body;

   type Smart_Pointer is private;
   Null_Smart_Pointer : constant Smart_Pointer;

   type Unchecked_Reference is access all T;
   for Unchecked_Reference'Storage_Size use 0;

   function Create return Smart_Pointer;

   function Deref
     (CA : in Smart_Pointer)
     return Unchecked_Reference;

private

   type Any is access T;

   type Smart_Pointer is new Ada.Finalization.Controlled with
      record
         A : Any;
      end record;

   procedure Initialize (CA : in out Smart_Pointer);
   procedure Adjust (CA : in out Smart_Pointer);
   procedure Finalize (CA : in out Smart_Pointer);

   Null_Smart_Pointer : constant Smart_Pointer :=
     (Ada.Finalization.Controlled with A => null);

end Broca.Refs.Smart_Pointers;
