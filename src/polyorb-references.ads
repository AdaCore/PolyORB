--  Object references.

--  $Id$

with Ada.Unchecked_Deallocation;
with Sequences.Unbounded;

with PolyORB.Binding_Data; use PolyORB.Binding_Data;
with PolyORB.Smart_Pointers;
with PolyORB.Storage_Pools;
pragma Elaborate_All (PolyORB.Storage_Pools);

package PolyORB.References is

   pragma Elaborate_Body;

   package Profile_Seqs is
      new Sequences.Unbounded (Binding_Data.Profile_Access);
   subtype Profile_Array is Profile_Seqs.Element_Array;

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   --  An object reference of any kind.

   procedure Create_Reference
     (Profiles : Profile_Array;
      Type_Id  : String;
      R        : out Ref);
   --  Create a reference with Profiles as its profiles.
   --  The returned ref R is nil iff Profiles'Length = 0.

   function Profiles_Of (R : Ref) return Profile_Array;
   --  Return the list of profiles constituting Ref.

   function Type_Id_Of  (R : Ref) return String;
   --  Return the type identifier of Ref.

   --  function Is_Nil (R : Ref) return Boolean;
   --  True iff R is a Nil reference, i.e. a reference that
   --  does not designate any object.

   function Image (R : Ref) return String;
   --  For debugging purposes.

   type Ref_Ptr is access all Ref;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Ref, Ref_Ptr);

private

   subtype Profile_Seq is Profile_Seqs.Sequence;

   type String_Ptr is access Standard.String;
   for String_Ptr'Storage_Pool
     use PolyORB.Storage_Pools.Debug_Pool;

   type Reference_Info is new PolyORB.Smart_Pointers.Entity with
      record
         Type_Id  : String_Ptr;
         Profiles : Profile_Seq;
         --  The collection of tagged profiles that designate
         --  transport access points where this object can be
         --  contacted, together with the object ids to be used.
      end record;

   procedure Finalize (RI : in out Reference_Info);

end PolyORB.References;
