--  $Id$

with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);

with CORBA.Impl;

package CORBA.AbstractBase is

   pragma Elaborate_Body;

   type Ref is new PolyORB.Smart_Pointers.Ref with private;

--    procedure Set
--      (The_Ref : in out Ref;
--       The_Object : CORBA.Impl.Object_Ptr);
   --  Since CORBA.Impl.Object_Ptr is declared as a subtype
   --  of PolyORB.Smart_Pointers.Entity_Ptr, the Set operation
   --  is implicitly inherited from PolyORB.Smart_Pointers.Ref.

   function Object_Of (The_Ref : Ref) return CORBA.Impl.Object_Ptr;

   function Get (The_Ref : Ref) return CORBA.Impl.Object_Ptr
     renames Object_Of;

   --  The following primitive operations are inherited
   --  from PolyORB.Smart_Pointers.Ref.

   --    procedure Set
   --      (The_Ref : in out Ref;
   --       The_Entity : Ref_Ptr);

   --    procedure Unref (The_Ref : in out Ref)
   --      renames Finalize;

   --    function Is_Nil (The_Ref : Ref) return Boolean;
   --    function Is_Null (The_Ref : Ref) return Boolean
   --      renames Is_Nil;

   --    procedure Duplicate (The_Ref : in out Ref)
   --      renames Adjust;

   --    procedure Release (The_Ref : in out Ref);

   Nil_Ref : constant Ref;

private

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   Nil_Ref : constant Ref
     := (PolyORB.Smart_Pointers.Nil_Ref with null record);

end CORBA.AbstractBase;
