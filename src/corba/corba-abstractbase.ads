--  $Id$

with Droopi.Refs;
with CORBA.Impl;

package CORBA.AbstractBase is

   pragma Elaborate_Body;

   type Ref is new Droopi.Refs.Ref with private;

   procedure Set
     (The_Ref : in out Ref;
      The_Object : CORBA.Impl.Object_Ptr);

   function Object_Of (The_Ref : Ref) return CORBA.Impl.Object_Ptr;

   function Get (The_Ref : Ref) return CORBA.Impl.Object_Ptr
     renames Object_Of;

   --  The following primitive operations are inherited
   --  from Droopi.Refs.Ref.

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

   type Ref is new Droopi.Refs.Ref with null record;
   Nil_Ref : constant Ref
     := (Droopi.Refs.Nil_Ref with null record);

end CORBA.AbstractBase;
