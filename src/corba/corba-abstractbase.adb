--  $Id$

package body CORBA.AbstractBase is

   ---------------
   -- Object_Of --
   ---------------

   function Object_Of
     (The_Ref : Ref)
     return CORBA.Impl.Object_Ptr is
   begin
      return CORBA.Impl.Object_Ptr (Entity_Of (The_Ref));
   end Object_Of;

   ---------
   -- Set --
   ---------

   procedure Set
     (The_Ref : in out Ref;
      The_Object : CORBA.Impl.Object_Ptr) is
   begin
      Set (The_Ref, Droopi.Smart_Pointers.Entity_Ptr (The_Object));
   end Set;

end CORBA.AbstractBase;
