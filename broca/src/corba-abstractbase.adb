package body CORBA.AbstractBase is

   procedure Set
     (The_Ref : in out Ref;
      The_Object : CORBA.Impl.Object_Ptr) is
   begin
      Set (The_Ref, Broca.Refs.Ref_Ptr (The_Object));
   end Set;

   function Object_Of
     (The_Ref : Ref)
     return CORBA.Impl.Object_Ptr is
   begin
      return CORBA.Impl.Object_Ptr (Entity_Of (The_Ref));
   end Object_Of;

end CORBA.AbstractBase;
