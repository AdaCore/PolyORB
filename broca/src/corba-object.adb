with Broca.Ior;

package body Corba.Object is
   function Is_Nil (Self : in Ref) return CORBA.Boolean is
      use Broca.Refs;
   begin
      return Get (Self) = null;
   end Is_Nil;

   function Object_To_String (Obj : CORBA.Object.Ref) return CORBA.String is
   begin
      return Broca.Ior.Buffer_To_Ior_String
        (Broca.Refs.Object_To_IOR (Get (Obj).all));
   end Object_To_String;
end Corba.Object;
