with Broca.Ior;
with Broca.Buffers; use Broca.Buffers;

package body Corba.Object is
   function Is_Nil (Self : in Ref) return CORBA.Boolean is
      use Broca.Refs;
   begin
      return Get (Self) = null;
   end Is_Nil;

   function Object_To_String (Obj : CORBA.Object.Ref) return CORBA.String is
      Buffer : Buffer_Descriptor;
   begin
      Compute_New_Size (Buffer, Obj);
      Allocate_Buffer (Buffer);
      Marshall (Buffer, Obj);
      return Broca.Ior.Buffer_To_Ior_String (Buffer);
   end Object_To_String;
end Corba.Object;
