with Broca.IOR;
with Broca.Buffers; use Broca.Buffers;
with Broca.Refs; use Broca.Refs;

package body CORBA.Object is

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : Ref) return CORBA.Boolean is
      use Broca.Refs;
   begin
      return Get (Self) = null;
   end Is_Nil;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String (Obj : CORBA.Object.Ref) return CORBA.String
   is
      Buffer : Buffer_Descriptor;
   begin
      Compute_New_Size (Buffer, Obj);
      Allocate_Buffer (Buffer);
      Marshall (Buffer, Obj);
      return Broca.IOR.Buffer_To_IOR_String (Buffer);
   end Object_To_String;

end CORBA.Object;
