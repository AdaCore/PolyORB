with Broca.Marshalling;
use Broca.Marshalling;
package body M1.Stream is
   procedure Marshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : My_String)
   is
   begin
      Marshall (Stream, CORBA.String (Val));
   end Marshall;

   procedure Unmarshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Res : out My_String)
   is
      Tmp : CORBA.String;
   begin
      Unmarshall (Stream, Tmp);
      Res := My_String (Tmp);
   end Unmarshall;

   procedure Compute_New_Size
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : My_String)
   is
   begin
      Compute_New_Size (Stream, CORBA.String (Val));
   end Compute_New_Size;

end M1.Stream;