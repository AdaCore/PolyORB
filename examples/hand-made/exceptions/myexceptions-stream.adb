with Broca.Marshalling;
with Broca.Refs;
with Broca.Exceptions;
use Broca.Marshalling;
use Broca.Refs;
use Broca.Exceptions;
package body myexceptions.Stream is

   procedure Marshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in toto_Members)
   is
      use Broca.Marshalling;
   begin
      Marshall (Stream, Val.i);
   end Marshall;

   procedure Unmarshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Res : out toto_Members)
   is
      use Broca.Marshalling;
   begin
      Unmarshall (Stream, Res.i);
   end Unmarshall;

   procedure Compute_New_Size
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in toto_Members)
   is
      use Broca.Marshalling;
   begin
      Compute_New_Size (Stream, Val.i);
   end Compute_New_Size;

   procedure Unmarshall_And_Raise_toto
      (Stream : in out Broca.Buffers.Buffer_Descriptor)
   is
      Bod : Broca.Exceptions.IDL_Exception_Members_Ptr;
   begin
      Bod := new toto_Members;
      Broca.Marshalling.Skip_String (Stream);
      Unmarshall (Stream, toto_Members (Bod.all));
      Broca.Exceptions.User_Raise_Exception
         (toto'Identity, Bod);
   end Unmarshall_And_Raise_toto;

   procedure Raise_toto
      (Bod : toto_Members)
   is
      Members : Broca.Exceptions.IDL_Exception_Members_Ptr;
   begin
      Members := new toto_Members'(Bod);
      Broca.Exceptions.User_Raise_Exception
         (toto'Identity, Members);
   end Raise_toto;

end myexceptions.Stream;
