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

   procedure Marshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet.Length (Val));
   begin
      Marshall (Stream, Len);
      for I in 1 .. Len loop
         Marshall
            (Stream, IDL_Sequence_Octet.Element_Of (Val, Integer (I)));
      end loop;
   end Marshall;

   procedure Unmarshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Res : out IDL_Sequence_Octet.Sequence)
   is
      Len : CORBA.Unsigned_Long;
      Tmp : IDL_Sequence_Octet.Sequence;
      El : CORBA.Octet;
   begin
      Unmarshall (Stream, Len);
      Tmp := IDL_Sequence_Octet.To_Sequence (Natural (Len));
      for I in 1 .. Len loop
         Unmarshall (Stream, El);
         IDL_Sequence_Octet.Replace_Element (Tmp, Positive (Len), El);
      end loop;
      Res := Tmp;
   end Unmarshall;

   procedure Compute_New_Size
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet.Length (Val));
   begin
      Compute_New_Size (Stream, UL_Size, UL_Size);
      for I in 1 .. Len loop
         Compute_New_Size
            (Stream, IDL_Sequence_Octet.Element_Of (Val, Integer (I)));
      end loop;
   end Compute_New_Size;
   procedure Marshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap1)
   is
   begin
      Marshall (Stream, IDL_Sequence_Octet.Sequence(Val));
   end Marshall;

   procedure Unmarshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Res : out Encap1)
   is
      Tmp : IDL_Sequence_Octet.Sequence;
   begin
      Unmarshall (Stream, Tmp);
      Res := Encap1(Tmp);
   end Unmarshall;

   procedure Compute_New_Size
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap1)
   is
   begin
      Compute_New_Size (Stream, IDL_Sequence_Octet.Sequence(Val));
   end Compute_New_Size;

   procedure Marshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet_1.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet_1.Length (Val));
   begin
      Marshall (Stream, Len);
      for I in 1 .. Len loop
         Marshall
            (Stream, IDL_Sequence_Octet_1.Element_Of (Val, Integer (I)));
      end loop;
   end Marshall;

   procedure Unmarshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Res : out IDL_Sequence_Octet_1.Sequence)
   is
      Len : CORBA.Unsigned_Long;
      Tmp : IDL_Sequence_Octet_1.Sequence;
      El : CORBA.Octet;
   begin
      Unmarshall (Stream, Len);
      Tmp := IDL_Sequence_Octet_1.To_Sequence (Natural (Len));
      for I in 1 .. Len loop
         Unmarshall (Stream, El);
         IDL_Sequence_Octet_1.Replace_Element (Tmp, Positive (Len), El);
      end loop;
      Res := Tmp;
   end Unmarshall;

   procedure Compute_New_Size
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet_1.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet_1.Length (Val));
   begin
      Compute_New_Size (Stream, UL_Size, UL_Size);
      for I in 1 .. Len loop
         Compute_New_Size
            (Stream, IDL_Sequence_Octet_1.Element_Of (Val, Integer (I)));
      end loop;
   end Compute_New_Size;
   procedure Marshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap2)
   is
   begin
      Marshall (Stream, IDL_Sequence_Octet_1.Sequence(Val));
   end Marshall;

   procedure Unmarshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Res : out Encap2)
   is
      Tmp : IDL_Sequence_Octet_1.Sequence;
   begin
      Unmarshall (Stream, Tmp);
      Res := Encap2(Tmp);
   end Unmarshall;

   procedure Compute_New_Size
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap2)
   is
   begin
      Compute_New_Size (Stream, IDL_Sequence_Octet_1.Sequence(Val));
   end Compute_New_Size;

   procedure Marshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet_4.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet_4.Length (Val));
   begin
      Marshall (Stream, Len);
      for I in 1 .. Len loop
         Marshall
            (Stream, IDL_Sequence_Octet_4.Element_Of (Val, Integer (I)));
      end loop;
   end Marshall;

   procedure Unmarshall
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Res : out IDL_Sequence_Octet_4.Sequence)
   is
      Len : CORBA.Unsigned_Long;
      Tmp : IDL_Sequence_Octet_4.Sequence;
      El : CORBA.Octet;
   begin
      Unmarshall (Stream, Len);
      Tmp := IDL_Sequence_Octet_4.To_Sequence (Natural (Len));
      for I in 1 .. Len loop
         Unmarshall (Stream, El);
         IDL_Sequence_Octet_4.Replace_Element (Tmp, Positive (Len), El);
      end loop;
      Res := Tmp;
   end Unmarshall;

   procedure Compute_New_Size
     (Stream : in out Broca.Buffers.Buffer_Descriptor;
      Val : in IDL_Sequence_Octet_4.Sequence)
   is
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (IDL_Sequence_Octet_4.Length (Val));
   begin
      Compute_New_Size (Stream, UL_Size, UL_Size);
      for I in 1 .. Len loop
         Compute_New_Size
            (Stream, IDL_Sequence_Octet_4.Element_Of (Val, Integer (I)));
      end loop;
   end Compute_New_Size;
   procedure Marshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap5)
   is
   begin
      Marshall (Stream, IDL_Sequence_Octet_4.Sequence(Val));
   end Marshall;

   procedure Unmarshall
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Res : out Encap5)
   is
      Tmp : IDL_Sequence_Octet_4.Sequence;
   begin
      Unmarshall (Stream, Tmp);
      Res := Encap5(Tmp);
   end Unmarshall;

   procedure Compute_New_Size
      (Stream : in out Broca.Buffers.Buffer_descriptor;
       Val : Encap5)
   is
   begin
      Compute_New_Size (Stream, IDL_Sequence_Octet_4.Sequence(Val));
   end Compute_New_Size;

end M1.Stream;