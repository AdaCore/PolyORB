with Broca.Marshalling;

package body Broca.Sequences is
   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Seq_Octet : out IDL_SEQUENCE_Octet.Sequence)
   is
      use Broca.Marshalling;

      Length : CORBA.Unsigned_Long;
      Res : IDL_SEQUENCE_Octet.Sequence;
      El : CORBA.Octet;
   begin
      Unmarshall (Buffer, Length);
      Res := IDL_SEQUENCE_Octet.To_Sequence (Natural (Length));
      for I in 1 .. Length loop
         Unmarshall (Buffer, El);
         IDL_SEQUENCE_Octet.Replace_Element (Res, Natural (I), El);
      end loop;
      Seq_Octet := Res;
   end Unmarshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor;
      Seq_Octet : in IDL_SEQUENCE_Octet.Sequence)
   is
      use Broca.Marshalling;
      Length : Natural;
   begin
      Length := IDL_SEQUENCE_Octet.Length (Seq_Octet);
      Marshall (Stream, CORBA.Unsigned_Long (Length));
      for I in 0 .. Length - 1 loop
         --  Low bound is 1.
         Stream.Buffer (Stream.Pos + Buffer_Index_Type (I)) :=
           Types.Element (IDL_SEQUENCE_Octet.Element_Of (Seq_Octet, 1 + I));
      end loop;
      Stream.Pos := Stream.Pos + Buffer_Index_Type (Length);
   end Marshall;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor;
      Seq_Octet : in IDL_SEQUENCE_Octet.Sequence)
   is
      use Broca.Marshalling;
      Length : Natural;
   begin
      Length := IDL_SEQUENCE_Octet.Length (Seq_Octet);
      Marshall_Size_Unsigned_Long (Stream);
      Stream.Pos := Stream.Pos + Buffer_Index_Type (Length);
   end Marshall_Size;

end Broca.Sequences;
