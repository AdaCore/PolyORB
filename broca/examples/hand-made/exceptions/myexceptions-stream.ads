with Broca.Buffers;
with myexceptions;
with Broca.Marshalling;

package myexceptions.Stream is
   procedure Marshall
      (Stream : in out Broca.Buffers.Buffer_Descriptor;
       Val : toto_Members);

   procedure Unmarshall
      (Stream : in out Broca.Buffers.Buffer_Descriptor;
       Res : out toto_Members);

   procedure Compute_New_Size
      (Stream : in out Broca.Buffers.Buffer_Descriptor;
       Val : toto_Members);

   procedure Unmarshall_And_Raise_toto
      (Stream : in out Broca.Buffers.Buffer_Descriptor);

   procedure Raise_toto
      (Bod : toto_Members);
end myexceptions.Stream;
