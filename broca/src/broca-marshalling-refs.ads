with Broca.Refs;    use Broca.Refs;
with Broca.Buffers; use Broca.Buffers;

package Broca.Marshalling.Refs is

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Refs.Ref'Class);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Refs.Ref'Class);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Broca.Refs.Ref'Class);

end Broca.Marshalling.Refs;
