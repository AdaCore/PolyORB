with CORBA;
with CORBA.Sequences.Unbounded;
with Broca.Buffers; use Broca.Buffers;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package Broca.Sequences is

   package Octet_Sequences is new CORBA.Sequences.Unbounded (CORBA.Octet);

   subtype Octet_Sequence is Octet_Sequences.Sequence;

   Null_Sequence : Octet_Sequence renames Octet_Sequences.Null_Sequence;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Octet_Sequences.Sequence);

end Broca.Sequences;
