with CORBA;
with CORBA.Sequences.Unbounded;
with Broca.Types; use Broca.Types;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package Broca.Sequences is
   package IDL_SEQUENCE_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Seq_Octet : out IDL_SEQUENCE_Octet.Sequence);
   procedure Marshall
     (Stream : in out Buffer_Descriptor;
      Seq_Octet : in IDL_SEQUENCE_Octet.Sequence);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor;
      Seq_Octet : in IDL_SEQUENCE_Octet.Sequence);

   subtype Octet is IDL_SEQUENCE_Octet.Sequence;
   Null_Sequence : Octet renames IDL_SEQUENCE_Octet.Null_Sequence;
end Broca.Sequences;
