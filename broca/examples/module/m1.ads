with CORBA;
with Broca;
with CORBA.Sequences.Unbounded;
package M1 is
   type My_String is new CORBA.String;

   package IDL_Sequence_Octet is
      new CORBA.Sequences.Unbounded(CORBA.Octet);

   type Encap1 is new IDL_Sequence_Octet.Sequence;

   package IDL_Sequence_Octet_1 is
      new CORBA.Sequences.Unbounded(CORBA.Octet);

   type Encap2 is new IDL_Sequence_Octet_1.Sequence;

   package IDL_Sequence_Octet_4 is
      new CORBA.Sequences.Unbounded(CORBA.Octet);

   type Encap5 is new IDL_Sequence_Octet_4.Sequence;

end M1;