with CORBA;
with CORBA.Object;
with CORBA.Sequences.Unbounded;

package CORBA.Policy is
   type Ref is new CORBA.Object.Ref with null record;

   function Get_Policy_Type (Self: Ref) return Policytype;
   function Copy (Self: Ref) return Ref'Class;

   package IDL_SEQUENCE_Policy is new CORBA.Sequences.Unbounded (Ref);
   type PolicyList is new IDL_SEQUENCE_Policy.Sequence;
end CORBA.Policy;
