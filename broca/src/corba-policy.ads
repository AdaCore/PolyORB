with Corba;
with Corba.Object;
with Corba.Sequences.Unbounded;

package Corba.Policy is
   type Ref is new Corba.Object.Ref with null record;

   function Get_Policy_Type (Self: Ref) return Policytype;
   function Copy (Self: Ref) return Ref'Class;

   package IDL_SEQUENCE_Policy is new Corba.Sequences.Unbounded (Ref);
   type PolicyList is new IDL_SEQUENCE_Policy.Sequence;
end Corba.Policy;
