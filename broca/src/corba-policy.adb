with Broca.Policy;
with Broca.Object;

package body CORBA.Policy is
   function Get_Policy_Type (Self: Ref) return Policytype is
   begin
      return Broca.Policy.Get_Policy_Type
        (Broca.Policy.Policy_Object_Type (Self.A_Ref.all));
   end Get_Policy_Type;

   function Copy (Self: Ref) return Ref is
   begin
      return Ref'(A_Ref => Broca.Object.Ref_Ptr (Broca.Policy.Copy
                  (Broca.Policy.Policy_Object_Type'Class (Self.A_Ref.all))));
   end Copy;
end CORBA.Policy;
