package body Broca.Policy is
   function Get_Policy_Type (Self: Policy_Object_Type)
                             return Corba.Policytype is
   begin
      return Self.Policy;
   end Get_Policy_Type;
end Broca.Policy;
