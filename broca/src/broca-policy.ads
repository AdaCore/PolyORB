with CORBA;
with Broca.Object;

package Broca.Policy is

   type Policy_Object_Type is abstract new Broca.Object.Ref_Type with
     record
        Policy : CORBA.Policytype;
     end record;

   type Policy_Object_Ptr is access all Policy_Object_Type'Class;

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.Policytype;

   function Copy
     (Self : Policy_Object_Type)
     return Policy_Object_Ptr is abstract;

end Broca.Policy;
