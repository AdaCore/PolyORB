with CORBA;

with PolyORB.Smart_Pointers;

package PolyORB.CORBA_P.Policy is

   type Policy_Object_Type is new PolyORB.Smart_Pointers.Entity with private;

   type Policy_Object_Ptr is access all Policy_Object_Type'Class;

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.PolicyType;

   procedure Set_Policy_Type
     (Self   : in out Policy_Object_Type;
      Policy :        CORBA.PolicyType);

   function Get_Policy_Value
     (Self : Policy_Object_Type)
     return CORBA.Any;

   procedure Set_Policy_Value
     (Self  : in out Policy_Object_Type;
      Value :        CORBA.Any);

private

   type Policy_Object_Type is new PolyORB.Smart_Pointers.Entity with record
      Policy : CORBA.PolicyType;
      Value  : CORBA.Any;
   end record;

end PolyORB.CORBA_P.Policy;
