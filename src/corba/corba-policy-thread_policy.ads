with CORBA.Policy_Values; use CORBA.Policy_Values;
with Droopi.Objects;      use Droopi.Objects;
with CORBA.POA_Types;

package CORBA.Policy.Thread_Policy is

   type ThreadPolicy is abstract new Policy with
     record
         Value       : ThreadPolicyValue;
     end record;
   subtype Thread_Policy is ThreadPolicy;
   type ThreadPolicy_Access is access all ThreadPolicy'Class;
   subtype Thread_Policy_Access is ThreadPolicy_Access;

   function Create (Value : ThreadPolicyValue)
                   return ThreadPolicy_Access;
   function Create (P : ThreadPolicy)
                   return ThreadPolicy_Access;
   --  The factory to create the different policies according to
   --  the value of Value

   function Create return ThreadPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Check_Compatibility (Self : ThreadPolicy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
      is abstract;

   procedure Free (P   : in     ThreadPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

end CORBA.Policy.Thread_Policy;
