with CORBA.Policy_Values;           use CORBA.Policy_Values;
with CORBA.Object_Map;              use CORBA.Object_Map;

package CORBA.Policy.Id_Uniqueness_Policy is

   type IdUniquenessPolicy is abstract new Policy with
     record
         Value : IdUniquenessPolicyValue;
     end record;
   subtype Id_Uniqueness_Policy is IdUniquenessPolicy;
   type IdUniquenessPolicy_Access is access all IdUniquenessPolicy'Class;
   subtype Id_Uniqueness_Policy_Access is IdUniquenessPolicy_Access;

   function Create (Value : IdUniquenessPolicyValue)
                   return IdUniquenessPolicy_Access;
   function Create (P : IdUniquenessPolicy)
                   return IdUniquenessPolicy_Access;
   --  The factory to create the different policies according to
   --  the value of Value

   function Create return IdUniquenessPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Free (P   : in     IdUniquenessPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

--    procedure Ensure_Servant_Uniqueness
--      (Self          : in out IdUniquenessPolicy_Access;
--       Map           : in out CORBA.Object_Map.Object_Map;
--       P_Servant     : in Servant)
--      is abstract;
--    --  Case UNIQUE_ID:
--    --  Checks that the specified servant is not yet in the Active Objects Map.
--    --  If not, throws a ServantAlreadyActive exception.
--    --  Case MULTIPLE_ID:
--    --  Does nothing

end CORBA.Policy.Id_Uniqueness_Policy;
