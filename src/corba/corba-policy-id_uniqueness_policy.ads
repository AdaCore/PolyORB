with CORBA.Policy_Values;           use CORBA.Policy_Values;
with Droopi.Objects;                use Droopi.Objects;
with CORBA.Object_Map;              use CORBA.Object_Map;

package CORBA.Policy.Id_Uniqueness_Policy is

   type IdUniquenessPolicy is abstract new Policy with private;
   type IdUniquenessPolicy_Access is access all IdUniquenessPolicy;

   procedure Ensure_Servant_Uniqueness
     (Self          : in out IdUniquenessPolicy_Access;
      Map           : in out CORBA.Object_Map.Object_Map;
      P_Servant     : in Servant)
     is abstract;
   --  Case UNIQUE_ID:
   --  Checks that the specified servant is not yet in the Active Objects Map.
   --  If not, throws a ServantAlreadyActive exception.
   --  Case MULTIPLE_ID:
   --  Does nothing


private
   type IdUniquenessPolicy is new Policy with
      record
         Value : IdUniquenessPolicyValue;
      end record;


end CORBA.Policy.Id_Uniqueness_Policy;
