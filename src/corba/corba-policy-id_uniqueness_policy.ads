with CORBA.Policy_Values;           use CORBA.Policy_Values;
with Droopi.Objects;                use Droopi.Objects;
with CORBA.Object_Map;              use CORBA.Object_Map;

package CORBA.Policy.Id_Uniqueness_Policy is

   type Id_Uniqueness_Policy is abstract new Policy with private;
   type Id_Uniqueness_Policy_Ptr is access all Id_Uniqueness_Policy;

   procedure Ensure_Servant_Uniqueness
     (Self          : Id_Uniqueness_Policy_Ptr;
      Map           : CORBA.Object_Map.Object_Map;
      P_Servant     : Servant)
     is abstract;
   --  Case UNIQUE_ID:
   --  Checks that the specified servant is not yet in the Active Objects Map.
   --  If not, throws a ServantAlreadyActive exception.
   --  Case MULTIPLE_ID:
   --  Does nothing


private
   type Id_Uniqueness_Policy is new Policy with
      record
         Value : IdUniquenessPolicyValue;
      end record;


end CORBA.Policy.Id_Uniqueness_Policy;
