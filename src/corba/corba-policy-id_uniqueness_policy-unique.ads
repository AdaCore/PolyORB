package CORBA.Policy.Id_Uniqueness_Policy.Unique is

   type Unique_Id_Policy is new IdUniquenessPolicy with null record;
   type Unique_Id_Policy_Access is access all Unique_Id_Policy;

   function Create return Unique_Id_Policy_Access;

   procedure Check_Compatibility (Self : Unique_Id_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

end CORBA.Policy.Id_Uniqueness_Policy.Unique;
