with Ada.Unchecked_Deallocation;

package CORBA.Policy.Id_Uniqueness_Policy.Unique is

   type Unique_Id_Policy is new IdUniquenessPolicy with null record;
   type Unique_Id_Policy_Access is access all Unique_Id_Policy;

   function Create return Unique_Id_Policy_Access;

   procedure Check_Compatibility (Self : Unique_Id_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

   procedure Free (P   : in     Unique_Id_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation (Unique_Id_Policy,
                                                     Unique_Id_Policy_Access);

end CORBA.Policy.Id_Uniqueness_Policy.Unique;
