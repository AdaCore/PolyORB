with Ada.Unchecked_Deallocation;

package CORBA.Policy.Servant_Retention_Policy.Retain is

   type Retain_Policy is new ServantRetentionPolicy with null record;
   type Retain_Policy_Access is access all Retain_Policy;

   function Create return Retain_Policy_Access;
   procedure Check_Compatibility (Self : Retain_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

   procedure Free (P   : in     Retain_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation (Retain_Policy,
                                                     Retain_Policy_Access);

end CORBA.Policy.Servant_Retention_Policy.Retain;
