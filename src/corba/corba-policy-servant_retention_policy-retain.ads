with Ada.Unchecked_Deallocation;

package CORBA.Policy.Servant_Retention_Policy.Retain is

   type Retain_Policy is new ServantRetentionPolicy with null record;
   type Retain_Policy_Access is access all Retain_Policy;

   function Create return Retain_Policy_Access;
   procedure Check_Compatibility (Self : Retain_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

   function Activate_Object
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access;

   procedure Activate_Object_With_Id
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access;
      Oid       : Object_Id);

   procedure Deactivate
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      Oid       : Object_Id);

   function Servant_To_Id (Self      : Retain_Policy;
                           OA        : CORBA.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access;

   function Id_To_Servant (Self  : Retain_Policy;
                           OA    : CORBA.POA_Types.Obj_Adapter_Access;
                           U_Oid : Unmarshalled_Oid_Access)
                          return Servant_Access;

   procedure Free
     (P   : in     Retain_Policy;
      Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Retain_Policy,
      Retain_Policy_Access);

end CORBA.Policy.Servant_Retention_Policy.Retain;
