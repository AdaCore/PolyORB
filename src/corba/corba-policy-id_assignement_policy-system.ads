with Ada.Unchecked_Deallocation;

package CORBA.Policy.Id_Assignement_Policy.System is

   type System_Id_Policy is new IdAssignementPolicy with null record;
   type System_Id_Policy_Access is access all System_Id_Policy;

   function Create return System_Id_Policy_Access;

   procedure Check_Compatibility (Self : System_Id_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access);

   function Is_System (P : System_Id_Policy) return Boolean;

   function Activate_Object
     (Self   : System_Id_Policy;
      OA     : Droopi.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access) return Object_Id_Access;

   procedure Activate_Object_With_Id
     (Self   : System_Id_Policy;
      OA     : Droopi.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access;
      Oid    : Object_Id);

   procedure Ensure_Oid_Origin
     (Self  : System_Id_Policy;
      U_Oid : Unmarshalled_Oid_Access);

   procedure Ensure_Oid_Uniqueness
     (Self  : System_Id_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access);

   procedure Remove_Entry
     (Self  : System_Id_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access);

   function Id_To_Servant (Self  : System_Id_Policy;
                           OA    : Droopi.POA_Types.Obj_Adapter_Access;
                           U_Oid : Unmarshalled_Oid_Access)
                          return Servant_Access;

   procedure Free
     (P   : in     System_Id_Policy;
      Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (System_Id_Policy,
      System_Id_Policy_Access);

end CORBA.Policy.Id_Assignement_Policy.System;
