with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;
with CORBA.Policy.Servant_Retention_Policy;
with CORBA.Policy_Values;
with Droopi.POA;
with CORBA.Policy_Types;

package body CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only is

   use CORBA.Policy_Values;
   use CORBA.Policy_Types;

   ------------
   -- Create --
   ------------

   function Create return Active_Map_Only_Policy_Access
   is
      Policy : Active_Map_Only_Policy_Access;
   begin
      Policy
        := new Active_Map_Only_Policy'(Policy_Type =>
                                         REQUEST_PROCESSING_POLICY_ID,
                                       Value =>
                                         CORBA.Policy_Values.
                                         USE_ACTIVE_OBJECT_MAP_ONLY);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Active_Map_Only_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
      use Droopi.CORBA_P.Exceptions;
      Object : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
   begin
      if Object.Servant_Retention_Policy.Value /= RETAIN then
         Raise_Invalid_Policy;
      end if;

      null;
   end Check_Compatibility;

   ---------------------
   -- Etherealize_All --
   ---------------------

   procedure Etherealize_All (Self  : Active_Map_Only_Policy;
                              OA    : Droopi.POA_Types.Obj_Adapter_Access;
                              U_Oid : Unmarshalled_Oid_Access)
   is
   begin
      null;
   end Etherealize_All;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id (Self  : Active_Map_Only_Policy;
                           OA    : Droopi.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access
   is
      use CORBA.Policy.Servant_Retention_Policy;
      POA : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      Oid : Object_Id_Access;
   begin
      Oid := Servant_To_Id (POA.Servant_Retention_Policy.all,
                            OA,
                            P_Servant);
      return Oid;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant (Self : Active_Map_Only_Policy;
                           OA   : Droopi.POA_Types.Obj_Adapter_Access;
                           Oid  : Object_Id) return Servant_Access
   is
      use CORBA.Policy.Servant_Retention_Policy;
      POA     : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      U_Oid   : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
      Servant : Servant_Access;
   begin
      Servant := Id_To_Servant (POA.Servant_Retention_Policy.all,
                                OA,
                                U_Oid);
      if Servant = null then
         Raise_Object_Not_Active;
      end if;
      return Servant;
   end Id_To_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Active_Map_Only_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Active_Map_Only_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only;
