with PolyORB.CORBA_P.Exceptions; use PolyORB.CORBA_P.Exceptions;
with PolyORB.POA_Policies.Servant_Retention_Policy;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
with PolyORB.POA;

package body
  PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only
is

   ------------
   -- Create --
   ------------

   function Create return Active_Map_Only_Policy_Access is
   begin
      return new Active_Map_Only_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Active_Map_Only_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
      use PolyORB.CORBA_P.Exceptions;

   begin
      if not
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all in
         POA_Policies.Servant_Retention_Policy.Retain.Retain_Policy)
      then
         Raise_Invalid_Policy;
      end if;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Active_Map_Only_Policy)
     return String is
   begin
      return "REQUEST_PROCESSING_POLICY.ACTIVE_MAP_ONLY";
   end Policy_Id;

   ---------------------
   -- Etherealize_All --
   ---------------------

   procedure Etherealize_All
     (Self  : Active_Map_Only_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access)
   is
   begin
      null;
   end Etherealize_All;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self  : Active_Map_Only_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access) return Object_Id_Access
   is
      use PolyORB.POA_Policies.Servant_Retention_Policy;

      Oid : constant Object_Id_Access
        := Servant_To_Id
        (POA.Obj_Adapter_Access
         (OA).Servant_Retention_Policy.all,
         OA,
         P_Servant);
   begin
      return Oid;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : Active_Map_Only_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access;
      Oid  : Object_Id) return Servant_Access
   is
      use PolyORB.POA_Policies.Servant_Retention_Policy;

      U_Oid   : constant Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
      Servant : Servant_Access;
   begin
      Servant := Id_To_Servant
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all,
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

   procedure Free
     (P   : in     Active_Map_Only_Policy;
      Ptr : in out Policy_Access)
   is
   begin
      Free (Active_Map_Only_Policy_Access (Ptr));
   end Free;

end PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
