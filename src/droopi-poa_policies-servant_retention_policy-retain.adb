with Droopi.CORBA_P.Exceptions;          use Droopi.CORBA_P.Exceptions;
with Droopi.POA_Policies.Id_Assignment_Policy;
with Droopi.POA_Policies.Id_Uniqueness_Policy;
with Droopi.POA_Policies.Lifespan_Policy;
with Droopi.POA_Policies.Request_Processing_Policy;
with Droopi.POA_Policies.Implicit_Activation_Policy;
with Droopi.POA;

package body Droopi.POA_Policies.Servant_Retention_Policy.Retain is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Retain_Policy_Access
   is
      Policy : Retain_Policy_Access;
   begin
      Policy := new Retain_Policy'
        (Value => CORBA.Policy_Values.RETAIN);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Retain_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Retain_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
      use Droopi.POA_Policies.Id_Assignment_Policy;
      use Droopi.POA_Policies.Id_Uniqueness_Policy;

      POA : constant Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
   begin
      if not Is_System (POA.Id_Assignment_Policy.all) then
         Raise_Wrong_Policy;
      end if;
      Ensure_Servant_Uniqueness (POA.Id_Uniqueness_Policy.all,
                                 OA,
                                 P_Servant);
      return Activate_Object (POA.Id_Assignment_Policy.all, OA, P_Servant);
   end Activate_Object;

   ---------------------
   -- Activate_Object --
   ---------------------

   procedure Activate_Object_With_Id
     (Self      : Retain_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access;
      Oid       : Object_Id)
   is
      use Droopi.POA_Policies.Id_Assignment_Policy;
      use Droopi.POA_Policies.Id_Uniqueness_Policy;
      use Droopi.POA_Policies.Lifespan_Policy;

      POA       : constant Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      U_Oid     : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
   begin
      Ensure_Oid_Origin
        (POA.Id_Assignment_Policy.all, U_Oid);

      Ensure_Lifespan
        (POA.Lifespan_Policy.all, OA, U_Oid);

      Ensure_Oid_Uniqueness
        (POA.Id_Assignment_Policy.all, OA, U_Oid);

      Ensure_Servant_Uniqueness
        (POA.Id_Uniqueness_Policy.all, OA, P_Servant);

      Activate_Object_With_Id
        (POA.Id_Assignment_Policy.all, OA, P_Servant, Oid);

      Free (U_Oid);
   end Activate_Object_With_Id;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Self      : Retain_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      Oid       : Object_Id)
   is
      use Droopi.POA_Policies.Id_Assignment_Policy;
      use Droopi.POA_Policies.Request_Processing_Policy;

      POA       : constant Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);

      U_Oid     : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
   begin
      Etherealize_All
        (POA.Request_Processing_Policy.all, OA, U_Oid);

      --  In case a ServantManager is used
      Remove_Entry
        (POA.Id_Assignment_Policy.all, OA, U_Oid);
   end Deactivate;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : Retain_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
      use Droopi.POA_Policies.Id_Uniqueness_Policy;
      use Droopi.POA_Policies.Implicit_Activation_Policy;

      POA : constant Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      Oid : Object_Id_Access;
   begin
      Oid := Servant_To_Id
        (POA.Id_Uniqueness_Policy.all, OA, P_Servant);

      if Oid = null then
         Oid := Activate_Servant
           (POA.Implicit_Activation_Policy.all, OA, P_Servant);
      end if;

      return Oid;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self  : Retain_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access)
     return Servant_Access
   is
      use Droopi.POA_Policies.Id_Assignment_Policy;
   begin
      return Id_To_Servant
        (POA.Obj_Adapter_Access (OA).Id_Assignment_Policy.all,
         OA, U_Oid);
   end Id_To_Servant;

   ----------
   -- Free --
   ----------

   procedure Free
     (P   : in     Retain_Policy;
      Ptr : in out Policy_Access)
   is
   begin
      Free (Retain_Policy_Access (Ptr));
   end Free;

end Droopi.POA_Policies.Servant_Retention_Policy.Retain;

