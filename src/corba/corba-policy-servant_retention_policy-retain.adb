with Droopi.CORBA_P.Exceptions;          use Droopi.CORBA_P.Exceptions;
with CORBA.Policy.Id_Assignement_Policy;
with CORBA.Policy.Id_Uniqueness_Policy;
with CORBA.Policy.Lifespan_Policy;
with CORBA.Policy.Request_Processing_Policy;
with CORBA.Policy.Implicit_Activation_Policy;
with CORBA.POA;

package body CORBA.Policy.Servant_Retention_Policy.Retain is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Retain_Policy_Access
   is
      Policy : Retain_Policy_Access;
   begin
      Policy := new Retain_Policy'(Policy_Type =>
                                     SERVANT_RETENTION_POLICY_ID,
                                   Value =>
                                     CORBA.Policy_Values.RETAIN);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Retain_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
      use CORBA.Policy.Id_Assignement_Policy;
      use CORBA.Policy.Id_Uniqueness_Policy;
      POA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      if not Is_System (POA.Id_Assignement_Policy.all) then
         Raise_Wrong_Policy;
      end if;
      Ensure_Servant_Uniqueness (POA.Id_Uniqueness_Policy.all,
                                 OA,
                                 P_Servant);
      return Activate_Object (POA.Id_Assignement_Policy.all, OA, P_Servant);
   end Activate_Object;

   ---------------------
   -- Activate_Object --
   ---------------------

   procedure Activate_Object_With_Id
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access;
      Oid       : Object_Id)
   is
      use CORBA.Policy.Id_Assignement_Policy;
      use CORBA.Policy.Id_Uniqueness_Policy;
      use CORBA.Policy.Lifespan_Policy;
      POA       : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
      U_Oid     : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
   begin
      Ensure_Oid_Origin (POA.Id_Assignement_Policy.all,
                         U_Oid);
      Ensure_Lifespan (POA.Lifespan_Policy.all,
                       OA,
                       U_Oid);
      Ensure_Oid_Uniqueness (POA.Id_Assignement_Policy.all,
                             OA,
                             U_Oid);
      Ensure_Servant_Uniqueness (POA.Id_Uniqueness_Policy.all,
                                 OA,
                                 P_Servant);
      Activate_Object_With_Id (POA.Id_Assignement_Policy.all,
                               OA,
                               P_Servant,
                               Oid);
      Free (U_Oid);
   end Activate_Object_With_Id;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Self      : Retain_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      Oid       : Object_Id)
   is
      use CORBA.Policy.Id_Assignement_Policy;
      use CORBA.Policy.Request_Processing_Policy;
      U_Oid     : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
      POA       : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      Etherealize_All (POA.Request_Processing_Policy.all,
                       OA,
                       U_Oid);
      --  In case a ServantManager is used
      Remove_Entry (POA.Id_Assignement_Policy.all,
                    OA,
                    U_Oid);
   end Deactivate;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id (Self      : Retain_Policy;
                           OA        : CORBA.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access)
                          return Object_Id_Access
   is
      use CORBA.Policy.Id_Uniqueness_Policy;
      use CORBA.Policy.Implicit_Activation_Policy;
      POA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
      Oid : Object_Id_Access;
   begin
      Oid := Servant_To_Id (POA.Id_Uniqueness_Policy.all,
                            OA,
                            P_Servant);
      if Oid = null then
         Oid := Activate_Servant (POA.Implicit_Activation_Policy.all,
                                  OA,
                                  P_Servant);
      end if;
      return Oid;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant (Self  : Retain_Policy;
                           OA    : CORBA.POA_Types.Obj_Adapter_Access;
                           U_Oid : Unmarshalled_Oid_Access)
                          return Servant_Access
   is
      use CORBA.Policy.Id_Assignement_Policy;
      POA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      return Id_To_Servant (POA.Id_Assignement_Policy.all,
                            OA,
                            U_Oid);
   end Id_To_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Retain_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Retain_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Servant_Retention_Policy.Retain;

