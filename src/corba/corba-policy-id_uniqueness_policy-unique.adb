with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;
with Droopi.Locks;              use Droopi.Locks;

with CORBA.POA;
with CORBA.Object_Map.Sequence_Map;

package body CORBA.Policy.Id_Uniqueness_Policy.Unique is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Unique_Id_Policy_Access
   is
      Policy : Unique_Id_Policy_Access;
   begin
      Policy := new Unique_Id_Policy'(Policy_Type =>
                                        ID_UNIQUENESS_POLICY_ID,
                                      Value =>
                                        CORBA.Policy_Values.UNIQUE_ID);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Unique_Id_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   -------------------------------
   -- Ensure_Servant_Uniqueness --
   -------------------------------

   procedure Ensure_Servant_Uniqueness
     (Self      : Unique_Id_Policy;
      OA        : CORBA.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
   is
      P_OA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      if P_OA.Active_Object_Map /= null then
         Lock_R (P_OA.Map_Lock);
         if Is_Servant_In (P_OA.Active_Object_Map.all, P_Servant) then
            Raise_Servant_Already_Active;
         end if;
         Unlock_R (P_OA.Map_Lock);
      end if;
   end Ensure_Servant_Uniqueness;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id (Self      : Unique_Id_Policy;
                           OA        : CORBA.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access
   is
      use CORBA.Object_Map;
      P_OA        : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
      An_Entry    : Object_Map_Entry_Access;
   begin
      if P_OA.Active_Object_Map /= null then
         Lock_R (P_OA.Map_Lock);
         An_Entry := Get_By_Servant (P_OA.Active_Object_Map.all, P_Servant);
         if An_Entry /= null then
            return U_Oid_To_Oid (An_Entry.Oid);
         end if;
         Unlock_R (P_OA.Map_Lock);
      end if;
      return null;
   end Servant_To_Id;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Unique_Id_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Unique_Id_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Id_Uniqueness_Policy.Unique;
