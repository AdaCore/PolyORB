with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;
with CORBA.Object_Map.Sequence_Map;

with Droopi.POA;
with Droopi.POA_Policies.Lifespan_Policy;
with Droopi.Locks;                  use Droopi.Locks;
with Droopi.Types; use Droopi.Types;

package body Droopi.POA_Policies.Id_Assignment_Policy.System is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return System_Id_Policy_Access
   is
      Policy : System_Id_Policy_Access;
   begin
      Policy := new System_Id_Policy'
        (Value => CORBA.Policy_Values.SYSTEM_ID);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : System_Id_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Is_System --
   ---------------

   function Is_System (P : System_Id_Policy) return Boolean
   is
   begin
      return True;
   end Is_System;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self   : System_Id_Policy;
      OA     : Droopi.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access) return Object_Id_Access
   is
      use CORBA.Object_Map;
      use CORBA.Object_Map.Sequence_Map;
      use Droopi.POA_Policies.Lifespan_Policy;

      P_OA      : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      New_Entry : Seq_Object_Map_Entry_Access;
      New_U_Oid : Unmarshalled_Oid_Access;
      Index     : Integer;
   begin
      New_U_Oid         := new Unmarshalled_Oid;
      New_Entry         := new Seq_Object_Map_Entry;
      New_Entry.Oid     := New_U_Oid;
      New_Entry.Servant := Object;

      Lock_W (P_OA.Map_Lock);
      if P_OA.Active_Object_Map = null then
         P_OA.Active_Object_Map := Object_Map_Access (New_Map);
      end if;
      Index := Add (P_OA.Active_Object_Map.all'Access,
                    Object_Map_Entry_Access (New_Entry));

      New_U_Oid.Id := To_Droopi_String (Integer'Image (Index));
      New_U_Oid.System_Generated := True;
      New_U_Oid.Persistency_Flag := Get_Time_Stamp (P_OA.Lifespan_Policy.all,
                                                    OA);
      New_U_Oid.Creator := P_OA.Absolute_Address;
      Unlock_W (P_OA.Map_Lock);
      return U_Oid_To_Oid (New_U_Oid);
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self   : System_Id_Policy;
      OA     : Droopi.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access;
      Oid    : Object_Id)
   is
      use CORBA.Object_Map.Sequence_Map;
      use CORBA.Object_Map;
      P_OA      : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      New_U_Oid : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Oid);
      New_Entry : Seq_Object_Map_Entry_Access;
      Index     : Integer
        := Integer'Value (To_Standard_String (New_U_Oid.Id));
   begin
      New_Entry         := new Seq_Object_Map_Entry;
      New_Entry.Oid     := New_U_Oid;
      New_Entry.Servant := Object;
      Lock_W (P_OA.Map_Lock);
      Replace_By_Index (P_OA.Active_Object_Map.all'Access,
                        Object_Map_Entry_Access (New_Entry),
                        Index);
      Unlock_W (P_OA.Map_Lock);
   end Activate_Object_With_Id;

   -----------------------
   -- Ensure_Oid_Origin --
   -----------------------

   procedure Ensure_Oid_Origin
     (Self  : System_Id_Policy;
      U_Oid : Unmarshalled_Oid_Access)
   is
   begin
      if U_Oid.System_Generated = False then
         Raise_Bad_Param;
      end if;
   end Ensure_Oid_Origin;

   ---------------------------
   -- Ensure_Oid_Uniqueness --
   ---------------------------

   procedure Ensure_Oid_Uniqueness
     (Self  : System_Id_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access)
   is
      use CORBA.Object_Map.Sequence_Map;
      use CORBA.Object_Map;
      An_Entry : Object_Map_Entry_Access;
      P_OA      : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      Index : Integer := Integer'Value (To_Standard_String (U_Oid.Id));
   begin
      Lock_R (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      Unlock_R (P_OA.Map_Lock);
      if An_Entry /= null then
         Raise_Object_Already_Active;
      end if;
   end Ensure_Oid_Uniqueness;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry
     (Self  : System_Id_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access)
   is
      use CORBA.Object_Map.Sequence_Map;
      use CORBA.Object_Map;
      An_Entry : Object_Map_Entry_Access;
      Index    : Integer
        := Integer'Value (To_Standard_String (U_Oid.Id));
      P_OA     : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
   begin
      Lock_W (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry = null then
         Raise_Object_Not_Active;
      end if;
      An_Entry := Remove_By_Index (P_OA.Active_Object_Map.all'Access, Index);
      Unlock_W (P_OA.Map_Lock);
      --  Frees only the Unmarshalled_Oid_Access and the entry.
      --  The servant has to be freed by the application.
      Free (An_Entry.Oid);
      Free (Seq_Object_Map_Entry_Access (An_Entry));
   end Remove_Entry;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant (Self  : System_Id_Policy;
                           OA    : Droopi.POA_Types.Obj_Adapter_Access;
                           U_Oid : Unmarshalled_Oid_Access)
                          return Servant_Access
   is
      use CORBA.Object_Map.Sequence_Map;
      use CORBA.Object_Map;
      An_Entry : Object_Map_Entry_Access;
      Index    : Integer
        := Integer'Value (To_Standard_String (U_Oid.Id));
      P_OA     : Droopi.POA.Obj_Adapter_Access
        := Droopi.POA.Obj_Adapter_Access (OA);
      Servant  : Servant_Access;
   begin
      Lock_R (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry /= null then
         Servant := An_Entry.Servant;
      end if;
      Unlock_R (P_OA.Map_Lock);
      return Servant;
   end Id_To_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     System_Id_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (System_Id_Policy_Access (Ptr));
   end Free;

end Droopi.POA_Policies.Id_Assignment_Policy.System;
