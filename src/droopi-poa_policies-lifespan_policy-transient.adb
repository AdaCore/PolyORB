with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;
with Droopi.POA;
with Droopi.Types;

package body Droopi.POA_Policies.Lifespan_Policy.Transient is

   use CORBA.Policy_Values;
   use Droopi.Types;

   ------------
   -- Create --
   ------------

   function Create return Transient_Policy_Access
   is
      Policy : Transient_Policy_Access;
   begin
      Policy := new Transient_Policy'
        (Value => CORBA.Policy_Values.TRANSIENT);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Transient_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp
     (P  : Transient_Policy;
      OA : Droopi.POA_Types.Obj_Adapter_Access)
     return Time_Stamp is
   begin
      return Droopi.POA.Obj_Adapter_Access (OA).Boot_Time;
   end Get_Time_Stamp;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan
     (P     : Transient_Policy;
      OA    : Droopi.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access) is
   begin
      if U_Oid.Persistency_Flag
        /= Droopi.POA.Obj_Adapter_Access (OA).Boot_Time
      then
         Raise_Bad_Param;
      end if;
   end Ensure_Lifespan;

   ----------
   -- Free --
   ----------

   procedure Free
     (P   : in     Transient_Policy;
      Ptr : in out Policy_Access)
   is
   begin
      Free (Transient_Policy_Access (Ptr));
   end Free;

end Droopi.POA_Policies.Lifespan_Policy.Transient;
