with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;

with CORBA.POA;
with CORBA.Policy_Types;

package body CORBA.Policy.Lifespan_Policy.Transient is

   use CORBA.Policy_Values;
   use CORBA.Policy_Types;

   ------------
   -- Create --
   ------------

   function Create return Transient_Policy_Access
   is
      Policy : Transient_Policy_Access;
   begin
      Policy := new Transient_Policy'(Policy_Type =>
                                        LIFESPAN_POLICY_ID,
                                      Value =>
                                        CORBA.Policy_Values.TRANSIENT);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Transient_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (P  : Transient_Policy;
                            OA : CORBA.POA_Types.Obj_Adapter_Access)
     return Time_Stamp
   is
      POA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      return POA.Boot_Time;
   end Get_Time_Stamp;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan (P     : Transient_Policy;
                              OA    : CORBA.POA_Types.Obj_Adapter_Access;
                              U_Oid : Unmarshalled_Oid_Access)
   is
      POA : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      if U_Oid.Persistency_Flag /= POA.Boot_Time then
         Raise_Bad_Param;
      end if;
   end Ensure_Lifespan;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Transient_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Transient_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Lifespan_Policy.Transient;
