with PolyORB.CORBA_P.Exceptions; use PolyORB.CORBA_P.Exceptions;
--  XXX remove dep on CORBA_P!

with PolyORB.POA;
with PolyORB.Types;

package body PolyORB.POA_Policies.Lifespan_Policy.Transient is

   use PolyORB.Types;

   ------------
   -- Create --
   ------------

   function Create return Transient_Policy_Access is
   begin
      return new Transient_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Transient_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Transient_Policy)
     return String is
   begin
      return "LIFESPAN_POLICY.TRANSIENT";
   end Policy_Id;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp
     (P  : Transient_Policy;
      OA : PolyORB.POA_Types.Obj_Adapter_Access)
     return Time_Stamp is
   begin
      return PolyORB.POA.Obj_Adapter_Access (OA).Boot_Time;
   end Get_Time_Stamp;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan
     (P     : Transient_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access) is
   begin
      if U_Oid.Persistency_Flag
        /= PolyORB.POA.Obj_Adapter_Access (OA).Boot_Time
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

end PolyORB.POA_Policies.Lifespan_Policy.Transient;
