with CORBA.POA;
with Droopi.CORBA_P.Exceptions;
with CORBA.Policy_Values;

package body CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only is

   use CORBA.Policy_Values;

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
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
      use Droopi.CORBA_P.Exceptions;
      Object : CORBA.POA.Obj_Adapter_Access
        := CORBA.POA.Obj_Adapter_Access (OA);
   begin
      if Object.Servant_Retention_Policy.Value /= RETAIN then
         Raise_Invalid_Policy;
      end if;

      null;
   end Check_Compatibility;

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
