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

end CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only;
