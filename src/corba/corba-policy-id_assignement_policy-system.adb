package body CORBA.Policy.Id_Assignement_Policy.System is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return System_Id_Policy_Access
   is
      Policy : System_Id_Policy_Access;
   begin
      Policy := new System_Id_Policy'(Policy_Type =>
                                        ID_ASSIGNEMENT_POLICY_ID,
                                      Value =>
                                        CORBA.Policy_Values.SYSTEM_ID);
      return Policy;
   end Create;

end CORBA.Policy.Id_Assignement_Policy.System;
