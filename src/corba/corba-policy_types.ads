package CORBA.Policy_Types is

   subtype PolicyType is CORBA.Short range 16 .. 22;

   THREAD_POLICY_ID               : constant PolicyType := 16;
   LIFESPAN_POLICY_ID             : constant PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID        : constant PolicyType := 18;
   ID_ASSIGNEMENT_POLICY_ID       : constant PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID  : constant PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID    : constant PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID   : constant PolicyType := 22;

end CORBA.Policy_Types;
