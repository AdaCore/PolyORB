package CORBA.Policy_Types is

   --  Values from the standard IDL for module PortableServer.
   --  XXX These belong in package PortableServer -- there should
   --      not be a package CORBA.Policy_Types!

   THREAD_POLICY_ID               : constant PolicyType := 16;
   LIFESPAN_POLICY_ID             : constant PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID        : constant PolicyType := 18;
   ID_ASSIGNMENT_POLICY_ID        : constant PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID  : constant PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID    : constant PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID   : constant PolicyType := 22;

end CORBA.Policy_Types;
