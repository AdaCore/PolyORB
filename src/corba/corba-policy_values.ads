package CORBA.Policy_Values is

   type Policy_Value is
     (ORB_CTRL_MODEL,
      SINGLE_THREAD_MODEL,
      MAIN_THREAD_MODEL,
      TRANSIENT,
      PERSISTENT,
      UNIQUE_ID,
      MULTIPLE_ID,
      USER_ID,
      SYSTEM_ID,
      IMPLICIT_ACTIVATION,
      NO_IMPLICIT_ACTIVATION,
      RETAIN,
      NON_RETAIN,
      USE_ACTIVE_OBJECT_MAP_ONLY,
      USE_DEFAULT_SERVANT,
      USE_SERVANT_MANAGER);

   subtype ThreadPolicyValue is Policy_Value
     range ORB_CTRL_MODEL .. MAIN_THREAD_MODEL;
   subtype LifespanPolicyValue is Policy_Value
     range TRANSIENT .. PERSISTENT;
   subtype IdUniquenessPolicyValue is Policy_Value
     range UNIQUE_ID .. MULTIPLE_ID;
   subtype IdAssignementPolicyValue is Policy_Value
     range USER_ID .. SYSTEM_ID;
   subtype ImplicitActivationPolicyValue is Policy_Value
     range IMPLICIT_ACTIVATION .. NO_IMPLICIT_ACTIVATION;
   subtype ServantRetentionPolicyValue is Policy_Value
     range RETAIN .. NON_RETAIN;
   subtype RequestProcessingPolicyValue is Policy_Value
     range USE_ACTIVE_OBJECT_MAP_ONLY .. USE_SERVANT_MANAGER;

end CORBA.Policy_Values;
