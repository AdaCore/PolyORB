package PortableServer is

   package POA_Forward is new CORBA.Forward;

   type Servant_Base is new Implementation_Defined;

   -- ... implementation defined, controlled type Servant is access
   --  all Servant_Base'CLASS;

   function "=" (Left, Right: Servant_Base) return Boolean;

   function Get_Default_POA (For_Servant : Servant_Base)
     return POA_Forward.Ref;

   function Get_Interface (For_Servant : Servant_Base)
     return CORBA.InterfaceDef.Ref;

   function Is_A (For_Servant : Servant_Base;
                  Logical_Type_ID : Standard.String)
     return Boolean;

   function Non_Existent (For_Servant : Servant_Base)
     return Boolean;

   package IDL_SEQUENCE_Octet is
     new CORBA.Sequences.Unbounded (CORBA.Octet);

   type ObjectId is new IDL_SEQUENCE_Octet.Sequence;

   ForwardRequest : exception;

   type ForwardRequest_Members is
    new CORBA.IDL_Exception_Members with
      record
         Forward_Reference : CORBA.Object.Ref;
      end record;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members);

   THREAD_POLICY_ID              : constant CORBA.PolicyType := 16;
   LIFESPAN_POLICY_ID            : constant CORBA.PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID       : constant CORBA.PolicyType := 18;
   ID_ASSIGNMENT_POLICY_ID       : constant CORBA.PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID : constant CORBA.PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID   : constant CORBA.PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID  : constant CORBA.PolicyType := 22;

   type ThreadPolicyValue is (ORB_CTRL_MODEL,
                              SINGLE_THREAD_MODEL);

   type LifespanPolicyValue is (TRANSIENT, PERSISTENT);
   type IdUniquenessPolicyValue is (UNIQUE_ID, MULTIPLE_ID);
   type IdAssignmentPolicyValue is (USER_ID, SYSTEM_ID);
   type ImplicitActivationPolicyValue is (IMPLICIT_ACTIVATION,
                                          NO_IMPLICIT_ACTIVATION);

   type ServantRetentionPolicyValue is (RETAIN, NON_RETAIN);
   type RequestProcessingPolicyValue is
     (USE_ACTIVE_OBJECT_MAP_ONLY,
      USE_DEFAULT_SERVANT,
      USE_SERVANT_MANAGER);

end PortableServer;
