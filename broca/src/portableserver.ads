with Ada.Finalization;
with CORBA;
with CORBA.Forward;
with CORBA.Object;
with Broca.Sequences;
with Broca.Buffers;
pragma Elaborate_All (CORBA.Forward);

package PortableServer is

   package POA_Forward is new CORBA.Forward;

   --  21.41.1
   --  Conforming implementations must provide a controlled (tagged)
   --  Servant_Base type and default implementations of the primitve
   --  operations on Servant_Base that meet the required semantics.
   type Servant_Base is new Ada.Finalization.Controlled with private;

   --  Get the type_id.
   function Get_Type_Id (Obj : Servant_Base) return CORBA.RepositoryId;

   --  Call an operation.
   --  Only standard exceptions (defined in module CORBA) can be caught
   --  outside of GIOP_DISPATCH, ie user defined exception must be marshalled.
   procedure GIOP_Dispatch
     (Obj : access Servant_Base;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Buffers.Buffer_Descriptor);

   type Servant is access all Servant_Base'Class;

   --  FIXME: how to implement this ?
   --  function "=" (Left, Right : Servant) return Boolean;
   --  pragma Convention (Intrinsic, "=");

   function Get_Default_POA (For_Servant : Servant_Base)
     return POA_Forward.Ref;

   type ObjectId is new Broca.Sequences.Octet_Sequence;

   ForwardRequest : exception;

   type ForwardRequest_Members is new CORBA.IDL_Exception_Members with
      record
         Forward_Reference : CORBA.Object.Ref;
      end record;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members);

   --  Calling ForwardRequest does not increase the usage counter of
   --  REFERENCE.  As a result, the user must ensure not to release
   --  REFERENCE while the exception is processed.
   procedure Raise_Forward_Request (Reference : CORBA.Object.Ref);
   pragma No_Return (Raise_Forward_Request);

   THREAD_POLICY_ID              : constant CORBA.PolicyType := 16;
   LIFESPAN_POLICY_ID            : constant CORBA.PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID       : constant CORBA.PolicyType := 18;
   ID_ASSIGNMENT_POLICY_ID       : constant CORBA.PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID : constant CORBA.PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID   : constant CORBA.PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID  : constant CORBA.PolicyType := 22;

   type ThreadPolicyValue is (ORB_CTRL_MODEL, SINGLE_THREAD_MODEL);
   type LifespanPolicyValue is (TRANSIENT, PERSISTENT);
   type IdUniquenessPolicyValue is (UNIQUE_ID, MULTIPLE_ID);
   type IdAssignmentPolicyValue is (USER_ID, SYSTEM_ID);
   type ImplicitActivationPolicyValue is
      (IMPLICIT_ACTIVATION, NO_IMPLICIT_ACTIVATION);
   type ServantRetentionPolicyValue is (RETAIN, NON_RETAIN);
   type RequestProcessingPolicyValue is
     (USE_ACTIVE_OBJECT_MAP_ONLY, USE_DEFAULT_SERVANT, USE_SERVANT_MANAGER);

private
   type Servant_Base is new Ada.Finalization.Controlled with
     null record;
end PortableServer;
