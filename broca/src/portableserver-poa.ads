with Ada.Exceptions;
with CORBA.Object;
with Broca.Refs;
with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;
--  with PortableServer.ThreadPolicy;

package PortableServer.POA is
   type Ref is new CORBA.Object.Ref with null record;

   AdapterAlreadyExists : exception;

   type AdapterAlreadyExists_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members);

   AdapterNonExistent : exception;

   type AdapterNonExistent_Members is
     new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To   : out AdapterNonExistent_Members);

   function To_Ref (Self : CORBA.Object.Ref) return Ref;

   AdapterInactive : exception;
   ObjectAlreadyActive : exception;
   ObjectNotActive : exception;
   ServantAlreadyActive : exception;
   ServantNotActive : exception;
   WrongAdapter : exception;
   WrongPolicy : exception;
   NoServant : exception;

   function Create_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Tp           : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ip : IdAssignmentPolicyValue;
      Ap : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Ref'CLASS;

--    function Create_POA
--      (Self         : Ref;
--       Adapter_Name : CORBA.String;
--       A_POAManager : PortableServer.POAManager.Ref;
--       Value : ThreadPolicyValue)
--       Policies     : CORBA.Policy.PolicyList)
--      return Ref'CLASS;

   function Find_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Ref'CLASS;

   procedure Destroy
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);

   --  Policies

--    function Create_Thread_Policy
--      (Self  : Ref;
--       Value : ThreadPolicyValue)
--      return PortableServer.ThreadPolicy.Ref;

   function Get_The_Name (Self : Ref) return CORBA.String;

   function Get_The_Parent (Self : Ref) return Ref'CLASS;

   function Get_The_POAManager (Self : Ref)
     return PortableServer.POAManager.Ref;

   function Get_The_Activator (Self : Ref)
     return PortableServer.AdapterActivator.Ref;

   procedure Set_The_Activator
     (Self : in Ref;
      To   : in PortableServer.AdapterActivator.Ref);

   function Get_Servant_Manager (Self : Ref)
                                 return PortableServer.ServantManager.Ref;

   procedure Set_Servant_Manager
     (Self : in Ref;
      Imgr : in PortableServer.ServantManager.Ref);

   function Get_Servant (Self : Ref) return Servant;

   procedure Set_Servant (Self : in Ref;
                          P_Servant : in Servant);

   function Activate_Object
     (Self      : Ref;
      P_Servant : Servant) return ObjectId;

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant);

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId);

   function Create_Reference
     (Self : Ref;
      Intf : CORBA.RepositoryId) return CORBA.Object.Ref;

   function Create_Reference_With_Id
     (Self : Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId) return CORBA.Object.Ref;

   function Servant_To_Id
     (Self : Ref;
      P_Servant : Servant) return ObjectId;

   function Servant_To_Reference
     (Self      : Ref;
      P_Servant : Servant) return CORBA.Object.Ref;

   function Reference_To_Servant
     (Self      : Ref;
      Reference : CORBA.Object.Ref'CLASS) return Servant;

   function Reference_To_Id
     (Self      : Ref;
      Reference : CORBA.Object.Ref'CLASS) return ObjectId;

   function Id_To_Servant
     (Self : Ref;
      Oid : ObjectId) return Servant;

   function Id_To_Reference
     (Self : Ref;
      Oid : ObjectId) return CORBA.Object.Ref;

   package Convert is new PortableServer.POA_Forward.Convert (Ref);
end PortableServer.POA;
