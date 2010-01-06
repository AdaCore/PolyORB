------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with CORBA.Object;
with CORBA.Policy;

with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;

with PortableServer.IdAssignmentPolicy;
with PortableServer.IdUniquenessPolicy;
with PortableServer.ImplicitActivationPolicy;
with PortableServer.LifespanPolicy;
with PortableServer.RequestProcessingPolicy;
with PortableServer.ServantRetentionPolicy;
with PortableServer.ThreadPolicy;

with PolyORB.Errors;
with PolyORB.POA;

package PortableServer.POA is

   type Local_Ref is new CORBA.Object.Ref with null record;

   AdapterAlreadyExists : exception;
   AdapterNonExistent   : exception;
   InvalidPolicy        : exception;
   NoServant            : exception;
   ObjectAlreadyActive  : exception;
   ObjectNotActive      : exception;
   ServantAlreadyActive : exception;
   ServantNotActive     : exception;
   WrongAdapter         : exception;
   WrongPolicy          : exception;

   --  POA creation and destruction

   function Create_POA
     (Self         : Local_Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Local_Ref;
      Policies     : CORBA.Policy.PolicyList)
     return Local_Ref'Class;

   function Find_POA
     (Self         : Local_Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Local_Ref'Class;

   procedure Destroy
     (Self                : in out Local_Ref;
      Etherealize_Objects : CORBA.Boolean;
      Wait_For_Completion : CORBA.Boolean);

   --  Factories for Policy objects

   function Create_Thread_Policy
     (Value : PortableServer.ThreadPolicyValue)
     return PortableServer.ThreadPolicy.Ref;

   function Create_Lifespan_Policy
     (Value : PortableServer.LifespanPolicyValue)
     return PortableServer.LifespanPolicy.Ref;

   function Create_Id_Uniqueness_Policy
     (Value : PortableServer.IdUniquenessPolicyValue)
     return PortableServer.IdUniquenessPolicy.Ref;

   function Create_Id_Assignment_Policy
     (Value : PortableServer.IdAssignmentPolicyValue)
     return PortableServer.IdAssignmentPolicy.Ref;

   function Create_Implicit_Activation_Policy
     (Value : PortableServer.ImplicitActivationPolicyValue)
     return PortableServer.ImplicitActivationPolicy.Ref;

   function Create_Servant_Retention_Policy
     (Value : PortableServer.ServantRetentionPolicyValue)
     return PortableServer.ServantRetentionPolicy.Ref;

   function Create_Request_Processing_Policy
     (Value : PortableServer.RequestProcessingPolicyValue)
     return PortableServer.RequestProcessingPolicy.Ref;

   --  POA attributes

   function Get_The_Name
     (Self : Local_Ref)
     return CORBA.String;

   function Get_The_Parent
     (Self : Local_Ref)
     return Local_Ref'Class;

   function Get_The_Children
     (Self : Local_Ref)
     return POAList;

   function Get_The_POAManager
     (Self : Local_Ref)
     return PortableServer.POAManager.Local_Ref;

   function Get_The_Activator
     (Self : Local_Ref)
     return PortableServer.AdapterActivator.Ref'Class;

   procedure Set_The_Activator
     (Self : Local_Ref;
      To   : access PortableServer.AdapterActivator.Ref'Class);

   --  Servant Manager registration

   function Get_Servant_Manager
     (Self : Local_Ref)
     return PortableServer.ServantManager.Local_Ref'Class;

   procedure Set_Servant_Manager
     (Self : Local_Ref;
      Imgr : PortableServer.ServantManager.Local_Ref'Class);

   --  operations for the USE_DEFAULT_SERVANT policy

   function Get_Servant
     (Self : Local_Ref)
     return Servant;

   procedure Set_Servant
     (Self      : Local_Ref;
      P_Servant : Servant);

   --  object activation and deactivation

   function Activate_Object
     (Self      : Local_Ref;
      P_Servant : Servant)
     return ObjectId;

   procedure Activate_Object_With_Id
     (Self      : Local_Ref;
      Oid       : ObjectId;
      P_Servant : Servant);

   procedure Deactivate_Object
     (Self : Local_Ref;
      Oid  : ObjectId);

   --  reference creation operations

   function Create_Reference
     (Self : Local_Ref;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id
     (Self : Local_Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   --  identity mapping operations

   function Servant_To_Id
     (Self      : Local_Ref;
      P_Servant : Servant)
     return ObjectId;

   function Servant_To_Reference
     (Self      : Local_Ref;
      P_Servant : Servant)
     return CORBA.Object.Ref;

   function Reference_To_Servant
     (Self      : Local_Ref;
      Reference : CORBA.Object.Ref'Class)
     return Servant;

   function Reference_To_Id
     (Self      : Local_Ref;
      Reference : CORBA.Object.Ref'Class)
     return ObjectId;

   function Id_To_Servant
     (Self : Local_Ref;
      Oid  : ObjectId)
     return Servant;

   function Id_To_Reference
     (Self : Local_Ref;
      Oid  : ObjectId)
     return CORBA.Object.Ref;

   ----------------------------------
   -- Convert from POA_Forward Ref --
   ----------------------------------

   package Convert is new
     PortableServer.POA_Forward.Convert (Local_Ref);

   ----------------------------------------------
   -- PortableServer.POA Exceptions Management --
   ----------------------------------------------

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String);

   --  AdapterAlreadyExists

   type AdapterAlreadyExists_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members);

   procedure Raise_AdapterAlreadyExists
     (Excp_Memb : AdapterAlreadyExists_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_AdapterAlreadyExists);

   --  AdapterNonExistent

   type AdapterNonExistent_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterNonExistent_Members);

   procedure Raise_AdapterNonExistent
     (Excp_Memb : AdapterNonExistent_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_AdapterNonExistent);

   --  InvalidPolicy

   type InvalidPolicy_Members is new CORBA.IDL_Exception_Members with record
      Index : CORBA.Unsigned_Short;
   end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidPolicy_Members);

   procedure Raise_InvalidPolicy
     (Excp_Memb : InvalidPolicy_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_InvalidPolicy);

   --  NoServant

   type NoServant_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NoServant_Members);

   procedure Raise_NoServant
     (Excp_Memb : NoServant_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_NoServant);

   --  ObjectAlreadyActive

   type ObjectAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectAlreadyActive_Members);

   procedure Raise_ObjectAlreadyActive
     (Excp_Memb : ObjectAlreadyActive_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_ObjectAlreadyActive);

   --  ObjectNotActive

   type ObjectNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectNotActive_Members);

   procedure Raise_ObjectNotActive
     (Excp_Memb : ObjectNotActive_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_ObjectNotActive);

   --  ServantAlreadyActive

   type ServantAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ServantAlreadyActive_Members);

   procedure Raise_ServantAlreadyActive
     (Excp_Memb : ServantAlreadyActive_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_ServantAlreadyActive);

   --  ServantNotActive

   type ServantNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ServantNotActive_Members);

   procedure Raise_ServantNotActive
     (Excp_Memb : ServantNotActive_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_ServantNotActive);

   --  WrongAdapter

   type WrongAdapter_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out WrongAdapter_Members);

   procedure Raise_WrongAdapter
     (Excp_Memb : WrongAdapter_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_WrongAdapter);

   --  WrongPolicy

   type WrongPolicy_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out WrongPolicy_Members);

   procedure Raise_WrongPolicy
     (Excp_Memb : WrongPolicy_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_WrongPolicy);

   Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableServer/POA:1.0";

   package Internals is

      function To_CORBA_POA
        (Referenced : PolyORB.POA.Obj_Adapter_Access)
         return Local_Ref;
      --  Convert a PolyORB.POA.Obj_Adapter_Access into
      --  a PortableServer.POA.Ref.

   end Internals;

end PortableServer.POA;
