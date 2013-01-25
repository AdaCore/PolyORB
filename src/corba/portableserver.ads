------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

pragma Ada_2005;

with Ada.Exceptions;

with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);
with CORBA.IDL_SEQUENCES;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ServerRequest;
with CORBA.Sequences.Unbounded;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Objects;
with PolyORB.Requests;

package PortableServer is

   pragma Elaborate_Body;

   --  Forward declaration

   package POA_Forward is new CORBA.Forward;

   package IDL_SEQUENCE_PortableServer_POA_Forward is new
     CORBA.Sequences.Unbounded (POA_Forward.Ref);

   type POAList is new IDL_SEQUENCE_PortableServer_POA_Forward.Sequence;

   ForwardRequest : exception;
   NotAGroupObject : exception;

   ---------------------------
   -- DynamicImplementation --
   ---------------------------

   --  The root of all implementation objects:
   --  DynamicImplementation.

   type DynamicImplementation is
     abstract new CORBA.Impl.Object with private;

   procedure Invoke
     (Self    : access DynamicImplementation;
      Request : CORBA.ServerRequest.Object_Ptr)
      is abstract;

   type Servant is access all DynamicImplementation'Class;
   --  The root of all static implementations: Servant_Base,
   --  a type derived from DynamicImplementation (which provides
   --  a default implementation of the Invoke operation.)

   type Servant_Base is
     abstract new DynamicImplementation with private;
   --  21.41.1
   --  Conforming implementations must provide a controlled (tagged)
   --  Servant_Base type and default implementations of the primitive
   --  operations on Servant_Base that meet the required semantics.

   overriding procedure Invoke
     (Self    : access Servant_Base;
      Request : CORBA.ServerRequest.Object_Ptr);

   --  XXX What is the status of these commented spec ?

   --  FIXME: how to implement this ?
   --  function "=" (Left, Right : Servant) return Boolean;
   --  pragma Convention (Intrinsic, "=");

   --  function Get_Default_POA
   --   (For_Servant : Servant_Base)
   --    return POA_Forward.Ref;

   --     function Get_Interface
   --       (For_Servant : Servant_Base)
   --       return CORBA.InterfaceDef.Ref;

   --     function Is_A
   --       (For_Servant : Servant_Base;
   --        Logical_Type_ID : Standard.String)
   --       return Boolean;

   --     function Non_Existent
   --       (For_Servant : Servant_Base)
   --       return Boolean;

   --------------
   -- ObjectId --
   --------------

   type ObjectId is new CORBA.IDL_SEQUENCES.OctetSeq;

   function String_To_ObjectId (Id : String) return ObjectId;
   --  Convert string Id into an ObjectId.

   function ObjectId_To_String (Id : ObjectId) return String;
   --  Convert ObjectId Id into a string.

   --  Implementation Notes: these functions are not defined in the
   --  CORBA specification, but defined in various C++ ORB
   --  implementations. They are provided as a facility.

   type ObjectId_Access is access ObjectId;

   package Sequence_IDs is new CORBA.Sequences.Unbounded (ObjectId);
   --  XXX Part of the MIOP specifications. Should be moved to
   --  package PortableGroup.

   type IDs is new Sequence_IDs.Sequence;
   --  XXX Part of the MIOP specifications. Should be moved to
   --  package PortableGroup.

   ---------------
   -- Constants --
   ---------------

   THREAD_POLICY_ID              : constant CORBA.PolicyType := 16;
   LIFESPAN_POLICY_ID            : constant CORBA.PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID       : constant CORBA.PolicyType := 18;
   ID_ASSIGNMENT_POLICY_ID       : constant CORBA.PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID : constant CORBA.PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID   : constant CORBA.PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID  : constant CORBA.PolicyType := 22;

   type ThreadPolicyValue is
     (ORB_CTRL_MODEL,
      SINGLE_THREAD_MODEL,
      MAIN_THREAD_MODEL);

   type LifespanPolicyValue is
     (TRANSIENT,
      PERSISTENT);

   type IdUniquenessPolicyValue is
     (UNIQUE_ID,
      MULTIPLE_ID);

   type IdAssignmentPolicyValue is
     (USER_ID,
      SYSTEM_ID);

   type ImplicitActivationPolicyValue is
     (IMPLICIT_ACTIVATION,
      NO_IMPLICIT_ACTIVATION);

   type ServantRetentionPolicyValue is
     (RETAIN,
      NON_RETAIN);

   type RequestProcessingPolicyValue is
     (USE_ACTIVE_OBJECT_MAP_ONLY,
      USE_DEFAULT_SERVANT,
      USE_SERVANT_MANAGER);

   ------------------------------------------
   -- PortableServer Exceptions Management --
   ------------------------------------------

   type ForwardRequest_Members is new CORBA.IDL_Exception_Members with record
      Forward_Reference : CORBA.Object.Ref;
   end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ForwardRequest_Members);

   type NotAGroupObject_Members is new CORBA.IDL_Exception_Members
     with null record;
   --  XXX Part of the MIOP specifications. Should be moved to
   --  package PortableGroup.

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotAGroupObject_Members);
   --  XXX Part of the MIOP specifications. Should be moved to
   --  package PortableGroup.

   procedure Raise_NotAGroupObject
     (Excp_Memb : NotAGroupObject_Members);
   pragma No_Return (Raise_NotAGroupObject);
   --  XXX Part of the MIOP specifications. Should be moved to
   --  package PortableGroup.

   --  XXX What is the status of this comment ??

   --  Calling ForwardRequest does not increase the usage counter of
   --  REFERENCE.  As a result, the user must ensure not to release
   --  REFERENCE while the exception is processed.
   --  There is a dilemna here:
   --  - if we increase the counter, the usage counter will never
   --    be decreased if get_members is not called
   --  - if we do not increase it, the object may be deleted
   --    before the exception is caught.

   package Internals is

      --  Implementation Note: This package defines internal subprograms
      --  specific to PolyORB. You must not use them.

      type Request_Dispatcher is access procedure
        (For_Servant : Servant;
         Request     : CORBA.ServerRequest.Object_Ptr);
      --  Same signature as primitive Invoke of type
      --  DynamicImplementation.

      type Servant_Class_Predicate is access function
        (For_Servant : Servant) return Boolean;

      type Servant_Class_Is_A_Operation is access function
        (Logical_Type_Id : Standard.String) return CORBA.Boolean;

      procedure Register_Skeleton
        (Type_Id     : String;
         Is_A        : Servant_Class_Predicate;
         Target_Is_A : Servant_Class_Is_A_Operation;
         Dispatcher  : Request_Dispatcher := null);
      --  Associate a type id with a class predicate.
      --  A Dispatcher function can also be specified if the class predicate
      --  corresponds to a class derived from PortableServer.Servant_Base.
      --  For classes derived from PortableServer.DynamicImplementation, the
      --  user must override the Invoke operation himself, and the Dispatcher
      --  must be set to null.
      --  NOTE: This procedure is not thread safe.

      function Get_Type_Id (For_Servant : Servant) return Standard.String;

      --  Subprograms for PortableInterceptor implementation

      function Target_Most_Derived_Interface
        (For_Servant : Servant) return Standard.String;
      --  Return Type_Id of most derived servant interface

      function Target_Is_A
        (For_Servant     : Servant;
         Logical_Type_Id : Standard.String) return CORBA.Boolean;
      --  Check is servant support specified interface

      function To_PortableServer_ObjectId
        (Id : PolyORB.Objects.Object_Id) return ObjectId;
      --  Convert neutral Object_Id into PortableServer's ObjectId

      function To_PolyORB_Object_Id
        (Id : ObjectId) return PolyORB.Objects.Object_Id;
      --  Convert PortableServer's ObjectId into neutral Object_Id

   end Internals;

private

   type DynamicImplementation is
     abstract new CORBA.Impl.Object with null record;

   overriding function Execute_Servant
     (Self : not null access DynamicImplementation;
      Req  : PolyORB.Requests.Request_Access) return Boolean;

   type Servant_Base is
     abstract new DynamicImplementation with null record;

   type PortableServer_Current_Note is new PolyORB.Annotations.Note with record
      Request : PolyORB.Requests.Request_Access;
      Profile : PolyORB.Binding_Data.Profile_Access;
   end record;

   Null_PortableServer_Current_Note : constant PortableServer_Current_Note
     := (PolyORB.Annotations.Note with
          Request => null,
          Profile => null);

   PortableServer_Current_Registered : Boolean := False;

end PortableServer;
