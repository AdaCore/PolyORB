------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Exceptions;

with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);

with CORBA.Impl;
with CORBA.Object;
with CORBA.ServerRequest;
with CORBA.Sequences.Unbounded;

pragma Warnings (Off);              --  WAG:3.15
with PolyORB.Any;                   --  WAG:3.15
pragma Elaborate_All (PolyORB.Any); --  WAG:3.15
pragma Warnings (On);               --  WAG:3.15

with PolyORB.Components;
with PolyORB.Objects;

package PortableServer is

   pragma Elaborate_Body;

   --  forward declaration

   package POA_Forward is new CORBA.Forward;

   package IDL_Sequence_POA_Forward is new
     CORBA.Sequences.Unbounded (POA_Forward.Ref);

   subtype POAList is IDL_Sequence_POA_Forward.Sequence;

   ---------------------------
   -- DynamicImplementation --
   ---------------------------

   --  The root of all implementation objects:
   --  DynamicImplementation.

   type DynamicImplementation is
     abstract new CORBA.Impl.Object with private;

   procedure Invoke
     (Self    : access DynamicImplementation;
      Request : in CORBA.ServerRequest.Object_Ptr)
      is abstract;

   type Servant is access all DynamicImplementation'Class;
   --  The root of all static implementations: Servant_Base,
   --  a type derived from DynamicImplementation (which provides
   --  a default implementation of the Invoke operation.)

   type Servant_Base is
     abstract new DynamicImplementation with private;
   --  21.41.1
   --  Conforming implementations must provide a controlled (tagged)
   --  Servant_Base type and default implementations of the primitve
   --  operations on Servant_Base that meet the required semantics.

   procedure Invoke
     (Self    : access Servant_Base;
      Request : in     CORBA.ServerRequest.Object_Ptr);

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

   type ObjectId is new PolyORB.Objects.Object_Id;

   function String_To_ObjectId (Id : String) return ObjectId;
   --  Convert string Id into an ObjectID.

   function ObjectId_To_String (Id : ObjectId) return String;
   --  Convert ObjectId Id into a string.

   --  XXX these functions are not defined in the CORBA specification,
   --  but defined in various C++ ORB implementation. Moreover, how
   --  can we build an ObjectId without such a conversion function ?

   ------------------------------
   -- Exception ForwardRequest --
   ------------------------------

   ForwardRequest : exception;

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
   -------------------------------------------

   type ForwardRequest_Members is new CORBA.IDL_Exception_Members with record
      Forward_Reference : CORBA.Object.Ref;
   end record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ForwardRequest_Members);

   procedure Raise_ForwardRequest
     (Excp_Memb : in ForwardRequest_Members);
   pragma No_Return (Raise_ForwardRequest);

   --  XXX What is the status of this comment ??

   --  Calling ForwardRequest does not increase the usage counter of
   --  REFERENCE.  As a result, the user must ensure not to release
   --  REFERENCE while the exception is processed.
   --  There is a dilemna here:
   --  - if we increase the counter, the usage counter will never
   --    be decreased if get_members is not called
   --  - if we do not increase it, the object may be deleted
   --    before the exception is caught.

   -----------------------------
   -- Helpers for PolicyValue --
   -----------------------------

   --  ThreadPolicyValue

   TC_ThreadPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return ThreadPolicyValue;

   function To_Any
     (Item : in ThreadPolicyValue)
     return CORBA.Any;

   --  LifespanPolicyValue

   TC_LifespanPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return LifespanPolicyValue;

   function To_Any
     (Item : in LifespanPolicyValue)
     return CORBA.Any;

   --  IdUniquenessPolicyValue

   TC_IdUniquenessPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return IdUniquenessPolicyValue;

   function To_Any
     (Item : in IdUniquenessPolicyValue)
     return CORBA.Any;

   --  IdAssignmentPolicyValue

   TC_IdAssignmentPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return IdAssignmentPolicyValue;

   function To_Any
     (Item : in IdAssignmentPolicyValue)
     return CORBA.Any;

   --  ImplicitActivationPolicyValue

   TC_ImplicitActivationPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return ImplicitActivationPolicyValue;

   function To_Any
     (Item : in ImplicitActivationPolicyValue)
     return CORBA.Any;

   --  ServantRetentionPolicyValue

   TC_ServantRetentionPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return ServantRetentionPolicyValue;

   function To_Any
     (Item : in ServantRetentionPolicyValue)
     return CORBA.Any;

   --  RequestProcessingPolicyValue

   TC_RequestProcessingPolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.TC_Enum;

   function From_Any
     (Item : in CORBA.Any)
     return RequestProcessingPolicyValue;

   function To_Any
     (Item : in RequestProcessingPolicyValue)
     return CORBA.Any;

   -------------------------
   -- Specific to PolyORB --
   -------------------------

   --  XXX Old AdaBroker-specific spec, kept here for
   --  now for easy reference. Please do not remove yet.
   --  XXX is the above comment still pertinent ?

   function Get_Type_Id (For_Servant : Servant)
     return CORBA.RepositoryId;

   type Request_Dispatcher is access procedure
     (For_Servant : in Servant;
      Request     : in CORBA.ServerRequest.Object_Ptr);
   --  Same signature as primitive Invoke of type
   --  DynamicImplementation.

   type Servant_Class_Predicate is access function
     (For_Servant : Servant)
     return Boolean;

   procedure Register_Skeleton
     (Type_Id    : in CORBA.RepositoryId;
      Is_A       : in Servant_Class_Predicate;
      Dispatcher : in Request_Dispatcher := null);
   --  Associate a type id with a class predicate.
   --  A Dispatcher function can also be specified if the
   --  class predicate corresponds to a class derived from
   --  PortableServer.Servant_Base. For other classes derived
   --  from PortableServer.DynamicImplementation, the user
   --  must override the Invoke operation himself, and the
   --  Dispatcher will be ignored and can be null.
   --  NOTE: This procedure is not thread safe.

private

   type DynamicImplementation is
     abstract new CORBA.Impl.Object with null record;

   function Execute_Servant
     (Self : access DynamicImplementation;
      Msg  :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Servant_Base is
     abstract new DynamicImplementation with null record;

end PortableServer;
