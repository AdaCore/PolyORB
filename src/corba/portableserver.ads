------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Exceptions;

with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);

with CORBA.Impl;
with CORBA.Object;
with CORBA.ServerRequest;

pragma Warnings (Off);              --  WAG:3.15
with PolyORB.Any;                   --  WAG:3.15
pragma Elaborate_All (PolyORB.Any); --  WAG:3.15
pragma Warnings (On);               --  WAG:3.15

with PolyORB.Components;
with PolyORB.Objects;

package PortableServer is

   pragma Elaborate_Body;

   package POA_Forward is new CORBA.Forward;

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

   --  FIXME: how to implement this ?
   --  function "=" (Left, Right : Servant) return Boolean;
   --  pragma Convention (Intrinsic, "=");

   function Get_Default_POA
     (For_Servant : Servant_Base)
     return POA_Forward.Ref;

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
   --  Convert string 'Id' into an ObjectID.

   function ObjectId_To_String (Id : ObjectId) return String;
   --  Convert ObjectId 'Id' into a string.

   --  XXX these functions are not defined in the CORBA specification,
   --  but defined in various C++ ORB implementation. Moreover, how
   --  can we build an ObjectId without such a conversion function ?

   ------------------------------
   -- Exception ForwardRequest --
   ------------------------------

   ForwardRequest : exception;

   type ForwardRequest_Members is new CORBA.IDL_Exception_Members with record
      Forward_Reference : CORBA.Object.Ref;
   end record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ForwardRequest_Members);

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

   -----------------------------
   -- Helpers for PolicyValue --
   -----------------------------

   TC_ThreadPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return ThreadPolicyValue;

   function To_Any
     (Item : in ThreadPolicyValue)
     return CORBA.Any;

   TC_LifespanPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return LifespanPolicyValue;

   function To_Any
     (Item : in LifespanPolicyValue)
     return CORBA.Any;

   TC_IdUniquenessPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return IdUniquenessPolicyValue;

   function To_Any
     (Item : in IdUniquenessPolicyValue)
     return CORBA.Any;

   TC_IdAssignmentPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return IdAssignmentPolicyValue;

   function To_Any
     (Item : in IdAssignmentPolicyValue)
     return CORBA.Any;

   TC_ImplicitActivationPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return ImplicitActivationPolicyValue;

   function To_Any
     (Item : in ImplicitActivationPolicyValue)
     return CORBA.Any;

   TC_ServantRetentionPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return ServantRetentionPolicyValue;

   function To_Any
     (Item : in ServantRetentionPolicyValue)
     return CORBA.Any;

   TC_RequestProcessingPolicyValue : CORBA.TypeCode.Object :=
     CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
                     return RequestProcessingPolicyValue;

   function To_Any
     (Item : in RequestProcessingPolicyValue)
     return CORBA.Any;

   --  XXX Old AdaBroker-specific spec, kept here for
   --  now for easy reference. Please do not remove yet.

   -------------------------
   -- Specific to PolyORB --
   -------------------------

   function Get_Type_Id (For_Servant : Servant)
     return CORBA.RepositoryId;

   type Request_Dispatcher is access procedure
     (For_Servant : in Servant;
      Request     : in CORBA.ServerRequest.Object_Ptr);
   --  Same signature as primitive 'Invoke' of type
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

--    --  Calling ForwardRequest does not increase the usage counter of
--    --  REFERENCE.  As a result, the user must ensure not to release
--    --  REFERENCE while the exception is processed.
--    --  There is a dilemna here:
--    --  - if we increase the counter, the usage counter will never
--    --    be decreased if get_members is not called
--    --  - if we do not increase it, the object may be deleted
--    --    before the exception is caught.
--    procedure Raise_Forward_Request (Reference : in CORBA.Object.Ref);
--    pragma No_Return (Raise_Forward_Request);

private

   type DynamicImplementation is
     abstract new CORBA.Impl.Object with null record;

   function Execute_Servant
     (Self : access DynamicImplementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Servant_Base is
     abstract new DynamicImplementation with null record;

   procedure Invoke
     (Self    : access Servant_Base;
      Request : in CORBA.ServerRequest.Object_Ptr);

end PortableServer;
