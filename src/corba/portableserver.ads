------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       P O R T A B L E S E R V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Droopi.Objects;

with CORBA;
with CORBA.Object;
with CORBA.Impl;
with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);
with CORBA.ServerRequest;

package PortableServer is

   pragma Elaborate_Body;

   package POA_Forward is new CORBA.Forward;

   type Servant_Base is new CORBA.Impl.Object with private;
   type DynamicImplementation is new Servant_Base with private;

   procedure Invoke
     (Self : DynamicImplementation;
      Request : CORBA.ServerRequest.Object);

   --  21.41.1
   --  Conforming implementations must provide a controlled (tagged)
   --  Servant_Base type and default implementations of the primitve
   --  operations on Servant_Base that meet the required semantics.

   type Servant is access all Servant_Base'Class;

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

--    package IDL_SEQUENCE_Octet renames Broca.Sequences.Octet_Sequences;
   --    type ObjectId is new IDL_SEQUENCE_Octet.Sequence;

   type ObjectId is new Droopi.Objects.Object_Id;

   ------------------------------
   -- Exception ForwardRequest --
   ------------------------------

   ForwardRequest : exception;

   type ForwardRequest_Members is
     new CORBA.IDL_Exception_Members with
      record
         Forward_Reference : CORBA.Object.Ref;
      end record;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
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
      SINGLE_THREAD_MODEL);

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

   --  XXX Old AdaBroker-specific spec, kept here for
   --  now for easy reference. Please do not remove yet.

--    -----------------------
--    -- Specific to Broca --
--    -----------------------

--    procedure Marshall
--      (Buffer : access Broca.Buffers.Buffer_Type;
--       Data   : in ObjectId);

--    function Unmarshall
--      (Buffer : access Broca.Buffers.Buffer_Type)
--      return ObjectId;

--    function Get_Type_Id
--      (For_Servant : Servant) return CORBA.RepositoryId;

--    procedure GIOP_Dispatch
--      (For_Servant       : in Servant;
--       Operation         : in String;
--       Request_Id        : in CORBA.Unsigned_Long;
--       Response_Expected : in CORBA.Boolean;
--       Request_Buffer    : access Broca.Buffers.Buffer_Type;
--       Reply_Buffer      : access Broca.Buffers.Buffer_Type);
--    --  Call an operation.
--    --  Only standard exceptions (defined in module CORBA) can be
--    --  caught outside of GIOP_DISPATCH, ie user defined exception must
--    --  be marshalled.


--    --  The data to be provided by the skeleton of an interface.

--    type GIOP_Dispatcher is access procedure
--      (For_Servant      : in Servant;
--       Operation        : in String;
--       Request_Id       : in CORBA.Unsigned_Long;
--       Reponse_Expected : in CORBA.Boolean;
--       Request_Buffer   : access Broca.Buffers.Buffer_Type;
--       Reply_Buffer     : access Broca.Buffers.Buffer_Type);

--    type Servant_Class_Predicate is access function
--      (For_Servant : Servant)
--      return Boolean;

--    procedure Register_Skeleton
--      (Type_Id    : in CORBA.RepositoryId;
--       Is_A       : in Servant_Class_Predicate;
--       Dispatcher : in GIOP_Dispatcher);

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

   type Servant_Base is new CORBA.Impl.Object with null record;
   type DynamicImplementation is new Servant_Base with null record;

end PortableServer;
