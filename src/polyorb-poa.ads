------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . P O A                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

--  Abstract interface for the POA Object Adapter.

--  This package provides a higher level abstraction (the POA) of a
--  PolyORB's Object Adapter as defined in PolyORB.Obj_Adapters.
--  PolyORB's POA is notionnaly equivalent to CORBA's POA.

--  PolyORB's POA can be accessed through two different interfaces:
--   - a CORBA-like interface', which encompasses CORBA POA API;
--   - the PolyORB Obj_Adapter interface, as defined in PolyORB.Obj_Adapters.

--  Thus, an implementation of this interface must provide both the
--  CORBA-like POA interface and the PolyORB Obj_Adapter interface.

with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.Object_Maps;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.POA_Policies.Thread_Policy;
with PolyORB.POA_Policies.Request_Processing_Policy;
with PolyORB.POA_Policies.Id_Assignment_Policy;
with PolyORB.POA_Policies.Id_Uniqueness_Policy;
with PolyORB.POA_Policies.Servant_Retention_Policy;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.POA_Policies.Implicit_Activation_Policy;
with PolyORB.POA_Types;
with PolyORB.QoS;
with PolyORB.Servants;
with PolyORB.Tasking.Mutexes;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package PolyORB.POA is

   pragma Elaborate_Body;

   use PolyORB.POA_Policies.Thread_Policy;
   use PolyORB.POA_Policies.Request_Processing_Policy;
   use PolyORB.POA_Policies.Id_Assignment_Policy;
   use PolyORB.POA_Policies.Id_Uniqueness_Policy;
   use PolyORB.POA_Policies.Servant_Retention_Policy;
   use PolyORB.POA_Policies.Lifespan_Policy;
   use PolyORB.POA_Policies.Implicit_Activation_Policy;
   use PolyORB.POA_Types;
   use PolyORB.Utils.Strings;

   ---------------------------
   -- POA Obj_Adapter type. --
   ---------------------------

   type Obj_Adapter is abstract new PolyORB.POA_Types.Obj_Adapter with record
      Name                       : String_Ptr;
      --  The POA's name. If this is null, the object has been destroyed

      Boot_Time                  : Duration;
      --  Creation date of this POA

      Absolute_Address           : String_Ptr;
      --  Absolute path of this POA relative to the root POA

      POA_Manager                : PolyORB.POA_Manager.Ref;
      --  POA Manager attached to this POA

      Adapter_Activator          : AdapterActivator_Access;
      --  Adapter Activator attached to this POA (null if not used).

      Active_Object_Map          : PolyORB.Object_Maps.Object_Map_Access;
      --  The active object map (null if the policies used for this POA
      --  do not require one).

      Default_Servant            : Servants.Servant_Access;
      --  The default servant (null if the policies used for this POA
      --  do not require one).

      Servant_Manager            : ServantManager_Access;
      --  The servant manager (null if the policies used for this POA
      --  do not require one).

      --  Policies (one of each is required)
      Thread_Policy              : ThreadPolicy_Access             := null;
      Request_Processing_Policy  : RequestProcessingPolicy_Access  := null;
      Id_Assignment_Policy       : IdAssignmentPolicy_Access       := null;
      Id_Uniqueness_Policy       : IdUniquenessPolicy_Access       := null;
      Servant_Retention_Policy   : ServantRetentionPolicy_Access   := null;
      Lifespan_Policy            : LifespanPolicy_Access           := null;
      Implicit_Activation_Policy : ImplicitActivationPolicy_Access := null;

      Father                     : Obj_Adapter_Access;
      --  Parent POA.

      Children                   : POATable_Access;
      --  All child-POAs of this POA.

      POA_Lock                   : Tasking.Mutexes.Mutex_Access;
      Children_Lock              : Tasking.Mutexes.Mutex_Access;
      Map_Lock                   : Tasking.Mutexes.Mutex_Access;
      --  Locks

   end record;

   type Obj_Adapter_Access is access all Obj_Adapter'Class;
   --  The POA object
   --  XXX Part of this should be private (locks, active object map, father...)

   ------------------------------
   -- CORBA-like POA interface --
   ------------------------------

   procedure Create_POA
     (Self         : access Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided.
   --  Compability of Policies is checked.

   procedure Initialize_POA
     (Self         : access Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          : in out Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container);
   --  Create a POA given its name and a list of policies Policies are
   --  optionnal : defaults values are provided. Compability of Policies is
   --  checked.

   procedure Find_POA
     (Self        : access Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out Obj_Adapter_Access;
      Error       : in out PolyORB.Errors.Error_Container);
   --  Starting from given POA, looks for the POA in all the descendancy whose
   --  name is Name. Returns null if not found.

   procedure Destroy
     (Self                : access Obj_Adapter;
      Etherealize_Objects : Types.Boolean;
      Wait_For_Completion : Types.Boolean);
   --  Destroys recursively the POA and all his descendants

   procedure Create_Object_Identification
     (Self  : access Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);
   --  Reserve a complete object identifier, possibly using the given Hint (if
   --  not null) for the construction of the object identifier included in the
   --  Object_Id.

   procedure Activate_Object
     (Self      : access Obj_Adapter;
      P_Servant :        Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Errors.Error_Container);
   --  Activates an object, i.e. associate it with a local identification,
   --  possibly using the given Hint (if not null) for the construction of the
   --  object identifier included in the Object_Id.

   procedure Deactivate_Object
     (Self  : access Obj_Adapter;
      Oid   : Object_Id;
      Error : in out PolyORB.Errors.Error_Container);
   --  Deactivates an object from the Active Object Map (requires the RETAIN
   --  policy). In case a ServantManager is used, calls its etherealize
   --  method.
   --  Active requests should be completed before the object is removed
   --  XXX ??? How do we implement that? How do we implement the queue?

   procedure Servant_To_Id
     (Self      : access Obj_Adapter;
      P_Servant : Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container);

   procedure Id_To_Servant
     (Self    : access Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);
   --  Requires RETAIN or USE_DEFAULT_SERVANT
   --  Case RETAIN:
   --    Look for the given Object_Id in the Active Object Map.
   --    If found, returns the associated servant.
   --  Case USE_DEFAULT_SERVANT:
   --    If the Object_Id is not in the map, or the NON_RETAIN policy is used,
   --    returns the default servant (if one has been registered).
   --  Otherwise:
   --    Raises ObjectNotActive

   procedure Get_Servant
     (Self    : access Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Set_Servant
     (Self    : access Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Set_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_The_Children
     (Self     : access Obj_Adapter;
      Children :    out POAList);

   --------------------------------------------------
   -- PolyORB Obj_Adapter interface implementation --
   --------------------------------------------------

   procedure Create
     (OA : access Obj_Adapter);

   procedure Destroy
     (OA : access Obj_Adapter);

   procedure Export
     (OA    : access Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Unexport
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Object_Key
     (OA      : access Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_QoS
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container);

   function Get_Empty_Arg_List
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any;

   procedure Find_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access);

   -----------------------
   -- Utility functions --
   -----------------------

   procedure Copy_Obj_Adapter
     (From : Obj_Adapter;
      To   : access Obj_Adapter);
   --  Copy values from one Obj_Adapter to another (Obj_Adapter is limited)

   procedure Remove_POA_By_Name
     (Self       : access Obj_Adapter;
      Child_Name :        Standard.String);
   --  Remove a child POA from Self's list of children
   --  Does not lock the list of children

   procedure Oid_To_Rel_URI
     (OA    : access Obj_Adapter;
      Id    : access Object_Id;
      URI   : out Types.String;
      Error : in out PolyORB.Errors.Error_Container);
   --  Convert an object id to its representation as a relative URI

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI :        String)
     return Object_Id_Access;
   --  Convert an object id from its representation as a relative URI

private

   procedure Init_With_User_Policies
     (OA       : access Obj_Adapter;
      Policies :        POA_Policies.PolicyList);
   --  Initialize OA with a set of policies provided by the user

   procedure Init_With_Default_Policies
     (OA : access Obj_Adapter);
   --  Initialize OA with a default set of policies provided by the currently
   --  active POA configuration.

   procedure Check_Policies_Compatibility
     (OA    :        Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Destroy_Policies
     (OA : in out Obj_Adapter);
   pragma Warnings (Off);
   pragma Unreferenced (Destroy_Policies);
   pragma Warnings (On);
   --  Destroys OA's policies

   procedure Destroy_OA (OA : Obj_Adapter_Access);
   --  Destroy OA's components (this does not deallocate the OA itself, since
   --  this is usually done by the reference counting system).
   --  ??? Why isn't this just an override of Finalize?

   procedure Create_Root_POA
     (New_Obj_Adapter : access Obj_Adapter);
   --  Create the Root of all POAs

   procedure Find_Servant
     (OA       : access Obj_Adapter;
      Id       : access Objects.Object_Id;
      Do_Check :        Boolean;
      Servant  :    out Servants.Servant_Access;
      Error    : in out PolyORB.Errors.Error_Container);
   --  The Find_Servant from PolyORB, plus a parameter.
   --  If Do_Check is True, then the POA checks the state of its POA
   --  Manager.

   procedure Set_Policies
     (OA       : access Obj_Adapter;
      Policies : POA_Policies.PolicyList;
      Default  : Boolean);
   --  Set OA policies from the values in Policies.
   --  If Default is True, set only those policies that
   --  are not yet explicitly set in OA. If Default is False,
   --  set all policies, and warn for duplicates.

   function POA_Manager_Of
     (OA : access Obj_Adapter)
     return POA_Manager.POAManager_Access;
   --  Return the POA Manager associated to 'OA'.

end PolyORB.POA;
