------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . P O A                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  Abstract interface for the POA Object Adapter.

--  This package provides a higher level abstraction (the POA) of a
--  PolyORB's Object Adapter as defined in PolyORB.Obj_Adapters.
--  PolyORB's POA is notionnaly equivalent to CORBA's POA.

--  PolyORB's POA can be accessed through two different interfaces:
--   - a CORBA-like interface', which encompasses CORBA POA API;
--   - the PolyORB Obj_Adapter interface, as defined in PolyORB.Obj_Adapters.

--  Thus, an implementation of this interface must provide both the
--  CORBA-like POA interface and the PolyORB Obj_Adapter interface.

--  $Id$

with PolyORB.Exceptions;
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
with PolyORB.Servants;
with PolyORB.Tasking.Rw_Locks;
with PolyORB.Types;

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

   ---------------------------
   -- POA Obj_Adapter type. --
   ---------------------------

   type Obj_Adapter is abstract new PolyORB.POA_Types.Obj_Adapter with record

      Name                       : Types.String;
      Boot_Time                  : Time_Stamp;
      Absolute_Address           : Types.String;

      POA_Manager                : PolyORB.POA_Manager.Ref;
      --  POA Manager attached to this POA.

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

      POA_Lock                   : Tasking.Rw_Locks.Rw_Lock_Access;
      Children_Lock              : Tasking.Rw_Locks.Rw_Lock_Access;
      Map_Lock                   : Tasking.Rw_Locks.Rw_Lock_Access;
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
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided.
   --  Compability of Policies is checked.

   procedure Find_POA
     (Self        : access Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out Obj_Adapter_Access;
      Error       : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Starting from given POA, looks for the POA in all the descendancy whose
   --  name is Name. Returns null if not found.

   procedure Destroy
     (Self                : access Obj_Adapter;
      Etherealize_Objects : in     Types.Boolean;
      Wait_For_Completion : in     Types.Boolean)
      is abstract;
   --  Destroys recursively the POA and all his descendants

   procedure Create_Object_Identification
     (Self  : access Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Reserve a complete object identifier, possibly using
   --  the given Hint (if not null) for the construction of
   --  the object identifier included in the Object_Id.

   procedure Activate_Object
     (Self      : access Obj_Adapter;
      P_Servant :        Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Activates an object, i.e. associate it with a local
   --  identification, possibly using the given Hint (if not null)
   --  for the construction of the object identifier included
   --  in the Object_Id.

   procedure Deactivate_Object
     (Self  : access Obj_Adapter;
      Oid   : in     Object_Id;
      Error : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Deactivates an object from the Active Object Map (requires the RETAIN
   --  policy). In case a ServantManager is used, calls its etherealize
   --  method.
   --  Active requests should be completed before the object is removed
   --  XXX ??? How do we implement that? How do we implement the queue?

   procedure Servant_To_Id
     (Self      : access Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Requires USE_DEFAULT_SERVANT or RETAIN and either UNIQUE_ID
   --  or IMPLICIT_ACTIVATION
   --  Case RETAIN and UNIQUE_ID:
   --    Looks in the object map for the Id of the given servant
   --  Case RETAIN and IMPLICIT_ACTIVATION:
   --    The servant is activated and its Id is returned
   --  Case USE_DEFAULT_SERVANT:
   --    If the servant is not found in the Active Object Map,
   --    the Id of the current invocation is returned.
   --    ???
   --  Otherwise:
   --    Raises a ServantNotActive exception

   procedure Id_To_Servant
     (Self    : access Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
     is abstract;
   --  Requires RETAIN or USE_DEFAULT_SERVANT
   --  Case RETAIN:
   --    Look for the given Object_Id in the Active Object Map.
   --    If found, returns the associated servant.
   --  Case USE_DEFAULT_SERVANT:
   --    If the Object_Id is not in the map, or the NON_RETAIN policy
   --    is used, returns the default servant (if one has been registered).
   --  Otherwise:
   --    Raises ObjectNotActive

   procedure Get_Servant
     (Self    : access Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;

   procedure Set_Servant
     (Self    : access Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;

   procedure Get_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;

   procedure Set_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;

   -----------------------
   -- Utility functions --
   -----------------------

   procedure Copy_Obj_Adapter
     (From : in     Obj_Adapter;
      To   : access Obj_Adapter)
      is abstract;
   --  Copy values from one Obj_Adapter to another
   --  (Obj_Adapter is limited...)

   procedure Remove_POA_By_Name
     (Self       : access Obj_Adapter;
      Child_Name :        Types.String)
     is abstract;
   --  Remove a child POA from Self's list of children
   --  Doesn't lock the list of children

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : access Object_Id)
     return Types.String;
   --  Convert an object id to its representation as a relative URI.

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI :        Types.String)
     return Object_Id_Access;
   --  Convert an object id from its representation as a relative URI.

end PolyORB.POA;
