------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . P O A                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Abstract interface for the POA.

--  $Id$

with PolyORB.Locks;
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

   --  Unit has no proper body: no elab control necessary.

   Invalid_Object_Id : exception renames PolyORB.POA_Types.Invalid_Object_Id;
   Invalid_Method    : exception renames PolyORB.POA_Types.Invalid_Method;

   type Obj_Adapter is abstract new PolyORB.POA_Types.Obj_Adapter with
      record
         Name                       : Types.String;
         POA_Manager                : PolyORB.POA_Manager.Ref;
         Boot_Time                  : Time_Stamp;
         Absolute_Address           : Types.String;
         Active_Object_Map          : PolyORB.Object_Maps.Object_Map_Access;

         --  Policies (one of each is required)
         Thread_Policy              : ThreadPolicy_Access             := null;
         Request_Processing_Policy  : RequestProcessingPolicy_Access  := null;
         Id_Assignment_Policy       : IdAssignmentPolicy_Access      := null;
         Id_Uniqueness_Policy       : IdUniquenessPolicy_Access       := null;
         Servant_Retention_Policy   : ServantRetentionPolicy_Access   := null;
         Lifespan_Policy            : LifespanPolicy_Access           := null;
         Implicit_Activation_Policy : ImplicitActivationPolicy_Access := null;

         --  Siblings
         Father                     : Obj_Adapter_Access := null;
         Children                   : POAList_Access     := null;

         --  Locks
         Children_Lock              : PolyORB.Locks.Rw_Lock_Access;
         Map_Lock                   : PolyORB.Locks.Rw_Lock_Access;
      end record;

   --  The POA object
   --  XXX Part of this should be private (locks, active object map, father...)
   --  The policies are used by all corba-policy-*, we can keep them public

   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   --------------------------------------------------
   --  Procedures and functions required by CORBA  --
   --------------------------------------------------

   function Create_POA
     (Self         : access Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        PolyORB.POA_Policies.PolicyList_Access)
     return Obj_Adapter_Access
      is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   procedure Destroy
     (Self                : access Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean)
      is abstract;
   --  Destroys recursively the POA and all his descendants

   function Activate_Object
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id
      is abstract;
   --  Activates an object

   procedure Activate_Object_With_Id
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access;
      Oid       : in     Object_Id)
      is abstract;
   --  Activates an object with a specified Id

   procedure Deactivate_Object
     (Self : access Obj_Adapter;
      Oid  : in     Object_Id)
      is abstract;
   --  Deactivates an object from the Active Object Map (requires the RETAIN
   --  policy). In case a ServantManager is used, calls its etherealize
   --  method.
   --  Active requests should be completed before the object is removed
   --  XXX ??? How do we implement that? How do we implement the queue?

   function Servant_To_Id
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id is abstract;
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

   function Id_To_Servant
     (Self : access Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access is abstract;
   --  Requires RETAIN or USE_DEFAULT_SERVANT
   --  Case RETAIN:
   --    Look for the given Object_Id in the Active Object Map.
   --    If found, returns the associated servant.
   --  Case USE_DEFAULT_SERVANT:
   --    If the Object_Id is not in the map, or the NON_RETAIN policy
   --    is used, returns the default servant (if one has been registered).
   --  Otherwise:
   --    Raises ObjectNotActive

   --------------------------------------------------------
   -- Functions and procedures not in the CORBA standard --
   --------------------------------------------------------

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
   --  XXX documentation?

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : Object_Id)
     return Types.String;

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : Types.String)
     return Object_Id;

   Invalid_Name : exception;

   Invalid_Policy : exception;
   Adapter_Inactive : exception;
   Adapter_Already_Exists : exception;
   Servant_Not_Active : exception;
   Servant_Already_Active : exception;
   Transient : exception;
   Bad_Param : exception;
   Object_Already_Active : exception;
   Object_Not_Active : exception;
   Object_Not_Exist : exception;
   --  Inspired from equivalent CORBA POA exceptions.

end PolyORB.POA;
