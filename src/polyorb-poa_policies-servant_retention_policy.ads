------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.POA_Types;     use PolyORB.POA_Types;

package PolyORB.POA_Policies.Servant_Retention_Policy is

   type ServantRetentionPolicy is abstract new Policy with null record;
   subtype Servant_Retention_Policy is ServantRetentionPolicy;
   type ServantRetentionPolicy_Access is
     access all ServantRetentionPolicy'Class;
   subtype Servant_Retention_Policy_Access is ServantRetentionPolicy_Access;

   function Create return ServantRetentionPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Free (P   : in     ServantRetentionPolicy;
                   Ptr : in out Policy_Access)
     is abstract;

   function Activate_Object
     (Self      : ServantRetentionPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access is abstract;
   --  Case RETAIN:
   --    Activates the object (servant) in the Active Object Map
   --    The Id_Uniqueness_Policy checks that the servant is not yet active
   --    The Id_Assign_Policy generates an Object_Id
   --  Case NON_RETAIN:
   --    Raises a WrongPolicy exception

   procedure Activate_Object_With_Id
     (Self      : ServantRetentionPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access;
      Oid       : Object_Id) is abstract;
   --  Case RETAIN:
   --    Registers the servant in the active objects map
   --    Checks that the object_id is not yet used
   --    The Id_Uniqueness_Policy checks that the servant is not yet
   --    in the active objects map
   --  Case NON_RETAIN:
   --    Raises WrongPolicy

   procedure Deactivate
     (Self      : ServantRetentionPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      Oid       : Object_Id) is abstract;
   --  Case RETAIN:
   --  Deactivates an object from the Active Object Map; waits for the
   --  complection of current active requests. Calls etherealize if
   --  necessary (ServantManager used).
   --  Case NON_RETAIN:
   --  Raises WrongPolicy

   function Servant_To_Id (Self      : ServantRetentionPolicy;
                           OA        : PolyORB.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access
     is abstract;
   --  Case RETAIN:
   --    Asks the Id_Uniqueness_Policy to return the Object_Id of the
   --    specified servant.
   --    If not found, raises an ServantNotActive exception.
   --  Case NON_RETAIN:
   --    Returns null

   function Id_To_Servant
     (Self      : ServantRetentionPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid     : Unmarshalled_Oid)
     return Servant_Access
      is abstract;
   --  Case RETAIN:
   --    Asks the Id_Assignment_Policy to look for the given Object_Id.
   --    If found, returns the associated servant. Otherwisem returns null.
   --  Case NON_RETAIN:
   --    Raises WrongPolicy.

end PolyORB.POA_Policies.Servant_Retention_Policy;
