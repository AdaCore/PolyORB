------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY               --
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

with PolyORB.Exceptions;
with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.POA_Policies.Servant_Retention_Policy is

   use PolyORB.POA_Types;

   type ServantRetentionPolicy is abstract new Policy with null record;

   type ServantRetentionPolicy_Access is
     access all ServantRetentionPolicy'Class;

   procedure Retain_Servant_Association
     (Self      :        ServantRetentionPolicy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      U_Oid     :        Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container)
     is abstract;

   procedure Forget_Servant_Association
     (Self  :        ServantRetentionPolicy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      Oid   :        Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Remove a previously-retained servant/oid association.

   function Retained_Servant_To_Id
     (Self      : ServantRetentionPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access)
     return Object_Id_Access
     is abstract;
   --  Case RETAIN:
   --    Look up the active object map for an oid associated with
   --    P_Servant.
   --  Case NON_RETAIN:
   --    Returns null

   procedure Retained_Id_To_Servant
     (Self    :        ServantRetentionPolicy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;
   --  Case RETAIN:
   --    Asks the Id_Assignment_Policy to look for the given Object_Id.
   --    If found, returns the associated servant. Otherwisem returns null.
   --  Case NON_RETAIN:
   --    Raises WrongPolicy.

   procedure Ensure_Servant_Manager_Type
     (Self    :        ServantRetentionPolicy;
      Manager :        ServantManager'Class;
      Error   : in out PolyORB.Exceptions.Error_Container)
      is abstract;

end PolyORB.POA_Policies.Servant_Retention_Policy;
