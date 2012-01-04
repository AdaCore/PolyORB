------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;
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
      Error     : in out PolyORB.Errors.Error_Container)
     is abstract;

   procedure Forget_Servant_Association
     (Self  :        ServantRetentionPolicy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      Oid   :        Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
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
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Case RETAIN:
   --    Asks the Id_Assignment_Policy to look for the given Object_Id.
   --    If found, returns the associated servant. Otherwisem returns null.
   --  Case NON_RETAIN:
   --    Raises WrongPolicy.

   procedure Ensure_Servant_Manager_Type
     (Self    :        ServantRetentionPolicy;
      Manager :        ServantManager'Class;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;

end PolyORB.POA_Policies.Servant_Retention_Policy;
