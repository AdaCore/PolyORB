------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.SYSTEM              --
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

pragma Ada_2005;

package PolyORB.POA_Policies.Id_Assignment_Policy.System is

   type System_Id_Policy is new IdAssignmentPolicy with null record;
   type System_Id_Policy_Access is access all System_Id_Policy;

   function Create return System_Id_Policy_Access;

   overriding procedure Check_Compatibility
     (Self           : System_Id_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   overriding function Policy_Id (Self : System_Id_Policy) return String;

   overriding function Create_Object_Map (Self : System_Id_Policy)
     return PolyORB.Object_Maps.Object_Map_Access;

   overriding procedure Assign_Object_Identifier
     (Self  : System_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint  : Object_Id_Access;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);

   overriding procedure Reconstruct_Object_Identifier
     (Self  : System_Id_Policy;
      OA    : Obj_Adapter_Access;
      Oid   : Object_Id;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);

   overriding procedure Object_Identifier
     (Self   : System_Id_Policy;
      Oid    : Object_Id_Access;
      Result : out Object_Id_Access;
      Error  : in out PolyORB.Errors.Error_Container);

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
