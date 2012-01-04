------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY                 --
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

with PolyORB.Object_Maps;
with PolyORB.POA_Types;

package PolyORB.POA_Policies.Id_Assignment_Policy is

   use PolyORB.POA_Types;

   type IdAssignmentPolicy is abstract new Policy with null record;
   type IdAssignmentPolicy_Access is access all IdAssignmentPolicy'Class;

   function Create_Object_Map (Self : IdAssignmentPolicy)
     return PolyORB.Object_Maps.Object_Map_Access is abstract;

   procedure Assign_Object_Identifier
     (Self   : IdAssignmentPolicy;
      OA     : Obj_Adapter_Access;
      Hint   : Object_Id_Access;
      U_Oid  : out Unmarshalled_Oid;
      Error  : in out PolyORB.Errors.Error_Container) is abstract;

   procedure Reconstruct_Object_Identifier
     (Self  : IdAssignmentPolicy;
      OA    : Obj_Adapter_Access;
      Oid   : Object_Id;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container) is abstract;

   procedure Object_Identifier
     (Self   : IdAssignmentPolicy;
      Oid    : Object_Id_Access;
      Result : out Object_Id_Access;
      Error  : in out PolyORB.Errors.Error_Container) is abstract;
   --  Return the ObjectId stored in Oid. Note that Result is a newly
   --  allocated variable that must be deallocated by the caller after use.

end PolyORB.POA_Policies.Id_Assignment_Policy;
