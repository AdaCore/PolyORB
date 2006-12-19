------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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
