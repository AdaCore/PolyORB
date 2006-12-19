------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.USER               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package PolyORB.POA_Policies.Id_Assignment_Policy.User is

   type User_Id_Policy is new IdAssignmentPolicy with null record;
   type User_Id_Policy_Access is access all User_Id_Policy;

   function Create return User_Id_Policy_Access;

   procedure Check_Compatibility
     (Self           : User_Id_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   function Policy_Id (Self : User_Id_Policy) return String;

   function Create_Object_Map (Self : User_Id_Policy)
     return PolyORB.Object_Maps.Object_Map_Access;

   procedure Assign_Object_Identifier
     (Self  : User_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint  : Object_Id_Access;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Reconstruct_Object_Identifier
     (Self  : User_Id_Policy;
      OA    : Obj_Adapter_Access;
      Oid   : Object_Id;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Object_Identifier
     (Self   : User_Id_Policy;
      Oid    : Object_Id_Access;
      Result : out Object_Id_Access;
      Error  : in out PolyORB.Errors.Error_Container);

end PolyORB.POA_Policies.Id_Assignment_Policy.User;
