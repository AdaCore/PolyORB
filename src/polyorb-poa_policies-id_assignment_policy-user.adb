------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.USER               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

with PolyORB.Objects;
with PolyORB.POA;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Types;

package body PolyORB.POA_Policies.Id_Assignment_Policy.User is

   ------------
   -- Create --
   ------------

   function Create return User_Id_Policy_Access is
   begin
      return new User_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : User_Id_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      null;
      --  XXX TODO check compat for USER_ID

   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : User_Id_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "ID_ASSIGNMENT_POLICY.USER_ID";
   end Policy_Id;

   ------------------------------
   -- Assign_Object_Identifier --
   ------------------------------

   function Assign_Object_Identifier
     (Self   : User_Id_Policy;
      OA     : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint   : Object_Id_Access)
     return Unmarshalled_Oid
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      if Hint = null then
         raise PolyORB.POA.Bad_Param;
      end if;

      return Unmarshalled_Oid'
        (Id => Types.To_PolyORB_String (Objects.To_String (Hint.all)),
         System_Generated => False,
         Persistency_Flag =>
           PolyORB.POA_Policies.Lifespan_Policy.Get_Lifespan_Cookie
            (POA.Lifespan_Policy.all, OA),
         Creator => POA.Absolute_Address);
   end Assign_Object_Identifier;

   ---------------
   -- Is_System --
   ---------------

   function Is_System (Self : User_Id_Policy) return Boolean
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return False;
   end Is_System;

   -----------------------
   -- Ensure_Oid_Origin --
   -----------------------

   procedure Ensure_Oid_Origin
     (Self  : User_Id_Policy;
      U_Oid : Unmarshalled_Oid)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      if U_Oid.System_Generated then
         raise PolyORB.POA.Bad_Param;
      end if;
   end Ensure_Oid_Origin;

end PolyORB.POA_Policies.Id_Assignment_Policy.User;
