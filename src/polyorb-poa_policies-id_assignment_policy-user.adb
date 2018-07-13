------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.USER               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Object_Maps.User;
with PolyORB.POA;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.POA_Types;
with PolyORB.Types;

package body PolyORB.POA_Policies.Id_Assignment_Policy.User is

   use PolyORB.Log;

   package L is new Log.Facility_Log
     ("polyorb.poa_policies.id_assignement_policy.user");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

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

   overriding procedure Check_Compatibility
     (Self           : User_Id_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, Other_Policies, Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to check
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   overriding function Policy_Id (Self : User_Id_Policy) return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "ID_ASSIGNMENT_POLICY.USER_ID";
   end Policy_Id;

   -----------------------
   -- Create_Object_Map --
   -----------------------

   overriding function Create_Object_Map (Self : User_Id_Policy)
     return PolyORB.Object_Maps.Object_Map_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : constant PolyORB.Object_Maps.Object_Map_Access
        := new PolyORB.Object_Maps.User.User_Object_Map;
   begin
      PolyORB.Object_Maps.Initialize (Result.all);
      return Result;
   end Create_Object_Map;

   ------------------------------
   -- Assign_Object_Identifier --
   ------------------------------

   overriding procedure Assign_Object_Identifier
     (Self  : User_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint  : Object_Id_Access;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Errors;

      use PolyORB.POA_Policies.Lifespan_Policy;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      pragma Debug (C, O ("Assign_Object_Identifier: enter"));

      if Hint = null then
         pragma Debug (C, O ("Hint is null !"));

         Throw (Error,
                WrongPolicy_E,
                Null_Member);
         return;
      end if;

      pragma Debug (C, O ("Object Name is '"
                       & PolyORB.Objects.Oid_To_Hex_String (Hint.all)
                       & "'"));

      U_Oid := PolyORB.POA_Types.Create_Id
        (Name             => PolyORB.Objects.Oid_To_Hex_String (Hint.all),
         System_Generated => False,
         Persistency_Flag =>
           Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
         Creator          => POA.Absolute_Address.all);

      pragma Debug (C, O ("Assign_Object_Identifier: leave"));
   end Assign_Object_Identifier;

   -----------------------------------
   -- Reconstruct_Object_Identifier --
   -----------------------------------

   overriding procedure Reconstruct_Object_Identifier
     (Self  : User_Id_Policy;
      OA    : Obj_Adapter_Access;
      Oid   : Object_Id;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Error);

      use PolyORB.POA_Policies.Lifespan_Policy;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
   begin
      U_Oid := PolyORB.POA_Types.Create_Id
        (Name             => PolyORB.Objects.Oid_To_Hex_String (Oid),
         System_Generated => False,
         Persistency_Flag =>
           Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
         Creator          => POA.Absolute_Address.all);
   end Reconstruct_Object_Identifier;

   -----------------------
   -- Object_Identifier --
   -----------------------

   overriding procedure Object_Identifier
     (Self   : User_Id_Policy;
      Oid    : Object_Id_Access;
      Result : out Object_Id_Access;
      Error  : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;
      pragma Unreferenced (Self);
      U_Oid : Unmarshalled_Oid;
   begin
      Oid_To_U_Oid (Oid.all, U_Oid, Error);
      if Found (Error) then
         return;
      end if;
      Result := new Object_Id'
        (PolyORB.Objects.Hex_String_To_Oid
         (PolyORB.Types.To_Standard_String (U_Oid.Id)));
   end Object_Identifier;

end PolyORB.POA_Policies.Id_Assignment_Policy.User;
