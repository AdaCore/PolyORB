------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.USER               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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
   pragma Unreferenced (C); --  For conditional pragma Debug

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

   function Policy_Id (Self : User_Id_Policy) return String
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

   function Create_Object_Map (Self : User_Id_Policy)
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

   procedure Assign_Object_Identifier
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
      use PolyORB.Types;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      pragma Debug (O ("Assign_Object_Identifier: enter"));

      if Hint = null then
         pragma Debug (O ("Hint is null !"));

         Throw (Error,
                WrongPolicy_E,
                Null_Member);
         return;
      end if;

      pragma Debug (O ("Object Name is '"
                       & PolyORB.Objects.Oid_To_Hex_String (Hint.all)
                       & "'"));

      U_Oid := PolyORB.POA_Types.Create_Id
        (Name             => PolyORB.Objects.Oid_To_Hex_String (Hint.all),
         System_Generated => False,
         Persistency_Flag =>
           Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
         Creator          => POA.Absolute_Address.all);

      pragma Debug (O ("Assign_Object_Identifier: leave"));
   end Assign_Object_Identifier;

   -----------------------------------
   -- Reconstruct_Object_Identifier --
   -----------------------------------

   procedure Reconstruct_Object_Identifier
     (Self  : User_Id_Policy;
      OA    : Obj_Adapter_Access;
      Oid   : Object_Id;
      U_Oid : out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Error);

      use PolyORB.POA_Policies.Lifespan_Policy;
      use PolyORB.Types;

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

   procedure Object_Identifier
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
