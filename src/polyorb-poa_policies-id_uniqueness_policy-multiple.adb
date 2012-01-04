------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY.MULTIPLE             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.POA;

package body PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple is

   ------------
   -- Create --
   ------------

   function Create
     return Multiple_Id_Policy_Access is
   begin
      return new Multiple_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        Multiple_Id_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, Other_Policies, Error);
      pragma Warnings (On);
   begin
      null;
      --  No rule to test.
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Multiple_Id_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "ID_UNIQUENESS_POLICY.MULTIPLE_ID";
   end Policy_Id;

   -------------------------------
   -- Ensure_Servant_Uniqueness --
   -------------------------------

   procedure Ensure_Servant_Uniqueness
     (Self      :        Multiple_Id_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, P_Servant, Error);
      pragma Warnings (On);

   begin
      null;
      --  MULTIPLE_ID: nothing to do.
   end Ensure_Servant_Uniqueness;

   --------------------
   -- Activate_Again --
   --------------------

   procedure Activate_Again
     (Self      :        Multiple_Id_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Oid       :        Object_Id_Access;
      Result    :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      U_Oid : Unmarshalled_Oid;

   begin
      PolyORB.POA.Activate_Object
        (PolyORB.POA.Obj_Adapter_Access (OA),
         P_Servant,
         Oid,
         U_Oid,
         Error);
      --  Activate servant again, regardless of its current
      --  activation state.

      if PolyORB.Errors.Found (Error) then
         return;
      end if;

      Result := U_Oid_To_Oid (U_Oid);
   end Activate_Again;

end PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
