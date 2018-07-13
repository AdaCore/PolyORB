------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.POA;
with PolyORB.POA_Policies.Servant_Retention_Policy;

package body
  PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager
is

   use PolyORB.Errors;

   ------------
   -- Create --
   ------------

   function Create
     return Use_Servant_Manager_Policy_Access is
   begin
      return new Use_Servant_Manager_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   overriding procedure Check_Compatibility
     (Self           :        Use_Servant_Manager_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   overriding function Policy_Id
     (Self : Use_Servant_Manager_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "REQUEST_PROCESSING_POLICY.USE__SERVANT_MANAGER";
   end Policy_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   overriding procedure Id_To_Servant
     (Self    :        Use_Servant_Manager_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.POA_Policies.Servant_Retention_Policy;

   begin

      --  Lookup object in Active Object Map

      Retained_Id_To_Servant
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all,
         OA,
         U_Oid,
         Servant,
         Error);

      if Found (Error) then
         return;
      end if;

      --  Under USE_SERVANT_MANAGER policy, if no servant is found and
      --  if we are processing a request, we may try to activate
      --  one. This is done by the POA.

   end Id_To_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   overriding procedure Set_Servant
     (Self    :        Use_Servant_Manager_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Unreferenced (Servant);

   begin
      Throw (Error,
             WrongPolicy_E,
             Null_Members'(Null_Member));
   end Set_Servant;

   -----------------
   -- Get_Servant --
   -----------------

   overriding procedure Get_Servant
     (Self    :        Use_Servant_Manager_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);

   begin
      Servant := null;

      Throw (Error,
             WrongPolicy_E,
             Null_Members'(Null_Member));
   end Get_Servant;

   ----------------------------
   -- Ensure_Servant_Manager --
   ----------------------------

   overriding procedure Ensure_Servant_Manager
     (Self  :        Use_Servant_Manager_Policy;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Error);
   begin
      null;
   end Ensure_Servant_Manager;

end PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager;
