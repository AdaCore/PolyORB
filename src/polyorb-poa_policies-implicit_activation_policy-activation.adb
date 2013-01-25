------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.ACTIVATION         --
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

pragma Ada_2005;

with Ada.Tags;

with PolyORB.POA;
with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

package body PolyORB.POA_Policies.Implicit_Activation_Policy.Activation is

   ------------
   -- Create --
   ------------

   function Create
     return Activation_Policy_Access is
   begin
      return new Activation_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   overriding procedure Check_Compatibility
     (Self           :        Activation_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)

   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use Ada.Tags;

      use PolyORB.Errors;

      use PolyORB.POA_Policies.Servant_Retention_Policy;
      use PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

      use PolyORB.POA_Policies.Id_Assignment_Policy;
      use PolyORB.POA_Policies.Id_Assignment_Policy.System;

   begin
      --  Implcit activation requires System_ID and Retain Policies.

      for J in Other_Policies'Range loop
         if Other_Policies (J).all in ServantRetentionPolicy'Class
           and then Other_Policies (J).all'Tag /= Retain_Policy'Tag then
            Throw (Error,
                   InvalidPolicy_E,
                   InvalidPolicy_Members'(Index => 0));
         end if;

         if Other_Policies (J).all in IdAssignmentPolicy'Class
           and then Other_Policies (J).all'Tag /= System_Id_Policy'Tag then
            Throw (Error,
                   InvalidPolicy_E,
                   InvalidPolicy_Members'(Index => 0));
         end if;

      end loop;

   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   overriding function Policy_Id
     (Self : Activation_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "IMPLICIT_ACTIVATION_POLICY.ACTIVATION";
   end Policy_Id;

   -------------------------------
   -- Implicit_Activate_Servant --
   -------------------------------

   overriding procedure Implicit_Activate_Servant
     (Self      : Activation_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access;
      Hint      : Object_Id_Access;
      Oid       : out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      U_Oid : Unmarshalled_Oid;
   begin
      PolyORB.POA.Activate_Object
        (PolyORB.POA.Obj_Adapter_Access (OA), P_Servant, Hint, U_Oid, Error);
      Oid := U_Oid_To_Oid (U_Oid);
   end Implicit_Activate_Servant;

   -----------------------------------
   -- Ensure_No_Implicit_Activation --
   -----------------------------------

   overriding procedure Ensure_No_Implicit_Activation
     (Self      :        Activation_Policy;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      pragma Unreferenced (Self);

   begin
      Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
   end Ensure_No_Implicit_Activation;

end PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
