------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.ACTIVATION         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

   procedure Check_Compatibility
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

   function Policy_Id
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

   procedure Implicit_Activate_Servant
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

   procedure Ensure_No_Implicit_Activation
     (Self      :        Activation_Policy;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      pragma Unreferenced (Self);

   begin
      Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
   end Ensure_No_Implicit_Activation;

end PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
