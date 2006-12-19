------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.NO_ACTIVATION       --
--                                                                          --
--                                 B o d y                                  --
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

package body PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation is

   ------------
   -- Create --
   ------------

   function Create
     return No_Activation_Policy_Access is
   begin
      return new No_Activation_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        No_Activation_Policy;
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
      --  No rule to test.
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : No_Activation_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "IMPLICIT_ACTIVATION_POLICY.NO_ACTIVATION";
   end Policy_Id;

   -------------------------------
   -- Implicit_Activate_Servant --
   -------------------------------

   procedure Implicit_Activate_Servant
     (Self      :        No_Activation_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Unreferenced (P_Servant);
      pragma Unreferenced (Hint);

      use PolyORB.Errors;

   begin
      Oid := null;

      Throw
        (Error,
         ServantNotActive_E,
         Null_Member);
   end Implicit_Activate_Servant;

   -----------------------------------
   -- Ensure_No_Implicit_Activation --
   -----------------------------------

   procedure Ensure_No_Implicit_Activation
     (Self      :        No_Activation_Policy;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Error);

   begin
      null;
   end Ensure_No_Implicit_Activation;
end PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;
