------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.ACTIVATION         --
--                                                                          --
--                                 S p e c                                  --
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

package PolyORB.POA_Policies.Implicit_Activation_Policy.Activation is

   type Activation_Policy is new ImplicitActivationPolicy with null record;

   type Activation_Policy_Access is access all Activation_Policy;

   function Create
     return Activation_Policy_Access;

   overriding procedure Check_Compatibility
     (Self           :        Activation_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   overriding function Policy_Id
     (Self : Activation_Policy)
     return String;

   overriding procedure Implicit_Activate_Servant
     (Self      :        Activation_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container);

   overriding procedure Ensure_No_Implicit_Activation
     (Self      :        Activation_Policy;
      Error     : in out PolyORB.Errors.Error_Container);

end PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
