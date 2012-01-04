------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   POLYORB.POA_POLICIES.REQUEST_PROCESSING_POLICY.USE_DEFAULT_SERVANT     --
--                                                                          --
--                                 S p e c                                  --
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

package PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant is

   type Use_Default_Servant_Policy is
     new RequestProcessingPolicy with null record;

   type Use_Default_Servant_Policy_Access is
     access all Use_Default_Servant_Policy;

   function Create
     return Use_Default_Servant_Policy_Access;

   procedure Check_Compatibility
     (Self           :        Use_Default_Servant_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   function Policy_Id
     (Self : Use_Default_Servant_Policy)
     return String;

   procedure Id_To_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Set_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Ensure_Servant_Manager
     (Self  :        Use_Default_Servant_Policy;
      Error : in out PolyORB.Errors.Error_Container);

end PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
