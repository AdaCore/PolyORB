------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.LIFESPAN_POLICY.PERSISTENT              --
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

package body PolyORB.POA_Policies.Lifespan_Policy.Persistent is

   ------------
   -- Create --
   ------------

   function Create
     return Persistent_Policy_Access is
   begin
      return new Persistent_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   overriding procedure Check_Compatibility
     (Self           :        Persistent_Policy;
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

   overriding function Policy_Id
     (Self : Persistent_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "LIFESPAN_POLICY.PERSISTENT";
   end Policy_Id;

   -------------------------
   -- Get_Lifespan_Cookie --
   -------------------------

   overriding function Get_Lifespan_Cookie
     (Self : Persistent_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
     return Time_Stamp
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);

   begin
      return Null_Time_Stamp;
   end Get_Lifespan_Cookie;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   overriding procedure Ensure_Lifespan
     (Self  :        Persistent_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid :        Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);

      use PolyORB.Errors;

   begin
      if U_Oid.Persistency_Flag /= Null_Time_Stamp then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
      end if;
   end Ensure_Lifespan;

end PolyORB.POA_Policies.Lifespan_Policy.Persistent;
