------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY.UNIQUE              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with Ada.Tags;

with PolyORB.Object_Maps;
with PolyORB.POA;
with PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
with PolyORB.Tasking.Mutexes;

package body PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique is

   use PolyORB.Errors;
   use PolyORB.Object_Maps;
   use PolyORB.Tasking.Mutexes;

   ------------
   -- Create --
   ------------

   function Create
     return Unique_Id_Policy_Access is
   begin
      return new Unique_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   overriding procedure Check_Compatibility
     (Self           :        Unique_Id_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use Ada.Tags;

      use PolyORB.POA_Policies.Servant_Retention_Policy;
      use PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;

   begin
      --  Unique_Id and Non_Retain policies are not compatible.

      for J in Other_Policies'Range loop
         if Other_Policies (J).all in ServantRetentionPolicy'Class
           and then Other_Policies (J).all'Tag = Non_Retain_Policy'Tag
         then
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
     (Self : Unique_Id_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "ID_UNIQUENESS_POLICY.UNIQUE_ID";
   end Policy_Id;

   -------------------------------
   -- Ensure_Servant_Uniqueness --
   -------------------------------

   overriding procedure Ensure_Servant_Uniqueness
     (Self      :        Unique_Id_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      if POA.Active_Object_Map /= null then
         Enter (POA.Map_Lock);

         if Is_Servant_In (POA.Active_Object_Map.all, P_Servant) then
            Throw (Error,
                   ServantAlreadyActive_E,
                   Null_Members'(Null_Member));
         end if;

         Leave (POA.Map_Lock);
      end if;
   end Ensure_Servant_Uniqueness;

   --------------------
   -- Activate_Again --
   --------------------

   overriding procedure Activate_Again
     (Self      :        Unique_Id_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Oid       :        Object_Id_Access;
      Result    :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      if Oid /= null then
         --  UNIQUE policy: if already active, return the
         --  previous value.
         Result := Oid;
      else
         --  If this servant is not activated yet, try to do
         --  implicit activation now.
         declare
            U_Oid : Unmarshalled_Oid;

         begin
            PolyORB.POA.Activate_Object
              (PolyORB.POA.Obj_Adapter_Access (OA),
               P_Servant,
               Oid,
               U_Oid,
               Error);

            if Found (Error) then
               return;
            end if;

            Result := U_Oid_To_Oid (U_Oid);
         end;
      end if;
   end Activate_Again;

end PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;
