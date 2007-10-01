------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY.NON_RETAIN          --
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

with Ada.Tags;

with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager;

package body PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain is

   ------------
   -- Create --
   ------------

   function Create
     return Non_Retain_Policy_Access is
   begin
      return new Non_Retain_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        Non_Retain_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use Ada.Tags;

      use PolyORB.Errors;
      use PolyORB.POA_Policies.Request_Processing_Policy;
      use PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
      use PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager;

   begin
      --  Compatiblity between Non_Retain and Id_Uniqueness done in
      --   PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique.

      --  Non_Retain requires either Use_Default_Servant
      --   or Use_Servant_Manager.

      for J in Other_Policies'Range loop
         if Other_Policies (J).all in RequestProcessingPolicy'Class
           and then not (Other_Policies (J).all'Tag
                         = Use_Default_Servant_Policy'Tag
                         or else Other_Policies (J).all'Tag
                         = Use_Servant_Manager_Policy'Tag)
         then
            Throw
              (Error,
               InvalidPolicy_E,
               InvalidPolicy_Members'(Index => 0));
         end if;
      end loop;

   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Non_Retain_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "SERVANT_RETENTION_POLICY.NON_RETAIN";
   end Policy_Id;

   --------------------------------
   -- Retain_Servant_Association --
   --------------------------------

   procedure Retain_Servant_Association
     (Self      :        Non_Retain_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      U_Oid     :        Unmarshalled_Oid;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, P_Servant, U_Oid, Error);
      pragma Warnings (On);

   begin
      --  NON_RETAIN: No active object map, nothing to retain,
      --  no way of checking ID uniqueness.

      null;
   end Retain_Servant_Association;

   --------------------------------
   -- Forget_Servant_Association --
   --------------------------------

   procedure Forget_Servant_Association
     (Self  :        Non_Retain_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid :        Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, U_Oid, Error);
      pragma Warnings (On);

   begin
      --  NON_RETAIN: Nothing to do.

      null;
   end Forget_Servant_Association;

   ----------------------------
   -- Retained_Servant_To_Id --
   ----------------------------

   function Retained_Servant_To_Id
     (Self      : Non_Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access)
     return Object_Id_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, P_Servant);
      pragma Warnings (On);

   begin
      --  NON_RETAIN: No retained object id available.

      return null;
   end Retained_Servant_To_Id;

   ----------------------------
   -- Retained_Id_To_Servant --
   ----------------------------

   procedure Retained_Id_To_Servant
     (Self    :        Non_Retain_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, U_Oid, Error);
      pragma Warnings (On);

   begin
      --  NON_RETAIN: No retained servant available.

      Servant := null;
   end Retained_Id_To_Servant;

   ---------------------------------
   -- Ensure_Servant_Manager_Type --
   ---------------------------------

   procedure Ensure_Servant_Manager_Type
     (Self    :        Non_Retain_Policy;
      Manager :        ServantManager'Class;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);

      use PolyORB.Errors;

   begin
      if Manager not in ServantLocator'Class then
         Throw (Error,
                Obj_Adapter_E,
                System_Exception_Members'(Minor     => 4,
                                          Completed => Completed_No));
      end if;
   end Ensure_Servant_Manager_Type;

end PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
