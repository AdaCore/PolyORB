------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   POLYORB.POA_POLICIES.REQUEST_PROCESSING_POLICY.USE_DEFAULT_SERVANT     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Tags;

with PolyORB.POA;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Servant_Retention_Policy;

package body
  PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant
is

   ------------
   -- Create --
   ------------

   function Create
     return Use_Default_Servant_Policy_Access is
   begin
      return new Use_Default_Servant_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        Use_Default_Servant_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use Ada.Tags;

      use PolyORB.Exceptions;

      use PolyORB.POA_Policies.Id_Uniqueness_Policy;
      use PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;

   begin
      --  Use_Default_Servant requires Multiple_Id

      for J in Other_Policies'Range loop
         if Other_Policies (J).all in IdUniquenessPolicy'Class
           and then Other_Policies (J).all'Tag
           /= Multiple_Id_Policy'Tag then
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
     (Self : Use_Default_Servant_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "REQUEST_PROCESSING_POLICY.USE_DEFAULT_SERVANT";
   end Policy_Id;

   ---------------------
   -- Etherealize_All --
   ---------------------

   procedure Etherealize_All
     (Self  : Use_Default_Servant_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Unreferenced (U_Oid);
      pragma Warnings (On);
   begin
      null;
   end Etherealize_All;

   -------------------
   -- Id_To_Servant --
   -------------------

   procedure Id_To_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Exceptions;
      use PolyORB.POA_Policies.Servant_Retention_Policy;

      use type PolyORB.Servants.Servant_Access;

   begin
      Retained_Id_To_Servant
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all,
         OA,
         U_Oid,
         Servant,
         Error);

      if Found (Error) then
         return;
      end if;

      if Servant = null then
         if POA.Obj_Adapter_Access (OA).Default_Servant /= null then
            Servant := POA.Obj_Adapter_Access (OA).Default_Servant;
         else
            Throw (Error,
                   NoServant_E,
                   Null_Members'(Null_Member));
         end if;
      end if;
   end Id_To_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (Error);
      pragma Warnings (On); --  WAG:3.15

   begin
      POA.Obj_Adapter_Access (OA).Default_Servant := Servant;
   end Set_Servant;

   -----------------
   -- Get_Servant --
   -----------------

   procedure Get_Servant
     (Self    :        Use_Default_Servant_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15

      use PolyORB.Exceptions;

      use type PolyORB.Servants.Servant_Access;

   begin
      if POA.Obj_Adapter_Access (OA).Default_Servant /= null then
         Servant := POA.Obj_Adapter_Access (OA).Default_Servant;
      else
         Throw (Error,
                NoServant_E,
                Null_Members'(Null_Member));
      end if;
   end Get_Servant;

   ----------------------------
   -- Ensure_Servant_Manager --
   ----------------------------

   procedure Ensure_Servant_Manager
     (Self  :        Use_Default_Servant_Policy;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15

      use PolyORB.Exceptions;

   begin
      Throw (Error,
             WrongPolicy_E,
             Null_Members'(Null_Member));
   end Ensure_Servant_Manager;

end PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
