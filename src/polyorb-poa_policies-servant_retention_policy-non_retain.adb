------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY.NON_RETAIN          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

package body PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain is

   ------------
   -- Create --
   ------------

   function Create return Non_Retain_Policy_Access is
   begin
      return new Non_Retain_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Non_Retain_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Warnings (On);
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Non_Retain_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "SERVANT_RETENTION_POLICY.RETAIN";
   end Policy_Id;

   --------------------------------
   -- Retain_Servant_Association --
   --------------------------------

   procedure Retain_Servant_Association
     (Self      : Non_Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Objects.Servant_Access;
      U_Oid     : Unmarshalled_Oid)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA, P_Servant, U_Oid);
      pragma Warnings (On);
   begin
      null;
      --  NON_RETAIN: No active object map, nothing to retain,
      --  no way of checking ID uniqueness.
   end Retain_Servant_Association;

   --------------------------------
   -- Forget_Servant_Association --
   --------------------------------

   procedure Forget_Servant_Association
     (Self  : Non_Retain_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
      pragma Unreferenced (Self, OA, U_Oid);
   begin
      null;
      --  NON_RETAIN: Nothing to do.
   end Forget_Servant_Association;

   ----------------------------
   -- Retained_Servant_To_Id --
   ----------------------------

   function Retained_Servant_To_Id
     (Self      : Non_Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Objects.Servant_Access)
     return Object_Id_Access
   is
      pragma Unreferenced (Self, OA, P_Servant);
   begin
      --  NON_RETAIN: No retained object id available.
      return null;
   end Retained_Servant_To_Id;

   ----------------------------
   -- Retained_Id_To_Servant --
   ----------------------------

   function Retained_Id_To_Servant
     (Self  : Non_Retain_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
     return Objects.Servant_Access
   is
      pragma Unreferenced (Self, OA, U_Oid);
   begin
      --  NON_RETAIN: No retained servant available.
      return null;
   end Retained_Id_To_Servant;

end PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
