------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  POLYORB.POA_POLICIES.REQUEST_PROCESSING_POLICY.ACTIVE_OBJECT_MAP_ONLY   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

with PolyORB.POA_Policies.Servant_Retention_Policy;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
with PolyORB.POA;

package body
  PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only
is

   ------------
   -- Create --
   ------------

   function Create return Active_Map_Only_Policy_Access is
   begin
      return new Active_Map_Only_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Active_Map_Only_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      if not
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all in
         POA_Policies.Servant_Retention_Policy.Retain.Retain_Policy)
      then
         raise PolyORB.POA.Invalid_Policy;
      end if;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Active_Map_Only_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "REQUEST_PROCESSING_POLICY.ACTIVE_MAP_ONLY";
   end Policy_Id;

   ---------------------
   -- Etherealize_All --
   ---------------------

   procedure Etherealize_All
     (Self  : Active_Map_Only_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Unreferenced (U_Oid);
      pragma Warnings (On);
      null;
   end Etherealize_All;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self  : Active_Map_Only_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
      use PolyORB.POA_Policies.Servant_Retention_Policy;

      Oid : constant Object_Id_Access
        := Servant_To_Id
        (POA.Obj_Adapter_Access
         (OA).Servant_Retention_Policy.all,
         OA,
         P_Servant);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return Oid;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : Active_Map_Only_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access;
      Oid  : Object_Id)
     return Servant_Access
   is
      use PolyORB.POA_Policies.Servant_Retention_Policy;

      Oid_A : Object_Id_Access := new Object_Id'(Oid);
      Servant : Servant_Access;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      Servant := Id_To_Servant
        (POA.Obj_Adapter_Access (OA).Servant_Retention_Policy.all,
         OA,
         Oid_To_U_Oid (Oid_A));
      Free (Oid_A);
      if Servant = null then
         raise PolyORB.POA.Object_Not_Active;
      end if;
      return Servant;
   end Id_To_Servant;

end PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
