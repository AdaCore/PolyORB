------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY.UNIQUE              --
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

with PolyORB.Locks;
with PolyORB.Object_Maps;
with PolyORB.POA;

package body PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique is

   use PolyORB.Locks;
   use PolyORB.Object_Maps;

   ------------
   -- Create --
   ------------

   function Create return Unique_Id_Policy_Access is
   begin
      return new Unique_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Unique_Id_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access) is
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
     (Self : Unique_Id_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "ID_UNIQUENESS_POLICY.UNIQUE_ID";
   end Policy_Id;

   -------------------------------
   -- Ensure_Servant_Uniqueness --
   -------------------------------

   procedure Ensure_Servant_Uniqueness
     (Self      : Unique_Id_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
   is
      P_OA : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      if P_OA.Active_Object_Map /= null then
         Lock_R (P_OA.Map_Lock);
         if Is_Servant_In (P_OA.Active_Object_Map.all, P_Servant) then
            raise PolyORB.POA.Servant_Already_Active;
         end if;
         Unlock_R (P_OA.Map_Lock);
      end if;
   end Ensure_Servant_Uniqueness;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : Unique_Id_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
      P_OA        : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      An_Entry    : Object_Map_Entry_Access;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      if P_OA.Active_Object_Map /= null then
         Lock_R (P_OA.Map_Lock);
         An_Entry := Get_By_Servant (P_OA.Active_Object_Map.all, P_Servant);
         if An_Entry /= null then
            return U_Oid_To_Oid (An_Entry.Oid.all);
         end if;
         Unlock_R (P_OA.Map_Lock);
      end if;
      return null;
   end Servant_To_Id;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in     Unique_Id_Policy;
                   Ptr  : in out Policy_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      Free (Unique_Id_Policy_Access (Ptr));
   end Free;

end PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;
