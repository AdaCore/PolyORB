------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY.RETAIN            --
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
with PolyORB.Types;
with PolyORB.POA;
with PolyORB.POA_Policies.Id_Uniqueness_Policy;
with PolyORB.POA_Policies.Lifespan_Policy;

package body PolyORB.POA_Policies.Servant_Retention_Policy.Retain is

   use PolyORB.Locks;
   use PolyORB.Object_Maps;

   ------------
   -- Create --
   ------------

   function Create return Retain_Policy_Access is
   begin
      return new Retain_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Retain_Policy;
      Other_Policies   : AllPolicies)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Warnings (On);
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Retain_Policy)
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
     (Self      : Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access;
      U_Oid     : Unmarshalled_Oid)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Locks;
      use PolyORB.Object_Maps;
      use PolyORB.POA_Policies.Id_Uniqueness_Policy;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      Ensure_Servant_Uniqueness
        (POA.Id_Uniqueness_Policy.all, OA, P_Servant);

      Lock_W (POA.Map_Lock);
      if U_Oid.System_Generated then
         Object_Maps.Get_By_Index
           (POA.Active_Object_Map.all,
            Integer'Value (Types.To_Standard_String (U_Oid.Id))).Servant
           := P_Servant;
      else
         declare
            The_Entry : Object_Map_Entry_Access
              := Get_By_Id (POA.Active_Object_Map.all, U_Oid);
            Index : Integer;
         begin
            if The_Entry = null then
               The_Entry := new Object_Map_Entry;
               The_Entry.Oid := new Unmarshalled_Oid'(U_Oid);
               Index := Add (POA.Active_Object_Map, The_Entry);
            end if;
            The_Entry.Servant := P_Servant;
         end;
      end if;
      Unlock_W (POA.Map_Lock);
   end Retain_Servant_Association;

   --------------------------------
   -- Forget_Servant_Association --
   --------------------------------

   procedure Forget_Servant_Association
     (Self  : Retain_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
      POA      : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

      An_Entry : Object_Map_Entry_Access;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Lock_W (POA.Map_Lock);
      An_Entry := Object_Maps.Remove_By_Id
        (POA.Active_Object_Map.all'Access, U_Oid);
      Unlock_W (POA.Map_Lock);

      if An_Entry = null then
         raise PolyORB.POA.Object_Not_Active;
      end if;

      --  Free the Unmarshalled_Oid_Access and the entry.
      --  The servant has to be freed by the application.
      Free (An_Entry.Oid);
      Free (An_Entry);
   end Forget_Servant_Association;

   ----------------------------
   -- Retained_Servant_To_Id --
   ----------------------------

   function Retained_Servant_To_Id
     (Self      : Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access)
     return Object_Id_Access
   is
      use PolyORB.POA_Policies.Id_Uniqueness_Policy;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      An_Entry : Object_Map_Entry_Access;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      if POA.Active_Object_Map /= null then
         Lock_R (POA.Map_Lock);
         An_Entry := Get_By_Servant (POA.Active_Object_Map.all, P_Servant);
         Unlock_R (POA.Map_Lock);

         if An_Entry /= null then
            return U_Oid_To_Oid (An_Entry.Oid.all);
         end if;
      end if;

      return null;
   end Retained_Servant_To_Id;

   ----------------------------
   -- Retained_Id_To_Servant --
   ----------------------------

   function Retained_Id_To_Servant
     (Self  : Retain_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
     return Servants.Servant_Access
   is
      use PolyORB.POA_Policies.Lifespan_Policy;

      An_Entry : Object_Map_Entry_Access;
      POA      : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Ensure_Lifespan
        (POA.Lifespan_Policy.all,
         OA, U_Oid);

      Lock_R (POA.Map_Lock);
      An_Entry := Get_By_Id (POA.Active_Object_Map.all, U_Oid);
      Unlock_R (POA.Map_Lock);
      if An_Entry /= null then
         return An_Entry.Servant;
      else
         return null;
      end if;
   end Retained_Id_To_Servant;

end PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
