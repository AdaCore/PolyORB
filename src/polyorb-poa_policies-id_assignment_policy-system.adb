------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.SYSTEM              --
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

with PolyORB.Object_Maps;
with PolyORB.POA;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Locks;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.POA_Policies.Id_Assignment_Policy.System is

   use PolyORB.Locks;
   use PolyORB.Object_Maps;

   ------------
   -- Create --
   ------------

   function Create return System_Id_Policy_Access is
   begin
      return new System_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : System_Id_Policy;
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
     (Self : System_Id_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "ID_ASSIGNMENT_POLICY.SYSTEM_ID";
   end Policy_Id;

   ---------------
   -- Is_System --
   ---------------

   function Is_System (Self : System_Id_Policy) return Boolean
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return True;
   end Is_System;

   function Assign_Object_Identifier
     (Self   : System_Id_Policy;
      OA     : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint   : Object_Id_Access)
     return Unmarshalled_Oid
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      Object_Id_Info : Unmarshalled_Oid;
      The_Entry : Object_Map_Entry_Access;
      Index : Integer;

      use PolyORB.Object_Maps;

   begin
      Lock_W (POA.Map_Lock);
      if POA.Active_Object_Map = null then
         POA.Active_Object_Map := new Object_Map;
      end if;
      pragma Assert (POA.Active_Object_Map /= null);

      if Hint /= null then
         Object_Id_Info := Oid_To_U_Oid (Hint);
         if not Object_Id_Info.System_Generated then
            raise PolyORB.POA.Invalid_Policy;
         end if;
         The_Entry := Get_By_Index
           (POA.Active_Object_Map.all,
            Integer'Value (Types.To_Standard_String
                           (Object_Id_Info.Id)));
         if The_Entry = null then
            raise Not_Implemented;
            --  Actually should bring the proper index
            --  in the active object map into existence.
         end if;
      else

         --  XXX possible memory leak, to investigate.

         The_Entry := new Object_Map_Entry;
         Index := Add (POA.Active_Object_Map, The_Entry);

         The_Entry.Oid := new Unmarshalled_Oid;
         The_Entry.Oid.Id := To_PolyORB_String
           (PolyORB.Utils.Trimmed_Image (Index));
         The_Entry.Oid.System_Generated := True;
         The_Entry.Oid.Persistency_Flag
           := PolyORB.POA_Policies.Lifespan_Policy.Get_Lifespan_Cookie
           (POA.Lifespan_Policy.all, OA);
         The_Entry.Oid.Creator := POA.Absolute_Address;
      end if;
      Unlock_W (POA.Map_Lock);
      return The_Entry.Oid.all;
   end Assign_Object_Identifier;

   -----------------------
   -- Ensure_Oid_Origin --
   -----------------------

   procedure Ensure_Oid_Origin
     (Self  : System_Id_Policy;
      U_Oid : Unmarshalled_Oid)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      if U_Oid.System_Generated = False then
         raise PolyORB.POA.Bad_Param;
      end if;
   end Ensure_Oid_Origin;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry
     (Self  : System_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
      An_Entry : Object_Map_Entry_Access;
      Index    : Integer
        := Integer'Value (To_Standard_String (U_Oid.Id));
      P_OA     : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      Lock_W (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry = null then
         raise PolyORB.POA.Object_Not_Active;
      end if;
      An_Entry := Object_Maps.Remove_By_Index
        (P_OA.Active_Object_Map.all'Access, Index);
      Unlock_W (P_OA.Map_Lock);
      --  Frees only the Unmarshalled_Oid_Access and the entry.
      --  The servant has to be freed by the application.
      Free (An_Entry.Oid);
      Free (An_Entry);
   end Remove_Entry;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self  : System_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
     return Servant_Access
   is
      An_Entry : Object_Map_Entry_Access;
      Index    : Integer
        := Integer'Value (To_Standard_String (U_Oid.Id));
      P_OA     : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      Servant  : Servant_Access;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      Lock_R (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry /= null then
         Servant := An_Entry.Servant;
      end if;
      Unlock_R (P_OA.Map_Lock);
      return Servant;
   end Id_To_Servant;

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
