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

with Ada.Unchecked_Conversion;

with PolyORB.Object_Maps;
with PolyORB.POA;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Locks;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils;
with PolyORB.Utils.Strings;

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

   ------------------------------
   -- Assign_Object_Identifier --
   ------------------------------

   function Assign_Object_Identifier
     (Self   : System_Id_Policy;
      OA     : PolyORB.POA_Types.Obj_Adapter_Access;
      Hint   : Object_Id_Access)
     return Unmarshalled_Oid
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      --  Object_Id_Info : Unmarshalled_Oid;
      The_Entry : Object_Map_Entry_Access;
      Index : Integer;

      use PolyORB.Object_Maps;

      function As_String_Ptr is new Ada.Unchecked_Conversion
        (Object_Id_Access, Utils.Strings.String_Ptr);

   begin
      Lock_W (POA.Map_Lock);
      if POA.Active_Object_Map = null then
         POA.Active_Object_Map := new Object_Map;
      end if;
      pragma Assert (POA.Active_Object_Map /= null);

      if Hint /= null then
         begin
            Index := Integer'Value (As_String_Ptr (Hint).all);
         exception
            when others =>
               raise PolyORB.POA.Invalid_Policy;
         end;

         The_Entry := Get_By_Index
           (POA.Active_Object_Map.all, Index);
         Unlock_W (POA.Map_Lock);

         if The_Entry = null then
            raise PolyORB.POA.Invalid_Policy;
            --  Could not determine the slot associated with
            --  this index.
            --  XXX if this is a POA with the PERSISTENT lifespan
            --  policy, then dummy slots should be allocated to
            --  bring this index back into existence.
         end if;
      else

         --  XXX possible memory leak, to investigate.
         --  XXX If the servant retention policy is NON_RETAIN,
         --   should we not get rid of the active object map
         --   altogether? But in that case how does system id
         --   attribution cooperate with id_uniqueness_policy?

         The_Entry := new Object_Map_Entry;
         Index := Add (POA.Active_Object_Map, The_Entry);
         Unlock_W (POA.Map_Lock);

         The_Entry.Oid := new Unmarshalled_Oid;
         The_Entry.Oid.Id := To_PolyORB_String
           (PolyORB.Utils.Trimmed_Image (Index));
         The_Entry.Oid.System_Generated := True;
         The_Entry.Oid.Persistency_Flag
           := PolyORB.POA_Policies.Lifespan_Policy.Get_Lifespan_Cookie
           (POA.Lifespan_Policy.all, OA);
         The_Entry.Oid.Creator := POA.Absolute_Address;
      end if;

      return The_Entry.Oid.all;
   exception
      when others =>
         Unlock_W (POA.Map_Lock);
         raise;
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

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
