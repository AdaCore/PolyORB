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
with PolyORB.Object_Maps.Seq;
with PolyORB.POA;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Locks;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.POA_Policies.Id_Assignment_Policy.System is

   use PolyORB.Locks;
   use PolyORB.Object_Maps;
   use PolyORB.Object_Maps.Seq;

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
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : System_Id_Policy)
     return String is
   begin
      return "ID_ASSIGNMENT_POLICY.SYSTEM_ID";
   end Policy_Id;

   ---------------
   -- Is_System --
   ---------------

   function Is_System (P : System_Id_Policy) return Boolean
   is
   begin
      return True;
   end Is_System;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self   : System_Id_Policy;
      OA     : PolyORB.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access)
     return Object_Id_Access
   is
      P_OA      : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      New_Entry : Seq_Object_Map_Entry_Access;
      Index     : Integer;
   begin
      New_Entry         := new Seq_Object_Map_Entry;
      New_Entry.Servant := Object;

      Lock_W (P_OA.Map_Lock);
      if P_OA.Active_Object_Map = null then
         P_OA.Active_Object_Map := Object_Map_Access (New_Map);
      end if;
      Index := Add (P_OA.Active_Object_Map.all'Access,
                    Object_Map_Entry_Access (New_Entry));

      New_Entry.Oid := new Unmarshalled_Oid;
      New_Entry.Oid.Id := To_PolyORB_String
        (PolyORB.Utils.Trimmed_Image (Index));
      New_Entry.Oid.System_Generated := True;
      New_Entry.Oid.Persistency_Flag
        := PolyORB.POA_Policies.Lifespan_Policy.Get_Time_Stamp
           (P_OA.Lifespan_Policy.all, OA);
      New_Entry.Oid.Creator := P_OA.Absolute_Address;
      Unlock_W (P_OA.Map_Lock);
      return U_Oid_To_Oid (New_Entry.Oid.all);
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   --  XXX Oid should be passed as anonymous access.

   procedure Activate_Object_With_Id
     (Self   : System_Id_Policy;
      OA     : PolyORB.POA_Types.Obj_Adapter_Access;
      Object : Servant_Access;
      Oid    : Object_Id)
   is
      P_OA      : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      Oid_A : Object_Id_Access := new Object_Id'(Oid);
      New_Entry : Seq_Object_Map_Entry_Access;
   begin
      New_Entry         := new Seq_Object_Map_Entry;
      New_Entry.Oid     := new Unmarshalled_Oid'(Oid_To_U_Oid (Oid_A));
      Free (Oid_A);
      New_Entry.Servant := Object;

      Lock_W (P_OA.Map_Lock);
      Replace_By_Index
        (P_OA.Active_Object_Map.all'Access,
         Object_Map_Entry_Access (New_Entry),
         Integer'Value (To_Standard_String (New_Entry.Oid.Id)));
      Unlock_W (P_OA.Map_Lock);
   end Activate_Object_With_Id;

   -----------------------
   -- Ensure_Oid_Origin --
   -----------------------

   procedure Ensure_Oid_Origin
     (Self  : System_Id_Policy;
      U_Oid : Unmarshalled_Oid)
   is
   begin
      if U_Oid.System_Generated = False then
         raise PolyORB.POA.Bad_Param;
      end if;
   end Ensure_Oid_Origin;

   ---------------------------
   -- Ensure_Oid_Uniqueness --
   ---------------------------

   procedure Ensure_Oid_Uniqueness
     (Self  : System_Id_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid)
   is
      An_Entry : Object_Map_Entry_Access;
      P_OA      : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      Index : Integer := Integer'Value (To_Standard_String (U_Oid.Id));
   begin
      Lock_R (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      Unlock_R (P_OA.Map_Lock);
      if An_Entry /= null then
         raise PolyORB.POA.Object_Already_Active;
      end if;
   end Ensure_Oid_Uniqueness;

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
      Lock_W (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry = null then
         raise PolyORB.POA.Object_Not_Active;
      end if;
      An_Entry := Remove_By_Index (P_OA.Active_Object_Map.all'Access, Index);
      Unlock_W (P_OA.Map_Lock);
      --  Frees only the Unmarshalled_Oid_Access and the entry.
      --  The servant has to be freed by the application.
      Free (An_Entry.Oid);
      Free (Seq_Object_Map_Entry_Access (An_Entry));
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
      Lock_R (P_OA.Map_Lock);
      An_Entry := Get_By_Index (P_OA.Active_Object_Map.all, Index);
      if An_Entry /= null then
         Servant := An_Entry.Servant;
      end if;
      Unlock_R (P_OA.Map_Lock);
      return Servant;
   end Id_To_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     System_Id_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (System_Id_Policy_Access (Ptr));
   end Free;

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
