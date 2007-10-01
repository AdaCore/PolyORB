------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY.RETAIN            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Log;
with PolyORB.Object_Maps.System;
with PolyORB.Object_Maps.User;
with PolyORB.POA;
with PolyORB.POA_Policies.Id_Assignment_Policy;
with PolyORB.POA_Policies.Id_Uniqueness_Policy;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Tasking.Mutexes;
with PolyORB.Types;

package body PolyORB.POA_Policies.Servant_Retention_Policy.Retain is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Object_Maps;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;

   package L is new Log.Facility_Log
     ("polyorb.poa_policies.servant_retention_policy.retain");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ------------
   -- Create --
   ------------

   function Create
     return Retain_Policy_Access is
   begin
      return new Retain_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        Retain_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to test.
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Retain_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "SERVANT_RETENTION_POLICY.RETAIN";
   end Policy_Id;

   --------------------------------
   -- Retain_Servant_Association --
   --------------------------------

   procedure Retain_Servant_Association
     (Self      :        Retain_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      U_Oid     :        Unmarshalled_Oid;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Object_Maps.System;
      use PolyORB.Object_Maps.User;
      use PolyORB.POA_Policies.Id_Assignment_Policy;
      use PolyORB.POA_Policies.Id_Uniqueness_Policy;
      use type PolyORB.Servants.Servant_Access;

      POA : constant PolyORB.POA.Obj_Adapter_Access :=
        PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      pragma Debug (O ("Retain_Servant_Association: enter"));

      pragma Debug (O ("Inserting object '"
                       & To_Standard_String (U_Oid.Id)
                       & "'"));

      Ensure_Servant_Uniqueness
        (POA.Id_Uniqueness_Policy.all,
         OA,
         P_Servant,
         Error);

      if Found (Error) then
         return;
      end if;

      Enter (POA.Map_Lock);

      if POA.Active_Object_Map = null then
         pragma Debug (O ("Creating Object Map"));
         POA.Active_Object_Map := Create_Object_Map
           (POA.Id_Assignment_Policy.all);
      end if;
      pragma Assert (POA.Active_Object_Map /= null);

      --  Ensure compability between Object_Map and Object Id

      if (U_Oid.System_Generated
          and then POA.Active_Object_Map.all not in System_Object_Map'Class)
        or else
        ((not U_Oid.System_Generated)
         and then POA.Active_Object_Map.all not in User_Object_Map'Class)
      then
         Throw (Error,
                Internal_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         Leave (POA.Map_Lock);
         return;
      end if;

      --  Store Entry in Object Map

      declare
         The_Entry : Object_Map_Entry_Access :=
           Get_By_Id (POA.Active_Object_Map.all, U_Oid);
      begin
         if The_Entry = null then
            pragma Debug (O ("The entry is null, inserting new entry"));

            The_Entry     := new Object_Map_Entry;
            The_Entry.Oid := new Unmarshalled_Oid'(U_Oid);
            The_Entry.Servant := P_Servant;

            if U_Oid.System_Generated then
               pragma Debug (O ("Insert object at reused index "
                                & To_Standard_String (U_Oid.Id)));
               Add (System_Object_Map (POA.Active_Object_Map.all)'Access,
                    The_Entry,
                    Integer'Value (To_Standard_String (U_Oid.Id)));

            else
               Add (User_Object_Map (POA.Active_Object_Map.all)'Access,
                    The_Entry);
            end if;

         else
            pragma Debug (O ("The entry is not null"));

            if The_Entry.Servant /= null then
               Throw (Error,
                      ObjectAlreadyActive_E,
                      Null_Members'(Null_Member));
               Leave (POA.Map_Lock);
               return;
            end if;

            The_Entry.Servant := P_Servant;
         end if;
      end;

      Leave (POA.Map_Lock);
      pragma Debug (O ("Retain_Servant_Association: leave"));
   end Retain_Servant_Association;

   --------------------------------
   -- Forget_Servant_Association --
   --------------------------------

   procedure Forget_Servant_Association
     (Self  :        Retain_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid :        Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      POA : constant PolyORB.POA.Obj_Adapter_Access :=
        PolyORB.POA.Obj_Adapter_Access (OA);

      An_Entry : Object_Map_Entry_Access;
   begin
      pragma Debug (O ("Removing object '"
                       & To_Standard_String (U_Oid.Id)
                       & "'"));

      Enter (POA.Map_Lock);
      An_Entry := Object_Maps.Remove_By_Id (POA.Active_Object_Map, U_Oid);
      Leave (POA.Map_Lock);

      if An_Entry = null then
         PolyORB.Errors.Throw
           (Error,
            ObjectNotActive_E,
            Null_Member);
      else

         --  Free the Unmarshalled_Oid_Access and the entry.
         --  Note: The servant has to be freed by the application.

         Free (An_Entry.Oid);
         Free (An_Entry);
      end if;
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
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.POA_Policies.Id_Uniqueness_Policy;

      POA : constant PolyORB.POA.Obj_Adapter_Access :=
        PolyORB.POA.Obj_Adapter_Access (OA);
      An_Entry : Object_Map_Entry_Access;
   begin
      if POA.Active_Object_Map /= null then
         Enter (POA.Map_Lock);
         An_Entry := Get_By_Servant (POA.Active_Object_Map.all, P_Servant);
         Leave (POA.Map_Lock);

         pragma Debug (O ("Retained_Servant_To_Id : entry null ? "
                          & Boolean'Image (An_Entry = null)));

         if An_Entry /= null then
            pragma Debug (O ("Entry name is: " &
                             To_Standard_String (An_Entry.Oid.Id)));

            return U_Oid_To_Oid (An_Entry.Oid.all);
         end if;
      end if;

      return null;
   end Retained_Servant_To_Id;

   ----------------------------
   -- Retained_Id_To_Servant --
   ----------------------------

   procedure Retained_Id_To_Servant
     (Self    :        Retain_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);

      use PolyORB.POA_Policies.Lifespan_Policy;

      An_Entry : Object_Map_Entry_Access;
      POA      : constant PolyORB.POA.Obj_Adapter_Access :=
        PolyORB.POA.Obj_Adapter_Access (OA);

   begin
      Ensure_Lifespan
        (POA.Lifespan_Policy.all,
         OA,
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      pragma Debug (O ("Looking for object '"
                       & To_Standard_String (U_Oid.Id)
                       & "'"));

      if POA.Active_Object_Map = null then
         pragma Debug (O ("Active Object Map is null!"));
         Servant := null;
         return;
      end if;

      Enter (POA.Map_Lock);
      An_Entry := Get_By_Id (POA.Active_Object_Map.all, U_Oid);
      Leave (POA.Map_Lock);

      if An_Entry /= null then
         pragma Debug (O ("Object found"));
         Servant := An_Entry.Servant;
      else
         pragma Debug (O ("Object not found"));
         Servant := null;
      end if;
   end Retained_Id_To_Servant;

   ---------------------------------
   -- Ensure_Servant_Manager_Type --
   ---------------------------------

   procedure Ensure_Servant_Manager_Type
     (Self    :        Retain_Policy;
      Manager :        ServantManager'Class;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);

   begin
      if Manager not in ServantActivator'Class then
         Throw (Error,
                Obj_Adapter_E,
                System_Exception_Members'(Minor     => 4,
                                          Completed => Completed_No));
      end if;
   end Ensure_Servant_Manager_Type;

end PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
