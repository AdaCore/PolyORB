------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.SYSTEM              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

with PolyORB.Log;
with PolyORB.Object_Maps.System;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Tasking.Mutexes;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.POA_Policies.Id_Assignment_Policy.System is

   use PolyORB.Log;
   use PolyORB.Object_Maps;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;

   package L is new Log.Facility_Log
     ("polyorb.poa_policies.id_assignement_policy.system");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ------------
   -- Create --
   ------------

   function Create
     return System_Id_Policy_Access is
   begin
      return new System_Id_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        System_Id_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, Other_Policies, Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to test.
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : System_Id_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "ID_ASSIGNMENT_POLICY.SYSTEM_ID";
   end Policy_Id;

   -----------------------
   -- Create_Object_Map --
   -----------------------

   function Create_Object_Map
     (Self : System_Id_Policy)
     return PolyORB.Object_Maps.Object_Map_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      Result : constant PolyORB.Object_Maps.Object_Map_Access
        := new PolyORB.Object_Maps.System.System_Object_Map;
   begin
      PolyORB.Object_Maps.Initialize (Result.all);
      return Result;
   end Create_Object_Map;

   ------------------------------
   -- Assign_Object_Identifier --
   ------------------------------

   procedure Assign_Object_Identifier
     (Self  :        System_Id_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)

   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.POA_Policies.Lifespan_Policy;
      use PolyORB.Object_Maps.System;
      use PolyORB.Exceptions;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      The_Entry : Object_Map_Entry_Access;
      Index : Integer;

   begin
      pragma Debug (O ("Assign_Object_Identifier: enter"));

      Enter (POA.Map_Lock);

      if POA.Active_Object_Map = null then
         POA.Active_Object_Map := Create_Object_Map
           (POA.Id_Assignment_Policy.all);
      end if;

      if POA.Active_Object_Map.all not in System_Object_Map'Class then
         Throw (Error,
                Internal_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         Leave (POA.Map_Lock);
         return;
      end if;

      if Hint /= null then
         pragma Debug (O ("Hint is not null"));

         declare
            U_Hint : Unmarshalled_Oid;

         begin
            Oid_To_U_Oid (Hint.all, U_Hint, Error);

            if not U_Hint.System_Generated
              or else Found (Error)
            then
               Throw (Error,
                      Bad_Param_E,
                      System_Exception_Members'(Minor => 0,
                                                Completed => Completed_No));
               Leave (POA.Map_Lock);
               return;
            end if;

            --  Hint is a valid system generated oid. We reserve a
            --  slot for this oid in POA's active object map. Servant
            --  information is still null at this point. It will be
            --  added later.

            Index := Integer'Value (To_Standard_String (U_Hint.Id));
            The_Entry := new Object_Map_Entry;
            The_Entry.Oid
              := PolyORB.POA_Types.Create_Id
              (Name             =>
                 To_PolyORB_String (PolyORB.Utils.Trimmed_Image (Index)),
               System_Generated => True,
               Persistency_Flag =>
                 Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
               Creator          => POA.Absolute_Address);

            Add (System_Object_Map (POA.Active_Object_Map.all)'Access,
                 The_Entry,
                 Index);

            Leave (POA.Map_Lock);
         end;

      else
         pragma Debug (O ("Hint is null"));
         --  XXX possible memory leak, to investigate.
         --  XXX If the servant retention policy is NON_RETAIN,
         --   should we not get rid of the active object map
         --   altogether? But in that case how does system id
         --   attribution cooperate with id_uniqueness_policy?

         The_Entry := new Object_Map_Entry;

         Index := Add
           (System_Object_Map (POA.Active_Object_Map.all)'Access,
            The_Entry);

         The_Entry.Oid
           := PolyORB.POA_Types.Create_Id
           (Name             =>
              To_PolyORB_String (PolyORB.Utils.Trimmed_Image (Index)),
            System_Generated => True,
            Persistency_Flag =>
              Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
            Creator          => POA.Absolute_Address);

         Leave (POA.Map_Lock);
      end if;

      pragma Debug (O ("Object Name is '"
                       & PolyORB.Utils.Trimmed_Image (Index)
                       & "'"));

      U_Oid := The_Entry.Oid.all;
      pragma Debug (O ("Assign_Object_Identifier: leave"));
   end Assign_Object_Identifier;

   -----------------------------------
   -- Reconstruct_Object_Identifier --
   -----------------------------------

   procedure Reconstruct_Object_Identifier
     (Self  :        System_Id_Policy;
      OA    :        Obj_Adapter_Access;
      Oid   :        Object_Id;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); -- WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (OA);
      pragma Warnings (On); -- WAG:3.15

   begin
      PolyORB.POA_Types.Oid_To_U_Oid (Oid, U_Oid, Error);
   end Reconstruct_Object_Identifier;

   -----------------------
   -- Object_Identifier --
   -----------------------

   procedure Object_Identifier
     (Self   :     System_Id_Policy;
      Oid    :     Object_Id_Access;
      Result : out Object_Id_Access)
   is
      pragma Warnings (Off); -- WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); -- WAG:3.15

   begin
      Result := new Object_Id'(Oid.all);
   end Object_Identifier;

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
