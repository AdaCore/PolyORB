------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.ID_ASSIGNMENT_POLICY.SYSTEM              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Conversion;

with PolyORB.Object_Maps;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.POA_Policies.Lifespan_Policy;
with PolyORB.Tasking.Rw_Locks;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.POA_Policies.Id_Assignment_Policy.System is

   use PolyORB.Object_Maps;
   use PolyORB.Tasking.Rw_Locks;
   use PolyORB.Types;

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
     (Self           : System_Id_Policy;
      Other_Policies : AllPolicies;
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
      use PolyORB.Object_Maps;
      use PolyORB.Exceptions;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access (OA);
      The_Entry : Object_Map_Entry_Access;
      Index : Integer;

      function As_String_Ptr is new Ada.Unchecked_Conversion
        (Object_Id_Access, Utils.Strings.String_Ptr);

   begin
      Lock_W (POA.Map_Lock);

      if POA.Active_Object_Map = null then
         POA.Active_Object_Map := new Object_Map;
      end if;
      pragma Assert (POA.Active_Object_Map /= null);

      if Hint /= null then

         Index := Integer'Value (As_String_Ptr (Hint).all);
         The_Entry := Get_By_Id
           (POA.Active_Object_Map.all, Oid_To_U_Oid (Hint));
         Unlock_W (POA.Map_Lock);

         if The_Entry = null then
            Throw (Error,
                   InvalidPolicy_E,
                   InvalidPolicy_Members'(Index => 0));

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

         The_Entry.Oid
           := PolyORB.POA_Types.Create_Id
           (Name => To_PolyORB_String (PolyORB.Utils.Trimmed_Image (Index)),
            System_Generated => True,
            Persistency_Flag =>
              Get_Lifespan_Cookie (POA.Lifespan_Policy.all, OA),
            Creator => POA.Absolute_Address);
      end if;

      U_Oid := The_Entry.Oid.all;
   end Assign_Object_Identifier;

end PolyORB.POA_Policies.Id_Assignment_Policy.System;
