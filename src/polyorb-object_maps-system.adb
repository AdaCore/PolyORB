------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O B J E C T _ M A P S . S Y S T E M            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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
with PolyORB.Types;

package body PolyORB.Object_Maps.System is

   use Map_Entry_Tables;

   use PolyORB.Log;
   use PolyORB.POA_Types;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.object_maps.system");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (O_Map : in out System_Object_Map) is
   begin
      Initialize (O_Map.System_Map);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O_Map : in out System_Object_Map) is
   begin
      Deallocate (O_Map.System_Map);
   end Finalize;

   ---------
   -- Add --
   ---------

   function Add
     (O_Map : access System_Object_Map;
      Obj   : Object_Map_Entry_Access)
     return Integer is
   begin
      pragma Debug (C, O ("Add: enter"));

      if Obj.Oid /= null then
         raise Program_Error;
      end if;

      --  First try to reuse one slice in object map

      for J in First (O_Map.System_Map) .. Last (O_Map.System_Map) loop
         if Is_Null (O_Map.System_Map.Table (J)) then
            pragma Debug (C, O ("Replacing element" & Integer'Image (J)));
            O_Map.System_Map.Table (1 + J - First (O_Map.System_Map)) := Obj;

            pragma Debug (C, O ("Add: leave"));
            return J;
         end if;
      end loop;

      --  else, allocate one new element in table

      pragma Debug (C, O ("Appending element"));
      Increment_Last (O_Map.System_Map);
      O_Map.System_Map.Table (Last (O_Map.System_Map)) := Obj;

      pragma Debug (C, O ("Add: leave"));
      return Last (O_Map.System_Map);
   end Add;

   procedure Add
     (O_Map : access System_Object_Map;
      Obj   : Object_Map_Entry_Access;
      Index : Integer)
   is
      use type PolyORB.Servants.Servant_Access;

   begin
      pragma Debug (C, O ("Add: enter"));

      if False
        or else not Obj.Oid.System_Generated
        or else (not Is_Null (O_Map.System_Map.Table (Index))
                 and then O_Map.System_Map.Table (Index).Servant /= null)
      then
         --  We cannot add Obj at Index if it is not system generated,
         --  or if a servant is already set for a non null entry at Index.

         raise Program_Error;
      end if;

      if not Is_Null (O_Map.System_Map.Table (Index)) then

         --  An incomplete object map entry has been previously
         --  created to reserve Index in this active object map.
         --  We now free it.

         Free (O_Map.System_Map.Table (Index));
      end if;

      --  Add new object map entry.

      O_Map.System_Map.Table (1 + Index - First (O_Map.System_Map)) := Obj;

      pragma Debug (C, O ("Add: leave"));
   end Add;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id
     (O_Map : System_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access is
   begin
      pragma Debug (C, O ("Get_By_Id: enter"));
      pragma Debug (C, O ("Looking for: " & To_Standard_String (Item.Id)));

      if not Item.System_Generated then
         raise Program_Error;
      end if;

      pragma Debug (C, O ("System generated OID, directly return element"));
      return O_Map.System_Map.Table
        (Integer'Value (To_Standard_String (Item.Id)));
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant
     (O_Map  : System_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
   is
      use type PolyORB.Servants.Servant_Access;

   begin
      pragma Debug (C, O ("Get_By_Servant: enter"));

      for J in First (O_Map.System_Map) .. Last (O_Map.System_Map) loop
         if not Is_Null (O_Map.System_Map.Table (J)) then
            pragma Debug (C, O ("Examinating elt: "
                             & To_Standard_String
                             (O_Map.System_Map.Table (J).Oid.Id)));

            if O_Map.System_Map.Table (J).Servant = Item then
               pragma Debug (C, O ("Found !"));
               return O_Map.System_Map.Table (J);
            end if;
         end if;
      end loop;

      pragma Debug (C, O ("Not Found !"));
      return null;

   end Get_By_Servant;

   ------------------
   -- Remove_By_Id --
   ------------------

   function Remove_By_Id
     (O_Map : access System_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Old_Entry : Object_Map_Entry_Access;

   begin
      pragma Debug (C, O ("Remove_By_Id: enter"));
      pragma Debug (C, O ("Looking for: " & To_Standard_String (Item.Id)));

      if not Item.System_Generated then
         raise Program_Error;
      end if;

      pragma Debug (C, O ("System generated OID, directly remove element"));

      declare
         Index : constant Integer
           := Integer'Value (To_Standard_String (Item.Id));

      begin
         Old_Entry := O_Map.System_Map.Table (Index);
         O_Map.System_Map.Table (Index) := null;
         return Old_Entry;
      end;

   end Remove_By_Id;

end PolyORB.Object_Maps.System;
