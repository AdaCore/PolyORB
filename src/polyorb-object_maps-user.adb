------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . O B J E C T _ M A P S . U S E R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.Object_Maps.User is

   use Map_Entry_HTables;

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.object_maps.user");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (O_Map : in out User_Object_Map) is
   begin
      Initialize (O_Map.User_Map);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (O_Map : in out User_Object_Map) is
   begin
      Finalize (O_Map.User_Map);
   end Finalize;

   ---------
   -- Add --
   ---------

   procedure Add
     (O_Map : access User_Object_Map;
      Obj   : Object_Map_Entry_Access) is
   begin
      Insert (O_Map.User_Map, To_Standard_String (Obj.Oid.Id), Obj);
   end Add;

   ---------------
   -- Get_By_Id --
   ---------------

   overriding function Get_By_Id
     (O_Map : User_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access is
   begin
      pragma Debug (C, O ("User generated OID, look up in table"));

      return Lookup (O_Map.User_Map, To_Standard_String (Item.Id), null);
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   overriding function Get_By_Servant
     (O_Map  : User_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
   is
      use type PolyORB.Servants.Servant_Access;

      It : Iterator := First (O_Map.User_Map);
   begin
      while not Last (It) loop
         if not Is_Null (Value (It)) then
            pragma Debug (C, O ("Examinating elt: "
                             & To_Standard_String (Value (It).Oid.Id)));

            if Value (It).Servant = Item then
               pragma Debug (C, O ("Found !"));
               return Value (It);
            end if;
         end if;

         Next (It);
      end loop;

      pragma Debug (C, O ("Not Found !"));
      return null;
   end Get_By_Servant;

   ------------------
   -- Remove_By_Id --
   ------------------

   overriding function Remove_By_Id
     (O_Map : access User_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Old_Entry : Object_Map_Entry_Access;

      Name : constant String := To_Standard_String (Item.Id);
   begin
      Old_Entry := Lookup (O_Map.User_Map, Name, null);
      Delete (O_Map.User_Map, Name);
      return Old_Entry;
   end Remove_By_Id;

end PolyORB.Object_Maps.User;
