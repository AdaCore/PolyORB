------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . O B J E C T _ M A P S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Abstract model for the POA Active Object Map.

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.Object_Maps is

   ----------------------
   -- Object_Map_Entry --
   ----------------------

   type Object_Map_Entry is limited record
      Oid     : PolyORB.POA_Types.Unmarshalled_Oid_Access;
      Servant : PolyORB.Servants.Servant_Access;
   end record;

   type Object_Map_Entry_Access is access all Object_Map_Entry;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Object_Map_Entry, Name => Object_Map_Entry_Access);

   ----------------
   -- Object_Map --
   ----------------

   type Object_Map is abstract tagged limited private;

   type Object_Map_Access is access all Object_Map'Class;

   procedure Initialize (O_Map : in out Object_Map)
      is abstract;
   --  Initialize object map O_Map private structures

   procedure Finalize (O_Map : in out Object_Map)
      is abstract;
   --  Finalize object map O_Map private structures

   function Is_Servant_In
     (O_Map : Object_Map;
      Item  : PolyORB.Servants.Servant_Access)
     return Boolean;
   --  Checks if a servant is already in the map
   --  (and return True if it is the case)

   function Is_Object_Id_In
     (O_Map  : Object_Map;
      Item   : PolyORB.POA_Types.Unmarshalled_Oid)
     return Boolean;
   --  Checks if an object_id is already used in the map
   --  (and return True if it is the case)

   function Get_By_Id
     (O_Map : Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
      is abstract;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
      is abstract;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Remove_By_Id
     (O_Map : access Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
     is abstract;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

private

   type Object_Map is abstract tagged limited null record;

   function Is_Null
     (Item : Object_Map_Entry_Access)
     return Boolean;

end PolyORB.Object_Maps;
