------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O B J E C T _ M A P S . S Y S T E M            --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

--  Implementation of an Active Object Map optimized for System defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid construction for
--  SYSTEM_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.System

with PolyORB.Utils.Dynamic_Tables;

package PolyORB.Object_Maps.System is

   type System_Object_Map is new Object_Map with private;

   overriding procedure Initialize (O_Map : in out System_Object_Map);
   --  Initialize object map O_Map private structures

   overriding procedure Finalize (O_Map : in out System_Object_Map);
   --  Finalize object map O_Map private structures

   function Add
     (O_Map : access System_Object_Map;
      Obj   : Object_Map_Entry_Access)
     return Integer;
   --  Adds a new entry in the map, returning its index.

   procedure Add
     (O_Map : access System_Object_Map;
      Obj   : Object_Map_Entry_Access;
      Index : Integer);
   --  Adds a new entry in the map at the given index.

   overriding function Get_By_Id
     (O_Map : System_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   overriding function Get_By_Servant
     (O_Map  : System_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   overriding function Remove_By_Id
     (O_Map : access System_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

private

   package Map_Entry_Tables is new PolyORB.Utils.Dynamic_Tables
     (Object_Map_Entry_Access, Natural, 1, 10, 1);

   type System_Object_Map is new Object_Map with record
      System_Map  : Map_Entry_Tables.Instance;
   end record;

end PolyORB.Object_Maps.System;
