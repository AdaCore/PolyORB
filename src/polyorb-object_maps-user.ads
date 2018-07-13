------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . O B J E C T _ M A P S . U S E R              --
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

--  Implementation of an Active Object Map optimized for User defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid constrution for
--  USER_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.User

with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;

package PolyORB.Object_Maps.User is

   type User_Object_Map is new Object_Map with private;

   overriding procedure Initialize (O_Map : in out User_Object_Map);

   overriding procedure Finalize (O_Map : in out User_Object_Map);

   procedure Add
     (O_Map : access User_Object_Map;
      Obj   : Object_Map_Entry_Access);
   --  Adds a new entry in the map.

   overriding function Get_By_Id
     (O_Map : User_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   overriding function Get_By_Servant
     (O_Map  : User_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   overriding function Remove_By_Id
     (O_Map : access User_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

private

   package Map_Entry_HTables is new PolyORB.Utils.HTables.Perfect
     (Object_Map_Entry_Access,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);

   subtype Map_EntryTable is Map_Entry_HTables.Table_Instance;

   type User_Object_Map is new Object_Map with record
      User_Map    : Map_EntryTable;
   end record;

end PolyORB.Object_Maps.User;
