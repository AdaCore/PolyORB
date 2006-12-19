------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . O B J E C T _ M A P S . U S E R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  Implementation of an Active Object Map optimized for User defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid constrution for
--  USER_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.User

with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;

package PolyORB.Object_Maps.User is

   type User_Object_Map is new Object_Map with private;

   procedure Initialize (O_Map : in out User_Object_Map);

   procedure Finalize (O_Map : in out User_Object_Map);

   procedure Add
     (O_Map : access User_Object_Map;
      Obj   : Object_Map_Entry_Access);
   --  Adds a new entry in the map.

   function Get_By_Id
     (O_Map : User_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : User_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Remove_By_Id
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
