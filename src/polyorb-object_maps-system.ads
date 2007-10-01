------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O B J E C T _ M A P S . S Y S T E M            --
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

--  Implementation of an Active Object Map optimized for System defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid construction for
--  SYSTEM_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.System

with PolyORB.Utils.Dynamic_Tables;

package PolyORB.Object_Maps.System is

   type System_Object_Map is new Object_Map with private;

   procedure Initialize (O_Map : in out System_Object_Map);
   --  Initialize object map O_Map private structures

   procedure Finalize (O_Map : in out System_Object_Map);
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

   function Get_By_Id
     (O_Map : System_Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : System_Object_Map;
      Item   : PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Remove_By_Id
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
