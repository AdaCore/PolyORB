------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . O B J E C T _ M A P S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Abstract model for the Active Object Map.
--  An implementation of this map needs to be able to access an entry
--  by its index (an Integer).

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Sequences.Unbounded;
with PolyORB.Objects;
with PolyORB.POA_Types;

package PolyORB.Object_Maps is

   type Object_Map_Entry is limited record
      Oid     : PolyORB.POA_Types.Unmarshalled_Oid_Access;
      Servant : PolyORB.Objects.Servant_Access;
   end record;

   type Object_Map_Entry_Access is access all Object_Map_Entry;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object_Map_Entry, Object_Map_Entry_Access);

   type Object_Map is limited private;
   type Object_Map_Access is access all Object_Map;

   function Add
     (O_Map : access Object_Map;
      Obj   : in     Object_Map_Entry_Access)
     return Integer;
   --  Adds a new entry in the map, returning its index.

   procedure Replace_By_Index
     (O_Map : access Object_Map;
      Obj   : in     Object_Map_Entry_Access;
      Index : in     Integer);
   --  Replace an element in the map, given its index.

   function Is_Servant_In
     (O_Map : in Object_Map;
      Item  : in PolyORB.Objects.Servant_Access)
     return Boolean;
   --  Checks if a servant is already in the map
   --  (and return True if it is the case)

   function Is_Object_Id_In
     (O_Map  : in Object_Map;
      Item   : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Boolean;
   --  Checks if an object_id is already used in the map
   --  (and return True if it is the case)

   function Get_By_Id
     (O_Map : in Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : in Object_Map;
      Item   : in PolyORB.Objects.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Get_By_Index
     (O_Map : in Object_Map;
      Index : in Integer)
     return Object_Map_Entry_Access;
   --  Given an index, returns the corrsponding map entry
   --  If Index is out of bounds, returns null.

   function Remove_By_Id
     (O_Map : access Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

   function Remove_By_Index
     (O_Map : access Object_Map;
      Index : in     Integer)
     return Object_Map_Entry_Access;
   --  Given an index, removes an entry from the map
   --  and returns it. A null value means that the index
   --  points to an empty value.
   --  The caller is responsible for freeing the Oid
   --  and the object map entry.

private

   package Map_Entry_Seqs is new PolyORB.Sequences.Unbounded
     (Object_Map_Entry_Access);

   type Object_Map is limited record
      Map : Map_Entry_Seqs.Sequence;
   end record;

end PolyORB.Object_Maps;
