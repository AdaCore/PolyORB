------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        C O R B A . O B J E C T _ M A P . S E Q U E N C E _ M A P         --
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

--  An implementation of the Active Object Map using the sequences from PolyORB
--  (PolyORB.Unbounded_Sequences). It uses the generic package
--  PolyORB.Object_Map to do so.

--  $Id$

with PolyORB.Object_Map;
pragma Elaborate_All (PolyORB.Object_Map);
with PolyORB.POA_Types;

package CORBA.Object_Map.Sequence_Map is

   type Seq_Object_Map_Entry is new CORBA.Object_Map.Object_Map_Entry
     with null record;
   type Seq_Object_Map_Entry_Access is access all Seq_Object_Map_Entry;

   --  Functions required by the generic package PolyORB.Object_Map

   function Is_Servant_Equal (Item : in Object_Map_Entry_Access;
                              To   : in PolyORB.POA_Types.Servant_Access)
                             return Boolean;

   function Is_Object_Id_Equal
     (Item : in Object_Map_Entry_Access;
      To   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
     return Boolean;

   function Is_Null (Item : in Object_Map_Entry_Access)
                    return Boolean;

   function Null_Entry return Object_Map_Entry_Access;

   package Active_Object_Map is
      new PolyORB.Object_Map (Object_Map_Entry_Access,
                             PolyORB.POA_Types.Servant_Access,
                             PolyORB.POA_Types.Unmarshalled_Oid_Access);
   --  Instanciation of the generic package PolyORB.Object_Map

   type Seq_Object_Map is new CORBA.Object_Map.Object_Map with private;
   type Seq_Object_Map_Access is access all Seq_Object_Map;
   --  The composite type for this implementation of the Active Object Map

   function New_Map return Seq_Object_Map_Access;
   --  Creates a new Seq_Object_Map_Access

   procedure Free_Map (S_Map : in out Seq_Object_Map_Access);
   --  Frees a Seq_Object_Map object

   function Add (O_Map : access Seq_Object_Map;
                 Obj   : in     Object_Map_Entry_Access)
                return Integer;
   --  Adds a new entry in the map
   --  and returns it's index

   procedure Replace_By_Index (O_Map : access Seq_Object_Map;
                               Obj   : in     Object_Map_Entry_Access;
                               Index : in     Integer);
   --  Replace an element in the map, given an index

   function Is_Servant_In (O_Map  : in Seq_Object_Map;
                           Item   : in PolyORB.POA_Types.Servant_Access)
                          return Boolean;
   --  Checks if a servant is already in the map
   --  (and return True if it is the case)

   function Is_Object_Id_In
     (O_Map  : in Seq_Object_Map;
      Item   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
     return Boolean;
   --  Checks if an object_id is already used in the map
   --  (and return True if it is the case)

   function Get_By_Id (O_Map  : in Seq_Object_Map;
                       Item   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
                      return Object_Map_Entry_Access;
   --  Given an Object_Id, looks for the corresponding map entry

   function Get_By_Servant (O_Map  : in Seq_Object_Map;
                            Item   : in PolyORB.POA_Types.Servant_Access)
                           return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map

   function Get_By_Index (O_Map : in Seq_Object_Map;
                          Index : in Integer)
                         return Object_Map_Entry_Access;
   --  Given an index, returns the corrsponding map entry

   function Remove (O_Map : access Seq_Object_Map;
                    Item  : in     PolyORB.POA_Types.Unmarshalled_Oid_Access)
                   return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

   function Remove_By_Index (O_Map : access Seq_Object_Map;
                             Index : in     Integer)
                            return Object_Map_Entry_Access;
   --  Given an index, removes an entry from the map
   --  and returns it. A null value means that the index
   --  points to an empty value.

   procedure Free is new
     Ada.Unchecked_Deallocation (Seq_Object_Map_Entry,
                                 Seq_Object_Map_Entry_Access);
   --  Frees an entry of the map

private

   type Seq_Object_Map is new CORBA.Object_Map.Object_Map with
      record
         Map : Active_Object_Map.Object_Map_Access;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Active_Object_Map.Object_Map,
      Active_Object_Map.Object_Map_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Seq_Object_Map,
      Seq_Object_Map_Access);

end CORBA.Object_Map.Sequence_Map;
