--  Implementation of an Activa Object Map optimized for System defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid constrution for
--  SYSTEM_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.System

with PolyORB.Sequences.Unbounded;

package PolyORB.Object_Maps.System is

   type System_Object_Map is new Object_Map with private;

   function Add
     (O_Map : access System_Object_Map;
      Obj   : in     Object_Map_Entry_Access)
     return Integer;
   --  Adds a new entry in the map, returning its index.

   procedure Add
     (O_Map : access System_Object_Map;
      Obj   : in     Object_Map_Entry_Access;
      Index : in     Integer);
   --  Adds a new entry in the map at the given index.

   function Get_By_Id
     (O_Map : in System_Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : in System_Object_Map;
      Item   : in PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Remove_By_Id
     (O_Map : access System_Object_Map;
      Item  : in     PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

private

   package Map_Entry_Seqs is new PolyORB.Sequences.Unbounded
     (Object_Map_Entry_Access);

   type System_Object_Map is new Object_Map with record
      System_Map       : Map_Entry_Seqs.Sequence;
   end record;

end PolyORB.Object_Maps.System;
