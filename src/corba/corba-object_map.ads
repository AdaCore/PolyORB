--  Abstract model for the Active Object Map.
--  An implementation of this map needs to be able to access an entry
--  by its index (an Integer).

with Droopi.POA_Types;

package CORBA.Object_Map is

   type Object_Map_Entry is abstract tagged
      record
         Oid     : Droopi.POA_Types.Unmarshalled_Oid_Access;
         Servant : Droopi.POA_Types.Servant_Access;
      end record;
   type Object_Map_Entry_Access is access all Object_Map_Entry'Class;

   type Object_Map is abstract tagged null record;
   type Object_Map_Access is access all Object_Map'Class;

   function Add (O_Map : access Object_Map;
                 Obj   : in     Object_Map_Entry_Access)
                return Integer is abstract;
   --  Adds a new entry in the map
   --  and returns it's index

   procedure Replace_By_Index (O_Map : access Object_Map;
                               Obj   : in     Object_Map_Entry_Access;
                               Index : in     Integer)
      is abstract;
   --  Replace an element in the map, given an index

   function Is_Servant_In (O_Map : in Object_Map;
                           Item  : in Droopi.POA_Types.Servant_Access)
                          return Boolean is abstract;
   --  Checks if a servant is already in the map
   --  (and return True if it is the case)

   function Is_Object_Id_In
     (O_Map  : in Object_Map;
      Item   : in Droopi.POA_Types.Unmarshalled_Oid_Access)
     return Boolean is abstract;
   --  Checks if an object_id is already used in the map
   --  (and return True if it is the case)

   function Get_By_Id (O_Map : in Object_Map;
                       Item  : in Droopi.POA_Types.Unmarshalled_Oid_Access)
                      return Object_Map_Entry_Access is abstract;
   --  Given an Object_Id, looks for the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant (O_Map  : in Object_Map;
                            Item   : in Droopi.POA_Types.Servant_Access)
                           return Object_Map_Entry_Access is abstract;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Get_By_Index (O_Map : in Object_Map;
                          Index : in Integer)
                         return Object_Map_Entry_Access is abstract;
   --  Given an index, returns the corrsponding map entry
   --  If Index is out of bounds, returns null.

   function Remove (O_Map : access Object_Map;
                    Item  : in     Droopi.POA_Types.Unmarshalled_Oid_Access)
                   return Object_Map_Entry_Access is abstract;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

   function Remove_By_Index (O_Map : access Object_Map;
                             Index : in     Integer)
                            return Object_Map_Entry_Access is abstract;
   --  Given an index, removes an entry from the map
   --  and returns it. A null value means that the index
   --  points to an empty value.

end CORBA.Object_Map;
