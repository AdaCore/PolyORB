--  Implementation of an Activa Object Map optimized for User defined
--  Object Identifier.

--  Note: this package depends on Unmarshalled_Oid constrution for
--  USER_ID POA policy as defined in the package
--  PolyORB.POA_Policies.Id_Assignment_Policy.User

with PolyORB.Utils.HFunctions.Mul;
with PolyORB.Utils.HTables.Perfect;

package PolyORB.Object_Maps.User is

   type User_Object_Map is new Object_Map with private;

   procedure Add
     (O_Map : access User_Object_Map;
      Obj   : in     Object_Map_Entry_Access);
   --  Adds a new entry in the map.

   function Get_By_Id
     (O_Map : in User_Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, look up the corresponding map entry.
   --  If not found, returns null.

   function Get_By_Servant
     (O_Map  : in User_Object_Map;
      Item   : in PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  If not found, returns null.

   function Remove_By_Id
     (O_Map : access User_Object_Map;
      Item  : in     PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access;
   --  Given an Object_Id, removes an entry from the map
   --  and returns it . A null value means
   --  that the object_id wasn't in the map.

private

   package Map_Entry_HTables is new PolyORB.Utils.HTables.Perfect
     (Object_Map_Entry_Access,
      PolyORB.Utils.HFunctions.Mul.Hash_Mul_Parameters,
      PolyORB.Utils.HFunctions.Mul.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Mul.Hash,
      PolyORB.Utils.HFunctions.Mul.Next_Hash_Parameters);

   subtype Map_EntryTable is Map_Entry_HTables.Table_Instance;

   type User_Object_Map is new Object_Map with record
      User_Map    : Map_EntryTable;
      Initialized : Boolean := False;
   end record;

end PolyORB.Object_Maps.User;
