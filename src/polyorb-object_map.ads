--  A generic package to manage an object map (a map of associations between
--  a servant an an object_id. The entries are stored in an unbounded sequence
--  and when an entry is removed, it is actually replaced by a null value.
--  Four functions are to be provided:
--  Is_Servant_Equal   : compares a servant with an entry;
--                       returns true if equality
--  Is_Object_Id_Equal : compares an object_id with an entry;
--                       returns true if equality
--  Is_Null            : returns true if the entry is null
--  Null_Entry         : returns a null entry
--
--  Note: the remove_by_index and get_by_index may raise an INDEX_ERROR
--  exception.

with Sequences.Unbounded;

generic

   type Map_Entry is private;
   type Servant is private;
   type Object_Id is private;
   with function Is_Servant_Equal (Item : in Map_Entry;
                                   To   : in Servant)
                                  return Boolean is <>;

   with function Is_Object_Id_Equal (Item : in Map_Entry;
                                     To   : in Object_Id)
                                    return Boolean is <>;

   with function Is_Null (Item : in Map_Entry) return Boolean is <>;
   with function Null_Entry return Map_Entry is <>;

package PolyORB.Object_Map is

   type Object_Map is private;
   type Object_Map_Access is access all Object_Map;

   Index_Out_Of_Bounds : exception;

   function Add (O_Map : access Object_Map;
                 Obj   : in     Map_Entry)
                return Integer;
   --  Adds a new entry in the map
   --  and returns it's index

   procedure Replace_By_Index (O_Map : access Object_Map;
                               Obj   : in     Map_Entry;
                               Index : in     Integer);
   --  Replace element with index Index by Obj

   function Is_Servant_In (O_Map  : in Object_Map;
                           Item   : in Servant)
                          return Boolean;
   --  Checks if a servant is already in the map
   --  (and return True if it is the case)

   function Is_Object_Id_In (O_Map  : in Object_Map;
                             Item   : in Object_Id)
                            return Boolean;
   --  Checks if an object_id is already used in the map
   --  (and return True if it is the case)

   function Get_By_Id (O_Map  : in Object_Map;
                       Item   : in Object_Id)
                      return Map_Entry;
   --  Given an Object_Id, looks for the corresponding map entry
   --  Returns Null_Entry if not found

   function Get_By_Servant (O_Map  : in Object_Map;
                            Item   : in Servant)
                           return Map_Entry;
   --  Given a servant, looks for the corresponding map entry
   --  Doesn't check that the servant is only once in the map
   --  Returns Null_Entry if not found;

   function Get_By_Index (O_Map : in Object_Map;
                          Index : in Integer)
                         return Map_Entry;
   --  Given an index, returns the corrsponding map entry
   --  Raises Index_Out_Of_Bounds if Index
   --  is out of the map bounds.

   function Remove (O_Map : access Object_Map;
                    Item  : in     Object_Id)
                   return Map_Entry;
   --  Given an object_Id, removes an entry from the map
   --  and returns it (for the function). A null value means
   --  that the object_id wasn't in the map.

   function Remove_By_Index (O_Map : access Object_Map;
                             Index : in     Integer)
                            return Map_Entry;
   --  Given an index, removes an entry from the map
   --  and returns it. A null value means that the index
   --  points to an empty value.
   --  Raises Index_Out_Of_Bounds if Index is not in the map bounds

private
   package Object_Map_Entry_Seqs is new Sequences.Unbounded
     (Map_Entry);
   subtype Object_Map_Entry_Seq is Object_Map_Entry_Seqs.Sequence;

   type Object_Map is
      record
         Map : aliased Object_Map_Entry_Seq;
      end record;

end PolyORB.Object_Map;

