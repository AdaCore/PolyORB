package CORBA.Object_Map is
   --  ??? This package could be an instantation of a generic package
   --  Pb with procedure slot_by_object_id for example: need to compare
   --  two object ids. Can be done by providing access to comparison functions.

   type Object_Map_Entry is null record;
   type Slot_Index is new CORBA.Unsigned_Long;
   type Object_Map is array (Slot_Index range <>)
     of Object_Map_Entry;

end CORBA.Object_Map;
