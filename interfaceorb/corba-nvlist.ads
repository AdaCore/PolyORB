package CORBA.NVList is

   type Object is private;

   procedure Add_Item
     (Self       : in out Object;
      Item_Name  : in     Identifier;
      Item       : in     Any;
      Item_Flags : in     Flags);

   procedure Add_Item
     (Self : in out Object;
      Item : in     NamedValue);

   -- free and free_memory Are unneeded

   procedure Get_Count
     (Self : Object;
      Count : out CORBA.Long);

private

   -- implemantation defined

end CORBA.NVList;
