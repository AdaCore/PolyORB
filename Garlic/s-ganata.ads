package System.Garlic.Name_Table is

   subtype Name_Id is Natural;
   Null_Name : constant Name_Id := 0;

   function  Get (S : String)  return Name_Id;
   function  Get (N : Name_Id) return String;

   function  Get_Info (N : Name_Id) return Integer;
   procedure Set_Info (N : Name_Id; I : Integer);

end System.Garlic.Name_Table;
