package body Ada_Be.Identifiers is


   function Ada_Full_Name
     (Node : N_Root_Acc)
     return String is
   begin
      return Get_Name (N_Named_Acc (Node).all);
   end Ada_Full_Name;

   function Ada_Name
     (Node : N_Root_Acc)
     return String
   is
      Full_Name : constant String
        := Ada_Full_Name (Node);
      Last_Dot : Integer := Full_Name'First - 1;
   begin
      for I in Full_Name'Range loop
         if Full_Name (I) = '.' then
            Last_Dot := Integer (I);
         end if;
      end loop;
      return Full_Name (Last_Dot + 1 .. Full_Name'Last);
   end Ada_Name;

end Ada_Be.Identifiers;
