package body Ada_Be.Identifiers is


   function Get_Ada_Full_Name (Node : in N_Named'Class)
                               return String is
   begin
      return Get_Name (Node);
   end Get_Ada_Full_Name;


   function Get_Ada_Name (Node : in N_Named'Class)
                          return String is
   begin
      return Get_Name (Node);
   end Get_Ada_Name;

end Ada_Be.Identifiers;
