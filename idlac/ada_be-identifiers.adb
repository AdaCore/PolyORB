package body Ada_Be.Identifiers is


   function Get_Full_Ada_Name (Node : in Types.N_Named'Class)
                               return String is
   begin
      return Types.Get_Name (Node);
   end Get_Full_Ada_Name;


   function Get_Ada_Name (Node : in Types.N_Named'Class)
                          return String is
   begin
      return Types.Get_Name (Node);
   end Get_Ada_Name;

end Ada_Be.Identifiers;
