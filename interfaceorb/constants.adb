package body Constants is

   ------------------------
   -- To_Standard_String --
   ------------------------

   function To_Standard_String
     (S : in Constants.Exception_Id)
      return Standard.String
   is
   begin
      return Standard.String (S);
   end To_Standard_String;

   ---------------------
   -- To_Exception_Id --
   ---------------------

   function To_Exception_Id
     (S : in Standard.String)
      return Constants.Exception_Id
   is
   begin
      return Constants.Exception_Id (S);
   end To_Exception_Id;

end Constants;
