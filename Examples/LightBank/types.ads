package Types is

   pragma Pure;

   N_Customer_IDs : constant := 20;

   type Customer_ID      is range 0 .. N_Customer_IDs;
   subtype Customer_Type is String;
   subtype Password_Type is String;

   Wrong_Password  : exception;
   Wrong_Customer  : exception;
   Wrong_Donator   : exception;
   No_More_IDs     : exception;

end Types;
