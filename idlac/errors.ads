package Errors is

   --  defines two levels of error
   type Error_Kind is (Error, Warning);

   --  displays a new error
   procedure Display_Error (Message : in String;
                            Line : in Natural;
                            Column : in Positive;
                            Kind : in Error_Kind);

   --  was there any errors ?
   function Is_Error return Boolean;

   --  was there any warning ?
   function Is_Warning return Boolean;

   --  returns the number of warnings
   function Warning_Number return Natural;

   --  returns the number of errors
   function Error_Number return Natural;

end Errors;
