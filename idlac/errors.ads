with Ada.Unchecked_Deallocation;

package Errors is

   type File_Name_Ptr is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, File_Name_Ptr);

   --  defines a place in one of the parsed files
   type Location is record
      Filename : File_Name_Ptr;
      Line : Natural;
      Col : Natural;
   end record;

   --  this exception is for internal use,
   --  it is raised when idlac reaches an inconsistent state
   Internal_Error : exception;

   --  this exception is raised when the compiler cannot parse
   --  its entry
   Fatal_Error : exception;

   --  defines three levels of error
   type Error_Kind is (Fatal, Error, Warning);

   --  deals with a Lexer error, raise it if level is fatal
   procedure Lexer_Error (Message : in String;
                          Level : in Error_Kind);

   --  deals with a Parser error, raise it if level is fatal
   procedure Parser_Error (Message : in String;
                          Level : in Error_Kind);

   --  was there any errors ?
   function Is_Error return Boolean;

   --  was there any warning ?
   function Is_Warning return Boolean;

   --  returns the number of warnings
   function Warning_Number return Natural;

   --  returns the number of errors
   function Error_Number return Natural;

end Errors;
