with Types; use Types;

package Backend is

   procedure Set_Current_Language (Language : String);
   --  Reset the current language.

   function Current_Language return String;
   --  Return language previously set. Null string when uninitialized.

   type Configure_Procedure is access procedure;

   procedure Configure;
   --  Configure backend with specific flags. To do so scan flags
   --  using Getopt from GNAT.Command_Line.

   type Generate_Procedure is access procedure (Root : Node_Id);

   procedure Generate (Root : Node_Id);
   --  Generate code for the current language.

   procedure Register
     (Generate  : Generate_Procedure;
      Configure : Configure_Procedure;
      Language  : String;
      Comments  : String);
   --  Register a new language with its code generation procedure, its
   --  name and a comment associated to it (for usage output).
   --  The current language is set to this last language.

   function Is_Valid_Language (L : String) return Boolean;
   --  Return True when there is a backend corresponding to L

   procedure Write_Languages (L, C : Natural);
   --  For each language backend avaible write at column L the name
   --  and at column C the comments associated to a language.

end Backend;

