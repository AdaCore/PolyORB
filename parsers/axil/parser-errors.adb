with Lexer;        use Lexer;
with Locations;    use Locations;
with Namet;        use Namet;
with Output;       use Output;
with Types;        use Types;

package body Parser.Errors is

   Parsing_Str : constant array (Parsing_Code) of String_Ptr :=
     (
      PC_AADL_Declaration         => new String'("AADL_Delcaration"),
      PC_AADL_Specification       => new String'("AADL_Specification"),
      PC_Annex_Specification      => new String'("Annex_Specification"),
      PC_Component                => new String'("Component"),
      PC_Component_Category       => new String'("Component_Category"),
      PC_Component_Implementation => new String'("Component_Implementation"),
      PC_Component_Type           => new String'("Component_Type"),
      PC_Defining_Name            => new String'("Defining_Name"),
      PC_Identifiers              => new String'("Identifiers"),
      PC_None_Statement           => new String'("None_Statement"),
      PC_Package_Items            => new String'("Package_Items"),
      PC_Package_Specification    => new String'("Package_Specification"),
      PC_Port_Group               => new String'("Port_Group"),
      PC_Property_Association     => new String'("Property_Association"),
      PC_Property_Set             => new String'("Property_Set"),
      PC_System_Instance          => new String'("System_Instance"),
      others                      => new String'("[UNKNOWN]")
     );

   procedure Display_Parsing_Code (Code : Parsing_Code);
   pragma Inline (Display_Parsing_Code);
   --  Display corresponding string of given parsing code

   --------------------------
   -- Display_Parsing_Code --
   --------------------------

   procedure Display_Parsing_Code (Code : Parsing_Code) is
      Msg_Ptr : constant String_Ptr := Parsing_Str (Code);
   begin
      Write_Str (Image (Token_Location));
      Write_Str (": parsing ");
      Write_Str (Msg_Ptr.all);
      Write_Str (", ");
   end Display_Parsing_Code;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error (Msg : String) is
   begin
      Set_Standard_Error;
      Write_Str (Image (Token_Location));
      Write_Str (": ");

      Write_Str (Msg);
      Write_Char (' ');
      Write_Line (Image_Current_Token);

      Set_Standard_Output;
   end Display_Parsing_Error;

   procedure Display_Parsing_Error (Code : Parsing_Code) is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("unexpected token ");
      Write_Line (Image_Current_Token);

      Set_Standard_Output;
   end Display_Parsing_Error;

   procedure Display_Parsing_Error (Code : Parsing_Code;
                                    Identifier : Name_Id) is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("identifier <");
      Write_Str (Get_Name_String (Identifier));
      Write_Str ("> is expected, found ");
      Write_Line (Image_Current_Token);

      Set_Standard_Output;
   end Display_Parsing_Error;

   procedure Display_Parsing_Error (Code : Parsing_Code;
                                    Expected_Token : Token_Type) is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("token ");
      Write_Str (Image (Expected_Token));
      Write_Str ("is expected, found ");
      Write_Line (Image_Current_Token);

      Set_Standard_Output;
   end Display_Parsing_Error;

end Parser.Errors;
