with Lexer;     use Lexer;
with Types;     use Types;

package Parser.Errors is

   --  Parsing Code

   type Parsing_Code is
     (
      PC_AADL_Declaration,
      PC_AADL_Specification,
      PC_Annex_Specification,
      PC_Component,
      PC_Component_Category,
      PC_Component_Implementation,
      PC_Component_Type,
      PC_Defining_Identifier,
      PC_Defining_Name,
      PC_Identifiers,
      PC_None_Statement,
      PC_Package_Items,
      PC_Package_Specification,
      PC_Parameters,
      PC_Port_Group,
      PC_Properties,
      PC_Property_Association,
      PC_Property_Set,
      PC_Provides,
      PC_Requires,
      PC_System_Instance
     );

   procedure Display_Parsing_Error (Code : Parsing_Code; Msg : String);
   procedure DPE (Code : Parsing_Code;
                  Msg : String) renames Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., <Msg>

   procedure Display_Parsing_Error (Code : Parsing_Code);
   procedure DPE (Code : Parsing_Code) renames Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., unexpected token Image_Current_Token

   procedure Display_Parsing_Error (Code : Parsing_Code;
                                    Identifier : Name_Id);
   procedure DPE (Code : Parsing_Code;
                  Identifier : Name_Id) renames Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., identifier <...> expected, found
   --                                                     Image_Current_Token

   procedure Display_Parsing_Error (Code : Parsing_Code;
                                    Expected_Token : Token_Type);
   procedure DPE (Code : Parsing_Code;
                  Expected_Token : Token_Type) renames Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., token ... expected, found Image_Current_Token

end Parser.Errors;
