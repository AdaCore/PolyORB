with Lexer;      use Lexer;
with Locations;  use Locations;
with Namet;      use Namet;
with Nodes;      use Nodes;
with Nutils;     use Nutils;
with Output;     use Output;
with Types;      use Types;

package body Parser is

   Specification : Node_Id;

   --  Component categories

--   Cat_Data        : constant Natural :=  1;
--   Cat_Subprogram  : constant Natural :=  2;
--   Cat_Thread      : constant Natural :=  3;
--   Cat_Threadgroup : constant Natural :=  4;
--   Cat_Process     : constant Natural :=  5;
--   Cat_Memory      : constant Natural :=  6;
--   Cat_Processor   : constant Natural :=  7;
--   Cat_Bus         : constant Natural :=  8;
--   Cat_Device      : constant Natural :=  9;
--   Cat_System      : constant Natural := 10;

   procedure Add_Package_Items (S : Node_Id; D : Node_Id);
   --  Add all package items of S into D (i.e. all items and properties)
   --  S and D are K_Package_Items type

   procedure Display_Error_Current_Token (S : String);
   procedure DECT (S : String) renames Display_Error_Current_Token;
   --  Display output error message S and image of the current token
   --  No processing of  meta-character !
   --  This procedure takes some informations of current token to complete
   --  the error message

   function P_AADL_Declaration return Node_Id;
   function P_AADL_Specification return Node_Id;

   function P_Component return Node_Id;
   --  Parse Component_Type, Component_Type_Extension
   --        Component_Implementation, Component_Implementation_Extension

   function P_Expected_Identifiers (Identifiers : List_Id;
                                    Delimiter : Token_Type) return Boolean;
   --  Parse { Identifier Delimiter } * Identifier
   --  These parsed identifiers must be the same as in list L
   --  These parsed identifiers will NOT be added in list L
   --  This function is useful for parsing 'end' clause for example
   --  Return TRUE if everything is OK

   function  P_Identifiers (Identifiers : List_Id; Delimiter : Token_Type)
                           return Boolean;
   --  Parse { Identifier Delimiter } * Identifier
   --  Parsed identifiers is added into list L
   --  Retrun TRUE if no error

   function P_Package_Items return Node_Id;
   function P_Package_Specification return Node_Id;
   function P_Port_Group return Node_Id;
   function P_Property_Set return Node_Id;
   function P_System_Instance return Node_Id;

   -----------------------
   -- Add_Package_Items --
   -----------------------

   procedure Add_Package_Items (S : Node_Id; D : Node_Id) is
   begin
      Append_Node_To_List (First_Node (Items (S)), Items (D));
      Append_Node_To_List (First_Node (Properties (S)), Properties (D));
   end Add_Package_Items;

   ---------------------------------
   -- Display_Error_Current_Token --
   ---------------------------------

   procedure Display_Error_Current_Token (S : String) is
   begin
      Set_Str_To_Name_Buffer (Image (Token_Location));
      Add_Str_To_Name_Buffer (": ");
      Add_Str_To_Name_Buffer (S);
      Add_Str_To_Name_Buffer (Image_Current_Token);
      Set_Standard_Error;
      Write_Line (Name_Buffer (1 .. Name_Len));
      Set_Standard_Output;
   end Display_Error_Current_Token;

   ----------------------------
   -- P_Expected_Identifiers --
   ----------------------------

   function P_Expected_Identifiers (Identifiers : List_Id;
                                    Delimiter : Token_Type) return Boolean is
      Identifier   : Node_Id;   --  Current identifier in list of identifiers
      Current_Name : Name_Id;   --  Name of current identifier
      Loc          : Location;

   begin
      if Is_Empty (Identifiers) then
         return True;
      end if;

      Identifier := First_Node (Identifiers);

      while Present (Identifier) loop
         Current_Name := Name (Identifier);
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Identifier then
            if Token_Name /= Current_Name then
               DECT ("Parsing Defining_Name, identifier <" &
                     Get_Name_String (Current_Name) &
                     "> is expected, found another identifier ");
               Restore_Lexer (Loc);
               return False;
            end if;
         else
            DECT ("Parsing Defining_Name, identifier <" &
                  Get_Name_String (Current_Name) &
                  "> is expected, found ");
            Restore_Lexer (Loc);
            return False;
         end if;

         Identifier := Next_Node (Identifier);

         if Present (Identifier) then
            --  Parse delimiter
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= Delimiter then
               DECT ("Parsing Defining_Name, identifier delimiter " &
                     Image (Delimiter) & " is expected, found ");
               Restore_Lexer (Loc);
               return False;
            end if;
         end if;
      end loop;

      return True;
   end P_Expected_Identifiers;

   -------------------
   -- P_Identifiers --
   -------------------

   function P_Identifiers (Identifiers : List_Id; Delimiter : Token_Type)
                          return Boolean is
      Identifier : Node_Id;
      Loc        : Location;

   begin
      loop
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Identifier then
            Identifier := New_Node (K_Identifier, Token_Location);
            Set_Name (Identifier, Token_Name);
            Append_Node_To_List (Identifier, Identifiers);
         else
            DECT ("Parsing identifiers, unexpected token found ");
            Restore_Lexer (Loc);
            return False;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         if Token /= Delimiter then
            Restore_Lexer (Loc);
            return True;
         end if;
      end loop;
   end P_Identifiers;

   ------------------------
   -- P_AADL_Declaration --
   ------------------------

   --  AADL_declaration ::= component_classifier | package_spec |
   --                       port_group_type | port_group_type_extension |
   --                       system_instance | property_set

   --  component_classifier          begins with 'component'
   --  package_spec                  begins with 'package'
   --  port_group_type               begins with 'port type'
   --  port_group_type_extension     begins with 'port type'
   --  system_instance               begins with an identifier
   --  property_set                  begins with 'property set'

   function P_AADL_Declaration return Node_Id is
      Declaration : Node_Id := No_Node;

   begin
      Scan_Token;

      case Token is
         when T_Component =>
            Declaration := P_Component;

         when T_Package =>
            Declaration := P_Package_Specification;

         when T_Port =>
            Scan_Token;
            if Token = T_Group then
               Declaration := P_Port_Group;
            else
               DECT ("Parsing Port_Group, reserved word 'group' is expected, "
                     & "found ");
            end if;

         when T_Property =>
            Scan_Token;
            if Token = T_Set then
               Declaration := P_Property_Set;
            else
               DECT ("Parsing Property_Set, reserved word 'set' is expected, "
                   & "found ");
            end if;

         when T_Identifier =>
            Declaration := P_System_Instance;

         when others =>
            DECT ("Parsing AADL_Declaration, unexpected token ");
      end case;

      return Declaration;
   end P_AADL_Declaration;

   --------------------------
   -- P_AADL_Specification --
   --------------------------

   --  AADL_specification ::= { AADL_declaration } +

   function P_AADL_Specification return Node_Id is
      Declarations    : List_Id;
      Declaration     : Node_Id;

   begin
      Specification := New_Node (K_AADL_Specification, Token_Location);
      Declarations  := New_List (K_AADL_Declaration_List, Token_Location);

      loop
         exit when End_Of_File;
         Declaration := P_AADL_Declaration;
         if Present (Declaration) then
            Append_Node_To_List (Declaration, Declarations);
         end if;
      end loop;

      Set_Declarations (Specification, Declarations);
      return Specification;
   end P_AADL_Specification;

   -----------------
   -- P_Component --
   -----------------

   function P_Component return Node_Id is
   begin
      return No_Node;
   end P_Component;

   ---------------------
   -- P_Package_Items --
   ---------------------

   --  package_items ::=
   --   { component_type | component_type_extension |
   --     component_implementation | component_implementation_extension |
   --     port_group_type | port_group_type_extension |
   --     annex_specification } +
   --   [ properties
   --     ( property_association { property_association } | none_statement ) ]

   function P_Package_Items return Node_Id is

      Package_Items : Node_Id;
      Items         : List_Id;
      Properties    : List_Id;

   begin
      Package_Items := New_Node (K_Package_Items, Token_Location);
      Items         := New_List (K_List_Id, Token_Location);
      Properties    := New_List (K_List_Id, Token_Location);
      Set_Items (Package_Items, Items);
      Set_Properties (Package_Items, Properties);

      return Package_Items;
   end P_Package_Items;

   -----------------------------
   -- P_Package_Specification --
   -----------------------------

   --  package_spec ::=
   --     package defining_package_name
   --      ( public package_items [ private package_items ]
   --         | private package_items )
   --     end defining_package_name;

   --  package_name ::= { package_identifier . } * package_identifier

   function P_Package_Specification return Node_Id is

      Package_Spec   : Node_Id;      --  result
      Defining_Name  : List_Id;      --  package name
      Current_Items  : Node_Id;      --  items parsed by P_Package_Items
      Public_Items   : Node_Id;      --  all public items of the package
      Private_Items  : Node_Id;      --  all private items of the package
      Loc            : Location;

   begin
      Defining_Name := New_List (K_Package_Name, Token_Location);
      Public_Items  := No_Node;
      Private_Items := No_Node;

      if not P_Identifiers (Defining_Name, T_Dot) then
         --  Defining_Package_Name is not parsed correctly, quit
         return No_Node;
      end if;

      loop
         Scan_Token;
         if Token = T_Public then
            if Present (Public_Items) then
               Current_Items := P_Package_Items;
               Add_Package_Items (Current_Items, Public_Items);
            else
               Public_Items := P_Package_Items;
            end if;

         elsif Token = T_Private then
            if Present (Private_Items) then
               Current_Items := P_Package_Items;
               Add_Package_Items (Current_Items, Private_Items);
            else
               Private_Items := P_Package_Items;
            end if;

         elsif Token = T_End then
            exit;

         else
            DECT ("Parsing Package_Specification, reserved word "
                  & "'public' or 'private' or 'end' is expected, found ");
            return No_Node;  --  OPT: ignore to next ';'
         end if;
      end loop;

      if P_Expected_Identifiers (Defining_Name, T_Dot) then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Semicolon then
            Package_Spec := New_Node (K_Package_Spec, Token_Location);
            Set_Full_Name (Package_Spec, Defining_Name);
            Set_Public_Package_Items (Package_Spec, Public_Items);
            Set_Private_Package_Items (Package_Spec, Private_Items);

            return Package_Spec;
         else
            DECT ("Parsing Package_Specification, ';' is expected, found ");
            Restore_Lexer (Loc);
            return No_Node;
         end if;
      else
         return No_Node;
      end if;
   end P_Package_Specification;

   ------------------
   -- P_Port_Group --
   ------------------

   function P_Port_Group return Node_Id is
   begin
      return No_Node;
   end P_Port_Group;

   --------------------
   -- P_Property_Set --
   --------------------

   function P_Property_Set return Node_Id is
   begin
      return No_Node;
   end P_Property_Set;

   -----------------------
   -- P_System_Instance --
   -----------------------

   function P_System_Instance return Node_Id is

      --  First identifier is in CURRENT TOKEN

   begin
      return No_Node;
   end P_System_Instance;

   -------------
   -- Process --
   -------------

   procedure Process (Root : out Node_Id) is
   begin
      Root := P_AADL_Specification;
   end Process;

end Parser;
