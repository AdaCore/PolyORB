with ErrorMsg;   use ErrorMsg;
with Errors;     use Errors;
with Lexer;      use Lexer;
with Locations;  use Locations;
with Nodes;      use Nodes;
with Nutils;     use Nutils;
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

   function P_AADL_Declaration return Node_Id;
   function P_AADL_Specification return Node_Id;

   function P_Annex_Specification return Node_Id;

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
   --  Return TRUE if no error

   function P_None_Statement return Boolean;
   --  Parse " none ; ", this function does NOT return a Node_Id because
   --  no useful data will be retrieved
   --  Return TRUE if the reserved word 'none' is followed by a ';'

   function P_Package_Items return Node_Id;
   function P_Package_Specification return Node_Id;
   function P_Port_Group return Node_Id;
   function P_Property_Association return Node_Id;
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
      Loc         : Location;

   begin
      Save_Lexer (Loc);
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
               DPE (PC_Port_Group, T_Group);
               Restore_Lexer (Loc);
               return No_Node;  --  OPT: ignore ---> ';'
            end if;

         when T_Property =>
            Scan_Token;
            if Token = T_Set then
               Declaration := P_Property_Set;
            else
               DPE (PC_Property_Set, T_Set);
               Restore_Lexer (Loc);
               return No_Node;  --  OPT: ignore ---> ';'
            end if;

         when T_Identifier =>
            Declaration := P_System_Instance;

         when others =>
            DPE (PC_AADL_Declaration);
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
      Declarations := New_List (K_AADL_Declaration_List, Token_Location);

      loop
         exit when End_Of_File;
         Declaration := P_AADL_Declaration;
         if Present (Declaration) then
            Append_Node_To_List (Declaration, Declarations);
         end if;
      end loop;

      Specification := New_Node (K_AADL_Specification, Token_Location);
      Set_Declarations (Specification, Declarations);

      return Specification;
   end P_AADL_Specification;

   ---------------------------
   -- P_Annex_Specification --
   ---------------------------

   function P_Annex_Specification return Node_Id is
   begin
      return No_Node;
   end P_Annex_Specification;

   -----------------
   -- P_Component --
   -----------------

   function P_Component return Node_Id is
   begin
      return No_Node;
   end P_Component;

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
               DPE (PC_Defining_Name, Current_Name);
               Restore_Lexer (Loc);
               return False;
            end if;
         else
            DPE (PC_Defining_Name, Current_Name);
            Restore_Lexer (Loc);
            return False;
         end if;

         Identifier := Next_Node (Identifier);

         if Present (Identifier) then
            --  Parse delimiter
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= Delimiter then
               DPE (PC_Defining_Name, Delimiter);
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
            DPE (PC_Identifiers);
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

   ----------------------
   -- P_None_Statement --
   ----------------------

   --  none_statement ::= none ;

   function P_None_Statement return Boolean is
      Loc : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Semicolon then
         return True;
      else
         DPE (PC_None_Statement, T_Semicolon);
         Restore_Lexer (Loc);
         return False;
      end if;
   end P_None_Statement;

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
      Item          : Node_Id;
      Property      : Node_Id;
      Loc           : Location;

   begin
      --  Parse items

      Items := New_List (K_List_Id, Token_Location);
      loop
         Save_Lexer (Loc);
         Scan_Token;
         case Token is
            when T_Component =>
               Item := P_Component;

            when T_Group =>
               Scan_Token;
               if Token = T_Group then
                  Item := P_Port_Group;
               else
                  DPE (PC_Port_Group, T_Group);
                  Restore_Lexer (Loc);
                  return No_Node;   --  OPT: ignore ----> ";"
               end if;

            when T_Annex =>
               Item := P_Annex_Specification;

            when others =>
               Restore_Lexer (Loc);
               exit;
         end case;

         if Present (Item) then
            Append_Node_To_List (Item, Items);
         else
            --  Error when parsing package item, quit
            return No_Node;  --  OPT: ignore ----> ";"
         end if;
      end loop;

      --  Parse properties

      Properties := New_List (K_List_Id, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Properties then
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_None then
            if not P_None_Statement then
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
            loop
               Save_Lexer (Loc);
               Property := P_Property_Association;
               if Present (Property) then
                  Append_Node_To_List (Property, Properties);
               else
                  --  Error when parsing Property_Association, restore lexer
                  Restore_Lexer (Loc);
                  if Is_Empty (Properties) then
                     --  Warning: Properties list must contain at least one
                     --           element
                     Error_Loc (1) := Token_Location;
                     DE ("properties list is empty", K_Warning);
                  end if;
                  exit;
               end if;
            end loop;
         end if;

      else
         --  No property declared
         Restore_Lexer (Loc);
      end if;

      --  Return result

      Package_Items := New_Node (K_Package_Items, Token_Location);
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
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Public then
            Current_Items := P_Package_Items;
            if Present (Current_Items) then
               if Present (Public_Items) then
                  Add_Package_Items (Current_Items, Public_Items);
               else
                  Public_Items := Current_Items;
               end if;
            else
               --  Error when parsing package items, quit
               return No_Node;  --  OPT: ignore ----> ';'
            end if;

         elsif Token = T_Private then
            Current_Items := P_Package_Items;
            if Present (Current_Items) then
               if Present (Private_Items) then
                  Add_Package_Items (Current_Items, Private_Items);
               else
                  Private_Items := Current_Items;
               end if;
            else
               --  Error when parsing package items, quit
               return No_Node;  --  OPT: ignore ----> ';'
            end if;

         elsif Token = T_End then
            exit;

         else
            DPE (PC_Package_Specification);
            Restore_Lexer (Loc);
            return No_Node;  --  OPT: ignore ----> ';'
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
            DPE (PC_Package_Specification, T_Semicolon);
            Restore_Lexer (Loc);
            return No_Node;
         end if;
      else
         --  Error when parsing Defining_Name, quit
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

   ----------------------------
   -- P_Property_Association --
   ----------------------------

   function P_Property_Association return Node_Id is
   begin
      return No_Node;
   end P_Property_Association;

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
