with Errors;     use Errors;
with Lexer;      use Lexer;
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

   function P_AADL_Specification return Node_Id;
   function P_AADL_Declaration return Node_Id;

   function P_Component return Node_Id;
   --  Parse Component_Type, Component_Type_Extension
   --        Component_Implementation, Component_Implementation_Extension

   function P_Package_Specification return Node_Id;
   function P_Port_Group return Node_Id;
   function P_Property_Set return Node_Id;
   function P_System_Instance return Node_Id;

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
               Error_Loc (1) := Token_Location;
               DE ("Parsing Port_Group, reserved word 'group' is expected, "
                   & "found " & Image_Current_Token);
            end if;

         when T_Property =>
            Scan_Token;
            if Token = T_Set then
               Declaration := P_Property_Set;
            else
               Error_Loc (1) := Token_Location;
               DE ("Parsing Property_Set, reserved word 'set' is expected, "
                   & "found " & Image_Current_Token);
            end if;

         when T_Identifier =>
            Declaration := P_System_Instance;

         when others =>
            Error_Loc (1) := Token_Location;
            DE ("Parsing AADL_Declaration, unexpected token "
                & Image_Current_Token);
      end case;

      return Declaration;
   end P_AADL_Declaration;

   --------------------------
   -- P_AALD_Specification --
   --------------------------

   --  AADL_specification ::= { AADL_declaration } +

   function P_AADL_Specification return Node_Id is
      Declarations    : List_Id;
      Declaration     : Node_Id;

   begin
      Specification := New_Node (K_AADL_Specification, Token_Location);
      Declarations  := New_List (K_AADL_Declaration_List, Token_Location);
      Set_Declarations (Specification, Declarations);

      loop
         Declaration := P_AADL_Declaration;
         if Present (Declaration) then
            Append_Node_To_List (Declaration, Declarations);
         end if;
         exit when Token = T_EOF;
      end loop;

      return Specification;
   end P_AADL_Specification;

   -----------------
   -- P_Component --
   -----------------

   function P_Component return Node_Id is
   begin
      return No_Node;
   end P_Component;

   -----------------------------
   -- P_Package_Specification --
   -----------------------------

   function P_Package_Specification return Node_Id is
   begin
      return No_Node;
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
