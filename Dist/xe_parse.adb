------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with Osint;            use Osint;
with Output;           use Output;
with Namet;            use Namet;
with Types;
with XE_Scan;          use XE_Scan;
with XE_Utils;         use XE_Utils;
with XE;               use XE;
with Table;

package body XE_Parse is

   use type Types.Name_Id;
   use type Types.Unit_Name_Type;
   use type Types.Int;

   subtype Name_Id        is Types.Name_Id;
   subtype Unit_Name_Type is Types.Unit_Name_Type;
   subtype Int            is Types.Int;

   Null_Name : constant Name_Id := Types.No_Name;

   Attribute_Prefix : Name_Id;
   Type_Prefix      : Name_Id;
   Pragma_Prefix    : Name_Id;

   Unique     : constant Boolean := True;
   Not_Unique : constant Boolean := False;

   procedure Exit_On_Parsing_Error;
   --  Print configuration if verbose_mode and then raise Parsing_Error.

   procedure Print_Configuration;
   --  Print node tree for debugging purpose. The global variable
   --  Configuration_Node is used as tree root.

   procedure Print
     (Node : in Node_Id;
      Head : in String);
   --  Print any node with string Head at the beginning of each line.

   procedure Print
     (Node : in Type_Id;
      Head : in String);
   --  Print a type node with its component and attributes.
   --  Attributes are printed with their values.

   procedure Print
     (Node : in Variable_Id;
      Head : in String);
   --  Print a variable node with its values and its attributes.
   --  Attributes are printed with their values.

   procedure Print
     (Node : in Parameter_Id;
      Head : in String);
   --  Print a parameter node with its value.

   procedure Print
     (Node : in Component_Id;
      Head : in String;
      Data : in Boolean);
   --  Print a component node. If Data is true, print only component values.
   --  If not, print only component attributes.

   procedure Print
     (Node : in Subprogram_Id;
      Head : in String);
   --  Print a subprogram node with its formal parameter list.

   procedure Print
     (Node : in Statement_Id;
      Head : in String);
   --  Print a statement node. This procedure currently doesn't print
   --  the actual parameters.

   procedure Print
     (Node : in Configuration_Id;
      Head : in String);
   --  Print a configuration node.

   procedure Has_Not_Been_Already_Declared
     (Declaration_Name : in Name_Id;
      Declaration_Sloc : in Location_Type);
   --  Check that this declaration is not already present.

   procedure Search_Declaration
     (Declaration_Name : in  Name_Id;
      Declaration_Node : out Node_Id);
   --  Search for the first occurrence of a declaration of name
   --  Declaration_Name. If unsuccessful, returns Null_Node.

   procedure Search_Subprogram
     (Subprogram_Name : in  Name_Id;
      Subprogram_Node : out Subprogram_Id);
   --  Search for the first occurrence of a subprogram of name
   --  Subprogram_Name. If unsuccessful, returns Null_Subprogram.

   procedure Search_Type
     (Type_Name : in  Name_Id;
      Type_Kind : out Predefined_Type;
      Type_Node : out Type_Id);
   --  Search for the first occurrence of a type of name Type_Name.
   --  If unsuccessful, returns Null_Type. If successful, Type_Kind
   --  is set to its predefined type enumeration litteral. Otherwise,
   --  it is set to Pre_Type_Unknown.

   procedure Search_Pragma
     (Pragma_Name : in  Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id);
   --  Search for the first occurrence of a pragma of name Pragma_Name.
   --  If unsuccessful, returns Null_Pragma. If successful, Pragma_Kind
   --  is set to its pragma type enumeration litteral. Otherwise,
   --  it is set to Pragma_Unknown.


   procedure Search_Component
     (Component_Name : in  Name_Id;
      Type_Node      : in  Type_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component of name Component_Name
   --  in a type Type_Node. If unsuccessful, returns Null_Component.

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Variable_Node  : in  Variable_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component of name Component_Name
   --  in a variable Variable_Node. If unsuccessful, returns Null_Component.


   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Node : out Variable_Id);
   --  Search for the first occurrence of a variable of name
   --  Variable_Name. If unsuccessful, returns Null_Variable.

   procedure Search_Actual_Parameter
     (Actual_Name : in  Name_Id;
      Actual_Type : in  Type_Id;
      Actual_Node : out Variable_Id);
   --  Similar to Search_Variable but with a additional restrictions.
   --  Variable_Node type should be Actual_Type.

   type Convention_Type is (Named, Positional);
   procedure Search_Matching_Parameter
     (Subprogram_Node : Subprogram_Id;
      Convention      : Convention_Type;
      Formal_Name     : in out Name_Id;
      Parameter_Node  : in out Parameter_Id);
   --  Search for a formal parameter that has no actual associated parameter.
   --  This choice should follow Convention requirements. If Convention
   --  is Named, then returns Parameter_Node of name Formal_Name. If
   --  is Positional, returns the next unmatched parameter and returns
   --  also its name in Formal_Name.

   procedure Search_Function_Returned_Parameter
     (Function_Node  : in Subprogram_Id;
      Parameter_Node : out Parameter_Id);
   --  Search for the last parameter of this subprogram. This is by
   --  convention the returned parameter.

   procedure Append_Declaration
     (Configuration_Node : in Configuration_Id;
      Declaration_Node   : in Node_Id)
     renames XE.Append_Configuration_Declaration;

   procedure Unmark_Subprogram_Parameters
     (Subprogram_Node : Subprogram_Id;
      N_Parameter     : out Int);
   --  Unmark all subprogram parameter. Marking a formal parameter means
   --  that this parameter has a corresponding actual parameter.

   procedure Update_Variable
     (Conf_Node     : in  Configuration_Id;
      Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Variable_Node : out Variable_Id);
   --  Update an old variable into the configuration context. This variable
   --  of name Variable_Name has a corresponding type, indicated by the
   --  type node Variable_Type.

   procedure Declare_Type
     (Conf_Node : in  Configuration_Id;
      Type_Name : in  Name_Id;
      Type_Kind : in  Predefined_Type;
      Structure : in  Boolean;
      Type_Node : out Type_Id);
   --  Declare a new type into the configuration context. Provide the
   --  type name and a possible predefined enumeration litteral.
   --  Structure indicates that this type has several fields.

   procedure Declare_Literal
     (Literal_Name : in  Name_Id;
      Literal_Type : in  Type_Id;
      Literal_Node : out Variable_Id);
   --  Declare a new literal.

   procedure Declare_Variable
     (Conf_Node     : in  Configuration_Id;
      Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Is_Unique     : in  Boolean;
      Variable_Sloc : in  Location_Type;
      Variable_Node : out Variable_Id);
   --  Declare a new variable into the configuration context. This variable
   --  of name Variable_Name has a corresponding type, indicated by the
   --  type node Variable_Type.

   procedure Declare_Component
     (Type_Node          : in Type_Id;
      Component_Name     : in Name_Id;
      Comp_Type_Node     : in Type_Id;
      Component_Node     : out Component_Id);
   --  Declare a component for a given type. This procedure creates
   --  a component of type Comp_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Attribute
     (Type_Node          : in Type_Id;
      Attribute_Name     : in Name_Id;
      Attr_Type_Node     : in Type_Id;
      Attribute_Kind     : in Attribute_Type;
      Attribute_Node     : out Attribute_Id);
   --  Declare an attribute for a given type. This procedure creates
   --  a component of type Attr_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Attribute
     (Variable_Node      : in Variable_Id;
      Attribute_Name     : in Name_Id;
      Attribute_Node     : out Attribute_Id);
   --  Declare an attribute for a given variable. This procedure creates
   --  a copy of the attribute that can be found into the variable type node.

   procedure Declare_Component
     (Variable_Node      : in Variable_Id;
      Component_Name     : in Name_Id;
      Component_Type     : in Type_Id;
      Component_Value    : in Variable_Id;
      Is_An_Attribute    : in Boolean;
      Component_Node     : out Component_Id);
   --  Declare a component for a given variable. This component is
   --  possibly an attribute and is initialized to Component_Value.
   --  The component type is given by Component_Type.

   procedure Declare_Subprogram
     (Subprogram_Name  : in  Name_Id;
      Is_A_Procedure   : in  Boolean;
      Subprogram_Sloc  : in  Location_Type;
      Subprogram_Node  : out Subprogram_Id);
   --  Declare a subprogram into the configuration context. This subprogram
   --  is possibly a function. At this point, the subprogram has no
   --  parameter.

   procedure Declare_Subprogram_Parameter
     (Parameter_Name  : in  Name_Id;
      Para_Type_Node  : in  Type_Id;
      Subprogram_Node : in  Subprogram_Id;
      Parameter_Node  : out Parameter_Id);
   --  Declare a parameter for a declared subprogram. The last parameter
   --  corresponds to a returned value when the subprogram is a function.

   procedure Declare_Procedure_Call
     (Subprogram_Node : in Subprogram_Id);
   --  Declare a call to a procedure. A statement node is created and
   --  contains an entire copy of the subprogram node.

   procedure Assign_Variable
     (Source, Target : Variable_Id);
   --  Mostly, assign a formal parameter to a given value in case
   --  of a procedure call.

   function Is_Expression_Of_Type
     (Expr_Node : in Node_Id;
      Type_Node : in Type_Id)
      return Boolean;
   --  When Expr_Node is a variable, compares the given type and the
   --  variable type. When Expr_Node is a function, compares the given
   --  type and the type of the returned parameter.


   -- Token subprograms --

   procedure Take_Token (T : Token_Type);
   procedure Take_Token (L : Token_List_Type);

   function  Match (L : Token_List_Type) return Boolean;

   procedure No_Match (L : Token_List_Type);
   procedure No_Match (T : Token_Type);

   procedure T_String_Literal;
   procedure T_Identifier;
   procedure T_Dot;
   procedure T_Apostrophe;
   procedure T_Left_Paren;
   procedure T_Right_Paren;
   procedure T_Comma;
   procedure T_Colon_Equal;
   procedure T_Colon;
   procedure T_Configuration;
   procedure T_Pragma;
   procedure T_Procedure;
   procedure T_Is;
   procedure T_In;
   procedure T_For;
   procedure T_Use;
   procedure T_Function;
   procedure T_End;
   procedure T_Arrow;
   procedure T_EOF;
   procedure T_Semicolon;
   procedure T_Return;

   procedure P_Configuration_Declaration;
   procedure P_Configuration_Body;
   procedure P_Configuration_End;
   procedure P_Procedure_Declaration;
   procedure P_Function_Declaration;
   procedure P_Pragma;
   procedure P_Representation_Clause;
   procedure P_Full_Ada_Identifier;

   procedure P_Aggregate_Assignement     (Variable_Node   : in Variable_Id);
   procedure Associate_Actual_To_Formal  (Subprogram_Node : in Subprogram_Id);
   procedure P_Variable_List_Declaration
     (Previous_Name   : in Name_Id;
      Previous_Sloc   : in Location_Type);

   --------------
   -- No_Match --
   --------------

   procedure No_Match (L : Token_List_Type) is
   begin
      Write_Location (Get_Token_Location);
      Write_Token (L (L'First));
      for Index in L'First + 1 .. L'Last loop
         Write_Str (" or ");
         Write_Token (L (Index));
      end loop;
      Write_Str (" was expected");
      Write_Eol;
      Exit_On_Parsing_Error;
   end No_Match;

   --------------
   -- No_Match --
   --------------

   procedure No_Match (T : Token_Type) is
   begin
      Write_Location (Get_Token_Location);
      Write_Token (T);
      Write_Str (" was expected");
      Write_Eol;
      Exit_On_Parsing_Error;
   end No_Match;

   -----------
   -- Match --
   -----------

   function Match (L : Token_List_Type) return Boolean is
   begin
      for Index in L'Range loop
         if L (Index) = Token then
            return True;
         end if;
      end loop;
      return False;
   end Match;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (T : Token_Type) is
   begin
      Next_Token;
      if T /= Token then
         No_Match (T);
      end if;
   end Take_Token;

   ----------------
   -- Take_Token --
   ----------------

   procedure Take_Token (L : Token_List_Type) is
   begin
      Next_Token;
      if Match (L) then
         return;
      end if;
      No_Match (L);
   end Take_Token;

   ----------------------
   -- T_String_Literal --
   ----------------------

   procedure T_String_Literal is
   begin
      Take_Token (Tok_String_Literal);
   end T_String_Literal;

   ------------------
   -- T_Identifier --
   ------------------

   procedure T_Identifier is
   begin
      Take_Token (Tok_Identifier);
   end T_Identifier;

   -----------
   -- T_Dot --
   -----------

   procedure T_Dot is
   begin
      Take_Token (Tok_Dot);
   end T_Dot;

   ------------------
   -- T_Apostrophe --
   ------------------

   procedure T_Apostrophe is
   begin
      Take_Token (Tok_Apostrophe);
   end T_Apostrophe;

   ------------------
   -- T_Left_Paren --
   ------------------

   procedure T_Left_Paren is
   begin
      Take_Token (Tok_Left_Paren);
   end T_Left_Paren;

   -------------------
   -- T_Right_Paren --
   -------------------

   procedure T_Right_Paren is
   begin
      Take_Token (Tok_Right_Paren);
   end T_Right_Paren;

   -------------
   -- T_Comma --
   -------------

   procedure T_Comma is
   begin
      Take_Token (Tok_Comma);
   end T_Comma;

   -------------------
   -- T_Colon_Equal --
   -------------------

   procedure T_Colon_Equal is
   begin
      Take_Token (Tok_Colon_Equal);
   end T_Colon_Equal;

   -------------
   -- T_Colon --
   -------------

   procedure T_Colon is
   begin
      Take_Token (Tok_Colon);
   end T_Colon;

   ---------------------
   -- T_Configuration --
   ---------------------

   procedure T_Configuration is
   begin
      Take_Token (Tok_Configuration);
   end T_Configuration;

   --------------
   -- T_Pragma --
   --------------

   procedure T_Pragma is
   begin
      Take_Token (Tok_Pragma);
   end T_Pragma;

   -----------------
   -- T_Procedure --
   -----------------

   procedure T_Procedure is
   begin
      Take_Token (Tok_Procedure);
   end T_Procedure;

   ----------
   -- T_Is --
   ----------

   procedure T_Is is
   begin
      Take_Token (Tok_Is);
   end T_Is;

   ----------
   -- T_In --
   ----------

   procedure T_In is
   begin
      Take_Token (Tok_In);
   end T_In;

   -----------
   -- T_For --
   -----------

   procedure T_For is
   begin
      Take_Token (Tok_For);
   end T_For;

   -----------
   -- T_Use --
   -----------

   procedure T_Use is
   begin
      Take_Token (Tok_Use);
   end T_Use;

   --------------
   -- T_Return --
   --------------

   procedure T_Return is
   begin
      Take_Token (Tok_Return);
   end T_Return;

   ----------------
   -- T_Function --
   ----------------

   procedure T_Function is
   begin
      Take_Token (Tok_Function);
   end T_Function;

   -----------
   -- T_End --
   -----------

   procedure T_End is
   begin
      Take_Token (Tok_End);
   end T_End;

   -------------
   -- T_Arrow --
   -------------

   procedure T_Arrow is
   begin
      Take_Token (Tok_Arrow);
   end T_Arrow;

   -----------
   -- T_EOF --
   -----------

   procedure T_EOF is
   begin
      Take_Token (Tok_EOF);
   end T_EOF;

   -----------------
   -- T_Semicolon --
   -----------------

   procedure T_Semicolon is
   begin
      Take_Token (Tok_Semicolon);
   end T_Semicolon;

   ----------------------------
   -- P_Function_Declaration --
   ----------------------------

   procedure P_Function_Declaration is
      Function_Name  : Name_Id;
      Function_Node  : Subprogram_Id;
      Parameter_Name : Name_Id;
      Parameter_Node : Parameter_Id;
      Para_Type_Name : Name_Id;
      Para_Type_Node : Type_Id;
      Para_Type_Kind : Predefined_Type;
   begin

      --  The following is the only allowed signature :
      --     function <F> (<X> : String) return String;
      --  where <F> and <X> are to be defined.

      --  Token FUNCTION has already been parsed.

      T_Identifier;
      Function_Name := Token_Name;

      --  Create a new subprogram node for this newly declared function.

      Declare_Subprogram
        (Function_Name,
         False,
         Get_Token_Location,
         Function_Node);

      T_Left_Paren;

      --  Get parameter <X>.

      T_Identifier;
      Parameter_Name := Token_Name;

      T_Colon;

      --  Get parameter type.

      T_Identifier;
      Para_Type_Name := Token_Name;

      Search_Type (Para_Type_Name, Para_Type_Kind, Para_Type_Node);

      --  String is the only expected type.

      if Para_Type_Node /= String_Type_Node then
         Write_Location (Get_Token_Location);
         Write_Str  ("""");
         Write_Name (Para_Type_Name);
         Write_Str  (""" is not the expected type");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  Declare <X> as a formal parameter.

      Declare_Subprogram_Parameter
        (Parameter_Name,
         Para_Type_Node,
         Function_Node,
         Parameter_Node);

      T_Right_Paren;
      T_Return;

      --  Get returned parameter type.

      T_Identifier;
      Para_Type_Name := Token_Name;

      Search_Type (Para_Type_Name, Para_Type_Kind, Para_Type_Node);

      --  String is the only type allowed at this level.

      if Para_Type_Node /= String_Type_Node then
         Write_Location (Get_Token_Location);
         Write_Str  ("""");
         Write_Name (Para_Type_Name);
         Write_Str  (""" is not the expected type");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  Declare returned parameter type. As a naming convention
      --  we use keyword Returned_Param as the anonymous parameter.

      Declare_Subprogram_Parameter
        (Returned_Param,
         Para_Type_Node,
         Function_Node,
         Parameter_Node);

      T_Semicolon;

   end P_Function_Declaration;

   -------------------------
   -- P_Configuration_End --
   -------------------------

   procedure P_Configuration_End is
      Conf : Node_Id;
   begin
      Take_Token ((Tok_Identifier, Tok_Semicolon));

      --  Check that the configuration name is matching the current
      --  configuration name.

      if Token = Tok_Identifier then
         Search_Declaration (Token_Name, Conf);
         if not Is_Configuration (Conf) then
            Write_Location (Get_Token_Location);
            Write_Str ("name mismatch");
            Write_Eol;
            Exit_On_Parsing_Error;
         end if;
         T_Semicolon;
      end if;

   end P_Configuration_End;

   --------------
   -- P_Pragma --
   --------------

   procedure P_Pragma is
      Pragma_Kind : Pragma_Type;
      Pragma_Node : Subprogram_Id;
      Pragma_Name : Name_Id;
   begin

      --  Token PRAGMA has already been parsed.

      T_Identifier;

      --  Known pragmas are prefixed by Pragma_Prefix.

      Pragma_Name := Pragma_Prefix & Token_Name;

      --  Is this pragma a legal pragma.

      Search_Pragma (Pragma_Name, Pragma_Kind, Pragma_Node);
      if Pragma_Kind = Pragma_Unknown then
         Write_Location (Get_Token_Location);
         Write_Str  ("unrecognized pragma """);
         Write_Name (Token_Name);
         Write_Str  ("""");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  Parse a pragma as a procedure call.

      Associate_Actual_To_Formal (Pragma_Node);

      --  When successful, declare the procedure call node.

      Declare_Procedure_Call (Pragma_Node);

   end P_Pragma;

   -----------------------------
   -- P_Procedure_Declaration --
   -----------------------------

   procedure P_Procedure_Declaration is
      Unit_Name      : Name_Id;
      Unit_Node      : Variable_Id;
      Unit_Sloc      : Location_Type;
      Partition_Name : Name_Id;
      Partition_Node : Variable_Id;
      Partition_Sloc : Location_Type;
      Component_Node : Component_Id;
      Procedure_Node : Subprogram_Id;
   begin

      --  Token PROCEDURE has already been parsed.

      T_Identifier;

      Unit_Name := Token_Name;
      Unit_Sloc := Get_Token_Location;
      Search_Variable
        (Unit_Name,
         Unit_Node);

      --  Is this unit already declared ?

      if Unit_Node /= Null_Variable then

         --  If yes, then it has to be an ada unit.

         if Get_Variable_Type (Unit_Node) /= Ada_Unit_Type_Node then
            Write_Location (Unit_Sloc);
            Write_Str  ("""");
            Write_Name (Unit_Name);
            Write_Str  (""" conflicts with a previous declaration");
            Write_Eol;
            Exit_On_Parsing_Error;
         end if;

      end if;

      Take_Token ((Tok_Is, Tok_Semicolon));

      case Token is

         when Tok_Is =>

            --  Ada_Unit_Type is a extensible enumeration type. Declares
            --  a new enumeration litteral for this special type.

            Declare_Variable
              (Configuration_Node,
               Unit_Name,
               Ada_Unit_Type_Node,
               Unique,
               Unit_Sloc,
               Unit_Node);

            T_In;

            --  This should be an already declared variable.

            T_Identifier;
            Partition_Name := Token_Name;
            Partition_Sloc := Get_Token_Location;
            Search_Variable
              (Partition_Name,
               Partition_Node);

            --  This variable has to be already declared. Its type has to be
            --  of the predefined type Partition_Type_Node.

            if Partition_Node = Null_Variable or else
              Get_Variable_Type (Partition_Node) /= Partition_Type_Node then
               Write_Location (Partition_Sloc);
               Write_Str  ("variable """);
               Write_Name (Partition_Name);
               Write_Str  (""" has not been declared as a partition");
               Write_Eol;
               Exit_On_Parsing_Error;
            end if;

            --  Partition_Type_Node is an unconstraint array type. In this
            --  context, Partition_Node has a new element Unit_Node.
            --  As a naming convention, we use the keyword Procedure_Unit
            --  to indicate a main subprogram.

            Declare_Component
              (Variable_Node      => Partition_Node,
               Component_Name     => Procedure_Unit,
               Component_Type     => Ada_Unit_Type_Node,
               Component_Value    => Unit_Node,
               Is_An_Attribute    => False,
               Component_Node     => Component_Node);

            T_Semicolon;

         when Tok_Semicolon =>

            --  This is just a parameterless procedure declaration.

            Declare_Subprogram
              (Unit_Name,
               True,
               Unit_Sloc,
               Procedure_Node);

         when others =>
            null;

      end case;

   end P_Procedure_Declaration;

   -----------------------------
   -- P_Representation_Clause --
   -----------------------------

   procedure P_Representation_Clause is
      Direct_Name : Name_Id;
      Direct_Node : Node_Id;
      Direct_Type : Type_Id;
      Attr_Name   : Name_Id;
      Attr_Node   : Attribute_Id;
      Attr_Type   : Type_Id;
      Comp_Node   : Component_Id;
      Expr_Name   : Name_Id;
      Expr_Node   : Node_Id;
      Expr_Sloc   : Location_Type;
      Is_A_Type   : Boolean;
   begin

      --  Token FOR has already been parsed.

      T_Identifier;
      Direct_Name := Token_Name;
      Search_Declaration (Direct_Name, Direct_Node);

      --  This identifier has to be already declared.

      if Direct_Node /= Null_Node then

         --  If legal, retrieve variable Direct_Node type.

         if Is_Variable (Direct_Node) then
            Direct_Type := Get_Variable_Type (Variable_Id (Direct_Node));
            Is_A_Type := False;

         elsif Is_Type (Direct_Node) then
            Direct_Type := Type_Id (Direct_Node);
            Is_A_Type := True;

         --  Only variables and types are subject to representation clause.

         else
            Write_Location (Get_Token_Location);
            Write_Str ("identifier """);
            Write_Name (Direct_Name);
            Write_Str (""" is neither a variable");
            Write_Str (" nor a predefined type");
            Write_Eol;
            Exit_On_Parsing_Error;
         end if;

      else
         Write_Location (Get_Token_Location);
         Write_Str ("identifier """);
         Write_Name (Direct_Name);
         Write_Str (""" is undefined here");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      T_Apostrophe;

      --  Get the attribute name.

      T_Identifier;
      Attr_Name := Token_Name;

      --  Attributes are always prefixed by Attribute_Prefix.

      Search_Component
        (Attribute_Prefix & Attr_Name,
         Direct_Type,
         Comp_Node);

      --  Check that this attribute is a legal attribute for the given type.

      if Comp_Node = Null_Component or else
         not Is_Component_An_Attribute (Comp_Node) then
         Write_Location (Get_Token_Location);
         Write_Str  ("unrecognized attribute """);
         Write_Name (Attr_Name);
         Write_Str  ("""");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  If variable, duplicate attribute for the variable only.

      if not Is_A_Type then
         Declare_Attribute
           (Variable_Id (Direct_Node),
            Attr_Name,
            Attr_Node);
      else
         Attr_Node := Attribute_Id (Comp_Node);
      end if;

      T_Use;
      Take_Token ((Tok_Identifier, Tok_String_Literal));
      Expr_Name := Token_Name;
      Expr_Sloc := Get_Token_Location;

      --  If string literal, declare an anonymous variable.

      if Token = Tok_String_Literal then
         Declare_Literal
           (Expr_Name,
            String_Type_Node,
            Variable_Id (Expr_Node));

      --  Otherwise, retrieve the declaration.

      else
         Search_Declaration (Expr_Name, Expr_Node);
         if Expr_Node = Null_Node then
            Write_Location (Expr_Sloc);
            Write_Str ("""");
            Write_Name (Expr_Name);
            Write_Str (""" has not been declared");
            Write_Eol;
            Exit_On_Parsing_Error;
         end if;
      end if;

      Attr_Type := Get_Component_Type (Component_Id (Attr_Node));

      --  Check that the expression has the correct type.

      if not Is_Expression_Of_Type (Expr_Node, Attr_Type) then
         Write_Location (Get_Token_Location);
         Write_Str ("""");
         Write_Name (Get_Node_Name (Node_Id (Expr_Node)));
         Write_Str (""" is an invalid expression here");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  Set attribute to the given value.

      Set_Component_Value (Component_Id (Attr_Node), Expr_Node);

      T_Semicolon;

   end P_Representation_Clause;

   ---------------------------
   -- P_Full_Ada_Identifier --
   ---------------------------

   procedure P_Full_Ada_Identifier is
      Identifier : Name_Id := Token_Name;
      Location   : Location_Type;
   begin
      loop
         Next_Token;
         Location := Get_Token_Location;

         --  If token is '.' then continue ...

         if Token = Tok_Dot then
            T_Identifier;
            Identifier := Identifier & Dot_Sep_Id & Token_Name;

         --  If not, then this is the identifier end.

         else
            Set_Token_Location (Location);
            Token_Name := Identifier;
            Token      := Tok_Identifier;
            exit;
         end if;
      end loop;

   end P_Full_Ada_Identifier;

   -----------------------------
   -- P_Aggregate_Assignement --
   -----------------------------

   procedure P_Aggregate_Assignement (Variable_Node : in Variable_Id) is
      Expression_Name : Name_Id;
      Expression_Node : Variable_Id;
      Expression_Sloc : Location_Type;
      Component_Node  : Component_Id;
   begin
      T_Left_Paren;
      loop
         Take_Token ((Tok_Identifier, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

         --  Ada unit names are allowed.

         Expression_Sloc := Get_Token_Location;
         P_Full_Ada_Identifier;
         Expression_Name := Token_Name;

         --  Increase the ada unit enumeration type.

         Declare_Variable
           (Configuration_Node,
            Expression_Name,
            Ada_Unit_Type_Node,
            Unique,
            Expression_Sloc,
            Expression_Node);

         --  As a naming convention, we use the keyword Conf_Ada_Unit
         --  as a anonymous component name.

         Declare_Component
           (Variable_Node      => Variable_Node,
            Component_Name     => Conf_Ada_Unit,
            Component_Type     => Ada_Unit_Type_Node,
            Component_Value    => Expression_Node,
            Is_An_Attribute    => False,
            Component_Node     => Component_Node);

         Take_Token ((Tok_Comma, Tok_Right_Paren));
         exit when Token = Tok_Right_Paren;

      end loop;

      T_Semicolon;

   end P_Aggregate_Assignement;

   ---------------------------------
   -- P_Variable_List_Declaration --
   ---------------------------------

   procedure P_Variable_List_Declaration
     (Previous_Name : in Name_Id;
      Previous_Sloc : in Location_Type) is
      Previous_Node : Variable_Id;
      Variable_Name : Name_Id;
      Variable_Node : Variable_Id;
      Variable_Sloc : Location_Type;
      Var_Type_Name : Name_Id;
      Var_Type_Node : Type_Id;
      Var_Type_Kind : Predefined_Type;
      Var_Type_Sloc : Location_Type;
   begin

      Take_Token ((Tok_Comma, Tok_Colon));

      --  Is it a list of identifiers ?

      if Token = Tok_Comma then

         T_Identifier;
         Variable_Name := Token_Name;
         Variable_Sloc := Get_Token_Location;

         --  Declare a temporary variable of any type.

         Declare_Variable
           (Configuration_Node,
            Previous_Name,
            Partition_Type_Node,
            Unique,
            Previous_Sloc,
            Previous_Node);

         --  Call recursively P_Variable_List_Declaration until the
         --  end of list. Variable_Node is a node to the next
         --  declared variable.

         P_Variable_List_Declaration (Variable_Name, Variable_Sloc);
         Search_Variable (Variable_Name, Variable_Node);

         --  Update the temporary variable type with variable_node type

         Update_Variable
           (Conf_Node          => Configuration_Node,
            Variable_Name      => Previous_Name,
            Variable_Type      => Get_Variable_Type (Variable_Node),
            Variable_Node      => Previous_Node);

         --  If previous variable has been initialized, initialize
         --  this newly declared variable as well.

         Assign_Variable (Variable_Node, Previous_Node);

      else

         --  The following identifier is a type.

         T_Identifier;
         Var_Type_Name := Token_Name;
         Var_Type_Sloc := Get_Token_Location;

         --  Has this type been declared ?

         Search_Type
           (Var_Type_Name,
            Var_Type_Kind,
            Var_Type_Node);

         if Var_Type_Node = Null_Type then
            Write_Location (Var_Type_Sloc);
            Write_Str  ("unexpected type """);
            Write_Name (Var_Type_Name);
            Write_Str  ("""");
            Write_Eol;
            Exit_On_Parsing_Error;
         end if;

         --  Declare this new variable of type Var_Type_Node.

         Declare_Variable
           (Configuration_Node,
            Previous_Name,
            Var_Type_Node,
            Unique,
            Previous_Sloc,
            Previous_Node);

         Take_Token ((Tok_Semicolon, Tok_Colon_Equal));

         --  Is there an initialization ?

         if Token = Tok_Colon_Equal then
            P_Aggregate_Assignement (Previous_Node);
         end if;

      end if;

   end P_Variable_List_Declaration;

   --------------------------
   -- P_Configuration_Body --
   --------------------------

   procedure P_Configuration_Body is
      Name : Name_Id;
   begin
      loop
         Take_Token
           ((Tok_Identifier,
             Tok_Null,
             Tok_End));
         case Token is
            when Tok_Identifier =>

               --  This is an assignement. Includes a list of ada units
               --  into a partition.

               declare
                  Variable_Node : Variable_Id;
               begin
                  Name := Token_Name;
                  Search_Variable (Name, Variable_Node);
                  if Variable_Node = Null_Variable then
                     Write_Location (Get_Token_Location);
                     Write_Str ("""");
                     Write_Name (Name);
                     Write_Str (""" has not been declared");
                     Write_Eol;
                     Exit_On_Parsing_Error;
                  end if;
                  T_Colon_Equal;

                  --  Read the ada units aggregate.

                  P_Aggregate_Assignement (Variable_Node);

               end;

            when Tok_End        =>
               P_Configuration_End; exit;

            when others         =>  null;

         end case;

      end loop;

   end P_Configuration_Body;

   --------------------------------
   -- Associate_Actual_To_Formal --
   --------------------------------

   procedure Associate_Actual_To_Formal
     (Subprogram_Node : in Subprogram_Id) is
      Convention     : Convention_Type;
      Actual_Name    : Name_Id;
      Formal_Name    : Name_Id;
      Actual_Node    : Variable_Id;
      Formal_Node    : Parameter_Id;
      N_Parameter    : Int;
      Location       : Location_Type;
   begin

      --  Look for the matching (marked) parameters. When a formal
      --  parameter has an associated actual parameter, mark the
      --  formal parameter and set the formal parameter value to
      --  the actual parameter.

      Unmark_Subprogram_Parameters (Subprogram_Node, N_Parameter);

      Location := Get_Token_Location;

      --  At the beginning, convention is unknown.

      if N_Parameter > 0 then
         T_Left_Paren;

         --  What is the the convention used here.

         T_Identifier;
         Location := Get_Token_Location;
         Take_Token ((Tok_Arrow, Tok_Comma, Tok_Right_Paren));
         if Token = Tok_Arrow then
            Convention := Named;
         else
            Convention := Positional;
         end if;
         Set_Token_Location (Location);

         loop
            Location := Get_Token_Location;

            if Convention = Named then
               T_Identifier;
               Formal_Name := Token_Name;
               T_Arrow;
            end if;

            --  If convention = named, check that such a formal parameter
            --     belongs to the subprogram parameter list.
            --  If convention = positional, retrieve the first unmarked
            --     (unmatched) parameter (name and node).

            Search_Matching_Parameter
              (Subprogram_Node,
               Convention,
               Formal_Name,
               Formal_Node);

            if Formal_Node = Null_Parameter then
               Write_Location (Location);
               Write_Str ("formal parameter mismatch");
               Write_Eol;
               Exit_On_Parsing_Error;
            end if;

            Take_Token ((Tok_Identifier, Tok_String_Literal));
            Location    := Get_Token_Location;
            Actual_Name := Token_Name;

            if Token = Tok_String_Literal then

               if Get_Parameter_Type (Formal_Node) /= String_Type_Node then
                  Write_Location (Location);
                  Write_Str ("actual parameter mismatch");
                  Write_Eol;
                  Exit_On_Parsing_Error;
               end if;

               --  Create a dummy declaration that contains the literal.

               Declare_Literal
                 (Actual_Name,
                  String_Type_Node,
                  Actual_Node);

            else

               --  Does this actual parameter really exist ?

               Search_Actual_Parameter
                 (Actual_Name,
                  Get_Parameter_Type (Formal_Node),
                  Actual_Node);

               if Actual_Node = Null_Variable then
                  Write_Location (Location);
                  Write_Str ("actual parameter mismatch");
                  Write_Eol;
                  Exit_On_Parsing_Error;
               end if;

            end if;

            --  Mark the matching parameter and set its value to actual
            --  parameter value.

            Assign_Variable (Actual_Node, Variable_Id (Formal_Node));

            --  There is one less parameter to match.

            Set_Parameter_Mark (Formal_Node, 1);
            N_Parameter := N_Parameter - 1;

            Take_Token ((Tok_Comma, Tok_Right_Paren));

            if Token = Tok_Right_Paren then
               exit when N_Parameter = 0;
               Write_Location (Get_Token_Location);
               Write_Str ("missing parameters");
               Write_Eol;
               Exit_On_Parsing_Error;
            elsif Token /= Tok_Right_Paren and then
               N_Parameter = 0 then
               Write_Location (Get_Token_Location);
               Write_Str ("too many parameters");
               Write_Eol;
               Exit_On_Parsing_Error;
            end if;

         end loop;

      end if;

      T_Semicolon;

   end Associate_Actual_To_Formal;

   ---------------------------------
   -- P_Configuration_Declaration --
   ---------------------------------

   procedure P_Configuration_Declaration is
      Conf_Name : Name_Id;
      Conf_Node : Configuration_Id;
   begin

      --  Use "standard" configuration to start.

      T_Configuration;
      T_Identifier;
      Conf_Name := Token_Name;
      T_Is;

      --  We have the real configuration node. Let's use this one.

      Create_Configuration (Conf_Node, Conf_Name);

      --  Append the "standard" root configuration to the new one.

      Append_Declaration   (Conf_Node, Node_Id (Configuration_Node));

      --  Now, the new configuration is the root configuration.

      Configuration_Node := Conf_Node;

   end P_Configuration_Declaration;

   -----------
   -- Parse --
   -----------

   procedure Parse is
   begin  --  Parse

      Maybe_Most_Recent_Stamp (Source_File_Stamp (Configuration_File));

      Load_File (Configuration_File);

      P_Configuration_Declaration;

      loop

         Take_Token
           ((Tok_Identifier,
             Tok_Procedure,
             Tok_Function,
             Tok_For,
             Tok_Pragma,
             Tok_Begin,
             Tok_End));

         case Token is

            when Tok_Function   =>
               P_Function_Declaration;

            when Tok_Procedure  =>
               P_Procedure_Declaration;

            when Tok_For        =>
               P_Representation_Clause;

            when Tok_Pragma     =>
               P_Pragma;

            when Tok_Identifier =>
               P_Variable_List_Declaration (Token_Name, Get_Token_Location);

            when Tok_Begin      =>
               P_Configuration_Body;
               exit;

            when Tok_End        =>
               P_Configuration_End;
               exit;

            when others         => null;
         end case;

      end loop;

      T_EOF;

      Print_Configuration;

   end Parse;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Attribute_Node : Attribute_Id;
      Variable_Node  : Variable_Id;
      Parameter_Node : Parameter_Id;
      Component_Node : Component_Id;

   begin

      Attribute_Prefix := Str_To_Id ("attr__");
      Pragma_Prefix    := Str_To_Id ("pragma__");
      Type_Prefix      := Str_To_Id ("type__");

      Conf_Ada_Unit  := Str_To_Id ("_any__ada__unit");
      Part_Main_Unit := Str_To_Id ("_main_procedure");
      Procedure_Unit := Str_To_Id ("_appl_procedure");
      Returned_Param := Str_To_Id ("_returned_param");
      Sub_Prog_Param := Str_To_Id ("_sub_prog_param");
      Procedure_Call := Str_To_Id ("_procedure_call");

      --  As a naming convention, we use the rserved keyword "private"
      --  for the standard configuration name.

      Create_Configuration
        (Configuration_Node,
         Str_To_Id ("private"));

      --  type string (standard)

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Str_To_Id ("string"),
         Type_Kind    => Pre_Type_String,
         Structure    => False,
         Type_Node    => String_Type_Node);

      --  type type__host_function (standard)
      --     function F (...: String) return String;

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Type_Prefix & Str_To_Id ("host_function"),
         Type_Kind    => Pre_Type_Function,
         Structure    => True,
         Type_Node    => Host_Function_Type_Node);

      Declare_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => Sub_Prog_Param,
         Comp_Type_Node   => String_Type_Node,
         Component_Node   => Component_Node);

      Declare_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => Returned_Param,
         Comp_Type_Node   => String_Type_Node,
         Component_Node   => Component_Node);

      --  type type__main_procedure (standard)
      --     procedure P

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Type_Prefix & Str_To_Id ("main_procedure"),
         Type_Kind    => Pre_Type_Procedure,
         Structure    => False,
         Type_Node    => Main_Procedure_Type_Node);

      --  type Partition (standard)

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Str_To_Id ("partition"),
         Type_Kind    => Pre_Type_Partition,
         Structure    => True,
         Type_Node    => Partition_Type_Node);

      --  Legal attribute : 'Main
      --  Legal attribute : 'Host
      --  Legal attribute : 'Storage_Dir
      --  Legal attribute : 'Command_Line

      Declare_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("main"),
         Attr_Type_Node => Main_Procedure_Type_Node,
         Attribute_Kind => Attribute_Main,
         Attribute_Node => Attribute_Node);

      Declare_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("host"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Host,
         Attribute_Node => Attribute_Node);

      Declare_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("storage_dir"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Storage_Dir,
         Attribute_Node => Attribute_Node);

      Declare_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("command_line"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Command_Line,
         Attribute_Node => Attribute_Node);

      --  type type__ada_unit (standard)

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Type_Prefix & Str_To_Id ("ada_unit"),
         Type_Kind    => Pre_Type_Ada_Unit,
         Structure    => False,
         Type_Node    => Ada_Unit_Type_Node);

      --  type Starter_Type is (Ada, Shell, None); (standard)
      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Type_Prefix & Str_To_Id ("starter"),
         Type_Kind    => Pre_Type_Starter,
         Structure    => False,
         Type_Node    => Starter_Type_Node);

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("ada"),
         Starter_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, Convert (Ada_Starter));

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("shell"),
         Starter_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, Convert (Shell_Starter));

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("none"),
         Starter_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, Convert (None_Starter));

      --  type Boolean_Type is (False, True); (standard)

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Str_To_Id ("boolean"),
         Type_Kind    => Pre_Type_Boolean,
         Structure    => False,
         Type_Node    => Boolean_Type_Node);

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("true"),
         Boolean_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, 1);

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("false"),
         Boolean_Type_Node,
         Unique,
         Null_Location,
         Variable_Node);
      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, 0);


      --  type Convention_Type is (Ada, Shell); (standard)

      Declare_Type
        (Conf_Node    => Configuration_Node,
         Type_Name    => Type_Prefix & Str_To_Id ("convention"),
         Type_Kind    => Pre_Type_Convention,
         Structure    => False,
         Type_Node    => Convention_Type_Node);

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("ada"),
         Convention_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, Convert (Ada_Import));

      Declare_Variable
        (Configuration_Node,
         Str_To_Id ("shell"),
         Convention_Type_Node,
         Not_Unique,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Variable_Mark (Variable_Node, Convert (Shell_Import));

      --  pragma starter ... or
      --  procedure pragma__starter
      --    (method : starter__type);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("starter"),
         True,
         Null_Location,
         Pragma_Starter_Node);

      --  To easily retrieve the enumeration literal.
      Set_Subprogram_Mark (Pragma_Starter_Node, Convert (Pragma_Starter));

      Declare_Subprogram_Parameter
        (Str_To_Id ("method"),
         Starter_Type_Node,
         Pragma_Starter_Node,
         Parameter_Node);

      --  pragma Import ... or
      --  procedure pragma__import
      --    (convention : convention__type;
      --     entity     : procedure;
      --     link_name  : string);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("import"),
         True,
         Null_Location,
         Pragma_Import_Node);

      --  To easily retrieve the enumeration literal.
      Set_Subprogram_Mark (Pragma_Import_Node, Convert (Pragma_Import));

      Declare_Subprogram_Parameter
        (Str_To_Id ("convention"),
         Convention_Type_Node,
         Pragma_Import_Node,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("entity"),
         Ada_Unit_Type_Node,
         Pragma_Import_Node,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("link_name"),
         String_Type_Node,
         Pragma_Import_Node,
         Parameter_Node);

      --  pragma invocation_key ... or
      --  procedure pragma__starter
      --    (method : starter__type);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("invocation"),
         True,
         Null_Location,
         Pragma_Invocation_Node);

      --  To easily retrieve the enumeration literal.
      Set_Subprogram_Mark
        (Pragma_Invocation_Node,
         Convert (Pragma_Invocation));

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_name"),
         String_Type_Node,
         Pragma_Invocation_Node,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_data"),
         String_Type_Node,
         Pragma_Invocation_Node,
         Parameter_Node);

      Print_Configuration;

   end Initialize;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable
     (Conf_Node     : in  Configuration_Id;
      Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Variable_Node : out Variable_Id) is
      V : Variable_Id;
   begin

      Search_Variable (Variable_Name, V);
      Set_Variable_Type  (V, Variable_Type);
      Variable_Node := V;

   end Update_Variable;

   ------------------------
   -- Declare_Subprogram --
   ------------------------

   procedure Declare_Subprogram
     (Subprogram_Name  : in  Name_Id;
      Is_A_Procedure   : in  Boolean;
      Subprogram_Sloc  : in  Location_Type;
      Subprogram_Node  : out Subprogram_Id) is
      Node : Subprogram_Id;
      Junk : Variable_Id;
   begin

      Has_Not_Been_Already_Declared
        (Subprogram_Name,
         Subprogram_Sloc);

      Create_Subprogram         (Node, Subprogram_Name);
      Subprogram_Is_A_Procedure (Node, Is_A_Procedure);
      Append_Declaration        (Configuration_Node, Node_Id (Node));
      Subprogram_Node := Node;

      --  We create a variable of type Ada_Unit_Type_Node to handle
      --  a pragma statement like a procedure call. The variable value is
      --  a reference to the previous subprogram node. It is an ada unit
      --  in order to allow further configuration operations.

      Declare_Variable
        (Configuration_Node,
         Subprogram_Name,
         Ada_Unit_Type_Node,
         Not_Unique,
         Subprogram_Sloc,
         Junk);

      Set_Variable_Value (Junk, Variable_Id (Node));

   end Declare_Subprogram;

   ----------------------------------
   -- Declare_Subprogram_Parameter --
   ----------------------------------

   procedure Declare_Subprogram_Parameter
     (Parameter_Name  : in  Name_Id;
      Para_Type_Node  : in  Type_Id;
      Subprogram_Node : in  Subprogram_Id;
      Parameter_Node  : out Parameter_Id) is
      Node : Parameter_Id;
   begin

      Create_Parameter           (Node, Parameter_Name);
      Set_Parameter_Type         (Node, Para_Type_Node);
      Add_Subprogram_Parameter   (Subprogram_Node, Node);
      Parameter_Node := Node;

   end Declare_Subprogram_Parameter;

   ------------------
   -- Declare_Type --
   ------------------

   procedure Declare_Type
     (Conf_Node : in  Configuration_Id;
      Type_Name : in  Name_Id;
      Type_Kind : in  Predefined_Type;
      Structure : in  Boolean;
      Type_Node : out Type_Id) is
      T : Type_Id;
   begin

      Has_Not_Been_Already_Declared (Type_Name, Get_Token_Location);
      Create_Type         (T, Type_Name);
      Type_Is_A_Structure (T, Structure);
      Set_Type_Mark       (T, Convert (Type_Kind));
      Append_Declaration  (Conf_Node, Node_Id (T));
      Type_Node := T;

   end Declare_Type;

   ---------------------
   -- Declare_Literal --
   ---------------------

   procedure Declare_Literal
     (Literal_Name : in  Name_Id;
      Literal_Type : in  Type_Id;
      Literal_Node : out Variable_Id) is
      V : Variable_Id;
   begin

      Create_Variable    (V, Literal_Name);
      Set_Variable_Type  (V, Literal_Type);
      Literal_Node := V;

   end Declare_Literal;

   ----------------------
   -- Declare_Variable --
   ----------------------

   procedure Declare_Variable
     (Conf_Node     : in  Configuration_Id;
      Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Is_Unique     : in  Boolean;
      Variable_Sloc : in  Location_Type;
      Variable_Node : out Variable_Id) is
      V : Variable_Id;
   begin

      if Is_Unique then
         Has_Not_Been_Already_Declared (Variable_Name, Variable_Sloc);
      end if;
      Create_Variable    (V, Variable_Name);
      Append_Declaration (Conf_Node, Node_Id (V));
      Set_Variable_Type  (V, Variable_Type);
      Variable_Node := V;

   end Declare_Variable;

   -----------------------
   -- Declare_Attribute --
   -----------------------

   procedure Declare_Attribute
     (Variable_Node      : in Variable_Id;
      Attribute_Name     : in Name_Id;
      Attribute_Node     : out Attribute_Id) is
      A : Component_Id;
      T : Type_Id;
      K : Int;
      N : Name_Id;
   begin

      --  Attributes are always prefixed by Attribute_Prefix.

      N := Attribute_Prefix & Attribute_Name;

      --  Is this attribute a legal attribute for varaible type ?

      T := Get_Variable_Type (Variable_Node);
      Search_Component (N, T, A);
      if A = Null_Component then
         Write_Location (Get_Token_Location);
         Write_Str ("no such attribute for variable");
         Write_Eol;
         Exit_On_Parsing_Error;
      end if;

      --  Make a copy of the type attribute.

      T := Get_Component_Type   (A);
      K := Get_Component_Mark   (A);
      Create_Component          (A, N);
      Set_Component_Type        (A, T);
      Component_Is_An_Attribute (A, True);
      Add_Variable_Component    (Variable_Node, A);
      Set_Component_Mark        (Component_Id (A), K);
      Attribute_Node            := Attribute_Id (A);

   end Declare_Attribute;

   -----------------------
   -- Declare_Component --
   -----------------------

   procedure Declare_Component
     (Type_Node          : in Type_Id;
      Component_Name     : in Name_Id;
      Comp_Type_Node     : in Type_Id;
      Component_Node     : out Component_Id) is
      C : Component_Id;
   begin

      Create_Component        (C, Component_Name);
      Set_Component_Type      (C, Comp_Type_Node);
      Component_Is_An_Attribute (C, False);
      Add_Type_Component      (Type_Node, C);
      Component_Node          := C;

   end Declare_Component;

   -----------------------
   -- Declare_Attribute --
   -----------------------

   procedure Declare_Attribute
     (Type_Node          : in Type_Id;
      Attribute_Name     : in Name_Id;
      Attr_Type_Node     : in Type_Id;
      Attribute_Kind     : in Attribute_Type;
      Attribute_Node     : out Attribute_Id) is
      A : Attribute_Id;
   begin

      Declare_Component
        (Type_Node,
         Attribute_Prefix & Attribute_Name,
         Attr_Type_Node,
         Component_Id (A));
      Component_Is_An_Attribute (Component_Id (A), True);
      Set_Component_Mark        (Component_Id (A), Convert (Attribute_Kind));
      Attribute_Node := A;

   end Declare_Attribute;

   -----------------------
   -- Declare_Component --
   -----------------------

   procedure Declare_Component
     (Variable_Node      : in Variable_Id;
      Component_Name     : in Name_Id;
      Component_Type     : in Type_Id;
      Component_Value    : in Variable_Id;
      Is_An_Attribute    : in Boolean;
      Component_Node     : out Component_Id) is
      C : Component_Id;
   begin

      Create_Component        (C, Component_Name);
      Set_Component_Type      (C, Component_Type);
      Set_Component_Value     (C, Node_Id (Component_Value));
      Component_Is_An_Attribute (C, Is_An_Attribute);
      Add_Variable_Component  (Variable_Node, C);
      Component_Node := C;

   end Declare_Component;

   ----------------------------
   -- Declare_Procedure_Call --
   ----------------------------

   procedure Declare_Procedure_Call
     (Subprogram_Node : in Subprogram_Id) is
      New_Statement  : Statement_Id;
      Old_Subprogram : Subprogram_Id;
      New_Subprogram : Subprogram_Id;
      Old_Parameter  : Parameter_Id;
      New_Parameter  : Parameter_Id;
   begin

      Old_Subprogram := Subprogram_Node;

      --  As a parser naming convention, the keyword Procedure_Call
      --  indicates a procedure call.

      Create_Statement
        (New_Statement,
         Procedure_Call);

      --  Make a entire copy of subprogram node.

      Create_Subprogram
        (New_Subprogram,
         Get_Node_Name (Node_Id (Old_Subprogram)));
      Set_Subprogram_Mark
        (New_Subprogram,
         Get_Subprogram_Mark (Old_Subprogram));
      Subprogram_Is_A_Procedure
        (New_Subprogram,
         Is_Subprogram_A_Procedure (Old_Subprogram));

      --  ... even parameter nodes ...

      First_Subprogram_Parameter
        (Old_Subprogram,
         Old_Parameter);
      while Old_Parameter /= Null_Parameter loop

         --  Make a copy of parameters.

         Declare_Subprogram_Parameter
           (Get_Node_Name (Node_Id (Old_Parameter)),
            Get_Parameter_Type (Old_Parameter),
            New_Subprogram,
            New_Parameter);

         --  and assign the formal parameters as they were during
         --  the parameter matching phase.

         Assign_Variable
           (Get_Variable_Value (Variable_Id (Old_Parameter)),
            Variable_Id (New_Parameter));

         Set_Parameter_Mark (New_Parameter, 1);
         Next_Subprogram_Parameter (Old_Parameter);

      end loop;

      Set_Subprogram_Call (New_Statement, New_Subprogram);

      Append_Declaration
        (Configuration_Node,
         Node_Id (New_Statement));

   end Declare_Procedure_Call;

   ---------------------
   -- Assign_Variable --
   ---------------------

   procedure Assign_Variable
     (Source, Target : Variable_Id) is
      C : Component_Id;
      X : Component_Id;
      V : Variable_Id;
      T : Type_Id;
      N : Name_Id;
   begin
      pragma Assert (Get_Variable_Type (Source) = Get_Variable_Type (Target));

      T := Get_Variable_Type (Source);

      --  Do we need to assign a element list or a single element ?

      if Is_Type_A_Structure (T) then

         --  Assign a list ...

         First_Variable_Component (Source, C);

         --  As a naming convention, we use the keyword
         --  for a anonymous component name.

         N := Conf_Ada_Unit;
         while C /= Null_Component loop
            if not Is_Component_An_Attribute (C) then
               V := Variable_Id (Get_Component_Value (C));
               T := Get_Variable_Type (V);
               Declare_Component (Target, N, T, V, False, X);
            end if;
            Next_Variable_Component (C);
         end loop;

      --  Assign a single element.

      else
         Set_Variable_Value (Target, Source);
      end if;

   end Assign_Variable;

   ---------------------------
   -- Is_Expression_Of_Type --
   ---------------------------

   function Is_Expression_Of_Type
     (Expr_Node : in Node_Id;
      Type_Node : in Type_Id)
      return Boolean is
      P : Parameter_Id;
      C : Component_Id;
   begin

      --  If variable, check the variable type with the given type.

      if Is_Variable (Expr_Node) then
         return Get_Variable_Type (Variable_Id (Expr_Node)) = Type_Node;

      elsif Is_Subprogram (Expr_Node) then

         --  If Expr_Node is a function, check returned parameter.

         if not Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
           Convert (Get_Type_Mark (Type_Node)) /= Pre_Type_Function then
            Search_Function_Returned_Parameter (Subprogram_Id (Expr_Node), P);
            return Get_Parameter_Type (P) = Type_Node;

         end if;

         --  Is the expression a function when the type is a function type
         --  or a procedure when the type is a procedure type ?

         if (Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
             Convert (Get_Type_Mark (Type_Node)) /= Pre_Type_Procedure) or else
            (not Is_Subprogram_A_Procedure (Subprogram_Id (Expr_Node)) and then
             Convert (Get_Type_Mark (Type_Node)) /= Pre_Type_Function) then
            return False;
         end if;

         --  Check also type of parameters ...

         First_Type_Component (Type_Node, C);
         First_Subprogram_Parameter (Subprogram_Id (Expr_Node), P);
         while C /= Null_Component and then P /= Null_Parameter loop
            if Get_Component_Type (C) /= Get_Parameter_Type (P) then
               return False;
            end if;
            Next_Type_Component (C);
            Next_Subprogram_Parameter (P);
         end loop;

         --  Check we have the same number of parameters ...

         return C = Null_Component and P = Null_Parameter;

      else
         return False;
      end if;

   end Is_Expression_Of_Type;

   -----------------------------------
   -- Has_Not_Been_Already_Declared --
   -----------------------------------

   procedure Has_Not_Been_Already_Declared
     (Declaration_Name : in Name_Id;
      Declaration_Sloc : in Location_Type) is
      Node : Node_Id;
   begin
      Search_Declaration (Declaration_Name, Node);
      if Node = Null_Node then
         return;
      elsif Is_Variable (Node) and then
         Convert (Get_Type_Mark (Get_Variable_Type (Variable_Id (Node)))) /=
         Pre_Type_Unknown then
         return;
      end if;
      Write_Location (Declaration_Sloc);
      Write_Str  ("""");
      Write_Name (Declaration_Name);
      Write_Str  (""" conflicts with a previous declaration");
      Write_Eol;
      Exit_On_Parsing_Error;
   end Has_Not_Been_Already_Declared;

   ----------------------------------------
   -- Search_Function_Returned_Parameter --
   ----------------------------------------

   procedure Search_Function_Returned_Parameter
     (Function_Node  : in Subprogram_Id;
      Parameter_Node : out Parameter_Id) is
      Prev, Next : Parameter_Id;
   begin
      pragma Assert (not Is_Subprogram_A_Procedure (Function_Node));

      --  Basically, look for the last parameter.
      Prev := Null_Parameter;
      First_Subprogram_Parameter (Function_Node, Next);
      while Next /= Null_Parameter loop
         Prev := Next;
         Next_Subprogram_Parameter (Next);
      end loop;
      Parameter_Node := Parameter_Id (Prev);

   end Search_Function_Returned_Parameter;

   -----------------------------
   -- Search_Actual_Parameter --
   -----------------------------

   procedure Search_Actual_Parameter
     (Actual_Name : in  Name_Id;
      Actual_Type : in  Type_Id;
      Actual_Node : out Variable_Id) is
      Actual : Node_Id;
   begin

      --  Scan the configuration to retrieve a possible variable of
      --  name Actual_Name ...

      First_Configuration_Declaration (Configuration_Node, Actual);
      while Actual /= Null_Node loop
         if Is_Variable       (Actual) and then
            Get_Node_Name     (Actual) = Actual_Name and then
            Get_Variable_Type (Variable_Id (Actual)) = Actual_Type then
            Actual_Node := Variable_Id (Actual);
            return;
         end if;
         Next_Configuration_Declaration (Actual);
      end loop;

      Write_Location (Get_Token_Location);
      Write_Str  ("identifier """);
      Write_Name (Actual_Name);
      Write_Str  (""" is undefined here");
      Write_Eol;
      Exit_On_Parsing_Error;

   end Search_Actual_Parameter;

   ------------------------
   -- Search_Declaration --
   ------------------------

   procedure Search_Declaration
     (Declaration_Name : in Name_Id;
      Declaration_Node : out Node_Id) is
      Node : Node_Id;
      Name : Name_Id;
   begin

      First_Configuration_Declaration (Configuration_Node, Node);
      while Node /= Null_Node loop
         Name := Get_Node_Name (Node);
         exit when Name = Declaration_Name;
         Next_Configuration_Declaration (Node);
      end loop;
      Declaration_Node := Node;

   end Search_Declaration;

   ----------------------
   -- Search_Component --
   ----------------------

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Type_Node      : in  Type_Id;
      Component_Node : out Component_Id) is
      C : Component_Id;
   begin

      First_Type_Component (Type_Node, C);
      while C /= Null_Component loop
         exit when Get_Node_Name (Node_Id (C)) = Component_Name;
         Next_Type_Component (C);
      end loop;
      Component_Node := C;

   end Search_Component;

   ----------------------
   -- Search_Component --
   ----------------------

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Variable_Node  : in  Variable_Id;
      Component_Node : out Component_Id) is
      C : Component_Id;
   begin

      First_Variable_Component (Variable_Node, C);
      while C /= Null_Component loop
         exit when Get_Node_Name (Node_Id (C)) = Component_Name;
         Next_Variable_Component (C);
      end loop;
      Component_Node := C;

   end Search_Component;

   ---------------------
   -- Search_Variable --
   ---------------------

   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Node : out Variable_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Variable_Name, Node);
      if Node /= Null_Node  and then
         not Is_Variable (Node) then
         Node := Null_Node;
      end if;
      Variable_Node := Variable_Id (Node);

   end Search_Variable;

   -----------------------
   -- Search_Subprogram --
   -----------------------

   procedure Search_Subprogram
     (Subprogram_Name : in  Name_Id;
      Subprogram_Node : out Subprogram_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Subprogram_Name, Node);
      if Node /= Null_Node and then
         not Is_Subprogram (Node) then
         Node := Null_Node;
      end if;
      Subprogram_Node := Subprogram_Id (Node);

   end Search_Subprogram;

   -------------------
   -- Search_Pragma --
   -------------------

   procedure Search_Pragma
     (Pragma_Name : in  Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id) is
      Node : Subprogram_Id;
   begin

      Search_Subprogram (Pragma_Name, Node);
      if Node = Null_Subprogram then
         Pragma_Kind := Pragma_Unknown;
      else
         Pragma_Kind := Convert (Get_Subprogram_Mark (Node));
      end if;
      Pragma_Node := Node;

   end Search_Pragma;

   -----------------
   -- Search_Type --
   -----------------

   procedure Search_Type
     (Type_Name : in  Name_Id;
      Type_Kind : out Predefined_Type;
      Type_Node : out Type_Id) is
      Node : Node_Id;
   begin

      Search_Declaration (Type_Name, Node);
      if Node /= Null_Node and then
         not Is_Type (Node) then
         Node := Null_Node;
      end if;
      Type_Node := Type_Id (Node);
      if Node /= Null_Node then
         Type_Kind := Convert (Get_Type_Mark (Type_Id (Node)));
      else
         Type_Kind := Pre_Type_Unknown;
      end if;

   end Search_Type;

   ----------------------------------
   -- Unmark_Subprogram_Parameters --
   ----------------------------------

   procedure Unmark_Subprogram_Parameters
     (Subprogram_Node : Subprogram_Id;
      N_Parameter     : out Int) is
      Count : Int    := 0;
      Parameter_Node  : Parameter_Id;
   begin

      First_Subprogram_Parameter (Subprogram_Node, Parameter_Node);
      while Parameter_Node /= Null_Parameter loop
         Set_Parameter_Mark (Parameter_Node, 0);
         Count := Count + 1;
         Next_Subprogram_Parameter (Parameter_Node);
      end loop;
      N_Parameter := Count;

   end Unmark_Subprogram_Parameters;

   -------------------------------
   -- Search_Matching_Parameter --
   -------------------------------

   procedure Search_Matching_Parameter
     (Subprogram_Node : Subprogram_Id;
      Convention      : Convention_Type;
      Formal_Name     : in out Name_Id;
      Parameter_Node  : in out Parameter_Id) is
   begin

      First_Subprogram_Parameter (Subprogram_Node, Parameter_Node);
      while Parameter_Node /= Null_Parameter loop
         case Convention is

            --  If Positional, find the first unmarked parameter.
            when Positional =>
               if Get_Parameter_Mark (Parameter_Node) = 0 then
                  Formal_Name := Get_Node_Name (Node_Id (Parameter_Node));
                  return;
               end if;

            --  If Named, use Formal_Name to return format parameter node.
            when Named =>
               if Get_Node_Name (Node_Id (Parameter_Node)) = Formal_Name then
                  return;
               end if;

         end case;
         Next_Subprogram_Parameter (Parameter_Node);
      end loop;

      Write_Location (Get_Token_Location);
      Write_Str  ("no matching parameter");
      Write_Eol;
      Exit_On_Parsing_Error;

   end Search_Matching_Parameter;

   ---------------------------
   -- Exit_On_Parsing_Error --
   ---------------------------

   procedure Exit_On_Parsing_Error is
   begin
      Print_Configuration;
      raise Parsing_Error;
   end Exit_On_Parsing_Error;

   -------------------------
   -- Print_Configuration --
   -------------------------

   procedure Print_Configuration is
      Node : Node_Id;
   begin
      if Debug_Mode then
         Write_Eol;
         Write_Str ("configuration");
         Write_Eol;
         Write_Str ("=============");
         Write_Eol;
         Write_Eol;
         First_Configuration_Declaration (Configuration_Node, Node);
         Print (Node, "");
      end if;
   end Print_Configuration;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Variable_Id;
      Head : in String) is
      T : Type_Id;
      C : Component_Id;
      H : String (1 .. Head'Length + 6) := (others => ' ');
   begin
      Write_Str  (Head);
      Write_Str  ("Type : ");
      T := Get_Variable_Type (Node);
      Write_Name (Get_Node_Name (Node_Id (T)));
      Write_Eol;
      Write_Str  (Head);
      Write_Str  ("Mark : ");
      Write_Int (Get_Variable_Mark (Node));
      Write_Eol;
      if Is_Type_A_Structure (T) then
         Write_Str  (Head);
         Write_Str  ("    Data :");
         Write_Eol;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            Print (C, H, True);
            Next_Variable_Component (C);
         end loop;
         Write_Str  (Head);
         Write_Str  ("    Attr :");
         Write_Eol;
         First_Variable_Component (Node, C);
         while C /= Null_Component loop
            Print (C, H, False);
            Next_Variable_Component (C);
         end loop;
      elsif Get_Variable_Value (Node) /= Null_Variable then
         Write_Str  (Head);
         Write_Str  ("    Data :");
         Write_Eol;
         Write_Str  (Head);
         Write_Str  ("       ");
         Write_Name (Get_Node_Name (Node_Id (Get_Variable_Value (Node))));
         Write_Eol;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Type_Id;
      Head : in String) is
      C : Component_Id;
      H : String (1 .. Head'Length + 6) := (others => ' ');
   begin
      Write_Str  ("Mark : ");
      Write_Int (Get_Type_Mark (Node));
      Write_Eol;
      First_Type_Component (Node, C);
      if C /= Null_Component then
         Write_Str  (Head);
         Write_Str  ("   Attr : ");
         Write_Eol;
         while C /= Null_Component loop
            Print (C, H, False);
            Next_Type_Component (C);
         end loop;
      end if;
      First_Type_Component (Node, C);
      if C /= Null_Component then
         Write_Str  (Head);
         Write_Str  ("   Data : ");
         Write_Eol;
         while C /= Null_Component loop
            Print (C, H, True);
            Next_Type_Component (C);
         end loop;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Component_Id;
      Head : in String;
      Data : in Boolean) is
      Item  : Node_Id;
      Value : Node_Id;
   begin
      if not Data and then Is_Component_An_Attribute (Node) then
         Write_Str  (Head);
         Write_Name (Get_Node_Name (Node_Id (Node)));
         Write_Str  (" : ");
         Item := Node_Id (Get_Component_Type (Node));
         Write_Name (Get_Node_Name (Item));
         Value := Get_Component_Value (Node);
         if Value /= Null_Node then
            Write_Eol;
            Write_Str (Head);
            Write_Str ("   = (");
            Write_Name (Get_Node_Name (Node_Id (Value)));
            Write_Str  (", ");
            if Is_Subprogram (Value) then
               Write_Int  (Get_Subprogram_Mark (Subprogram_Id (Value)));
            elsif Is_Variable (Value) then
               Write_Int  (Get_Variable_Mark (Variable_Id (Value)));
            else
               Write_Str ("<none>");
            end if;
            Write_Str (")");
         end if;
         Write_Eol;
      elsif Data and then not Is_Component_An_Attribute (Node) then
         Write_Str  (Head);
         Write_Name (Get_Node_Name (Node_Id (Node)));
         Write_Str  (" : ");
         Item := Get_Component_Value (Node);
         Write_Name (Get_Node_Name (Item));
         Write_Eol;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Parameter_Id;
      Head : in String) is
      Item  : Type_Id;
   begin
      Write_Str  (Head);
      Write_Name (Get_Node_Name (Node_Id (Node)));
      Write_Str  (" : ");
      Item := Get_Parameter_Type (Node);
      Write_Name (Get_Node_Name (Node_Id (Item)));
      if Get_Parameter_Mark (Node) /= 0 then
         declare
            H : String (1 .. Head'Length + 3) := (others => ' ');
            V : Variable_Id;
         begin
            V := Get_Variable_Value (Variable_Id (Node));
            Write_Str (" := ");
            Write_Name (Get_Node_Name (Node_Id (V)));
         end;
      end if;
      Write_Eol;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Subprogram_Id;
      Head : in String) is
      H : String (1 .. Head'Length + 6) := (others => ' ');
      P : Parameter_Id;
   begin
      Write_Str (Head);
      Write_Str  ("    Mark : ");
      Write_Int (Get_Subprogram_Mark (Node));
      Write_Eol;
      Write_Str (Head);
      Write_Str  ("    Proc : ");
      if Is_Subprogram_A_Procedure (Node) then
         Write_Str ("true");
      else
         Write_Str ("false");
      end if;
      Write_Eol;
      Write_Str  (Head);
      Write_Str  ("    Para :");
      Write_Eol;
      First_Subprogram_Parameter (Node, P);
      while P /= Null_Parameter loop
         Print (P, H);
         Next_Subprogram_Parameter (P);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Statement_Id;
      Head : in String) is
      S : Subprogram_Id;
   begin
      S := Get_Subprogram_Call (Node);
      Write_Str  (Head);
      Write_Str  ("Call : ");
      Write_Name (Get_Node_Name (Node_Id (S)));
      Write_Eol;
      Print (S, Head);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Configuration_Id;
      Head : in String) is
   begin
      null;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node : in Node_Id;
      Head : in String) is
      Next : Node_Id;
   begin
      Write_Str  (Head);
      Write_Str  ("Name : ");
      Write_Name (Get_Node_Name (Node));
      Write_Eol;
      Write_Str (Head);
      Write_Str ("Kind : ");
      if Is_Variable (Node) then
         Write_Str ("variable");
         Write_Eol;
         Print (Variable_Id (Node), Head);
      elsif Is_Type (Node) then
         Write_Str ("type");
         Write_Eol;
         Print (Type_Id (Node), Head);
      elsif Is_Subprogram (Node) then
         Write_Str ("subprogram");
         Write_Eol;
         Print (Subprogram_Id (Node), Head);
      elsif Is_Configuration (Node) then
         Write_Str ("configuration");
         Write_Eol;
         Print (Configuration_Id (Node), Head);
      elsif Is_Statement (Node) then
         Write_Str ("statement");
         Write_Eol;
         Print (Statement_Id (Node), Head);
      else
         Write_Str ("unknown");
         Write_Eol;
      end if;
      Write_Eol;
      Next := Node;
      Next_Configuration_Declaration (Next);
      if Next /= Null_Node then
         Print (Next, Head);
      end if;
   end Print;

end XE_Parse;
