------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with XE;       use XE;
with XE_Scan;  use XE_Scan;
with XE_Utils; use XE_Utils;

package XE_Parse is

   type Convention_Type is (Named, Positional);

   Attribute_Prefix : Name_Id;
   Type_Prefix      : Name_Id;
   Pragma_Prefix    : Name_Id;

   Unbounded        : constant Int     := Int'Last;

   procedure Check_Not_Declared
     (Declaration_Name : in Name_Id;
      Declaration_Sloc : in Location_Type);
   --  Check that such a declaration has not already been done.

   procedure Declare_Literal
     (Literal_Name : in  Name_Id;
      Literal_Type : in  Type_Id;
      Literal_Sloc : in  Location_Type;
      Literal_Node : out Variable_Id);
   --  Declare a new literal.

   procedure Declare_Procedure_Call
     (Subprogram_Node : in Subprogram_Id);
   --  Declare a call to a procedure. A statement node is created and
   --  contains an entire copy of the subprogram node.

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
      Parameter_Sloc  : in  Location_Type;
      Parameter_Node  : out Parameter_Id);
   --  Declare a parameter for a subprogram. The last parameter corresponds
   --  to a returned value when the subprogram is a function.

   procedure Declare_Type
     (Type_Name : in  Name_Id;
      Type_Kind : in  Predefined_Type;
      List_Size : in  Int;
      Comp_Type : in  Type_Id;
      Is_Frozen : in  Boolean;
      Type_Sloc : in  Location_Type;
      Type_Node : out Type_Id);
   --  Declare a new type into the configuration context. If List_Size is
   --  zero, it is non component list type. If List_Size is Unbounded, then
   --  it is an unbounded array. Otherwise, it is a component list.

   procedure Declare_Type_Attribute
     (Type_Node          : in Type_Id;
      Attribute_Name     : in Name_Id;
      Attr_Type_Node     : in Type_Id;
      Attribute_Kind     : in Attribute_Type;
      Attribute_Sloc     : in Location_Type;
      Attribute_Node     : out Attribute_Id);
   --  Declare an attribute for a given type. This procedure creates a
   --  component of type Attr_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Type_Component
     (Type_Node          : in Type_Id;
      Component_Name     : in Name_Id;
      Comp_Type_Node     : in Type_Id;
      Component_Sloc     : in Location_Type;
      Component_Node     : out Component_Id);
   --  Declare a component for a given type. This procedure creates a
   --  component of type Comp_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Variable
     (Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Variable_Sloc : in  Location_Type;
      Variable_Node : out Variable_Id);
   --  Declare a new variable into the configuration context. This variable
   --  of name Variable_Name is of type Variable_Type.

   procedure Declare_Variable_Component
     (Variable_Node      : in Variable_Id;
      Component_Name     : in Name_Id;
      Component_Type     : in Type_Id;
      Component_Value    : in Variable_Id;
      Attribute_Kind     : in Attribute_Type;
      Component_Sloc     : in Location_Type;
      Component_Node     : out Component_Id);
   --  Add a component for a given variable. This component is possibly an
   --  attribute and is initialized to Component_Value.  The component type
   --  is given by Component_Type.

   procedure Duplicate_Component
     (Source : in Component_Id;
      Target : out Component_Id);
   --  Duplicate component, attribute or not, but do not duplicate
   --  component value.

   procedure Duplicate_Variable
     (Source, Target : in Variable_Id);
   --  Duplicate all the content except attributes.

   procedure Exit_On_Parsing_Error;
   --  Print configuration if verbose_mode and then raise Parsing_Error.

   procedure Initialize;
   --  Elaboration code.

   function Is_Expression_Of_Type
     (Expr_Node : Node_Id;
      Type_Node : Type_Id)
      return Boolean;
   --  When Expr_Node is a variable, compares the given type and the
   --  variable type. When Expr_Node is a function, compares the given type
   --  and the type of the returned parameter.

   function  Match    (L : Token_List_Type) return Boolean;

   procedure Match_Actual_With_Formal
     (Subprogram_Node : in Subprogram_Id);
   --  Parse a subprogram call and associate actual parameters to formal
   --  parameters.

   procedure No_Match (L : in Token_List_Type);

   procedure No_Match (T : in Token_Type);
   --  Utilities.

   procedure P_Aggregate_Assignement
     (Variable_Node   : in Variable_Id);
   --  Parse an aggregat assignement.

   procedure P_Configuration_Body;

   procedure P_Configuration_Declaration;

   procedure P_Configuration_End;

   procedure P_Full_Ada_Identifier;

   procedure P_Function_Declaration;

   procedure P_Pragma;

   procedure P_Procedure_Declaration;

   procedure P_Representation_Clause;

   procedure P_Variable_List_Declaration
     (Previous_Name   : in Name_Id;
      Previous_Sloc   : in Location_Type);
   --  Parse a list of identifiers.

   procedure Parse;
   --  Main procedure.

   procedure Print
     (Node : in Node_Id;
      Head : in String);
   --  Print any node with string Head at the beginning of each line.

   procedure Print
     (Node : in Type_Id;
      Head : in String);
   --  Print a type node with its component and attributes.

   procedure Print
     (Node : in Variable_Id;
      Head : in String);
   --  Print a variable node with its values and its attributes.

   procedure Print
     (Node : in Parameter_Id;
      Head : in String);
   --  Print a parameter node with its value.

   procedure Print
     (Node : in Component_Id;
      Head : in String;
      Attr : in Boolean);
   --  Print a component node. If Attr is true, print only attribute
   --  components.  If not, print only standard components.

   procedure Print
     (Node : in Subprogram_Id;
      Head : in String);
   --  Print a subprogram node with its formal parameter list.

   procedure Print
     (Node : in Statement_Id;
      Head : in String);
   --  Print a statement node.

   procedure Print
     (Node : in Configuration_Id;
      Head : in String);
   --  Print a configuration node.

   procedure Print;
   --  Print node tree for debugging purpose. The global variable
   --  Configuration_Node is used as tree root.

   procedure Search_Actual_Parameter
     (Actual_Name : in  Name_Id;
      Actual_Type : in  Type_Id;
      Actual_Node : out Variable_Id);
   --  Similar to Search_Variable but check name *and* type.

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Type_Node      : in  Type_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  type Type_Node. If unsuccessful, returns Null_Component.

   procedure Search_Component
     (Component_Name : in  Name_Id;
      Variable_Node  : in  Variable_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  variable Variable_Node. If unsuccessful, returns Null_Component.

   procedure Search_Declaration
     (Declaration_Name : in  Name_Id;
      Declaration_Node : out Node_Id);
   --  Search for the first occurrence of a declaration
   --  Declaration_Name. If unsuccessful, returns Null_Node.

   procedure Search_Function_Returned_Parameter
     (Function_Node  : in Subprogram_Id;
      Parameter_Node : out Parameter_Id);
   --  Search for the last parameter of this subprogram. This is by
   --  convention the returned parameter.

   procedure Search_Matching_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Convention      : in Convention_Type;
      Formal_Name     : in out Name_Id;
      Formal_Type     : in out Type_Id;
      Parameter_Node  : in out Parameter_Id);
   --  Search for a formal parameter that has no actual associated
   --  parameter. This choice should follow Convention requirements. If
   --  Convention is Named, then returns Parameter_Node of name
   --  Formal_Name. If is Positional, returns the next unmatched parameter
   --  and returns also its name in Formal_Name.

   procedure Search_Pragma
     (Pragma_Name : in  Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id);
   --  Search for the first occurrence of a pragma Pragma_Name. If
   --  unsuccessful, returns Null_Pragma. If successful, Pragma_Kind is set
   --  to its corresponding litteral. Otherwise, it is set to
   --  Pragma_Unknown.

   procedure Search_Subprogram
     (Subprogram_Name : in  Name_Id;
      Subprogram_Node : out Subprogram_Id);
   --  Search for the first occurrence of a subprogram Subprogram_Name. If
   --  unsuccessful, returns Null_Subprogram.

   procedure Search_Type
     (Type_Name : in  Name_Id;
      Type_Kind : out Predefined_Type;
      Type_Node : out Type_Id);
   --  Search for the first occurrence of a type Type_Name. If
   --  unsuccessful, returns Null_Type. If successful, Type_Kind is set to
   --  its corresponding litteral. Otherwise, it is set to
   --  Pre_Type_Unknown.

   procedure Search_Uninitialized_Component
     (Variable_Node  : in  Variable_Id;
      Component_Type : in  Type_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of an uninitialized component in a
   --  variable Variable_Node. Attributes are discarded. If unsuccessful,
   --  returns Null_Component.

   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Node : out Variable_Id);
   --  Search for the first occurrence of a variable Variable_Name. If
   --  unsuccessful, returns Null_Variable.

   procedure Search_Variable
     (Variable_Name : in  Name_Id;
      Variable_Type : in  Type_Id;
      Variable_Node : out Variable_Id);
   --  Search for the first occurrence of a variable Variable_Name. If
   --  unsuccessful, returns Null_Variable. Check Varaible_Type also.

   procedure Set_Node_Location
     (Node     : in Node_Id;
      Location : in Location_Type);
   --  Set SLOC node to Location.

   procedure T_Apostrophe;

   procedure T_Arrow;

   procedure T_Colon;

   procedure T_Colon_Equal;

   procedure T_Comma;

   procedure T_Configuration;

   procedure T_Dot;

   procedure T_End;

   procedure T_EOF;

   procedure T_For;

   procedure T_Function;

   procedure T_Identifier;

   procedure T_In;

   procedure T_Is;

   procedure T_Left_Paren;

   procedure T_Pragma;

   procedure T_Procedure;

   procedure T_Return;

   procedure T_Right_Paren;

   procedure T_Semicolon;

   procedure T_String_Literal;

   procedure T_Use;

   procedure Take_Token (T : in Token_Type);

   procedure Take_Token (L : in Token_List_Type);

end XE_Parse;
