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
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with Types;
with XE;
with XE_Scan;

package XE_Parse is

   type Convention_Type is (Named, Positional);

   Attribute_Prefix : Types.Name_Id;
   Type_Prefix      : Types.Name_Id;
   Pragma_Prefix    : Types.Name_Id;

   Unbounded        : constant Types.Int := Types.Int'Last;

   procedure Check_Not_Declared
     (Declaration_Name : in Types.Name_Id;
      Declaration_Sloc : in XE_Scan.Location_Type);
   --  Check that such a declaration has not already been done

   procedure Declare_Literal
     (Literal_Name : in  Types.Name_Id;
      Literal_Type : in  XE.Type_Id;
      Literal_Sloc : in  XE_Scan.Location_Type;
      Literal_Node : out XE.Variable_Id);

   procedure Declare_Procedure_Call
     (Subprogram_Node : in XE.Subprogram_Id);
   --  Declare a call to a procedure. A statement node is created and
   --  contains an entire copy of the subprogram node.

   procedure Declare_Subprogram
     (Subprogram_Name  : in  Types.Name_Id;
      Pragma_Kind      : in  XE.Pragma_Type;
      Is_A_Procedure   : in  Boolean;
      Subprogram_Sloc  : in  XE_Scan.Location_Type;
      Subprogram_Node  : out XE.Subprogram_Id);
   --  Declare a subprogram into the configuration context. This subprogram
   --  is possibly a function. At this point, the subprogram has no
   --  parameter.

   procedure Declare_Subprogram_Parameter
     (Parameter_Name  : in  Types.Name_Id;
      Para_Type_Node  : in  XE.Type_Id;
      Subprogram_Node : in  XE.Subprogram_Id;
      Parameter_Sloc  : in  XE_Scan.Location_Type;
      Parameter_Node  : out XE.Parameter_Id);
   --  Declare a parameter for a subprogram. The last parameter corresponds
   --  to a returned value when the subprogram is a function.

   procedure Declare_Type
     (Type_Name : in  Types.Name_Id;
      Type_Kind : in  XE.Predefined_Type;
      List_Size : in  Types.Int;
      Comp_Type : in  XE.Type_Id;
      Is_Frozen : in  Boolean;
      Type_Sloc : in  XE_Scan.Location_Type;
      Type_Node : out XE.Type_Id);
   --  Declare a new type into the configuration context. If List_Size is
   --  zero, it is non component list type. If List_Size is Unbounded, then
   --  it is an unbounded array. Otherwise, it is a component list.

   procedure Declare_Type_Attribute
     (Type_Node          : in XE.Type_Id;
      Attribute_Name     : in Types.Name_Id;
      Attr_Type_Node     : in XE.Type_Id;
      Attribute_Kind     : in XE.Attribute_Type;
      Attribute_Sloc     : in XE_Scan.Location_Type;
      Attribute_Node     : out XE.Attribute_Id);
   --  Declare an attribute for a given type. This procedure creates a
   --  component of type Attr_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Type_Component
     (Type_Node          : in XE.Type_Id;
      Component_Name     : in Types.Name_Id;
      Comp_Type_Node     : in XE.Type_Id;
      Component_Sloc     : in XE_Scan.Location_Type;
      Component_Node     : out XE.Component_Id);
   --  Declare a component for a given type. This procedure creates a
   --  component of type Comp_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Variable
     (Variable_Name : in  Types.Name_Id;
      Variable_Type : in  XE.Type_Id;
      Variable_Sloc : in  XE_Scan.Location_Type;
      Variable_Node : out XE.Variable_Id);
   --  Declare a new variable into the configuration context. This variable
   --  of name Variable_Name is of type Variable_Type. Allocate the
   --  component nodes if needed (not attributes).

   procedure Declare_Variable_Component
     (Variable_Node      : in  XE.Variable_Id;
      Component_Name     : in  Types.Name_Id;
      Component_Type     : in  XE.Type_Id;
      Component_Value    : in  XE.Variable_Id;
      Attribute_Kind     : in  XE.Attribute_Type;
      Component_Sloc     : in  XE_Scan.Location_Type;
      Component_Node     : out XE.Component_Id);
   --  Add a component for a given variable. This component is possibly an
   --  attribute and is initialized to Component_Value.  The component type
   --  is given by Component_Type.

   procedure Duplicate_Component
     (Source : in  XE.Component_Id;
      Target : out XE.Component_Id);
   --  Duplicate component, attribute or not, but do not duplicate
   --  component value.

   procedure Duplicate_Variable
     (Source, Target : in XE.Variable_Id);
   --  Duplicate all the content except attributes

   procedure Exit_On_Parsing_Error;
   --  Print configuration if verbose_mode and then raise Parsing_Error

   procedure Initialize;
   --  Elaboration code

   procedure Match_Actual_With_Formal
     (Subprogram_Node : in XE.Subprogram_Id);
   --  Parse a subprogram call and associate actual parameters to formal
   --  parameters.

   procedure P_Aggregate_Assignement
     (Variable_Node   : in XE.Variable_Id);
   --  Parse an aggregate assignement

   procedure P_Configuration_Body;

   procedure P_Configuration_Declaration;

   procedure P_Configuration_End;

   procedure P_Full_Ada_Identifier;

   procedure P_Function_Declaration;

   procedure P_Pragma;

   procedure P_Procedure_Declaration;

   procedure P_Representation_Clause;

   procedure P_Variable_List_Declaration
     (Previous_Name   : in Types.Name_Id;
      Previous_Sloc   : in XE_Scan.Location_Type);
   --  Parse a list of identifiers

   procedure Parse;
   --  Main procedure

   procedure Print;
   --  Print node tree for debugging purpose. The global variable
   --  Configuration_Node is used as tree root.

   procedure Print_Component
     (Node : in XE.Component_Id;
      Many : in Types.Int);

   procedure Print_Parameter
     (Node : in XE.Parameter_Id;
      Many : in Types.Int);

   procedure Print_Statement
     (Node : in XE.Statement_Id;
      Many : in Types.Int);

   procedure Print_Subprogram
     (Node : in XE.Subprogram_Id;
      Many : in Types.Int);

   procedure Print_Type
     (Node : in XE.Type_Id;
      Many : in Types.Int);

   procedure Print_Variable
     (Node : in XE.Variable_Id;
      Many : in Types.Int);

   procedure Search_Actual_Parameter
     (Actual_Name : in  Types.Name_Id;
      Actual_Type : in  XE.Type_Id;
      Actual_Node : out XE.Variable_Id);
   --  Similar to Search_Variable but check name *and* type

   procedure Search_Component
     (Component_Name : in  Types.Name_Id;
      Type_Node      : in  XE.Type_Id;
      Component_Node : out XE.Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  type Type_Node. If unsuccessful, returns Null_Component.

   procedure Search_Component
     (Component_Name : in  Types.Name_Id;
      Variable_Node  : in  XE.Variable_Id;
      Component_Node : out XE.Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  variable Variable_Node. If unsuccessful, returns Null_Component.

   procedure Search_Declaration
     (Declaration_Name : in  Types.Name_Id;
      Declaration_Node : out XE.Node_Id);
   --  Search for the first occurrence of a declaration
   --  Declaration_Name. If unsuccessful, returns Null_Node.

   procedure Search_Function_Returned_Parameter
     (Function_Node  : in  XE.Subprogram_Id;
      Parameter_Node : out XE.Parameter_Id);
   --  Search for the last parameter of this subprogram. This is by
   --  convention the returned parameter.

   procedure Search_Matching_Parameter
     (Subprogram_Node : in     XE.Subprogram_Id;
      Convention      : in     Convention_Type;
      Formal_Name     : in out Types.Name_Id;
      Formal_Type     : in out XE.Type_Id;
      Parameter_Node  : in out XE.Parameter_Id);
   --  Search for a formal parameter that has no actual associated
   --  parameter. This choice should follow Convention requirements. If
   --  Convention is Named, then returns Parameter_Node of name
   --  Formal_Name. If is Positional, returns the next unmatched parameter
   --  and returns also its name in Formal_Name.

   procedure Search_Pragma
     (Pragma_Name : in  Types.Name_Id;
      Pragma_Kind : out XE.Pragma_Type;
      Pragma_Node : out XE.Subprogram_Id);
   --  Search for the first occurrence of a pragma Pragma_Name. If
   --  unsuccessful, returns Null_Pragma. If successful, Pragma_Kind is set
   --  to its corresponding litteral. Otherwise, it is set to
   --  Pragma_Unknown.

   procedure Search_Subprogram
     (Subprogram_Name : in  Types.Name_Id;
      Subprogram_Node : out XE.Subprogram_Id);
   --  Search for the first occurrence of a subprogram Subprogram_Name. If
   --  unsuccessful, returns Null_Subprogram.

   procedure Search_Type
     (Type_Name : in  Types.Name_Id;
      Type_Kind : out XE.Predefined_Type;
      Type_Node : out XE.Type_Id);
   --  Search for the first occurrence of a type Type_Name. If
   --  unsuccessful, returns Null_Type. If successful, Type_Kind is set to
   --  its corresponding litteral. Otherwise, it is set to
   --  Pre_Type_Unknown.

   procedure Search_Uninitialized_Component
     (Variable_Node  : in  XE.Variable_Id;
      Component_Type : in  XE.Type_Id;
      Component_Node : out XE.Component_Id);
   --  Search for the first occurrence of an uninitialized component in a
   --  variable Variable_Node. Attributes are discarded. If unsuccessful,
   --  returns Null_Component.

   procedure Search_Variable
     (Variable_Name : in  Types.Name_Id;
      Variable_Node : out XE.Variable_Id);
   --  Search for the first occurrence of a variable Variable_Name. If
   --  unsuccessful, returns Null_Variable.

   procedure Search_Variable
     (Variable_Name : in  Types.Name_Id;
      Variable_Type : in  XE.Type_Id;
      Variable_Node : out XE.Variable_Id);
   --  Search for the first occurrence of a variable Variable_Name. If
   --  unsuccessful, returns Null_Variable. Check Varaible_Type also.

   procedure Set_Node_Location
     (Node     : in XE.Node_Id;
      Location : in XE_Scan.Location_Type);
   --  Set SLOC node to Location

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

   procedure Take_Token (T : in XE.Token_Type);

   procedure Take_Token (L : in XE.Token_List_Type);

end XE_Parse;
