------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ P A R S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2006 Free Software Foundation, Inc.           --
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

--  This package contains routines to parse the configuration file.

with XE;       use XE;
with XE_Scan;  use XE_Scan;
with XE_Types; use XE_Types;

package XE_Parse is

   type Convention_Type is (Named, Positional);

   Attribute_Prefix : Name_Id;
   Type_Prefix      : Name_Id;
   Pragma_Prefix    : Name_Id;

   procedure Check_Not_Declared
     (Declaration_Name : Name_Id;
      Declaration_Sloc : XE_Scan.Location_Type);
   --  Check that such a declaration has not already been done

   procedure Declare_Literal
     (Literal_Name : Name_Id;
      Literal_Type : Type_Id;
      Literal_Sloc : XE_Scan.Location_Type;
      Literal_Node : out Variable_Id);

   procedure Declare_Procedure_Call
     (Subprogram_Node : Subprogram_Id;
      Subprogram_Sloc : XE_Scan.Location_Type);
   --  Declare a call to a procedure. A statement node is created and
   --  contains an entire copy of the subprogram node.

   procedure Declare_Subprogram
     (Subprogram_Name  : Name_Id;
      Pragma_Kind      : Pragma_Type;
      Is_A_Procedure   : Boolean;
      Subprogram_Sloc  : XE_Scan.Location_Type;
      Subprogram_Node  : out Subprogram_Id);
   --  Declare a subprogram into the configuration context. This subprogram
   --  is possibly a function. At this point, the subprogram has no
   --  parameter.

   procedure Declare_Subprogram_Parameter
     (Parameter_Name  : Name_Id;
      Para_Type_Node  : Type_Id;
      Subprogram_Node : Subprogram_Id;
      Parameter_Sloc  : XE_Scan.Location_Type;
      Parameter_Node  : out Parameter_Id);
   --  Declare a parameter for a subprogram. The last parameter corresponds
   --  to a returned value when the subprogram is a function.

   procedure Declare_Type
     (Type_Name : Name_Id;
      Type_Kind : Predefined_Type;
      Composite : Boolean;
      Array_Len : Int;
      Comp_Type : Type_Id;
      Type_Sloc : XE_Scan.Location_Type;
      Type_Node : out Type_Id);
   --  Declare a new type into the configuration context. If type is
   --  not a composite, then it is a scalar type or a string type. If
   --  Array_Len is zero, it is a record type. Comp_Type is the type
   --  of an array component type.

   procedure Declare_Type_Attribute
     (Type_Node          : Type_Id;
      Attribute_Name     : Name_Id;
      Attr_Type_Node     : Type_Id;
      Attribute_Kind     : Attribute_Type;
      Attribute_Sloc     : XE_Scan.Location_Type;
      Attribute_Node     : out Attribute_Id);
   --  Declare an attribute for a given type. This procedure creates a
   --  component of type Attr_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Type_Component
     (Type_Node          : Type_Id;
      Component_Name     : Name_Id;
      Comp_Type_Node     : Type_Id;
      Component_Sloc     : XE_Scan.Location_Type;
      Component_Node     : out Component_Id);
   --  Declare a component for a given type. This procedure creates a
   --  component of type Comp_Type_Node and includes it in the type
   --  component list.

   procedure Declare_Variable
     (Variable_Name : Name_Id;
      Variable_Type : Type_Id;
      Variable_Sloc : XE_Scan.Location_Type;
      Variable_Node : out Variable_Id);
   --  Declare a new variable into the configuration context. This variable
   --  of name Variable_Name is of type Variable_Type. Allocate the
   --  component nodes if needed (not attributes).

   procedure Declare_Variable_Component
     (Variable_Node      : Variable_Id;
      Component_Name     : Name_Id;
      Component_Type     : Type_Id;
      Attribute_Kind     : Attribute_Type;
      Component_Sloc     : XE_Scan.Location_Type;
      Component_Node     : out Component_Id);
   --  Add a component for a given variable. This component is
   --  possibly an attribute. The component type is Component_Type.

   procedure Duplicate_Variable
     (Source, Target : Variable_Id);
   --  Duplicate all the content except attributes

   procedure Exit_On_Error;
   --  Print configuration if verbose_mode and then raise Parsing_Error

   procedure Initialize;
   --  Elaboration code

   procedure Match_Actual_With_Formal
     (Subprogram_Node : Subprogram_Id);
   --  Parse a subprogram call and associate actual parameters to formal
   --  parameters.

   procedure P_Aggregate_Assignment
     (Variable_Node   : Variable_Id);
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
     (Previous_Name   : Name_Id;
      Previous_Sloc   : XE_Scan.Location_Type);
   --  Parse a list of identifiers

   procedure Parse;
   --  Main procedure

   procedure Print;
   --  Print node tree for debugging purpose. The global variable
   --  Configuration_Node is used as tree root.

   procedure Print_Component
     (Node : Component_Id;
      Many : Int);

   procedure Print_Parameter
     (Node : Parameter_Id;
      Many : Int);

   procedure Print_Statement
     (Node : Statement_Id;
      Many : Int);

   procedure Print_Subprogram
     (Node : Subprogram_Id;
      Many : Int);

   procedure Print_Type
     (Node : Type_Id;
      Many : Int);

   procedure Print_Variable
     (Node : Variable_Id;
      Many : Int);

   procedure Search_Actual_Parameter
     (Actual_Name : Name_Id;
      Actual_Type : Type_Id;
      Actual_Node : out Variable_Id);
   --  Similar to Search_Variable but check name *and* type

   procedure Search_Component
     (Component_Name : Name_Id;
      Type_Node      : Type_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  type Type_Node. If unsuccessful, returns Null_Component.

   procedure Search_Component
     (Component_Name : Name_Id;
      Variable_Node  : Variable_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of a component Component_Name in a
   --  variable Variable_Node. If unsuccessful, returns Null_Component.

   procedure Search_Declaration
     (Declaration_Name : Name_Id;
      Declaration_Node : out Node_Id);
   --  Search for the first occurrence of a declaration
   --  Declaration_Name. If unsuccessful, returns Null_Node.

   procedure Search_Function_Returned_Parameter
     (Function_Node  : Subprogram_Id;
      Parameter_Node : out Parameter_Id);
   --  Search for the last parameter of this subprogram. This is by
   --  convention the returned parameter.

   procedure Search_Matching_Parameter
     (Subprogram_Node : Subprogram_Id;
      Convention      : Convention_Type;
      Formal_Name     : in out Name_Id;
      Formal_Type     :    out Type_Id;
      Parameter_Node  : in out Parameter_Id);
   --  Search for a formal parameter that has no actual associated
   --  parameter. This choice should follow Convention requirements. If
   --  Convention is Named, then returns Parameter_Node of name
   --  Formal_Name. If is Positional, returns the next unmatched parameter
   --  and returns also its name in Formal_Name.

   procedure Search_Next_Component
     (Component_Name : Name_Id;
      Component_Node : in out Component_Id);
   --  Search for the next occurrence of a component Component_Name in
   --  a list of components starting from Component_Node. If
   --  unsuccessful, returns Null_Component.

   procedure Search_Next_Declaration
     (Declaration_Name : Name_Id;
      Declaration_Node : in out Node_Id);
   --  Search the next occurence of a declaration Declaration_Name in
   --  the configuration starting from Declaratio_Node. If unsuccessful,
   --  returns Null_Node.

   procedure Search_Next_Pragma
     (Pragma_Name : Name_Id;
      Pragma_Node : in out Subprogram_Id);
   --  Search for the next occurrence of a pragma Pragma_Name in a
   --  configuration starting from Subprogram_Node. If unsuccessful,
   --  returns Null_Subprogram.

   procedure Search_Next_Subprogram
     (Subprogram_Name : Name_Id;
      Subprogram_Node : in out Subprogram_Id);
   --  Search for the next occurrence of a subprogram Subprogram_Name
   --  in a configuration starting from Subprogram_Node. If
   --  unsuccessful, returns Null_Subprogram.

   procedure Search_Pragma
     (Pragma_Name : Name_Id;
      Pragma_Kind : out Pragma_Type;
      Pragma_Node : out Subprogram_Id);
   --  Search for the first occurrence of a pragma Pragma_Name. If
   --  unsuccessful, returns Null_Pragma. If successful, Pragma_Kind is set
   --  to its corresponding litteral. Otherwise, it is set to
   --  Pragma_Unknown.

   procedure Search_Subprogram
     (Subprogram_Name : Name_Id;
      Subprogram_Node : out Subprogram_Id);
   --  Search for the first occurrence of a subprogram Subprogram_Name. If
   --  unsuccessful, returns Null_Subprogram.

   procedure Search_Type
     (Type_Name : Name_Id;
      Type_Kind : out Predefined_Type;
      Type_Node : out Type_Id);
   --  Search for the first occurrence of a type Type_Name. If
   --  unsuccessful, returns Null_Type. If successful, Type_Kind is set to
   --  its corresponding litteral. Otherwise, it is set to
   --  Pre_Type_Unknown.

   procedure Search_Uninitialized_Component
     (Variable_Node  : Variable_Id;
      Component_Type : Type_Id;
      Component_Node : out Component_Id);
   --  Search for the first occurrence of an uninitialized component in a
   --  variable Variable_Node. Attributes are discarded. If unsuccessful,
   --  returns Null_Component.

   procedure Search_Variable
     (Variable_Name : Name_Id;
      Variable_Node : out Variable_Id);
   --  Search for the first occurrence of a variable Variable_Name. If
   --  unsuccessful, returns Null_Variable.

   procedure Set_Node_Location
     (Node     : Node_Id;
      Location : XE_Scan.Location_Type);
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

   procedure Take_Token (T : Token_Type);

   procedure Take_Token (L : Token_List_Type);

end XE_Parse;
