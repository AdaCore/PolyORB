------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                   X E                                    --
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

with Table;
with Opt;
with XE_Utils;   use XE_Utils;

package XE is

   --  Several names are reserved keywords. For each of these names, a key
   --  is associated in the hash table. This allows to retrieve the nature
   --  of the name and especially its type. The key (an integer) is in one
   --  of the following ranges and therefore, the name corresponds to the
   --  image of an element in the enumeration type.


   --  Keyword --

   type Token_Type is new Int range 100 .. 125;

   Tok_Unknown        : constant Token_Type := 100;  -- (0)  Identifier
   Tok_String_Literal : constant Token_Type := 101;  -- (1)  string literal
   Tok_Identifier     : constant Token_Type := 102;  -- (2)  identifer
   Tok_Dot            : constant Token_Type := 103;  -- (3)  .
   Tok_Apostrophe     : constant Token_Type := 104;  -- (4)  '
   Tok_Left_Paren     : constant Token_Type := 105;  -- (5)  (
   Tok_Right_Paren    : constant Token_Type := 106;  -- (6)  )
   Tok_Comma          : constant Token_Type := 107;  -- (7)  ,
   Tok_Colon_Equal    : constant Token_Type := 108;  -- (8)  :=
   Tok_Colon          : constant Token_Type := 109;  -- (9)  :
   Tok_Configuration  : constant Token_Type := 110;  -- (10) CONFIGURATION
   Tok_Pragma         : constant Token_Type := 111;  -- (11) PRAGMA
   Tok_Procedure      : constant Token_Type := 112;  -- (12) PROCEDURE
   Tok_Is             : constant Token_Type := 113;  -- (13) IS
   Tok_In             : constant Token_Type := 114;  -- (14) IN
   Tok_For            : constant Token_Type := 115;  -- (15) FOR
   Tok_Use            : constant Token_Type := 116;  -- (16) USE
   Tok_Function       : constant Token_Type := 117;  -- (17) FUNCTION
   Tok_End            : constant Token_Type := 118;  -- (18) END
   Tok_Begin          : constant Token_Type := 119;  -- (19) BEGIN
   Tok_Null           : constant Token_Type := 120;  -- (20) NULL
   Tok_Semicolon      : constant Token_Type := 121;  -- (21) ;
   Tok_Arrow          : constant Token_Type := 122;  -- (22) =>
   Tok_Return         : constant Token_Type := 123;  -- (23) return
   Tok_EOF            : constant Token_Type := 124;  -- (24) end of file
   Tok_Reserved       : constant Token_Type := 125;  -- (25) Ada keywords

   type Token_List_Type is array (Positive range <>) of Token_Type;

   Reserved  : array (Token_Type) of Boolean := (others => False);


   -- Attribute_Type --

   type Attribute_Type is new Int range 200 .. 207;

   Attribute_Unknown      : constant Attribute_Type := 200;
   Attribute_Host         : constant Attribute_Type := 201;
   Attribute_Storage_Dir  : constant Attribute_Type := 202;
   Attribute_Main         : constant Attribute_Type := 203;
   Attribute_Command_Line : constant Attribute_Type := 204;
   Attribute_Termination  : constant Attribute_Type := 205;
   Attribute_Leader       : constant Attribute_Type := 206;
   Attribute_Filter       : constant Attribute_Type := 207;


   -- Pragma_Type --

   type Pragma_Type is new Int range 300 .. 305;

   Pragma_Unknown         : constant Pragma_Type := 300;
   Pragma_Starter         : constant Pragma_Type := 301;
   Pragma_Import          : constant Pragma_Type := 302;
   Pragma_Boot_Server     : constant Pragma_Type := 303;
   Pragma_Version         : constant Pragma_Type := 304;
   Pragma_Filter          : constant Pragma_Type := 305;


   -- Import_Method_Type --

   type Import_Method_Type is new Int range 341 .. 343;

   Ada_Import             : constant Import_Method_Type := 341;
   Shell_Import           : constant Import_Method_Type := 342;
   None_Import            : constant Import_Method_Type := 343;


   -- Predefined_Type --

   type Predefined_Type is new Int range 401 .. 410;

   Pre_Type_Partition       : constant Predefined_Type := 401;
   Pre_Type_Channel         : constant Predefined_Type := 402;
   Pre_Type_Boolean         : constant Predefined_Type := 403;
   Pre_Type_Integer         : constant Predefined_Type := 404;
   Pre_Type_String          : constant Predefined_Type := 405;
   Pre_Type_Entity          : constant Predefined_Type := 406;
   Pre_Type_Convention      : constant Predefined_Type := 407;
   Pre_Type_Ada_Unit        : constant Predefined_Type := 408;
   Pre_Type_Function        : constant Predefined_Type := 409;
   Pre_Type_Procedure       : constant Predefined_Type := 410;


   -- Termination_Type --

   type Termination_Type is new Int range 500 .. 503;

   Unknown_Termination  : constant Termination_Type := 500;
   Local_Termination    : constant Termination_Type := 501;
   Global_Termination   : constant Termination_Type := 502;
   Deferred_Termination : constant Termination_Type := 503;


   -- Node_Id --

   type Node_Id          is new Int range 10_000 .. 99_999;
   type Type_Id          is new Node_Id;
   type Variable_Id      is new Node_Id;
   type Component_Id     is new Node_Id;
   type Parameter_Id     is new Node_Id;
   type Attribute_Id     is new Node_Id;
   type Statement_Id     is new Node_Id;
   type Subprogram_Id    is new Node_Id;
   type Configuration_Id is new Node_Id;

   Null_Node  : constant Node_Id := Node_Id'First;
   First_Node : constant Node_Id := Null_Node + 1;

   NN                 : constant Node_Id          := Null_Node;
   Null_Type          : constant Type_Id          := Type_Id (NN);
   Null_Variable      : constant Variable_Id      := Variable_Id (NN);
   Null_Parameter     : constant Parameter_Id     := Parameter_Id (NN);
   Null_Component     : constant Component_Id     := Component_Id (NN);
   Null_Subprogram    : constant Subprogram_Id    := Subprogram_Id (NN);
   Null_Configuration : constant Configuration_Id := Configuration_Id (NN);


   -- Standard nodes --

   Configuration_Node       : Configuration_Id;

   Partition_Type_Node      : Type_Id;
   Channel_Type_Node        : Type_Id;
   Boolean_Type_Node        : Type_Id;
   Integer_Type_Node        : Type_Id;
   String_Type_Node         : Type_Id;
   Convention_Type_Node     : Type_Id;
   Ada_Unit_Type_Node       : Type_Id;
   Subprogram_Type_Node     : Type_Id;
   Main_Procedure_Type_Node : Type_Id;
   Host_Function_Type_Node  : Type_Id;

   Pragma_Starter_Node      : Subprogram_Id;
   Pragma_Import_Node       : Subprogram_Id;
   Pragma_Boot_Server_Node  : Subprogram_Id;
   Pragma_Version_Node      : Subprogram_Id;


   -- Internal System Names --

   ISN_List_Comp            : Name_Id;
   ISN_Array_Comp           : Name_Id;
   ISN_Proc_Main            : Name_Id;
   ISN_Appl_Main            : Name_Id;
   ISN_Return_Par           : Name_Id;
   ISN_Subpro_Par           : Name_Id;
   ISN_Proc_Call            : Name_Id;


   Configuration_File       : File_Name_Type  := No_File;


   -- GNATDIST flags --

   Verbose_Mode       : Boolean;
   Debug_Mode         : Boolean;
   Quiet_Output       : Boolean;
   No_Recompilation   : Boolean;
   Building_Script    : Boolean;


   procedure Add_Configuration_Declaration
     (Configuration_Node : in Configuration_Id;
      Declaration_Node   : in Node_Id);
   --  Add a configuration node to the list of configuration.

   procedure Add_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : in Parameter_Id);
   --  Add a parameter node to the subprogram parameters list.

   procedure Add_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : in Component_Id);
   --  Add a component to the type component list.

   procedure Add_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : in Component_Id);
   --  Add a component to the variable component list.

   procedure Component_Is_Initialized
     (Component_Node : in Component_Id;
      Is_Initialized : in Boolean);
   --  Has this component a value.

   function Convert (Item : Attribute_Type) return Int;

   function Convert (Item : Int) return Attribute_Type;

   function Convert (Item : Pragma_Type) return Int;

   function Convert (Item : Int) return Pragma_Type;

   function Convert (Item : Import_Method_Type) return Int;

   function Convert (Item : Int) return Import_Method_Type;

   function Convert (Item : Predefined_Type) return Int;

   function Convert (Item : Int) return Predefined_Type;

   function Convert (Item : Termination_Type) return Int;

   function Convert (Item : Int) return Termination_Type;

   procedure Create_Component
     (Component_Node : out Component_Id;
      Component_Name : in  Name_Id);
   pragma Inline (Create_Component);

   procedure Create_Configuration
     (Configuration_Node : out Configuration_Id;
      Configuration_Name : in  Name_Id);
   pragma Inline (Create_Configuration);

   procedure Create_Parameter
     (Parameter_Node : out Parameter_Id;
      Parameter_Name : in  Name_Id);
   pragma Inline (Create_Parameter);

   procedure Create_Statement
     (Statement_Node : out Statement_Id;
      Statement_Name : in  Name_Id);
   pragma Inline (Create_Statement);

   procedure Create_Subprogram
     (Subprogram_Node : out Subprogram_Id;
      Subprogram_Name : in  Name_Id);
   pragma Inline (Create_Subprogram);

   procedure Create_Type
     (Type_Node : out Type_Id;
      Type_Name : in  Name_Id);
   pragma Inline (Create_Type);

   procedure Create_Variable
     (Variable_Node : out Variable_Id;
      Variable_Name : in  Name_Id);
   pragma Inline (Create_Variable);

   procedure First_Configuration_Declaration
     (Configuration_Node : in  Configuration_Id;
      Declaration_Node   : out Node_Id);
   --  Set to the first declaration in the configuration list.

   procedure First_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : out Parameter_Id);
   --  Set to the first parameter in the subprogram parameter list.

   procedure First_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : out Component_Id);
   --  Set to the first type component in the type component list.

   procedure First_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : out Component_Id);
   --  Set to the first component of the variable component list. This
   --  could be different from the type component list (partition_type).

   function Get_Array_Component_Type
     (Type_Node : Type_Id)
     return Type_Id;
   --  When the type is a component array type, this function returns the
   --  type of a component. Otherwise, it returns null_type.

   function Get_Attribute_Kind
     (Component_Node : Component_Id)
     return Attribute_Type;
   --  A type or a variable is a set of components and of attributes. This
   --  function returns the attribute type, Attrbiute_Unknown when it is
   --  not an attribute.

   function Get_Component_List_Size
     (Type_Node : Type_Id)
     return Int;
   --  When the type is an array type or a list type, this function returns
   --  the size of the array or the list. Otherwise, it returns 0 (neither
   --  a list nor an array).

   function Get_Component_Type
     (Component_Node : Component_Id)
     return Type_Id;

   function  Get_Component_Value
     (Component_Node : Component_Id)
     return Node_Id;
   --  Return the variable set as value for this component.

   function Get_Node_Name
     (Node : Node_Id)
     return Name_Id;
   pragma Inline (Get_Node_Name);

   procedure Get_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : out Int;
      Loc_Y : out Int);
   pragma Inline (Get_Node_SLOC);

   function  Get_Parameter_Type
     (Parameter_Node : Parameter_Id)
     return Type_Id;

   function  Get_Pragma_Kind
     (Subprogram_Node : Subprogram_Id)
      return Pragma_Type;
   --  Subprograms are used to implement pragmas and ada units. This
   --  function returns Pragma_Unknown when this subprogram does not
   --  implement a pragma.

   function  Get_Scalar_Value
     (Variable_Node : Variable_Id)
      return Int;
   --  Return a scalar rather than a variable as a value.

   function  Get_Subprogram_Call
     (Statement_Node  : Statement_Id)
      return Subprogram_Id;
   --  This statement is a procedure call. This returns a copy of the
   --  subprogram call with the initialized parameters in it.

   function  Get_Token (N : Name_Id) return Token_Type;

   function  Get_Type_Kind
     (Type_Node : Type_Id)
      return Predefined_Type;
   --  This function is used to easily retrieve a predefined_type id.

   function Get_Variable_Type
     (Variable_Node : Variable_Id)
     return Type_Id;

   function Get_Variable_Value
     (Variable_Node : Variable_Id)
     return Variable_Id;
   --  Return a variable rather than a scalar as a value.

   function  Is_Component
     (Node : Node_Id)
      return Boolean;
   pragma Inline (Is_Component);

   function Is_Component_Initialized
     (Component_Node : Component_Id)
      return Boolean;
   --  Has this component a value.

   function  Is_Configuration
     (Node : Node_Id)
      return Boolean;
   pragma Inline (Is_Configuration);

   function  Is_Parameter_Initialized
     (Parameter_Node : Parameter_Id)
      return Boolean;
   --  Has this parameter a value. Parameter are marked to find what
   --  parameter is missing in a subprogram call.

   function  Is_Statement
     (Node : Node_Id)
      return Boolean;
   pragma Inline (Is_Statement);

   function  Is_Subprogram
     (Node : Node_Id)
      return Boolean;
   pragma Inline (Is_Subprogram);

   function Is_Subprogram_A_Procedure
     (Subprogram_Node : Subprogram_Id)
     return Boolean;

   function  Is_Type
     (Node : Node_Id)
     return Boolean;
   pragma Inline (Is_Type);

   function Is_Type_Frozen
     (Type_Node : Type_Id)
     return Boolean;
   --  Is it possible to add new litteral in an enumeration type. Ada Unit
   --  Type is an extensible enumeration type. When parsing, a variable of
   --  this type can be pushed automatically in the declaration.

   function  Is_Variable
     (Node : Node_Id)
     return Boolean;
   pragma Inline (Is_Variable);

   procedure Next_Configuration_Declaration
     (Declaration_Node   : in out Node_Id);
   --  There are two configurations : the user one and the standard
   --  one. When the next declaration is a configuration node, go into this
   --  configuration and return the next declaration node.

   procedure Next_Subprogram_Parameter
     (Parameter_Node  : in out Parameter_Id);
   --  Set to the next parameter in the subprogram list parameters.

   procedure Next_Type_Component
     (Component_Node  : in out Component_Id);
   --  Set to the next component of type component list.

   procedure Next_Variable_Component
     (Component_Node  : in out Component_Id);
   --  Set to the next component in the variable component list.

   procedure Parameter_Is_Initialized
     (Parameter_Node : in Parameter_Id;
      Is_Initialized : in Boolean);
   --  Parameter are marked to find what parameter is missing in a
   --  subprogram call.

   procedure Set_Array_Component_Type
     (Type_Node : in Type_Id;
      Comp_Type : in Type_Id);
   --  This type becomes an component list type. Each element is of type
   --  element_type.

   procedure Set_Attribute_Kind
     (Component_Node : in Component_Id;
      Attribute_Kind : in Attribute_Type);
   --  See Get_Attribute_Kind.

   procedure Set_Component_List_Size
     (Type_Node : in Type_Id;
      List_Size : in Int);
   --  This type becomes an component list type. List_size indicates
   --  whether it is a constrained array or not.

   procedure Set_Component_Type
     (Component_Node : in Component_Id;
      Type_Node      : in Type_Id);
   --  See Get_Component_Type.

   procedure Set_Component_Value
     (Component_Node : in Component_Id;
      Value_Node     : in Node_Id);
   --  See Get_Component_Value.

   procedure Set_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : in Int;
      Loc_Y : in Int);
   pragma Inline (Set_Node_SLOC);

   procedure Set_Parameter_Type
     (Parameter_Node : in Parameter_Id;
      Parameter_Type : in Type_Id);
   --  See Get_Parameter_Type.

   procedure Set_Pragma_Kind
     (Subprogram_Node : in Subprogram_Id;
      Pragma_Kind     : in Pragma_Type);
   --  See Get_Pragam_Kind.

   procedure Set_Scalar_Value
     (Variable_Node : in Variable_Id;
      Scalar_Value  : in Int);
   --  See Get_Scalar_Value.

   procedure Set_Subprogram_Call
     (Statement_Node  : in Statement_Id;
      Subprogram_Node : in Subprogram_Id);
   --  Initiliaze this statement with a copy of the subprogram tree. This
   --  tree contains the parameters with their associated values.

   procedure Set_Token (N : String; T : Token_Type);

   procedure Set_Type_Kind
     (Type_Node : in Type_Id;
      Type_Kind : in Predefined_Type);
   --  See Get_Type_Kind.

   procedure Set_Variable_Type
     (Variable_Node : in Variable_Id;
      Variable_Type : in Type_Id);
   --  See Get_Variable_Type.

   procedure Set_Variable_Value
     (Variable_Node : in Variable_Id;
      Value_Node    : in Variable_Id);
   --  See Get_Variable_Value.

   procedure Subprogram_Is_A_Procedure
     (Subprogram_Node : in Subprogram_Id;
      Procedure_Node  : in Boolean);

   procedure Type_Is_Frozen
     (Type_Node  : in Type_Id;
      Extensible : in Boolean);
   --  See Is_Type_Frozen.

   procedure Write_SLOC (Node : Node_Id);

end XE;
