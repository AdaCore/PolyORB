------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . T R E E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------
--
--  This package defines the structure of the Project File tree.

with GNAT.HTable;
with Prj.Attr; use Prj.Attr;
with Prj.Com;  use Prj.Com;
with Types;    use Types;
with Table;

package Prj.Tree is

   Project_Nodes_Initial   : constant := 1_000;
   Project_Nodes_Increment : constant := 100;

   Project_Node_Low_Bound  : constant := 0;
   Project_Node_High_Bound : constant := 099_999_999;
   --  In practice, infinite

   type Project_Node_Id is range
     Project_Node_Low_Bound .. Project_Node_High_Bound;

   Empty_Node    : constant Project_Node_Id := Project_Node_Low_Bound;
   First_Node_Id : constant Project_Node_Id := Project_Node_Low_Bound;

   type Project_Node_Kind is
     (N_Project,
      --  Name:      project name
      --  Path_Name: project path name
      --  Expr_Kind: Undefined
      --  Field1:    first with clause
      --  Field2:    project declaration
      --  Field3:    first string type
      --  Value:     modified project path name (if any)
      N_With_Clause,
      --  Name:      imported project name
      --  Path_Name: imported project path name
      --  Expr_Kind: Undefined
      --  Field1:    project node
      --  Field2:    next with clause
      --  Field3:    not used
      --  Value:     literal string withed
      N_Project_Declaration,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: Undefined
      --  Field1:    first declarative item
      --  Field2:    modified project
      --  Field3:    not used
      --  Value:     not used
      N_Declarative_Item,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: Undefined
      --  Field1:    current item node
      --  Field2:    next declarative item
      --  Field3:    not used
      --  Value:     not used
      N_Package_Declaration,
      --  Name:      package name
      --  Path_Name: not used
      --  Expr_Kind: Undefined
      --  Field1:    project of renamed package (if any)
      --  Field2:    first declarative item
      --  Field3:    next package in project
      --  Value:     not used
      N_String_Type_Declaration,
      --  Name:      type name
      --  Path_Name: not used
      --  Expr_Kind: Undefined
      --  Field1:    first literal string
      --  Field2:    next string type
      --  Field3:    not used
      --  Value:     not used
      N_Literal_String,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: Single
      --  Field1:    next literal string
      --  Field2:    not used
      --  Field3:    not used
      --  Value:     string value
      N_Attribute_Declaration,
      --  Name:      attribute name
      --  Path_Name: not used
      --  Expr_Kind: attribute kind
      --  Field1:    expression
      --  Field2:    empty_node
      --  Field3:    not used
      --  Value:     associative array index (if an associative array element)
      N_Typed_Variable_Declaration,
      --  Name:      variable name
      --  Path_Name: not used
      --  Expr_Kind: Single
      --  Field1:    expression
      --  Field2:    first string of string type
      --  Field3:    next variable
      --  Value:     not used
      N_Variable_Declaration,
      --  Name:      variable name
      --  Path_Name: not used
      --  Expr_Kind: variable kind
      --  Field1:    expression
      --  Field2:    empty_node
      --  Field3:    next variable
      --  Value:     not used
      N_Expression,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: expression kind
      --  Field1:    first term
      --  Field2:    next expression in list
      --  Field3:    not used
      --  Value:     not used
      N_Term,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: term kind
      --  Field1:    current term
      --  Field2:    next term
      --  Field3:    not used
      --  Value:     not used
      N_Literal_String_List,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: List
      --  Field1:    first expression
      --  Field2:    not used
      --  Field3:    not used
      --  Value:     not used
      N_Variable_Reference,
      --  Name:      variable name
      --  Path_Name: not used
      --  Expr_Kind: variable kind
      --  Field1:    project (if specified)
      --  Field2:    package (if specified)
      --  Field3:    string type (if any)
      --  Value:     not used
      N_External_Value,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: Single
      --  Field1:    first argument (literal string)
      --  Field2:    second argument (literal string)
      --  Field3:    not used
      --  Value:     not used
      N_Atribute_Reference,
      --  Name:      attribute name
      --  Path_Name: not used
      --  Expr_Kind: attribute kind
      --  Field1:    project
      --  Field2:    package (if attribute of a package)
      --  Field3:    not used
      --  Value:     not used
      N_Case_Construction,
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: Undefined
      --  Field1:    case variable reference
      --  Field2:    first case item
      --  Field3:    not used
      --  Value:     not used
      N_Case_Item);
      --  Name:      not used
      --  Path_Name: not used
      --  Expr_Kind: not used
      --  Field1:    first choice (literal string)
      --  Field2:    first declarative item
      --  Field3:    next case item
      --  Value:     not used

   type Project_Node_Record is record

      Kind        : Project_Node_Kind;

      Location    : Source_Ptr    := No_Location;

      Directory   : Name_Id       := No_Name;
      --  only for N_Project

      Expr_Kind   : Variable_Kind := Undefined;
      --  See above

      Variables   : Project_Node_Id := Empty_Node;
      --  first variable in a project or a package

      Packages    : Project_Node_Id := Empty_Node;
      --  first package declaration in a project

      Pkg_Id      : Package_Node_Id := Empty_Package;
      --  only use in Package_Declaration

      Name        : Name_Id         := No_Name;
      --  see above

      Path_Name   : Name_Id         := No_Name;
      --  see above

      Value       : String_Id       := No_String;
      --  see above

      Field1      : Project_Node_Id := Empty_Node;
      --  see above

      Field2      : Project_Node_Id := Empty_Node;
      --  see above

      Field3      : Project_Node_Id := Empty_Node;
      --  see above

   end record;

   function Default_Project_Node
     (Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined)
     return Project_Node_Record;
   --  Returns a Project_Node_REcord with the specified Kind and
   --  Expr_Kind; all the other components have default nil values.

   package Project_Nodes is
      new Table.Table (Table_Component_Type => Project_Node_Record,
                       Table_Index_Type     => Project_Node_Id,
                       Table_Low_Bound      => First_Node_Id,
                       Table_Initial        => Project_Nodes_Initial,
                       Table_Increment      => Project_Nodes_Increment,
                       Table_Name           => "Project_Nodes");
   --  This table contains the syntactic tree of project data
   --  from project files.

   type Project_Name_And_Node is record
      Name     : Name_Id;
      Node     : Project_Node_Id;
      Modified : Boolean;
   end record;

   No_Project_Name_And_Node : constant Project_Name_And_Node :=
     (Name => No_Name, Node => Empty_Node, Modified => True);

   package Projects_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Project_Name_And_Node,
      No_Element => No_Project_Name_And_Node,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table contains a mapping of project names to project nodes.

end Prj.Tree;
