------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
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

--  The following package declares the data types for GNAT project.
--  These data types may be used by GNAT Project-aware tools.

--  Children of these package implements various services on these data types.
--  See in particular Prj.Pars and Prj.Env.

with Casing;      use Casing;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Scans;       use Scans;
with Table;
with Types;       use Types;

package Prj is

   type Verbosity is (Default, Medium, High);
   --  Verbosity when parsing GNAT Project Files.
   --  Default is default (very quiet, if no errors).
   --  Medium is more verbose.
   --  High is extremely verbose.

   function Empty_String return String_Id;

   type String_List_Id is new Nat;
   Nil_String : constant String_List_Id := 0;
   type String_Element is record
      Value    : String_Id      := No_String;
      Location : Source_Ptr     := No_Location;
      Next     : String_List_Id := Nil_String;
   end record;
   --  To hold values for string list variables and array elements.

   package String_Elements is new Table.Table
     (Table_Component_Type => String_Element,
      Table_Index_Type     => String_List_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Prj.String_Elements");
   --  The table for string elements in string lists.

   type Variable_Kind is (Undefined, List, Single);
   --  Different kinds of variables

   type Variable_Value (Kind : Variable_Kind := Undefined) is record
      Location : Source_Ptr := No_Location;
      Default  : Boolean    := False;
      case Kind is
         when Undefined =>
            null;
         when List =>
            Values : String_List_Id := Nil_String;
         when Single =>
            Value : String_Id := No_String;
      end case;
   end record;
   --  Values for variables and array elements

   Nil_Variable_Value : constant Variable_Value :=
     (Kind     => Undefined,
      Location => No_Location,
      Default  => False);
   --  Value of a non existing variable or array element.

   type Variable_Id is new Nat;
   No_Variable : constant Variable_Id := 0;
   type Variable is record
      Next     : Variable_Id := No_Variable;
      Name     : Name_Id;
      Value    : Variable_Value;
   end record;
   --  To hold the list of variables in a project file and in packages.

   package Variable_Elements is new Table.Table
     (Table_Component_Type => Variable,
      Table_Index_Type     => Variable_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Prj.Variable_Elements");
   --  The table of variable in list of variables.

   type Array_Element_Id is new Nat;
   No_Array_Element : constant Array_Element_Id := 0;
   type Array_Element is record
      Index    : Name_Id;
      Value    : Variable_Value;
      Next     : Array_Element_Id := No_Array_Element;
   end record;
   --  Each Array_Element represents an array element.
   --  Each Array_Element is linked (Next) to the next array element,
   --  if any, in the array.

   package Array_Elements is new Table.Table
     (Table_Component_Type => Array_Element,
      Table_Index_Type     => Array_Element_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Prj.Array_Elements");
   --  The table that contains all array elements

   type Array_Id is new Nat;
   No_Array : constant Array_Id := 0;
   type Array_Data is record
      Name  : Name_Id          := No_Name;
      Value : Array_Element_Id := No_Array_Element;
      Next  : Array_Id         := No_Array;
   end record;
   --  Each Array_Data represents an array.
   --  Value is the id of the first element.
   --  Next is the id of the next array in the project file or package.

   package Arrays is new Table.Table
     (Table_Component_Type => Array_Data,
      Table_Index_Type     => Array_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Prj.Arrays");
   --  The table that contains all arrays

   type Package_Id is new Nat;
   No_Package : constant Package_Id := 0;
   type Declarations is record
      Variables  : Variable_Id := No_Variable;
      Attributes : Variable_Id := No_Variable;
      Arrays     : Array_Id    := No_Array;
      Packages   : Package_Id  := No_Package;
   end record;

   No_Declarations : constant Declarations :=
     (Variables  => No_Variable,
      Attributes => No_Variable,
      Arrays     => No_Array,
      Packages   => No_Package);
   --  Declarations. Used in project structures and packages.

   type Package_Element is record
      Name   : Name_Id      := No_Name;
      Decl   : Declarations := No_Declarations;
      Parent : Package_Id   := No_Package;
      Next   : Package_Id   := No_Package;
   end record;
   --  A package. Includes declarations that may include
   --  other packages.

   package Packages is new Table.Table
     (Table_Component_Type => Package_Element,
      Table_Index_Type     => Package_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Prj.Packages");
   --  The table that contains all arays.

   function Image (Casing : Casing_Type) return String;
   --  Similar to 'Image

   function Value (Image : String) return Casing_Type;
   --  Similar to 'Value
   --  This is to avoid s-valenu in the closure of the tools
   --  Raises Constraint_Error if not a Casing_Type image.

   type Naming_Data is record
      Dot_Replacement      : Name_Id          := No_Name;
      Dot_Repl_Loc         : Source_Ptr       := No_Location;
      Casing               : Casing_Type      := All_Lower_Case;
      Specification_Append : Name_Id          := No_Name;
      Spec_Append_Loc      : Source_Ptr       := No_Location;
      Body_Append          : Name_Id          := No_Name;
      Body_Append_Loc      : Source_Ptr       := No_Location;
      Separate_Append      : Name_Id          := No_Name;
      Sep_Append_Loc       : Source_Ptr       := No_Location;
      Specifications       : Array_Element_Id := No_Array_Element;
      Bodies               : Array_Element_Id := No_Array_Element;
   end record;
   --  A naming scheme.

   function Standard_Naming_Data return Naming_Data;
   pragma Inline (Standard_Naming_Data);
   --  The standard GNAT naming scheme.

   function Same_Naming_Scheme
     (Left, Right : Naming_Data)
      return        Boolean;
   --  Returns True if Left and Right are the same naming scheme
   --  not considering Specifications and Bodies.

   type Project_Id is new Nat;
   No_Project : constant Project_Id := 0;
   --  Id of a Project File

   type Project_List is new Nat;
   Empty_Project_List : constant Project_List := 0;
   --  A list of project files.

   type Project_Element is record
      Project : Project_Id   := No_Project;
      Next    : Project_List := Empty_Project_List;
   end record;
   --  Element in a list of project file.
   --  Next is the id of the next project file in the list.

   package Project_Lists is new Table.Table
     (Table_Component_Type => Project_Element,
      Table_Index_Type     => Project_List,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Prj.Project_Lists");
   --  The table that contains the lists of project files.

   type Project_Data is record
      First_Referred_By  : Project_Id     := No_Project;
      Name               : Name_Id        := No_Name;
      Path_Name          : Name_Id        := No_Name;
      Location           : Source_Ptr     := No_Location;
      Directory          : Name_Id        := No_Name;
      File_Name          : Name_Id        := No_Name;
      Sources            : String_List_Id := Nil_String;
      Source_Dirs        : String_List_Id := Nil_String;
      Object_Directory   : Name_Id        := No_Name;
      Modifies           : Project_Id     := No_Project;
      Modified_By        : Project_Id     := No_Project;
      Naming             : Naming_Data    := Standard_Naming_Data;
      Decl               : Declarations   := No_Declarations;
      Imported_Projects  : Project_List   := Empty_Project_List;
      Include_Path       : String_Access  := null;
      Objects_Path       : String_Access  := null;
      Gnat_Adc_Generated : Boolean        := False;
      Checked            : Boolean        := False;
   end record;
   --  Project File representation.

   function Empty_Project return Project_Data;

   package Projects is new Table.Table (
     Table_Component_Type => Project_Data,
     Table_Index_Type     => Project_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Prj.Projects");

   procedure Expect (The_Token : Token_Type; Token_Image : String);
   --  Check that the current token is The_Token.
   --  If it is not, output an error message.

   procedure Initialize;
   --  This procedure must be called before using any services
   --  from the Prj hierarchy.
   --  Assumption: Namet.Initialize have already been called.

private

   procedure Scan;
   --  Calls Scn.Scan and change any Operator_Symbol to String_Literal

end Prj;
