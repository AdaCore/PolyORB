------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 P R J                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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
--  See in particular Project.Parsing and Project.Environment.

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Prj is

   type Verbosity is (Default, Medium, High);
   --  Verbosity when parsing GNAT Project Files.
   --  Default is default (very quiet, if no errors).
   --  Medium is more verbose.
   --  High is extremely verbose.

   type String_Element;
   type String_List is access String_Element;
   type String_Element is record
      Value : String_Access;
      Next  : String_List;
   end record;
   --  Linked list of Strings. Used for list variables
   --  and list array elements.

   procedure Free is new Ada.Unchecked_Deallocation
     (String_Element, String_List);

   type Variable_Kind is (Undefined, List, Single);
   --  Different kinds of variables

   type Variable_Value (Kind : Variable_Kind := Undefined) is record
      case Kind is
         when Undefined =>
            null;
         when List =>
            Values : String_List;
         when Single =>
            Value : String_Access;
      end case;
   end record;
   --  Values for variables and array elements

   Nil_Variable_Value : constant Variable_Value := (Kind => Undefined);
   --  Value of a non existing variable or array element.

   type Variable_Element;
   type Variable_List is access Variable_Element;
   type Variable_Element is record
      Next  : Variable_List;
      Name  : String_Access;
      Value : Variable_Value;
   end record;
   --  A list of variable. Used in declarations.

   type Array_Component;
   type Array_Component_Reference is access Array_Component;
   type Array_Component is record
      Index : String_Access;
      Value : Variable_Value;
      Next  : Array_Component_Reference;
   end record;
   --  An array. Each Array_Component represents an array element.

   type Array_Element;
   type Array_List is access Array_Element;
   type Array_Element is record
      Name  : String_Access;
      Value : Array_Component_Reference;
      Next  : Array_List;
   end record;
   --  A list of arrays. Each Array_Element represents an array.
   --  Used in declarations.

   type Package_Element;
   type Package_List is access Package_Element;
   type Declarations is record
      Variables : Variable_List;
      Arrays    : Array_List;
      Packages  : Package_List;
   end record;
   --  Declarations. Used in project structures and packages.

   type Package_Element is record
      Name   : String_Access;
      Decl   : Declarations;
      Parent : Package_List;
      Next   : Package_List;
   end record;
   --  A package. Includes declarations that may include
   --  other packages.

   Standard_Dot_Replacement      : constant String_Access :=
                                     new String'("-");
   Standard_Specification_Append : constant String_Access :=
                                     new String'(".ads");
   Standard_Body_Append          : constant String_Access :=
                                     new String'(".adb");

   type Casing_Type is (Lowercase, Uppercase, Mixedcase);

   function Image (Casing : Casing_Type) return String;
   --  Similar to 'Image

   function Value (Image : String) return Casing_Type;
   --  Similar to 'Value
   --  This is to avoid s-valenu in the closure of the tools
   --  Raises Constraint_Error if not a Casing_Type image.

   type Naming_Data is record
     Dot_Replacement      : String_Access := Standard_Dot_Replacement;
     Casing               : Casing_Type   := Lowercase;
     Specification_Append : String_Access := Standard_Specification_Append;
     Body_Append          : String_Access := Standard_Body_Append;
     Separate_Append      : String_Access := Standard_Body_Append;
     Specifications       : Array_Component_Reference := null;
     Bodies               : Array_Component_Reference := null;
   end record;
   --  A naming scheme.

   function Same_Naming_Scheme
     (Left, Right : Naming_Data)
      return        Boolean;
   --  Returns True if Left and Right are the same naming scheme
   --  not considering Specifications and Bodies.

   Standard_Naming_Data : constant Naming_Data :=
     (Dot_Replacement      => Standard_Dot_Replacement,
      Casing               => Lowercase,
      Specification_Append => Standard_Specification_Append,
      Body_Append          => Standard_Body_Append,
      Separate_Append      => Standard_Body_Append,
      Specifications       => null,
      Bodies               => null);
   --  The standard GNAT naming scheme.

   type Structure;
   type Reference is access Structure;
   --  GNAT Project File representation.

   type Reference_Data;
   type Reference_List is access Reference_Data;
   type Reference_Data is record
      Ref  : Reference;
      Next : Reference_List;
   end record;
   --  A list of GNAT Project Files. Used for imported projects.

   type Structure is record
      First_Referred_By  : Reference;
      Name               : String_Access;
      Path_Name          : String_Access;
      Directory          : String_Access;
      File_Name          : String_Access;
      Sources            : String_List;
      Source_Dirs        : String_List;
      Object_Directory   : String_Access;
      Modifies           : Reference;
      Modified_By        : Reference;
      Naming             : Naming_Data := Standard_Naming_Data;
      Decl               : Declarations;
      Imported_Projects  : Reference_List;
      Include_Path       : String_Access;
      Objects_Path       : String_Access;
      Gnat_Adc_Generated : Boolean := False;
   end record;
   --  GNAT Project File representation.

end Prj;
