------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                A D A _ B E . S O U R C E _ S T R E A M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Ada_BE.Source_Streams is

   Indent_Size : constant := 3;

   type Compilation_Unit is private;
   --  A complete compilation unit.

   procedure Set_Empty (Unit : in out Compilation_Unit);
   --  Set the Empty flag on the compilation unit.
   pragma Inline (Set_Empty);

   procedure Put
     (Unit : in out Compilation_Unit;
      Text : String);
   --  Append a text fragment to a compilation unit.

   procedure Put_Line
     (Unit : in out Compilation_Unit;
      Line : String);
   --  Append a whole line to a compilation unit.

   procedure New_Line (Unit : in out Compilation_Unit);
   --  Append a blank line to a compilation unit, or
   --  terminate an unfinished line.

   procedure Inc_Indent (Unit : in out Compilation_Unit);
   procedure Dec_Indent (Unit : in out Compilation_Unit);
   --  Increment or decrement the indentation level
   --  for the compilation unit.

   type Elab_Control_Pragma is
     (None,           --  Add no elaboration control pragma
      Elaborate,      --  Add a pragma Elaborate
      Elaborate_All); --  Add a pragma Elaborate_All

   procedure Add_With (Unit      : in out Compilation_Unit;
                       Dep       : String;
                       Use_It    : Boolean := False;
                       Elab_Control : Elab_Control_Pragma := None);
   --  Add Dep to the semantic dependecies of Unit,
   --  if it is not already present. If Use_It is true,
   --  a "use" clause will be added for that unit.
   --  Additionnally, an elaboration control pragma may
   --  be inserted according to Elab_Control.

   procedure Add_Elaborate_Body (Unit : in out Compilation_Unit);
   --  Add a pragma Elaborate_Body to the spec denoted by Unit.

   procedure Add_No_Warning (Unit : in out Compilation_Unit);
   --  Remove warning such as "Do not modify this file". Used for
   --  implementations.

   type String_Ptr is access String;
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   type Unit_Kind is
     (Unit_Spec, Unit_Body);

   function New_Package
     (Name : String;
      Kind : Unit_Kind)
     return Compilation_Unit;
   --  Prepare to generate a new compilation unit.

   procedure Generate
     (Unit : Compilation_Unit;
      Is_Generic_Instanciation : Boolean := False;
      To_Stdout : Boolean := False);
   --  Produce the source code for Unit.
   --  If Is_Generic_Instanciation, then Unit's Kind must
   --  be Unit_Spec, and Unit must be a library-level
   --  instanciation of a generic package.
   --  If To_Stdout, the code is emitted to standard output.

private

   type Dependency_Node;
   type Dependency is access Dependency_Node;

   type Dependency_Node is record
      Library_Unit : String_Ptr;
      Use_It : Boolean := False;
      Elab_Control : Elab_Control_Pragma := None;
      Next : Dependency;
   end record;

   type Compilation_Unit is new Ada.Finalization.Controlled with record

      Library_Unit_Name : String_Ptr;
      Kind              : Unit_Kind;
      Elaborate_Body    : Boolean := False;
      No_Warning        : Boolean := False;

      Context_Clause : Dependency
        := null;
      Library_Item   : Unbounded_String;
      Empty          : Boolean
        := True;
      Indent_Level   : Natural
        := 1;
      At_BOL         : Boolean := True;
      --  True if a line has just been ended, and the
      --  indentation space for the new line has not
      --  been written yet.

   end record;

   procedure Finalize (Object : in out Compilation_Unit);

end Ada_BE.Source_Streams;
