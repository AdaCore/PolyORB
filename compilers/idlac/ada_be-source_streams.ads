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

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Ada_Be.Source_Streams is

   Indent_Size : constant := 3;

   type Compilation_Unit is private;
   --  A complete compilation unit.

   type Unit_Kind is
     (Unit_Spec, Unit_Body);
   --  The kind of a compilation unit.

   Max_Diversions : constant := 32;

   type Diversion is private;
   --  A compilation unit can have several diversions,
   --  each of which is a linear stream of source code
   --  lines. There can be at most Max_Diversions of these
   --  in one compilation unit.

   --  Predefined diversions:
   Visible_Declarations : constant Diversion;
   Private_Declarations : constant Diversion;
   Generic_Formals      : constant Diversion;
   Elaboration          : constant Diversion;
   --  The Visible_Declarations and Private_Declarations
   --  diversions correspond to the visible and private
   --  parts of the compilation unit's declarative region.
   --  The Elaboration diversion corresponds to the elaboration
   --  statements in a package body.

   type Elab_Control_Pragma is
     (None,           --  Add no elaboration control pragma
      Elaborate,      --  Add a pragma Elaborate
      Elaborate_All); --  Add a pragma Elaborate_All
   --  Possible elaboration control pragmas that can be added
   --  for a dependency.

   ---------------------------------------------------
   -- The following subprograms operate globally on --
   -- a compilation unit.                           --
   ---------------------------------------------------

   function Name (CU : Compilation_Unit) return String;
   --  Return the name of CU.

   function Allocate_User_Diversion return Diversion;
   --  Creates a system-wide user-defined diversion identifier
   --  and returns it.

   procedure Divert
     (CU     : in out Compilation_Unit;
      Whence : Diversion);
   --  Set CU's current diversion to Whence.
   --  If CU is a Unit_Spec, it is not allowed to set the current
   --  diversion to Elaboration.
   --  If CU is a Unit_Body, it is not allowed to set the current
   --  diversion to Private_Declarations or Generic_Formals.

   procedure Undivert
     (CU : in out Compilation_Unit;
      D  : Diversion);
   --  Insert the contents of diversion D into CU at the current
   --  position. D is emptied and unused after Undivert returns.

   procedure Add_With
     (Unit         : in out Compilation_Unit;
      Dep          :        String;
      Use_It       :        Boolean             := False;
      Elab_Control :        Elab_Control_Pragma := None;
      No_Warnings  :        Boolean             := False);
   --  Add Dep to the semantic dependecies of Unit,
   --  if it is not already present. If Use_It is true,
   --  a "use" clause will be added for that unit.
   --  Additionnally, an elaboration control pragma may
   --  be inserted according to Elab_Control.
   --  If No_Warnings is True, also emit a
   --    pragma Warnings (Off, Withed_Unit) (useful e.g.
   --  when no entities from the withed unit are referenced.)

   --  If Add_With is called several times for the same unit:
   --    - the unit is use'd if at least one call was made with
   --      Use_It set to True;
   --    - the elab control is set to Elaborate_All if any call
   --      was made with Elab_Control = Elaborate_All,
   --    - else the elab control is set to Elaborate if any call
   --      was made with Elab_Control = Elaborate,
   --    - else the elab control is set to None.

   procedure Add_Elaborate_Body (Unit : in out Compilation_Unit);
   --  Add a pragma Elaborate_Body to the spec denoted by Unit.

   procedure Suppress_Warning_Message (Unit : in out Compilation_Unit);
   --  Remove warning such as "Do not modify this file". Used for
   --  implementations.

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

   ----------------------------------------------------------------
   -- The following subprograms operate on the current diversion --
   ----------------------------------------------------------------

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

private

   type String_Ptr is access String;

   type Dependency_Node;
   type Dependency is access Dependency_Node;

   type Diversion is new Integer range 0 .. Max_Diversions - 1;

   Visible_Declarations : constant Diversion := 0;
   Private_Declarations : constant Diversion := 1;
   Generic_Formals      : constant Diversion := 2;
   Elaboration          : constant Diversion := 3;

   subtype Predefined_Diversions is Diversion
     range Visible_Declarations .. Elaboration;
   subtype User_Diversions is Diversion
     range Predefined_Diversions'Last + 1 .. Diversion'Last;

   type Diversion_Data is record
      Empty          : Boolean := True;
      --  True iff some text has been Insert'ed in this diversion.

      Library_Item   : Unbounded_String;
      Indent_Level   : Positive := 1;
      At_BOL         : Boolean := True;
      --  True if a line has just been ended, and the
      --  indentation space for the new line has not
      --  been written yet.
   end record;

   type Diversion_Set is array (Diversion) of aliased Diversion_Data;

   type Compilation_Unit is new Ada.Finalization.Controlled with record
      Library_Unit_Name : String_Ptr;
      Kind              : Unit_Kind;
      Elaborate_Body    : Boolean    := False;
      No_Warning        : Boolean    := False;

      Context_Clause    : Dependency := null;

      Current_Diversion : Diversion  := Visible_Declarations;

      Diversions        : Diversion_Set;
   end record;

   procedure Finalize   (CU : in out Compilation_Unit);

end Ada_Be.Source_Streams;
