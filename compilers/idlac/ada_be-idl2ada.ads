------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Types; use Idl_Fe.Types;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;
pragma Elaborate_All (Ada_Be.Source_Streams);

with Ada_Be.Mappings.CORBA;

package Ada_Be.Idl2Ada is

   procedure Generate
     (Use_Mapping : Ada_Be.Mappings.Mapping_Type'Class;
      Node        : Node_Id;
      Implement   : Boolean                            := False;
      Intf_Repo   : Boolean                            := False;
      To_Stdout   : Boolean                            := False);
   --  Generate the Ada mapping of the IDL tree
   --  rooted at Node.
   --  If Implement is true, produce only a template
   --  for the Impl package of each interface, to
   --  be completed by the user.
   --  If Intf_Repo is true, also produce CORBA
   --  Interface Repository packages.
   --  If To_Stdout is true, all produced source code
   --  is emitted on standard output (e. g. for use
   --  with GNATCHOP).

private

   function Ada_Type_Name (Node : Node_Id) return String;
   --  The name of the Ada type that maps Node.
   --  This is the fully qualified name.

   function Ada_Operation_Name
     (Node : Node_Id)
     return String;
   --  The name of the Ada subprogram that maps
   --  K_Operation Node.

   function Repository_Id_Name
     (Node : Node_Id)
     return String;
   --  The name of the Ada constant that contains
   --  the repository ID of K_Named Node.

   function Ada_TC_Name (Node : Node_Id) return String;
   --  The name of the typecode corresponding to an Ada type

   function Ada_Full_TC_Name (Node : Node_Id) return String;
   --  The full name of the typecode corresponding to an Ada type

   --------------------------------------
   -- Top-level generation subprograms --
   --------------------------------------

   procedure Gen_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
--    procedure Gen_Node_Stubs_Body
--      (CU   : in out Compilation_Unit;
--       Node : Node_Id);
   --  Generate the stubs code for a node.

   -------------------------
   -- Utility subprograms --
   -------------------------

   function Idl_Operation_Id
     (Node : Node_Id)
     return String;
   --  The GIOP operation identifier (to use in
   --  a GIOP Request message) corresponding
   --  to K_Operation Node.

   procedure Add_With_Entity
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Add a semantic dependency of CU on the
   --  package that contains the mapping of
   --  the entity defined by Node.

   function Helper_Unit (Node : Node_Id) return String;
   --  The name of the Helper unit containing helper subprograms for Node
   --  (including From_Any and To_Any).

   function TC_Unit (Node : Node_Id) return String;
   --  The name of the Helper unit containing the TypeCode for Node

   function Conditional_Call
     (Func      : String;
      Only_When : Boolean;
      Expr      : String) return String;
   --  Return Func (Expr) if Only_When is true, Expr otherwise

   procedure Gen_When_Clause
     (CU   : in out Compilation_Unit;
      Node : Node_Id;
      Default_Case_Seen : in out Boolean);
   --  Generate "when" clause for union K_Case Node.
   --  If this K_Case has a "default:" label, then
   --  Default_Case_Seen is set to True, else its
   --  value is left unchanged.

   procedure Gen_When_Others_Clause
     (CU   : in out Compilation_Unit);
   --  Generate a "when others => null;" clause.

   procedure Gen_Operation_Profile
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Object_Type : String;
      With_Name   : Boolean          := True;
      Is_Delegate : Boolean          := False);
   --  Generate the profile for an K_Operation node,
   --  with the Self formal parameter mode and type taken
   --  from the Object_Type string.
   --  If With_name is false, then the profile is generated, without
   --  the subprogram name, to create an access to subprogram type.
   --  If Delegate is True, "with" is added in front of the declaration
   --  and "is <>" at the end.

   procedure Gen_Initializer_Profile
     (CU : in out Compilation_Unit;
      Return_Type : String;
      Node : Node_Id);
   --  Generate the profile for an K_Initializer node,
   --  with the specified Return_Type

   procedure Gen_Local_Is_A
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate a function that checks locally whether a
   --  given repository ID denotes an ancestor type of
   --  Node.

   procedure Gen_Constant_Value
     (CU   : in out Compilation_Unit;
      Expr : Node_Id;
      Typ  : Node_Id);
   --  Generate the representation of a constant expression. Expr is the
   --  expression node, and Typ is the IDL type of the expression. Typ may be
   --  No_Node for integer constants, in which case the context must expect
   --  a Standard.Integer value.

   procedure Gen_Node_Default
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the text for a node whose mapping is
   --  common to all generated files.

   procedure Gen_Forward_Conversion
     (CU        : in out Compilation_Unit;
      T_Node    : Node_Id;
      Direction : String;
      What      : String);
   pragma Unreferenced (Gen_Forward_Conversion);
   --  Generate a call to a forward <-> actual reference conversion,
   --  if necessary.

   -------------------
   -- Text handling --
   -------------------

   function Justify (S : String; Max : Integer) return String;

   ---------------------
   -- User diversions --
   ---------------------

   Deferred_Initialization : constant Source_Streams.Diversion
                               := Source_Streams.Allocate_User_Diversion;
   --  Body of initialization subprogram

   Initialization_Dependencies : constant Source_Streams.Diversion
                                   := Source_Streams.Allocate_User_Diversion;
   --  List of initialization dependencies

   Operation_Body : constant Source_Streams.Diversion
                      := Source_Streams.Allocate_User_Diversion;
   --  Body of operation stub

   ------------------------------------------
   -- The current language mapping variant --
   ------------------------------------------

   type CORBA_Mapping_Access
     is access Ada_Be.Mappings.CORBA.CORBA_Mapping_Type'Class;
   Mapping : CORBA_Mapping_Access;

   ---------------
   -- Shortcuts --
   ---------------

   procedure NL
     (CU : in out Compilation_Unit)
     renames New_Line;
   procedure PL
     (CU   : in out Compilation_Unit;
      Line : String)
     renames Put_Line;

   procedure II
     (CU : in out Compilation_Unit)
     renames Inc_Indent;
   procedure DI
     (CU : in out Compilation_Unit)
     renames Dec_Indent;

end Ada_Be.Idl2Ada;
