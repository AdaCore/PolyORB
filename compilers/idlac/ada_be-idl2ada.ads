------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
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

with Idl_Fe.Types; use Idl_Fe.Types;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;

package Ada_Be.Idl2Ada is

   procedure Generate
     (Node      : in Node_Id;
      Implement : Boolean := False;
      To_Stdout : Boolean := False);
   --  Generate the Ada mapping of the IDL tree
   --  rooted at Node.
   --  If Implement is true, produce only a template
   --  for the Impl package of each interface, to
   --  be completed by the user.
   --  If To_Stdout is true, all produced source code
   --  is emitted on standard output (e. g. for use
   --  with GNATCHOP).

private

   function Ada_Type_Name (Node : Node_Id) return String;
   --  The name of the Ada type that maps Node.
   --  This is the fully qualified name.

   function Ada_Type_Defining_Name
     (Node : Node_Id)
     return String;
   --  The defining name of the Ada type that maps Node
   --  (a K_Interface or K_ValueType).
   --  This is not the fully qualified name.

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

--    procedure Add_With_Stream
--      (CU : in out Compilation_Unit;
--       Node : Node_Id);
   --  Add a semantic dependency of CU on the
   --  package that contains the marshalling and
   --  unmarshalling subprograms for the type defined
   --  by Node.

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

   function Helper_Unit
     (Node : Node_Id)
     return String;
   --  The name of the Helper unit containing To_Any and
   --  From_Any for type Node.

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
      Object_Type : in String;
      Node        : in Node_Id;
      With_Name   : in Boolean := True;
      Delegate    : in Boolean := False);
   --  Generate the profile for an K_Operation node,
   --  with the Self formal parameter mode and type taken
   --  from the Object_Type string.
   --  If With_name is false, then the profile is generated, without
   --  the subprogram name, to create an access to subprogram type.
   --  If Delegate is True, "with" is added in front of the declaration
   --  and "is <>" at the end.

   procedure Gen_Initializer_Profile
     (CU : in out Compilation_Unit;
      Return_Type : in String;
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
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the representation of a constant expression.

   procedure Gen_Node_Default
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the text for a node whose mapping is
   --  common to all generated files.

   procedure Gen_Forward_Conversion
     (CU        : in out Compilation_Unit;
      T_Node    : in     Node_Id;
      Direction : in     String;
      What      : in     String);
   --  Generate a call to a forward <-> actual reference conversion,
   --  if necessary.

   -------------------
   -- Text handling --
   -------------------

   function Justify (S : in String; Max : in Integer) return String;

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
