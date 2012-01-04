------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C I A O . A S I S _ Q U E R I E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

--  Various ASIS queries for CIAO.
with Asis;
with Asis.Exceptions;

package CIAO.ASIS_Queries is

   ASIS_Inappropriate_Element : exception
     renames Asis.Exceptions.ASIS_Inappropriate_Element;
   ASIS_Failed                : exception
     renames Asis.Exceptions.ASIS_Failed;

   function Is_Ancestor (Ancestor_Compilation_Unit : Asis.Compilation_Unit;
                         Compilation_Unit          : Asis.Compilation_Unit)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Ancestor_Compilation_Unit - Specifies a putative ancestor.
   --  Compilation_Unit          - Specifies the compilation unit to query.
   --
   --  Returns True if, and only if, Ancestor_Compilation_Unit is
   --  an ancestor of Compilation_Unit.
   --
   --  All Unit_Kinds are expected.

   function Corresponding_Entity_Name_Definition
     (Reference : Asis.Expression)
     return Asis.Defining_Name;
   ----------------------------------------------------------------------------
   --  Reference   - Specifies an expression to query
   --
   --  Returns the defining_identifier, defining_character_literal,
   --  defining_operator_symbol, or defining_program_unit_name from the
   --  declaration of the referenced entity.
   --
   --  In case of renaming, the function returns the new name for the entity.
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference
   --
   --  Returns Element_Kinds:
   --       Not_An_Element
   --       A_Defining_Name
   --

   function Corresponding_Entity_Name_Declaration
     (Reference : Asis.Expression)
     return Asis.Declaration;
   ----------------------------------------------------------------------------
   --  Reference   - Specifies the expression to query
   --
   --  Returns the declaration that declared the entity named by the given
   --  reference.  The result is exactly the same as:
   --
   --       Result := Corresponding_Entity_Name_Definition (Subtype_Mark);
   --       if not Is_Nil (Result) then
   --           Result := Enclosing_Element (Result);
   --       end if;
   --       return Result;
   --
   --  Appropriate Element_Kinds:
   --       An_Expression
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference
   --
   --  Returns Element_Kinds:
   --       A_Declaration
   --       A_Nil_Element
   --

   function Is_Type_Conformant
     (Declaration_1, Declaration_2 : Asis.Declaration)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Declaration_1, Declaration_2 - Specify the declarations to query.
   --
   --  Returns True if, and only if, the two declarations are
   --  type conformant subprogram declarations.
   --
   --  Returns False for any unexpected Declaration.
   --
   --  Expected Declaration_Kinds:
   --       A_Procedure_Declaration
   --       A_Function_Declaration

   function Is_Tagged_Type (Declaration : Asis.Declaration)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Declaration               - Specifies the declaration to query.
   --
   --  Returns True if, and only if, the Declaration declares
   --  a subtype of a tagged type.
   --
   --  Returns False for any unexpected Declaration.
   --
   --  Expected Declaration_Kinds:
   --       A_Subtype_Declaration
   --       An_Ordinary_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration

   function Is_Limited_Type (Declaration : Asis.Declaration)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Declaration               - Specifies the declaration to query.
   --
   --  Returns True if, and only if, the Declaration declares
   --  a subtype of a limited type.
   --
   --  Returns False for any unexpected Declaration.
   --
   --  Expected Declaration_Kinds:
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Subtype_Declaration
   --       An_Ordinary_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration

   function Discrete_Subtype_Name
     (Definition : Asis.Definition)
     return Asis.Program_Text;
   ----------------------------------------------------------------------------
   --  Definition - Specifies the definition to query.
   --
   --  Returns the name of the named subtype underlying
   --  the (anonymous) discrete subtype denoted by a
   --  Discrete_Subtype_Definition.
   --
   --  Appropriate Definition_Kinds:
   --       A_Discrete_Subtype_Definition

   function Is_Overriding_Inherited_Subprogram
     (Subprogram_Declaration    : Asis.Declaration;
      Derived_Type_Declaration  : Asis.Declaration)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Subprogram_Declaration   - Specifies the subprogram declaration to query
   --  Derived_Type_Declaration - Specifies the derived type declaration
   --                             to query
   --
   --  Returns True if the Subprogram_Declaration overrides a subprogram
   --  that was implicitly inherited from the parent type in the
   --  Derived_Type_Declaration.
   --
   --  Returns False for any unexpected Declaration.
   --
   --  Expected Declaration_Kinds:
   --       A_Procedure_Declaration
   --       A_Function_Declaration

   function Is_Controlling_Result
     (Result_Profile          : Asis.Expression)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Result_Profile            - Specifies the Result_Profile to query.
   --
   --  Returns True if, and only if, Result_Profile is the result
   --  profile of a function declaration and it is a controlling result.
   --
   --  Returns False for any unexpected Expression.
   --
   --  Expected Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference

   function Is_Controlling_Formal
     (Parameter_Specification : Asis.Parameter_Specification)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Parameter_Specification   - Specifies the Parameter_Specification
   --                              to query.
   --
   --  Returns True if, and only if, the Parameter_Specification
   --  defines a controlling formal parameter of its subprogram declaration.

   function Controlling_Formal_Parameters (Declaration : Asis.Declaration)
     return Asis.Parameter_Specification_List;
   ----------------------------------------------------------------------------
   --  Declaration - Specifies the subprogram or entry declaration to query
   --
   --  Returns a list of parameter specifications in the formal part of the
   --  subprogram or entry declaration that are controlling formal parameters,
   --  in their order of appearance.
   --
   --  Returns a Nil_Element_List if the subprogram or entry has no
   --  controlling formal parameters.
   --
   --  Appropriate Declaration_Kinds:
   --       A_Procedure_Declaration
   --       A_Function_Declaration
   --       A_Procedure_Body_Declaration
   --       A_Function_Body_Declaration
   --       A_Procedure_Renaming_Declaration
   --       A_Function_Renaming_Declaration
   --       An_Entry_Declaration
   --       An_Entry_Body_Declaration
   --       A_Procedure_Body_Stub
   --       A_Function_Body_Stub
   --       A_Generic_Function_Declaration
   --       A_Generic_Procedure_Declaration
   --       A_Formal_Function_Declaration
   --       A_Formal_Procedure_Declaration
   --
   --  Returns Declaration_Kinds:
   --       A_Parameter_Specification

   function Enclosing_Basic_Declaration (Element : Asis.Element)
     return Asis.Declaration;
   ----------------------------------------------------------------------------
   --  Element        - Specifies the element to query
   --
   --  Returns the basic_declaration Element that immediately encloses the
   --  given element.  If Element is a basic_declaration, return Element.
   --
   --  Appropriate Element_Kinds:
   --       A_Pragma
   --       A_Defining_Name
   --       A_Definition
   --       An_Expression
   --       An_Association
   --       A_Declaration
   --
   --  Appropriate *_Kinds... XXX
   --

   function Isolated_Element_Image (Element : Asis.Element)
     return Asis.Program_Text;
   ----------------------------------------------------------------------------
   --  Element - Specifies the element to query
   --
   --  Returns a program text image of the element.  The image of an element
   --  can span more than one line, in which case the program text returned by
   --  the function Delimiter_Image separates the individual lines.  The bounds
   --  on the returned program text value are 1..N, N is as large as necessary.
   --
   --  Returns a null string if not Is_Text_Available(Element).
   --
   --  The image will not be space-padded.
   --
   --  NOTE: The image of a large element can exceed the range of Program_Text.
   --  In this case, the exception ASIS_Failed is raised with a Status of
   --  Capacity_Error.  Use the Lines function to operate on the image of large
   --  elements.
   --

   function Ada_Full_Name (Declaration : Asis.Declaration)
     return Asis.Program_Text;
   ----------------------------------------------------------------------------
   --  Element - Specifies the element to query
   --
   --  Returns a program text image of the fully qualified name of the entity
   --  (unambiguously) declared by the Declaration (which shall be a
   --  declaration with a unique name, such as a subprogram, type,
   --  subtype or formal parameter declaration.
   --
   --  The image will not be space-padded.
   --
   --  Appropriate Element_Kinds:
   --       A_Declaration
   --

   function Declaration_Name (Declaration : Asis.Declaration)
     return Asis.Defining_Name;
   ----------------------------------------------------------------------------
   --  Element - Specifies the element to query
   --
   --  Returns the single defining name of a declaration that has only one
   --  (such as a subprogram, type or formal parameter declaration).
   --
   --  Appropriate Element_Kinds:
   --       A_Declaration
   --

   function Is_Asynchronous
     (Element : Asis.Declaration)
     return Boolean;
   ----------------------------------------------------------------------------
   --  Element - Specifies the declaration to query.
   --  Compilation_Unit          - Specifies the compilation unit to query.
   --
   --  Returns True if, and only if, Element is a declaration to
   --  which a pragma Asynchronous applies (Declaration may be
   --  a remote procedure declaration, a RAS declaration or a RACW
   --  declaration).
   --
   --  Appropriate Element_Kinds:
   --       A_Declaration
   --

   type Unit_Categories is
     (Pure, Remote_Types, Remote_Call_Interface, Other);
   --  A type used to denote the category of a library unit
   --  as defined by the Distributed Systems Annex.

   function Unit_Category (LU : in ASIS.Compilation_Unit)
     return Unit_Categories;
   --  Returns the category (Pure, RT, RCI or Other)
   --  of a library unit.

end CIAO.ASIS_Queries;
