------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           B A C K E N D . B E _ C O R B A _ A D A . S K E L S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Values;    use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Common;      use Backend.BE_CORBA_Ada.Common;

with GNAT.Perfect_Hash_Generators; use GNAT.Perfect_Hash_Generators;

package body Backend.BE_CORBA_Ada.Skels is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No Skel package is generated for an abstract or a local
         --  interface.

         if FEN.Is_Abstract_Interface (E) or else
           FEN.Is_Local_Interface (E)
         then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Skeleton_Spec;

         N := Make_Pragma (Pragma_Elaborate_Body);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Skel_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));

         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;

         Pop_Entity;
      end Visit_Specification;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      Invoke_Then_Statements  : List_Id := No_List;
      Invoke_Elsif_Statements : List_Id := No_List;
      Package_Initialization  : List_Id := No_List;
      Choice_List             : List_Id := No_List;
      Dependency_List         : List_Id := No_List;

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id;
      --  Generate the body of the deferred initialization procedure

      function Gen_Invoke_Part (E : Node_Id) return Node_Id;
      --  Generate the statements related to the operation `E' in the
      --  Invoke procedure.

      function Invoke_Body
        (E              : Node_Id;
         Is_A_Invk_Part : Node_Id)
        return Node_Id;

      procedure Invoke_Declaration (L : List_Id);
      function Invoke_Spec return Node_Id;
      function Is_A_Invoke_Part return Node_Id;
      function Implicit_CORBA_Methods return List_Id;
      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id;
      procedure Skeleton_Initialization (L : List_Id);
      function Non_User_Exception_Handler return Node_Id;

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      --  The entities below are used in case of optimization using
      --  minimal perfect hash functions.

      N_Subprograms           : Unsigned_Long_Long;
      Register_Procedure_List : List_Id;
      Invoke_Subp_Bodies      : List_Id;
      Optim                   : Optimization;

      function Hash_Package_Name (E : Node_Id) return Name_Id;
      --  This function generates the name of the package that will
      --  contain the Hash function.

      procedure Initialize_Hash_Function_Optimization;
      --  This procedure initialise the lists above. It initialises
      --  the GNAT Perfect_Hash generator.

      procedure Achieve_Hash_Function_Optimization (E : Node_Id);
      --  This procedure computes the Perfect Hash function generator,
      --  produces it in an additional package and finally finalizes
      --  the generator.

      procedure Insert_And_Register_Statements (Subp_Name : Name_Id);
      --  This function inserts the name of the subprogram to the
      --  Perfect hash function generator. It produces also a
      --  "Register procedure" call statement which will be added to
      --  the Deferred_Initialization procedure statements.

      function Register_Procedure_Spec return Node_Id;
      function Register_Procedure_Body (E : Node_Id) return Node_Id;
      --  Generation of the Register_Procedure subprogram which is
      --  called to register a procedure in the hash table.

      ----------------------------------
      -- Deferred_Initialization_Body --
      ----------------------------------

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Profile    : constant List_Id := New_List (K_List_Id);
         Spec       : Node_Id;
         Statements : constant List_Id := New_List (K_List_Id);
      begin
         Spec := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
            No_List);
         N := Type_Def_Node (BE_Node (Identifier (E)));
         N := Next_Node (N);
         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_String),
            Make_List_Id (Expand_Designator (N)));
         Append_Node_To_List (N, Profile);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Servant_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Invoke)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (RE (RE_Register_Skeleton), Profile);
         Append_Node_To_List (N, Statements);

         --  In case of perfect hash function optimization, we
         --  register the Invoke_XXXX procedures at the package
         --  initialization.

         if Use_Minimal_Hash_Function then
            Append_Node_To_List
              (First_Node (Register_Procedure_List),
               Statements);
         end if;

         N := Make_Subprogram_Body (Spec, No_List, Statements);
         return N;
      end Deferred_Initialization_Body;

      ---------------------
      -- Gen_Invoke_Part --
      ---------------------

      function Gen_Invoke_Part (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         C                    : Node_Id;
         N                    : Node_Id;
         M                    : Node_Id;
         Param                : Node_Id;
         Param_Name           : Name_Id;
         Type_Node            : Node_Id;
         New_Name             : Name_Id;
         Params               : List_Id;
         Impl_Id              : Node_Id;
         Operation_Name       : Name_Id := FEN.IDL_Name (Identifier (E));
         Arg_Name             : Name_Id;
         Discret_Choice_Value : Value_Id;
         Record_Node          : Node_Id;
         Declarative_Part     : constant List_Id := New_List (K_List_Id);
         Statements           : constant List_Id := New_List (K_List_Id);
         Inv_Profile          : constant List_Id := New_List (K_List_Id);

         --  The flags below indicate whether the operation is mapped
         --  to an Ada function or an Ada procedure.

         Non_Void        : constant Boolean :=
           FEN.Kind (Type_Spec (E)) /= K_Void;
         Is_Ada_Function : constant Boolean :=
           Non_Void and then not Contains_Out_Parameters (E);

         function Exception_Handler_Alternative (E : Node_Id) return Node_Id;
         --  Generation of an alternative in the exception handler

         -----------------------------------
         -- Exception_Handler_Alternative --
         -----------------------------------

         function Exception_Handler_Alternative (E : Node_Id) return Node_Id is
            Result     : Node_Id;
            Selector   : Node_Id;
            Expression : Node_Id;
            N          : Node_Id;
            D          : constant List_Id := New_List (K_List_Id);
            S          : constant List_Id := New_List (K_List_Id);
         begin
            --  Getting the Exception name

            N := Expand_Designator
              (Stub_Node
               (BE_Node
                (Identifier
                 (Reference (E)))));

            Selector := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_E)),
               Object_Definition   => N);

            --  Declaration of the Members variable Getting the node
            --  corresponding to the declaration of the
            --  "Excp_Name"_Members type.

            N := Get_Type_Definition_Node (E);

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Members)),
               Object_Definition => N);
            Append_Node_To_List (N, D);

            --  Getting the node corresponding to the declaration of
            --  the Get_Members procedure. This procedure is declared
            --  2 nodes after the member type definition.

            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));
            N := Next_Node (Next_Node (N));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_E)),
                Make_Defining_Identifier (PN (P_Members))));
            Append_Node_To_List (N, S);

            --  Getting the node corresponding to the declaration of
            --  the To_Any procedure in the helper package.

            N := To_Any_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Members))));

            N := Make_Subprogram_Call
              (RE (RE_Set_Exception),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)), N));
            Append_Node_To_List (N, S);

            N := Make_Return_Statement (No_Node);
            Append_Node_To_List (N, S);

            Expression := Make_Block_Statement
              (Declarative_Part => D,
               Statements       => S);

            Result := Make_Component_Association (Selector, Expression);

            return Result;
         end Exception_Handler_Alternative;

      begin
         --  The first argument in the implementation call is an
         --  access to the object implementation. We create here this
         --  access and append it to the actual profile of the
         --  implementtation call.

         N := Implementation_Package (Current_Entity);
         N := First_Node (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Attribute_Reference (N, A_Class);
         C := Make_Explicit_Dereference (Make_Identifier (PN (P_Self)));
         N := Make_Type_Conversion (N, C);
         N := Make_Attribute_Reference (N, A_Access);
         Append_Node_To_List (N, Inv_Profile);

         --  Generate the code relative to the operation parameters

         Param := First_Entity (Parameters (E));

         while Present (Param) loop

            --  Get the parameter name

            Param_Name := To_Ada_Name
              (IDL_Name (Identifier (Declarator (Param))));

            --  Declare a local variable having the same type as the
            --  parameter.

            --  Get the Ada type generated from the parameter type
            --  spec.

            Type_Node := Get_Type_Definition_Node (Type_Spec (Param));

            --  Variable name

            Arg_Name := Map_Argument_Name (Param_Name);

            --  Declare the variable

            N :=  Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Arg_Name),
               Object_Definition   => Type_Node);
            Append_Node_To_List (N, Declarative_Part);

            --  Adding the variable to the profile of the
            --  implementation call.

            Append_Node_To_List
              (Make_Defining_Identifier (Arg_Name), Inv_Profile);

            --  The declaration below are not generated if the SII is
            --  used.

            if not Use_SII then
               --  Disable warnings on the declared variable

               N := Make_Pragma
                 (Pragma_Warnings,
                  Make_List_Id (RE (RE_Off), Make_Identifier (Arg_Name)));
               Append_Node_To_List (N, Declarative_Part);

               --  Prepare the actual profile of the Add_Item call

               Params := Make_List_Id (Make_Identifier (VN (V_Argument_List)));

               --  Declare the global variable corresponding to the
               --  argument name.

               C := Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_To_CORBA_String),
                  Actual_Parameter_Part => Make_List_Id
                  (Make_Literal (New_String_Value (Param_Name, False))));
               New_Name := Map_Argument_Identifier_Name
                 (Param_Name, Operation_Name);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (New_Name),
                  Constant_Present => True,
                  Object_Definition => RE (RE_Identifier_0),
                  Expression => C);
               Append_Node_To_List (N, BEN.Statements (Current_Package));

               --  Append the argument name to the actual profile of
               --  the Add_Item call.

               Append_Node_To_List (Make_Identifier (New_Name), Params);

               --  Declare the `Content' variable relative to the
               --  argument.

               C := Get_Wrap_Node
                 (FEU.Get_Original_Type_Declarator
                  (Type_Spec
                   (Param)));

               N := Make_Identifier (Arg_Name);

               --  Cast the parameter when necessary

               Cast_When_Necessary
                 (N,
                  Type_Spec (Param),
                  FEU.Get_Original_Type_Declarator (Type_Spec (Param)),
                  True);

               C := Make_Subprogram_Call
                 (C,
                  Make_List_Id (Make_Attribute_Reference
                                (N, A_Unrestricted_Access)));
               N := Make_Attribute_Reference (RE (RE_Content), A_Class);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (Map_Argument_Content_Name (Param_Name)),
                  Object_Definition   => N,
                  Expression          => C,
                  Aliased_Present     => True);
               Append_Node_To_List (N, Declarative_Part);

               --  Declaration of the `Any' argument variable

               C := Make_Attribute_Reference
                      (Make_Identifier
                         (Map_Argument_Content_Name (Param_Name)),
                       A_Unchecked_Access);

               C := Make_Subprogram_Call
                 (RE (RE_Get_Wrapper_Any),
                  Make_List_Id
                  (Get_TC_Node (Type_Spec (Param)),
                   C));

               N := Make_Object_Declaration
                      (Defining_Identifier => Make_Defining_Identifier
                                                (Map_Argument_Any_Name
                                                 (Param_Name)),
                       Constant_Present    => True,
                       Object_Definition   => RE (RE_Any),
                       Expression          => C);
               Append_Node_To_List (N, Declarative_Part);

               --  Append the Any variable the actual profile of the
               --  Add_Item call.

               N := Make_Identifier (Map_Argument_Any_Name (Param_Name));
               Append_Node_To_List (N, Params);

               --  Append the parameter mode is added to the Add_Item
               --  profile.

               if FEN.Parameter_Mode (Param) = Mode_Out then
                  N := RE (RE_ARG_OUT_0);
               elsif FEN.Parameter_Mode (Param) = Mode_In then
                  N := RE (RE_ARG_IN_0);
               else
                  N := RE (RE_ARG_INOUT_0);
               end if;

               Append_Node_To_List (N, Params);

               --  Add_Item call

               N := Make_Subprogram_Call
                 (RE (RE_Add_Item_0),
                  Params);
               Append_Node_To_List (N, Statements);
            end if;

            Param := Next_Entity (Param);
         end loop;

         --  Handling the case of a non void operation

         if Non_Void then
            --  Declare the Result_Ü variable

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Result)),
               Object_Definition   => Get_Type_Definition_Node
               (Type_Spec (E)));
            Append_Node_To_List (N, Declarative_Part);

            --  If this is a procedure then we add the result variable
            --  to the actual profile of the implementation call.

            if not Is_Ada_Function then
               Append_Node_To_List
                 (Make_Identifier (VN (V_Result)), Inv_Profile);
            end if;

            --  The following declaration are not for static request
            --  handling.

            if not Use_SII then
               --  Disable warnings on the Result_Ü variable

               N := Make_Pragma
                    (Pragma_Warnings,
                     Make_List_Id (RE (RE_Off),
                                   Make_Identifier (VN (V_Result))));
               Append_Node_To_List (N, Declarative_Part);

               --  Declaration of the `Content' argument variable

               C := Get_Wrap_Node
                 (FEU.Get_Original_Type_Declarator
                  (Type_Spec (E)));

               N := Make_Identifier (VN (V_Result));

               --  Cast the parameter when necessary

               Cast_When_Necessary
                 (N,
                  Type_Spec (E),
                  FEU.Get_Original_Type_Declarator (Type_Spec (E)),
                  True);

               C := Make_Subprogram_Call
                 (C,
                  Make_List_Id (Make_Attribute_Reference
                                (N,
                                 A_Unrestricted_Access)));

               N := Make_Attribute_Reference (RE (RE_Content), A_Class);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (Map_Argument_Content_Name (VN (V_Result))),
                  Constant_Present    => False,
                  Object_Definition   => N,
                  Expression          => C,
                  Aliased_Present     => True);
               Append_Node_To_List (N, Declarative_Part);

               --  Declaration of the `Any' argument variable

               C := Make_Attribute_Reference
                 (Make_Identifier
                  (Map_Argument_Content_Name (VN (V_Result))),
                  A_Unchecked_Access);
               C := Make_Subprogram_Call
                 (RE (RE_Get_Wrapper_Any),
                  Make_List_Id
                  (Get_TC_Node (Type_Spec (E)),
                   C));
               N := Make_Object_Declaration
                      (Defining_Identifier => Make_Defining_Identifier
                                                (Map_Argument_Any_Name
                                                   (VN (V_Result))),
                       Constant_Present    => True,
                       Object_Definition   => RE (RE_Any),
                       Expression          => C);
               Append_Node_To_List (N, Declarative_Part);
            end if;
         end if;

         --  In SII mode, the request payload has to be set before the
         --  request is processed. The Argument procedure is not the
         --  same as DII.

         if Use_SII then
            declare
               M : Node_Id;
            begin
               Params := New_List (K_List_Id);

               --  GIOP_Session is used to get the representation
               --  attribute (the declarative part the block).

               M := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Component)));

               N := Make_Subprogram_Call
                      (RE (RE_GIOP_Session), Make_List_Id (M));

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (VN (V_Session)),
                  Object_Definition   => RE (RE_GIOP_Session),
                  Renamed_Object      => N);
               Append_Node_To_List (N, Declarative_Part);

               N := Make_Subprogram_Call
                 (RE (RE_Get_Representation),
                  Make_List_Id (Make_Identifier (VN (V_Session))));

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (VN (V_Representation)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_CDR_Representation_Access),
                  Expression          => N);
               Append_Node_To_List (N, Declarative_Part);

               if not Use_Compiler_Alignment then
                  N := Expand_Designator
                    (Type_Def_Node
                     (BE_Node
                      (Identifier
                       (E))));

                  N := Make_Object_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (PN (P_Arg_List_Out)),
                     Aliased_Present     => True,
                     Object_Definition   => N);
                  Append_Node_To_List (N, Declarative_Part);
               end if;

               --  We need an Error Container

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (PN (P_Error)),
                  Object_Definition   => RE (RE_Error_Container));
               Append_Node_To_List (N, Declarative_Part);

               N := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (E))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Arg_List_In)),
                  Aliased_Present     => True,
                  Object_Definition   => N);
               Append_Node_To_List (N, Declarative_Part);

               Set_Str_To_Name_Buffer ("Processing request");
               Append_Node_To_List (Make_Ada_Comment (Name_Find), Statements);

               --  Unmarshall arguments

               C := Expand_Designator
                 (Unmarshaller_Node
                  (BE_Node
                   (Identifier
                    (E))));

               Append_Node_To_List (RE (RE_False), Params);

               M := Make_Identifier (PN (P_Arg_List_In));
               M := Make_Attribute_Reference (M, A_Access);

               Append_Node_To_List (M, Params);

               M := Make_Subprogram_Call
                 (RE (RE_Get_Buffer),
                  Make_List_Id (Make_Identifier (VN (V_Session))));

               Append_Node_To_List (M, Params);

               M := Make_Explicit_Dereference
                 (Make_Identifier
                    (VN (V_Component)));

               Append_Node_To_List
                 (Make_Explicit_Dereference
                    (Make_Identifier
                       (VN (V_Representation))),
                  Params);

               Append_Node_To_List
                 (Make_Literal (New_Integer_Value (8, 1, 10)), Params);

               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Error)), Params);

               --  The unmarshaller method call

               N := Make_Subprogram_Call (C, Params);
               Append_Node_To_List (N, Statements);

               --  Handling error

               C := Make_Subprogram_Call
                 (RE (RE_Found),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Error))));
               N := Make_Subprogram_Call
                 (RE (RE_Raise_From_Error),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Error))));

               N := Make_If_Statement
                 (Condition       => C,
                  Then_Statements => Make_List_Id (N));

               Append_Node_To_List (N, Statements);
            end;
         else
            Set_Str_To_Name_Buffer ("Processing request");
            Append_Node_To_List (Make_Ada_Comment (Name_Find), Statements);

            N := Make_Subprogram_Call
              (RE (RE_Arguments_1),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_Node_To_List (N, Statements);
         end if;

         --  The bloc above implements the generation of:

         --  * The call of the corresponding method implemented by the
         --  programmer.

         --  * The handling of possible exceptions thrown by the
         --  method.

         --  If the method could potentially throw an exception, the
         --  generated code will be put inside a block statement.
         --  Otherwise, no additional block statements will be used.

         declare
            Inner_Statements  : List_Id := No_List;
            Inner             : Boolean := False;
            Exception_Handler : List_Id := No_List;
            Excp_Node         : Node_Id;
            Predefined_Entity : RE_Id;
         begin

            --  Looking whether the operation throws exceptions and
            --  setting Inner_statement to the corresponding value.

            if not FEU.Is_Empty (Exceptions (E)) then
               Inner_Statements  := New_List (K_List_Id);
               Exception_Handler := New_List (K_List_Id);
               Inner             := True;

               --  Creating the exception handler statements

               Excp_Node := First_Entity (Exceptions (E));

               while Present (Excp_Node) loop
                  N := Exception_Handler_Alternative (Excp_Node);
                  Append_Node_To_List (N, Exception_Handler);
                  Excp_Node := Next_Entity (Excp_Node);
               end loop;
            else
               Inner_Statements := Statements;
            end if;

            --  In the case of SII, the parameter are set from the
            --  Args record.

            if Use_SII then
               Record_Node := Make_Identifier (PN (P_Arg_List_In));

               Param := First_Entity (Parameters (E));
               while Present (Param) loop
                  if  FEN.Parameter_Mode (Param) = Mode_In
                    or else FEN.Parameter_Mode (Param) = Mode_Inout
                  then
                     --  Get the parameter name

                     Param_Name := To_Ada_Name
                       (IDL_Name (Identifier (Declarator (Param))));

                     --  Variable name

                     Arg_Name := Map_Argument_Name (Param_Name);

                     --  Preparing the assigned value

                     --  Getting the record field

                     C := Make_Selected_Component
                       (Record_Node,
                        Make_Defining_Identifier (Param_Name));

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (Arg_Name),
                        C);
                     Append_Node_To_List (N, Inner_Statements);
                  end if;

                  Param := Next_Entity (Param);

               end loop;
            end if;

            --  Call Implementation

            Set_Str_To_Name_Buffer ("Call Implementation");
            Append_Node_To_List (Make_Ada_Comment (Name_Find),
                                 Inner_Statements);

            --  If the subprogram is inherited from a CORBA predefined
            --  entity, we must fetch this entity instead of the
            --  automatically generated one.

            Predefined_Entity := Get_Predefined_CORBA_Entity (E);

            if Predefined_Entity /= RE_Null then
               --  Do not add a with clause since the parent unit will
               --  be modified.

               C := Selector_Name (RE (Predefined_Entity, False));
            else
               C := Defining_Identifier (Impl_Node (BE_Node (Identifier (E))));
            end if;

            --  Re-adjusting the parent unit name of the
            --  operation. This is necessary in the case of operations
            --  or attributes inherited from the second until the last
            --  parent (multiple inheritance)

            Impl_Id := Make_Selected_Component
              (Defining_Identifier
               (Implementation_Package
                (Current_Entity)),
               Copy_Node (C));

            C := Make_Subprogram_Call
              (Copy_Expanded_Name (Impl_Id), Inv_Profile);

            if Is_Ada_Function then
               --  Cast class-wide results

               if Is_Class_Wide (E) then
                  C := Make_Type_Conversion
                    (Get_Type_Definition_Node (Type_Spec (E)), C);
               end if;

               C := Make_Assignment_Statement
                 (Make_Defining_Identifier (VN (V_Result)), C);
            end if;

            Append_Node_To_List (C, Inner_Statements);

            if Inner then
               Append_Node_To_List
                 (Make_Block_Statement
                  (Declarative_Part => No_List,
                   Statements => Inner_Statements,
                   Exception_Handler => Exception_Handler),
                  Statements);
            end if;
         end;

         --  Set Result

         if Non_Void then
            Set_Str_To_Name_Buffer ("Setting the result");
            Append_Node_To_List (Make_Ada_Comment (Name_Find), Statements);

            if Use_SII then
               C := Make_Selected_Component
                 (PN (P_Arg_List_Out), PN (P_Returns));

               N := Make_Assignment_Statement
                 (C, Make_Identifier (VN (V_Result)));
               Append_Node_To_List (N, Statements);
            else
               N := Make_Subprogram_Call
                 (RE (RE_Set_Result),
                  Make_List_Id
                  (Make_Identifier (PN (P_Request)),
                   Make_Identifier (Map_Argument_Any_Name (VN (V_Result)))));
               Append_Node_To_List (N, Statements);
            end if;
         end if;

         --  Setting out arguments

         if Use_SII then
            --  Out parameters of the Operation

            Param := First_Entity (Parameters (E));
            while Present (Param) loop
               if  FEN.Parameter_Mode (Param) = Mode_Out
                 or else FEN.Parameter_Mode (Param) = Mode_Inout then
                  Set_Str_To_Name_Buffer ("Setting out argument");
                  Append_Node_To_List
                    (Make_Ada_Comment (Name_Find), Statements);

                  Param_Name := To_Ada_Name
                    (IDL_Name (Identifier (Declarator (Param))));
                  Arg_Name := Map_Argument_Name (Param_Name);

                  C := Make_Defining_Identifier (Param_Name);

                  C := Make_Selected_Component
                    (Make_Identifier (PN (P_Arg_List_Out)), C);
                  N := Make_Assignment_Statement
                    (C, Make_Identifier (Arg_Name));
                  Append_Node_To_List (N, Statements);

               end if;

               Param := Next_Entity (Param);
            end loop;
         else
            --  Simply call `Clone_Out_Args' in all cases

            N := Make_Subprogram_Call
              (RE (RE_Clone_Out_Args),
               Make_List_Id (Make_Identifier (VN (V_Argument_List))));
            Append_Node_To_List (N, Statements);
         end if;

         if Use_SII then
            if Use_Compiler_Alignment then
               declare
                  Disc     : constant List_Id := New_List (K_List_Id);
                  Access_T : Node_Id;
                  Blk_Stat : constant List_Id := New_List (K_List_Id);
                  Dec_Stat : constant List_Id := New_List (K_List_Id);
                  J        : Unsigned_Long_Long;
               begin
                  if Non_Void then
                     C := Make_Defining_Identifier (PN (P_Returns));
                     Marshall_Args (Blk_Stat,
                                    Type_Spec (E),
                                    C,
                                    Make_Identifier (VN (V_Result)));

                     Get_Discriminants_Value
                       (E,
                        Type_Spec (E),
                        Disc);
                  end if;

                  Param := First_Entity (Parameters (E));

                  while Present (Param) loop
                     if  FEN.Parameter_Mode (Param) = Mode_Out
                       or else FEN.Parameter_Mode (Param) = Mode_Inout then
                        Param_Name := To_Ada_Name
                          (IDL_Name (Identifier (Declarator (Param))));
                        Arg_Name := Map_Argument_Name (Param_Name);

                        C := Make_Defining_Identifier (Arg_Name);
                        Marshall_Args (Blk_Stat,
                                       Type_Spec (Param),
                                       C);

                        Get_Discriminants_Value (Param,
                                                 Type_Spec (Param),
                                                 Disc);
                     end if;

                     Param := Next_Entity (Param);
                  end loop;

                  C := Expand_Designator
                    (Args_Out_Node
                     (BE_Node
                      (Identifier
                       (E))));

                  Access_T := Expand_Designator
                    (Access_Args_Out_Node
                     (BE_Node
                      (Identifier
                       (E))));

                  N := Make_Subprogram_Call (C, Disc);

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (VN (V_Args_Out)),
                     Object_Definition   => Access_T,
                     Expression          => Make_Object_Instantiation (N));
                  Append_Node_To_List (N, Dec_Stat);

                  C := Make_Attribute_Reference
                    (Make_Explicit_Dereference
                       (Make_Identifier
                          (VN (V_Args_Out))),
                     A_Address);

                  J := Unsigned_Long_Long (Length (Disc));
                  N := Make_Subprogram_Call
                    (RE (RE_Insert_Raw_Data),
                     Make_List_Id
                     (Make_Identifier (PN (P_Request)),
                      C,
                      Make_Attribute_Reference
                      (Make_Explicit_Dereference
                       (Make_Identifier
                        (VN (V_Args_Out))),
                       A_Size),
                      Make_Literal (New_Integer_Value (J, 1, 10)),
                      Make_Identifier (VN (V_Buffer))));
                  Append_Node_To_List (N, Blk_Stat);

                  N := Make_Block_Statement
                    (Declarative_Part => Dec_Stat,
                     Statements       => Blk_Stat);
                  Append_Node_To_List (N, Statements);
               end;
            else
               --  The marshaller method

               C := Expand_Designator
                 (Marshaller_Node
                  (BE_Node
                   (Identifier
                    (E))));

               Params := New_List (K_List_Id);
               Append_Node_To_List (RE (RE_False), Params);

               M := Make_Identifier (PN (P_Arg_List_Out));
               M := Make_Attribute_Reference (M, A_Access);
               Append_Node_To_List (M, Params);

               Append_Node_To_List
                 (Make_Defining_Identifier (VN (V_Buffer)), Params);
               M := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Component)));

               --  GIOP_Session is used to get the representation
               --  attribute (the declarative part the block).

               N := Make_Subprogram_Call
                 (RE (RE_GIOP_Session),
                  Make_List_Id (M));

               N := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Representation)));
               Append_Node_To_List (N, Params);

               Append_Node_To_List (Make_Literal (Int1_Val), Params);

               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Error)), Params);

               --  The marshaller method call

               N := Make_Subprogram_Call (C, Params);
               Append_Node_To_List (N, Statements);

               --  If any error we raise a program_error

               N := Make_Subprogram_Call
                 (RE (RE_Found),
                  Make_List_Id (Make_Identifier (PN (P_Error))));

               N := Make_If_Statement
                 (Condition       => N,
                  Then_Statements => Make_List_Id
                    (Make_Raise_Statement
                     (Make_Identifier
                      (EN (E_Program_Error)))));
               Append_Node_To_List (N, Statements);

               --  Add the buffer as a QoS parameter for the request

               Set_Str_To_Name_Buffer
                 ("Add the buffer to the request QoS parameters");
               Append_Node_To_List (Make_Ada_Comment (Name_Find), Statements);

               N := Make_Record_Aggregate
                 (Make_List_Id
                  (RE (RE_GIOP_Static_Buffer),
                   Make_Defining_Identifier (VN (V_Buffer))));

               N := Make_Object_Instantiation
                 (Make_Qualified_Expression
                  (RE (RE_QoS_GIOP_Static_Buffer_Parameter),
                   N));

               N := Make_Subprogram_Call
                 (RE (RE_Add_Request_QoS),
                  Make_List_Id
                  (Make_Defining_Identifier (VN (V_Request)),
                   RE (RE_GIOP_Static_Buffer),
                   N));

               Append_Node_To_List (N, Statements);
            end if;
         end if;

         --  Map the operation name

         Operation_Name := Map_Operation_Name_Literal (E);

         --  If no optimization is requested by the user, we generate
         --  an elsif statement. Else, we generate an case statement
         --  alternative

         if not Use_Minimal_Hash_Function then
            C := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Operation_Name, False)));

            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);
            N := Make_Elsif_Statement
              (C, Make_List_Id (N));
         else
            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Insert_And_Register_Statements (Operation_Name);

            --  Prepare the case alternative
            --  * Discret Choice : value of N_Subprogram minus 1

            Discret_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);

            N := Make_Case_Statement_Alternative
              (Make_List_Id (Make_Literal (Discret_Choice_Value)),
               Make_List_Id (N));
         end if;

         return N;
      end Gen_Invoke_Part;

      -----------------
      -- Invoke_Body --
      -----------------

      function Invoke_Body
        (E              : Node_Id;
         Is_A_Invk_Part : Node_Id)
        return Node_Id
      is
         N                 : Node_Id;
         Spec              : Node_Id;
         D                 : constant List_Id := New_List (K_List_Id);
         C_1               : Node_Id;
         Else_Statements   : constant List_Id := New_List (K_List_Id);
         Invoke_Statements : constant List_Id := New_List (K_List_Id);
         Exception_Handler : Node_Id;
         Is_A_Lowered_Name : Name_Id;
      begin
         Spec := Invoke_Spec;

         --  The declarative part

         Invoke_Declaration (D);

         --  We don't create the request if the SII is used

         if not Use_SII then
            N := Make_Subprogram_Call
              (RE (RE_Create_List),
               Make_List_Id
               (Make_Literal (Int0_Val),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_Node_To_List (N, Invoke_Statements);
         end if;

         if not Use_Minimal_Hash_Function then
            Append_Node_To_List (Is_A_Invk_Part, Invoke_Then_Statements);

            Set_Str_To_Name_Buffer ("_is_a");
            Is_A_Lowered_Name := Name_Find;

            C_1 := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Is_A_Lowered_Name, False)));
         else
            Append_Node_To_List (Is_A_Invk_Part, Invoke_Subp_Bodies);

            N := Make_Selected_Component
              (Make_Defining_Identifier (Hash_Package_Name (E)),
               Make_Defining_Identifier (SN (S_Hash)));

            --  Calculate the hash code of the operation

            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Defining_Identifier
                (VN (V_Operation))));
            N := Make_Assignment_Statement
              (Make_Defining_Identifier (VN (V_Index)),
               N);
            Append_Node_To_List (N, Invoke_Statements);

            --  Get the operation name corresponding to the hash code

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Invoke_Db)),
               Make_List_Id (Make_Defining_Identifier (VN (V_Index))));

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Invoke_Name_Access)),
               N);
            Append_Node_To_List (N, Invoke_Statements);

            --  The condition

            N := Make_Explicit_Dereference
              (Make_Identifier
                 (PN (P_Invoke_Name_Access)));

            C_1 := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               N);

            --  Generate the "case" statement after adding a "when
            --  others" clause.

            N := Make_Raise_Statement (Make_Identifier (EN (E_Program_Error)));
            N := Make_Case_Statement_Alternative
              (No_List,
               Make_List_Id (N));
            Append_Node_To_List (N, Invoke_Subp_Bodies);

            N := Make_Case_Statement
              (Make_Identifier (VN (V_Index)),
               Invoke_Subp_Bodies);
            Append_Node_To_List (N, Invoke_Then_Statements);
         end if;

         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Operation),
            Make_List_Id (RE (RE_Default_Sys_Member)));
         Append_Node_To_List (N, Else_Statements);

         N := Make_If_Statement
           (C_1,
            Invoke_Then_Statements,
            Invoke_Elsif_Statements,
            Else_Statements);

         Exception_Handler := Non_User_Exception_Handler;

         N := Make_Block_Statement
           (Declarative_Part  => No_List,
            Statements        =>
              Make_List_Id (N),
            Exception_Handler =>
              Make_List_Id (Exception_Handler));
         Append_Node_To_List (N, Invoke_Statements);

         --  Generation of the Invoke Procedure

         N := Make_Subprogram_Body (Spec, D, Invoke_Statements);

         return N;
      end Invoke_Body;

      ------------------------
      -- Invoke_Declaration --
      ------------------------

      procedure Invoke_Declaration (L : List_Id) is
         N : Node_Id;
      begin
         N := Make_Explicit_Dereference
           (Make_Identifier
            (PN (P_Request)));
         N := Make_Subprogram_Call
           (RE (RE_Operation),
            Make_List_Id (N));
         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            Make_List_Id (N));
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Operation)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_String_2),
            Expression          => N);
         Append_Node_To_List (N, L);

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Argument_List)),
            Object_Definition   => RE (RE_Ref_4));
         Append_Node_To_List (N, L);

         if Use_SII then
            declare
               C : Node_Id;
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Request_Access),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Request))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Request)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Request_Access),
                  Expression          => N);
               Append_Node_To_List (N, L);

               C := Make_Selected_Component
                 (VN (V_Request), PN (P_Dependent_Binding_Object));

               --  Request binding object

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Binding_Object)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Ref_10),
                  Expression          => C);
               Append_Node_To_List (N, L);

               --  The GIOP Session is the Component attribute
               --  Dependent_Binding_Object.

               C := Make_Subprogram_Call
                 (RE (RE_Get_Component),
                  Make_List_Id (Make_Identifier (VN (V_Binding_Object))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Component)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Component_Access),
                  Expression          => C);
               Append_Node_To_List (N, L);

               --  Buffer for marshalling the arguments

               C := Make_Object_Instantiation (RE (RE_Buffer_Type));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Buffer)),
                  Constant_Present => True,
                  Object_Definition => RE (RE_Buffer_Access),
                  Expression => C);
               Append_Node_To_List (N, L);
            end;
         end if;

         if Use_Minimal_Hash_Function then
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Index)),
               Object_Definition   => RE (RE_Natural));
            Append_Node_To_List (N, L);

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Invoke_Name_Access)),
               Object_Definition   => Make_Defining_Identifier
               (TN (T_String_Ptr)));
            Append_Node_To_List (N, L);

         end if;
      end Invoke_Declaration;

      -----------------
      -- Invoke_Spec --
      -----------------

      function Invoke_Spec return Node_Id is
         N       : Node_Id;
         Param   : Node_Id;
         Profile : List_Id;

      begin
         Profile := New_List (K_List_Id);
         Param   := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);
         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Request)),
            RE (RE_Object_Ptr));
         Append_Node_To_List (Param, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Invoke)),
            Profile,
            No_Node);
         return N;
      end Invoke_Spec;

      ----------------------
      -- Is_A_Invoke_Part --
      ----------------------

      function Is_A_Invoke_Part return Node_Id is
         N                    : Node_Id;
         Declarative_Part     : constant List_Id
           := New_List (K_Declaration_List);
         Statements           : constant List_Id
           := New_List (K_Statement_List);
         Discret_Choice_Value : Value_Id;

         Profile : List_Id;
      begin
         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Type_Id)),
            Object_Definition   => RE (RE_String_0));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (VN (V_Arg_Name_Type_Id)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Identifier_0),
            Expression          => Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               Make_List_Id (Make_Literal
                             (New_String_Value (VN (V_Type_Id), False)))));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (VN (V_Argument_Type_Id)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Any),
            Expression          => Make_Subprogram_Call
              (RE (RE_To_Any_0),
               Make_List_Id (Make_Defining_Identifier (VN (V_Type_Id)))));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Result)),
            Object_Definition   => RE (RE_Boolean));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements

         --  Call to CORBA.NVList.Add_Item

         Profile := New_List (K_Parameter_Profile);

         Append_Node_To_List (Make_Identifier (VN (V_Argument_List)),
                              Profile);
         Append_Node_To_List (Make_Identifier (VN (V_Arg_Name_Type_Id)),
                              Profile);
         Append_Node_To_List (Make_Identifier (VN (V_Argument_Type_Id)),
                              Profile);
         Append_Node_To_List (RE (RE_ARG_IN_0), Profile);

         N := Make_Subprogram_Call (RE (RE_Add_Item_0), Profile);
         Append_Node_To_List (N, Statements);

         --  Call to CORBA.ServerRequest.Arguments

         Profile := New_List (K_Parameter_Profile);

         Append_Node_To_List (Make_Identifier (PN (P_Request)),
                              Profile);
         Append_Node_To_List (Make_Identifier (VN (V_Argument_List)),
                              Profile);

         N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
         Append_Node_To_List (N, Statements);

         --  Assign the Type_Id

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Type_Id)),
            Make_Subprogram_Call (RE (RE_From_Any_0),
                                  Make_List_Id (Make_Identifier
                                                (VN (V_Argument_Type_Id)))));
         Append_Node_To_List (N, Statements);

         --  Call the implementation

         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            Make_List_Id (Make_Identifier (VN (V_Type_Id))));
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Is_A)),
            Make_List_Id (N));
         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Result)), N);
         Append_Node_To_List (N, Statements);

         --  Set the result

         Profile := New_List (K_Parameter_Profile);

         Append_Node_To_List (Make_Identifier (PN (P_Request)),
                              Profile);
         N := Make_Subprogram_Call (RE (RE_To_Any_0),
                                    Make_List_Id (Make_Identifier
                                                  (VN (V_Result))));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
         Append_Node_To_List (N, Statements);

         --  If no optimization is requested by the user, we generate
         --  an elsif statement. Else, we generate a case alternative
         --  statement

         if not Use_Minimal_Hash_Function then
            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);
         else
            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Set_Str_To_Name_Buffer ("_is_a");
            Insert_And_Register_Statements
              (Name_Find);

            --  Prepare the case alternative * Discret Choice : value
            --  of N_Subprogram minus 1

            Discret_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Block_Statement
              (Declarative_Part => Declarative_Part,
               Statements       => Statements);

            N := Make_Case_Statement_Alternative
              (Make_List_Id (Make_Literal (Discret_Choice_Value)),
               Make_List_Id (N));
         end if;

         return N;
      end Is_A_Invoke_Part;

      ----------------------------
      -- Implicit_CORBA_Methods --
      ----------------------------

      function Implicit_CORBA_Methods return List_Id is

         Result_List : constant List_Id := New_List (K_List_Id);

         procedure Add_Implicit_CORBA_Method
           (Declarations  : List_Id;
            Statements    : List_Id;
            Method_Name_1 : String;
            Method_Name_2 : String := "");
         --  To make the addition (or the removal) of an implicit
         --  CORBA method easier, we use this subprogram. It takes the
         --  method name, A declaration list and a statement list. It
         --  creates a block statement for each implicit method and
         --  fills a list depending on the optimization mode chosen by
         --  the user. If two method names correspond to the same
         --  treatment, the user may use the Method_Name_2 parameter.

         -------------------------------
         -- Add_Implicit_CORBA_Method --
         -------------------------------

         procedure Add_Implicit_CORBA_Method
           (Declarations  : List_Id;
            Statements    : List_Id;
            Method_Name_1 : String;
            Method_Name_2 : String := "")
         is
            N              : Node_Id;
            Discret_Choice : Node_Id;
            Op_Name        : Name_Id;
            C              : Node_Id;
         begin
            N := Make_Block_Statement (Declarative_Part => Declarations,
                                       Statements       => Statements);

            --  If no optimization is requested by the user, we
            --  generate an elsif statement. Else, we generate a case
            --  alternative statement

            if not Use_Minimal_Hash_Function then

               Set_Str_To_Name_Buffer (Method_Name_1);
               Op_Name := Name_Find;

               C := Make_Expression
                 (Make_Defining_Identifier (VN (V_Operation)),
                  Op_Equal,
                  Make_Literal (New_String_Value (Op_Name, False)));

               if Method_Name_2'Length /= 0 then
                  declare
                     C_2 : Node_Id;
                  begin
                     Set_Str_To_Name_Buffer (Method_Name_1);
                     Op_Name := Name_Find;
                     C_2 := Make_Expression
                       (Make_Defining_Identifier (VN (V_Operation)),
                        Op_Equal,
                        Make_Literal (New_String_Value (Op_Name, False)));
                     C := Make_Expression (C, Op_Or_Else, C_2);
                  end;
               end if;

               N := Make_Elsif_Statement
                 (C, Make_List_Id (N));
            else
               --  Insert the subprogram name into the hash function
               --  generator and add a call to Register_Procedure

               Set_Str_To_Name_Buffer (Method_Name_1);
               Insert_And_Register_Statements (Name_Find);

               --  Prepare the case alternative

               --  * Discret Choice : value of N_Subprogram minus 1

               Discret_Choice := Make_Literal
                 (New_Integer_Value (N_Subprograms - 1, 1, 10));

               if Method_Name_2'Length /= 0 then
                  declare
                     DC_2 : Node_Id;
                  begin
                     Set_Str_To_Name_Buffer (Method_Name_2);
                     Insert_And_Register_Statements (Name_Find);

                     DC_2 := Make_Literal
                       (New_Integer_Value (N_Subprograms - 1, 1, 10));

                     Discret_Choice := Make_Expression
                       (Discret_Choice, Op_Vertical_Bar, DC_2);
                  end;
               end if;

               N := Make_Case_Statement_Alternative
                 (Make_List_Id (Discret_Choice),
                  Make_List_Id (N));
            end if;

            Append_Node_To_List (N, Result_List);

         end Add_Implicit_CORBA_Method;

         N       : Node_Id;
      begin
         --  For each implicit CORBA Method, add a similar block
         --  statement

         --  The "Interface" implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List (K_Statement_List);
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List (K_Parameter_Profile);

            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);
            Append_Node_To_List (Make_Identifier (VN (V_Argument_List)),
                                 Profile);
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_Node_To_List (N, Statements);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List (K_Parameter_Profile);
            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               Make_List_Id (Make_Identifier (PN (P_Repository_Id))));
            N := Make_Subprogram_Call (RE (RE_Get_Interface_Definition),
                                       Make_List_Id (N));
            N := Make_Subprogram_Call (RE (RE_Ref_2),
                                       Make_List_Id (N));
            N := Make_Subprogram_Call (RE (RE_To_Any_3),
                                       Make_List_Id (N));
            Append_Node_To_List (N, Profile);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_Node_To_List (N, Statements);

            --  Add the handler

            Add_Implicit_CORBA_Method (No_List, Statements, "_interface");

         end;

         --  The Domain_Managers implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List (K_Statement_List);
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List (K_Parameter_Profile);

            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);
            Append_Node_To_List (Make_Identifier (VN (V_Argument_List)),
                                 Profile);
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_Node_To_List (N, Statements);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List (K_Parameter_Profile);
            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);

            N := Make_Subprogram_Call
              (RE (RE_Get_Domain_Managers),
               Make_List_Id (Make_Identifier (PN (P_Self))));
            Append_Node_To_List (N, Profile);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_Node_To_List (N, Statements);

            --  Add the handler

            Add_Implicit_CORBA_Method
              (No_List, Statements, "_domain_managers");
         end;

         --  The Non_Existent implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List (K_Statement_List);
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List (K_Parameter_Profile);

            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);
            Append_Node_To_List (Make_Identifier (VN (V_Argument_List)),
                                 Profile);
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_Node_To_List (N, Statements);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List (K_Parameter_Profile);
            Append_Node_To_List (Make_Identifier (PN (P_Request)), Profile);

            N := Make_Literal (New_Boolean_Value (False));
            N := Make_Qualified_Expression
              (Subtype_Mark => RE (RE_Boolean),
               Operand      => N);
            N := Make_Subprogram_Call
              (RE (RE_To_Any_0),
               Make_List_Id (N));
            Append_Node_To_List (N, Profile);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_Node_To_List (N, Statements);

            --  Add the handler

            Add_Implicit_CORBA_Method
              (No_List, Statements, "_non_existent", "_not_existent");
         end;

         return Result_List;
      end Implicit_CORBA_Methods;

      -----------------------
      -- Servant_Is_A_Body --
      -----------------------

      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id is
         Statements : constant List_Id := New_List (K_List_Id);
         N          : Node_Id;
      begin
         N := Implementation_Package (Current_Entity);
         N := First_Node
           (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Attribute_Reference (N, A_Class);
         N := Make_Expression
           (Make_Explicit_Dereference (Make_Defining_Identifier (PN (P_Obj))),
            Op_In,
            N);
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Statements);

         return  Make_Subprogram_Body (Spec, No_List, Statements);
      end Servant_Is_A_Body;

      -----------------------------
      -- Skeleton_Initialization --
      -----------------------------

      procedure Skeleton_Initialization (L : List_Id) is
         N                : Node_Id;
         V                : Value_Id;
         Dep              : Node_Id;
         Aggregates       : constant List_Id := New_List (K_List_Id);
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
      begin
         --  Declarative part
         --  Adding 'use' clauses to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements

         --  The package name

         N := Defining_Identifier (Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N), False);
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Name)),
            Expression    => N);
         Append_Node_To_List (N, Aggregates);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression    => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  The dependencies

         N := RE (RE_Empty);

         if not Is_Empty (Dependency_List) then
            Dep := First_Node (Dependency_List);

            while Present (Dep) loop
               N := Make_Expression (N, Op_And_Symbol, Dep);

               Dep := Next_Node (Dep);
            end loop;
         end if;

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Depends)),
            Expression    => N);
         Append_Node_To_List (N, Aggregates);

         --  Provides

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Provides)),
            Expression    => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Implicit)),
            Expression    => RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Init)),
            Expression    => Make_Attribute_Reference
              (Make_Identifier (SN (S_Deferred_Initialization)),
               A_Access));
         Append_Node_To_List (N, Aggregates);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_Node_To_List (N, Aggregates);

         --  Registering the module

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Operand      => Make_Record_Aggregate (Aggregates));

         N := Make_Subprogram_Call (RE (RE_Register_Module), Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         Append_Node_To_List (N, L);
      end Skeleton_Initialization;

      --------------------------------
      -- Non_User_Exception_Handler --
      --------------------------------

      function Non_User_Exception_Handler return Node_Id is
         Result     : Node_Id;
         Selector   : Node_Id;
         Expression : Node_Id;
         N          : Node_Id;
         D          : constant List_Id := New_List (K_List_Id);
         S          : constant List_Id := New_List (K_List_Id);
      begin
         --  Generation of the "E : others" statement

         Selector := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_E)),
            Object_Definition => No_Node);

         --  Body of the exception handler

         N := Make_Subprogram_Call
           (RE (RE_System_Exception_To_Any),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_E))));

         --  Set the exception

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Request)), N));
         Append_Node_To_List (N, S);

         --  Set the exception informations

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception_Information),
            Make_List_Id
            (Make_Identifier (PN (P_Request)),
             Make_Identifier (PN (P_E))));
         Append_Node_To_List (N, S);

         Expression := Make_Block_Statement
           (Declarative_Part => D,
            Statements       => S);

         Result := Make_Component_Association (Selector, Expression);

         return Result;
      end Non_User_Exception_Handler;

      -----------------------
      -- Hash_Package_Name --
      -----------------------

      function Hash_Package_Name (E : Node_Id) return Name_Id is
         pragma Assert (FEN.Kind (E) = K_Interface_Declaration);
      begin
         Get_Name_String
           (FEU.Fully_Qualified_Name
            (FEN.Identifier (E),
             Separator => "_"));
         Add_Str_To_Name_Buffer ("_Hash");

         return Name_Find;
      end Hash_Package_Name;

      -------------------------------------------
      -- Initialize_Hash_Function_Optimization --
      -------------------------------------------

      procedure Initialize_Hash_Function_Optimization is
      begin
         --  Checking whether the user chose to optimize memory space
         --  or CPU Time

         if Optimize_CPU and then not Optimize_Memory then
            Optim := CPU_Time;
         elsif Optimize_Memory and then not Optimize_CPU then
            Optim := Memory_Space;
         else
            declare Msg : constant String := "Cannot optimize CPU time"
              & " and memory space at the same time";
            begin
               raise Program_Error with Msg;
            end;
         end if;

         --  Initialize the lists and the number of subprograms

         N_Subprograms           := 0;
         Register_Procedure_List := New_List (K_List_Id);
         Invoke_Subp_Bodies      := New_List (K_List_Id);
      end Initialize_Hash_Function_Optimization;

      ----------------------------------------
      -- Achieve_Hash_Function_Optimization --
      ----------------------------------------

      procedure Achieve_Hash_Function_Optimization (E : Node_Id) is
         N     : Node_Id;
         V     : Natural;
         Seed  : constant Natural := 4321; --  Needed by the hash algorithm
         K_2_V : Float;                    --  The ratio of the algorithm
      begin
         --  We add a "with" clause to be able to use the "Hash"
         --  function

         Add_With_Package (Make_Defining_Identifier (Hash_Package_Name (E)));

         --  Declaration of the total number of subprograms

         N := Make_Literal (New_Integer_Value (N_Subprograms, 1, 10));
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_N_Operations)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Natural),
            Expression          => N);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Definition of a string access type

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (TN (T_String_Ptr)),
            Type_Definition     => Make_Access_Type_Definition
            (RE (RE_String_2)));
         Append_Node_To_List (N, Statements (Current_Package));

         --  Declaration of the hash table. The hash table size is
         --  equal to the number of subprograms

         N := Make_Range_Constraint
           (Make_Literal (Int0_Val),
            Make_Expression
            (Make_Defining_Identifier (PN (P_N_Operations)),
             Op_Minus,
             Make_Literal (Int1_Val)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Invoke_Db)),
            Object_Definition   => Make_Array_Type_Definition
            (Make_List_Id (N),
             Make_Defining_Identifier (TN (T_String_Ptr))),
            Expression          => Make_Record_Aggregate
            (Make_List_Id
             (Make_Component_Association
              (Selector_Name => No_Node, --  'others'
               Expression    => Make_Null_Statement))));
         Append_Node_To_List (N, Statements (Current_Package));

         --  Insert the spec and the body of the Register_Procedure
         --  procedure

         N := Register_Procedure_Spec;
         Append_Node_To_List (N, Statements (Current_Package));
         N := Register_Procedure_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Compute the hash function generator, we use all positions
         --  In the case of CPU time optimization, the algorithm
         --  should succeed from the first iteration. For the Memory
         --  space optimization the algorithm may fail, so we
         --  increment the number of the graph vertexes until it
         --  succeeds. We are sure that for V >= 257, the algorithm
         --  will succeed.

         V := 2 * Natural (N_Subprograms) + 1;
         loop
            K_2_V := Float (V) / Float (N_Subprograms);
            Initialize
              (Seed   => Seed,
               K_To_V => K_2_V,
               Optim  => Optim);

            begin
               Compute;
               exit;
            exception
               when others =>
                  if Optim = CPU_Time then
                     raise;
                  end if;

                  V := V + 1;
            end;
         end loop;

         --  Produce the package containing the Hash function

         Get_Name_String (Hash_Package_Name (E));
         Produce (Name_Buffer (1 .. Name_Len));

         --  Finalize the generator

         Finalize;
      end Achieve_Hash_Function_Optimization;

      ------------------------------------
      -- Insert_And_Register_Statements --
      ------------------------------------

      procedure Insert_And_Register_Statements
        (Subp_Name   : Name_Id)
      is
         Profile : constant List_Id := New_List (K_List_Id);
         N       : Node_Id;
      begin
         --  First of all, we increment the number of subprograms

         N_Subprograms := N_Subprograms + 1;

         --  Insert the subprogram name into the perfect hash table
         --  generator.

         Get_Name_String (Subp_Name);
         Insert (Name_Buffer (1 .. Name_Len));

         --  Generate the call to Register_Procedure, which put an
         --  access to the Invoke_XXXX in the right place into the
         --  hash table.

         N := Make_Literal
           (New_String_Value
            (Subp_Name, False));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Register_Procedure)),
            Profile);
         Append_Node_To_List (N, Register_Procedure_List);

      end Insert_And_Register_Statements;

      -----------------------------
      -- Register_Procedure_Spec --
      -----------------------------

      function Register_Procedure_Spec return Node_Id is
         N       : Node_Id;
         Profile : constant List_Id := New_List (K_List_Id);
      begin
         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Operation_Name)),
            Subtype_Mark        => RE (RE_String_2));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (SN (S_Register_Procedure)),
            Parameter_Profile   => Profile,
            Return_Type         => No_Node);

         return N;
      end Register_Procedure_Spec;

      -----------------------------
      -- Register_Procedure_Body --
      -----------------------------

      function Register_Procedure_Body (E : Node_Id) return Node_Id is
         Spec             : Node_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         N                : Node_Id;
      begin
         Spec := Register_Procedure_Spec;

         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (VN (V_Index)),
            Object_Definition   => RE (RE_Natural));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Invoke_Name_Access)),
            Object_Definition   => Make_Defining_Identifier
            (TN (T_String_Ptr)));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements part

         N := Make_Selected_Component
           (Make_Defining_Identifier (Hash_Package_Name (E)),
            Make_Defining_Identifier (SN (S_Hash)));

         N := Make_Subprogram_Call
           (N,
            Make_List_Id
            (Make_Defining_Identifier
             (PN (P_Operation_Name))));

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Index)),
            N);
         Append_Node_To_List (N, Statements);

         --  Test if the hash code was already found in which case
         --  raise a program error.

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            Make_List_Id (Make_Defining_Identifier (VN (V_Index))));

         N := Make_Expression
           (N,
            Op_Not_Equal,
            Make_Null_Statement);

         N := Make_If_Statement
           (Condition       => N,
            Then_Statements => Make_List_Id
            (Make_Raise_Statement
             (Make_Defining_Identifier
              (EN (E_Program_Error)))));
         Append_Node_To_List (N, Statements);

         --  Assigning the procedure actual name

         N :=  Make_Defining_Identifier (PN (P_Invoke_Name_Access));
         N := Make_Assignment_Statement
           (N,
            Make_Object_Instantiation
            (Make_Qualified_Expression
             (Subtype_Mark => RE (RE_String_2),
              Operand      => Make_Defining_Identifier
                (PN (P_Operation_Name)))));
         Append_Node_To_List (N, Statements);

         --  Update the hash table

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            Make_List_Id (Make_Defining_Identifier (VN (V_Index))));
         N := Make_Assignment_Statement
           (N,
            Make_Defining_Identifier (PN (P_Invoke_Name_Access)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body
           (Specification => Spec,
            Declarations  => Declarative_Part,
            Statements    => Statements);

         return N;
      end Register_Procedure_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N              : Node_Id;
         Param          : Node_Id;
         Profile        : constant List_Id := New_List (K_List_Id);
         Invk_Spec      : Node_Id;
         Invk_Body      : Node_Id;
         Is_A_Invk_Part : Node_Id;
         Implicit_CORBA : List_Id;
         Parent_Int     : Node_Id;
      begin
         --  No Skel package is generated for an abstract or a local
         --  interface.

         if FEN.Is_Abstract_Interface (E)
           or else FEN.Is_Local_Interface (E)
         then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));

         Set_Skeleton_Body;

         Invoke_Then_Statements := New_List (K_List_Id);
         Package_Initialization := New_List (K_List_Id);
         Dependency_List        := New_List (K_List_Id);

         --  If the user chose to generate optimised skeletons, we
         --  initialise the optimization related lists.

         if Use_Minimal_Hash_Function then
            Initialize_Hash_Function_Optimization;
            Choice_List := Invoke_Subp_Bodies;
         else
            Invoke_Elsif_Statements := New_List (K_List_Id);
            Choice_List := Invoke_Elsif_Statements;
         end if;

         N := First_Entity (Interface_Body (E));

         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parent interface
         --  including the first one.

         Map_Inherited_Entities_Bodies
           (Current_Interface    => E,
            Visit_Operation_Subp => Visit_Operation_Declaration'Access,
            Skel                 => True);

         --  We make a difference between the Is_A Method and the rest of
         --  implicit CORBA methods for two reasons:
         --  * Is_A is not implicit since it is declared in the stub.
         --  * in case of non-optimisation the _is_a test of the operation is
         --    always put at the beginning of the if .. elsif .. elsif
         --    statement to make the code generation of operation code simpler.

         Is_A_Invk_Part := Is_A_Invoke_Part;

         --  Here, we assign the list of the the implicit CORBA
         --  methods It's important to do this before the finalization
         --  of the hash function generator (in case of optimisation)
         --  so that all the hash keys could be inserted before the
         --  computation phase of the algorithm.

         Implicit_CORBA := Implicit_CORBA_Methods;

         --  At this point, all operations and attributes are
         --  visited. We achieve the perfect hash function generation
         --  and the building of the conditional structure which
         --  handles the request.

         if Use_Minimal_Hash_Function then
            Achieve_Hash_Function_Optimization (E);
         end if;

         --  Here, we append the implicit CORBA methods either to the
         --  elsif statements or to the case statement depending on
         --  the optimization mode chosen by the developer.

         Append_Node_To_List
           (First_Node (Implicit_CORBA),
            Choice_List);

         --  Build the Invoke procedure

         Invk_Spec := Invoke_Spec;
         Invk_Body := Invoke_Body (E, Is_A_Invk_Part);

         --  Add the Invoke procedure Spec

         Append_Node_To_List (Invk_Spec, Statements (Current_Package));

         --  Add the Invoke procedure Body

         Append_Node_To_List (Invk_Body, Statements (Current_Package));

         --  Generation of the Servant_Is_A function

         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Obj)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Servant_Is_A)),
            Profile,
            RE (RE_Boolean_0));
         Append_Node_To_List (N, Statements (Current_Package));

         N := Servant_Is_A_Body (N);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Generation of the Deferred_Initialization procedure

         N := Deferred_Initialization_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Make the current skeleton depend upon the skeletons of
         --  all the interface parent to guarantee their registration
         --  before the current skeleton.

         Parent_Int := First_Entity (Interface_Spec (E));

         while Present (Parent_Int) loop
            declare
               The_Interface : constant Node_Id := Reference (Parent_Int);
            begin
               if Present (BE_Node (Identifier (The_Interface))) then
                  Add_Dependency
                    (Expand_Designator
                     (Skeleton_Package
                      (IDL_Unit
                       (Package_Declaration
                        (BEN.Parent
                         (Type_Def_Node
                          (BE_Node
                           (Identifier
                            (The_Interface))))))),
                      False),
                     Dependency_List,
                     D_Skel,
                     True);
               end if;
            end;

            Parent_Int := Next_Entity (Parent_Int);
         end loop;

         Skeleton_Initialization (Package_Initialization);
         Set_Package_Initialization (Current_Package, Package_Initialization);

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Skel_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := Gen_Invoke_Part (E);
         Append_Node_To_List (N, Choice_List);
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));

         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;

         Pop_Entity;
      end Visit_Specification;

   end Package_Body;
end Backend.BE_CORBA_Ada.Skels;
