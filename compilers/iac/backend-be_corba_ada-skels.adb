------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           B A C K E N D . B E _ C O R B A _ A D A . S K E L S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with Ada.Directories;

with Namet;     use Namet;
with Values;    use Values;

with Flags;     use Flags;

with Frontend.Nodes; use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.IDL_To_Ada; use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;      use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;     use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;    use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Common;     use Backend.BE_CORBA_Ada.Common;

with GNAT.Perfect_Hash_Generators;

package body Backend.BE_CORBA_Ada.Skels is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEU renames Frontend.Nutils;
   package PHG renames GNAT.Perfect_Hash_Generators;

   use type PHG.Optimization;

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
         Append_To (Visible_Part (Current_Package), N);

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
      Invoke_Methods          : List_Id := No_List;
      Package_Initialization  : List_Id := No_List;
      Choice_List             : List_Id := No_List;
      Dependency_List         : List_Id := No_List;
      Has_Operations          : Boolean := False;
      Buffer_Necessary        : Boolean := False;

      function Gen_Invoke_Method
        (Operation_Name : Name_Id;
         Declarations   : List_Id;
         Statements     : List_Id) return Node_Id;
      --  Generates and returns a call to the Invoke_<Operation_Name>
      --  procedure. As a side effect, generates the spec and body of that
      --  procedure, and appends them to Invoke_Methods. There is one such
      --  procedure for each method, and they are called from the case
      --  statement in the Helper procedure below.

      function Gen_Invoke_Helper
        (Case_Statement : Node_Id) return Node_Id;
      --  Generates and returns a call to the Helper procedure. As a side
      --  effect, generates the spec and body of that procedure, and appends
      --  them to Invoke_Methods. The Helper procedure contains a case
      --  statement that calls the appropriate Invoke_<Operation_Name>. Note
      --  that this procedure is called "Helper" (as opposed to Invoke_Helper)
      --  to avoid possible conflict with some Invoke_<Operation_Name>.

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id;
      --  Generate the body of the deferred initialization procedure

      function Gen_Invoke_Part (E : Node_Id) return Node_Id;
      --  Generate an 'elsif' or 'when ...' containing the statements related
      --  to the operation `E' in the Invoke procedure.

      function Invoke_Body
        (E              : Node_Id;
         Is_A_Invk_Part : Node_Id)
        return Node_Id;
      --  Generate the body of procedure Invoke. This body contains one nested
      --  procedure Invoke_<Operation_Name> for each method, plus a procedure
      --  Helper, which contains a case statement calling all the procedures
      --  Invoke_<Operation_Name>. Invoke calls Helper and catches
      --  exceptions. One reason for separating out the nested procedures is to
      --  make the generated code compile in a reasonable amount of time/memory
      --  in sjlj mode. Also, it seems a bit more readable.

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
         Profile    : constant List_Id := New_List;
         Spec       : Node_Id;
         Statements : constant List_Id := New_List;
      begin
         Spec := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
            No_List);
         N := Type_Def_Node (BE_Node (Identifier (E)));
         N := Next_Node (N);
         N := Expand_Designator (N);
         Append_To (Profile, N);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Servant_Is_A)), A_Access);
         Append_To (Profile, N);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Is_A)), A_Access);
         Append_To (Profile, N);

         N := Make_Attribute_Reference
           (Make_Identifier (SN (S_Invoke)), A_Access);
         Append_To (Profile, N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Skeleton), Profile);
         Append_To (Statements, N);

         --  In case of perfect hash function optimization, we register the
         --  Invoke_XXXX procedures at package initialization.

         if Use_Minimal_Hash_Function then
            Append_To (Statements, First_Node (Register_Procedure_List));
         end if;

         N := Make_Subprogram_Body (Spec, No_List, Statements);
         return N;
      end Deferred_Initialization_Body;

      -----------------------
      -- Gen_Invoke_Helper --
      -----------------------

      function Gen_Invoke_Helper
        (Case_Statement : Node_Id) return Node_Id is

         Invoke_Helper : constant Node_Id :=
           Make_Defining_Identifier (SN (S_Helper));

         N : Node_Id;
      begin
         N := Make_Subprogram_Specification (Invoke_Helper, No_List);
         Append_To (Invoke_Methods, N);
         N := Make_Subprogram_Body
           (Specification =>
              Make_Subprogram_Specification (Invoke_Helper, No_List),
            Declarations  => No_List,
            Statements    => New_List (Case_Statement));
         Append_To (Invoke_Methods, N);

         N := Make_Subprogram_Call (Invoke_Helper, No_List);
         return N;
      end Gen_Invoke_Helper;

      -----------------------
      -- Gen_Invoke_Method --
      -----------------------

      function Gen_Invoke_Method
        (Operation_Name : Name_Id;
         Declarations   : List_Id;
         Statements     : List_Id) return Node_Id is

         Invoke_Method : constant Node_Id :=
           Make_Defining_Identifier
             (Add_Prefix_To_Name ("Invoke_", Operation_Name));

         N : Node_Id;
      begin
         N := Make_Subprogram_Specification (Invoke_Method, No_List);
         Append_To (Invoke_Methods, N);
         N := Make_Subprogram_Body
           (Specification =>
              Make_Subprogram_Specification (Invoke_Method, No_List),
            Declarations  => Declarations,
            Statements    => Statements);
         Append_To (Invoke_Methods, N);

         N := Make_Subprogram_Call (Invoke_Method, No_List);
         return N;
      end Gen_Invoke_Method;

      ---------------------
      -- Gen_Invoke_Part --
      ---------------------

      function Gen_Invoke_Part (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         C                     : Node_Id;
         N                     : Node_Id;
         M                     : Node_Id;
         Param                 : Node_Id;
         Param_Name            : Name_Id;
         Type_Node             : Node_Id;
         New_Name              : Name_Id;
         Params                : List_Id;
         Impl_Id               : Node_Id;
         Operation_Name        : Name_Id := FEN.IDL_Name (Identifier (E));
         Arg_Name              : Name_Id;
         Discrete_Choice_Value : Value_Id;
         Record_Node           : Node_Id;
         Declarative_Part      : constant List_Id := New_List;
         Statements            : constant List_Id := New_List;
         Inv_Profile           : constant List_Id := New_List;

         --  The flags below indicate whether the operation is mapped
         --  to an Ada function or an Ada procedure.

         Has_Out_Params  : constant Boolean := Contains_Out_Parameters (E);
         Non_Void        : constant Boolean :=
           FEN.Kind (Type_Spec (E)) /= K_Void;
         Is_Ada_Function : constant Boolean :=
           Non_Void and then not Has_Out_Params;

         function Exception_Handler_Alternative (E : Node_Id) return Node_Id;
         --  Generation of an alternative in the exception handler

         -----------------------------------
         -- Exception_Handler_Alternative --
         -----------------------------------

         function Exception_Handler_Alternative (E : Node_Id) return Node_Id is
            Result     : Node_Id;
            Selector   : Node_Id;
            Block      : Node_Id;
            N          : Node_Id;
            D          : constant List_Id := New_List;
            S          : constant List_Id := New_List;
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
            Append_To (D, N);

            --  Getting the node corresponding to the declaration of
            --  the Get_Members procedure. This procedure is declared
            --  2 nodes after the member type definition.

            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));
            N := Next_Node (Next_Node (N));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               New_List
               (Make_Defining_Identifier (PN (P_E)),
                Make_Defining_Identifier (PN (P_Members))));
            Append_To (S, N);

            --  Getting the node corresponding to the declaration of
            --  the To_Any procedure in the helper package.

            N := To_Any_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               New_List
               (Make_Defining_Identifier (PN (P_Members))));

            N := Make_Subprogram_Call
              (RE (RE_Set_Exception),
               New_List
               (Make_Defining_Identifier (PN (P_Request)), N));
            Append_To (S, N);

            N := Make_Return_Statement (No_Node);
            Append_To (S, N);

            Block := Make_Block_Statement
              (Declarative_Part => D,
               Statements       => S);

            Result := Make_Case_Statement_Alternative
              (New_List (Selector),
               New_List (Block));

            return Result;
         end Exception_Handler_Alternative;

         --  Start of processing for Gen_Invoke_Part

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
         Append_To (Inv_Profile, N);

         --  Generate the code relative to the operation parameters

         Param := First_Entity (Parameters (E));

         while Present (Param) loop

            --  Get the parameter name

            Param_Name := To_Ada_Name
              (IDL_Name (Identifier (Declarator (Param))));

            --  Declare a local variable having the same type as the
            --  parameter.

            --  Get the Ada type generated from the parameter type spec

            Type_Node := Get_Type_Definition_Node (Type_Spec (Param));

            --  Variable name

            Arg_Name := Map_Argument_Name (Param_Name);

            --  Declare the variable

            N :=  Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Arg_Name),
               Object_Definition   => Type_Node);
            Append_To (Declarative_Part, N);

            --  Adding the variable to the profile of the
            --  implementation call.

            Append_To (Inv_Profile, Make_Defining_Identifier (Arg_Name));

            --  The declaration below are not generated if the SII is
            --  used.

            if not Use_SII then
               --  Disable warnings on the declared variable

               N := Make_Pragma
                 (Pragma_Warnings,
                  New_List (RE (RE_Off), Make_Identifier (Arg_Name)));
               Append_To (Declarative_Part, N);

               --  Prepare the actual profile of the Add_Item call

               Params := New_List (Make_Identifier (VN (V_Argument_List)));

               --  Declare the global variable corresponding to the
               --  argument name.

               C := Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_To_CORBA_String),
                  Actual_Parameter_Part => New_List
                  (Make_Literal (New_String_Value (Param_Name, False))));
               New_Name := Map_Argument_Identifier_Name
                 (Param_Name, Operation_Name);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (New_Name),
                  Constant_Present => True,
                  Object_Definition => RE (RE_Identifier_0),
                  Expression => C);
               Append_To (BEN.Statements (Current_Package), N);

               --  Append the argument name to the actual profile of
               --  the Add_Item call.

               Append_To (Params, Make_Identifier (New_Name));

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
                  New_List (Make_Attribute_Reference
                                (N, A_Unrestricted_Access)));
               N := Make_Attribute_Reference (RE (RE_Content), A_Class);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (Map_Argument_Content_Name (Param_Name)),
                  Object_Definition   => N,
                  Expression          => C,
                  Aliased_Present     => True);
               Append_To (Declarative_Part, N);

               --  Declaration of the `Any' argument variable

               C := Make_Attribute_Reference
                      (Make_Identifier
                         (Map_Argument_Content_Name (Param_Name)),
                       A_Unchecked_Access);

               C := Make_Subprogram_Call
                 (RE (RE_Get_Wrapper_Any),
                  New_List
                  (Get_TC_Node (Type_Spec (Param)),
                   C));

               N := Make_Object_Declaration
                      (Defining_Identifier => Make_Defining_Identifier
                                                (Map_Argument_Any_Name
                                                 (Param_Name)),
                       Constant_Present    => True,
                       Object_Definition   => RE (RE_Any),
                       Expression          => C);
               Append_To (Declarative_Part, N);

               --  Append the Any variable the actual profile of the
               --  Add_Item call.

               N := Make_Identifier (Map_Argument_Any_Name (Param_Name));
               Append_To (Params, N);

               --  Append the parameter mode is added to the Add_Item
               --  profile.

               if FEN.Parameter_Mode (Param) = Mode_Out then
                  N := RE (RE_ARG_OUT_0);
               elsif FEN.Parameter_Mode (Param) = Mode_In then
                  N := RE (RE_ARG_IN_0);
               else
                  N := RE (RE_ARG_INOUT_0);
               end if;

               Append_To (Params, N);

               --  Add_Item call

               N := Make_Subprogram_Call
                 (RE (RE_Add_Item_0),
                  Params);
               Append_To (Statements, N);
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
            Append_To (Declarative_Part, N);

            --  If this is a procedure then we add the result variable
            --  to the actual profile of the implementation call.

            if not Is_Ada_Function then
               Append_To (Inv_Profile, Make_Identifier (VN (V_Result)));
            end if;

            --  The following declaration are not for static request
            --  handling.

            if not Use_SII then
               --  Disable warnings on the Result_Ü variable

               N := Make_Pragma
                    (Pragma_Warnings,
                     New_List (RE (RE_Off),
                                   Make_Identifier (VN (V_Result))));
               Append_To (Declarative_Part, N);

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
                  New_List (Make_Attribute_Reference
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
               Append_To (Declarative_Part, N);

               --  Declaration of the `Any' argument variable

               C := Make_Attribute_Reference
                 (Make_Identifier
                  (Map_Argument_Content_Name (VN (V_Result))),
                  A_Unchecked_Access);
               C := Make_Subprogram_Call
                 (RE (RE_Get_Wrapper_Any),
                  New_List
                  (Get_TC_Node (Type_Spec (E)),
                   C));
               N := Make_Object_Declaration
                      (Defining_Identifier => Make_Defining_Identifier
                                                (Map_Argument_Any_Name
                                                   (VN (V_Result))),
                       Constant_Present    => True,
                       Object_Definition   => RE (RE_Any),
                       Expression          => C);
               Append_To (Declarative_Part, N);
            end if;
         end if;

         --  In SII mode, the request payload has to be set before the
         --  request is processed. The Argument procedure is not the
         --  same as DII.

         if Use_SII then
            declare
               M : Node_Id;
            begin
               Params := New_List;

               --  GIOP_Session is used to get the representation
               --  attribute (the declarative part the block).

               M := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Component)));

               N := Make_Type_Conversion (RE (RE_GIOP_Session), M);

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (VN (V_Session)),
                  Object_Definition   => RE (RE_GIOP_Session),
                  Renamed_Object      => N);
               Append_To (Declarative_Part, N);

               N := Make_Subprogram_Call
                 (RE (RE_Get_Representation),
                  New_List
                  (Make_Attribute_Reference
                   (Make_Identifier (VN (V_Session)),
                    A_Unrestricted_Access)));

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (VN (V_Representation)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_CDR_Representation_Access),
                  Expression          => N);
               Append_To (Declarative_Part, N);

               if not Use_Compiler_Alignment and then
                 (Non_Void or else Has_Out_Params)
               then
                  N := Get_Type_Definition_Node (E);

                  N := Make_Object_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (PN (P_Arg_List_Out)),
                     Aliased_Present     => True,
                     Object_Definition   => N);
                  Append_To (Declarative_Part, N);
               end if;

               --  We need an Error Container

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                  (PN (P_Error)),
                  Object_Definition   => RE (RE_Error_Container));
               Append_To (Declarative_Part, N);

               N := Get_Type_Definition_Node (E);

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Arg_List_In)),
                  Aliased_Present     => True,
                  Object_Definition   => N);
               Append_To (Declarative_Part, N);

               Set_Str_To_Name_Buffer ("Processing request");
               Append_To (Statements, Make_Ada_Comment (Name_Find));

               --  Unmarshall arguments

               C := Get_Unmarshaller_Node (E);

               Append_To (Params, RE (RE_False));

               M := Make_Identifier (PN (P_Arg_List_In));
               M := Make_Attribute_Reference (M, A_Access);

               Append_To (Params, M);

               M := Make_Subprogram_Call
                 (RE (RE_Get_Buffer),
                  New_List
                  (Make_Attribute_Reference
                   (Make_Identifier (VN (V_Session)),
                    A_Unrestricted_Access)));

               Append_To (Params, M);

               M := Make_Explicit_Dereference
                 (Make_Identifier
                    (VN (V_Component)));

               Append_To (Params,
                 Make_Explicit_Dereference
                   (Make_Identifier (VN (V_Representation))));

               Append_To (Params, Make_Literal (New_Integer_Value (8, 1, 10)));

               Append_To (Params, Make_Defining_Identifier (PN (P_Error)));

               --  The unmarshaller method call

               N := Make_Subprogram_Call (C, Params);
               Append_To (Statements, N);

               --  Handling error

               C := Make_Subprogram_Call
                 (RE (RE_Found),
                  New_List (Make_Defining_Identifier (PN (P_Error))));
               N := Make_Subprogram_Call
                 (RE (RE_Raise_From_Error),
                  New_List (Make_Defining_Identifier (PN (P_Error))));

               N := Make_If_Statement
                 (Condition       => C,
                  Then_Statements => New_List (N));
               Append_To (Statements, N);

               --  In case oneway operation, client does not need to
               --  wait.

               if Is_Oneway (E) then
                  Set_Str_To_Name_Buffer ("Oneway operation,"
                                          & " release the client");
                  Append_To (Statements, Make_Ada_Comment (Name_Find));

                  N := Make_Assignment_Statement
                    (Make_Selected_Component
                     (VN (V_Request),
                      CN (C_Deferred_Arguments_Session)),
                     Make_Null_Statement);
                  Append_To (Statements, N);

                  N := Make_Qualified_Expression
                    (RE (RE_Flush),
                     Make_Record_Aggregate (No_List, RE (RE_Message)));
                  N := Make_Subprogram_Call
                    (RE (RE_Emit_No_Reply),
                     New_List (Make_Identifier (VN (V_Component)), N));
                  Append_To (Statements, N);
               end if;
            end;
         else
            Set_Str_To_Name_Buffer ("Processing request");
            Append_To (Statements, Make_Ada_Comment (Name_Find));

            N := Make_Subprogram_Call
              (RE (RE_Arguments_1),
               New_List
               (Make_Defining_Identifier (PN (P_Request)),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_To (Statements, N);
         end if;

         --  The block above implements the generation of:

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
            --  setting Inner_Statements to the corresponding value.

            if not FEU.Is_Empty (Exceptions (E)) then
               Inner_Statements  := New_List;
               Exception_Handler := New_List;
               Inner             := True;

               --  Creating the exception handler statements

               Excp_Node := First_Entity (Exceptions (E));

               while Present (Excp_Node) loop
                  N := Exception_Handler_Alternative (Excp_Node);
                  Append_To (Exception_Handler, N);
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
                     Append_To (Inner_Statements, N);
                  end if;

                  Param := Next_Entity (Param);

               end loop;
            end if;

            --  Call Implementation

            Set_Str_To_Name_Buffer ("Call Implementation");
            Append_To (Inner_Statements, Make_Ada_Comment (Name_Find));

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

            Append_To (Inner_Statements, C);

            if Inner then
               Append_To (Statements,
                 Make_Block_Statement
                   (Declarative_Part  => No_List,
                    Statements        => Inner_Statements,
                    Exception_Handler => Exception_Handler));
            end if;
         end;

         --  Set Result

         if Non_Void then
            Set_Str_To_Name_Buffer ("Setting the result");
            Append_To (Statements, Make_Ada_Comment (Name_Find));

            if Use_SII then
               C := Make_Selected_Component
                 (PN (P_Arg_List_Out), PN (P_Returns));

               N := Make_Assignment_Statement
                 (C, Make_Identifier (VN (V_Result)));
               Append_To (Statements, N);
            else
               N := Make_Subprogram_Call
                 (RE (RE_Set_Result),
                  New_List
                  (Make_Identifier (PN (P_Request)),
                   Make_Identifier (Map_Argument_Any_Name (VN (V_Result)))));
               Append_To (Statements, N);
            end if;
         end if;

         --  Setting out arguments

         if Use_SII then
            if Has_Out_Params then
               --  Out parameters of the Operation

               Param := First_Entity (Parameters (E));
               while Present (Param) loop
                  if  FEN.Parameter_Mode (Param) = Mode_Out
                    or else FEN.Parameter_Mode (Param) = Mode_Inout then
                     Set_Str_To_Name_Buffer ("Setting out argument");
                     Append_To (Statements, Make_Ada_Comment (Name_Find));

                     Param_Name := To_Ada_Name
                       (IDL_Name (Identifier (Declarator (Param))));
                     Arg_Name := Map_Argument_Name (Param_Name);

                     C := Make_Defining_Identifier (Param_Name);

                     C := Make_Selected_Component
                       (Make_Identifier (PN (P_Arg_List_Out)), C);
                     N := Make_Assignment_Statement
                       (C, Make_Identifier (Arg_Name));
                     Append_To (Statements, N);

                  end if;

                  Param := Next_Entity (Param);
               end loop;
            end if;
         else
            --  Simply call `Clone_Out_Args' in all cases

            N := Make_Subprogram_Call
              (RE (RE_Clone_Out_Args),
               New_List (Make_Identifier (VN (V_Argument_List))));
            Append_To (Statements, N);
         end if;

         if Use_SII and then (Has_Out_Params or else Non_Void) then
            --  Tell Invoke_Declaration to declare a buffer local
            --  variable.

            Buffer_Necessary := True;

            if Use_Compiler_Alignment then
               declare
                  Disc     : constant List_Id := New_List;
                  Access_T : Node_Id;
                  Blk_Stat : constant List_Id := New_List;
                  Dec_Stat : constant List_Id := New_List;
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
                  Append_To (Dec_Stat, N);

                  C := Make_Attribute_Reference
                    (Make_Explicit_Dereference
                       (Make_Identifier
                          (VN (V_Args_Out))),
                     A_Address);

                  J := Unsigned_Long_Long (Length (Disc));
                  N := Make_Subprogram_Call
                    (RE (RE_Insert_Raw_Data),
                     New_List
                     (Make_Identifier (PN (P_Request)),
                      C,
                      Make_Attribute_Reference
                      (Make_Explicit_Dereference
                       (Make_Identifier
                        (VN (V_Args_Out))),
                       A_Size),
                      Make_Literal (New_Integer_Value (J, 1, 10)),
                      Make_Identifier (VN (V_Buffer))));
                  Append_To (Blk_Stat, N);

                  N := Make_Block_Statement
                    (Declarative_Part => Dec_Stat,
                     Statements       => Blk_Stat);
                  Append_To (Statements, N);
               end;
            else
               --  The marshaller method

               C := Get_Marshaller_Node (E);

               Params := New_List;
               Append_To (Params, RE (RE_False));

               M := Make_Identifier (PN (P_Arg_List_Out));
               M := Make_Attribute_Reference (M, A_Access);
               Append_To (Params, M);

               Append_To (Params, Make_Defining_Identifier (VN (V_Buffer)));

               N := Make_Explicit_Dereference
                 (Make_Identifier (VN (V_Representation)));
               Append_To (Params, N);

               Append_To (Params, Make_Literal (Int1_Val));

               Append_To (Params, Make_Defining_Identifier (PN (P_Error)));

               --  The marshaller method call

               N := Make_Subprogram_Call (C, Params);
               Append_To (Statements, N);

               --  If any error we raise a program_error

               N := Make_Subprogram_Call
                 (RE (RE_Found),
                  New_List (Make_Identifier (PN (P_Error))));

               N := Make_If_Statement
                 (Condition       => N,
                  Then_Statements => New_List
                    (Make_Raise_Statement
                     (Make_Identifier
                      (EN (E_Program_Error)))));
               Append_To (Statements, N);

               --  Add the buffer as a QoS parameter for the request

               Set_Str_To_Name_Buffer
                 ("Add the buffer to the request QoS parameters");
               Append_To (Statements, Make_Ada_Comment (Name_Find));

               N := Make_Record_Aggregate
                 (New_List
                  (RE (RE_GIOP_Static_Buffer),
                   Make_Defining_Identifier (VN (V_Buffer))));

               N := Make_Object_Instantiation
                 (Make_Qualified_Expression
                  (RE (RE_QoS_GIOP_Static_Buffer_Parameter),
                   N));

               N := Make_Subprogram_Call
                 (RE (RE_Add_Request_QoS),
                  New_List
                  (Make_Defining_Identifier (VN (V_Request)),
                   RE (RE_GIOP_Static_Buffer),
                   N));

               Append_To (Statements, N);
            end if;
         end if;

         --  Map the operation name

         Operation_Name := Map_Operation_Name_Literal (E);

         --  If no optimization is requested by the user, we generate an elsif
         --  statement. Otherwise, we generate a case statement alternative.

         N := Gen_Invoke_Method
           (Operation_Name, Declarative_Part, Statements);

         if not Use_Minimal_Hash_Function then
            C := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Operation_Name, False)));

            N := Make_Elsif_Statement
              (C, New_List (N));
         else
            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Insert_And_Register_Statements (Operation_Name);

            --  Prepare the case alternative
            --  * Discrete Choice : value of N_Subprogram minus 1

            Discrete_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Case_Statement_Alternative
              (New_List (Make_Literal (Discrete_Choice_Value)),
               New_List (N));
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
         D                 : constant List_Id := New_List;
         C_1               : Node_Id;
         Else_Statements   : constant List_Id := New_List;
         Invoke_Statements : constant List_Id := New_List;
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
               New_List
               (Make_Literal (Int0_Val),
                Make_Defining_Identifier (VN (V_Argument_List))));
            Append_To (Invoke_Statements, N);
         end if;

         if not Use_Minimal_Hash_Function then
            Append_To (Invoke_Then_Statements, Is_A_Invk_Part);

            Set_Str_To_Name_Buffer ("_is_a");
            Is_A_Lowered_Name := Name_Find;

            C_1 := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Is_A_Lowered_Name, False)));
         else
            Append_To (Invoke_Subp_Bodies, Is_A_Invk_Part);

            N := Make_Selected_Component
              (Make_Defining_Identifier (Hash_Package_Name (E)),
               Make_Defining_Identifier (SN (S_Hash)));

            --  Calculate the hash code of the operation

            N := Make_Subprogram_Call
              (N,
               New_List
               (Make_Defining_Identifier
                (VN (V_Operation))));
            N := Make_Assignment_Statement
              (Make_Defining_Identifier (VN (V_Index)),
               N);
            Append_To (Invoke_Statements, N);

            --  Get the operation name corresponding to the hash code

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Invoke_Db)),
               New_List (Make_Defining_Identifier (VN (V_Index))));

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Invoke_Name_Access)),
               N);
            Append_To (Invoke_Statements, N);

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
               New_List (N));
            Append_To (Invoke_Subp_Bodies, N);

            N := Make_Case_Statement
              (Make_Identifier (VN (V_Index)),
               Invoke_Subp_Bodies);
            N := Gen_Invoke_Helper (N);
            Append_To (Invoke_Then_Statements, N);
         end if;

         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Operation),
            New_List (RE (RE_Default_Sys_Member)));
         Append_To (Else_Statements, N);

         N := Make_If_Statement
           (C_1,
            Invoke_Then_Statements,
            Invoke_Elsif_Statements,
            Else_Statements);

         Exception_Handler := Non_User_Exception_Handler;

         N := Make_Block_Statement
           (Declarative_Part  => No_List,
            Statements        =>
              New_List (N),
            Exception_Handler =>
              New_List (Exception_Handler));
         Append_To (Invoke_Statements, N);

         --  Generation of the Invoke Procedure. Note that Append_To is
         --  appending the entire Invoke_Methods list onto D.

         Append_To (D, First_Node (Invoke_Methods));
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
            New_List (N));
         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            New_List (N));
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Operation)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_String_2),
            Expression          => N);
         Append_To (L, N);

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Argument_List)),
            Object_Definition   => RE (RE_Ref_4));
         Append_To (L, N);

         --  Do not declare SII related local variable if the
         --  interface does not containe (or inherit) any operation.

         if Use_SII and then Has_Operations then
            declare
               C : Node_Id;
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Request_Access),
                  New_List
                  (Make_Defining_Identifier (PN (P_Request))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Request)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Request_Access),
                  Expression          => N);
               Append_To (L, N);

               --  Request binding object

               C := Make_Selected_Component
                 (VN (V_Request), PN (P_Dependent_Binding_Object));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Binding_Object)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Ref_10),
                  Expression          => C);
               Append_To (L, N);

               --  The GIOP Session is the Component attribute
               --  Dependent_Binding_Object.

               C := Make_Subprogram_Call
                 (RE (RE_Get_Component),
                  New_List (Make_Identifier (VN (V_Binding_Object))));

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Component)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Component_Access),
                  Expression          => C);
               Append_To (L, N);

               --  Buffer for marshalling the arguments

               if Buffer_Necessary then
                  C := Make_Object_Instantiation (RE (RE_Buffer_Type));

                  N := Make_Object_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (VN (V_Buffer)),
                     Constant_Present => True,
                     Object_Definition => RE (RE_Buffer_Access),
                     Expression => C);
                  Append_To (L, N);
               end if;
            end;
         end if;

         if Use_Minimal_Hash_Function then
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Index)),
               Object_Definition   => RE (RE_Natural));
            Append_To (L, N);

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Invoke_Name_Access)),
               Object_Definition   => Make_Defining_Identifier
               (TN (T_String_Ptr)));
            Append_To (L, N);

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
         Profile := New_List;
         Param   := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            RE (RE_Servant));
         Append_To (Profile, Param);
         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Request)),
            RE (RE_Object_Ptr));
         Append_To (Profile, Param);
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
         N                     : Node_Id;
         Declarative_Part      : constant List_Id
           := New_List;
         Statements            : constant List_Id
           := New_List;
         Discrete_Choice_Value : Value_Id;

         Profile : List_Id;

         Operation_Name : Name_Id;
      begin
         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Type_Id)),
            Object_Definition   => RE (RE_String_0));
         Append_To (Declarative_Part, N);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (VN (V_Arg_Name_Type_Id)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Identifier_0),
            Expression          => Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List (Make_Literal
                             (New_String_Value (VN (V_Type_Id), False)))));
         Append_To (Declarative_Part, N);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (VN (V_Argument_Type_Id)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_Any),
            Expression          => Make_Subprogram_Call
              (RE (RE_To_Any_0),
               New_List (Make_Defining_Identifier (VN (V_Type_Id)))));
         Append_To (Declarative_Part, N);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Result)),
            Object_Definition   => RE (RE_Boolean));
         Append_To (Declarative_Part, N);

         --  Statements

         --  Call to CORBA.NVList.Add_Item

         Profile := New_List;

         Append_To (Profile, Make_Identifier (VN (V_Argument_List)));
         Append_To (Profile, Make_Identifier (VN (V_Arg_Name_Type_Id)));
         Append_To (Profile, Make_Identifier (VN (V_Argument_Type_Id)));
         Append_To (Profile, RE (RE_ARG_IN_0));

         N := Make_Subprogram_Call (RE (RE_Add_Item_0), Profile);
         Append_To (Statements, N);

         --  Call to CORBA.ServerRequest.Arguments

         Profile := New_List;

         Append_To (Profile, Make_Identifier (PN (P_Request)));
         Append_To (Profile, Make_Identifier (VN (V_Argument_List)));

         N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
         Append_To (Statements, N);

         --  Assign the Type_Id

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Type_Id)),
            Make_Subprogram_Call (RE (RE_From_Any_0),
                                  New_List (Make_Identifier
                                                (VN (V_Argument_Type_Id)))));
         Append_To (Statements, N);

         --  Call the implementation

         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            New_List (Make_Identifier (VN (V_Type_Id))));
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Is_A)),
            New_List (N));
         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Result)), N);
         Append_To (Statements, N);

         --  Set the result

         Profile := New_List;

         Append_To (Profile, Make_Identifier (PN (P_Request)));
         N := Make_Subprogram_Call (RE (RE_To_Any_0),
                                    New_List (Make_Identifier
                                                  (VN (V_Result))));
         Append_To (Profile, N);

         N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
         Append_To (Statements, N);

         --  If no optimization is requested by the user, we generate
         --  an elsif??? statement. Else, we generate a case alternative
         --  statement.

         Set_Str_To_Name_Buffer ("_is_a");
         Operation_Name := Name_Find;

         N := Gen_Invoke_Method
           (Operation_Name, Declarative_Part, Statements);

         if not Use_Minimal_Hash_Function then
            null;  --  ???No elsif here.
         else
            --  Insert the subprogram name into the hash function
            --  generator and add a call to Register_Procedure

            Insert_And_Register_Statements (Operation_Name);

            --  Prepare the case alternative * Discrete Choice : value
            --  of N_Subprogram minus 1

            Discrete_Choice_Value := New_Integer_Value
              (N_Subprograms - 1, 1, 10);

            N := Make_Case_Statement_Alternative
              (New_List (Make_Literal (Discrete_Choice_Value)),
               New_List (N));
         end if;

         return N;
      end Is_A_Invoke_Part;

      ----------------------------
      -- Implicit_CORBA_Methods --
      ----------------------------

      function Implicit_CORBA_Methods return List_Id is

         Result_List : constant List_Id := New_List;

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
            N                    : Node_Id;
            Discrete_Choice      : Node_Id;
            Op_Name_1, Op_Name_2 : Name_Id;
            C                    : Node_Id;
         begin
            Set_Str_To_Name_Buffer (Method_Name_1);
            Op_Name_1 := Name_Find;
            if Method_Name_2 /= "" then
               Set_Str_To_Name_Buffer (Method_Name_2);
               Op_Name_2 := Name_Find;
            end if;

            N := Gen_Invoke_Method
              (Op_Name_1, Declarations, Statements);

            --  If no optimization is requested by the user, we
            --  generate an elsif statement. Else, we generate a case
            --  alternative statement

            if not Use_Minimal_Hash_Function then

               C := Make_Expression
                 (Make_Defining_Identifier (VN (V_Operation)),
                  Op_Equal,
                  Make_Literal (New_String_Value (Op_Name_1, False)));

               if Method_Name_2 /= "" then
                  declare
                     C_2 : Node_Id;
                  begin
                     C_2 := Make_Expression
                       (Make_Defining_Identifier (VN (V_Operation)),
                        Op_Equal,
                        Make_Literal (New_String_Value (Op_Name_2, False)));
                     C := Make_Expression (C, Op_Or_Else, C_2);
                  end;
               end if;

               N := Make_Elsif_Statement
                 (C, New_List (N));
            else
               --  Insert the subprogram name into the hash function
               --  generator and add a call to Register_Procedure

               Insert_And_Register_Statements (Op_Name_1);

               --  Prepare the case alternative

               --  * Discrete Choice : value of N_Subprogram minus 1

               Discrete_Choice := Make_Literal
                 (New_Integer_Value (N_Subprograms - 1, 1, 10));

               if Method_Name_2 /= "" then
                  declare
                     DC_2 : Node_Id;
                  begin
                     Insert_And_Register_Statements (Op_Name_2);

                     DC_2 := Make_Literal
                       (New_Integer_Value (N_Subprograms - 1, 1, 10));

                     Discrete_Choice := Make_Expression
                       (Discrete_Choice, Op_Vertical_Bar, DC_2);
                  end;
               end if;

               N := Make_Case_Statement_Alternative
                 (New_List (Discrete_Choice),
                  New_List (N));
            end if;

            Append_To (Result_List, N);

         end Add_Implicit_CORBA_Method;

         N       : Node_Id;

         --  Start of processing for Implicit_CORBA_Methods

      begin
         --  For each implicit CORBA Method, add a similar block
         --  statement

         --  The "Interface" implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List;
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List;

            Append_To (Profile, Make_Identifier (PN (P_Request)));
            Append_To (Profile, Make_Identifier (VN (V_Argument_List)));
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_To (Statements, N);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List;
            Append_To (Profile, Make_Identifier (PN (P_Request)));

            N := Make_Subprogram_Call
              (RE (RE_To_CORBA_String),
               New_List (Make_Identifier (PN (P_Repository_Id))));
            N := Make_Subprogram_Call (RE (RE_Get_Interface_Definition),
                                       New_List (N));
            N := Make_Subprogram_Call (RE (RE_Ref_2),
                                       New_List (N));
            N := Make_Subprogram_Call (RE (RE_To_Any_3),
                                       New_List (N));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_To (Statements, N);

            --  Add the handler

            Add_Implicit_CORBA_Method (No_List, Statements, "_interface");

         end;

         --  The Domain_Managers implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List;
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List;

            Append_To (Profile, Make_Identifier (PN (P_Request)));
            Append_To (Profile, Make_Identifier (VN (V_Argument_List)));
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_To (Statements, N);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List;
            Append_To (Profile, Make_Identifier (PN (P_Request)));

            N := Make_Subprogram_Call
              (RE (RE_Get_Domain_Managers),
               New_List (Make_Identifier (PN (P_Self))));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_To (Statements, N);

            --  Add the handler

            Add_Implicit_CORBA_Method
              (No_List, Statements, "_domain_managers");
         end;

         --  The Non_Existent implicit method

         declare
            Profile    : List_Id;
            Statements : constant List_Id := New_List;
         begin
            --  Call CORBA.ServerRequest.Arguments

            Profile := New_List;

            Append_To (Profile, Make_Identifier (PN (P_Request)));
            Append_To (Profile, Make_Identifier (VN (V_Argument_List)));
            N := Make_Subprogram_Call (RE (RE_Arguments_1), Profile);
            Append_To (Statements, N);

            --  Call CORBA.ServerRequest.Set_Result

            Profile := New_List;
            Append_To (Profile, Make_Identifier (PN (P_Request)));

            N := Make_Literal (New_Boolean_Value (False));
            N := Make_Qualified_Expression
              (Subtype_Mark => RE (RE_Boolean),
               Operand      => N);
            N := Make_Subprogram_Call
              (RE (RE_To_Any_0),
               New_List (N));
            Append_To (Profile, N);

            N := Make_Subprogram_Call (RE (RE_Set_Result), Profile);
            Append_To (Statements, N);

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
         Statements : constant List_Id := New_List;
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
         Append_To (Statements, N);

         return  Make_Subprogram_Body (Spec, No_List, Statements);
      end Servant_Is_A_Body;

      -----------------------------
      -- Skeleton_Initialization --
      -----------------------------

      procedure Skeleton_Initialization (L : List_Id) is
         N                : Node_Id;
         V                : Value_Id;
         Dep              : Node_Id;
         Aggregates       : constant List_Id := New_List;
         Declarative_Part : constant List_Id := New_List;
         Statements       : constant List_Id := New_List;
      begin
         --  Declarative part
         --  Adding 'use' clauses to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_To (Declarative_Part, N);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_To (Declarative_Part, N);

         --  Statements

         --  The package name

         N := Defining_Identifier (Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N), False);
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Name)),
            Expression    => N);
         Append_To (Aggregates, N);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression    => RE (RE_Empty));
         Append_To (Aggregates, N);

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
         Append_To (Aggregates, N);

         --  Provides

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Provides)),
            Expression    => RE (RE_Empty));
         Append_To (Aggregates, N);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Implicit)),
            Expression    => RE (RE_False));
         Append_To (Aggregates, N);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Init)),
            Expression    => Make_Attribute_Reference
              (Make_Identifier (SN (S_Deferred_Initialization)),
               A_Access));
         Append_To (Aggregates, N);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_To (Aggregates, N);

         --  Registering the module

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Operand      => Make_Record_Aggregate (Aggregates));

         N := Make_Subprogram_Call (RE (RE_Register_Module), New_List (N));
         Append_To (Statements, N);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         Append_To (L, N);
      end Skeleton_Initialization;

      --------------------------------
      -- Non_User_Exception_Handler --
      --------------------------------

      function Non_User_Exception_Handler return Node_Id is
         Result     : Node_Id;
         Selector   : Node_Id;
         N          : Node_Id;
         S          : constant List_Id := New_List;
      begin
         --  Generation of the "E : others" statement

         Selector := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_E)),
            Object_Definition => No_Node);

         --  Body of the exception handler

         N := Make_Subprogram_Call
           (RE (RE_System_Exception_To_Any),
            New_List
            (Make_Defining_Identifier (PN (P_E))));

         --  Set the exception

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception),
            New_List
            (Make_Defining_Identifier (PN (P_Request)), N));
         Append_To (S, N);

         --  Set the exception informations

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception_Information),
            New_List
            (Make_Identifier (PN (P_Request)),
             Make_Identifier (PN (P_E))));
         Append_To (S, N);

         Result := Make_Case_Statement_Alternative
           (New_List (Selector), S);

         return Result;
      end Non_User_Exception_Handler;

      -----------------------
      -- Hash_Package_Name --
      -----------------------

      function Hash_Package_Name (E : Node_Id) return Name_Id is
         pragma Assert (FEN.Kind (E) = K_Interface_Declaration);
      begin
         Get_Name_String
           (Fully_Qualified_Name (Map_Fully_Qualified_Identifier (E)));

         --  Note: the generated code assumes no user entities hide any
         --  standard entities, so we can't generate Hash as a child unit
         --  of the mapped stubs package.

         for J in 1 .. Name_Len loop
            if Name_Buffer (J) = '.' then
               Name_Buffer (J) := '_';
            end if;
         end loop;

         Add_Str_To_Name_Buffer ("_Hash");

         return Name_Find;
      end Hash_Package_Name;

      -------------------------------------------
      -- Initialize_Hash_Function_Optimization --
      -------------------------------------------

      procedure Initialize_Hash_Function_Optimization is
      begin
         --  Initialize the lists and the number of subprograms

         N_Subprograms           := 0;
         Register_Procedure_List := New_List;
         Invoke_Subp_Bodies      := New_List;
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
         Append_To (Statements (Current_Package), N);

         --  Definition of a string access type

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (TN (T_String_Ptr)),
            Type_Definition     => Make_Access_Type_Definition
            (RE (RE_String_2)));
         Append_To (Statements (Current_Package), N);

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
            (New_List (N),
             Make_Defining_Identifier (TN (T_String_Ptr))),
            Expression          => Make_Record_Aggregate
            (New_List
             (Make_Component_Association
              (Selector_Name => No_Node, --  'others'
               Expression    => Make_Null_Statement))));
         Append_To (Statements (Current_Package), N);

         --  Insert the spec and the body of the Register_Procedure
         --  procedure

         N := Register_Procedure_Spec;
         Append_To (Statements (Current_Package), N);
         N := Register_Procedure_Body (E);
         Append_To (Statements (Current_Package), N);

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
            PHG.Initialize (Seed, K_2_V, Optimization_Mode);

            begin
               PHG.Compute;
               exit;
            exception
               when PHG.Too_Many_Tries =>
                  if Optimization_Mode = PHG.CPU_Time then
                     raise;
                  end if;

                  V := V + 1;
            end;
         end loop;

         Get_Name_String (Hash_Package_Name (E));

         --  Produce the package containing the Hash function; if the user
         --  specified an output directory, ensure the package is output there.

         if Output_Directory = null then
            PHG.Produce (Pkg_Name => Name_Buffer (1 .. Name_Len));
         else
            --  Change directory before calling Produce (which always generates
            --  sources in the current directory).

            declare
               use Ada.Directories;
               Save_Current_Directory : constant String := Current_Directory;
            begin
               Set_Directory (Output_Directory.all);
               PHG.Produce (Pkg_Name => Name_Buffer (1 .. Name_Len));
               Set_Directory (Save_Current_Directory);
            exception
               when others =>
                  Set_Directory (Save_Current_Directory);
                  raise;
            end;
         end if;
      end Achieve_Hash_Function_Optimization;

      ------------------------------------
      -- Insert_And_Register_Statements --
      ------------------------------------

      procedure Insert_And_Register_Statements
        (Subp_Name   : Name_Id)
      is
         Profile : constant List_Id := New_List;
         N       : Node_Id;
      begin
         --  First of all, we increment the number of subprograms

         N_Subprograms := N_Subprograms + 1;

         --  Insert the subprogram name into the perfect hash table
         --  generator.

         Get_Name_String (Subp_Name);
         PHG.Insert (Name_Buffer (1 .. Name_Len));

         --  Generate the call to Register_Procedure, which put an
         --  access to the Invoke_XXXX in the right place into the
         --  hash table.

         N := Make_Literal
           (New_String_Value
            (Subp_Name, False));
         Append_To (Profile, N);

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Register_Procedure)),
            Profile);
         Append_To (Register_Procedure_List, N);

      end Insert_And_Register_Statements;

      -----------------------------
      -- Register_Procedure_Spec --
      -----------------------------

      function Register_Procedure_Spec return Node_Id is
         N       : Node_Id;
         Profile : constant List_Id := New_List;
      begin
         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Operation_Name)),
            Subtype_Mark        => RE (RE_String_2));
         Append_To (Profile, N);

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
         Declarative_Part : constant List_Id := New_List;
         Statements       : constant List_Id := New_List;
         N                : Node_Id;
      begin
         Spec := Register_Procedure_Spec;

         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (VN (V_Index)),
            Object_Definition   => RE (RE_Natural));
         Append_To (Declarative_Part, N);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Invoke_Name_Access)),
            Object_Definition   => Make_Defining_Identifier
            (TN (T_String_Ptr)));
         Append_To (Declarative_Part, N);

         --  Statements part

         N := Make_Selected_Component
           (Make_Defining_Identifier (Hash_Package_Name (E)),
            Make_Defining_Identifier (SN (S_Hash)));

         N := Make_Subprogram_Call
           (N,
            New_List
            (Make_Defining_Identifier
             (PN (P_Operation_Name))));

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Index)),
            N);
         Append_To (Statements, N);

         --  Test if the hash code was already found in which case
         --  raise a program error.

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            New_List (Make_Defining_Identifier (VN (V_Index))));

         N := Make_Expression
           (N,
            Op_Not_Equal,
            Make_Null_Statement);

         N := Make_If_Statement
           (Condition       => N,
            Then_Statements => New_List
            (Make_Raise_Statement
             (Make_Defining_Identifier
              (EN (E_Program_Error)))));
         Append_To (Statements, N);

         --  Assigning the procedure actual name

         N :=  Make_Defining_Identifier (PN (P_Invoke_Name_Access));
         N := Make_Assignment_Statement
           (N,
            Make_Object_Instantiation
            (Make_Qualified_Expression
             (Subtype_Mark => RE (RE_String_2),
              Operand      => Make_Defining_Identifier
                (PN (P_Operation_Name)))));
         Append_To (Statements, N);

         --  Update the hash table

         N := Make_Subprogram_Call
           (Make_Defining_Identifier (PN (P_Invoke_Db)),
            New_List (Make_Defining_Identifier (VN (V_Index))));
         N := Make_Assignment_Statement
           (N,
            Make_Defining_Identifier (PN (P_Invoke_Name_Access)));
         Append_To (Statements, N);

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
         Profile        : constant List_Id := New_List;
         Invk_Spec      : Node_Id;
         Invk_Body      : Node_Id;
         Is_A_Invk_Part : Node_Id;
         Implicit_CORBA : List_Id;
         Parent_Int     : Node_Id;

         function In_Imported (Ent : Node_Id) return Boolean;
         --  True if Ent, or any of its parent scopes, is imported

         -----------------
         -- In_Imported --
         -----------------

         function In_Imported (Ent : Node_Id) return Boolean is
         begin
            if No (Ent) then
               return False;

            elsif Imported (Ent) then
               return True;

            else
               return Imported (Scope_Entity (Identifier (Ent)));
            end if;
         end In_Imported;

      --  Start of processing for Visit_Interface_Declaration

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

         Invoke_Then_Statements := New_List;
         Invoke_Methods         := New_List;
         Package_Initialization := New_List;
         Dependency_List        := New_List;
         Has_Operations         := False;
         Buffer_Necessary       := False;

         --  If the user chose to generate optimised skeletons, we
         --  initialise the optimization related lists.

         if Use_Minimal_Hash_Function then
            Initialize_Hash_Function_Optimization;
            Choice_List := Invoke_Subp_Bodies;
         else
            Invoke_Elsif_Statements := New_List;
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

         --  Here, we assign the list of the implicit CORBA methods. It's
         --  important to do this before the finalization of the hash function
         --  generator (in case of optimisation) so that all the hash keys
         --  can be inserted before the computation phase of the algorithm.

         Implicit_CORBA := Implicit_CORBA_Methods;

         --  At this point, all operations and attributes are visited. We
         --  achieve the perfect hash function generation and the building of
         --  the conditional structure which handles the request.

         if Use_Minimal_Hash_Function then
            if not In_Imported (E) then
               Achieve_Hash_Function_Optimization (E);
            end if;

            PHG.Finalize;
         end if;

         --  Here, we append the implicit CORBA methods either to the
         --  elsif statements or to the case statement depending on
         --  the optimization mode chosen by the developer.

         Append_To (Choice_List, First_Node (Implicit_CORBA));

         --  Build the Invoke procedure

         Invk_Spec := Invoke_Spec;
         Invk_Body := Invoke_Body (E, Is_A_Invk_Part);

         --  Add the Invoke procedure Spec

         Append_To (Statements (Current_Package), Invk_Spec);

         --  Add the Invoke procedure Body

         Append_To (Statements (Current_Package), Invk_Body);

         --  Generation of the Servant_Is_A function

         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Obj)),
            RE (RE_Servant));
         Append_To (Profile, Param);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Servant_Is_A)),
            Profile,
            RE (RE_Boolean_0));
         Append_To (Statements (Current_Package), N);

         N := Servant_Is_A_Body (N);
         Append_To (Statements (Current_Package), N);

         --  Generation of the Deferred_Initialization procedure

         N := Deferred_Initialization_Body (E);
         Append_To (Statements (Current_Package), N);

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
         Has_Operations := True;
         N := Gen_Invoke_Part (E);
         Append_To (Choice_List, N);
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
