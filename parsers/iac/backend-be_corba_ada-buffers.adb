------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--            B A C K E N D . B E _ C O R B A _ A D A . BUFFERS             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;


package body Backend.BE_CORBA_Ada.Buffers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;

   package body Package_Spec is

      --  Builds a record type declaration. The members of the record type
      --  are the operation arguments and result.
      --  function Args_Type_Record (E : Node_Id) return Node_Id;

      --  Builds the spec of the static buffer size subprogram
      function Buffer_Size_Spec (E : Node_Id) return Node_Id;

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      ----------------------
      -- Buffer_Size_Spec --
      ----------------------

      function Buffer_Size_Spec (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         Profile   : List_Id;
         Parameter : Node_Id;
         S         : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);

         --  'Role' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Role)),
            Subtype_Mark        => RE (RE_Entity_Role),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  'Args' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Args)),
            Subtype_Mark        => RE (RE_Request_Args_Access),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  'Buffer' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Buffer)),
            Subtype_Mark        => RE (RE_Buffer_Access),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);


         --  'First_Arg_Alignment' parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Subprogram Specification

         S := Make_Subprogram_Specification
           (Map_Buffer_Size_Identifier (Defining_Identifier (Spec)),
            Profile,
            No_Node);
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (S),
            Defining_Identifier (Buffers_Package (Current_Entity)));

         return S;
      end Buffer_Size_Spec;


      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

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
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N    : Node_Id;
         D    : Node_Id;
      begin
         Set_Buffers_Spec;

         D := First_Entity (Declarators (E));
         while Present (D) loop

            --  Explaining comment

            Set_Str_To_Name_Buffer
              ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Visible_Part (Current_Package));

            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No buffers package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Buffers_Spec;

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
         if not Map_Particular_CORBA_Parts (E, PK_Buffers_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N    : Node_Id;
      begin
         Set_Buffers_Spec;

         --  Explaining comment

         Set_Str_To_Name_Buffer
           ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  Generating the 'Operation_Name'_Buffer_Size spec

         N := Buffer_Size_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_Buffer_Size (Identifier (E), N);

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
   end Package_Spec;

   package body Package_Body is
      use Values;

      function Buffer_Size_Body (E : Node_Id) return Node_Id;

      --  These functions returns new variable names. They are used to avoid
      --  conflicts
      --  function Get_Element_Name return Name_Id;

      function Get_Index_Name return Name_Id;

      Index_Number   : Nat := 0;

      --  This function builds a type conversion of a variable to a PolyORB
      --  type

      function Cast_Variable_To_PolyORB_Type
        (Var_Node : Node_Id; Var_Type : Node_Id)
        return Node_Id;

      --  This function tests wether the mode is IN or INOUT

      function Is_In (Par_Mode : Mode_Id) return Boolean;
      pragma Inline (Is_In);

      --  This function tests wether the mode is OUT or INOUT

      function Is_Out (Par_Mode : Mode_Id) return Boolean;
      pragma Inline (Is_Out);

      --  The two subprograms below use the two subprograms above to
      --  chack the parameter mode of an IDL operation

      function Contains_In_Parameters (E : Node_Id) return Boolean;
      function Contains_Out_Parameters (E : Node_Id) return Boolean;

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------------------
      --  Compute_Padding  --
      -----------------------

      function Compute_Padding
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id;

      ----------------------
      --  Parameter_Size  --
      ----------------------

      function Parameter_Size
        (N : Node_Id)
        return Value_Id;

      ----------------------
      -- Buffer_Size_Body --
      ----------------------

      function Buffer_Size_Body (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Subp_Spec         : Node_Id;
         Subp_Statements   : constant List_Id := New_List (K_List_Id);
         Subp_Declarations : constant List_Id := New_List (K_List_Id);

         P                 : constant List_Id := Parameters (E);
         T                 : constant Node_Id := Type_Spec (E);

         Client_Case       : constant List_Id := Make_List_Id
           (RE (RE_Client_Entity));
         Client_Statements : constant List_Id := New_List (K_List_Id);

         Server_Case       : constant List_Id := Make_List_Id
           (RE (RE_Server_Entity));
         Server_Statements : constant List_Id := New_List (K_List_Id);

         Case_Alternatives : constant List_Id := New_List (K_List_Id);

         Alignment_Const   : Boolean := True;

         Args_Id           : Node_Id;
         Parameter         : Node_Id;
         Parameter_Name    : Name_Id;
         Parameter_Mode    : Mode_Id;
         Rewinded_Type     : Node_Id;
         N                 : Node_Id;
         M                 : Node_Id;

      begin
         --  generate instructions to allocate the buffer needed to
         --  marshall the body message

         --  The declarative part generation of the subprogram is postponed
         --  after the handling of the arguments and the result because it
         --  depends on the result of this handling


         --  Subprogram specification

         Subp_Spec := Buffer_Size_Node (BE_Node (Identifier (E)));
         Args_Id   := Map_Args_Identifier
           (Defining_Identifier
            (Stub_Node
             (BE_Node
              (Identifier
               (E)))));


         --  If the subprogram is a function, we handle the result

         if Present (T) and then FEN.Kind (T) /= K_Void then

            Rewinded_Type := FEU.Get_Original_Type (T);

            --  Explaining comment

            Set_Str_To_Name_Buffer
              ("padding for Result    : ");
            Get_Name_String_And_Append  (PN (P_Returns));
            Add_Str_To_Name_Buffer (" => ");
            Add_Str_To_Name_Buffer
              (FEN.Node_Kind'Image
               (FEN.Kind
                (Rewinded_Type)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Server_Statements);


            --  Body alignment

            N := Make_Subprogram_Call
              (RE (RE_Pad_Compute),
               Make_List_Id
               (Make_Designator (VN (V_CDR_Position)),
                Make_Designator (VN (V_Buffer_Size)),
                Make_Designator (PN (P_Data_Alignment))));
            Append_Node_To_List (N, Server_Statements);


            --  Initilize body alignment to "1"

            if Contains_Out_Parameters (E) then
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (PN (P_Data_Alignment)),
                  Make_Literal (Int1_Val));
               Append_Node_To_List (N, Server_Statements);
               Alignment_Const := False;
            end if;

            --  Compute memory needed for result marshalling

            N := Make_Defining_Identifier (PN (P_Returns));
            Set_Correct_Parent_Unit_Name (N, Copy_Node (Args_Id));
            N := Compute_Padding (N, T, PN (P_Buffer));
            Append_Node_To_List (N, Server_Statements);
         end if;


         --  Handling parameters

         if not FEU.Is_Empty (P) then

            --  Body alignment
            if Contains_Out_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Compute),
                  Make_List_Id
                  (Make_Designator (VN (V_CDR_Position)),
                   Make_Designator (VN (V_Buffer_Size)),
                   Make_Designator (PN (P_Data_Alignment))));

               Append_Node_To_List (N, Server_Statements);
            end if;

            if Contains_In_Parameters (E) then
               N := Make_Subprogram_Call
                 (RE (RE_Pad_Compute),
                  Make_List_Id
                  (Make_Designator (VN (V_CDR_Position)),
                   Make_Designator (VN (V_Buffer_Size)),
                   Make_Designator (PN (P_Data_Alignment))));

               Append_Node_To_List (N, Client_Statements);
            end if;

            --  Parameters

            Parameter := First_Entity (P);

            while Present (Parameter) loop
               Rewinded_Type  := FEU.Get_Original_Type (Type_Spec (Parameter));
               Parameter_Name := To_Ada_Name
                 (IDL_Name
                  (Identifier
                   (Declarator
                    (Parameter))));

               Parameter_Mode := FEN.Parameter_Mode (Parameter);

               --  The IN    parameters are marshalled by client
               --  The OUT   parameters are marshalled by server
               --  The INOUT parameters are marshalled by client and server

               --  Explaining comment

               Set_Str_To_Name_Buffer
                 ("padding for Parameter : ");
               Get_Name_String_And_Append (Parameter_Name);
               Add_Str_To_Name_Buffer (" => ");
               Add_Str_To_Name_Buffer
                 (FEN.Node_Kind'Image
                  (FEN.Kind
                   (FEU.Get_Original_Type
                    (Type_Spec
                     (Parameter)))));

               if Is_In (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_Node_To_List (N, Client_Statements);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Make_Ada_Comment (Name_Find);
                  Append_Node_To_List (N, Server_Statements);
               end if;

               --  Compute parameter size

               if Is_In (Parameter_Mode) then
                  N := Make_Defining_Identifier (Parameter_Name);
                  Set_Correct_Parent_Unit_Name (N, Copy_Node (Args_Id));
                  N := Compute_Padding
                    (N,
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_Node_To_List (N, Client_Statements);
               end if;

               if Is_Out (Parameter_Mode) then
                  N := Make_Defining_Identifier (Parameter_Name);
                  Set_Correct_Parent_Unit_Name (N, Copy_Node (Args_Id));
                  N := Compute_Padding
                    (N,
                     Type_Spec (Parameter),
                     PN (P_Buffer));
                  Append_Node_To_List (N, Server_Statements);
               end if;
               Parameter := Next_Entity (Parameter);
            end loop;

         end if;

         --  Allocate Buffer_Size octets

         if not BEU.Is_Empty (Client_Statements) then
            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               Make_List_Id
               (Make_Designator (PN (P_Buffer)),
                Make_Designator (VN (V_Buffer_Size))));

            Append_Node_To_List (N, Client_Statements);
         end if;

         if not BEU.Is_Empty (Server_Statements) then
            N := Make_Subprogram_Call
              (RE (RE_Preallocate_Buffer),
               Make_List_Id
               (Make_Designator (PN (P_Buffer)),
                Make_Designator (VN (V_Buffer_Size))));

            Append_Node_To_List (N, Server_Statements);
         end if;

         --  The declarative part of the subprogram :

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            declare
               Unref_Entities : constant array (Positive range <>) of Name_Id
                 := (PN (P_Role),
                     PN (P_Args),
                     PN (P_Buffer),
                     PN (P_First_Arg_Alignment));
            begin
               for Index in Unref_Entities'Range loop
                  N := Make_Subprogram_Call
                    (Make_Designator (GN (Pragma_Unreferenced)),
                     Make_List_Id
                     (Make_Designator (Unref_Entities (Index))));
                  N := Make_Pragma_Statement (N);
                  Append_Node_To_List (N, Subp_Declarations);
               end loop;
            end;
         else
            --  It's complicated to determin if the parameters 'Args'
            --  is or isn't refrenced (depending) on the types
            --  handled. So we ignore warnings raised about these
            --  parameter

            N := Make_Subprogram_Call
              (Make_Designator (GN (Pragma_Warnings)),
               Make_List_Id
               (RE (RE_Off),
                Make_Designator (PN (P_Args))));

            N := Make_Pragma_Statement (N);
            Append_Node_To_List (N, Subp_Declarations);

            --  Common declarations

            --  1/ Data_Alignment : This variable modified when there are
            --     OUT or INOUT parameters in order to avoid the alignment
            --     of buffer more than one time

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Data_Alignment)),
               Object_Definition   => RE (RE_Alignment_Type),
               Constant_Present    => Alignment_Const,
               Expression          => Make_Designator
               (PN (P_First_Arg_Alignment)));
            Append_Node_To_List (N, Subp_Declarations);

            --  Use type instruction for arithmetic operation on
            --  Buffer_Size and CDR_Position

            N := Make_Used_Type
              (Make_Designator
               (Fully_Qualified_Name
                (RE (RE_Unsigned_Long_1))));

            Append_Node_To_List (N, Subp_Declarations);

            N := Make_Used_Type
              (Make_Designator
               (Fully_Qualified_Name
                (RE (RE_Stream_Element_Count))));

            Append_Node_To_List (N, Subp_Declarations);

            --  Buffer_Size declaration and initialization

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_Buffer_Size)),
               Object_Definition   => RE (RE_Stream_Element_Count),
               Constant_Present    => False,
               Expression          =>

                 Make_Expression
               (Make_Subprogram_Call
                (RE (RE_CDR_Position),
                 Make_List_Id (Make_Designator (PN (P_Buffer)))),
                Op_Minus,
                Make_Subprogram_Call
                (RE (RE_Length),
                 Make_List_Id (Make_Designator (PN (P_Buffer))))));

            Append_Node_To_List (N, Subp_Declarations);

            --  CDR_Position declaration and initialization

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (VN (V_CDR_Position)),
               Object_Definition   => RE (RE_Stream_Element_Count),
               Constant_Present    => False,
               Expression          =>
                 Make_Subprogram_Call
               (RE (RE_CDR_Position),
                Make_List_Id (Make_Designator (PN (P_Buffer)))));

            Append_Node_To_List (N, Subp_Declarations);

            --  2/ This is the record that contains the operation parameters

            N := Expand_Designator
              (Type_Def_Node
               (BE_Node
                (Identifier
                 (E))));

            M := Make_Designator
              (Designator => PN (P_Args),
               Is_All     => True);
            N := Make_Object_Declaration
              (Defining_Identifier => Args_Id,
               Object_Definition   => N,
               Expression          => Make_Subprogram_Call
               (N, Make_List_Id (M)));
            Append_Node_To_List (N, Subp_Declarations);

            --  The record can be not used

            N := Make_Subprogram_Call
              (Make_Designator (GN (Pragma_Warnings)),
               Make_List_Id
               (RE (RE_Off),
                Args_Id));
            N := Make_Pragma_Statement (N);

            Append_Node_To_List (N, Subp_Declarations);

            N := Make_Subprogram_Call
              (Make_Designator (GN (Pragma_Unreferenced)),
               Make_List_Id
               (Args_Id));

            N := Make_Pragma_Statement (N);
            Append_Node_To_List (N, Subp_Declarations);
         end if;

         --  If the subprogram is a procedure without arguments, we add a
         --  null statement to the subprogram statements, else we build a
         --  swithch case

         if BEU.Is_Empty (Client_Statements)
           and then BEU.Is_Empty (Server_Statements)
         then
            Append_Node_To_List (Make_Null_Statement, Subp_Statements);
         else
            --  Building the case statement

            if BEU.Is_Empty (Client_Statements) then
               Append_Node_To_List (Make_Null_Statement, Client_Statements);
            end if;

            N := Make_Case_Statement_Alternative
              (Client_Case, Client_Statements);
            Append_Node_To_List (N, Case_Alternatives);

            if BEU.Is_Empty (Server_Statements) then
               Append_Node_To_List (Make_Null_Statement, Server_Statements);
            end if;

            N := Make_Case_Statement_Alternative
              (Server_Case, Server_Statements);
            Append_Node_To_List (N, Case_Alternatives);

            N := Make_Case_Statement
              (Make_Designator (PN (P_Role)), Case_Alternatives);
            Append_Node_To_List (N, Subp_Statements);
         end if;

         --  Building the subprogram implementation

         N := Make_Subprogram_Implementation
           (Specification => Subp_Spec,
            Declarations  => Subp_Declarations,
            Statements    => Subp_Statements);
         return N;
      end Buffer_Size_Body;


      -----------------------------------
      -- Cast_Variable_To_PolyORB_Type --
      -----------------------------------

      function Cast_Variable_To_PolyORB_Type
        (Var_Node : Node_Id; Var_Type : Node_Id)
        return Node_Id
      is
         N         : Node_Id;
         Orig_Type : Node_Id;
      begin
         N := Var_Node;

         Orig_Type := FEU.Get_Original_Type (Var_Type);

         case FEN.Kind (Orig_Type) is

            when K_Long =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Long_1), Make_List_Id (N));
               end;

            when K_Long_Long =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Long_Long_1), Make_List_Id (N));
               end;

            when K_Unsigned_Long =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Long_1), Make_List_Id (N));
               end;

            when K_Unsigned_Long_Long =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Long_Long_1), Make_List_Id (N));
               end;

            when K_Short =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Short_1), Make_List_Id (N));
               end;

            when K_Unsigned_Short =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Short_1), Make_List_Id (N));
               end;

            when K_Float =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Float_1), Make_List_Id (N));
               end;

            when K_Double =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Double_1), Make_List_Id (N));
               end;

            when K_Long_Double =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Long_Double_1), Make_List_Id (N));
               end;

            when K_Char =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Char_1), Make_List_Id (N));
               end;

            when K_Wide_Char =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Wchar_1), Make_List_Id (N));
               end;

            when K_Octet =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Octet_1), Make_List_Id (N));
               end;

            when K_Boolean =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Boolean_1), Make_List_Id (N));
               end;

            when K_Fixed_Point_Type =>
               declare
                  Declaration      : Node_Id;
                  Declarator       : Node_Id;
                  FP_Type_Node     : Node_Id;
               begin

                  --  Getting the fixed point type

                  Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));
                  FP_Type_Node := Expand_Designator
                    (Stub_Type_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  N := Make_Subprogram_Call
                    (FP_Type_Node, Make_List_Id (N));
               end;

            when K_Object =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Ref), Make_List_Id (N));
               end;

            when K_Interface_Declaration =>
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_Ref_2), Make_List_Id (N));
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Ref), Make_List_Id (N));
               end;

            when K_Enumeration_Type =>
               declare
                  Ada_Enum_Type : constant Node_Id := Expand_Designator
                    (Type_Def_Node
                     (BE_Node
                      (Identifier
                       (Orig_Type))));
                  M : Node_Id;
               begin
                  if FEN.Kind (Var_Type) = K_Scoped_Name
                    and then FEN.Kind (Reference (Var_Type))
                    /= K_Enumeration_Type
                  then
                     N := Make_Subprogram_Call
                       (Ada_Enum_Type,
                        Make_List_Id (N));
                  end if;

                  --  Even if the type is not directly an enumeration and
                  --  is defined basing on an enumeration, we still have
                  --  access to the 'Pos attribute. So there is
                  --  no need to cast the variable to  the original
                  --  enumeration type.

                  M := Make_Type_Attribute (Ada_Enum_Type, A_Pos);
                  M := Make_Subprogram_Call (M, Make_List_Id (N));
                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Long_1),
                     Make_List_Id (M));
               end;

            when K_String =>
               begin
                  if FEN.Kind (Var_Type) /= K_String then
                     N := Make_Subprogram_Call
                       (RE (RE_String_0),
                        Make_List_Id (N));
                  end if;

                  N := Make_Subprogram_Call
                    (RE (RE_To_Standard_String),
                     Make_List_Id (N));
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_String),
                     Make_List_Id (N));
               end;

            when K_String_Type =>
               declare
                  Declaration      : Node_Id;
                  Declarator       : Node_Id;
                  Str_Package_Node : Node_Id;
                  Str_Type         : Node_Id;
                  Str_Convert_Subp : Node_Id;
               begin

                  --  Getting the instanciated package node

                  Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));
                  Str_Package_Node := Defining_Identifier
                    (Stub_Package_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  --  Getting the conversion subprogram

                  Str_Type := Make_Designator (TN (T_Bounded_String));
                  Set_Correct_Parent_Unit_Name (Str_Type, Str_Package_Node);

                  Str_Convert_Subp := Make_Designator (SN (S_To_String));
                  Set_Correct_Parent_Unit_Name
                    (Str_Convert_Subp, Str_Package_Node);

                  N := Make_Subprogram_Call
                    (Str_Type,
                     Make_List_Id (N));

                  N := Make_Subprogram_Call
                    (Str_Convert_Subp,
                     Make_List_Id (N));

                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_String),
                     Make_List_Id (N));
               end;

            when K_Wide_String =>
               begin
                  if FEN.Kind (Var_Type) /= K_Wide_String then
                     N := Make_Subprogram_Call
                       (RE (RE_Wide_String),
                        Make_List_Id (N));
                  end if;

                  N := Make_Subprogram_Call
                    (RE (RE_To_Standard_Wide_String),
                     Make_List_Id (N));
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Wide_String),
                     Make_List_Id (N));
               end;

            when K_Wide_String_Type =>
               declare
                  Declaration      : Node_Id;
                  Declarator       : Node_Id;
                  Str_Package_Node : Node_Id;
                  Str_Type         : Node_Id;
                  Str_Convert_Subp : Node_Id;
               begin

                  --  Getting the instanciated package node

                  Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));
                  Str_Package_Node := Defining_Identifier
                    (Stub_Package_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  --  Getting the conversion subprogram

                  Str_Type := Make_Designator (TN (T_Bounded_Wide_String));
                  Set_Correct_Parent_Unit_Name (Str_Type, Str_Package_Node);

                  Str_Convert_Subp := Make_Designator
                    (SN (S_To_Wide_String));
                  Set_Correct_Parent_Unit_Name
                    (Str_Convert_Subp, Str_Package_Node);

                  N := Make_Subprogram_Call
                    (Str_Type,
                     Make_List_Id (N));

                  N := Make_Subprogram_Call
                    (Str_Convert_Subp,
                     Make_List_Id (N));

                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Wide_String),
                     Make_List_Id (N));
               end;

            when K_Sequence_Type =>
               declare
                  Declaration      : Node_Id;
                  Declarator       : Node_Id;
                  Seq_Package_Node : Node_Id;
                  Seq_Type         : Node_Id;
               begin

                  --  Getting the instanciated package node

                  Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));
                  Seq_Package_Node := Defining_Identifier
                    (Stub_Package_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  --  Sequence type

                  Seq_Type := Make_Designator (TN (T_Sequence));
                  Set_Correct_Parent_Unit_Name (Seq_Type, Seq_Package_Node);

                  N := Make_Subprogram_Call
                    (Seq_Type,
                     Make_List_Id (N));
               end;

            when others =>
               null;
         end case;

         return N;
      end Cast_Variable_To_PolyORB_Type;


      -----------
      -- Is_In --
      -----------

      function Is_In (Par_Mode : Mode_Id) return Boolean is
      begin
         return Par_Mode = Mode_In or else Par_Mode = Mode_Inout;
      end Is_In;

      ------------
      -- Is_Out --
      ------------

      function Is_Out (Par_Mode : Mode_Id) return Boolean is
      begin
         return Par_Mode = Mode_Out or else Par_Mode = Mode_Inout;
      end Is_Out;

      ----------------------------
      -- Contains_In_Parameters --
      ----------------------------

      function Contains_In_Parameters (E : Node_Id) return Boolean is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Parameter : Node_Id;
         Result    : Boolean := False;
      begin
         Parameter := First_Entity (Parameters (E));
         while Present (Parameter) loop
            if Is_In (FEN.Parameter_Mode (Parameter)) then
               Result := True;
               exit;
            end if;
            Parameter := Next_Entity (Parameter);
         end loop;
         return Result;
      end Contains_In_Parameters;

      -----------------------------
      -- Contains_Out_Parameters --
      -----------------------------

      function Contains_Out_Parameters (E : Node_Id) return Boolean is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

         Parameter : Node_Id;
         Result    : Boolean := False;
      begin
         Parameter := First_Entity (Parameters (E));
         while Present (Parameter) loop
            if Is_Out (FEN.Parameter_Mode (Parameter)) then
               Result := True;
               exit;
            end if;
            Parameter := Next_Entity (Parameter);
         end loop;
         return Result;
      end Contains_Out_Parameters;

      --------------------
      -- Get_Index_Name --
      --------------------

      function Get_Index_Name return Name_Id is
         Index : Name_Id;
      begin
         Set_Str_To_Name_Buffer ("Index_");
         Index_Number := Index_Number + 1;
         Add_Nat_To_Name_Buffer (Index_Number);
         Index := Name_Find;
         return Index;
      end Get_Index_Name;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

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
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N    : Node_Id;
         D    : Node_Id;
      begin
         Set_Buffers_Body;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            Set_Str_To_Name_Buffer
              ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Statements (Current_Package));

            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No buffers package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Buffers_Body;

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
         if not Map_Particular_CORBA_Parts (E, PK_Buffers_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N     : Node_Id;
      begin
         Set_Buffers_Body;

         Set_Str_To_Name_Buffer
           ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Generating the 'Operation_Name'_Buffer_Size Body
         N := Buffer_Size_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
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

      -----------------------
      --  Compute_Padding  --
      -----------------------

      function Compute_Padding
        (Var_Node : Node_Id;
         Var_Type : Node_Id;
         Buff     : Name_Id)
        return Node_Id
      is
         Block_Dcl        : constant List_Id := New_List (K_List_Id);
         Block_St         : constant List_Id := New_List (K_List_Id);
         N                : Node_Id;
         Type_Spec_Node   : Node_Id;
         Direct_Type_Node : Node_Id;
      begin
         --  Getting the original type

         Type_Spec_Node := FEU.Get_Original_Type (Var_Type);
         if FEN.Kind (Var_Type) = K_Simple_Declarator
           or else FEN.Kind (Var_Type) = K_Complex_Declarator
         then
            Direct_Type_Node := Type_Spec (Declaration (Var_Type));
         else
            Direct_Type_Node := Var_Type;
         end if;

         case FEN.Kind (Type_Spec_Node) is
            when K_Object
              | K_Interface_Declaration =>
               declare
                  Padding_Value : Value_Id;
               begin
                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  we send an IOR so we make a padding on 4 octets

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;
            when K_Wide_Char =>
               declare
                  M : Node_Id;
               begin
                  --  The padding of Wchar depend on the GIOP version
                  --  and the Code Set used so we make a padding for
                  --  the worst case (GIOP 1.1 with ISO 10646 UCS-4CS)

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Designator (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Designator (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  Wchar size

                  M := Make_Subprogram_Call
                    (RE (RE_Type_Size),
                     Make_List_Id
                     (Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression

                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       Make_List_Id (M)),

                      Op_Plus,

                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression

                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       Make_List_Id (M)),

                      Op_Plus,

                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;
            when K_Boolean
              | K_Double
              | K_Float
              | K_Long
              | K_Long_Long
              | K_Octet
              | K_Short
              | K_Unsigned_Long
              | K_Unsigned_Long_Long
              | K_Unsigned_Short
              | K_Enumeration_Type
              | K_Char =>

               declare
                  Padding_Value  : Value_Id;
               begin
                  --  Getting the parameter size
                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  Padding

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (Padding_Value)));

                  Append_Node_To_List (N, Block_St);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;

            when K_String_Type
              | K_Wide_String_Type =>
               declare
                  Padding_Value : Value_Id;
               begin
                  --  Getting the string length

                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  --  Padding for the string length marshalling

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (New_Integer_Value (4, 1, 10)),
                      Op_Plus,
                      Make_Designator (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (New_Integer_Value (4, 1, 10)),
                      Op_Plus,
                      Make_Designator (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);

                  --  Adding the string length to Buffer_Size and
                  --  CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;

            when K_Fixed_Point_Type =>
               declare
                  M            : Node_Id;
                  Declaration  : Node_Id;
                  Declarator   : Node_Id;
               begin
                  --  Getting the fixed_point type

                  Declaration := FEU.Get_Original_Type_Declaration
                    (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));
                  N := Expand_Designator
                    (Stub_Type_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  --  Instanciate the package
                  --  PolyORB.Buffers.Optimization.Fixed_Point

                  N := Make_Package_Instantiation
                    (Make_Defining_Identifier (VN (V_FXS)),
                     RU (RU_PolyORB_Buffers_Optimization_Fixed_Point),
                     Make_List_Id
                     (N));
                  Append_Node_To_List (N, Block_Dcl);

                  --  Make the suprogram call

                  N := Make_Designator
                    (Designator => SN (S_Type_Size),
                     Parent     => VN (V_FXS));


                  M := Make_Subprogram_Call
                    (RE (RE_Stream_Element_Count),
                     (Make_List_Id
                      (Make_Subprogram_Call
                       (N,
                        Make_List_Id
                        (Cast_Variable_To_PolyORB_Type
                         (Var_Node, Direct_Type_Node))))));

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression

                     (M,
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression

                     (M,
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;

            when K_Long_Double =>
               declare
                  Padding_Value  : Value_Id;
               begin
                  --  Alignment for Long Double is not equal to
                  --  the his size (/= 16)

                  Padding_Value := Parameter_Size (Type_Spec_Node);

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (8, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  Update Buffer_Size and CDR_Position

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Literal (Padding_Value),
                      Op_Plus,
                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;

            when K_String
              | K_Wide_String =>
               declare
                  M : Node_Id;
               begin
                  --  Padding for string length

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  Update Buffer_Size and CDR_Position for the
                  --  marshalling of string length

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Designator (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);


                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Designator (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  Call of Type_Size subprogram return the string
                  --  length

                  M := Make_Subprogram_Call
                    (RE (RE_Type_Size),
                     Make_List_Id
                     (Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression

                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       Make_List_Id (M)),

                      Op_Plus,

                      Make_Defining_Identifier (VN (V_Buffer_Size))));

                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression

                     (Make_Subprogram_Call
                      (RE (RE_Stream_Element_Count),
                       Make_List_Id (M)),

                      Op_Plus,

                      Make_Defining_Identifier (VN (V_CDR_Position))));

                  Append_Node_To_List (N, Block_St);
               end;

            when K_Sequence_Type =>
               declare
                  Declaration      : Node_Id;
                  Declarator       : Node_Id;
                  Seq_Package_Node : Node_Id;
                  Seq_Element      : Node_Id;
                  Index_Node       : Node_Id;
                  Range_Constraint : Node_Id;
                  Padding_Value    : Value_Id;
                  For_Statements   : constant List_Id := New_List (K_List_Id);
               begin
                  --  padding for sequence length

                  N := Make_Subprogram_Call
                    (RE (RE_Pad_Compute),
                     Make_List_Id
                     (Make_Designator (VN (V_CDR_Position)),
                      Make_Designator (VN (V_Buffer_Size)),
                      Make_Literal (New_Integer_Value (4, 1, 10))));

                  Append_Node_To_List (N, Block_St);

                  --  updating Buffer_Size and CDR_Position for
                  --  sequence length marshalling

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Buffer_Size)),
                     Make_Expression
                     (Make_Designator (VN (V_Buffer_Size)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));
                  Append_Node_To_List (N, Block_St);

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_CDR_Position)),
                     Make_Expression
                     (Make_Designator (VN (V_CDR_Position)),
                      Op_Plus,
                      Make_Literal (New_Integer_Value (4, 1, 10))));
                  Append_Node_To_List (N, Block_St);

                  --  Getting the instanciated package node

                  Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
                  Declarator := First_Entity (Declarators (Declaration));

                  Seq_Package_Node := Defining_Identifier
                    (Stub_Package_Node
                     (BE_Ada_Instanciations
                      (BE_Node
                       (Identifier
                        (Declarator)))));

                  --  Getting the sequence length

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (VN (V_Seq_Len)),
                     Object_Definition   => RE (RE_Unsigned_Long_1));

                  Append_Node_To_List (N, Block_Dcl);

                  N := Make_Designator (SN (S_Length));
                  Set_Correct_Parent_Unit_Name (N, Seq_Package_Node);

                  N := Make_Subprogram_Call
                    (N,
                     Make_List_Id
                     (Cast_Variable_To_PolyORB_Type
                      (Var_Node, Direct_Type_Node)));


                  N := Make_Subprogram_Call
                    (RE (RE_Unsigned_Long_1),
                     Make_List_Id (N));

                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Seq_Len)), N);

                  Append_Node_To_List (N, Block_St);


                  N := Make_Designator (SN (S_Element_Of));
                  Set_Correct_Parent_Unit_Name (N, Seq_Package_Node);


                  --  verify if the element type is or is not simple

                  Padding_Value := Parameter_Size (Type_Spec (Type_Spec_Node));

                  if Padding_Value = Int0_Val then
                     --  Sequence element type is complex

                     Index_Node := Make_Defining_Identifier (VN (V_Index));

                     --  Creating the range constraint

                     Range_Constraint := New_Node (K_Range_Constraint);
                     Set_First
                       (Range_Constraint,
                        Make_Literal (Int1_Val));

                     Set_Last
                       (Range_Constraint,
                        Make_Defining_Identifier (VN (V_Seq_Len)));

                     --  Getting the sequence element

                     Seq_Element := Make_Subprogram_Call
                       (N,
                        Make_List_Id
                        (Cast_Variable_To_PolyORB_Type
                         (Var_Node, Direct_Type_Node),
                         Make_Subprogram_Call
                         (RE (RE_Positive),
                          Make_List_Id (Index_Node))));

                     N := Compute_Padding
                       (Var_Node => Seq_Element,
                        Var_Type => Type_Spec (Type_Spec_Node),
                        Buff     => Buff);
                     Append_Node_To_List (N, For_Statements);

                     --  Building the loop

                     N := Make_For_Statement
                       (Index_Node,
                        Range_Constraint,
                        For_Statements);
                     Append_Node_To_List (N, Block_St);

                  else
                     --  Sequence element type is simple so we can
                     --  compute sequence size without a loop

                     declare
                        M : Node_Id;
                     begin
                        --  Padding for the first element

                        N := Make_Subprogram_Call
                          (RE (RE_Pad_Compute),
                           Make_List_Id
                           (Make_Designator (VN (V_CDR_Position)),
                            Make_Designator (VN (V_Buffer_Size)),
                            Make_Literal (Padding_Value)));

                        Append_Node_To_List (N, Block_St);

                        --  Multiply len by sequence element size

                        M := Make_Expression
                          (Make_Defining_Identifier (VN (V_Seq_Len)),
                           Op_Asterisk,
                           Make_Literal (Padding_Value));

                        --  Update buffer_size and CDR_Position

                        N := Make_Assignment_Statement
                          (Make_Defining_Identifier (VN (V_Buffer_Size)),

                           Make_Expression
                           (Make_Subprogram_Call
                            (RE (RE_Stream_Element_Count),
                             Make_List_Id (M)),

                            Op_Plus,

                            Make_Defining_Identifier (VN (V_Buffer_Size))));
                        Append_Node_To_List (N, Block_St);

                        N := Make_Assignment_Statement
                          (Make_Defining_Identifier (VN (V_CDR_Position)),

                           Make_Expression
                           (Make_Subprogram_Call
                            (RE (RE_Stream_Element_Count),
                             Make_List_Id (M)),

                            Op_Plus,

                            Make_Defining_Identifier (VN (V_CDR_Position))));
                        Append_Node_To_List (N, Block_St);
                     end;
                  end if;
               end;
            when K_Complex_Declarator =>
               declare
                  I                    : Nat := 0;
                  Sizes                : constant List_Id :=
                    Range_Constraints
                    (Type_Definition
                     (Type_Def_Node
                      (BE_Node
                       (Identifier
                        (Type_Spec_Node)))));
                  Dim                  : Node_Id;
                  Loop_Statements      : List_Id := No_List;
                  Enclosing_Statements : List_Id;
                  Index_List           : constant List_Id :=
                    New_List (K_List_Id);
                  Index_Node           : Node_Id := No_Node;
                  Index_Name           : constant Name_Id :=
                    Get_Index_Name;

               begin
                  --  Building the nested loops

                  Dim := First_Node (Sizes);
                  loop
                     Get_Name_String (Index_Name);
                     Add_Char_To_Name_Buffer ('_');
                     Add_Nat_To_Name_Buffer (I);
                     Index_Node := Make_Defining_Identifier
                       (Add_Suffix_To_Name (Var_Suffix, Name_Find));
                     Append_Node_To_List (Index_Node, Index_List);
                     Enclosing_Statements := Loop_Statements;
                     Loop_Statements := New_List (K_List_Id);
                     N := Make_For_Statement
                       (Index_Node, Dim, Loop_Statements);

                     if I > 0 then
                        Append_Node_To_List (N, Enclosing_Statements);
                     else
                        Append_Node_To_List (N, Block_St);
                     end if;

                     I := I + 1;
                     Dim := Next_Node (Dim);
                     exit when No (Dim);
                  end loop;

                  --  Filling the statements of the deepest loop by the
                  --  making padding for the correspnding array element

                  N := Make_Subprogram_Call (Var_Node, Index_List);
                  N := Compute_Padding
                    (Var_Node => N,
                     Var_Type => Type_Spec (Declaration (Type_Spec_Node)),
                     Buff     => Buff);
                  Append_Node_To_List (N, Loop_Statements);

               end;

            when K_Structure_Type =>
               declare
                  Member       : Node_Id;
                  Declarator   : Node_Id;
                  Dcl_Ada_Name : Name_Id;
                  Dcl_Ada_Node : Node_Id;
               begin
                  Member := First_Entity (Members (Type_Spec_Node));
                  while Present (Member) loop
                     Declarator := First_Entity (FEN.Declarators (Member));
                     while Present (Declarator) loop

                        --  Getting the record field name

                        Dcl_Ada_Name := To_Ada_Name
                          (IDL_Name
                           (Identifier
                            (Declarator)));
                        Dcl_Ada_Node := Make_Designator (Dcl_Ada_Name);
                        Set_Correct_Parent_Unit_Name (Dcl_Ada_Node, Var_Node);

                        --  Marshalling the record field

                        N := Compute_Padding
                          (Var_Node => Dcl_Ada_Node,
                           Var_Type => Declarator,
                           Buff     => Buff);
                        Append_Node_To_List (N, Block_St);

                        Declarator := Next_Entity (Declarator);
                     end loop;
                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_Union_Type =>
               declare
                  Switch_Node         : Node_Id;
                  Switch_Alternatives : List_Id;
                  Switch_Alternative  : Node_Id;
                  Variant             : Node_Id;
                  Choices             : List_Id;
                  Choice              : Node_Id;
                  Label               : Node_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Block_Statements    : List_Id;
                  Switch_Type         : Node_Id;
                  Dcl_Ada_Name        : Name_Id;
                  Dcl_Ada_Node        : Node_Id;
                  Declarator          : Node_Id;
               begin

                  --  1/ Marshall the union switch

                  Switch_Node := Make_Designator (CN (C_Switch));
                  Set_Correct_Parent_Unit_Name (Switch_Node, Var_Node);

                  N := Compute_Padding
                 (Var_Node => Switch_Node,
                  Var_Type => Switch_Type_Spec (Type_Spec_Node),
                  Buff     => Buff);

                  Append_Node_To_List (N, Block_St);

                  --  2/ Depending on the switch value, marshall the
                  --  corresponding flag

                  Switch_Type := FEU.Get_Original_Type
                    (Switch_Type_Spec
                     (Type_Spec_Node));
                  if FEN.Kind (Switch_Type) = K_Enumeration_Type then
                     Literal_Parent := Map_Designator
                       (Scope_Entity
                        (Identifier
                         (Switch_Type)));
                  end if;

                  Switch_Alternatives := New_List (K_Variant_List);
                  Switch_Alternative := First_Entity
                    (Switch_Type_Body
                     (Type_Spec_Node));

                  while Present (Switch_Alternative) loop
                     Variant := New_Node (K_Variant);
                     Choices := New_List (K_Discrete_Choice_List);
                     Label   := First_Entity (Labels (Switch_Alternative));
                     while Present (Label) loop

                        Choice := Make_Literal
                          (Value             => FEN.Value (Label),
                           Parent_Designator => Literal_Parent);
                        Append_Node_To_List (Choice, Choices);
                        Label := Next_Entity (Label);
                     end loop;
                     Block_Statements := New_List (K_List_Id);

                     --  Getting the field name

                     Declarator := FEN.Declarator
                       (Element
                        (Switch_Alternative));

                     Dcl_Ada_Name := To_Ada_Name
                       (IDL_Name
                        (Identifier
                         (Declarator)));
                     Dcl_Ada_Node := Make_Designator (Dcl_Ada_Name);
                     Set_Correct_Parent_Unit_Name (Dcl_Ada_Node, Var_Node);

                     --  Marshalling the record field

                     N := Compute_Padding
                       (Var_Node => Dcl_Ada_Node,
                        Var_Type => Declarator,
                        Buff     => Buff);
                     Append_Node_To_List (N, Block_Statements);

                     --  Building the switch alternative

                     N := Make_Block_Statement
                       (Declarative_Part => No_List,
                        Statements       => Block_Statements);

                     Set_Component (Variant, N);
                     Set_Discrete_Choices (Variant, Choices);
                     Append_Node_To_List (Variant, Switch_Alternatives);

                     Switch_Alternative := Next_Entity (Switch_Alternative);
                  end loop;

                  N := Make_Variant_Part
                    (Switch_Node,
                     Switch_Alternatives);
                  Append_Node_To_List (N, Block_St);

               end;

            when others =>
               Append_Node_To_List (Make_Null_Statement, Block_St);
         end case;

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_St);
         return N;
      end Compute_Padding;

      ----------------------
      --  Parameter_Size  --
      ----------------------

      function Parameter_Size (N : Node_Id)
                              return Value_Id
      is
         Type_Spec_Node : Node_Id;
      begin

         Type_Spec_Node := FEU.Get_Original_Type (N);

         case FEN.Kind (Type_Spec_Node) is

            when K_Boolean
              | K_Char =>
               return New_Integer_Value (1, 1, 10);

            when K_Short
              | K_Unsigned_Short =>
               return New_Integer_Value (2, 1, 10);

            when K_Unsigned_Long
              | K_Float
              | K_Enumeration_Type
              | K_Long =>
               return New_Integer_Value (4, 1, 10);

            when K_Long_Long
              | K_Unsigned_Long_Long
              | K_Double =>
               return New_Integer_Value (8, 1, 10);

            when K_Long_Double =>
               return New_Integer_Value (16, 1, 10);

            when K_Octet =>
               return New_Integer_Value (1, 1, 10);

            when K_String_Type
              | K_Wide_String_Type =>
               declare
                  String_Size : Unsigned_Long_Long;
               begin
                  String_Size := Unsigned_Long_Long'Value
                    (Values.Image (FEN.Value (Max_Size (Type_Spec_Node))));
                  return New_Integer_Value (String_Size, 1, 10);
               end;

            when K_Object
              | K_Interface_Declaration =>
               --  Just an estimation is not a fixed value for IOR length
               return New_Integer_Value (176, 1, 10);

            when others =>
               return Int0_Val;
         end case;
      end Parameter_Size;

   end Package_Body;
end Backend.BE_CORBA_Ada.Buffers;
