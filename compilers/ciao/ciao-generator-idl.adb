----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  The interface description (IDL) generator.
--  $Id: //depot/ciao/main/ciao-generator-idl.adb#18 $

with CIAO.IDL_Tree;   use CIAO.IDL_Tree;
with CIAO.IDL_Syntax; use CIAO.IDL_Syntax;
with CIAO.Namet;      use CIAO.Namet;
with CIAO.Nlists;     use CIAO.Nlists;
with CIAO.Options;    use CIAO.Options;
with CIAO.Types;      use CIAO.Types;

package body CIAO.Generator.IDL is

   use Ada.Text_Io;

   procedure Generate
     (Tree : in Node_Id;
      File : in Ada.Text_Io.File_Type) is

      Indent_Level  : Natural := 0;

      procedure Put_Indent;
      --  Generate the whitespace corresponding to the current
      --  indentation level.

      procedure Put_Name (N : Name_Id);
      --  Generate the textual representation for a Name.

      procedure Generate_Node (N : Node_Id);
      --  Generate the text representation for IDL node N.
      --  File and Indent_String are considered global
      --  variables in this procedure.

      procedure Put_Indent is
         Indent_String : constant String (1 .. Indent * Indent_Level)
           := (others => ' ');
      begin
         Put (File, Indent_String);
      end Put_Indent;

      procedure Put_Name (N : Name_Id) is
      begin
         Get_Name_String (N);
         Put (File, Name_Buffer (1 .. Name_Len));
      end Put_Name;

      procedure Generate_Node (N : Node_Id) is
      begin
         case Node_Kind (N) is

            -------------
            -- Keyword --
            -------------

            when N_Keyword_Default =>
               Put (File, "default");
            when N_Keyword_Void =>
               Put (File, "void");
            when N_Keyword_In =>
               Put (File, "in");
            when N_Keyword_Out =>
               Put (File, "out");
            when N_Keyword_Inout =>
               Put (File, "inout");

            ---------------
            -- Base type --
            ---------------

            when N_Base_Type_Char =>
               Put (File, "char");
            when N_Base_Type_Boolean =>
               Put (File, "boolean");
            when N_Base_Type_Long =>
               Put (File, "long");
            when N_Base_Type_Double =>
               Put (File, "double");
            when N_Base_Type_Unsigned_Long =>
               Put (File, "unsigned long");
            when N_Base_Type_Long_Long =>
               Put (File, "long long");
            when N_Base_Type_Long_Double =>
               Put (File, "long double");
            when N_Base_Type_Unsigned_Long_Long =>
               Put (File, "unsigned long long");
            when N_Base_Type_String =>
               Put (File, "string");
            when N_Base_Type_Octet =>
               Put (File, "octet");

            ---------------------
            -- Other terminals --
            ---------------------

            -- Actually generated in N_Scoped_Name.
            -- when N_Absolute =>
            --    Put (File, "::");

            when N_Preprocessor_Include =>
               if Unit_Used (N) then
                  Put (File, "#include """);
                  Put_Name (Name (N));
                  Put_Line (File, """");
               end if;

            -------------------
            -- Non-terminals --
            -------------------

            when N_Specification =>
               declare
                  Dir : Node_Id := First (Directives (N));
                  Def : Node_Id := First (Definitions (N));
                  Int : Node_Id := First (Interfaces (N));
               begin
                  while Present (Dir) loop
                     Generate_Node (Dir);
                     Dir := Next (Dir);
                  end loop;

                  New_Line (File);

                  while Present (Def) loop
                     Generate_Node (Def);
                     Put_Line (File, ";");
                     Def := Next (Def);
                  end loop;
                  while Present (Int) loop
                     Generate_Node (Int);
                     Put_Line (File, ";");
                     Int := Next (Int);
                  end loop;
               end;

            when N_Module =>
               declare
                  Def       : Node_Id := First (Definitions (N));
                  Interface : Node_Id := First (Interfaces (N));
               begin
                  Put_Indent;
                  Put (File, "module ");
                  Put_Name (Name (N));
                  Put (File, " {");
                  Indent_Level := Indent_Level + 1;
                  while Present (Def) loop
                     New_Line (File);
                     Generate_Node (Def);
                     Put_Line (File, ";");
                     Def := Next (Def);
                  end loop;
                  while Present (Interface) loop
                     New_Line (File);
                     Generate_Node (Interface);
                     Put_Line (File, ";");
                     Interface := Next (Interface);
                  end loop;
                  Indent_Level := Indent_Level - 1;
                  Put_Indent;
                  Put (File, "}");
               end;

            when N_Interface =>
               Generate_Node (Specific_Interface (N));

            when N_Interface_Dcl =>
               declare
                  Export : Node_Id := First (Interface_Body (N));
                  Int : Node_Id := First (Interfaces (N));
               begin
                  Generate_Node (Interface_Header (N));
                  Put_Line (File, " {");
                  Indent_Level := Indent_Level + 1;
                  while Present (Export) loop
                     Put_Indent;
                     Generate_Node (Export);
                     Put_Line (File, ";");
                     Export := Next (Export);
                  end loop;

                  while Present (Int) loop
                     Generate_Node (Int);
                     Int := Next (Int);
                  end loop;

                  Indent_Level := Indent_Level - 1;
                  Put_Indent;
                  Put (File, "}");
               end;

            when N_Forward_Dcl =>
               Put_Indent;
               Put (File, "interface ");
               Put_Name (Name (N));

            when N_Interface_Header =>
               declare
                  Inh  : Node_Id := First (Inheritance_Spec (N));
                  First_Inheritance_Spec : Boolean := True;
               begin
                  Put_Indent;
                  Put (File, "interface ");
                  Put_Name (Name (N));

                  while Present (Inh) loop
                     if First_Inheritance_Spec then
                        Put (File, " : ");
                        First_Inheritance_Spec := False;
                     else
                        Put (File, ", ");
                     end if;
                     Generate_Node (Inh);
                     Inh := Next (Inh);
                  end loop;
               end;

            when N_Scoped_Name =>
               if Present (Prefix (N)) then
                  if Node_Kind (Prefix (N)) = N_Scoped_Name then
                     Generate_Node (Prefix (N));
                  end if;
                  Put (File, "::");
               end if;
               Put_Name (Name (N));

            when N_Type_Dcl =>
               Put_Indent;
               Put (File, "typedef ");
               Generate_Node (Type_Declarator (N));

            when
              N_Type_Declarator |
              N_Member          =>
               --  Indentation and ";" around N_Member are
               --  actually generated in enclosing node.
               Generate_Node (Type_Spec (N));
               Put (File, " ");

               declare
                  Decl : Node_Id := First (Declarators (N));
                  First_Declarator : Boolean := True;
               begin
                  while Present (Decl) loop
                     if First_Declarator then
                        First_Declarator := False;
                     else
                        Put (File, ", ");
                     end if;
                     Generate_Node (Decl);
                     Decl := Next (Decl);
                  end loop;
               end;

            when N_Declarator =>
               Generate_Node (Specific_Declarator (N));

            when
              N_Simple_Declarator |
              N_Enumerator        =>
               Put_Name (Name (N));

            when N_Simple_Type_Spec  =>
               -- XXX Check that!
               -- XXX Untyped traversal! Node1 is either <scoped_name> or <template_type_spec>.
               Generate_Node (Node1 (N));

            when N_Type_Spec =>
               Generate_Node (Specific_Type_Spec (N));

            when N_Constr_Type_Spec =>
               Generate_Node (Structure (N));

            when N_Struct_Type =>
               declare
                  Member : Node_Id := First (Members (N));
               begin
                  Put (File, "struct ");
                  Put_Name (Name (N));
                  Put_Line (File, " {");
                  Indent_Level := Indent_Level + 1;
                  while Present (Member) loop
                     Put_Indent;
                     Generate_Node (Member);
                     Put_Line (File, ";");
                     Member := Next (Member);
                  end loop;
                  Indent_Level := Indent_Level - 1;
                  Put_Indent;
                  Put (File, "}");
               end;

            when N_Union_Type =>
               -- XXX TODO
               raise Program_Error;

            when N_Case_Element =>
               -- XXX TODO
               raise Program_Error;

            when N_Element_Spec =>
               -- XXX TODO
               raise Program_Error;

            when N_Enum_Type =>
               declare
                  Enumerator : Node_Id := First (Enumerators (N));
               begin
                  Put (File, "enum ");
                  Put_Name (Name (N));
                  Put_Line (File, " {");
                  Indent_Level := Indent_Level + 1;
                  while Present (Enumerator) loop
                     Put_Indent;
                     Generate_Node (Enumerator);
                     Enumerator := Next (Enumerator);
                     if Present (Enumerator) then
                        Put (File, ",");
                     end if;
                     New_Line (File);
                  end loop;
                  Indent_Level := Indent_Level - 1;
                  Put_Indent;
                  Put (File, "}");
               end;

            when N_Sequence_Type =>
               Put (File, "sequence<");
               Generate_Node (Specific_Type_Spec (N));
               Put (File, " >");

            when N_Array_Declarator =>
               declare
                  Array_Size : Node_Id := First (Fixed_Array_Sizes (N));
               begin
                  Put_Name (Name (N));

                  while Present (Array_Size) loop
                     Put (File, "[");
                     Put (File, Unbiased_Uint'Image (Size_Value (Array_Size)));
                     Put (File, "]");
                     Array_Size := Next (Array_Size);
                  end loop;
               end;

            when N_Op_Dcl =>
               declare
                  Param_Dcl : Node_Id := First (Param_Dcls (N));
                  First_Parameter : Boolean := True;
               begin
                  --  <op_attribute> NOT IMPLEMENTED.
                  Generate_Node (Op_Type_Spec (N));
                  Put (File, " ");
                  Put_Name (Name (N));
                  Put (File, " (");

                  while Present (Param_Dcl) loop
                     if First_Parameter then
                        First_Parameter := False;
                     else
                        Put (File, ", ");
                     end if;
                     Generate_Node (Param_Dcl);
                     Param_Dcl := Next (Param_Dcl);
                  end loop;
                  Put (File, ")");

                  -- XXX TODO
                  -- Generate_Node (Raises_Expr (N));
                  --  <context_expr> NOT IMPLEMENTED.
               end;

            when N_Op_Type_Spec =>
               Generate_Node (Operation_Value_Type (N));

            when N_Param_Type_Spec =>
               -- XXX ugly abstraction violation!
               Generate_Node (Node1 (N));

            when N_Param_Dcl =>
               Generate_Node (Parameter_Attribute (N));
               Put (File, " ");
               Generate_Node (Param_Type_Spec (N));
               Put (File, " ");
               Put_Name (Name (N));

            when N_Param_Attribute =>
               -- XXX ugly abstraction violation! Untyped traversal.
               Generate_Node (Node1 (N));

            when N_Raises_Expr =>
               -- XXX TODO
               raise Program_Error;
            when N_Empty =>
               -- XXX DEBUG ONLY
               Put (File, "XXX N_Empty XXX");
            when others =>
               --  Impossible, should not happen!
               --  (N_Empty, N_Error, N_Unused_At_Start).
               raise Program_Error;
         end case;
      end Generate_Node;

   begin
      Generate_Node (Tree);
   end Generate;

end CIAO.Generator.IDL;
