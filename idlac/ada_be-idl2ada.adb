with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Tree.Accessors; use Idl_Fe.Tree.Accessors;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;

package body Ada_Be.Idl2Ada is

   -------------------------------------------------
   -- General purpose code generation subprograms --
   -------------------------------------------------

   procedure Generate_Scope_Stubs
     (Node : N_Root_Acc);
   --  procedure Generate_Scope_Stream
   --    (Node : N_Root_Acc);
   --  procedure Generate_Scope_Skel
   --    (Node : N_Root_Acc);
   --  procedure Generate_Scope_Impl
   --    (Node : N_Root_Acc);

   procedure Generate_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc);
   --  Generate the package declaration for the
   --  stubs of a node.

   procedure Generate_Node_Default
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc);
   --  Generate the text for a node whose mapping is
   --  common to all generated files.

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Generate_Object_Reference_Declaration
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc;
      Full_View : Boolean);
   --  Generate the declaration of an object reference
   --  type.

   procedure Generate
     (Node : in N_Root_Acc) is
   begin
      pragma Assert (Is_Scope (Node));

      case Get_Kind (Node.all) is
         when
           K_Repository |
           K_Module     =>

            Generate_Scope_Stubs (Node);
            --  Generate_Scope_Stream (Node);

         when
           K_Interface |
           K_ValueType =>

            Generate_Scope_Stubs (Node);
            --  Generate_Scope_Stream (Node);

            --  Generate_Scope_Skel (Node);

            --  if Falgs.Generate_Impl then
            --     Generate_Scope_Impl (Node);
            --  end if;

         when others =>
            --  This never happens.
            raise Program_Error;
      end case;
   end Generate;

   procedure Generate_Scope_Stubs
     (Node : N_Root_Acc)
   is
      Stubs_Name : constant String
        := Ada_Full_Name (Node);

      Stubs_Spec : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Spec);
      Stubs_Body : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Body);

   begin
      case Get_Kind (Node.all) is
         when K_ValueType =>
            --  Not implemented yet.
            raise Program_Error;

         when
           K_Repository |
           K_Module     =>

            declare
               It   : Node_Iterator;
               Decl_Node : N_Root_Acc;
            begin
               Init (It, Contents (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);

                  if Is_Scope (Decl_Node) then
                     Generate_Scope_Stubs (Decl_Node);
                  else
                     Generate_Node_Stubs_Spec (Stubs_Spec, Decl_Node);
                  end if;

                  --  No stubs body for a module or repository.

                  Next (It);
               end loop;
            end;

         when K_Interface =>

            Generate_Object_Reference_Declaration
              (Stubs_Spec, Node, Full_View => False);

            declare
               Forward_Node : constant N_Root_Acc
                 := N_Root_Acc (N_Forward_Interface_Acc'(Forward (Node)));
            begin
               if Forward_Node /= null then
                  --  This interface has a forward declaration.

                  Put_Line (Stubs_Spec, "package Convert_Forward is");
                  Put_Line (Stubs_Spec, "  new "
                       & Ada_Full_Name (Forward_Node)
                       & ".Convert (Ref_Type => Ref);");
               end if;
            end;

            declare
               IT   : Node_Iterator;
               Export_Node : N_Root_Acc;
            begin
               Init (IT, Contents (Node));
               while not Is_End (IT) loop
                  Export_Node := Get_Node (IT);
                  Generate_Node_Stubs_Spec (Stubs_Spec, Export_Node);
                  --  Generate_Node_Stubs_Body (Stubs_Body, Export_Node);

                  if Is_Scope (Export_Node) then
                     Generate_Scope_Stubs (Export_Node);
                  end if;

                  Next (IT);
               end loop;
            end;

            --  XXX Declare also inherited subprograms
            --  (for multiple inheritance.)

            New_Line (Stubs_Spec);
            Put_Line
              (Stubs_Spec,
               "function To_Ref (The_Ref : in CORBA.Object.Ref)");
            Put_Line
              (Stubs_Spec,
               "  return Ref;");
            New_Line (Stubs_Spec);

            Dec_Indent (Stubs_Spec);
            Put_Line (Stubs_Spec, "private");
            Inc_Indent (Stubs_Spec);

            Generate_Object_Reference_Declaration
              (Stubs_Spec, Node, Full_View => True);

         when others =>
            pragma Assert (False);
            --  This never happens.

            null;
      end case;

      Generate (Stubs_Spec);
      Generate (Stubs_Body);
   end Generate_Scope_Stubs;

   procedure Generate_Object_Reference_Declaration
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc;
      Full_View : Boolean) is
   begin
      case Get_Kind (Node.all) is

         when K_Interface =>

               if Parents (Node) = Nil_List then
                  Add_With (CU, "CORBA");
                  Put (CU, "type Ref is new CORBA.Object.Ref with ");
               else
                  declare
                     First_Parent_Name : constant String
                       := Ada_Full_Name (Head (Parents (Node)));
                  begin
                     Add_With (CU, First_Parent_Name);
                     Put (CU,
                          "type Ref is new "
                          & First_Parent_Name
                          & ".Ref with ");
                  end;
               end if;

               if Full_View then
                  Put_Line (CU, "null record;");
               else
                  Put_Line (CU, "private;");
               end if;
               New_Line (CU);

               --  when K_ValueType =>...

         when others =>
            raise Program_Error;

      end case;
   end Generate_Object_Reference_Declaration;

   procedure Generate_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc) is
   begin
      case Get_Kind (Node.all) is

         --  Scopes

         when
           K_Repository |
           K_Module     |
           K_Interface  =>
            null;

         when K_Forward_Interface =>
            declare
               Forward_Instanciation : Compilation_Unit
                 := New_Package (Ada_Full_Name (Node), Unit_Spec);
            begin
               Dec_Indent (Forward_Instanciation);
               Put_Line (Forward_Instanciation, "  new CORBA.Forward;");
               Generate
                 (Forward_Instanciation,
                  Is_Generic_Instanciation => True);
            end;

         -----------------
         -- Value types --
         -----------------

         when K_ValueType =>
            null;
         when K_Forward_ValueType =>
            null;
         when K_Boxed_ValueType =>
            null;
         when K_State_Member =>
            null;
         when K_Initializer =>
            null;

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>

            --  Subprogram name

            New_Line (CU);
            if Get_Kind (Operation_Type (Node).all) = K_Void then
               Put (CU, "procedure ");
            else
               Put (CU, "function ");
            end if;

            Put (CU, Ada_Name (Node));

            --  Formals

            declare
               First_Parameter : Boolean := True;
               It   : Node_Iterator;
               P_Node : N_Root_Acc;
            begin

               Init (It, Parameters (Node));
               while not Is_End (It) loop
                  if First_Parameter then
                     New_Line (CU);
                     Put (CU, "  (");
                     Inc_Indent (CU);
                  end if;

                  P_Node := Get_Node (It);

                  Generate_Node_Stubs_Spec (CU, P_Node);

                  Next (It);
                  if Is_End (It) then
                     Put (CU, ")");
                     Dec_Indent (CU);
                  else
                     Put_Line (CU, ";");
                  end if;
               end loop;
            end;

            --  Return type

            if Get_Kind (Operation_Type (Node).all) /= K_Void then
               New_Line (CU);
               Put (CU, "  return ");
               Generate_Node_Stubs_Spec (CU, Operation_Type (Node));
            end if;

            Put_Line (CU, ";");

            --        when K_Attribute =>
            --  null;

         when K_Param =>

            Generate_Node_Stubs_Spec (CU, N_Root_Acc (Declarator (Node)));
            case Mode (Node) is
               when Mode_In =>
                  Put (CU, " : in ");
               when Mode_Out =>
                  Put (CU, " : out ");
               when Mode_Inout =>
                  Put (CU, " : in out ");
            end case;
            Generate_Node_Stubs_Spec (CU, N_Root_Acc (Param_Type (Node)));

         when K_Exception =>

            Put_Line (CU, Ada_Name (Node) & " : exception;");

         when K_Member =>

            declare
               It   : Node_Iterator;
               Decl_Node : N_Root_Acc;
            begin
               Init (It, Decl (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);

                  Generate_Node_Stubs_Spec (CU, Decl_Node);
                  Put (CU, " : ");
                  Generate_Node_Stubs_Spec (CU, M_Type (Node));
                  Put_Line (CU, ";");

                  Next (It);
               end loop;
            end;

         when K_Enum =>
            Put_Line (CU, "type " & Ada_Name (Node) & " is");

            declare
               First_Enumerator : Boolean := True;
               It   : Node_Iterator;
               E_Node : N_Root_Acc;
            begin

               Init (It, Enumerators (Node));
               while not Is_End (It) loop
                  if First_Enumerator then
                     First_Enumerator := False;
                     Put (CU, "  (");
                     Inc_Indent (CU);
                  end if;

                  E_Node := Get_Node (It);

                  Generate_Node_Stubs_Spec (CU, E_Node);

                  Next (It);
                  if Is_End (It) then
                     Put_Line (CU, ");");
                     Dec_Indent (CU);
                  else
                     Put_Line (CU, ",");
                  end if;
               end loop;
            end;

         when K_Type_Declarator =>
            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               declare
                  It   : Node_Iterator;
                  Decl_Node : N_Root_Acc;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Decl_Node := Get_Node (It);

                     declare
                        Bounds_It : Node_Iterator;
                        Bound_Node : N_Root_Acc;
                        First_Bound : Boolean := True;
                        Is_Array : constant Boolean
                          := not Is_Empty (Array_Bounds (Decl_Node));
                     begin
                        Init (Bounds_It, Array_Bounds (Decl_Node));

                        if Is_Interface
                          and then not Is_Array then
                           --  A typedef where the <type_spec>
                           --  denotes an interface type, and
                           --  which is not an array declaration.
                           Put (CU, "subtype ");
                        else
                           Put (CU, "type ");
                        end if;

                        Generate_Node_Stubs_Spec (CU, Decl_Node);

                        Put (CU, " is ");

                        if Is_Array then
                           while not Is_End (Bounds_It) loop
                              Bound_Node := Get_Node (Bounds_It);
                              if First_Bound then
                                 Put (CU, "array (");
                                 First_Bound := False;
                              else
                                 Put (CU, ", ");
                              end if;

                              Put (CU, "0 .. ");
                              Generate_Node_Stubs_Spec (CU, Bound_Node);
                              Put (CU, " - 1");
                           end loop;
                           Put (CU, ") of ");
                        else
                           if not Is_Interface then
                              Put (CU, "new ");
                           end if;
                        end if;

                        Generate_Node_Stubs_Spec (CU, T_Type (Node));

                        Put_Line (CU, ";");
                        Next (It);
                     end;
                  end loop;
               end;
            end;

         when K_Const_Dcl =>
            null;

         when K_Union =>
            Put_Line (CU, "type " & Ada_Name (Node)
                      & "(Switch : ");
            Generate_Node_Stubs_Spec (CU, Switch_Type (Node));
            Put (CU, " := ");
            Generate_Node_Stubs_Spec (CU, Switch_Type (Node));
            Put_Line (CU, "'First) is record");
            Inc_Indent (CU);
            Put_Line (CU, "case Switch is");
            Inc_Indent (CU);

            declare
               It   : Node_Iterator;
               Case_Node : N_Root_Acc;
            begin
               Init (It, Cases (Node));
               while not Is_End (It) loop
                  Case_Node := Get_Node (It);
                  Generate_Node_Stubs_Spec (CU, Case_Node);

                  Next (It);
               end loop;
            end;

            Dec_Indent (CU);
            Put_Line (CU, "end case;");
            Dec_Indent (CU);
            Put_Line (CU, "end record;");

         when K_Case =>
            declare
               It   : Node_Iterator;
               Label_Node : N_Root_Acc;
               First_Label : Boolean := True;
               Multiple_Labels : Boolean;
            begin
               Init (It, Labels (Node));
               while not Is_End (It) loop
                  Label_Node := Get_Node (It);
                  Next (It);

                  Multiple_Labels := not Is_End (It);

                  if First_Label then
                     Put (CU, "when");
                  end if;

                  if Multiple_Labels then
                     pragma Assert (Label_Node /= null);
                     --  The null label is the "default:"
                     --  one, and must have its own case.

                     if not First_Label then
                        Put_Line (CU, " |");
                     else
                        New_Line (CU);
                     end if;
                  end if;

                  if Label_Node /= null then
                     Generate_Node_Stubs_Spec (CU, Label_Node);
                  else
                     Put (CU, "others");
                  end if;

                  First_Label := False;
               end loop;
            end;
            Put_Line (CU, " =>");
            Inc_Indent (CU);

            Generate_Node_Stubs_Spec (CU, N_Root_Acc (Case_Decl (Node)));
            Put (CU, " : ");
            Generate_Node_Stubs_Spec (CU, N_Root_Acc (Case_Type (Node)));
            Put_Line (CU, ";");

            Dec_Indent (CU);

         when K_Sequence =>
            null;

         when K_Struct =>
            Put_Line (CU, "type " & Ada_Name (Node)
                      & " is record");
            Inc_Indent (CU);

            declare
               It   : Node_Iterator;
               Decl_Node : N_Root_Acc;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);
                  Generate_Node_Stubs_Spec (CU, Decl_Node);

                  Next (It);
               end loop;
            end;

            Dec_Indent (CU);
            Put_Line (CU, "end record;");

         when K_ValueBase =>
            null;
         when K_Native =>
            null;

         when K_Object =>
            null;
         when K_Any =>
            null;
         when K_Void =>
            null;

         when K_Fixed =>

            raise Program_Error;

            --  XXX This mapping shall be used for a
            --  {fixed} note created by the expander, NOT
            --  for the original (anonymous) <fixed_type_spec>.

            --  Put (CU, "delta 10 ** -(");
            --  Generate_Node_Stubs_Spec (CU, N_Root_Acc (Scale (Node)));
            --  Put (CU, ") digits ");
            --  Generate_Node_Stubs_Spec (CU, N_Root_Acc (Digits_Nb (Node)));

         when others =>
            Generate_Node_Default (CU, Node);
      end case;

   end Generate_Node_Stubs_Spec;

   procedure Generate_Node_Default
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc) is
   begin
      case Get_Kind (Node.all) is

         when K_Scoped_Name =>
            declare
               FN : constant String
                 :=  Ada_Full_Name
                 (N_Root_Acc (N_Named_Acc'(Value (Node))));
               VK : constant Node_Kind
                 := Get_Kind (Value (Node).all);
            begin
               Put (CU, FN);
               if VK = K_Interface
                 or else VK = K_Forward_Interface then
                  --  A usage occurence of an interface
                  --  denotes an object reference.
                  Put (CU, ".Ref");

                  --  The object reference type is declared in
                  --  the package that maps the interface.
                  Add_With (CU, FN);
               end if;
            end;

         when K_Declarator =>
            Put (CU, Ada_Name (Node));
            --  A simple or complex (array) declarator.

         --  Base types
         when K_Short =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Short");

         when K_Long =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Long");

         when K_Long_Long =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Long_Long");

         when K_Unsigned_Short =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Unsigned_Short");

         when K_Unsigned_Long =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Unsigned_Long");

         when K_Unsigned_Long_Long =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Unsigned_Long_Long");

         when K_Char =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Char");

         when K_Wide_Char =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Wide_Char");

         when K_Boolean =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Boolean");

         when K_Float =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Float");

         when K_Double =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Double");

         when K_Long_Double =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Long_Double");

         when K_String =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.String");

         when K_Wide_String =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Wide_String");

         when K_Octet =>
            Add_With (CU, "CORBA");
            Put (CU, "CORBA.Octet");


         when K_Enumerator =>
            Put (CU, Ada_Name (Node));

         when K_Or =>                   --  Binary operators.
            null;
         when K_Xor =>
            null;
            --        when K_And =>
            --        when K_Sub =>
            --        when K_Add =>
            --        when K_Shr =>
            --        when K_Shl =>
            --        when K_Mul =>
            --        when K_Div =>
            --        when K_Mod =>
            --        when K_Id =>                   --  Unary operators.
            --        when K_Neg =>
            --        when K_Not =>
            --        when K_Lit_Integer =>          --  Literals.
            --        when K_Lit_Floating_Point =>
            --        when K_Lit_Fixed_Point =>
            --        when K_Lit_Char =>
            --        when K_Lit_Wchar =>
         when K_Lit_String =>
            null;
            --        when K_Lit_Wstring =>
            --        when K_Lit_True =>
            --        when K_Lit_False =>





         when others =>
            null;
      end case;
   end Generate_Node_Default;

end Ada_Be.Idl2Ada;
