with Ada.Text_IO; use Ada.Text_IO;

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Tree.Accessors; use Idl_Fe.Tree.Accessors;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;

package body Ada_Be.Idl2Ada is

   ---------------
   -- Constants --
   ---------------

   Stream_Suffix : constant String
     := ".Stream";
   --  The name of the CDR packages for a scope.

   -------------------------------------------------
   -- General purpose code generation subprograms --
   -------------------------------------------------

   procedure Generate_Scope
     (Node : N_Root_Acc);
   --  Generate all the files for scope Node.

   procedure Generate_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc);
   --  Generate the package declaration for the
   --  stubs of a node.

   --  procedure Generate_Node_Stubs_Body
   --    (CU   : in out Compilation_Unit;
   --     Node : N_Root_Acc);
   --  Generate the package declaration for the
   --  stubs of a node.

   procedure Generate_Node_Stream_Spec
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc);
   --  Generate the package declaration for the
   --  marchalling function of a node.

   procedure Generate_Node_Stream_Body
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc);
   --  Generate the package declaration for the
   --  marchalling function of a node.

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

   procedure Generate_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String);
   --  Generate the profile for the Marshall procedure
   --  of a type.
   --  FIXME: This is marshall-by-value.
   --         Marshall-by-reference should be produced
   --         as well.

   procedure Generate_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String);
   --  Generate the profile for the Unmarshall function
   --  of a type.
   --  FIXME: This is unmarshall-by-value (see aboce).

   ------------------------
   -- Helper subprograms --
   ------------------------

   function Ada_Type_Name
     (Node : N_Root_Acc)
     return String;
   --  The name of the Ada type that maps Node.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   procedure Generate
     (Node : in N_Root_Acc) is
   begin
      pragma Assert (Is_Scope (Node));

      Generate_Scope (Node);
   end Generate;

   procedure Generate_Scope
     (Node : N_Root_Acc)
   is
      Stubs_Name : constant String
        := Ada_Full_Name (Node);
      Stream_Name : constant String
        := Stubs_Name & Stream_Suffix;

      Stubs_Spec : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Spec);
      Stubs_Body : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Body);

      Stream_Spec : Compilation_Unit
        := New_Package (Stream_Name, Unit_Spec);
      Stream_Body : Compilation_Unit
        := New_Package (Stream_Name, Unit_Body);

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
                  Next (It);

                  if Is_Scope (Decl_Node) then
                     Generate_Scope (Decl_Node);
                  else
                     Generate_Node_Stubs_Spec
                       (Stubs_Spec, Decl_Node);

                     --  No stubs body for a module or
                     --  repository.

                     Generate_Node_Stream_Spec
                       (Stream_Spec, Decl_Node);
                     Generate_Node_Stream_Body
                       (Stream_Body, Decl_Node);
                  end if;

               end loop;
            end;

         when K_Interface =>

            Generate_Object_Reference_Declaration
              (Stubs_Spec, Node, Full_View => False);
            --  The object reference type.

            Generate_Node_Stream_Spec
              (Stream_Spec, Node);
            Generate_Node_Stream_Body
              (Stream_Body, Node);
            --  Marshalling subprograms for the object
            --  reference type.

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
               It   : Node_Iterator;
               Export_Node : N_Root_Acc;
            begin
               Init (It, Contents (Node));
               while not Is_End (It) loop
                  Export_Node := Get_Node (It);
                  Next (It);
                  if Is_Scope (Export_Node) then
                     Generate_Scope (Export_Node);
                  else
                     Generate_Node_Stubs_Spec
                       (Stubs_Spec, Export_Node);
                     --  Generate_Node_Stubs_Body
                     --    (Stubs_Body, Export_Node);

                     Generate_Node_Stream_Spec
                       (Stream_Spec, Export_Node);
                     --  Generate_Node_Stream_Body
                     --    (Stream_Body, Export_Node);
                  end if;

               end loop;
            end;

            --  XXX Declare also inherited subprograms
            --  (for multiple inheritance.) -> Expansion ?

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
      Generate (Stream_Spec);
      Generate (Stream_Body);

   end Generate_Scope;

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
                  Next (It);

                  Generate_Node_Stubs_Spec (CU, P_Node);

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
            Generate_Node_Stubs_Spec (CU, Param_Type (Node));

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
                  Next (It);

                  Generate_Node_Stubs_Spec (CU, Decl_Node);
                  Put (CU, " : ");
                  Generate_Node_Stubs_Spec (CU, M_Type (Node));
                  Put_Line (CU, ";");

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
                  Next (It);

                  Generate_Node_Stubs_Spec (CU, E_Node);

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
                     Next (It);

                     declare
                        Bounds_It : Node_Iterator;
                        Bound_Node : N_Root_Acc;
                        First_Bound : Boolean := True;
                        Is_Array : constant Boolean
                          := not Is_Empty (Array_Bounds (Decl_Node));
                     begin

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
                           Init (Bounds_It, Array_Bounds (Decl_Node));
                           while not Is_End (Bounds_It) loop
                              Bound_Node := Get_Node (Bounds_It);
                              Next (Bounds_It);

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
                  Next (It);
                  Generate_Node_Stubs_Spec (CU, Case_Node);
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

            Generate_Node_Stubs_Spec
              (CU, N_Root_Acc (Case_Decl (Node)));
            Put (CU, " : ");
            Generate_Node_Stubs_Spec (CU, Case_Type (Node));
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
                  Next (It);
                  Generate_Node_Stubs_Spec (CU, Decl_Node);

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

   procedure Generate_Node_Stream_Spec
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc) is
   begin
      case Get_Kind (Node.all) is

         when K_Exception =>
            --  ???
            null;

         when
           K_Interface |
           K_Enum      |
           K_Union     |
           K_Struct    =>
            declare
               Type_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               New_Line (CU);
               Generate_Marshall_Profile (CU, Type_Name);
               Put_Line (CU, ";");
               Generate_Unmarshall_Profile (CU, Type_Name);
               Put_Line (CU, ";");
            end;

         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               if not Is_Interface then

                  declare
                     It   : Node_Iterator;
                     Decl_Node : N_Root_Acc;
                  begin
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Decl_Node := Get_Node (It);
                        Next (It);

                        New_Line (CU);
                        Generate_Marshall_Profile
                          (CU, Ada_Name (Decl_Node));
                        Put_Line (CU, ";");
                        Generate_Unmarshall_Profile
                          (CU, Ada_Name (Decl_Node));
                        Put_Line (CU, ";");
                     end loop;
                  end;
               end if;
            end;

         when others =>
            null;
      end case;

   end Generate_Node_Stream_Spec;

   procedure Generate_Node_Stream_Body
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc) is
   begin
      case Get_Kind (Node.all) is

         when K_Exception =>
            --  ???
            null;

         when K_Member =>
            null;

         when K_Enum =>

            declare
               E_Name : constant String
                 := Ada_Name (Node);
            begin
               Add_With (CU, "CORBA");
               Add_With (CU, "Broca.CDR", Use_It => True);

               New_Line (CU);
               Generate_Marshall_Profile (CU, E_Name);
               Put_Line (CU, " is");
               Put_Line (CU, "begin");
               Inc_Indent (CU);
               Put_Line (CU, "Marshall");
               Put_Line (CU, "  (Stream,");
               Put_Line (CU, "   CORBA.Unsigned_Long ("
                         & E_Name & "'Pos (Val)));");
               Dec_Indent (CU);
               Put_Line (CU, "end Marshall;");

               New_Line (CU);
               Generate_Unmarshall_Profile (CU, E_Name);
               Put_Line (CU, " is");
               Put_Line (CU, "begin");
               Inc_Indent (CU);
               Put_Line (CU, "return " & E_Name
                         &"'Val");
               Put_Line
                 (CU,
                  "  (CORBA.Unsigned_Long'(Unmarshall (Stream));");
               Dec_Indent (CU);
               Put_Line (CU, "end Unmarshall;");
            end;


         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               if not Is_Interface then

                  declare
                     Base_Type_Name : String
                       := Ada_Name (T_Type (Node));

                     It   : Node_Iterator;
                     Decl_Node : N_Root_Acc;
                  begin
                     --  XXX Add_With for
                     --  <Scope (Base_Type_Node)>.Stream;

                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Decl_Node := Get_Node (It);
                        Next (It);

                        declare
                           Type_Name : constant String
                             := Ada_Name (Decl_Node);
                           Array_Dimensions : constant Natural
                             := Length (Array_Bounds (Decl_Node));
                        begin
                           New_Line (CU);
                           Generate_Marshall_Profile
                             (CU, Type_Name);
                           Put_Line (CU, " is");
                           Put_Line (CU, "begin");
                           Inc_Indent (CU);
                           if Array_Dimensions = 0 then
                              Put_Line (CU, "Marshall");
                              Put_Line (CU, "  (Stream,");
                              Put_Line (CU, "   "
                                        & Base_Type_Name
                                        & " (Val));");
                           else
                              for Dimen in 1 .. Array_Dimensions loop
                                 declare
                                    DImg : constant String
                                      := Dimen'Img;
                                    D : constant String
                                      := DImg (DImg'First + 1 .. DImg'Last);
                                 begin
                                    Put_Line
                                      (CU, "for I_" & D
                                       & " in Val'Range ("
                                       & D & ") loop");
                                    Inc_Indent (CU);
                                 end;
                              end loop;

                              Put_Line (CU, "Marshall");
                              Put_Line (CU, "  (Stream,");
                              Put (CU, "   Val (");
                              for Dimen in 1 .. Array_Dimensions loop
                                 declare
                                    DImg : constant String
                                      := Dimen'Img;
                                    D : constant String
                                      := DImg (DImg'First + 1 .. DImg'Last);
                                 begin
                                    if Dimen /= 1 then
                                       Put (CU, ", ");
                                    end if;
                                    Put (CU, "I_" & D);
                                 end;
                              end loop;
                              Put_Line (CU, ");");
                              for Dimen in 1 .. Array_Dimensions loop
                                 Dec_Indent (CU);
                                 Put_Line (CU, "end loop;");
                              end loop;
                           end if;
                           Dec_Indent (CU);
                           Put_Line (CU, "end Marshall;");

                           New_Line (CU);
                           Generate_Unmarshall_Profile
                             (CU, Type_Name);
                           if Array_Dimensions = 0 then
                              Put_Line (CU, " is");
                              Put_Line (CU, "begin");
                              Inc_Indent (CU);
                              Put_Line (CU, "return " & Type_Name);
                              Put_Line (CU, "  (" & Base_Type_Name & "'");
                              Put_Line (CU, "   (Unmarshall (Stream)));");
                           else
                              New_Line (CU);
                              Put_Line (CU, "is");
                              Inc_Indent (CU);
                              Put_Line (CU, "Returns : " & Type_Name & ";");
                              Dec_Indent (CU);
                              Put_Line (CU, "begin");
                              Inc_Indent (CU);

                              --  XXX FIXME: multi-dimensional.
                              Put_Line (CU, "for I in Returns'Range loop");
                              Inc_Indent (CU);
                              Put_Line
                                (CU, "Returns (I) := Unmarshall (Stream);");
                              Dec_Indent (CU);
                              Put_Line (CU, "end loop;");
                              --  XXX END FIXME.

                              Put_Line (CU, "return Returns;");
                           end if;
                           Dec_Indent (CU);
                           Put_Line (CU, "end Unmarshall;");
                        end;
                     end loop;
                  end;
               end if;
            end;


         when K_Interface =>
            null;

         when others =>
            null;
      end case;

   end Generate_Node_Stream_Body;

   procedure Generate_Node_Default
     (CU   : in out Compilation_Unit;
      Node : N_Root_Acc) is
   begin
      case Get_Kind (Node.all) is

         when K_Scoped_Name =>

            declare
               Denoted_Entity : constant N_Named_Acc
                 := Value (Node);
            begin
               case Get_Kind (Denoted_Entity.all) is
                  when
                    K_Forward_Interface |
                    K_Interface         =>
                     Add_With
                       (CU, Ada_Name (N_Root_Acc (Denoted_Entity)));
                  when others =>
                     --  XXX FIXME Add with for scope containing
                     --  Denoted_Entity.
                     null;
               end case;

               Put (CU, Ada_Type_Name (Node));
               --  In this context, a usage occurence of a
               --  scoped name is always denoting a type.
            end;

         when K_Declarator =>
            Put (CU, Ada_Name (Node));
            --  A simple or complex (array) declarator.

         --  Base types
         when K_Simple_Type'Range =>
            Add_With (CU, "CORBA");
            Put (CU, Ada_Type_Name (Node));

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

   procedure Generate_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      Put_Line (CU, "procedure Marshall");
      Put_Line (CU, "  (Buffer : access Buffer_Type;");
      Put      (CU, "  (Val    : in " & Type_Name & ")");
   end Generate_Marshall_Profile;

   procedure Generate_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      Put_Line (CU, "function Unmarshall");
      Put_Line (CU, "  (Buffer : access Buffer_Type)");
      Put      (CU, "  return " & Type_Name);
   end Generate_Unmarshall_Profile;

   function Ada_Type_Name
     (Node : N_Root_Acc)
     return String
   is
      NK : constant Node_Kind
        := Get_Kind (Node.all);
   begin
      case NK is
         when K_Interface =>
            return Ada_Name (Node) & ".Ref";
         when
           K_Enum   |
           K_Union  |
           K_Struct =>
            return Ada_Name (Node);
         when K_Scoped_Name =>
            return Ada_Type_Name
              (N_Root_Acc (N_Named_Acc'(Value (Node))));

         when K_Short =>
            return "CORBA.Short";

         when K_Long =>
            return "CORBA.Long";

         when K_Long_Long =>
            return "CORBA.Long_Long";

         when K_Unsigned_Short =>
            return "CORBA.Unsigned_Short";

         when K_Unsigned_Long =>
            return "CORBA.Unsigned_Long";

         when K_Unsigned_Long_Long =>
            return "CORBA.Unsigned_Long_Long";

         when K_Char =>
            return "CORBA.Char";

         when K_Wide_Char =>
            return "CORBA.Wide_Char";

         when K_Boolean =>
            return "CORBA.Boolean";

         when K_Float =>
            return "CORBA.Float";

         when K_Double =>
            return "CORBA.Double";

         when K_Long_Double =>
            return "CORBA.Long_Double";

         when K_String =>
            return "CORBA.String";

         when K_Wide_String =>
            return "CORBA.Wide_String";

         when K_Octet =>
            return "CORBA.Octet";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Put_Line ("ATN: " & NK'Img);
            raise Program_Error;
      end case;
   end Ada_Type_Name;

end Ada_Be.Idl2Ada;
