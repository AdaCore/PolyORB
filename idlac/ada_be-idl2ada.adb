with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Tree.Traversal; use Idl_Fe.Tree.Traversal;

--  with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;

package body Ada_Be.Idl2Ada is

   function Is_Scope
     (Node : N_Root_Acc)
     return Boolean;
   --  True iff Node is a Scope (ie N_Repository,
   --  N_Module, N_Interface or N_ValueType).

   procedure Generate_Scope_Stubs
     (Node : N_Root_Acc);
   --  procedure Generate_Scope_Stream
   --    (Node : N_Root_Acc);
   --  procedure Generate_Scope_Skel
   --    (Node : N_Root_Acc);
   --  procedure Generate_Scope_Impl
   --    (Node : N_Root_Acc);

   procedure Generate_Node_Stubs_Spec
     (Node : N_Root_Acc;
      CU   : in out Compilation_Unit);
   --  Generate the package declaration for the
   --  stubs of a node.

   procedure Generate_Node_Default
     (Node : N_Root_Acc;
      CU   : in out Compilation_Unit);
   --  Generate the text for a node whose mapping is
   --  common to all generated files.

   function Is_Scope
     (Node : N_Root_Acc)
     return Boolean
   is
      K : constant Node_Kind
        := Get_Kind (Node.all);
   begin
      return (False
        or else K = K_Repository
        or else K = K_Module
        or else K = K_Interface
        or else K = K_ValueType);
   end Is_Scope;

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
                  Generate_Node_Stubs_Spec (Decl_Node, Stubs_Spec);
                  --  No stubs body for a module or repository.

                  if Is_Scope (Decl_Node) then
                     Generate_Scope_Stubs (Decl_Node);
                  end if;

                  Next (It);
               end loop;
            end;

         when
           K_Interface |
           K_ValueType =>

            Generate_Node_Stubs_Spec (Node, Stubs_Spec);

            declare
               IT   : Node_Iterator;
               Export_Node : N_Root_Acc;
            begin
               Init (IT, Contents (Node));
               while not Is_End (IT) loop
                  Export_Node := Get_Node (IT);
                  Generate_Node_Stubs_Spec (Export_Node, Stubs_Spec);
                  --  Generate_Node_Stubs_Body (Export_Node, Stubs_Body);

                  if Is_Scope (Export_Node) then
                     Generate_Scope_Stubs (Export_Node);
                  end if;

                  Next (IT);
               end loop;
            end;

         when others =>
            pragma Assert (False);
            --  This never happens.

            null;
      end case;

      Generate (Stubs_Spec);
      Generate (Stubs_Body);
   end Generate_Scope_Stubs;

   procedure Generate_Node_Stubs_Spec
     (Node : N_Root_Acc;
      CU   : in out Compilation_Unit) is
   begin
      case Get_Kind (Node.all) is

         when K_Repository =>
            null;

         when K_Module =>
            null;

         when K_Interface =>

               if Parents (Node) = Nil_List then
                  Add_With (CU, "CORBA");

                  Put_Line (CU,
                          "type Ref is new CORBA.Object.Ref with private;");
               else
                  declare
                     First_Parent_Name : constant String
                       := Ada_Full_Name (Head (Parents (Node)));
                  begin
                     Add_With (CU, First_Parent_Name);

                     Put_Line (CU,
                             "type Ref is new "
                             & First_Parent_Name
                             & ".Ref with Private;");
                  end;
               end if;

         when K_Forward_Interface =>
            null;
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
         when K_Operation =>
            null;
            --        when K_Attribute =>
            --  null;
         when K_Param =>
            null;
         when K_Exception =>
            null;
         when K_Member =>

            declare
               It   : Node_Iterator;
               Decl_Node : N_Root_Acc;
            begin
               Init (It, Decl (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);

                  Generate_Node_Stubs_Spec (Decl_Node, CU);
                  Put (CU, " : ");
                  Generate_Node_Stubs_Spec (M_Type (Node), CU);
                  Put_Line (CU, ";");

                  Next (It);
               end loop;
            end;


         when K_Declarator =>

            Put (CU, Ada_Name (Node));

         when K_Type_Declarator =>
            null;
         when K_Const_Dcl =>
            null;
         when K_Union =>
            null;
         when K_Case =>
            null;
         when K_Sequence =>
            null;
         when K_Struct =>
            Put_Line (CU, "type " & Ada_Name (Node) & " is record");
            Inc_Indent (CU);

            declare
               It   : Node_Iterator;
               Decl_Node : N_Root_Acc;
            begin
               Init (It, Members (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);
                  Generate_Node_Stubs_Spec (Decl_Node, CU);

                  Next (It);
               end loop;
            end;

            Dec_Indent (CU);
            Put_Line (CU, "end record;");

         when K_ValueBase =>
            null;
         when K_Enumerator =>
            null;
         when K_Native =>
            null;
         when K_Scoped_Name =>

            Put (CU, Ada_Full_Name (Node));

         when K_Object =>
            null;
         when K_Any =>
            null;
         when K_Void =>
            null;
         when K_Fixed =>
            null;



         when others =>
            Generate_Node_Default (Node, CU);
      end case;

   end Generate_Node_Stubs_Spec;

   procedure Generate_Node_Default
     (Node : N_Root_Acc;
      CU   : in out Compilation_Unit) is
   begin
      case Get_Kind (Node.all) is

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


         when K_Enum =>
            null;
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
