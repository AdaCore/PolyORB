with Ada.Text_IO; use Ada.Text_IO;

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree;  use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;
with Ada_Be.Temporaries; use Ada_Be.Temporaries;
with Ada_Be.Debug;
pragma Elaborate (Ada_Be.Debug);


with Utils; use Utils;

package body Ada_Be.Idl2Ada is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ---------------
   -- Constants --
   ---------------

   Stream_Suffix : constant String
     := ".Stream";
   Skel_Suffix : constant String
     := ".Skel";
   Impl_Suffix : constant String
     := ".Impl";

   -------------------------------------------------
   -- General purpose code generation subprograms --
   -------------------------------------------------

   procedure Gen_Scope
     (Node : Node_Id;
      Implement : Boolean);
   --  Generate all the files for scope Node.
   --  The implementation templates for interfaces is
   --  generated only if Implement is true.

   procedure Gen_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Stubs_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the stubs code for a node.

   procedure Gen_Node_Stream_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Stream_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the marshalling code for a node.

   procedure Gen_Node_Skel_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the skeleton code for a node.

   procedure Gen_Node_Impl_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Impl_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate an implementation template
   --  for a node.

   procedure Gen_Node_Default
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the text for a node whose mapping is
   --  common to all generated files.

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Gen_Object_Reference_Declaration
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the declaration of an object
   --  reference type.

   procedure Gen_Object_Servant_Declaration
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the declaration of an object
   --  implementation type.

   procedure Gen_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String);
   --  Generate the profile for the Marshall procedure
   --  of a type.
   --  FIXME: This is marshall-by-value.
   --         Marshall-by-reference should be produced
   --         as well. For details on marshalling by
   --         value vs. marshalling by reference,
   --         see the spec of Broca.Buffers.

   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String);
   --  Generate the profile for the Unmarshall function
   --  of a type.
   --  FIXME: This is unmarshall-by-value (see above).

   procedure Gen_Array_Iterator
     (CU               : in out Compilation_Unit;
      Array_Name       : String;
      Array_Dimensions : Natural;
      Stmt_Template    : String);
   --  Generate "for" loops that iterate over Array_Name,
   --  an array with Array_Dimensions dimensions, performing
   --  statement Stmt_Template on each array cell. The first
   --  occurence of the '%' character in Stmt_Template is
   --  replaced by the proper indices, with parentheses.

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
     (CU : in out Compilation_Unit;
      Object_Type : in String;
      Node : Node_Id);
   --  Generate the profile for an K_Operation node,
   --  with the Self formal parameter mode and type taken
   --  from the Object_Type string.

   procedure Gen_To_Ref
     (Stubs_Spec : in out Compilation_Unit;
      Stubs_Body : in out Compilation_Unit);
   --  Generate the declaration and implementation
   --  of the Unchecked_To_Ref and To_Ref operations
   --  of an interface.

   ------------------------
   -- Helper subprograms --
   ------------------------

   function Ada_Type_Name
     (Node : Node_Id)
     return String;
   --  The name of the Ada type that maps Node.

   procedure Add_With_Entity
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Add a semantic dependency of CU on the
   --  package that contains the mapping of
   --  the entity defined by Node.

   procedure Add_With_Stream
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Add a semantic dependency of CU on the
   --  package that contains the marshalling and
   --  unmarshalling subprograms for the type defined
   --  by Node.

   function Ada_Operation_Name
     (Node : Node_Id)
     return String;
   --  The name of the Ada subprogram that maps
   --  K_Operation Node.

   function Idl_Operation_Id
     (Node : Node_Id)
     return String;
   --  The GIOP operation identifier (to use in
   --  a GIOP Request message) corresponding
   --  to K_Operation node.

   ---------------
   -- Shortcuts --
   ---------------

   procedure NL
     (CU : in out Compilation_Unit)
     renames Ada_Be.Source_Streams.New_Line;
   procedure PL
     (CU   : in out Compilation_Unit;
      Line : String)
     renames Ada_Be.Source_Streams.Put_Line;

   procedure II
     (CU : in out Compilation_Unit)
     renames Ada_Be.Source_Streams.Inc_Indent;
   procedure DI
     (CU : in out Compilation_Unit)
     renames Ada_Be.Source_Streams.Dec_Indent;

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   procedure Generate
     (Node : in Node_Id;
      Implement : Boolean := False) is
   begin
      pragma Assert (Is_Repository (Node));

      Gen_Scope (Node, Implement);
   end Generate;

   procedure Gen_Scope
     (Node : Node_Id;
      Implement : Boolean)
   is
      Stubs_Name : constant String
        := Ada_Full_Name (Node);
      Stream_Name : constant String
        := Stubs_Name & Stream_Suffix;
      Skel_Name : constant String
        := Stubs_Name & Skel_Suffix;
      Impl_Name : constant String
        := Stubs_Name & Impl_Suffix;

      Stubs_Spec : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Spec);
      Stubs_Body : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Body);

      Stream_Spec : Compilation_Unit
        := New_Package (Stream_Name, Unit_Spec);
      Stream_Body : Compilation_Unit
        := New_Package (Stream_Name, Unit_Body);

      Skel_Spec : Compilation_Unit
        := New_Package (Skel_Name, Unit_Spec);
      Skel_Body : Compilation_Unit
        := New_Package (Skel_Name, Unit_Body);

      Impl_Spec : Compilation_Unit
        := New_Package (Impl_Name, Unit_Spec);
      Impl_Body : Compilation_Unit
        := New_Package (Impl_Name, Unit_Body);

   begin
      case Kind (Node) is
         when K_ValueType =>
            --  Not implemented yet.
            raise Program_Error;

         when
           K_Repository   |
           K_Ben_Idl_File |
           K_Module       =>

            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Contents (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);
                  Next (It);

                  if Is_Gen_Scope (Decl_Node) then
                     Gen_Scope (Decl_Node, Implement);
                  else
                     Gen_Node_Stubs_Spec
                       (Stubs_Spec, Decl_Node);
                     Gen_Node_Stubs_Body
                       (Stubs_Body, Decl_Node);
                     --  Exception declarations cause
                     --  generation of a Get_Members procedure.

                     Gen_Node_Stream_Spec
                       (Stream_Spec, Decl_Node);
                     Gen_Node_Stream_Body
                       (Stream_Body, Decl_Node);
                  end if;

               end loop;
            end;

         when K_Interface =>

            Gen_Object_Reference_Declaration
              (Stubs_Spec, Node);
            --  The object reference type.

            NL (Skel_Spec);
            PL (Skel_Spec, "pragma Elaborate_Body;");

            Add_With (Skel_Body, "Broca.Buffers");
            Add_With (Skel_Body, "Broca.Exceptions");
            Add_With (Skel_Body, "PortableServer",
                      Use_It => False,
                      Elab_Control => Elaborate_All);
            Add_With (Skel_Body, Impl_Name);

            if Implement then
               Gen_Object_Servant_Declaration
                 (Impl_Spec, Node);
               --  The object implementation type.

               Add_With (Impl_Body, Skel_Name,
                         Use_It => False,
                         Elab_Control => Elaborate);
            end if;

            Gen_Node_Stream_Spec
              (Stream_Spec, Node);
            Gen_Node_Stream_Body
              (Stream_Body, Node);
            --  Marshalling subprograms for the object
            --  reference type.

            NL (Skel_Body);
            PL (Skel_Body, "type Object_Ptr is access all "
                & Impl_Name & ".Object'Class;");
            NL (Skel_Body);
            PL (Skel_Body, "--  Skeleton subprograms");
            NL (Skel_Body);
            PL (Skel_Body, "function Servant_Is_A");
            PL (Skel_Body, "  (Obj : PortableServer.Servant)");
            PL (Skel_Body, "  return Boolean;");
            PL (Skel_Body, "procedure GIOP_Dispatch");
            PL (Skel_Body, "  (Obj : PortableServer.Servant;");
            II (Skel_Body);
            PL (Skel_Body, "Operation : String;");
            PL (Skel_Body, "Request_Id : CORBA.Unsigned_Long;");
            PL (Skel_Body, "Response_Expected : CORBA.Boolean;");
            PL (Skel_Body,
                "Request_Buffer : access Broca.Buffers.Buffer_Type;");
            PL (Skel_Body,
                "Reply_Buffer   : access Broca.Buffers.Buffer_Type);");
            DI (Skel_Body);
            NL (Skel_Body);
            PL (Skel_Body, "function Servant_Is_A");
            PL (Skel_Body, "  (Obj : PortableServer.Servant)");
            PL (Skel_Body, "  return Boolean is");
            PL (Skel_Body, "begin");
            II (Skel_Body);
            PL (Skel_Body, "return Obj.all in "
                & Impl_Name & ".Object'Class;");
            DI (Skel_Body);
            PL (Skel_Body, "end Servant_Is_A;");
            NL (Skel_Body);
            PL (Skel_Body, "procedure GIOP_Dispatch");
            PL (Skel_Body, "  (Obj : PortableServer.Servant;");
            II (Skel_Body);
            PL (Skel_Body, "Operation : String;");
            PL (Skel_Body, "Request_Id : CORBA.Unsigned_Long;");
            PL (Skel_Body, "Response_Expected : CORBA.Boolean;");
            PL (Skel_Body,
                "Request_Buffer : access Broca.Buffers.Buffer_Type;");
            PL (Skel_Body,
                "Reply_Buffer   : access Broca.Buffers.Buffer_Type) is");
            DI (Skel_Body);
            PL (Skel_Body, "begin");
            II (Skel_Body);

            declare
               It   : Node_Iterator;
               Export_Node : Node_Id;
            begin
               Init (It, Contents (Node));
               while not Is_End (It) loop
                  Export_Node := Get_Node (It);
                  Next (It);
                  if Is_Gen_Scope (Export_Node) then
                     Gen_Scope (Export_Node, Implement);
                  else
                     Gen_Node_Stubs_Spec
                       (Stubs_Spec, Export_Node);
                     Gen_Node_Stubs_Body
                       (Stubs_Body, Export_Node);

                     Gen_Node_Stream_Spec
                       (Stream_Spec, Export_Node);
                     Gen_Node_Stream_Body
                       (Stream_Body, Export_Node);

                     --  No code produced per-node
                     --  in skeleton spec.
                     Gen_Node_Skel_Body
                       (Skel_Body, Export_Node);

                     if Implement then
                        Gen_Node_Impl_Spec
                          (Impl_Spec, Export_Node);
                        Gen_Node_Impl_Body
                          (Impl_Body, Export_Node);
                     end if;
                  end if;

                  --  Methods inherited from parents other that
                  --  the first one are added to the interface's
                  --  exports list by the expander.

               end loop;
            end;

            Add_With (Stubs_Spec, "CORBA",
                      Use_It => False,
                      Elab_Control => Elaborate_All);
            NL (Stubs_Spec);
            PL (Stubs_Spec, "Repository_Id : constant CORBA.RepositoryId");
            PL (Stubs_Spec, "  := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node) & """);");
            NL (Stubs_Spec);
            PL (Stubs_Spec, "function Is_A");
            PL (Stubs_Spec, "  (Self : Ref;");
            PL (Stubs_Spec, "   Type_Id : CORBA.RepositoryId)");
            PL (Stubs_Spec, "  return CORBA.Boolean;");

            NL (Stubs_Body);
            PL (Stubs_Body, "function Is_A");
            PL (Stubs_Body, "  (Self : Ref;");
            PL (Stubs_Body, "   Type_Id : CORBA.RepositoryId)");
            PL (Stubs_Body, "  return CORBA.Boolean");
            PL (Stubs_Body, "is");
            II (Stubs_Body);
            PL (Stubs_Body, "use CORBA;");
            DI (Stubs_Body);
            PL (Stubs_Body, "begin");
            II (Stubs_Body);
            PL (Stubs_Body, "return Type_Id = Repository_Id");
            PL (Stubs_Body, "  or else Type_Id =");
            PL (Stubs_Body, "    CORBA.To_CORBA_String");
            PL (Stubs_Body, "      (""IDL:omg.org/CORBA/OBJECT:1.0"")");

            declare
               Parents : Node_List
                 := All_Ancestors (Node);
               It : Node_Iterator;
               P_Node : Node_Id;
            begin
               Init (It, Parents);
               while not Is_End (It) loop
                  P_Node := Get_Node (It);
                  Next (It);

                  Add_With (Stubs_Body, Ada_Full_Name (P_Node));
                  PL (Stubs_Body, "  or else Type_Id = "
                      & Ada_Full_Name (P_Node)
                      & ".Repository_Id");
               end loop;
               Free (Parents);
            end;

            PL (Stubs_Body, "  or else False;");
            DI (Stubs_Body);
            PL (Stubs_Body, "end Is_A;");

            Gen_To_Ref (Stubs_Spec, Stubs_Body);

            declare
               Forward_Node : constant Node_Id
                 := Forward (Node);
            begin
               if Forward_Node /= No_Node then
                  --  This interface has a forward declaration.

                  NL (Stubs_Spec);
                  PL (Stubs_Spec, "package Convert_Forward is");
                  PL (Stubs_Spec, "  new "
                      & Ada_Full_Name (Forward (Forward_Node))
                      & "_Forward.Convert (Ref_Type => Ref);");
               end if;
            end;

            NL (Skel_Body);
            PL (Skel_Body, "Broca.Exceptions.Raise_Bad_Operation;");
            DI (Skel_Body);
            PL (Skel_Body, "end GIOP_Dispatch;");
            NL (Skel_Body);
            DI (Skel_Body);
            PL (Skel_Body, "begin");
            II (Skel_Body);
            PL (Skel_Body, "PortableServer.Register_Skeleton");
            PL (Skel_Body, "  (" & Stubs_Name & ".Repository_Id,");
            PL (Skel_Body, "   Servant_Is_A'Access,");
            PL (Skel_Body, "   GIOP_Dispatch'Access);");

         when others =>
            pragma Assert (False);
            --  This never happens.

            null;
      end case;

      NL (Stubs_Spec);
      NL (Stubs_Body);
      NL (Stream_Spec);
      NL (Stream_Body);
      NL (Skel_Spec);
      --  Skel body ends with elaboration code:
      --  no new-line required.
      NL (Impl_Spec);
      NL (Impl_Body);

      Generate (Stubs_Spec);
      Generate (Stubs_Body);
      Generate (Stream_Spec);
      Generate (Stream_Body);
      Generate (Skel_Spec);
      Generate (Skel_Body);
      if Implement then
         Generate (Impl_Spec);
         Generate (Impl_Body);
      end if;
   end Gen_Scope;

   procedure Gen_Object_Reference_Declaration
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>

            NL (CU);
            if Parents (Node) = Nil_List then
               Put (CU, "type Ref is new CORBA.Object.Ref");
            else
               declare
                  First_Parent_Name : constant String
                    := Ada_Full_Name (Head (Parents (Node)));
               begin
                  Add_With (CU, First_Parent_Name);
                  Put (CU,
                       "type Ref is new "
                       & First_Parent_Name
                       & ".Ref");
               end;
            end if;

            PL (CU, " with null record;");

         when others =>
            raise Program_Error;

      end case;
   end Gen_Object_Reference_Declaration;

   procedure Gen_Object_Servant_Declaration
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>

            NL (CU);
            PL (CU, "type Object is");
            if Parents (Node) = Nil_List then
               Add_With (CU, "PortableServer");
               Put (CU, "  abstract new PortableServer.Servant_Base");
            else
               declare
                  It : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Parents (Node));

                  while not Is_End (It) loop
                     P_Node := Get_Node (It);
                     Next (It);

                     Add_With (CU, Ada_Full_Name (P_Node)
                               & Impl_Suffix,
                               Use_It => False,
                               Elab_Control => Elaborate_All);
                     --  Make it so that the skeleton unit for
                     --  an interface is elaborated after those
                     --  of all its parents.

                     if First then
                        Put (CU, "  abstract new "
                             & Ada_Full_Name (P_Node)
                             & Impl_Suffix & ".Object");
                        First := False;
                     end if;
                  end loop;
               end;
            end if;

            PL (CU, " with null record;");

            --  when K_ValueType =>...

         when others =>
            raise Program_Error;

      end case;
   end Gen_Object_Servant_Declaration;

   procedure Gen_When_Clause
     (CU   : in out Compilation_Unit;
      Node : Node_Id;
      Default_Case_Seen : in out Boolean)
   is
      It   : Node_Iterator;
      Label_Node : Node_Id;
      First_Label : Boolean := True;
      Multiple_Labels : constant Boolean
        := Length (Labels (Node)) > 1;
   begin
      pragma Assert (Kind (Node) = K_Case);

      Init (It, Labels (Node));
      while not Is_End (It) loop
         Label_Node := Get_Node (It);
         Next (It);

         if First_Label then
            Put (CU, "when ");
         end if;

         if Multiple_Labels then
            pragma Assert (Label_Node /= No_Node);
            --  The null label is the "default:"
            --  one, and must have its own case.

            if not First_Label then
               PL (CU, " |");
            else
               NL (CU);
            end if;
            Put (CU, "  ");
         end if;

         if Label_Node /= No_Node then
            Gen_Node_Stubs_Spec (CU, Label_Node);
         else
            Put (CU, "others");
            Default_Case_Seen := True;
         end if;

         First_Label := False;
      end loop;

      PL (CU, " =>");
   end Gen_When_Clause;

   procedure Gen_When_Others_Clause
     (CU : in out Compilation_Unit) is
   begin
      NL (CU);
      PL (CU, "when others =>");
      II (CU);
      PL (CU, "null;");
      DI (CU);
   end Gen_When_Others_Clause;

   procedure Gen_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         --  Scopes

         when
           K_Repository |
           K_Module     |
           K_Interface  =>
            null;

         when K_Forward_Interface =>
            Add_With (CU, "CORBA.Forward");
            NL (CU);
            PL (CU, "package " & Ada_Name (Forward (Node))
                & "_Forward is new CORBA.Forward;");

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

            Gen_Operation_Profile (CU, "in Ref", Node);
            PL (CU, ";");

            --        when K_Attribute =>
            --  null;

         when K_Exception =>

            Add_With (CU, "Ada.Exceptions");
            NL (CU);
            PL (CU, Ada_Name (Node) & " : exception;");
            NL (CU);
            PL (CU, "procedure Get_Members");
            PL (CU, "  (From : Ada.Exceptions.Exception_Occurrence;");
            PL (CU, "   To   : out "
                & Ada_Name (Members_Type (Node))
                & ");");

         when K_Member =>

            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Decl (Node));
               while not Is_End (It) loop
                  Decl_Node := Get_Node (It);
                  Next (It);

                  Gen_Node_Stubs_Spec (CU, Decl_Node);
                  Put (CU, " : ");
                  Gen_Node_Stubs_Spec (CU, M_Type (Node));
                  PL (CU, ";");

               end loop;
            end;

         when K_Enum =>

            NL (CU);
            PL (CU, "type " & Ada_Name (Node) & " is");

            declare
               First_Enumerator : Boolean := True;
               It   : Node_Iterator;
               E_Node : Node_Id;
            begin

               Init (It, Enumerators (Node));
               while not Is_End (It) loop
                  if First_Enumerator then
                     First_Enumerator := False;
                     Put (CU, "  (");
                     II (CU);
                  end if;

                  E_Node := Get_Node (It);
                  Next (It);

                  Gen_Node_Stubs_Spec (CU, E_Node);

                  if Is_End (It) then
                     PL (CU, ");");
                     DI (CU);
                  else
                     PL (CU, ",");
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
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Decl_Node := Get_Node (It);
                     Next (It);

                     declare
                        Bounds_It : Node_Iterator;
                        Bound_Node : Node_Id;
                        First_Bound : Boolean := True;
                        Is_Array : constant Boolean
                          := not Is_Empty (Array_Bounds (Decl_Node));
                     begin
                        NL (CU);
                        if Is_Interface
                          and then not Is_Array then
                           --  A typedef where the <type_spec>
                           --  denotes an interface type, and
                           --  which is not an array declaration.
                           Put (CU, "subtype ");
                        else
                           Put (CU, "type ");
                        end if;

                        Gen_Node_Stubs_Spec (CU, Decl_Node);

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
                              Gen_Node_Stubs_Spec (CU, Bound_Node);
                              Put (CU, " - 1");
                           end loop;
                           Put (CU, ") of ");
                        else
                           if not Is_Interface then
                              Put (CU, "new ");
                           end if;
                        end if;

                        Gen_Node_Stubs_Spec (CU, T_Type (Node));
                        PL (CU, ";");
                     end;
                  end loop;
               end;
            end;

         when K_Union =>
            NL (CU);
            Put (CU, "type " & Ada_Name (Node)
                      & " (Switch : ");
            Gen_Node_Stubs_Spec (CU, Switch_Type (Node));
            Put (CU, " := ");
            Gen_Node_Stubs_Spec (CU, Switch_Type (Node));
            PL (CU, "'First) is record");
            II (CU);
            PL (CU, "case Switch is");
            II (CU);

            declare
               It   : Node_Iterator;
               Case_Node : Node_Id;
               Has_Default : Boolean := False;
            begin
               Init (It, Cases (Node));
               while not Is_End (It) loop
                  Case_Node := Get_Node (It);
                  Next (It);

                  Gen_When_Clause (CU, Case_Node, Has_Default);

                  II (CU);
                  Gen_Node_Stubs_Spec
                    (CU, Case_Decl (Case_Node));
                  Put (CU, " : ");
                  Gen_Node_Stubs_Spec
                    (CU, Case_Type (Case_Node));
                  PL (CU, ";");
                  DI (CU);
               end loop;

               if not Has_Default then
                  Gen_When_Others_Clause (CU);
               end if;
            end;

            DI (CU);
            PL (CU, "end case;");
            DI (CU);
            PL (CU, "end record;");

         when K_Sequence =>
            null;

         when K_Struct =>
            NL (CU);
            Put (CU, "type " & Ada_Name (Node) & " is");
            if Is_Exception_Members (Node) then
               NL (CU);
               PL (CU, "  new CORBA.IDL_Exception_Members with");
            else
               Put (CU, " ");
            end if;

            if Is_Empty (Members (Node)) then
               PL (CU, "null record;");
            else
               PL (CU, "record");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  while not Is_End (It) loop
                     Member_Node := Get_Node (It);
                     Next (It);
                     Gen_Node_Stubs_Spec (CU, Member_Node);
                  end loop;
               end;

               DI (CU);
               PL (CU, "end record;");
            end if;

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
            --  Gen_Node_Stubs_Spec (CU, Scale (Node));
            --  Put (CU, ") digits ");
            --  Gen_Node_Stubs_Spec (CU, Digits_Nb (Node));

         when others =>
            Gen_Node_Default (CU, Node);
      end case;

   end Gen_Node_Stubs_Spec;

   procedure Gen_Node_Impl_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         --  Scopes

         when
           K_Repository |
           K_Module     |
           K_Interface  =>
            null;

         when K_Forward_Interface =>
            null; --  ??? XXX

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

            Gen_Operation_Profile (CU, "access Object", Node);
            PL (CU, ";");

            --        when K_Attribute =>
            --  null;

         when others =>
            null;

      end case;

   end Gen_Node_Impl_Spec;

   procedure Gen_Node_Impl_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         --  Scopes

         when
           K_Repository |
           K_Module     |
           K_Interface  =>
            null;

         when K_Forward_Interface =>
            null; --  ??? XXX

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

            declare
               Is_Function : constant Boolean
                 := Kind (Operation_Type (Node)) /= K_Void;
            begin
               NL (CU);
               Gen_Operation_Profile (CU, "access Object", Node);
               if Is_Function then
                  NL (CU);
                  PL (CU, "is");
                  II (CU);
                  PL (CU, "Result : "
                      & Ada_Type_Name (Operation_Type (Node)) & ";");
                  DI (CU);
               else
                  PL (CU, " is");
               end if;
               PL (CU, "begin");
               II (CU);
               NL (CU);
               PL (CU, "--  Insert implementation of " & Ada_Name (Node));
               NL (CU);
               if Is_Function then
                  PL (CU, "return Result;");
               end if;
               DI (CU);
               PL (CU, "end " & Ada_Operation_Name (Node) & ";");
            end;

            --        when K_Attribute =>
            --  null;

         when others =>
            null;

      end case;

   end Gen_Node_Impl_Body;

   procedure Gen_Node_Skel_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      if Kind (Node) /= K_Operation then
         return;
      end if;

      Add_With (CU, "Broca.CDR", Use_It => True);
      Add_With (CU, "Broca.GIOP");

      declare
         I_Node : constant Node_Id
           := Parent_Scope (Node);
         --  The Interface node that contains
         --  this operation.
         Is_Function : constant Boolean
           := Kind (Operation_Type (Node)) /= K_Void;
      begin
         pragma Debug (O ("Node is a " & Kind (Node)'Img));
         pragma Debug (O ("Its parent scope is a " & Kind (I_Node)'Img));
         pragma Assert (Kind (I_Node) = K_Interface);

         NL (CU);
         PL (CU, "if Operation = """ & Idl_Operation_Id (Node) & """ then");
         II (CU);
         PL (CU, "declare");
         II (CU);

         --  Declare local args
         declare
            It   : Node_Iterator;
            P_Node : Node_Id;
         begin

            Init (It, Parameters (Node));
            while not Is_End (It) loop
               P_Node := Get_Node (It);
               Next (It);

               PL (CU, "IDL_"
                   & Ada_Name (Declarator (P_Node))
                   & " : "
                   & Ada_Type_Name (Param_Type (P_Node)) & ";");
            end loop;
         end;

         if Is_Function then
            PL (CU, T_Returns & " : "
                & Ada_Type_Name (Operation_Type (Node))
                & ";");
         end if;

         DI (CU);
         PL (CU, "begin");
         II (CU);

         declare
            It   : Node_Iterator;
            P_Node : Node_Id;
            First : Boolean := True;
         begin
            Init (It, Parameters (Node));
            while not Is_End (It) loop
               P_Node := Get_Node (It);
               Next (It);

               if First then
                  NL (CU);
                  PL (CU, "--  Unmarshall in and inout arguments");
                  First := False;
               end if;

               case Mode (P_Node) is
                  when
                    Mode_In    |
                    Mode_Inout =>
                     Add_With_Stream (CU, Param_Type (P_Node));

                     PL (CU, "IDL_"
                         & Ada_Name (Declarator (P_Node))
                         & " := Unmarshall (Request_Buffer);");
                  when others =>
                     null;
               end case;
            end loop;
         end;

         NL (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  Call implementation");

         if Is_Function then
            Put (CU, T_Returns & " := ");
         end if;
         PL (CU, Ada_Full_Name (I_Node) & Impl_Suffix
             & "." & Ada_Name (Node));
         Put (CU, "  (Object_Ptr (Obj)");
         II (CU);
         declare
            It   : Node_Iterator;
            P_Node : Node_Id;
         begin

            Init (It, Parameters (Node));
            while not Is_End (It) loop
               P_Node := Get_Node (It);
               Next (It);

               PL (CU, ",");
               Put (CU, "IDL_"
                    & Ada_Name (Declarator (P_Node)));
            end loop;
            PL (CU, ");");
         end;
         DI (CU);

         DI (CU);

         declare
            It : Node_Iterator;
            R_Node : Node_Id;
            E_Node : Node_Id;
            First : Boolean := True;
         begin
            Init (It, Raises (Node));

            while not Is_End (It) loop
               if First then
                  PL (CU, "exception");
                  First := False;
               end if;

               R_Node := Get_Node (It);
               Next (It);
               E_Node := Value (R_Node);
               --  Each R_Node is a scoped_name
               --  that denotes an exception.

               NL (CU);
               II (CU);
               PL (CU, "when E : " & Ada_Full_Name (E_Node)
                   & " =>");
               II (CU);

               PL (CU, "declare");
               II (CU);
               PL (CU, "Repository_Id : constant CORBA.String");
               PL (CU, "  := CORBA.To_CORBA_String ("""
                   & Idl_Repository_Id (E_Node) & """);");
               PL (CU, T_Members & " : "
                   & Ada_Type_Name (Members_Type (E_Node))
                   & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, Parent_Scope_Name (E_Node)
                   & ".Get_Members (E, " & T_Members & ");");
               NL (CU);
               PL (CU, "--  Marshall service context");
               PL (CU, "Marshall");
               PL (CU, "  (Reply_Buffer,");
               PL (CU, "   CORBA.Unsigned_Long (Broca.GIOP.No_Context));");

               NL (CU);
               PL (CU, "--  Marshall request ID");
               PL (CU, "Marshall (Reply_Buffer, Request_Id);");

               NL (CU);
               PL (CU, "--  Marshall reply status");
               PL (CU, "Broca.GIOP.Marshall");
               PL (CU, "  (Reply_Buffer,");
               PL (CU, "   Broca.GIOP.User_Exception);");

               NL (CU);
               PL (CU, "--  Marshall exception");
               PL (CU, "Marshall (Reply_Buffer, Repository_Id);");
               Add_With_Stream (CU, Members_Type (E_Node));
               PL (CU, "Marshall (Reply_Buffer, " & T_Members & ");");
               PL (CU, "return;");
               DI (CU);
               PL (CU, "end;");

               DI (CU);
               DI (CU);
            end loop;
         end;

         PL (CU, "end;");

         NL (CU);
         PL (CU, "--  Marshall service context");
         PL (CU, "Marshall");
         PL (CU, "  (Reply_Buffer,");
         PL (CU, "   CORBA.Unsigned_Long (Broca.GIOP.No_Context));");

         NL (CU);
         PL (CU, "--  Marshall request ID");
         PL (CU, "Marshall (Reply_Buffer, Request_Id);");

         NL (CU);
         PL (CU, "--  Marshall reply status");
         PL (CU, "Broca.GIOP.Marshall");
         PL (CU, "  (Reply_Buffer,");
         PL (CU, "   Broca.GIOP.No_Exception);");

         if Is_Function then
            NL (CU);
            PL (CU, "--  Marshall return value");
            Add_With_Stream (CU, Operation_Type (Node));

            PL (CU, "Marshall (Reply_Buffer, " & T_Returns & ");");
         end if;

         declare
            It   : Node_Iterator;
            P_Node : Node_Id;
            First : Boolean := True;
         begin
            if First then
               NL (CU);
               PL (CU, "--  Marshall inout and out arguments");
               First := False;
            end if;

            Init (It, Parameters (Node));
            while not Is_End (It) loop
               P_Node := Get_Node (It);
               Next (It);

               case Mode (P_Node) is
                  when
                    Mode_Inout |
                    Mode_Out   =>
                     Add_With_Stream (CU, Param_Type (P_Node));

                     PL (CU, "Marshall (Reply_Buffer, IDL_"
                         & Ada_Name (Declarator (P_Node)) & ");");
                  when others =>
                     null;
               end case;
            end loop;
         end;

         PL (CU, "return;");
         DI (CU);
         PL (CU, "end;");
         DI (CU);
         PL (CU, "end if;");
      end;

   end Gen_Node_Skel_Body;

   procedure Gen_Node_Stubs_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         --  Scopes

         when
           K_Repository |
           K_Module     |
           K_Interface  =>
            null;

         when K_Forward_Interface =>
            null;

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

            declare
               O_Name : constant String
                 := Ada_Operation_Name (Node);
               O_Type : constant Node_Id
                 := Operation_Type (Node);
               Response_Expected : constant Boolean
                 := not Is_Oneway (Node);
            begin
               Add_With (CU, "CORBA",
                         Use_It    => False,
                         Elab_Control => Elaborate_All);
               Add_With (CU, "Broca.GIOP");
               Add_With (CU, "Broca.Object");

               NL (CU);
               PL (CU, O_Name
                         & "_Operation : constant CORBA.Identifier");
               PL (CU, "  := CORBA.To_CORBA_String ("""
                         & Idl_Operation_Id (Node) & """);");

               Gen_Operation_Profile (CU, "in Ref", Node);
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, T_Handler & " : Broca.GIOP.Request_Handler;");
               PL (CU, T_Send_Request_Result & " : "
                         & "Broca.GIOP.Send_Request_Result_Type;");
               if Kind (O_Type) /= K_Void then
                  Add_With_Stream (CU, O_Type);
                  PL (CU, T_Returns & " : " & Ada_Type_Name (O_Type) & ";");
               end if;
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "loop");
               II (CU);
               PL (CU, "Broca.GIOP.Send_Request_Marshall");
               PL (CU, "  (" & T_Handler & ", Broca.Object.Object_Ptr");
               PL (CU, "   (Get (Self)), "
                         & Img (Response_Expected)
                         & ", " & O_Name & "_Operation);");

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     P_Node := Get_Node (It);
                     Next (It);

                     Add_With_Stream (CU, Param_Type (P_Node));

                     case Mode (P_Node) is
                        when Mode_In | Mode_Inout =>
                           if First then
                              NL (CU);
                              PL
                                (CU, "--  Marshall in and inout arguments.");
                              First := False;
                           end if;
                           PL (CU, "Marshall");
                           PL (CU, "  ("
                               & T_Handler & ".Buffer'Access, ");
                           if Is_Interface_Type (Param_Type (P_Node)) then
                              --  The formal is class-wide: type cast it before
                              --  use.
                              PL (CU, "   "
                                  & Ada_Type_Name (Param_Type (P_Node)));
                              PL (CU, "     ("
                                  & Ada_Name (Declarator (P_Node)) & "));");
                           else
                              PL (CU, Ada_Name (Declarator (P_Node)) & ");");
                           end if;
                        when others =>
                           null;
                     end case;

                  end loop;
               end;

               NL (CU);
               PL (CU, "Broca.GIOP.Send_Request_Send");
               PL (CU, "  (" & T_Handler & ", Broca.Object.Object_Ptr");
               PL (CU, "   (Get (Self)), "
                         & Img (Response_Expected)
                         & ", " & T_Send_Request_Result & ");");
               PL (CU, "case " & T_Send_Request_Result & " is");
               II (CU);
               PL (CU, "when Broca.GIOP.Sr_Reply =>");
               II (CU);

               if Kind (O_Type) /= K_Void then
                  NL (CU);
                  PL (CU, "--  Unmarshall return value.");
                  PL (CU, T_Returns & " := Unmarshall (" & T_Handler &
                      ".Buffer'Access);");
               end if;

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     P_Node := Get_Node (It);
                     Next (It);

                     case Mode (P_Node) is
                        when Mode_Inout | Mode_Out =>
                           if First then
                              NL (CU);
                              PL
                                (CU,
                                 "--  Unmarshall inout and out parameters.");
                              First := False;
                           end if;
                           PL (CU, Ada_Name (Declarator (P_Node))
                               & ":= Unmarshall (" & T_Handler &
                               ".Buffer'Access);");
                        when others =>
                           null;
                     end case;

                  end loop;
               end;

               if Kind (O_Type) /= K_Void then
                  PL (CU, "return " & T_Returns & ";");
               else
                  PL (CU, "return;");
               end if;

               DI (CU);
               PL (CU, "when Broca.GIOP.Sr_No_Reply =>");
               II (CU);
               --  XXX ??? What's this ? FIXME.
               PL (CU, "raise Program_Error;");
               DI (CU);
               PL (CU, "when Broca.GIOP.Sr_User_Exception =>");
               II (CU);

               declare
                  It : Node_Iterator;
                  R_Node : Node_Id;
                  E_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Raises (Node));
                  while not Is_End (It) loop
                     R_Node := Get_Node (It);
                     Next (It);
                     E_Node := Value (R_Node);
                     --  Each R_Node is a scoped name
                     --  that denotes an exception.

                     if First then
                        Add_With (CU, "Broca.Exceptions");

                        PL (CU, "declare");
                        II (CU);
                        PL (CU,
                            T_Exception_Repo_Id & " : constant String");
                        PL (CU, "  := CORBA.To_Standard_String");
                        PL (CU, "  (Unmarshall (" & T_Handler &
                            ".Buffer'Access));");
                        DI (CU);
                        PL (CU, "begin");
                        II (CU);

                        First := False;
                     end if;

                     NL (CU);
                     PL (CU, "if " & T_Exception_Repo_Id);
                     PL (CU, "  = """
                         & "XXXexcRepIdXXX" & """ then");
                     II (CU);
                     PL (CU, "declare");
                     II (CU);
                     Add_With_Stream (CU, Members_Type (E_Node));
                     PL (CU, T_Members & " : constant "
                         & Ada_Type_Name (Members_Type (E_Node)));
                     PL (CU, "  := Unmarshall (" & T_Handler &
                         ".Buffer'Access);");
                     DI (CU);
                     PL (CU, "begin");
                     II (CU);
                     PL (CU, "Broca.Exceptions.User_Raise_Exception");
                     PL (CU, "  (" & Ada_Full_Name (E_Node)
                         & "'Identity,");
                     PL (CU, "   " & T_Members & ");");
                     DI (CU);
                     PL (CU, "end;");
                     DI (CU);
                     PL (CU, "end if;");
                  end loop;
               end;
               PL (CU, "raise Program_Error;");

               if not Is_Empty (Raises (Node)) then
                  DI (CU);
                  PL (CU, "end;");
               end if;

               DI (CU);
               PL (CU, "when Broca.GIOP.Sr_Forward =>");
               II (CU);
               PL (CU, "null;");
               DI (CU);
               DI (CU);
               PL (CU, "end case;");
               DI (CU);
               PL (CU, "end loop;");
               DI (CU);
               PL (CU, "end " & O_Name & ";");
            end;

         when K_Exception =>
            Add_With (CU, "Broca.Exceptions");
            NL (CU);
            PL (CU, "procedure Get_Members");
            PL (CU, "  (From : Ada.Exceptions.Exception_Occurrence;");
            PL (CU, "   To   : out "
                & Ada_Name (Members_Type (Node))
                & ") is");
            PL (CU, "begin");
            II (CU);
            PL (CU, "Broca.Exceptions.User_Get_Members (From, To);");
            DI (CU);
            PL (CU, "end Get_Members;");


         when others =>
            null;
      end case;
   end Gen_Node_Stubs_Body;

   procedure Gen_Node_Stream_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

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
               NL (CU);
               Gen_Marshall_Profile (CU, Type_Name);
               PL (CU, ";");
               Gen_Unmarshall_Profile (CU, Type_Name);
               PL (CU, ";");
            end;

         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               if not Is_Interface then

                  declare
                     It   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Decl_Node := Get_Node (It);
                        Next (It);

                        NL (CU);
                        Gen_Marshall_Profile
                          (CU, Ada_Name (Decl_Node));
                        PL (CU, ";");
                        Gen_Unmarshall_Profile
                          (CU, Ada_Name (Decl_Node));
                        PL (CU, ";");
                     end loop;
                  end;
               end if;
            end;

         when others =>
            null;
      end case;

   end Gen_Node_Stream_Spec;

   procedure Gen_Operation_Profile
     (CU : in out Compilation_Unit;
      Object_Type : in String;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Operation =>
            --  Subprogram name

            NL (CU);
            if Kind (Operation_Type (Node)) = K_Void then
               Put (CU, "procedure ");
            else
               Put (CU, "function ");
            end if;

            Put (CU, Ada_Operation_Name (Node));

            --  Formals

            NL (CU);
            Put (CU, "  (Self : " & Object_Type);
            II (CU);

            declare
               It   : Node_Iterator;
               P_Node : Node_Id;
            begin

               Init (It, Parameters (Node));
               while not Is_End (It) loop
                  P_Node := Get_Node (It);
                  Next (It);

                  PL (CU, ";");
                  Gen_Operation_Profile
                    (CU, Object_Type, P_Node);
               end loop;

               Put (CU, ")");
               DI (CU);
            end;

            --  Return type

            if Kind (Operation_Type (Node)) /= K_Void then
               NL (CU);
               Add_With_Entity (CU, Operation_Type (Node));
               Put (CU, "  return "
                    & Ada_Type_Name (Operation_Type (Node)));
            end if;

         when K_Param =>

            Gen_Operation_Profile
              (CU, Object_Type, Declarator (Node));
            case Mode (Node) is
               when Mode_In =>
                  Put (CU, " : in ");
               when Mode_Out =>
                  Put (CU, " : out ");
               when Mode_Inout =>
                  Put (CU, " : in out ");
            end case;

            declare
               T_Node : constant Node_Id
                 := Param_Type (Node);
            begin
               Add_With_Entity (CU, T_Node);
               Put (CU, Ada_Type_Name (T_Node));
               if Is_Interface_Type (T_Node) then
                  --  An operation of an interface is a
                  --  primitive operation of the tagged type
                  --  that maps this interface. If it has
                  --  other formal parameters that are object
                  --  references as well, the operation cannot
                  --  be a primitive operation of their tagged
                  --  types as well.
                  --  (Ada RTF issue #2459).

                  Put (CU, "'Class");
               end if;
            end;

         when others =>
            Gen_Node_Default (CU, Node);

      end case;
   end Gen_Operation_Profile;

   procedure Gen_Array_Iterator
     (CU               : in out Compilation_Unit;
      Array_Name       : String;
      Array_Dimensions : Natural;
      Stmt_Template    : String)
   is
      Indices_Pos : Natural := Stmt_Template'Last + 1;
      Prefix_End, Suffix_Start : Natural;

      function Identifier (Dimension : Positive) return String;
      --  Return an identifier that depends on the dimension and on the
      --  total dimension. If there is only one dimension, "I" will be
      --  used, "I1", "I2", ..., "In" otherwise.

      ----------------
      -- Identifier --
      ----------------

      function Identifier (Dimension : Positive) return String is
      begin
         pragma Assert (Dimension <= Array_Dimensions);
         if Array_Dimensions = 1 then
            return "I";
         else
            return "I" & Img (Dimension);
         end if;
      end Identifier;

   begin
      for I in Stmt_Template'Range loop
         if Stmt_Template (I) = '%' then
            Indices_Pos := I;
            exit;
         end if;
      end loop;

      pragma Assert (Indices_Pos in Stmt_Template'Range);

      Prefix_End := Indices_Pos - 1;
      while Prefix_End >= Stmt_Template'First and then
        Stmt_Template (Prefix_End) = ' ' loop
         Prefix_End := Prefix_End - 1;
      end loop;

      Suffix_Start := Indices_Pos + 1;

      declare
         Stmt_Prefix : constant String
           := Stmt_Template
           (Stmt_Template'First .. Prefix_End);
         Stmt_Suffix : constant String
           := Stmt_Template
           (Suffix_Start .. Stmt_Template'Last);
      begin
         for Dimen in 1 .. Array_Dimensions loop
            PL
              (CU, "for " & Identifier (Dimen)
               & " in " & Array_Name & "'Range ("
               & Img (Dimen) & ") loop");
            II (CU);
         end loop;

         PL (CU, Stmt_Prefix);
         Put (CU, "  (");
         for Dimen in 1 .. Array_Dimensions loop
            if Dimen /= 1 then
               Put (CU, ", ");
            end if;
            Put (CU, Identifier (Dimen));
         end loop;
         PL (CU, ")" & Stmt_Suffix);
         for Dimen in 1 .. Array_Dimensions loop
            DI (CU);
            PL (CU, "end loop;");
         end loop;
      end;
   end Gen_Array_Iterator;

   procedure Gen_Node_Stream_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Exception =>
            --  ???
            null;

         when K_Struct =>

            declare
               S_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, S_Name);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  while not Is_End (It) loop
                     Member_Node := Get_Node (It);
                     Next (It);

                     Add_With_Stream (CU, M_Type (Member_Node));

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Decl_Node := Get_Node (DIt);
                           Next (DIt);

                           PL (CU, "Marshall (Buffer, Val."
                                     & Ada_Name (Decl_Node)
                                     & ");");
                        end loop;
                     end;
                  end loop;
               end;

               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, S_Name);
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, T_Returns & " : " & S_Name & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  while not Is_End (It) loop
                     Member_Node := Get_Node (It);
                     Next (It);

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Decl_Node := Get_Node (DIt);
                           Next (DIt);

                           PL (CU, T_Returns & "."
                                     & Ada_Name (Decl_Node)
                                     & " := Unmarshall (Buffer);");
                        end loop;
                     end;

                  end loop;
               end;
               PL (CU, "return " & T_Returns & ";");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Union =>

            declare
               U_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, U_Name);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);

               Add_With_Stream (CU, Switch_Type (Node));
               PL (CU, "Marshall (Buffer, Val.Switch);");
               PL (CU, "case Val.Switch is");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Case_Node : Node_Id;
                  Has_Default : Boolean := False;
               begin
                  Init (It, Cases (Node));
                  while not Is_End (It) loop
                     Case_Node := Get_Node (It);
                     Next (It);

                     NL (CU);
                     Gen_When_Clause (CU, Case_Node, Has_Default);
                     II (CU);
                     Add_With_Stream (CU, Case_Type (Case_Node));
                     PL (CU, "Marshall (Buffer, Val."
                               & Ada_Name
                               (Case_Decl (Case_Node))
                               & ");");
                     DI (CU);
                  end loop;

                  if not Has_Default then
                     Gen_When_Others_Clause (CU);
                  end if;
               end;

               DI (CU);
               PL (CU, "end case;");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, U_Name);
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, "Switch : "
                         & Ada_Type_Name (Switch_Type (Node))
                         & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "Switch := Unmarshall (Buffer);");
               NL (CU);
               PL (CU, "declare");
               II (CU);
               PL (CU, T_Returns & " : " & U_Name & " (Switch);");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "case Switch is");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Case_Node : Node_Id;
                  Has_Default : Boolean := False;
               begin
                  Init (It, Cases (Node));
                  while not Is_End (It) loop
                     Case_Node := Get_Node (It);
                     Next (It);

                     NL (CU);
                     Gen_When_Clause (CU, Case_Node, Has_Default);
                     II (CU);
                     PL (CU, T_Returns & "."
                         & Ada_Name (Case_Decl (Case_Node))
                         & " := Unmarshall (Buffer);");
                     DI (CU);

                  end loop;
                  if not Has_Default then
                     Gen_When_Others_Clause (CU);
                  end if;
               end;

               DI (CU);
               PL (CU, "end case;");
               NL (CU);
               PL (CU, "return " & T_Returns & ";");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Enum =>

            declare
               E_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               Add_With (CU, "CORBA");
               Add_With (CU, "Broca.CDR", Use_It => True);

               NL (CU);
               Gen_Marshall_Profile (CU, E_Name);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "Marshall");
               PL (CU, "  (Buffer,");
               PL (CU, "   CORBA.Unsigned_Long ("
                         & E_Name & "'Pos (Val)));");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, E_Name);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "return " & E_Name
                         &"'Val");
               PL
                 (CU,
                  "  (CORBA.Unsigned_Long'(Unmarshall (Buffer)));");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               if not Is_Interface then

                  declare
                     Base_Type_Name : String
                       := Ada_Type_Name (T_Type (Node));

                     It   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Add_With_Stream (CU, T_Type (Node));
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Decl_Node := Get_Node (It);
                        Next (It);

                        declare
                           Type_Name : constant String
                             := Ada_Type_Name (Decl_Node);
                           Array_Dimensions : constant Natural
                             := Length (Array_Bounds (Decl_Node));
                        begin
                           NL (CU);
                           Gen_Marshall_Profile
                             (CU, Type_Name);
                           PL (CU, " is");
                           PL (CU, "begin");
                           II (CU);
                           if Array_Dimensions = 0 then
                              PL (CU, "Marshall");
                              PL (CU, "  (Buffer,");
                              PL (CU, "   "
                                        & Base_Type_Name
                                        & " (Val));");
                           else
                              Gen_Array_Iterator
                                (CU, "Val", Array_Dimensions,
                                 "Marshall (Buffer, Val %);");
                           end if;
                           DI (CU);
                           PL (CU, "end Marshall;");

                           NL (CU);
                           Gen_Unmarshall_Profile
                             (CU, Type_Name);
                           if Array_Dimensions = 0 then
                              PL (CU, " is");
                              PL (CU, "begin");
                              II (CU);
                              PL (CU, "return " & Type_Name);
                              PL (CU, "  (" & Base_Type_Name & "'");
                              PL (CU, "   (Unmarshall (Buffer)));");
                           else
                              NL (CU);
                              PL (CU, "is");
                              II (CU);
                              PL (CU, T_Returns & " : " & Type_Name & ";");
                              DI (CU);
                              PL (CU, "begin");
                              II (CU);

                              Gen_Array_Iterator
                                (CU, T_Returns, Array_Dimensions,
                                 T_Returns & " % := Unmarshall (Buffer);");

                              PL (CU, "return " & T_Returns & ";");
                           end if;

                           DI (CU);
                           PL (CU, "end Unmarshall;");
                        end;
                     end loop;
                  end;
               end if;
            end;

         when K_Interface =>
            declare
               I_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, I_Name);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "Marshall_Reference (Buffer, Val);");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, I_Name);
               PL (CU, " is");
               II (CU);
               PL (CU, "New_Ref : Ref;");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "Unmarshall_Reference (Buffer, New_Ref);");
               PL (CU, "return New_Ref;");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when others =>
            null;
      end case;

   end Gen_Node_Stream_Body;

   procedure Gen_Node_Default
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Scoped_Name =>

            declare
               Denoted_Entity : constant Node_Id
                 := Value (Node);
            begin
               case Kind (Denoted_Entity) is
                  when
                    K_Forward_Interface |
                    K_Interface         =>
                     Add_With
                       (CU, Ada_Full_Name (Denoted_Entity));
                     Put (CU, Ada_Type_Name (Node));

                  when K_Enumerator =>
                     Put (CU, Ada_Full_Name (Node));

                  when others =>
                     Add_With_Entity (CU, Node);
                     Put (CU, Ada_Type_Name (Node));

               end case;
            end;

         when K_Declarator =>
            Put (CU, Ada_Name (Node));
            --  A simple or complex (array) declarator.

         --  Base types
         when
           K_Float              |
           K_Double             |
           K_Long_Double        |
           K_Short              |
           K_Long               |
           K_Long_Long          |
           K_Unsigned_Short     |
           K_Unsigned_Long      |
           K_Unsigned_Long_Long |
           K_Char               |
           K_Wide_Char          |
           K_Boolean            |
           K_String             |
           K_Wide_String        |
           K_Octet              =>
            Add_With (CU, "CORBA");
            Put (CU, Ada_Type_Name (Node));

         when K_Enumerator =>
            Put (CU, Ada_Name (Node));

         when K_Attribute =>
            null;
            --  Attributes are expanded into operations.

         when K_Or_Expr =>                   --  Binary operators.
            null;

         when K_Xor_Expr =>
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

         when K_Lit_String =>
            Put (CU, String_Value (Node).all);

         when K_Lit_Boolean =>
            Put (CU, Img (Bool_Value (Node)));

         when K_Primary_Expr =>
            Gen_Node_Default (CU, Operand (Node));

         when others =>
            Ada.Text_IO.Put_Line
              ("Error: Don't know what to do with a "
               & Kind (Node)'Img & " node.");
            raise Program_Error;

      end case;
   end Gen_Node_Default;

   procedure Gen_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "procedure Marshall");
      PL (CU, "  (Buffer : access Buffer_Type;");
      Put (CU, "   Val    : in " & Type_Name & ")");
   end Gen_Marshall_Profile;

   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Name : in String) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "function Unmarshall");
      PL (CU, "  (Buffer : access Buffer_Type)");
      Put      (CU, "  return " & Type_Name);
   end Gen_Unmarshall_Profile;

   function Ada_Type_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when K_Interface =>
            return Ada_Full_Name (Node) & ".Ref";

         when
           K_Enum   |
           K_Union  |
           K_Struct =>
            return Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_Type_Name (Value (Node));

         when K_Declarator =>
            --  A type created by a typedef.
            return Ada_Full_Name (Node);

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

            Ada.Text_IO.Put_Line ("Error: A "
                                  & NK'Img
                                  & " does not denote a type.");
            raise Program_Error;
      end case;
   end Ada_Type_Name;

   procedure Add_With_Entity
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when K_Interface =>
            Add_With (CU, Ada_Full_Name (Node));

         when
           K_Enum   |
           K_Union  |
           K_Struct |
           K_Declarator =>
            Add_With (CU, Ada_Full_Name (Parent_Scope (Node)));

         when K_Scoped_Name =>
            Add_With_Entity (CU, Value (Node));

         when
           K_Short              |
           K_Long               |
           K_Long_Long          |
           K_Unsigned_Short     |
           K_Unsigned_Long      |
           K_Unsigned_Long_Long |
           K_Char               |
           K_Wide_Char          |
           K_Boolean            |
           K_Float              |
           K_Double             |
           K_Long_Double        |
           K_String             |
           K_Wide_String        |
           K_Octet              =>
            Add_With (CU, "CORBA");

         when others =>
            Ada.Text_IO.Put_Line ("Error: A "
                                  & NK'Img
                                  & " is not a mapped entity.");
            raise Program_Error;
      end case;
   end Add_With_Entity;

   procedure Add_With_Stream
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when K_Interface =>
            Add_With (CU, Ada_Full_Name (Node) & Stream_Suffix,
                      Use_It => True);

         when
           K_Enum   |
           K_Union  |
           K_Struct |
           K_Declarator =>
            Add_With (CU, Ada_Full_Name (Parent_Scope (Node))
                      & Stream_Suffix,
                      Use_It => True);

         when K_Scoped_Name =>
            Add_With_Stream (CU, Value (Node));

         when
           K_Short              |
           K_Long               |
           K_Long_Long          |
           K_Unsigned_Short     |
           K_Unsigned_Long      |
           K_Unsigned_Long_Long |
           K_Char               |
           K_Wide_Char          |
           K_Boolean            |
           K_Float              |
           K_Double             |
           K_Long_Double        |
           K_String             |
           K_Wide_String        |
           K_Octet              =>
            Add_With (CU, "Broca.CDR",
                      Use_It => True);

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Ada.Text_IO.Put_Line ("Error: A "
                                  & NK'Img
                                  & " does not denote a type.");
            raise Program_Error;
      end case;
   end Add_With_Stream;

   ---------------------------------------------------------
   -- Ada_Operation_Name and Idl_Operation_Id differ      --
   -- for operations that are created by the expander and --
   -- represent attributes:                               --
   -- given an attribute Foo of an interface, the "get"   --
   -- and "set" operations will be generated with         --
   -- Ada_Operation_Names "get_Foo" and "set_Foo", and    --
   -- Idl_Operation_Ids "_get_Foo" and "_set_Foo".        --
   ---------------------------------------------------------

   function Ada_Operation_Name
     (Node : Node_Id)
     return String is
   begin
      pragma Assert (Kind (Node) = K_Operation);
      return Ada_Name (Node);
   end Ada_Operation_Name;

   function Idl_Operation_Id
     (Node : Node_Id)
     return String is
   begin
      pragma Assert (Kind (Node) = K_Operation);
      return Name (Node);
   end Idl_Operation_Id;

   procedure Gen_To_Ref
     (Stubs_Spec : in out Compilation_Unit;
      Stubs_Body : in out Compilation_Unit) is
   begin
      Add_With (Stubs_Spec, "CORBA.Object");
      NL (Stubs_Spec);
      PL (Stubs_Spec, "function Unchecked_To_Ref");
      PL (Stubs_Spec, "  (The_Ref : in CORBA.Object.Ref'Class)");
      PL (Stubs_Spec, "  return Ref;");
      PL (Stubs_Spec, "function To_Ref");
      PL (Stubs_Spec, "  (The_Ref : in CORBA.Object.Ref'Class)");
      PL (Stubs_Spec, "  return Ref;");

      Add_With (Stubs_Body, "Broca.Refs");
      Add_With (Stubs_Body, "Broca.Exceptions");

      NL (Stubs_Body);
      PL (Stubs_Body, "function Unchecked_To_Ref");
      PL (Stubs_Body, "  (The_Ref : in CORBA.Object.Ref'Class)");
      PL (Stubs_Body, "  return Ref");
      PL (Stubs_Body, "is");
      II (Stubs_Body);
      PL (Stubs_Body, "Result : Ref;");
      DI (Stubs_Body);
      PL (Stubs_Body, "begin");
      II (Stubs_Body);
      PL (Stubs_Body, "Broca.Refs.Set");
      PL (Stubs_Body, "  (Broca.Refs.Ref (Result),");
      PL (Stubs_Body,
          "   Broca.Refs.Get (Broca.Refs.Ref (The_Ref)));");
      PL (Stubs_Body, "return Result;");
      DI (Stubs_Body);
      PL (Stubs_Body, "end Unchecked_To_Ref;");
      NL (Stubs_Body);
      PL (Stubs_Body, "function To_Ref");
      PL (Stubs_Body, "  (The_Ref : in CORBA.Object.Ref'Class)");
      PL (Stubs_Body, "  return Ref");
      PL (Stubs_Body, "is");
      II (Stubs_Body);
      PL (Stubs_Body, "Result : Ref;");
      DI (Stubs_Body);
      PL (Stubs_Body, "begin");
      II (Stubs_Body);
      PL (Stubs_Body, "Result := Unchecked_To_Ref (The_Ref);");
      PL (Stubs_Body, "if Is_A (Result, Repository_Id) then");
      II (Stubs_Body);
      PL (Stubs_Body, "return Result;");
      DI (Stubs_Body);
      PL (Stubs_Body, "else");
      II (Stubs_Body);
      PL (Stubs_Body, "Broca.Exceptions.Raise_Bad_Param;");
      DI (Stubs_Body);
      PL (Stubs_Body, "end if;");
      DI (Stubs_Body);
      PL (Stubs_Body, "end To_Ref;");
   end Gen_To_Ref;

end Ada_Be.Idl2Ada;
