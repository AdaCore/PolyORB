-----------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Characters.Handling;

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;
with Ada_Be.Debug;

with Errors;                use Errors;
with Utils;                 use Utils;

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
   Value_Impl_Suffix : constant String
     := ".Value_Impl";
   Helper_Suffix : constant String
     := ".Helper";

   -------------------------------------------------
   -- General purpose code generation subprograms --
   -------------------------------------------------

   procedure Gen_Scope
     (Node : Node_Id;
      Implement : Boolean;
      To_Stdout : Boolean);
   --  Generate all the files for scope Node.
   --  The implementation templates for interfaces is
   --  generated only if Implement is true.

   procedure Gen_Value_Scope
     (Node : Node_Id;
      Implement : Boolean;
      To_Stdout : Boolean);

   procedure Gen_Interface_Module_Scope
     (Node : Node_Id;
      Implement : Boolean;
      To_Stdout : Boolean);

   procedure Gen_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Stubs_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the stubs code for a node.

   procedure Gen_Operation_Stubs_Body_For_ValueTypes
     (CU : in out Compilation_Unit;
      Node : in Node_Id);

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

   procedure Gen_Value_Impl_Decl
     (CU   : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate the declaration of a valuetype object
   --  in the Value_Impl package

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
      Type_Node : in Node_Id);
   --  Generate the profile for the Marshall procedure
   --  of a type.
   --  FIXME: This is marshall-by-value.
   --         Marshall-by-reference should be produced
   --         as well. For details on marshalling by
   --         value vs. marshalling by reference,
   --         see the spec of Broca.Buffers.

   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id);
   --  Generate the profile for the Unmarshall function
   --  of a type.
   --  FIXME: This is unmarshall-by-value (see above).

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the From_Any function of a type

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the To_Any function of a type

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

   procedure Gen_Helper
     (Node : in Node_Id;
      Helper_Spec : in out Compilation_Unit;
      Helper_Body : in out Compilation_Unit);
   --  generates the To_Ref functions for the node

   procedure Gen_Delegate
     (Node : in Node_Id;
      Delegate_Spec : in out Compilation_Unit;
      Delegate_Body : in out Compilation_Unit);
   --  generates the .delegate package

   ------------------------
   -- Helper subprograms --
   ------------------------

   function Ada_Type_Name (Node : Node_Id)
                           return String;
   --  The name of the Ada type that maps Node.

   function Ada_TC_Name (Node : Node_Id)
                         return String;
   --  The name of the typecode corresponding to an Ada type

   function Ada_Type_Defining_Name
     (Node : Node_Id)
     return String;
   --  The defining name of the Ada type that maps Node
   --  (a K_Interface or K_ValueType).

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

   procedure Gen_Constant_Value
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the representation of a constant expression.

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
      Implement : Boolean := False;
      To_Stdout : Boolean := False)
   is
      S_Node : Node_Id;
      It : Node_Iterator;
   begin
      pragma Assert (Is_Repository (Node));

      Init (It, Contents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, S_Node);
         Gen_Scope (S_Node, Implement, To_Stdout);
      end loop;

   end Generate;

   ----------------
   --  Gen_Scope --
   ----------------
   procedure Gen_Scope
     (Node : Node_Id;
      Implement : Boolean;
      To_Stdout : Boolean) is
   begin
      case Kind (Node) is
         when K_ValueType =>
            Gen_Value_Scope (Node, Implement, To_Stdout);
         when
           K_Ben_Idl_File |
           K_Module |
           K_Interface =>
            Gen_Interface_Module_Scope (Node, Implement, To_Stdout);
         when others =>
            raise Program_Error;
            --  should never happen
      end case;
   end Gen_Scope;

   ----------------------
   --  Gen_Value_Scope --
   ----------------------

   procedure Gen_Value_Scope (Node : Node_Id;
                              Implement : Boolean;
                              To_Stdout : Boolean) is
      Stubs_Name : constant String
        := Ada_Full_Name (Node);
      Stream_Name : constant String
        := Stubs_Name & Stream_Suffix;
      Value_Impl_Name : constant String
        := Stubs_Name & Value_Impl_Suffix;
      Helper_Name : constant String
        := Stubs_Name & Helper_Suffix;

      Stubs_Spec : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Spec);
      Stubs_Body : Compilation_Unit
        := New_Package (Stubs_Name, Unit_Body);

      Stream_Spec : Compilation_Unit
        := New_Package (Stream_Name, Unit_Spec);
      Stream_Body : Compilation_Unit
        := New_Package (Stream_Name, Unit_Body);

      Value_Impl_Spec : Compilation_Unit
        := New_Package (Value_Impl_Name, Unit_Spec);
      Value_Impl_Body : Compilation_Unit
        := New_Package (Value_Impl_Name, Unit_Body);

      Helper_Spec : Compilation_Unit
        := New_Package (Helper_Name, Unit_Spec);
      Helper_Body : Compilation_Unit
        := New_Package (Helper_Name, Unit_Body);

   begin
      --  the valuetype type
      Gen_Object_Reference_Declaration (Stubs_Spec, Node);
      PL (Stubs_Spec, "Null_Value : constant Value_Ref;");

      --  Marshalling subprograms for the object
      --  reference type.
      Gen_Node_Stream_Spec (Stream_Spec, Node);
      Gen_Node_Stream_Body (Stream_Body, Node);

      --  Value_Impl type
      Gen_Value_Impl_Decl (Value_Impl_Spec, Node);

      --  Helper package

      --  loop on node content
      declare
         It   : Node_Iterator;
         Export_Node : Node_Id;
      begin
         Init (It, Contents (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, Export_Node);
            if Is_Gen_Scope (Export_Node) then
               Gen_Scope (Export_Node, Implement, To_Stdout);
            else
               Gen_Node_Stubs_Spec (Stubs_Spec, Export_Node);
               if Kind (Export_Node) = K_Operation then
                  Gen_Operation_Stubs_Body_For_ValueTypes (Stubs_Body,
                                                           Export_Node);
               end if;

               Gen_Node_Stream_Spec (Stream_Spec, Export_Node);
               Gen_Node_Stream_Body (Stream_Body, Export_Node);

               Gen_Node_Impl_Spec (Value_Impl_Spec, Export_Node);

               Gen_Helper (Export_Node, Helper_Spec, Helper_Body);
            end if;

            --  Methods inherited from parents other that
            --  the first one are added to the interface's
            --  exports list by the expander.
         end loop;
      end;


      NL (Stubs_Spec);
      NL (Stubs_Body);
      NL (Stream_Spec);
      NL (Stream_Body);
      NL (Value_Impl_Spec);
      NL (Value_Impl_Body);

      if Implement then
         Generate (Value_Impl_Body, False, To_Stdout);
      else
         Generate (Value_Impl_Spec, False, To_Stdout);
         Generate (Stubs_Spec, False, To_Stdout);
         Generate (Stubs_Body, False, To_Stdout);
         Generate (Stream_Spec, False, To_Stdout);
         Generate (Stream_Body, False, To_Stdout);
      end if;

   end Gen_Value_Scope;

   ---------------------------------------------
   -- Gen_Operation_Stubs_Body_For_ValueTypes --
   ---------------------------------------------

   procedure Gen_Operation_Stubs_Body_For_ValueTypes
     (CU : in out Compilation_Unit;
      Node : in Node_Id) is
      Op_Name : constant String
        := Ada_Operation_Name (Node);
      Is_Function : constant Boolean
        := Kind (Operation_Type (Node)) /= K_Void;
   begin
      pragma Assert (Kind (Node) = K_Operation);
      if not Is_Implicit_Inherited (Node) then
         Gen_Operation_Profile
           (CU,
            Ada_Type_Defining_Name
            (Parent_Scope (Node)),
            Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);

         PL (CU, "if Is_Nil (Self) then");
         II (CU);
         PL (CU, "Broca.Exceptions.Raise_Inv_Objref;");
         DI (CU);
         PL (CU, "end if;");

         if Is_Function then
            PL (CU, "return");
            II (CU);
         end if;

         PL (CU, Ada_Full_Name (Parent_Scope (Node))
              & ".Value_Impl."
              & Op_Name);
         II (CU);

         --  The controlling formal parameter

         Put (CU, "(Self.Ptr.all");

         --  The remaining formals

         declare
            It : Node_Iterator;
            Param_Node : Node_Id;
         begin
            Init (It, Parameters (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Param_Node);

               PL (CU, ";");
               Put (CU, " " & Ada_Name (Declarator (Param_Node)));
            end loop;
         end;

         PL (CU, ");");

         DI (CU);
         if  Is_Function then
            DI (CU);
         end if;

         DI (CU);
         PL (CU, "end " & Op_Name);
      end if;

   end Gen_Operation_Stubs_Body_For_ValueTypes;


   --------------------------
   --  Gen_Value_Impl_Decl --
   --------------------------

   --  generate the type declaration for the Value_Impl
   --  package of a ValueType
   procedure Gen_Value_Impl_Decl (CU : in out Compilation_Unit;
                                  Node : in Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_ValueType);

      Put (CU, "type "
           & Ada_Type_Defining_Name (Node)
           & " is new ");

      --  check parent
      if Parents (Node) = Nil_List then
         Put (CU, "CORBA.Value.Impl_Base");
      else
         declare
            First_Parent : Node_Id := Head (Parents (Node));
         begin
            Add_With (CU, Ada_Full_Name (First_Parent));
            Put (CU, Ada_Type_Name (First_Parent));
         end;
      end if;

      --  write members
      if Is_Empty (Contents (Node)) then
         PL (CU, " with null record;");
      else
         PL (CU, " with record");
         II (CU);
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               if Kind (Member_Node) = K_State_Member then
                  declare
                     Decl_Iterator : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (Decl_Iterator, State_Declarators (Member_Node));
                     while not Is_End (Decl_Iterator) loop
                        Get_Next_Node (Decl_Iterator, Decl_Node);
                        if Kind (Decl_Node) = K_Declarator then
                           Gen_Node_Stubs_Spec (CU, Decl_Node);
                           Put (CU, " : ");
                           Gen_Node_Stubs_Spec (CU, State_Type (Member_Node));
                           PL (CU, ";");
                        end if;
                     end loop;
                  end;
               end if;
            end loop;
         end;

         DI (CU);
         PL (CU, "end record;");
         PL (CU, "type Object_Ptr is access all Object'Class;");
      end if;

   end Gen_Value_Impl_Decl;

   ---------------------------------
   --  Gen_Interface_Module_Scope --
   ---------------------------------

   procedure Gen_Interface_Module_Scope
     (Node : Node_Id;
      Implement : Boolean;
      To_Stdout : Boolean)
   is
      Stubs_Name : constant String
        := Ada_Full_Name (Node);
      Stream_Name : constant String
        := Stubs_Name & Stream_Suffix;
      Skel_Name : constant String
        := Stubs_Name & Skel_Suffix;
      Impl_Name : constant String
        := Stubs_Name & Impl_Suffix;
      Helper_Name : constant String
        := Stubs_Name & Helper_Suffix;

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

      Helper_Spec : Compilation_Unit
        := New_Package (Helper_Name, Unit_Spec);
      Helper_Body : Compilation_Unit
        := New_Package (Helper_Name, Unit_Body);


   begin
      case Kind (Node) is
         when K_ValueType =>
            raise Program_Error;
            --  should not be called, generated in Gen_Value_Scope
         when
           K_Ben_Idl_File |
           K_Module       =>

            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Contents (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Decl_Node);

                  if Is_Gen_Scope (Decl_Node) then
                     --  Ensure current unit has a non-empty
                     --  spec, if it has child packages.
                     if Kind (Node) /= K_Repository then
                        NL (Stubs_Spec);
                        Put (Stubs_Spec, "--  ");
                        case Kind (Decl_Node) is
                           when K_Module =>
                              Put (Stubs_Spec, "Module ");
                           when K_Interface =>
                              Put (Stubs_Spec, "Interface ");
                           when K_ValueType =>
                              Put (Stubs_Spec, "ValueType ");
                           when others =>
                              --  Does not happen.
                              raise Program_Error;
                        end case;
                        PL (Stubs_Spec, Name (Decl_Node)
                            & " is defined in a child unit.");

                     end if;
                     Gen_Scope (Decl_Node, Implement, To_Stdout);
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

                     Gen_Helper (Decl_Node, Helper_Spec, Helper_Body);
                  end if;

               end loop;
            end;

         when K_Interface =>

            Gen_Object_Reference_Declaration (Stubs_Spec, Node);
            --  The object reference type.

            Add_Elaborate_Body (Skel_Spec);

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

               Suppress_Warning_Message (Impl_Spec);
               Suppress_Warning_Message (Impl_Body);

               Add_With (Impl_Body, Skel_Name,
                         Use_It => False,
                         Elab_Control => None);
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
                  Get_Next_Node (It, Export_Node);
                  if Is_Gen_Scope (Export_Node) then
                     Gen_Scope (Export_Node, Implement, To_Stdout);
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

                     Gen_Helper (Export_Node, Helper_Spec, Helper_Body);
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
            PL (Stubs_Spec, T_Repository_Id
                & " : constant CORBA.RepositoryId");
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
            PL (Stubs_Body, "return Type_Id = " & T_Repository_Id);
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
                  Get_Next_Node (It, P_Node);

                  Add_With (Stubs_Body, Ada_Full_Name (P_Node));
                  PL (Stubs_Body, "  or else Type_Id = "
                      & Ada_Full_Name (P_Node)
                      & "." & T_Repository_Id);
               end loop;
               Free (Parents);
            end;

            PL (Stubs_Body, "  or else False;");
            DI (Stubs_Body);
            PL (Stubs_Body, "end Is_A;");

            --  CORBA 2.3
            Gen_Helper (Node, Helper_Spec, Helper_Body);

            declare
               Forward_Node : constant Node_Id
                 := Forward (Node);
            begin
               if Forward_Node /= No_Node then
                  --  This interface has a forward declaration.

                  NL (Stubs_Spec);
                  PL (Stubs_Spec, "package Convert_Forward is");
                  PL (Stubs_Spec, "  new "
                      & Ada_Full_Name (Forward_Node)
                      & ".Convert (Ref_Type => Ref);");
               end if;
            end;

            NL (Skel_Body);
            PL (Skel_Body, "Broca.Exceptions.Raise_Bad_Operation;");
            DI (Skel_Body);
            PL (Skel_Body, "end GIOP_Dispatch;");

            Divert (Skel_Body, Elaboration);

            PL (Skel_Body, "PortableServer.Register_Skeleton");
            PL (Skel_Body, "  (" & Stubs_Name
                & "." & T_Repository_Id &",");
            PL (Skel_Body, "   Servant_Is_A'Access,");
            PL (Skel_Body, "   GIOP_Dispatch'Access);");

         when others =>
            pragma Assert (False);
            --  This never happens.

            null;
      end case;

      if Implement then
         Generate (Impl_Spec, False, To_Stdout);
         Generate (Impl_Body, False, To_Stdout);
      else
         Generate (Stubs_Spec, False, To_Stdout);
         Generate (Stubs_Body, False, To_Stdout);
         Generate (Helper_Spec, False, To_Stdout);
         Generate (Helper_Body, False, To_Stdout);
         Generate (Stream_Spec, False, To_Stdout);
         Generate (Stream_Body, False, To_Stdout);
         Generate (Skel_Spec, False, To_Stdout);
         Generate (Skel_Body, False, To_Stdout);
      end if;
   end Gen_Interface_Module_Scope;

   ---------------------------------------
   --  Gen_Object_Reference_Declaration --
   ---------------------------------------

   --  Generate the declaration of the type
   --  in the stubs package (for interfaces and valuetypes)

   procedure Gen_Object_Reference_Declaration
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      pragma Assert (False
         or else Kind (Node) = K_Interface
         or else Kind (Node) = K_ValueType);

      NL (CU);
      Put (CU, "type "
           & Ada_Type_Defining_Name (Node)
           & " is new ");
      if Parents (Node) = Nil_List then
         case (Kind (Node)) is
            when K_Interface =>
               Add_With (CU, "CORBA.Object");
               Put (CU, "CORBA.Object.Ref");
            when K_ValueType =>
               Add_With (CU, "CORBA.Value");
               Put (CU, "CORBA.Value.Base");
            when others =>
               raise Program_Error;
               --  should not be called on another node
         end case;
      else
         declare
            First_Parent : Node_Id := Head (Parents (Node));
         begin
            Add_With (CU, Ada_Full_Name (First_Parent));
            Put (CU, Ada_Type_Name (First_Parent));
         end;
      end if;
      PL (CU, " with null record;");
      NL (CU);
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
               Put (CU, "  ");
               if Abst (Node) then
                  Put (CU, "abstract ");
               end if;
               Put (CU, "new PortableServer.Servant_Base");
            else
               declare
                  It : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Parents (Node));

                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     Add_With (CU, Ada_Full_Name (P_Node)
                               & Impl_Suffix,
                               Use_It => False,
                               Elab_Control => Elaborate_All);
                     --  Make it so that the skeleton unit for
                     --  an interface is elaborated after those
                     --  of all its parents.

                     if First then
                        Put (CU, "  ");
                        if Abst (Node) then
                           Put (CU, "abstract ");
                        end if;
                        Put (CU, "new "
                             & Ada_Full_Name (P_Node)
                             & Impl_Suffix & ".Object");
                        First := False;
                     end if;
                  end loop;
               end;
            end if;

            PL (CU, " with null record;");

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
         Get_Next_Node (It, Label_Node);

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

   -------------------------
   -- Gen_Node_Stubs_Spec --
   -------------------------

   procedure Gen_Node_Stubs_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         --  Scopes

         when
           K_Repository |
           K_Module =>
            null;

         when K_Interface  =>
            null;

         when K_Forward_Interface =>
            Add_With (CU, "CORBA.Forward");
            NL (CU);
            PL (CU, "package " & Ada_Name (Node)
                & " is new CORBA.Forward;");

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

            if not Is_Implicit_Inherited (Node) then
               Gen_Operation_Profile (CU,
                                      "in "
                                      & Ada_Type_Defining_Name
                                      (Parent_Scope (Node)),
                                      Node);
               PL (CU, ";");
            end if;

            --        when K_Attribute =>
            --  null;

         when K_Exception =>

            Add_With (CU, "Ada.Exceptions");
            Add_With (CU, "CORBA");
            NL (CU);
            PL (CU, Ada_Name (Node) & " : exception;");
            PL (CU, Ada_Name (Node) & "_" & T_Repository_Id
                & " : constant CORBA.RepositoryId");
            PL (CU, "  := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node) & """);");
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
                  Get_Next_Node (It, Decl_Node);

                  Gen_Node_Stubs_Spec (CU, Decl_Node);
                  Put (CU, " : ");
                  Gen_Node_Stubs_Spec (CU, M_Type (Node));
                  PL (CU, ";");

               end loop;
            end;

         when K_Enum =>
            --  type declaration
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

                  Get_Next_Node (It, E_Node);

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
               Is_Fixed : constant Boolean
                 := Kind (T_Type (Node)) = K_Fixed;
            begin
               declare
                  It   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);

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
                              Get_Next_Node (Bounds_It, Bound_Node);

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
                           if not (Is_Interface or else Is_Fixed) then
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
                  Get_Next_Node (It, Case_Node);

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

         when K_String_Instance =>
            NL (CU);
            PL (CU, "package " & Name (Node) & " is");
            if Is_Wide (Node) then
               Add_With (CU, "CORBA.Bounded_Wide_Strings",
                         Use_It => False,
                         Elab_Control => Elaborate_All);
               Put (CU, "  new CORBA.Bounded_Wide_Strings");
            else
               Add_With (CU, "CORBA.Bounded_Strings",
                         Use_It => False,
                         Elab_Control => Elaborate_All);
               Put (CU, "  new CORBA.Bounded_Strings");
            end if;
            PL (CU, " (" & Img (Integer_Value (Bound (Node)))
                & ");");

         when K_Sequence_Instance =>
            NL (CU);
            PL (CU, "package " & Name (Node) & " is");
            declare
               S_Node : constant Node_Id
                 := Sequence (Node);
               B_Node : constant Node_Id
                 := Bound (S_Node);
            begin
               if B_Node /= No_Node then
                  Add_With (CU, "Sequences.Bounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new Sequences.Bounded");
                  PL (CU, "    ("
                      & Ada_Type_Name (Sequence_Type (S_Node))
                      & ", " & Img (Integer_Value (B_Node))
                      & ");");
               else
                  Add_With (CU, "Sequences.Unbounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new Sequences.Unbounded");
                  PL (CU, "    ("
                      & Ada_Type_Name (Sequence_Type (S_Node))
                      & ");");
               end if;
            end;

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
                     Get_Next_Node (It, Member_Node);
                     Gen_Node_Stubs_Spec (CU, Member_Node);
                  end loop;
               end;

               DI (CU);
               PL (CU, "end record;");
            end if;

         when K_Const_Dcl =>
            NL (CU);
            Put (CU, Name (Node) & " : constant ");
            Gen_Node_Stubs_Spec (CU, Constant_Type (Node));
            NL (CU);
            Put (CU, "  := ");
            Gen_Constant_Value (CU, Expression (Node));
            PL (CU, ";");

         when K_ValueBase =>
            null;
         when K_Native =>
            null;

         when K_Object =>
            null;
--         when K_Any =>
--            null;
         when K_Void =>
            null;

         when K_Fixed =>
            Put (CU, "delta 10.0 ** (-");
            Gen_Node_Stubs_Spec (CU, Scale (Node));
            Put (CU, ") digits ");
            Gen_Node_Stubs_Spec (CU, Digits_Nb (Node));

         when others =>
            Gen_Node_Default (CU, Node);
      end case;

   end Gen_Node_Stubs_Spec;

   procedure Gen_Node_Impl_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin

      pragma Debug (O ("Gen_Node_Impl_Spec for node "
                       & Node_Kind'Image (Kind (Node))));

      case Kind (Node) is

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>

            --  No operation profile is generated for expanded
            --  state members.

            declare
               Old : Node_Id := Original_Node (Node);
            begin
               if Old /= No_Node then
                  if Kind (Old) = K_State_Member then
                     return;
                  end if;
               end if;
            end;

            if not Is_Implicit_Inherited (Node) then
               Gen_Operation_Profile (CU, "access Object", Node);
               PL (CU, ";");
            end if;

         when others =>
            null;

      end case;

   end Gen_Node_Impl_Spec;

   procedure Gen_Node_Impl_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>

            if Is_Implicit_Inherited (Node) then
               return;
            end if;

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
               else
                  PL (CU, "null;");
               end if;
               DI (CU);
               PL (CU, "end " & Ada_Operation_Name (Node) & ";");
            end;

         when others =>
            null;

      end case;

   end Gen_Node_Impl_Body;


   -------------------------
   --  Gen_Node_Skel_Body --
   -------------------------
   procedure Gen_Node_Skel_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
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
         NL (CU);
         PL (CU, "--  Sanity check");
         if Is_Oneway (Node) then
            PL (CU, "if Response_Expected then");
         else
            PL (CU, "if not Response_Expected then");
         end if;
         II (CU);
         PL (CU, "Broca.Exceptions.Raise_Bad_Param;");
         DI (CU);
         PL (CU, "end if;");
         NL (CU);

         PL (CU, "declare");
         II (CU);

         --  Declare local args
         declare
            It   : Node_Iterator;
            P_Node : Node_Id;
         begin

            Init (It, Parameters (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, P_Node);

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
               Get_Next_Node (It, P_Node);

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
               Get_Next_Node (It, P_Node);

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

               Get_Next_Node (It, R_Node);
               E_Node := Value (R_Node);
               --  Each R_Node is a scoped_name
               --  that denotes an exception.

               Add_With_Entity (CU, E_Node);
               NL (CU);
               II (CU);
               PL (CU, "when E : " & Ada_Full_Name (E_Node)
                   & " =>");
               II (CU);

               PL (CU, "declare");
               II (CU);
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
               PL (CU, "Marshall");
               PL (CU, "  (Reply_Buffer, CORBA.String (");
               PL (CU, "   " & Ada_Full_Name (E_Node)
                   & "_" & T_Repository_Id & "));");
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
               Get_Next_Node (It, P_Node);

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
           K_Module =>
            null;

         when K_Interface  =>
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

            if Is_Implicit_Inherited (Node) then
               return;
            end if;

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
               Add_With (CU, "Broca.Exceptions");

               NL (CU);
               PL (CU, O_Name
                   & "_Operation : constant CORBA.Identifier");
               PL (CU, "  := CORBA.To_CORBA_String ("""
                   & Idl_Operation_Id (Node) & """);");

               Gen_Operation_Profile (CU,
                                      "in "
                                      & Ada_Type_Defining_Name
                                      (Parent_Scope (Node)),
                                      Node);
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

               PL (CU, "if Is_Nil (Self) then");
               II (CU);
               PL (CU, "Broca.Exceptions.Raise_Inv_Objref;");
               DI (CU);
               PL (CU, "end if;");

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
                     Get_Next_Node (It, P_Node);

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

               if Response_Expected then
                  declare
                     It     : Node_Iterator;
                     P_Node : Node_Id;
                     First  : Boolean := True;
                  begin
                     Init (It, Parameters (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, P_Node);

                        case Mode (P_Node) is
                           when Mode_Inout | Mode_Out =>
                              if First then
                                 NL (CU);
                                 PL
                                   (CU,
                                    "--  Unmarshall inout and out " &
                                    "parameters.");
                                 First := False;
                              end if;
                              PL (CU, Ada_Name (Declarator (P_Node))
                                  & " := Unmarshall (" & T_Handler &
                                  ".Buffer'Access);");
                           when others =>
                              null;
                        end case;

                     end loop;
                  end;

                  PL (CU, "Broca.GIOP.Release (" & T_Handler & ");");
                  if Kind (O_Type) /= K_Void then
                     PL (CU, "return " & T_Returns & ";");
                  else
                     PL (CU, "return;");
                  end if;

               else
                  PL (CU, "Broca.GIOP.Release (" & T_Handler & ");");
                  PL (CU, "raise Program_Error;");
               end if;

               DI (CU);
               PL (CU, "when Broca.GIOP.Sr_No_Reply =>");
               II (CU);
               --  XXX ??? What's this ? FIXME.
               PL (CU, "Broca.GIOP.Release (" & T_Handler & ");");

               if Response_Expected then
                  PL (CU, "raise Program_Error;");
               else
                  PL (CU, "return;");
               end if;
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
                     Get_Next_Node (It, R_Node);
                     E_Node := Value (R_Node);
                     --  Each R_Node is a scoped name
                     --  that denotes an exception.

                     Add_With_Entity (CU, E_Node);

                     if First then
                        Add_With (CU, "Broca.Exceptions");

                        PL (CU, "declare");
                        II (CU);
                        PL (CU, T_Exception_Repo_Id
                            & " : constant CORBA.RepositoryId");
                        PL (CU, "  := CORBA.RepositoryId");
                        PL (CU, "      (CORBA.String'");
                        PL (CU, "       (Unmarshall (" & T_Handler &
                            ".Buffer'Access)));");
                        DI (CU);
                        PL (CU, "begin");
                        II (CU);

                        First := False;
                     end if;

                     NL (CU);
                     PL (CU, "if CORBA.""="" ("
                         & T_Exception_Repo_Id & ",");
                     PL (CU, "  " & Ada_Full_Name (E_Node)
                         & "_" & T_Repository_Id & ") then");
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
                     PL (CU, "Broca.GIOP.Release (" & T_Handler & ");");
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
               PL (CU, "Broca.GIOP.Release (" & T_Handler & ");");
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

   ---------------------------
   --  Gen_Node_Stream_Spec --
   ---------------------------

   procedure Gen_Node_Stream_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Exception =>
            null;

         when K_ValueType =>
            null;

         when
           K_Forward_Interface |
           K_Interface =>
            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, ";");

         when
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Sequence_Instance |
           K_String_Instance   =>
            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, ";");
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, ";");

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
                        Get_Next_Node (It, Decl_Node);

                        NL (CU);
                        Gen_Marshall_Profile
                          (CU, Decl_Node);
                        PL (CU, ";");
                        Gen_Unmarshall_Profile
                          (CU, Decl_Node);
                        PL (CU, ";");
                     end loop;
                  end;
               end if;
            end;

         when others =>
            null;
      end case;

   end Gen_Node_Stream_Spec;

   ---------------------------
   -- Gen_Operation_Profile --
   ---------------------------

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
                  Get_Next_Node (It, P_Node);

                  PL (CU, ";");
                  Gen_Operation_Profile
                    (CU, Object_Type, P_Node);
               end loop;

               Put (CU, ")");
               DI (CU);
            end;

            --  Return type

            declare
               O_Type : constant Node_Id
                 := Operation_Type (Node);
            begin
               if Kind (O_Type) /= K_Void then
                  NL (CU);
                  Add_With_Entity (CU, O_Type);
                  Put (CU, "  return "
                       & Ada_Type_Name (O_Type));

                  --  FIXME:
                  --  This is disabled for now because
                  --  it requires fixing the unmarshalling
                  --  of references (we must really be able to
                  --  unmarshall any kind of reference and
                  --  return a CORBA.Object.Ref'Class).

                  if False
                    and then Kind (O_Type) = K_Scoped_Name
                    and then S_Type (O_Type)
                    = Parent_Scope (Node) then
                     --  An operation of an interface is a
                     --  primitive operation of the tagged type
                     --  that maps this interface. If it has
                     --  other formal parameters that are object
                     --  references of the same interface type, then
                     --  these formals must not be controlling.
                     --  (Ada RTF issue #2459).

                     Put (CU, "'Class");
                  end if;
               end if;
            end;
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

               --  FIXME:
               --  Code disabled, see above.

               if False
                 and then Kind (T_Node) = K_Scoped_Name
                 and then S_Type (T_Node) = Parent_Scope
                 (Parent_Scope (Declarator (Node))) then

                  --  An operation of an interface is a
                  --  primitive operation of the tagged type
                  --  that maps this interface. If it has
                  --  other formal parameters that are object
                  --  references of the same interface type, then
                  --  these formals must not be controlling.
                  --  (Ada RTF issue #2459).
                  --  (see above) -- FIXME: code duplication.

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

   --------------------------
   -- Gen_Node_Stream_Body --
   --------------------------

   procedure Gen_Node_Stream_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is

         when K_Exception =>
            null;

         when K_ValueType =>
            null;

         when K_Struct =>

            declare
               S_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  if Is_End (It) then
                     PL (CU, "--  Empty structure.");
                     PL (CU, "null;");
                  end if;
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);

                     Add_With_Stream (CU, M_Type (Member_Node));

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Get_Next_Node (DIt, Decl_Node);

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
               Gen_Unmarshall_Profile (CU, Node);
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
                  if Is_End (It) then
                     PL (CU, "--  Empty structure.");
                  end if;
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Get_Next_Node (DIt, Decl_Node);

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
               Gen_Marshall_Profile (CU, Node);
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
                     Get_Next_Node (It, Case_Node);

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
               Gen_Unmarshall_Profile (CU, Node);
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
                     Get_Next_Node (It, Case_Node);

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
               Gen_Marshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "if Val'Valid then");
               II (CU);
               PL (CU, "Marshall");
               PL (CU, "  (Buffer,");
               PL (CU, "   CORBA.Unsigned_Long ("
                   & E_Name & "'Pos (Val)));");
               DI (CU);
               PL (CU, "else");
               II (CU);
               PL (CU, "Marshall");
               PL (CU, "  (Buffer,");
               PL (CU, "   CORBA.Unsigned_Long'(0));");
               DI (CU);
               PL (CU, "end if;");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, Node);
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
               Is_Fixed : constant Boolean
                 := Kind (T_Type (Node)) = K_Fixed;
            begin
               if Is_Interface then
                  null;
               elsif Is_Fixed then
                  declare
                     Decl_Node : constant Node_Id
                       := Head (Declarators (Node));
                     Type_Name : constant String
                       := Ada_Name (Decl_Node);
                  begin
                     NL (CU);
                     PL (CU, "package CDR_"
                         & Type_Name & " is");
                     PL (CU, "  new Broca.CDR.Fixed_Point ("
                         & Type_Name & ");");

                     Gen_Marshall_Profile
                       (CU, Decl_Node);
                     PL (CU, " renames CDR_" & Type_Name
                         & ".Marshall;");
                     Gen_Unmarshall_Profile
                       (CU, Decl_Node);
                     PL (CU, " renames CDR_" & Type_Name
                         & ".Unmarshall;");
                  end;
               else
                  declare
                     Base_Type_Name : constant String
                       := Ada_Type_Name (T_Type (Node));

                     It   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Add_With_Stream (CU, T_Type (Node));
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, Decl_Node);

                        declare
                           Type_Name : constant String
                             := Ada_Type_Name (Decl_Node);
                           Array_Dimensions : constant Natural
                             := Length (Array_Bounds (Decl_Node));
                        begin
                           NL (CU);
                           Gen_Marshall_Profile
                             (CU, Decl_Node);
                           PL (CU, " is");
                           PL (CU, "begin");
                           II (CU);
                           if Array_Dimensions /= 0 then
                              Gen_Array_Iterator
                                (CU, "Val", Array_Dimensions,
                                 "Marshall (Buffer, Val %);");
                           else
                              PL (CU, "Marshall");
                              PL (CU, "  (Buffer,");
                              PL (CU, "   "
                                  & Base_Type_Name
                                  & " (Val));");
                           end if;
                           DI (CU);
                           PL (CU, "end Marshall;");

                           NL (CU);
                           Gen_Unmarshall_Profile
                             (CU, Decl_Node);
                           if Array_Dimensions /= 0 then
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
                           else
                              PL (CU, " is");
                              PL (CU, "begin");
                              II (CU);
                              PL (CU, "return " & Type_Name);
                              PL (CU, "  (" & Base_Type_Name & "'");
                              PL (CU, "   (Unmarshall (Buffer)));");
                           end if;

                           DI (CU);
                           PL (CU, "end Unmarshall;");
                        end;
                     end loop;
                  end;
               end if;
            end;

         when
           K_Interface         |
           K_Forward_Interface =>
            Add_With (CU, "Broca.CDR");
--             if NK = K_Forward_Interface then
--                NL (CU);
--                Gen_Marshall_Profile (CU, Node);
--                if NK = K_Forward_Interface then
--                   NL (CU);
--                   PL (CU, "is");
--                   II (CU);
--                   PL (CU, "use " & Ada_Name (Node) & ";");
--                   DI (CU);
--                else
--                   PL (CU, " is");
--                end if;
--                PL (CU, "begin");
--                II (CU);
--                PL (CU, "Broca.CDR.Marshall (Buffer, Val);");
--                DI (CU);
--                PL (CU, "end Marshall;");
--             end if;

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            if NK = K_Forward_Interface then
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, "use " & Ada_Name (Node) & ";");
               DI (CU);
            else
               PL (CU, " is");
            end if;
            II (CU);
            PL (CU, "New_Ref : " & Ada_Type_Name (Node) & ";");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "Broca.CDR.Unmarshall (Buffer, New_Ref);");
            PL (CU, "return New_Ref;");
            DI (CU);
            PL (CU, "end Unmarshall;");

         when K_Sequence_Instance =>

            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Elements : constant Element_Array");
            PL (CU, "  := To_Element_Array (Val);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "Marshall");
            PL (CU, "  (Buffer, CORBA.Unsigned_Long (Elements'Length));");

            Add_With_Stream (CU, Sequence_Type (Sequence (Node)));
            PL (CU, "for I in Elements'Range loop");
            II (CU);
            PL (CU, "Marshall (Buffer, Elements (I));");
            DI (CU);
            PL (CU, "end loop;");
            DI (CU);
            PL (CU, "end Marshall;");

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            NL (CU);
            PL (CU, "is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Length : constant CORBA.Unsigned_Long");
            PL (CU, "  := Unmarshall (Buffer);");
            PL (CU, "Elements : Element_Array (1 .. Integer (Length));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "for I in Elements'Range loop");
            II (CU);
            PL (CU, "Elements (I) := Unmarshall (Buffer);");
            DI (CU);
            PL (CU, "end loop;");
            PL (CU, "return To_Sequence (Elements);");
            DI (CU);
            PL (CU, "end Unmarshall;");

         when K_String_Instance =>

            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            if Is_Wide (Node) then
               PL (CU, "Data : constant CORBA.Wide_String");
            else
               PL (CU, "Data : constant CORBA.String");
            end if;
            PL (CU, "  := To_String (Val);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "Broca.CDR");
            PL (CU, "Marshall");
            PL (CU, "  (Buffer, Data);");
            DI (CU);
            PL (CU, "end Marshall;");

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            NL (CU);
            PL (CU, "is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Data : constant Bounded_String");
            PL (CU, "  := To_Bounded_String (Unmarshall (Buffer));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return Data;");
            DI (CU);
            PL (CU, "end Unmarshall;");

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
           K_Octet              |
           K_Any                =>
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
            Put (CU, String_Value (Node));

         when K_Lit_Integer =>
            Put (CU, Img (Integer_Value (Node)));

         when K_Lit_Boolean =>
            Put (CU, Img (Boolean_Value (Node)));

         when K_Lit_Enum =>
            Put (CU, Ada_Name (Enum_Value (Node)));

         when K_Primary_Expr =>
            Gen_Node_Default (CU, Operand (Node));

         when others =>
            Error
              ("Don't know what to do with a "
               & Kind (Node)'Img & " node.",
               Fatal, Get_Location (Node));
      end case;
   end Gen_Node_Default;

   procedure Gen_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "procedure Marshall");
      PL (CU, "  (Buffer : access Buffer_Type;");
      Put (CU, "   Val    : in "
           & Ada_Type_Name (Type_Node) & ")");
   end Gen_Marshall_Profile;

   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "function Unmarshall");
      PL (CU, "  (Buffer : access Buffer_Type)");
      Put (CU, "  return "
           & Ada_Type_Name (Type_Node));
   end Gen_Unmarshall_Profile;

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "CORBA", Use_It => False);
      PL (CU, "function From_Any (Item : in CORBA.Any)");
      II (CU);
      Put (CU, "return "
           & Ada_Type_Name (Type_Node));
      DI (CU);
   end Gen_From_Any_Profile;

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "CORBA", Use_It => False);
      PL (CU, "function To_Any (Item : in "
          & Ada_Type_Name (Type_Node)
          & ")");
      II (CU);
      Put (CU, "return CORBA.Any");
      DI (CU);
   end Gen_To_Any_Profile;

   -----------------------------
   --  Ada_Type_Defining_Name --
   -----------------------------

   function Ada_Type_Defining_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface =>

            if Abst (Node) then
               return "Abstract_Ref";
            --  elsif Local (Node) then
            --   return "Local_Ref";
            else
               return "Ref";
            end if;

         when
           K_ValueType         |
           K_Forward_ValueType =>

            if Abst (Node) then
               return "Abstract_Value_Ref";
            else
               return "Value_Ref";
            end if;

         when others =>
            --  Improper use: node N is not an
            --  Interface or ValueType.

            Error
              ("Improper call of Ada_Type_Defining_Name with a " & NK'Img,
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_Type_Defining_Name;

   --------------------
   --  Ada_Type_Name --
   --------------------

   function Ada_Type_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface |
           K_ValueType         |
           K_Forward_ValueType =>
            return Ada_Full_Name (Node) & "." & Ada_Type_Defining_Name (Node);

         when K_Sequence_Instance =>
            return Ada_Full_Name (Node) & ".Sequence";

         when K_String_Instance =>
            return Ada_Full_Name (Node) & ".Bounded_String";

         when
           K_Enum       |
           K_Union      |
           K_Struct     |
           K_Declarator =>
            return Ada_Full_Name (Node);

         when K_Scoped_Name =>
            return Ada_Type_Name (Value (Node));

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

         when K_Object =>
            return "CORBA.Object.Ref";

         when K_Any =>
            return "CORBA.Any";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("A " & NK'Img
               & " does not denote a type.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_Type_Name;

   ------------------
   --  Ada_TC_Name --
   ------------------

   function Ada_TC_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
      Prefix : constant String := "TC_";
   begin
      case NK is
--          when
--            K_Interface         |
--            K_Forward_Interface |
--            K_ValueType         |
--            K_Forward_ValueType =>
--       return Prefix & Ada_Name (Node) & "." & Ada_Type_Defining_Name (Node);

--          when K_Sequence_Instance =>
--             return Ada_Full_Name (Node) & ".Sequence";

--          when K_String_Instance =>
--             return Ada_Full_Name (Node) & ".Bounded_String";

         when
           K_Enum       |
           K_Union      |
           K_Struct     |
           K_Declarator =>
            return Prefix & Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_TC_Name (Value (Node));

         when K_Short =>
            return "CORBA." & Prefix & "Short";

         when K_Long =>
            return "CORBA." & Prefix & "Long";

         when K_Long_Long =>
            return "CORBA." & Prefix & "Long_Long";

         when K_Unsigned_Short =>
            return "CORBA." & Prefix & "Unsigned_Short";

         when K_Unsigned_Long =>
            return "CORBA." & Prefix & "Unsigned_Long";

         when K_Unsigned_Long_Long =>
            return "CORBA." & Prefix & "Unsigned_Long_Long";

         when K_Char =>
            return "CORBA." & Prefix & "Char";

         when K_Wide_Char =>
            return "CORBA." & Prefix & "Wide_Char";

         when K_Boolean =>
            return "CORBA." & Prefix & "Boolean";

         when K_Float =>
            return "CORBA." & Prefix & "Float";

         when K_Double =>
            return "CORBA." & Prefix & "Double";

         when K_Long_Double =>
            return "CORBA." & Prefix & "Long_Double";

         when K_String =>
            return "CORBA." & Prefix & "String";

         when K_Wide_String =>
            return "CORBA." & Prefix & "Wide_String";

         when K_Octet =>
            return "CORBA." & Prefix & "Octet";

         when K_Object =>
            return "CORBA." & Prefix & "Object.Ref";

         when K_Any =>
            return "CORBA." & Prefix & "Any";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("A " & NK'Img
               & " does not denote a type. So there's no TypeCode"
               & " associated with it.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_TC_Name;

   procedure Add_With_Entity
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when
           K_Interface    |
           K_Module       |
           K_ValueType    |
           K_Ben_Idl_File =>
            Add_With (CU, Ada_Full_Name (Node));

         when
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Declarator        |
           K_Forward_Interface |
           K_Exception         |
           K_Sequence_Instance |
           K_String_Instance   =>
            Add_With_Entity (CU, Parent_Scope (Node));

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
           K_Octet              |
           K_Object             |
           K_Any                =>
            Add_With (CU, "CORBA");

         when others =>
            Error
              ("A " & NK'Img
               & " is not a mapped entity.",
               Fatal, Get_Location (Node));
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
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Forward_Interface |
           K_Sequence_Instance |
           K_String_Instance   =>
            Add_With (CU, Ada_Full_Name (Parent_Scope (Node))
                      & Stream_Suffix,
                      Use_It => True);

         when K_Declarator =>
            declare
               P_Node : constant Node_Id
                 := Parent (Node);
            begin
               if True
                 and then Kind (P_Node) = K_Type_Declarator
                 and then Is_Empty (Array_Bounds (Node))
                 and then Is_Interface_Type (T_Type (P_Node)) then
                  --  For a typedef of an interface type, the
                  --  dependance must be on the stream unit for
                  --  the scope that contains the actual definition.
                  Add_With_Stream (CU, T_Type (P_Node));
               else
                  Add_With
                    (CU, Ada_Full_Name (Parent_Scope (Node))
                     & Stream_Suffix,
                     Use_It => True);
               end if;
            end;

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
           K_Octet              |
           K_Any                =>

            Add_With (CU, "Broca.CDR",
                      Use_It => True);

         when K_Object =>
            Add_With (CU, "Broca.CDR",
                      Use_It => True);

         when K_Fixed =>
            Add_With (CU, "Broca.CDR");

         when K_ValueType =>
            null;

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("A " & NK'Img
               & " does not denote a streamable type.",
               Fatal, Get_Location (Node));
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

   ----------------
   -- Gen_Helper --
   ----------------

   procedure Gen_Helper
     (Node : in Node_Id;
      Helper_Spec : in out Compilation_Unit;
      Helper_Body : in out Compilation_Unit) is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when K_Interface =>
            declare
               Short_Type_Name : constant String
                 := Ada_Type_Defining_Name (Node);
               Type_Name : constant String
                 := Ada_Type_Name (Node);
            begin

               --  Unchecked_To_<reference>

               Add_With (Helper_Spec, "CORBA.Object");
               NL (Helper_Spec);
               PL (Helper_Spec, "function Unchecked_To_" & Short_Type_Name);
               PL (Helper_Spec, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (Helper_Spec, "  return " & Type_Name & ";");
               PL (Helper_Spec, "function To_" & Short_Type_Name);
               PL (Helper_Spec, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (Helper_Spec, "  return " & Type_Name & ";");

               Add_With (Helper_Body, "Broca.Refs");
               Add_With (Helper_Body, "Broca.Exceptions");

               NL (Helper_Body);
               PL (Helper_Body, "function Unchecked_To_" & Short_Type_Name);
               PL (Helper_Body, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (Helper_Body, "  return " & Type_Name);
               PL (Helper_Body, "is");
               II (Helper_Body);
               PL (Helper_Body, "Result : " & Type_Name & ";");
               DI (Helper_Body);
               PL (Helper_Body, "begin");
               II (Helper_Body);
               PL (Helper_Body, "Set (Result,");
               PL (Helper_Body,
                   "     CORBA.Object.Get (The_Ref));");
               PL (Helper_Body, "return Result;");
               DI (Helper_Body);
               PL (Helper_Body, "end Unchecked_To_" & Short_Type_Name & ";");

               --  To_<reference>
               NL (Helper_Body);
               PL (Helper_Body, "function To_" & Short_Type_Name);
               PL (Helper_Body, "  (The_Ref : in CORBA.Object.Ref'Class)");
               PL (Helper_Body, "  return " & Type_Name);
               PL (Helper_Body, "is");
               II (Helper_Body);
               PL (Helper_Body, "Result : " & Type_Name & ";");
               DI (Helper_Body);
               PL (Helper_Body, "begin");
               II (Helper_Body);
               PL (Helper_Body,
                   "Result := Unchecked_To_"
                   & Short_Type_Name
                   & " (The_Ref);");
               PL (Helper_Body, "if Is_A (Result, "
                   & T_Repository_Id & ") then");
               II (Helper_Body);
               PL (Helper_Body, "return Result;");
               DI (Helper_Body);
               PL (Helper_Body, "else");
               II (Helper_Body);
               PL (Helper_Body, "Broca.Exceptions.Raise_Bad_Param;");
               DI (Helper_Body);
               PL (Helper_Body, "end if;");
               DI (Helper_Body);
               PL (Helper_Body, "end To_" & Short_Type_Name & ";");
            end;

            --  From_Any
            NL (Helper_Spec);
            Gen_From_Any_Profile (Helper_Spec, Node);
            PL (Helper_Spec, ";");

            Add_With (Helper_Body, "CORBA.Object.Helper");
            NL (Helper_Body);
            Gen_From_Any_Profile (Helper_Body, Node);
            PL (Helper_Body, " is");
            PL (Helper_Body, "begin");
            II (Helper_Body);
            PL (Helper_Body, "return To_Ref (CORBA.Object.Helper."
                & "From_Any (Item));");
            DI (Helper_Body);
            PL (Helper_Body, "end From_Any;");

            --  To_Any
            NL (Helper_Spec);
            Gen_To_Any_Profile (Helper_Spec, Node);
            PL (Helper_Spec, ";");

            Add_With (Helper_Body, "CORBA.Object.Helper");
            NL (Helper_Body);
            Gen_To_Any_Profile (Helper_Body, Node);
            PL (Helper_Body, " is");
            PL (Helper_Body, "begin");
            II (Helper_Body);
            PL (Helper_Body, "return CORBA.Object.Helper.To_Any "
                & "(CORBA.Object.Ref (Item));");
            DI (Helper_Body);
            PL (Helper_Body, "end To_Any;");

         when K_Enum =>
            --  TypeCode
            NL (Helper_Spec);
            PL (Helper_Spec, Ada_TC_Name (Node)
                & " : constant CORBA.TypeCode.Object := ");
            II (Helper_Spec);
            PL (Helper_Spec, "CORBA.TypeCode.TC_Enum;");
            DI (Helper_Spec);

            --  From_Any
            NL (Helper_Spec);
            Gen_From_Any_Profile (Helper_Spec, Node);
            PL (Helper_Spec, ";");

            NL (Helper_Body);
            Gen_From_Any_Profile (Helper_Body, Node);
            PL (Helper_Body, " is");
            II (Helper_Body);
            PL (Helper_Body, "Index : CORBA.Any :=");
            II (Helper_Body);
            PL (Helper_Body, "CORBA.Get_Aggregate_Element (Item,");
            PL (Helper_Body, "                             "
                & "CORBA.TC_Unsigned_Long,");
            PL (Helper_Body, "                             "
                & "CORBA.Unsigned_Long (0));");
            DI (Helper_Body);
            PL (Helper_Body, "Position : CORBA.Unsigned_Long "
                & ":= CORBA.From_Any (Index);");
            DI (Helper_Body);
            PL (Helper_Body, "begin");
            II (Helper_Body);
            PL (Helper_Body, "return "
                & Ada_Name (Node)
                & "'Val (Position);");
            DI (Helper_Body);
            PL (Helper_Body, "end From_Any;");

            --  To_Any
            NL (Helper_Spec);
            Gen_To_Any_Profile (Helper_Spec, Node);
            PL (Helper_Spec, ";");

            NL (Helper_Body);
            Gen_To_Any_Profile (Helper_Body, Node);
            PL (Helper_Body, " is");
            II (Helper_Body);
            PL (Helper_Body, "Result : CORBA.Any := ");
            II (Helper_Body);
            PL (Helper_Body, "CORBA.Get_Empty_Any_Aggregate ("
                & Ada_TC_Name (Node)
                & ");");
            DI (Helper_Body);
            DI (Helper_Body);
            PL (Helper_Body, "begin");
            II (Helper_Body);
            PL (Helper_Body, "CORBA.Add_Aggregate_Element");
            II (Helper_Body);
            PL (Helper_Body, "(Result,");
            PL (Helper_Body, " CORBA.To_Any (CORBA.Unsigned_Long ("
                & Ada_Name (Node)
                & "'Pos (Item))));");
            DI (Helper_Body);
            PL (Helper_Body, "return Result;");
            DI (Helper_Body);
            PL (Helper_Body, "end To_Any;");

            --  to fill in the typecode TC_<name of the type>
--             PL (CU, "Add_Parameter ("
--                 & Ada_TC_Name (Node)
--                 & ", To_Any ("
--                 & Ada_Name (Node)
--                 & "));");
--             PL (CU, "Add_Parameter ("
--                 & Ada_TC_Name (Node)
--                 & ", To_Any ("
--                 & Idl_Repository_Id (Node)
--                 & "));");
--             declare
--                It   : Node_Iterator;
--                E_Node : Node_Id;
--             begin
--                Init (It, Enumerators (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, E_Node);
--                   PL (CU, "Add_Parameter ("
--                       & Ada_TC_Name (Node)
--                       & ", To_Any ("
--                       & Ada_Name (E_Node)
--                       & "));");
--                end loop;
--             end;

--          when K_Struct =>
--             --  typecode generation
--             NL (Helper_Spec);
--             PL (Helper_Spec, Ada_TC_Name (Node)
--                 & " : constant CORBA.TypeCode.Object := ");
--             II (Helper_Spec);
--             PL (Helper_Spec, "CORBA.TypeCode.TC_Struct;");
--             DI (Helper_Spec);

--             --  from_any function
--             NL (Helper_Spec);
--             Gen_From_Any_Profile (Helper_Spec, Node);
--             PL (Helper_Spec, ";");

--             NL (Helper_Spec);
--             Gen_To_Any_Profile (Helper_Spec, Node);
--             PL (Helper_Spec, ";");

--             Add_With (Helper_Body,
--                       "CORBA",
--                       Use_It => True);
--             NL (Helper_Body);
--             Gen_From_Any_Profile (Helper_Body, Node);
--             PL (Helper_Body, " is");
--             II (Helper_Body);
--             PL (Helper_Body, "Index : CORBA.Any;");
--             declare
--                It   : Node_Iterator;
--                Member_Node : Node_Id;
--             begin
--                Init (It, Members (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, Member_Node);
--                   declare
--                      It2   : Node_Iterator;
--                      Decl_Node : Node_Id;
--                   begin
--                      Init (It2, Decl (Member_Node));
--                      while not Is_End (It2) loop
--                         Get_Next_Node (It2, Decl_Node);
--                         PL (Helper_Body, "Result_"
--                             & Ada_Name (Decl_Node)
--                             & " : "
--                             & Ada_Type_Name (M_Type (Member_Node))
--                             & ";");
--                      end loop;
--                   end;
--                end loop;
--             end;
--             DI (Helper_Body);
--             PL (Helper_Body, "begin");
--             II (Helper_Body);
--             declare
--                It   : Node_Iterator;
--                Member_Node : Node_Id;
--                I : Integer := 0;
--             begin
--                Init (It, Members (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, Member_Node);
--                   declare
--                      It2   : Node_Iterator;
--                      Decl_Node : Node_Id;
--                   begin
--                      Init (It2, Decl (Member_Node));
--                      while not Is_End (It2) loop
--                         Get_Next_Node (It2, Decl_Node);
--                         PL (Helper_Body,
--                             "Index := CORBA.Get_Aggregate_Element (Item,");
--                         PL (Helper_Body,
--                             "                                      "
--                             & Ada_TC_Name (M_Type (Member_Node))
--                             & ",");
--                         PL (Helper_Body,
--                             "                                      "
--                             & "CORBA.Unsigned_Long ("
--                             & Integer'Image (I)
--                             &"));");
--                         PL (Helper_Body, "Result_"
--                             & Ada_Name (Decl_Node)
--                             & " := From_Any (Index);");
--                         I := I + 1;
--                      end loop;
--                   end;
--                end loop;
--             end;
--             PL (Helper_Body, "return");
--             II (Helper_Body);
--             declare
--                First_Member : Boolean := True;
--                Begin_Of_Line : String (1 .. 1) := "(";
--                End_Of_Line : String (1 .. 2) := ", ";
--                It   : Node_Iterator;
--                Member_Node : Node_Id;
--             begin
--                Init (It, Members (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, Member_Node);
--                   declare
--                      It2   : Node_Iterator;
--                      Decl_Node : Node_Id;
--                   begin
--                      Init (It2, Decl (Member_Node));
--                      while not Is_End (It2) loop
--                         Get_Next_Node (It2, Decl_Node);
--                         if Is_End (It) and Is_End (It2) then
--                            End_Of_Line := ");";
--                         end if;
--                         PL (Helper_Body, Begin_Of_Line
--                             & Ada_Name (Decl_Node)
--                             & " => Result_"
--                             & Ada_Name (Decl_Node)
--                             & End_Of_Line);
--                         if First_Member then
--                            First_Member := False;
--                            Begin_Of_Line := " ";
--                         end if;
--                      end loop;
--                   end;
--                end loop;
--             end;
--             DI (Helper_Body);
--             DI (Helper_Body);
--             PL (Helper_Body, "end From_Any;");

--             --  to_any function
--             NL (Helper_Body);
--             Gen_To_Any_Profile (Helper_Body, Node);
--             PL (Helper_Body, " is");
--             II (Helper_Body);
--             PL (Helper_Body, "Result : CORBA.Any := ");
--             II (Helper_Body);
--             PL (Helper_Body, "CORBA.Get_Empty_Any_Aggregate ("
--                 & Ada_TC_Name (Node)
--                 & ");");
--             DI (Helper_Body);
--             DI (Helper_Body);
--             PL (Helper_Body, "begin");
--             II (Helper_Body);
--             declare
--                It   : Node_Iterator;
--                Member_Node : Node_Id;
--             begin
--                Init (It, Members (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, Member_Node);
--                   declare
--                      It2   : Node_Iterator;
--                      Decl_Node : Node_Id;
--                   begin
--                      Init (It2, Decl (Member_Node));
--                      while not Is_End (It2) loop
--                         Get_Next_Node (It2, Decl_Node);
--                         PL (Helper_Body, "CORBA.Add_Aggregate_Element");
--                         II (Helper_Body);
--                         PL (Helper_Body, "(Result,");
--                         PL (Helper_Body, " CORBA.To_Any (Item."
--                             & Ada_Name (Decl_Node)
--                             & "));");
--                         DI (Helper_Body);
--                      end loop;
--                   end;
--                end loop;
--             end;
--             PL (Helper_Body, "return Result;");
--             DI (Helper_Body);
--             PL (Helper_Body, "end To_Any;");

            --  to fill in the typecode TC_<name of the type>
--             PL (CU, "Add_Parameter ("
--                 & Ada_TC_Name (Node)
--                 & ", To_Any ("
--                 & Ada_Name (Node)
--                 & "));");
--             PL (CU, "Add_Parameter ("
--                 & Ada_TC_Name (Node)
--                 & ", To_Any ("
--                 & Idl_Repository_Id (Node)
--                 & "));");
--             declare
--                It   : Node_Iterator;
--                Member_Node : Node_Id;
--             begin
--                Init (It, Members (Node));
--                while not Is_End (It) loop
--                   Get_Next_Node (It, Member_Node);
--                   PL (CU, "Add_Parameter ("
--                       & Ada_TC_Name (Node)
--                       & ", To_Any ("
--                       & Ada_TC_Name (Member_Node)
--                       & "));");
--                   PL (CU, "Add_Parameter ("
--                       & Ada_TC_Name (Node)
--                       & ", To_Any ("
--                       & Ada_Name (Member_Node)
--                       & "));");
--                end loop;
--             end;

         when others =>
            null;
      end case;

   end Gen_Helper;


   ------------------
   -- Gen_Delegate --
   ------------------

   procedure Gen_Delegate
     (Node : in Node_Id;
      Delegate_Spec : in out Compilation_Unit;
      Delegate_Body : in out Compilation_Unit) is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is
         when K_Interface =>
            if Abst (Node) then
               null;
            else
               PL (Delegate_Spec,
                   "type Wrapped is limited private;");
            end if;
         when K_Operation =>
            null; --  NIY
         when others =>
            null;
      end case;
   end Gen_Delegate;

   procedure Gen_Constant_Value
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
      Value : constant Constant_Value_Ptr
        := Expr_Value (Node);
   begin
      case Value.Kind is
         when
           C_Short           |
           C_Long            |
           C_LongLong        |
           C_UShort          |
           C_ULong           |
           C_ULongLong       |
           C_Octet           |
           C_General_Integer =>
            Put (CU, Img (Integer_Value (Node)));

         when C_Char =>
            Put (CU, "'" & Value.Char_Value & "'");

         when C_WChar =>
            Put (CU, Ada.Characters.Handling.To_String
                 ("'" & Value.WChar_Value & "'"));

         when C_Boolean =>
            Put (CU, Img (Boolean_Value (Node)));

         when
           C_Float         |
           C_Double        |
           C_LongDouble    |
           C_General_Float =>
            Put (CU, Img (Float_Value (Node)));

         when
           C_Fixed         |
           C_General_Fixed =>
            declare
               Value_Digits : constant String
                 := Img (Value.Fixed_Value);

               Zeroes : constant String
                 (1 .. Integer (Value.Scale) - Value_Digits'Length + 1)
                 := (others => '0');

               All_Digits : constant String
                 := Zeroes & Value_Digits;
            begin
               if Value.Scale = 0 then
                  Put (CU, Value_Digits);
               else
                  Put (CU,
                       All_Digits
                       (All_Digits'First
                        .. All_Digits'Last - Integer (Value.Scale))
                       & "."
                       & All_Digits
                       (All_Digits'Last - Integer (Value.Scale) + 1
                        .. All_Digits'Last));
               end if;
            end;

         when C_String =>
            Add_With (CU, "CORBA", Elab_Control => Elaborate);
            Put (CU, "CORBA.To_CORBA_String ("""
                 & String_Value (Node) & """)");

         when C_WString =>
            Add_With (CU, "CORBA", Elab_Control => Elaborate);
            Put (CU, "CORBA.To_CORBA_Wide_String ("""
                 & Ada.Characters.Handling.To_String
                 (WString_Value (Node)) & """)");

         when C_Enum =>
            Put (CU, Ada_Full_Name (Enum_Value (Node)));

         when C_No_Kind =>
            Error
              ("Constant without a kind.",
               Fatal, Get_Location (Node));
      end case;

   end Gen_Constant_Value;

end Ada_Be.Idl2Ada;
