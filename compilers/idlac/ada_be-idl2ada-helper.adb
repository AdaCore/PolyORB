------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . H E L P E R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2002 ENST Paris University, France.          --
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

--  $Id$

with Idlac_Flags;           use Idlac_Flags;
with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Utils;                 use Utils;

package body Ada_Be.Idl2Ada.Helper is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada.helper");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   --  Helpers need a special diversion for the initialization procedure.

   Deferred_Initialization     : constant Source_Streams.Diversion
     := Source_Streams.Allocate_User_Diversion;
   Initialization_Dependencies : constant Source_Streams.Diversion
     := Source_Streams.Allocate_User_Diversion;

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the From_Any operation of a type

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the To_Any operation of a type

   procedure Gen_Raise_From_Any_Profile
     (CU   : in out Compilation_Unit;
      Node : in     Node_Id);
   --  Generate the Raise_<exception>_From_Any procedure for an
   --  exception. The name of the procedure is
   --  Raise_From_Any_Name (Node).

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an interface declaration

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an interface declaration

   procedure Gen_ValueType_Spec
     (CU : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate the spec of the helper package for a valuetype declaration

   procedure Gen_ValueType_Body
     (CU : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate the body of the helper package for a valuetype declaration

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an enum declaration

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an enum declaration

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a struct or an
   --  exception declaration

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a struct or an
   --  exception declaration

   procedure Gen_String_Instance_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a string instance

   procedure Gen_String_Instance_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a string instance

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an union declaration

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an union declaration

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an array declaration

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an array declaration

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a sequence declaration

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a sequence declaration

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a fixed type declaration

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a fixed type declaration

   procedure Gen_Array_TC
     (CU                : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node         : in     Node_Id);
   --  generate lines to fill in an array typecode
   --  only used in the type_declarator part of gen_node_body

   function Raise_From_Any_Name (Node : in Node_Id) return String;
   --  Return the name of a procedure that raises that exception
   --  from an occurrence stored in an Any.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   --------------------
   -- Gen_Scope_Spec --
   --------------------

   procedure Gen_Node_Spec
     (CU        : in out Compilation_Unit;
      Node      :        Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            Gen_Interface_Spec (CU, Node);

         when K_Enum =>
            Gen_Enum_Spec (CU, Node);

         when K_Type_Declarator =>
--             if Is_Interface_Type (T_Type (Node)) then
--                null;
--             elsif
            if Kind (T_Type (Node)) = K_Fixed then
               Gen_Fixed_Spec (CU, Node);
            else
               declare
                  It   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Spec (CU, Decl_Node);
                  end loop;
               end;
            end if;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Struct_Exception_Spec (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Spec (CU, Node);

         when K_Union =>
            Gen_Union_Spec (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Spec (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Spec (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Spec (CU, Node);
            Gen_Raise_From_Any_Profile (CU, Node);
            PL (CU, ";");
            PL (CU, "pragma No_Return ("
                & Raise_From_Any_Name (Node) & ");");

         when others =>
            null;

      end case;
   end Gen_Node_Spec;

   --------------------
   -- Gen_Scope_Body --
   --------------------

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            Gen_Interface_Body (CU, Node);

         when K_Enum =>
            Gen_Enum_Body (CU, Node);

         when K_Type_Declarator =>
            if Is_Interface_Type (T_Type (Node)) then
               null;
            elsif Kind (T_Type (Node)) = K_Fixed then
               Gen_Fixed_Body (CU, Node);
            else
               declare
                  It   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Body (CU, Decl_Node);
                  end loop;
               end;
            end if;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Struct_Exception_Body (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Body (CU, Node);

         when K_Union =>
            Gen_Union_Body (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Body (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Body (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Body (CU, Node);
            Gen_Raise_From_Any_Profile (CU, Node);
            PL (CU, "");
            PL (CU, "is");
            II (CU);
            PL (CU, "Members : constant "
                & Ada_Name (Members_Type (Node)));
            PL (CU, "  := From_Any (Item);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "PolyORB.CORBA_P.Exceptions");
            PL (CU, "PolyORB.CORBA_P.Exceptions.User_Raise_Exception");
            PL (CU, "  (" & Ada_Name (Node) & "'Identity,");
            II (CU);
            PL (CU, "Members);");
            DI (CU);
            DI (CU);
            PL (CU, "end " & Raise_From_Any_Name (Node) & ";");

            Divert (CU, Deferred_Initialization);
            --  This has to be done in deferred initialization,
            --  after the TypeCode has been constructed.
            PL (CU, "PolyORB.CORBA_P.Exceptions.Register_Exception");
            PL (CU, "  (" & Ada_TC_Name (Node) & ",");
            II (CU);
            PL (CU, Raise_From_Any_Name (Node) & "'Access);");
            DI (CU);
            Divert (CU, Visible_Declarations);
         when others =>
            null;

      end case;
   end Gen_Node_Body;

   --------------------------
   -- Gen_From_Any_Profile --
   --------------------------

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "function From_Any (Item : in CORBA.Any)");
      II (CU);
      Put (CU, "return "
           & Ada_Type_Name (Type_Node));
      DI (CU);
   end Gen_From_Any_Profile;

   ------------------------
   -- Gen_To_Any_Profile --
   ------------------------

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "function To_Any");
      PL (CU, "  (Item : in "
          & Ada_Type_Name (Type_Node)
          & ")");
      Put (CU, "  return CORBA.Any");
   end Gen_To_Any_Profile;

   procedure Gen_Raise_From_Any_Profile
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "procedure " & Raise_From_Any_Name (Node));
      Put (CU, "  (Item : in CORBA.Any)");
   end Gen_Raise_From_Any_Profile;

   ------------------------
   -- Gen_Interface_Spec --
   ------------------------

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      --  Unchecked_To_<reference>
      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
      end;

      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := PolyORB.Any.TypeCode.TC_Object;");

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Interface_Spec;

   --------------------------------
   -- Gen_Forward_Interface_Spec --
   --------------------------------

   procedure Gen_Forward_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      --  Unchecked_To_<reference>
      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
      end;

      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := PolyORB.Any.TypeCode.TC_Object;");

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU,  Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Forward_Interface_Spec;

   procedure Gen_Spec_Postlude
     (CU : in out Compilation_Unit) is
   begin
      Divert (CU, Visible_Declarations);
      NL (CU);
      PL (CU, "procedure Deferred_Initialization;");
   end Gen_Spec_Postlude;

   procedure Gen_Body_Prelude
     (CU : in out Compilation_Unit) is
   begin
      NL (CU);
      PL (CU, "pragma Warnings (Off);");
      PL (CU, "--  Constructing typecodes tends to yield long lines.");

      Divert (CU, Deferred_Initialization);
      PL (CU, "procedure Deferred_Initialization is");
      PL (CU, "begin");
      II (CU);
      PL (CU, "if not Deferred_Initialization_Done then");
      II (CU);
      PL (CU, "null;");
      --  This 'if' block might be empty, so put a statement
      --  in there to keep the compiler happy.

      Divert (CU, Initialization_Dependencies);
      II (CU); II (CU); II (CU);
      PL (CU, "+""soft_links""");

      Divert (CU, Visible_Declarations);
   end Gen_Body_Prelude;

   procedure Gen_Body_Postlude
     (CU : in out Compilation_Unit) is
   begin
      Divert (CU, Deferred_Initialization);
      DI (CU);
      PL (CU, "end if;");
      NL (CU);
      PL (CU, "Deferred_Initialization_Done := True;");
      DI (CU);
      PL (CU, "end Deferred_Initialization;");

      Divert (CU, Visible_Declarations);
      NL (CU);
      PL (CU, "Deferred_Initialization_Done : Boolean := False;");
      NL (CU);
      Undivert (CU, Deferred_Initialization);

      Divert (CU, Visible_Declarations);
      Add_With
        (CU, "PolyORB.Initialization", Elab_Control => Elaborate_All);
      Add_With (CU, "PolyORB.Utils.Strings");

      Divert (CU, Elaboration);
      PL (CU, "declare");
      II (CU);
      PL (CU, "use PolyORB.Initialization;");
      PL (CU, "use PolyORB.Initialization.String_Lists;");
      PL (CU, "use PolyORB.Utils.Strings;");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "Register_Module");
      PL (CU, "  (Module_Info'");
      II (CU);
      PL (CU, "(Name      => +""" & Name (CU) & """,");
      PL (CU, " Conflicts => Empty,");
      PL (CU, " Depends   =>");
      Undivert (CU, Initialization_Dependencies);
      --  The creation of type codes requires the handling of
      --  'Any' types, which are controlled and use soft links
      --  to guard concurrent access to their internal structures.
      PL (CU, " ,");
      PL (CU, " Provides  => Empty,");
      PL (CU, " Init      => Deferred_Initialization'Access));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
   end Gen_Body_Postlude;

   ------------------------
   -- Gen_ValueType_Spec --
   ------------------------

   procedure Gen_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : in Node_Id)
   is
      Type_Name : constant String
        := Ada_Type_Defining_Name (Node);

      Type_Full_Name : constant String
        := Ada_Type_Name (Node);
      V_Impl_Name : constant String
        := Ada_Name (Node) & ".Value_Impl.Object'Class";
   begin
      pragma Assert (Kind (Node) = K_ValueType);
      Add_With (CU, "CORBA.Value");
      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : in CORBA.Value.Base'Class)");
      PL (CU, "  return " & Type_Full_Name & ";");

      --  generate code for supported interfaces
      --  generate this portion of code iff there is a non abstract
      --  supported interface.
      if Supports_Non_Abstract_Interface (Node) then
         Add_With (CU, Ada_Full_Name (Node) & ".Value_Impl");
         NL (CU);
         PL (CU, "type Servant");
         II (CU);
         PL (CU,
             "(Value : access "
             & V_Impl_Name
             & ")");
         Add_With (CU, "PortableServer");
         PL (CU, "is new PortableServer.Servant_Base with null record;");
         DI (CU);
         PL (CU,
             "type Servant_Ref is access all Servant'Class;");
         NL (CU);
         PL (CU, "function To_Servant");
         PL (CU, "  (Self : access "
             & V_Impl_Name
             & ")");
         PL (CU, "  return Servant_Ref;");
      end if;

   end Gen_ValueType_Spec;

   ------------------------
   -- Gen_ValueType_Body --
   ------------------------

   procedure Gen_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : in Node_Id) is

      Type_Name : constant String
        := Ada_Type_Defining_Name (Node);

      Type_Full_Name : constant String
        := Ada_Type_Name (Node);

      V_Impl_Name : constant String
        := Ada_Name (Node) & ".Value_Impl.Object'Class";
   begin
      pragma Assert (Kind (Node) = K_ValueType);

      Add_With (CU, "PolyORB.CORBA_P.Exceptions");

      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : in CORBA.Value.Base'Class)");
      PL (CU, "  return " & Type_Full_Name & " is");
      II (CU);
      PL (CU, "Result : " & Type_Full_Name & ";");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "if CORBA.Object.Is_Nil (The_Ref)");
      PL (CU, "  or else CORBA.Value.Is_A (The_Ref, "
          & Repository_Id_Name (Node) & ") then");
      II (CU);
      PL (CU, "Set (Result, CORBA.Value.Object_Of (The_Ref));");
      PL (CU, "return Result;");
      DI (CU);
      PL (CU, "else");
      II (CU);
      PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Bad_Param;");
      DI (CU);
      PL (CU, "end if;");
      DI (CU);
      PL (CU, "end To_" & Type_Name & ";");

      --  generate code for supported interfaces
      --  generate this portion of code iff there is a non abstract
      --  supported interface.
      if Supports_Non_Abstract_Interface (Node) then
         NL (CU);
         PL (CU, "function To_Servant");
         PL (CU, "  (Self : access "
             & V_Impl_Name
             & ")");
         PL (CU, "  return Servant_Ref is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return new Servant (Self);");
         DI (CU);
         PL (CU, "end To_Servant;");
      end if;

   end Gen_ValueType_Body;

   ------------------------
   -- Gen_Interface_Body --
   ------------------------

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Type_Defining_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "PolyORB.CORBA_P.Exceptions");

         NL (CU);
         PL (CU, "function Unchecked_To_" & Type_Defining_Name);
         Add_With (CU, "CORBA.Object");
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "Result : " & Type_Name & ";");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "Set (Result,");
         PL (CU,
             "     CORBA.Object.Object_Of (The_Ref));");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end Unchecked_To_" & Type_Defining_Name & ";");

         --  To_<reference>

         --    The standard mandates type checking during narrowing
         --    (4.6.2 Narrowing Object References).
         --    Doing the check properly implies either
         --       1. querying the interface repository
         --          (not implemented yet);
         --    or 2. calling Is_A (Repository_Id) on an
         --          object reference whose type maps the actual
         --          (i. e. most derived) interface of The_Ref.
         --          (which is impossible if that type is not
         --          known on the partition where To_Ref is called);
         --    or 3. a remote invocation of an Is_A method of
         --          the object.
         --
         --    The most general and correct solution to this
         --    problem is 3. When a remote call is not desired,
         --    the user should use Unchecked_To_Ref, whose purpose
         --    is precisely that.
         --
         --    This solution is implemented as a dispatching call
         --    to Is_A on the source object reference. The remote
         --    Is_A operation will be invoked if necessary.

         NL (CU);
         PL (CU, "function To_" & Type_Defining_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "use CORBA;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "if CORBA.Object.Is_Nil (The_Ref)");
         PL (CU, "  or else CORBA.Object.Is_A (The_Ref, "
             & Repository_Id_Name (Node) & ") then");
         II (CU);
         PL (CU, "return Unchecked_To_"
             & Type_Defining_Name
             & " (The_Ref);");
         DI (CU);
         PL (CU, "end if;");

         PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Bad_Param;");
         DI (CU);
         PL (CU, "end To_" & Type_Defining_Name & ";");
      end;

      if Generate_Dyn then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Node)
             & " (CORBA.Object.Helper."
             & "From_Any (Item));");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "A : CORBA.Any := CORBA.Object.Helper.To_Any");
         PL (CU, "  (CORBA.Object.Ref (Item));");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Set_Type (A, " & Ada_TC_Name (Node) & ");");
         PL (CU, "return A;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Interface_Body;

   --------------------------------
   -- Gen_Forward_Interface_Body --
   --------------------------------

   procedure Gen_Forward_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "PolyORB.CORBA_P.Exceptions");

         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         Add_With (CU, "CORBA.Object");
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "Result : " & Type_Name & ";");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, Ada_Name (Node) & ".Set (Result,");
         PL (CU,
             "     CORBA.Object.Object_Of (The_Ref));");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end Unchecked_To_" & Short_Type_Name & ";");

         --  To_<reference>
         --  see the corresponding comment in gen_interface_body
         --  if you want more information.

         NL (CU);
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "use CORBA;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "if CORBA.Object.Is_Nil (The_Ref)");
         PL (CU, "  or else CORBA.Object.Is_A (The_Ref, """
             & Idl_Repository_Id (Forward (Node)) & """) then");
         II (CU);
         PL (CU, "return Unchecked_To_"
             & Short_Type_Name
             & " (The_Ref);");
         DI (CU);
         PL (CU, "end if;");

         PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Bad_Param;");
         DI (CU);
         PL (CU, "end To_" & Short_Type_Name & ";");
      end;

      if Generate_Dyn then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Node)
             & " (CORBA.Object.Helper."
             & "From_Any (Item));");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return CORBA.Object.Helper.To_Any "
             & "(CORBA.Object.Ref (Item));");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Forward (Node))
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Forward_Interface_Body;

   -------------------
   -- Gen_Enum_Spec --
   -------------------

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_Enum;");
         DI (CU);

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Enum_Spec;

   -------------------
   -- Gen_Enum_body --
   -------------------

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      if Generate_Dyn then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Index : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element (Item,");
         PL (CU, "                             "
             & "CORBA.TC_Unsigned_Long,");
         PL (CU, "                             "
             & "CORBA.Unsigned_Long (0));");
         DI (CU);
         PL (CU, "Position : CORBA.Unsigned_Long "
             & ":= CORBA.From_Any (Index);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "return "
             & Ada_Name (Node)
             & "'Val (Position);");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         II (CU);
         PL (CU, "(Result,");
         PL (CU, " CORBA.To_Any (CORBA.Unsigned_Long ("
             & Ada_Name (Node)
             & "'Pos (Item))));");
         DI (CU);
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         declare
            It   : Node_Iterator;
            E_Node : Node_Id;
         begin
            Init (It, Enumerators (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, E_Node);
               PL (CU, Ada_Name (E_Node)
                   & "_Name : CORBA.String := CORBA.To_CORBA_String ("""
                   & Ada_Name (E_Node)
                   & """);");
            end loop;
         end;

         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         declare
            It   : Node_Iterator;
            E_Node : Node_Id;
         begin
            Init (It, Enumerators (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, E_Node);
               PL (CU, "CORBA.TypeCode.Add_Parameter ("
                   & Ada_TC_Name (Node)
                   & ", CORBA.To_Any ("
                   & Ada_Name (E_Node)
                   & "_Name));");
            end loop;
         end;
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Enum_Body;

   -------------------------------
   -- Gen_Struct_Exception_Spec --
   -------------------------------

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
      Struct_Node : Node_Id;
   begin
      if Generate_Dyn then
         --  Typecode generation
         Add_With (CU, "CORBA");

         NL (CU);
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         if Kind (Node) = K_Struct then
            PL (CU, "CORBA.TypeCode.TC_Struct;");
         else
            PL (CU, "CORBA.TypeCode.TC_Except;");
         end if;
         DI (CU);

         if Kind (Node) = K_Struct then
            Struct_Node := Node;
         else
            Struct_Node := Members_Type (Node);
         end if;

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Struct_Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Struct_Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Struct_Exception_Spec;

   -------------------------------
   -- Gen_Struct_Exception_Body --
   -------------------------------

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
      Struct_Node : Node_Id;
   begin
      if Generate_Dyn then
         if Kind (Node) = K_Struct then
            Struct_Node := Node;
         else
            Struct_Node := Members_Type (Node);
         end if;

         declare
            Is_Empty : Boolean;
         begin
            Is_Empty := Length (Members (Node)) = 0;

            --  From_Any
            Add_With (CU, "CORBA", Use_It => True);
            NL (CU);
            Gen_From_Any_Profile (CU, Struct_Node);
            PL (CU, " is");
            II (CU);
            if not Is_Empty then
               PL (CU, "Index : CORBA.Any;");
               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Struct_Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);
                     declare
                        It2   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (It2, Decl (Member_Node));
                        while not Is_End (It2) loop
                           Get_Next_Node (It2, Decl_Node);
                           PL (CU, "Result_"
                               & Ada_Name (Decl_Node)
                               & " : "
                               & Ada_Type_Name (M_Type (Member_Node))
                               & ";");
                        end loop;
                     end;
                  end loop;
               end;
            else
               PL (CU, "Result : "
                   & Ada_Name (Struct_Node)
                   & ";");
            end if;
            DI (CU);
            PL (CU, "begin");
            II (CU);
            if Is_Empty then
               PL (CU, "return Result;");
            else
               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
                  I : Integer := 0;
               begin
                  Init (It, Members (Struct_Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);
                     declare
                        It2   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (It2, Decl (Member_Node));
                        while not Is_End (It2) loop
                           Get_Next_Node (It2, Decl_Node);
                           PL (CU,
                               "Index := CORBA.Get_Aggrega"
                               & "te_Element (Item,");
                           Add_With (CU, Ada_Helper_Name
                                     (M_Type (Member_Node)));
                           PL (CU,
                               "                                      "
                               & Ada_Full_TC_Name (M_Type (Member_Node))
                               & ",");
                           PL (CU,
                               "                                      "
                               & "CORBA.Unsigned_Long ("
                               & Integer'Image (I)
                               &"));");
                           Add_With (CU, Ada_Helper_Name
                                     (M_Type (Member_Node)));
                           PL (CU, "Result_"
                               & Ada_Name (Decl_Node)
                               & " := "
                               & Ada_Helper_Name (M_Type (Member_Node))
                               & ".From_Any (Index);");
                           I := I + 1;
                        end loop;
                     end;
                  end loop;
               end;
               PL (CU, "return");
               II (CU);
               declare
                  First_Member : Boolean := True;
                  Begin_Of_Line : String (1 .. 1) := "(";
                  End_Of_Line : String (1 .. 2) := ", ";
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Struct_Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);
                     declare
                        It2   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (It2, Decl (Member_Node));
                        while not Is_End (It2) loop
                           Get_Next_Node (It2, Decl_Node);
                           if Is_End (It) and Is_End (It2) then
                              End_Of_Line := ");";
                           end if;
                           PL (CU, Begin_Of_Line
                               & Ada_Name (Decl_Node)
                               & " => Result_"
                               & Ada_Name (Decl_Node)
                               & End_Of_Line);
                           if First_Member then
                              First_Member := False;
                              Begin_Of_Line := " ";
                           end if;
                        end loop;
                     end;
                  end loop;
               end;
               DI (CU);
            end if;
            DI (CU);
            PL (CU, "end From_Any;");
         end;

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Struct_Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     PL (CU, "CORBA.Add_Aggregate_Element");
                     II (CU);
                     Add_With (CU, Ada_Helper_Name (M_Type (Member_Node)));
                     PL (CU, "(Result, "
                         & Ada_Helper_Name (M_Type (Member_Node))
                         & ".To_Any (Item."
                         & Ada_Name (Decl_Node)
                         & "));");
                     DI (CU);
                  end loop;
               end;
            end loop;
         end;
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     PL (CU, "Arg_Name_"
                         & Ada_Name (Decl_Node)
                         & " : CORBA.String := CORBA.To_CORBA_String ("""
                         & Ada_Name (Decl_Node)
                         & """);");
                  end loop;
               end;
            end loop;
         end;
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     Add_With (CU, Ada_Helper_Name (M_Type (Member_Node)));
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Full_TC_Name (M_Type (Member_Node))
                         & "));");
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any (Arg_Name_"
                         & Ada_Name (Decl_Node)
                         & "));");
                  end loop;
               end;
            end loop;
         end;
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Struct_Exception_Body;

   ------------------------------
   -- Gen_String_Instance_Spec --
   ------------------------------

   procedure Gen_String_Instance_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  Typecode generation
         Add_With (CU, "CORBA");

         NL (CU);
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_String;");
         DI (CU);

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_String_Instance_Spec;

   ------------------------------
   -- Gen_String_Instance_Body --
   ------------------------------

   procedure Gen_String_Instance_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  From_Any
         Add_With (CU, "CORBA");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Result : CORBA.String := CORBA.From_Any (Item);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  This is bad code. To be improved when "
             & "CORBA.Bounded_String will exist");
         PL (CU, "return "
             & Ada_Full_Name (Node)
             & ".To_Bounded_String (Result);");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  This is bad code. To be improved when "
             & "CORBA.Bounded_String will exist");
         PL (CU, "return CORBA.To_Any ("
             & Ada_Full_Name (Node)
             & ".To_String (Item));");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any ("
             & Utils.Img (Expr_Value (Bound (Node)))
             & "));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_String_Instance_Body;

   --------------------
   -- Gen_Union_Spec --
   --------------------

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      if Generate_Dyn then
         --  TypeCode generation

         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_Union;");
         DI (CU);

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>

         Add_Elaborate_Body (CU);
      end if;
   end Gen_Union_Spec;

   --------------------
   -- Gen_Union_Body --
   --------------------

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         Add_With (CU, "CORBA", Use_It => True);
         Add_With (CU, Ada_Helper_Name (Switch_Type (Node)));

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Label_Any : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element (Item,");
         PL (CU, "                             "
             & Ada_Full_TC_Name (Switch_Type (Node)) & ",");
         PL (CU, "                             "
             & "CORBA.Unsigned_Long (0));");
         DI (CU);
         PL (CU, "Label : "
             & Ada_Type_Name (Switch_Type (Node))
             & " := "
             & Ada_Helper_Name (Switch_Type (Node))
             & ".From_Any (Label_Any);");
         PL (CU, "Result : "
             & Ada_Type_Name (Node)
             & " (Label);");
         PL (CU, "Index : CORBA.Any;");
         PL (CU, "I : Natural := 1;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "case Label is");
         II (CU);
         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
            Has_Default : Boolean := False;
         begin
            Init (It, Cases (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);
               declare
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;
               begin
                  if Default_Index (Node) = I then
                     Has_Default := True;
                     Put (CU, "when others");
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        if First_Label then
                           Put (CU, "when ");
                           First_Label := False;
                        else
                           Put (CU, " | ");
                        end if;
                        Gen_Constant_Value (CU, Label_Node);
                     end loop;
                  end if;
                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "Index := CORBA.Get_Aggregate_Element");
                  II (CU);
                  PL (CU, "(Item,");

                  Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));

                  PL (CU, " "
                      & Ada_Full_TC_Name (Case_Type (Case_Node))
                      & ",");
                  PL (CU, " CORBA.Unsigned_Long (I));");
                  I := I + 1;
                  DI (CU);
                  PL (CU, "I := I + 1;");
                  PL (CU, "Result."
                      & Ada_Name (Case_Decl (Case_Node))
                      & " := "
                      & Ada_Helper_Name (Case_Type (Case_Node))
                      & ".From_Any (Index);");
                  DI (CU);
               end;
            end loop;
            if not Has_Default then
               Gen_When_Others_Clause (CU);
            end if;
         end;

         DI (CU);
         PL (CU, "end case;");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         II (CU);
         Add_With (CU, Ada_Helper_Name (Switch_Type (Node)));
         PL (CU, "(Result, "
             & Ada_Helper_Name (Switch_Type (Node))
             & ".To_Any (Item.Switch));");
         DI (CU);
         PL (CU, "case Item.Switch is");
         II (CU);

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
            Has_Default : Boolean := False;
         begin
            Init (It, Cases (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;
               begin
                  if Default_Index (Node) = I then
                     Put (CU, "when others");
                     Has_Default := True;
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        if First_Label then
                           Put (CU, "when ");
                           First_Label := False;
                        else
                           Put (CU, " | ");
                        end if;
                        Gen_Constant_Value (CU, Label_Node);
                     end loop;
                  end if;
                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "CORBA.Add_Aggregate_Element");
                  II (CU);
                  Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                  PL (CU, "(Result, "
                      & Ada_Helper_Name (Case_Type (Case_Node))
                      & ".To_Any (Item."
                      & Ada_Name (Case_Decl (Case_Node))
                      & "));");
                  I := I + 1;
                  DI (CU);
                  DI (CU);
               end;
            end loop;
            if not Has_Default then
               Gen_When_Others_Clause (CU);
            end if;
         end;

         DI (CU);
         PL (CU, "end case;");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
          & """);");

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               PL (CU, "Arg_Name_"
                   & Ada_Name (Case_Decl (Case_Node))
                   & " : CORBA.String := CORBA.To_CORBA_String ("""
                   & Ada_Name (Case_Decl (Case_Node))
                   & """);");
            end loop;
         end;

         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any ("
             & Ada_Full_TC_Name (Switch_Type (Node))
             & "));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (CORBA.Long ("
             & Long_Integer_Img (Default_Index (Node))
          & ")));");

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  It2   : Node_Iterator;
                  Label_Node : Node_Id;
               begin
                  if Default_Index (Node) = I then
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", " & Ada_Helper_Name (Switch_Type (Node))
                         & ".To_Any ("
                         & Ada_Type_Name (Switch_Type (Node))
                         & "'First));");
                     Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Full_TC_Name (Case_Type (Case_Node))
                         & "));");
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any (Arg_Name_"
                         & Ada_Name (Case_Decl (Case_Node))
                      & "));");
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        Put (CU, "CORBA.TypeCode.Add_Parameter ("
                             & Ada_TC_Name (Node)
                             & ", " & Ada_Helper_Name (Switch_Type (Node))
                             & ".To_Any ("
                             & Ada_Type_Name (Switch_Type (Node))
                             & " (");
                        Gen_Constant_Value (CU, Label_Node);
                        PL (CU, ")));");
                        Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ("
                            & Ada_Full_TC_Name (Case_Type (Case_Node))
                            & "));");
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any (Arg_Name_"
                            & Ada_Name (Case_Decl (Case_Node))
                            & "));");
                     end loop;
                  end if;
                  I := I + 1;
               end;
            end loop;
         end;

         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Union_Body;

   ------------------------------
   -- Gen_Type_Declarator_Spec --
   ------------------------------

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Is_Array : constant Boolean
        := Length (Array_Bounds (Node)) > 0;
   begin
      if Generate_Dyn then
         --  TypeCode

         NL (CU);
         Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

         Put (CU, Ada_TC_Name (Node)
              & " : CORBA.TypeCode.Object := CORBA.TypeCode.");
         if Is_Array then
            PL (CU, "TC_Array;");
         else
            PL (CU, "TC_Alias;");
         end if;

         if Is_Array then
            Add_Elaborate_Body (CU);
         end if;

         if not Is_Interface_Type (Node) then
            --  From_Any

            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, ";");

            --  To_Any

            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, ";");
         end if;
      end if;
   end Gen_Type_Declarator_Spec;

   ------------------------------
   -- Gen_Type_Declarator_Body --
   ------------------------------

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Is_Array : constant Boolean
        := Length (Array_Bounds (Node)) > 0;
      Type_Node : constant Node_Id := T_Type (Parent (Node));
      Helper_Name : constant String := Ada_Helper_Name (Type_Node);
   begin
      if Generate_Dyn then
         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");

         if Is_Array then
            for I in 1 .. Length (Array_Bounds (Node)) - 1 loop
               PL (CU, "TC_"
                   & Img (I)
                   & " : CORBA.TypeCode.Object := "
                   & "CORBA.TypeCode.TC_Array;");
            end loop;
         else
            PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Node)
                & """);");
            PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node)
                & """);");
         end if;

         DI (CU);
         PL (CU, "begin");
         II (CU);

         if Is_Array then
            Gen_Array_TC (CU, Type_Node, Node);
         else
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Name));");
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Id));");
            Add_With (CU, Ada_Helper_Name (Type_Node));
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any ("
                & Ada_Full_TC_Name (Type_Node)
                & "));");
         end if;

         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);

         if Is_Interface_Type (Type_Node) then
            return;
         end if;

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);

         Add_With (CU, Ada_Helper_Name (Type_Node));

         if Helper_Name /= "CORBA"
           and then Helper_Name /= "CORBA.Object.Helper"
           and then Helper_Name /= Name (CU)
         then
            Divert (CU, Initialization_Dependencies);
            PL (CU, "& """ & Ada_Helper_Name (Type_Node) & """");
            Divert (CU, Visible_Declarations);
         end if;

         if Is_Array then
            PL (CU, "Result : "
                & Ada_Type_Name (Node)
                & ";");
            DI (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It : Node_Iterator;
               Bound_Node : Node_Id;
               Number : Integer := 0;
            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  Put (CU, "for I"
                       & Img (Number)
                       & " in 0 .. ");
                  Gen_Node_Stubs_Spec (CU, Bound_Node);
                  PL (CU, " - 1 loop");
                  Number := Number + 1;
                  II (CU);
               end loop;

               Put (CU, "Result ");
               for I in 0 .. Number - 1 loop
                  if I = 0 then
                     Put (CU, "(");
                  else
                     Put (CU, ", ");
                  end if;
                  Put (CU, "I" & Img (I));
                  if I = Number - 1 then
                     Put (CU, ")");
                  end if;
               end loop;

               PL (CU, " := "
                   & Ada_Helper_Name (Type_Node)
                   & ".From_Any");
               II (CU);
               Add_With (CU, "CORBA");
               PL (CU, "(CORBA.Get_Aggregate_Element (Item,");
               Add_With (CU, Ada_Helper_Name (Type_Node));
               PL (CU, "                              "
                   & Ada_Full_TC_Name (Type_Node)
                   & ",");
               Put (CU, "                             "
                    & " CORBA.Unsigned_Long (");

               declare
                  Index : Natural := 0;
                  First_Bound : Boolean := True;
               begin
                  Init (Bounds_It, Array_Bounds (Node));

                  while not Is_End (Bounds_It) loop
                     Get_Next_Node (Bounds_It, Bound_Node);

                     if First_Bound then
                        First_Bound := False;
                     else
                        Put (CU, " + ");
                     end if;
                     Put (CU, "I" & Img (Index));
                     for J in Index + 1 .. Number - 1 loop
                        Put (CU, " * ");
                        Gen_Node_Stubs_Spec (CU, Bound_Node);
                     end loop;
                     Index := Index + 1;
                  end loop;
               end;

               PL (CU, ")));");
               DI (CU);
               for I in 1 .. Number loop
                  DI (CU);
                  PL (CU, "end loop;");
               end loop;
            end;
            PL (CU, "return Result;");

         else
            PL (CU, "Result : "
                & Ada_Type_Name (Type_Node)
                & " := "
                & Ada_Helper_Name (Type_Node)
                & ".From_Any (Item);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return "
                & Ada_Type_Name (Node)
                & " (Result);");

         end if;
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);

         if Is_Array then

            Add_With (CU, "CORBA");
            PL (CU, "Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate");
            PL (CU, "  (" & Ada_TC_Name (Node) & ");");
            DI (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It : Node_Iterator;
               Bound_Node : Node_Id;
               Number : Natural := 0;
            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  Put (CU, "for I"
                       & Img (Number)
                       & " in 0 .. ");
                  Gen_Node_Stubs_Spec (CU, Bound_Node);
                  PL (CU, " - 1 loop");
                  Number := Number + 1;
                  II (CU);
               end loop;

               PL (CU, "CORBA.Add_Aggregate_Element (Result,");
               Add_With (CU, Ada_Helper_Name (Type_Node));
               Put (CU, "                             "
                    & Ada_Helper_Name (Type_Node)
                    & ".To_Any (Item (I0");
               for I in 1 .. Number - 1 loop
                  Put (CU, ", I" & Img (I));
               end loop;
               PL (CU, ")));");
               for I in 1 .. Number loop
                  DI (CU);
                  PL (CU, "end loop;");
               end loop;

            end;
            PL (CU, "return Result;");

         else
            Add_With (CU, Ada_Helper_Name (Type_Node));
            Add_With (CU, "CORBA");
            PL (CU, "Result : CORBA.Any := "
                & Ada_Helper_Name (Type_Node)
                & ".To_Any ("
                & Ada_Type_Name (Type_Node)
                & " (Item));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.Set_Type (Result, "
                & Ada_TC_Name (Node)
                & ");");
            PL (CU, "return Result;");
         end if;

         DI (CU);
         PL (CU, "end To_Any;");

      end if;
   end Gen_Type_Declarator_Body;

   -----------------------
   -- Gen_Sequence_Spec --
   -----------------------

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  TypeCode

         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := CORBA.TypeCode.TC_Sequence;");

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>

         Add_Elaborate_Body (CU);
      end if;
   end Gen_Sequence_Spec;

   -----------------------
   -- Gen_Sequence_Body --
   -----------------------

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "use " & Ada_Name (Node) & ";");
         Add_With (CU, "CORBA");
         PL (CU, "Nb_Any : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element");
         PL (CU, "  (Item,");
         II (CU);
         PL (CU, "CORBA.TC_Unsigned_Long,");
         PL (CU, "CORBA.Unsigned_Long (0));");
         DI (CU);
         DI (CU);
         PL (CU, "Nb_Long : CORBA.Unsigned_Long := CORBA.From_Any (Nb_Any);");
         PL (CU, "Nb : Integer := Integer (nb_Long);");
         PL (CU, "Index : CORBA.Any;");
         PL (CU, "Result : Element_Array (1 .. Nb);");
         DI (CU);
         PL (CU, "begin");
         II (CU);

         if Bound (Sequence (Node)) /= No_Node then
            Put (CU, "if Nb > ");
            Gen_Constant_Value (CU, Bound (Sequence (Node)));
            PL (CU, " then");
            II (CU);
            Add_With (CU, "PolyORB.CORBA_P.Exceptions");
            PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Bad_TypeCode;");
            DI (CU);
            PL (CU, "end if;");
         end if;

         PL (CU, "for I in 1 .. Nb loop");
         II (CU);
         PL (CU, "Index :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element (Item,");
         Add_With (CU, Ada_Helper_Name (Sequence_Type (Sequence (Node))));
         PL (CU, "                             "
             & Ada_Full_TC_Name (Sequence_Type (Sequence (Node)))
             & ",");
         PL (CU, "                             CORBA.Unsigned_Long (I));");
         DI (CU);
         Add_With (CU, Ada_Helper_Name (Sequence_Type (Sequence (Node))));
         PL (CU, "Result (I) := "
             & Ada_Helper_Name (Sequence_Type (Sequence (Node)))
             & ".From_Any (Index);");
         DI (CU);
         PL (CU, "end loop;");
         PL (CU, "return To_Sequence (Result);");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "use " & Ada_Name (Node) & ";");
         PL (CU, "Array_Item : Element_Array := To_Element_Array (Item);");
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate");
         PL (CU, "  ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);

         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         PL (CU, "  (Result,");
         II (CU);
         PL (CU, " CORBA.To_Any (CORBA.Unsigned_Long (Length (Item))));");
         DI (CU);
         PL (CU, "for I in Array_Item'Range loop");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         PL (CU, "  (Result,");
         II (CU);
         declare
            Helper : constant String
              := Ada_Helper_Name (Sequence_Type (Sequence (Node)));
         begin
            Add_With (CU, Helper);
            PL (CU, Helper & ".To_Any (Array_Item (I)));");
         end;
         DI (CU);
         DI (CU);
         PL (CU, "end loop;");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         Add_With (CU, "CORBA");
         Put (CU, "CORBA.TypeCode.Add_Parameter ("
              & Ada_TC_Name (Node)
              & ", CORBA.To_Any (CORBA.Unsigned_Long (");
         if Bound (Sequence (Node)) /= No_Node then
            Gen_Constant_Value (CU, Bound (Sequence (Node)));
         else
            Put (CU, "0");
         end if;
         PL (CU, ")));");
         Add_With (CU, Ada_Helper_Name (Sequence_Type (Sequence (Node))));
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any ("
             & Ada_Full_TC_Name (Sequence_Type (Sequence (Node)))
             & "));");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Sequence_Body;

   --------------------
   -- Gen_Fixed_Spec --
   --------------------

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
      Decl_Node : Node_Id := Head (Declarators (Node));
   begin
      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Decl_Node)
             & " : CORBA.TypeCode.Object :="
             & " CORBA.TypeCode.TC_Fixed;");
         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Decl_Node);
         PL (CU, ";");
         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Decl_Node);
         PL (CU, ";");
         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Fixed_Spec;

   --------------------
   -- Gen_Fixed_Body --
   --------------------

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
      Decl_Node : Node_Id := Head (Declarators (Node));
      Type_Name : constant String := Ada_Name (Decl_Node);
   begin
      if Generate_Dyn then
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, "package CDR_"
             & Type_Name & " is");
         Add_With
           (CU, "CORBA.Fixed_Point",
            Elab_Control => Elaborate_All);
         PL (CU, "  new CORBA.Fixed_Point ("
             & Ada_Full_Name (Decl_Node) & ");");
         --  From_Any
         Gen_From_Any_Profile
           (CU, Decl_Node);
         PL (CU, " renames CDR_" & Type_Name
             & ".From_Any;");
         --  To_Any
         Gen_To_Any_Profile
           (CU, Decl_Node);
         PL (CU, " renames CDR_" & Type_Name
             & ".To_Any;");
         --  Fill in typecode TC_<name of the type>
         Divert (CU, Deferred_Initialization);
         NL (CU);
         Put (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Decl_Node)
             & ", CORBA.To_Any (CORBA.Unsigned_Short (");
         Gen_Constant_Value (CU, Digits_Nb (T_Type (Node)));
         PL (CU, ")));");
         Put (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Decl_Node)
             & ", CORBA.To_Any (CORBA.Short (");
         Gen_Constant_Value (CU, Scale (T_Type (Node)));
         PL (CU, ")));");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Fixed_Body;

   ------------------
   -- Gen_Array_TC --
   ------------------

   procedure Gen_Array_TC
     (CU        : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node : in     Node_Id)
   is

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Element_Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id);

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Element_Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id) is
         Bound_Node : Node_Id;
         Last_Bound : Boolean := False;
      begin
         Get_Next_Node (It, Bound_Node);
         if not Is_End (It) then
            Rec_Gen_Array_TC
              (CU, It, False, Index + 1,
               Element_Type_Node, Decl_Node);
         else
            Last_Bound := True;
         end if;
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         Put (CU, ", CORBA.To_Any (CORBA.Unsigned_Long (");
         Gen_Node_Stubs_Spec (CU, Bound_Node);
         PL (CU, ")));");
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         if Last_Bound then
            Put (CU, ", "
                 & "CORBA.To_Any ("
                 & Ada_Full_TC_Name (Element_Type_Node));
         else
            Put (CU, ", To_Any (TC_"
                 & Img (Index + 1));
         end if;
         PL (CU, "));");
      end Rec_Gen_Array_TC;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Add_With (CU, "CORBA");
      Rec_Gen_Array_TC
        (CU, Bounds_It, True, 0, Element_Type_Node, Decl_Node);
   end Gen_Array_TC;

   function Raise_From_Any_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_Exception);
      return "Raise_" & Ada_Name (Node) & "_From_Any";
   end Raise_From_Any_Name;

end Ada_Be.Idl2Ada.Helper;
