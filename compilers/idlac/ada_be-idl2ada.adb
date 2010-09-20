------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

--  This unit contains both generation routines that are
--  of general use to the whole Ada 95 back-end, and specialised
--  routines for the generation of calling stubs.

--  XXX The latter should be moved away to a Ada_Be.Idl2Ada.Stubs
--  child unit one day.

with Ada.Characters.Conversions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Idlac_Flags;           use Idlac_Flags;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Value_Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;
with Ada_Be.Idl2Ada.Skel;
with Ada_Be.Idl2Ada.IR_Info;

with Ada_Be.Mappings;       use Ada_Be.Mappings;
with Ada_Be.Mappings.CORBA; use Ada_Be.Mappings.CORBA;

with Idlac_Errors;          use Idlac_Errors;
with Idlac_Utils;           use Idlac_Utils;

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;

package body Ada_Be.Idl2Ada is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ---------------------------------------------
   -- The current state of the code generator --
   ---------------------------------------------

   type Library_Unit_Data is array (Unit_Kind) of aliased Compilation_Unit;

   procedure New_Library_Unit (Name : String; LU : out Library_Unit_Data);
   --  Create a spec and associated body for the named unit

   type Scope_State is limited record
      Stubs, Skel, Helper, IR_Info, Impl,
        Value_Skel, Delegate : Library_Unit_Data;
   end record;

   type Scope_State_Access is access Scope_State;
   procedure Free is new Ada.Unchecked_Deallocation
     (Scope_State, Scope_State_Access);

   -------------------------------------------------
   -- General purpose code generation subprograms --
   -------------------------------------------------

   procedure Gen_Scope
     (Node          : Node_Id;
      Implement     : Boolean;
      Intf_Repo     : Boolean;
      To_Stdout     : Boolean;
      Current_Scope : Scope_State_Access);
   --  Generate all the files for scope Node.
   --  The implementation templates for interfaces is
   --  generated only if Implement is true.

   procedure Gen_Value_Scope
     (Node      : Node_Id;
      Implement : Boolean;
      Intf_Repo : Boolean;
      To_Stdout : Boolean;
      In_Scope  : Scope_State_Access);

   procedure Gen_Interface_Module_Scope
     (Node      : Node_Id;
      Implement : Boolean;
      Intf_Repo : Boolean;
      To_Stdout : Boolean;
      In_Scope  : Scope_State_Access);

   procedure Gen_ValueType_Stubs_Body
     (CU : in out Compilation_Unit;
      Node : Node_Id);

   procedure Gen_Module_Init_Prelude
     (CU              : in out Compilation_Unit;
      With_Dependency : String := "");

   procedure Gen_Module_Init_Postlude
     (CU : in out Compilation_Unit);

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   function Access_Type_Name (Node : Node_Id) return String;
   --  Generates a name for an access to objet type.
   --  The rule used is to take the ada_type_name, replacing '.' with '_', and
   --  appending "_Access". Should be in expansion, but it would require too
   --  much work to do it now.

   procedure Gen_Repository_Id
     (Node : Node_Id;
      CU   : in out Compilation_Unit);
   --  Generate the RepositoryId for an entity.

   procedure Gen_Is_A
     (Node       : Node_Id;
      Stubs_Spec : in out Compilation_Unit;
      Stubs_Body : in out Compilation_Unit);
   --  Generate code for Repository_Id and Is_A
   --  object reference operation.

   procedure Gen_Local_Impl_Is_A
     (Node      : Node_Id;
      Impl_Spec : in out Compilation_Unit;
      Impl_Body : in out Compilation_Unit);
   --  Generate code for Is_A local object implementation operation.

   procedure Gen_Local_Is_A_Type_Checks
     (Node : Node_Id;
      CU   : in out Compilation_Unit);
   --  Generate a return statement with a list of Logical_Type_Id checks.

   procedure Gen_Client_Stub_Type_Declaration
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the declaration of a client stub type
   --  for an interface or valuetype (in the standard
   --  IDL -> Ada mapping, this is the Ref type.)

   procedure Gen_Object_Servant_Declaration
     (CU        : in out Compilation_Unit;
      Node      : Node_Id;
      Full_View : Boolean);
   --  Generate a template declaration for an object
   --  implementation type. If Full_View is False,
   --  the produced declaration is a private extension
   --  declaration, else it is an extension declaration
   --  with an empty extension.

   procedure Gen_Node_Stubs_Body_Dyn
     (CU   : in out Compilation_Unit;
      Node : Node_Id);

   procedure Gen_Convert_Forward_Declaration
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate package Convert if necessary for
   --  valuetypes and interfaces.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   ----------------------
   -- Conditional_Call --
   ----------------------

   function Conditional_Call
     (Func      : String;
      Only_When : Boolean;
      Expr      : String) return String is
   begin
      if Only_When then
         return Func & " (" & Expr & ")";
      else
         return Expr;
      end if;
   end Conditional_Call;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Use_Mapping : Ada_Be.Mappings.Mapping_Type'Class;
      Node        : Node_Id;
      Implement   : Boolean                            := False;
      Intf_Repo   : Boolean                            := False;
      To_Stdout   : Boolean                            := False)
   is
      S_Node : Node_Id;
      It : Node_Iterator;
   begin
      pragma Assert (Is_Repository (Node));

      Mapping := new Mappings.CORBA.CORBA_Mapping_Type'Class'
                       (CORBA_Mapping_Type'Class (Use_Mapping));

      Init (It, Contents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, S_Node);
         if Generate_Code (S_Node) then
            Gen_Scope (S_Node, Implement, Intf_Repo, To_Stdout,
                       Current_Scope => null);
         end if;
      end loop;

   end Generate;

   ---------------
   -- Gen_Scope --
   ---------------

   procedure Gen_Scope
     (Node          : Node_Id;
      Implement     : Boolean;
      Intf_Repo     : Boolean;
      To_Stdout     : Boolean;
      Current_Scope : Scope_State_Access)
   is
      In_Scope : Scope_State_Access := null;
   begin
      if Code_Generation_Suppressed (Mapping, Node) then
         declare
            It     : Node_Iterator;
            S_Node : Node_Id;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, S_Node);
               if Is_Gen_Scope (S_Node)
                 and then Generate_Scope_In_Child_Package (Mapping, S_Node)
               then
                  Gen_Scope
                    (S_Node, Implement, Intf_Repo, To_Stdout, Current_Scope);
               end if;
            end loop;

            return;
         end;
      end if;

      if not Generate_Scope_In_Child_Package (Mapping, Node) then
         In_Scope := Current_Scope;
      end if;

      case Kind (Node) is
         when K_ValueType =>
            Gen_Value_Scope
              (Node, Implement, Intf_Repo, To_Stdout, In_Scope);

         when
           K_Ben_Idl_File |
           K_Module       |
           K_Interface    =>
            Gen_Interface_Module_Scope
              (Node, Implement, Intf_Repo, To_Stdout, In_Scope);

         when others =>
            raise Program_Error;
            --  Should never happen
      end case;
   end Gen_Scope;

   procedure Initialize_Scope_State
     (Stubs_Name      :     String;
      Skel_Name       :     String;
      Helper_Name     :     String;
      IR_Info_Name    :     String;
      Impl_Name       :     String;
      Value_Skel_Name :     String;
      Delegate_Name   :     String;
      St              : out Scope_State);

   procedure Initialize_Scope_State
     (Stubs_Name      :     String;
      Skel_Name       :     String;
      Helper_Name     :     String;
      IR_Info_Name    :     String;
      Impl_Name       :     String;
      Value_Skel_Name :     String;
      Delegate_Name   :     String;
      St              : out Scope_State)
   is
      pragma Warnings (Off, St);
      --  Never assigned a (global) value, but all components are assigned

   begin
      New_Library_Unit (Stubs_Name,      St.Stubs);
      New_Library_Unit (Skel_Name,       St.Skel);
      New_Library_Unit (Helper_Name,     St.Helper);
      New_Library_Unit (Skel_Name,       St.Skel);
      New_Library_Unit (IR_Info_Name,    St.IR_Info);
      New_Library_Unit (Impl_Name,       St.Impl);
      New_Library_Unit (Value_Skel_Name, St.Value_Skel);
      New_Library_Unit (Delegate_Name,   St.Delegate);
   end Initialize_Scope_State;

   ---------------------
   -- Gen_Value_Scope --
   ---------------------

   procedure Gen_Value_Scope
     (Node      : Node_Id;
      Implement : Boolean;
      Intf_Repo : Boolean;
      To_Stdout : Boolean;
      In_Scope  : Scope_State_Access)
   is
      Stubs_Name : constant String
        := Client_Stubs_Unit_Name (Mapping, Node);
      Skel_Name : constant String
        := Server_Skel_Unit_Name (Mapping, Node);

      Impl_Name : constant String
        := Stubs_Name & Value_Impl.Suffix;
      Helper_Name : constant String
        := Stubs_Name & Helper.Suffix;
      IR_Info_Name : constant String
        := Stubs_Name & IR_Info.Suffix;
      Value_Skel_Name : constant String
        := Stubs_Name & Value_Skel.Suffix;

      S : Scope_State_Access;
   begin
      if In_Scope = null then
         S := new Scope_State;

         Initialize_Scope_State
           (Stubs_Name,
            Skel_Name,
            Helper_Name,
            IR_Info_Name,
            Impl_Name,
            Value_Skel_Name => Value_Skel_Name,
            Delegate_Name   => "",
            St              => S.all);
      else
         S := In_Scope;
      end if;

      if In_Scope = null then
         Gen_Module_Init_Prelude
           (S.Helper (Unit_Body), With_Dependency => "any");
         Gen_Module_Init_Prelude (S.Skel (Unit_Body));
         Gen_Module_Init_Prelude (S.Stubs (Unit_Body));

         if Intf_Repo then
            IR_Info.Gen_Spec_Prelude (S.IR_Info (Unit_Spec));
            IR_Info.Gen_Body_Prelude (S.IR_Info (Unit_Body));
         end if;
      end if;

      --  ValueType reference type

      Gen_Client_Stub_Type_Declaration
        (S.Stubs (Unit_Spec), Node);
      Gen_Repository_Id (Node, S.Stubs (Unit_Spec));

      if not Abst (Node) then
         NL (S.Stubs (Unit_Spec));
         PL (S.Stubs (Unit_Spec), "Null_Value : constant Value_Ref;");
         Divert (S.Stubs (Unit_Spec), Private_Declarations);
         PL (S.Stubs (Unit_Spec), "Null_Value : constant Value_ref");
         II (S.Stubs (Unit_Spec));
         Add_With (S.Stubs (Unit_Spec), "CORBA.AbstractBase");
         PL (S.Stubs (Unit_Spec),
             " := (CORBA.AbstractBase.Nil_Ref with null record);");
         DI (S.Stubs (Unit_Spec));
         Divert (S.Stubs (Unit_Spec), Visible_Declarations);
      end if;

      if not Abst (Node) then
         --  Value_Impl type
         Value_Impl.Gen_Node_Spec (S.Impl (Unit_Spec), Node);
         Value_Impl.Gen_Node_Body (S.Impl (Unit_Body), Node);
         Suppress_Warning_Message (S.Impl (Unit_Body));

         --  value_skel package
         Value_Skel.Gen_Node_Spec (S.Value_Skel (Unit_Spec), Node);
         Value_Skel.Gen_Node_Body (S.Value_Skel (Unit_Body), Node);
      end if;

      Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Node);
      Helper.Gen_Node_Body (S.Helper (Unit_Body), Node);

      if Intf_Repo then
         IR_Info.Gen_Node_Spec (S.IR_Info (Unit_Spec), Node);
         IR_Info.Gen_Node_Body (S.IR_Info (Unit_Body), Node);
      end if;

      --  Skel package
      Skel.Gen_Node_Spec
        (S.Skel (Unit_Spec), Node, Is_Delegate => False);
      Skel.Gen_Node_Body
        (S.Skel (Unit_Body), Node, Is_Delegate => False);

      --  generate code for node content
      declare
         It   : Node_Iterator;
         Export_Node : Node_Id;
      begin
         Init (It, Contents (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, Export_Node);

            pragma Debug (O ("valuetype content node: "
                             & Node_Kind'Image (Kind (Export_Node))));

            if Is_Gen_Scope (Export_Node) then
               Gen_Scope
                 (Export_Node, Implement, Intf_Repo, To_Stdout,
                  Current_Scope => S);
            else
               Gen_Node_Stubs_Spec (S.Stubs (Unit_Spec), Export_Node);
               Gen_ValueType_Stubs_Body (S.Stubs (Unit_Body), Export_Node);

               --  Value_Skel packages

               Value_Skel.Gen_Node_Spec
                 (S.Value_Skel (Unit_Spec), Export_Node);

               if not Abst (Node) then
                  Value_Skel.Gen_Node_Body
                    (S.Value_Skel (Unit_Body), Export_Node);
                  Value_Impl.Gen_Node_Spec
                    (S.Impl (Unit_Spec), Export_Node);
                  Value_Impl.Gen_Node_Body
                    (S.Impl (Unit_Body), Export_Node);

                  --  Skel package

                  if Kind (Export_Node) = K_Operation
                    and then Is_Directly_Supported (Export_Node) then
                     Skel.Gen_Node_Body
                       (S.Skel (Unit_Body), Export_Node,
                        Is_Delegate => False);
                  end if;
               end if;

               Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Export_Node);
               Helper.Gen_Node_Body (S.Helper (Unit_Body), Export_Node);
            end if;

            --  Methods inherited from parents other that
            --  the first one are added to the interface or
            --  valuetype's exports list by the expander.

         end loop;
      end;

      Gen_Convert_Forward_Declaration (S.Stubs (Unit_Spec), Node);

      if In_Scope /= null then
         return;
      end if;

      Gen_Module_Init_Postlude (S.Helper (Unit_Body));
      Gen_Module_Init_Postlude (S.Skel (Unit_Body));
      Gen_Module_Init_Postlude (S.Stubs (Unit_Body));
      Add_Elaborate_Body (S.Skel (Unit_Spec), S.Skel (Unit_Body));

      if Intf_Repo then
         IR_Info.Gen_Body_Postlude (S.IR_Info (Unit_Body));
      end if;

      if not Is_Empty (Supports (Node)) then
         Skel.Gen_Body_Common_End
           (S.Skel (Unit_Body), Node, Is_Delegate => False);
      end if;

      if Implement then
         Generate (S.Impl (Unit_Spec), False, To_Stdout);
         Generate (S.Impl (Unit_Body), False, To_Stdout);
      else
         Generate (S.Stubs (Unit_Spec), False, To_Stdout);
         Generate (S.Stubs (Unit_Body), False, To_Stdout);
         Generate (S.Helper (Unit_Spec), False, To_Stdout);
         Generate (S.Helper (Unit_Body), False, To_Stdout);
         if Intf_Repo then
            Generate (S.IR_Info (Unit_Spec), False, To_Stdout);
            Generate (S.IR_Info (Unit_Body), False, To_Stdout);
         end if;
         Generate (S.Value_Skel (Unit_Spec), False, To_Stdout);
         Generate (S.Value_Skel (Unit_Body), False, To_Stdout);
         Generate (S.Skel (Unit_Spec), False, To_Stdout);
         Generate (S.Skel (Unit_Body), False, To_Stdout);
      end if;

      if In_Scope = null then
         Free (S);
      end if;
   end Gen_Value_Scope;

   ------------------------------
   -- Gen_ValueType_Stubs_Body --
   ------------------------------

   procedure Gen_ValueType_Stubs_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is
         when K_Operation =>
            declare
               Op_Name : constant String := Ada_Operation_Name (Node);
               Is_Function : constant Boolean
                 := Kind (Operation_Type (Node)) /= K_Void;
               Original_VT_Name : constant String
                 := Ada_Full_Name
                 (Oldest_Supporting_ValueType (Node));
               Self_Expr : constant String :=
                             Self_For_Operation (Mapping, Node);
            begin
               if not Is_Implicit_Inherited (Node) then
                  Add_With (CU, Parent_Scope_Name (Node)
                            & Value_Skel.Suffix);
                  Add_With (CU, "CORBA.Impl");
                  Gen_Operation_Profile
                    (CU, Node,
                     Ada_Type_Defining_Name
                     (Mapping, Parent_Scope (Node)));
                  PL (CU, " is");
                  II (CU);
                  Add_With (CU,
                            Original_VT_Name
                            & Value_Skel.Suffix);
                  PL (CU,
                      Ada_Be.Temporaries.T_Value_Operation
                      & " : "
                      & Original_VT_Name
                      & Value_Skel.Suffix
                      & "."
                      & Ada_Operation_Name (Node)
                      & "_Type;");
                  PL (CU,
                      Ada_Be.Temporaries.T_Impl_Object_Ptr
                      & " : constant CORBA.Impl.Object_Ptr");
                  PL (CU, "  := CORBA.Impl.Object_Ptr (Object_Of");
                  II (CU);
                  PL (CU, "(" & Self_Expr & "));");
                  DI (CU);
                  DI (CU);
                  PL (CU, "begin");
                  II (CU);
                  PL (CU, "--  Sanity check");
                  PL (CU, "if Is_Nil (" & Self_Expr & ") then");
                  II (CU);
                  Add_With (CU, "CORBA");
                  PL (CU, "CORBA.Raise_Inv_Objref (Default_Sys_Member);");
                  DI (CU);
                  PL (CU, "end if;");
                  NL (CU);

                  PL (CU, "--  Find the operation");
                  PL (CU,
                      Ada_Be.Temporaries.T_Value_Operation
                      & " := "
                      & Original_VT_Name
                      & Value_Skel.Suffix
                      & "."
                      & Op_Name
                      & "_Store.Get_Operation ("
                      & Ada_Be.Temporaries.T_Impl_Object_Ptr
                      & ".all'Tag);");

                  NL (CU);
                  PL (CU, "--  Call it operation");
                  if Is_Function then
                     PL (CU, "return");
                     II (CU);
                  end if;
                  PL (CU, Ada_Be.Temporaries.T_Value_Operation);
                  Put (CU, "  (" & Ada_Be.Temporaries.T_Impl_Object_Ptr);
                  II (CU);

                  --  The remaining formals

                  declare
                     It : Node_Iterator;
                     Param_Node : Node_Id;
                  begin
                     Init (It, Parameters (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, Param_Node);

                        PL (CU, ",");
                        Put (CU, Ada_Name (Declarator (Param_Node)));
                     end loop;
                  end;

                  PL (CU, ");");

                  DI (CU);
                  if  Is_Function then
                     DI (CU);
                  end if;

                  DI (CU);
                  NL (CU);
                  PL (CU, "end " & Op_Name & ";");
               end if;
            end;

            when K_Initializer =>
               Gen_Initializer_Profile (CU,
                                        "Value_Ref'Class",
                                        Node);
               PL (CU, " is");
               II (CU);
               PL (CU, "Result : Value_Ref;");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               Add_With (CU,
                         Parent_Scope_Name (Node)
                         & Value_Impl.Suffix);
               PL (CU, "Set");
               PL (CU, "  (Result,");
               PL (CU, "   CORBA.Impl.Object_Ptr");
               Put (CU, "   ("
                    & Parent_Scope_Name (Node)
                    & Value_Impl.Suffix & "."
                    & Ada_Name (Node));

               --  The formal parameters
               declare
                  It : Node_Iterator;
                  Param_Node : Node_Id;
                  Is_First : Boolean := True;
               begin
                  Init (It, Param_Decls (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Param_Node);
                     if Is_First then
                        NL (CU);
                        Put (CU, "    (");
                        Is_First := False;
                     else
                        PL (CU, ",");
                        Put (CU, "     ");
                     end if;
                     Put (CU, Ada_Name (Declarator (Param_Node)));
                  end loop;
                  if not Is_First then
                     Put (CU, ")");
                  end if;
                  PL (CU, "));");
               end;

               PL (CU, "return Result;");
               DI (CU);
               PL (CU, "end "
                   & Ada_Name (Node)
                   & ";");

            when others =>
               null;
      end case;
   end Gen_ValueType_Stubs_Body;

   --------------------------------
   -- Gen_Interface_Module_Scope --
   --------------------------------

   procedure Gen_Interface_Module_Scope
     (Node      : Node_Id;
      Implement : Boolean;
      Intf_Repo : Boolean;
      To_Stdout : Boolean;
      In_Scope  : Scope_State_Access)
   is
      Stubs_Name    : constant String
        := Client_Stubs_Unit_Name (Mapping, Node);

      Skel_Name     : constant String
        := Server_Skel_Unit_Name (Mapping, Node);
      Skel_Required : constant Boolean :=
        not ((Kind (Node) = K_Interface and then Local (Node))
              or else Kind (Node) = K_Ben_Idl_File);

      Impl_Name     : constant String := Stubs_Name & Impl.Suffix;
      Helper_Name   : constant String := Stubs_Name & Helper.Suffix;
      Delegate_Name : constant String
        := Stubs_Name & Skel.Suffix (Is_Delegate => True);
      IR_Info_Name  : constant String := Stubs_Name & IR_Info.Suffix;

      S : Scope_State_Access;

   begin
      if In_Scope = null then
         S := new Scope_State;

         Initialize_Scope_State
           (Stubs_Name,
            Skel_Name,
            Helper_Name,
            IR_Info_Name,
            Impl_Name,
            Value_Skel_Name => "",
            Delegate_Name   => Delegate_Name,
            St              => S.all);
      else
         S := In_Scope;
      end if;

      if In_Scope = null then

         --  Really starting a new gen scope

         Gen_Module_Init_Prelude
           (S.Helper (Unit_Body), With_Dependency => "any");
         if Skel_Required then
            Gen_Module_Init_Prelude (S.Skel (Unit_Body));
            Gen_Module_Init_Prelude (S.Stubs (Unit_Body));
         end if;

         if Intf_Repo then
            IR_Info.Gen_Spec_Prelude (S.IR_Info (Unit_Spec));
            IR_Info.Gen_Body_Prelude (S.IR_Info (Unit_Body));
         end if;
      end if;

      case Kind (Node) is
         when K_ValueType =>
            raise Program_Error;
            --  Handled by Gen_Value_Scope

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

                     if Generate_Client_Code then
                        --  Ensure current unit has a non-empty spec, if the
                        --  mapping prescribes that it has a child package.

                        if Kind (Node) /= K_Repository
                          and then Kind (Node) /= K_Ben_Idl_File
                        then
                           NL (S.Stubs (Unit_Spec));
                           Put (S.Stubs (Unit_Spec), "--  ");
                           case Kind (Decl_Node) is
                              when K_Module =>
                                 Put (S.Stubs (Unit_Spec), "Module ");
                              when K_Interface =>
                                 Put (S.Stubs (Unit_Spec), "Interface ");
                              when K_ValueType =>
                                 Put (S.Stubs (Unit_Spec), "ValueType ");
                              when others =>
                                 --  Never happens
                                 raise Program_Error;
                           end case;
                           PL (S.Stubs (Unit_Spec), Name (Decl_Node));
                        end if;
                     end if;

                     Gen_Scope
                       (Decl_Node, Implement, Intf_Repo, To_Stdout,
                        Current_Scope => S);

                  else
                     if Generate_Client_Code then
                        if Kind (Decl_Node) = K_Forward_Interface then
                           Helper.Gen_Forward_Interface_Spec
                             (S.Helper (Unit_Spec), Decl_Node);
                           Helper.Gen_Forward_Interface_Body
                             (S.Helper (Unit_Body), Decl_Node);
                        end if;

                        Gen_Node_Stubs_Spec
                          (S.Stubs (Unit_Spec), Decl_Node);
                        Gen_Node_Stubs_Body_Dyn
                          (S.Stubs (Unit_Body), Decl_Node);

                        --  Exception declarations cause generation of
                        --  Get_Members procedure.

                        Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Decl_Node);
                        Helper.Gen_Node_Body (S.Helper (Unit_Body), Decl_Node);

                        if Intf_Repo then
                           IR_Info.Gen_Node_Spec
                             (S.IR_Info (Unit_Spec), Decl_Node);
                           IR_Info.Gen_Node_Body
                             (S.IR_Info (Unit_Body), Decl_Node);
                        end if;
                     end if;
                  end if;
               end loop;

               if Kind (Node) = K_Module
                 and then Generate_Client_Code
               then
                  Gen_Repository_Id (Node, S.Stubs (Unit_Spec));

                  if Intf_Repo then
                     IR_Info.Gen_Node_Spec (S.IR_Info (Unit_Spec), Node);
                     IR_Info.Gen_Node_Body (S.IR_Info (Unit_Body), Node);
                  end if;
               end if;
            end;

         when K_Interface =>

            --  Object reference type

            if Generate_Client_Code then
               Gen_Client_Stub_Type_Declaration (S.Stubs (Unit_Spec), Node);

               Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Node);
               Helper.Gen_Node_Body (S.Helper (Unit_Body), Node);
            end if;

            if not Abst (Node) then

               if not Local (Node)
                 and then Generate_Server_Code
               then
                  Skel.Gen_Node_Spec
                    (S.Skel (Unit_Spec), Node, Is_Delegate => False);
                  Skel.Gen_Node_Body
                    (S.Skel (Unit_Body), Node, Is_Delegate => False);
               end if;

               --  Delegate package

               if Generate_Delegate then
                  NL (S.Delegate (Unit_Body));
                  PL (S.Delegate (Unit_Body),
                      "function Create (From : access Wrapped) " &
                      "return Object_Ptr");
                  PL (S.Delegate (Unit_Body), "is");
                  PL (S.Delegate (Unit_Body),
                      "   Result : constant Object_Ptr := new Object;");
                  PL (S.Delegate (Unit_Body), "begin");
                  PL (S.Delegate (Unit_Body),
                      "   Result.Real := From.all'Unchecked_Access;");
                  PL (S.Delegate (Unit_Body), "   return Result;");
                  PL (S.Delegate (Unit_Body), "end Create;");

                  Skel.Gen_Node_Spec
                    (S.Delegate (Unit_Spec), Node, Is_Delegate => True);
                  Skel.Gen_Node_Body
                    (S.Delegate (Unit_Body), Node, Is_Delegate => True);

                  Add_With (S.Delegate (Unit_Spec), "PortableServer");
                  NL (S.Delegate (Unit_Spec));
                  PL (S.Delegate (Unit_Spec), "type Object (<>) is");
                  PL (S.Delegate (Unit_Spec),
                      "  new PortableServer.Servant_Base with private;");
                  PL (S.Delegate (Unit_Spec),
                      "type Object_Ptr is access all Object'Class;");
                  NL (S.Delegate (Unit_Spec));
                  PL (S.Delegate (Unit_Spec),
                      "function Create (From : access Wrapped) " &
                      "return Object_Ptr;");
                  Divert (S.Delegate (Unit_Spec), Private_Declarations);
                  NL (S.Delegate (Unit_Spec));
                  PL (S.Delegate (Unit_Spec),
                      "type Wrapped_Access is access all Wrapped;");
                  NL (S.Delegate (Unit_Spec));
                  PL (S.Delegate (Unit_Spec),
                      "type Object is " &
                      "new PortableServer.Servant_Base with record");
                  PL (S.Delegate (Unit_Spec),
                      "   Real : Wrapped_Access;");
                  PL (S.Delegate (Unit_Spec),
                      "end record;");
                  Divert (S.Delegate (Unit_Spec), Generic_Formals);
                  PL (S.Delegate (Unit_Spec),
                      "type Wrapped is limited private;");
                  Divert (S.Delegate (Unit_Spec), Visible_Declarations);
               end if;

               if Implement then
                  Divert (S.Impl (Unit_Spec), Private_Declarations);
                  Gen_Object_Servant_Declaration
                    (S.Impl (Unit_Spec), Node,
                     Full_View => True);

                  Divert (S.Impl (Unit_Spec), Visible_Declarations);
                  Gen_Object_Servant_Declaration
                    (S.Impl (Unit_Spec), Node,
                     Full_View => False);
                  --  The template object implementation type.

                  Suppress_Warning_Message (S.Impl (Unit_Spec));
                  Suppress_Warning_Message (S.Impl (Unit_Body));

                  if not Local (Node) then
                     Add_With (S.Impl (Unit_Body), Skel_Name,
                               Use_It => False,
                               Elab_Control => Elaborate,
                               No_Warnings => True);
                  end if;
               end if;
            end if;

            if Generate_Client_Code then
               Gen_Node_Stubs_Body_Dyn (S.Stubs (Unit_Body), Node);
            end if;

            declare
               It   : Node_Iterator;
               Export_Node : Node_Id;
            begin
               Init (It, Contents (Node));

               while not Is_End (It) loop
                  Get_Next_Node (It, Export_Node);

                  if Is_Gen_Scope (Export_Node) then
                     Gen_Scope
                       (Export_Node, Implement, Intf_Repo,
                        To_Stdout, Current_Scope => S);

                  else
                     if Generate_Client_Code then
                        Gen_Node_Stubs_Spec
                          (S.Stubs (Unit_Spec), Export_Node);
                        Gen_Node_Stubs_Body_Dyn
                          (S.Stubs (Unit_Body), Export_Node);
                     end if;

                     --  No code produced per-node in skeleton spec

                     if not Abst (Node) then

                        if not Local (Node)
                          and then Generate_Server_Code
                        then
                           Skel.Gen_Node_Body
                             (S.Skel (Unit_Body), Export_Node,
                              Is_Delegate => False);
                        end if;

                        if Generate_Delegate then
                           Divert (S.Delegate (Unit_Spec), Generic_Formals);
                           Impl.Gen_Node_Spec
                             (S.Delegate (Unit_Spec),
                              Export_Node, Is_Delegate => True);

                           Divert
                             (S.Delegate (Unit_Spec), Visible_Declarations);
                           Skel.Gen_Node_Body
                             (S.Delegate (Unit_Body),
                              Export_Node, Is_Delegate => True);
                        end if;

                        if Implement then
                           Impl.Gen_Node_Spec
                             (S.Impl (Unit_Spec), Export_Node);
                           Impl.Gen_Node_Body
                             (S.Impl (Unit_Body), Export_Node);
                        end if;
                     end if;

                     if Generate_Client_Code then
                        Helper.Gen_Node_Spec
                          (S.Helper (Unit_Spec), Export_Node);
                        Helper.Gen_Node_Body
                          (S.Helper (Unit_Body), Export_Node);
                     end if;

                     if Intf_Repo then
                        IR_Info.Gen_Node_Spec
                          (S.IR_Info (Unit_Spec), Export_Node);
                        IR_Info.Gen_Node_Body
                          (S.IR_Info (Unit_Body), Export_Node);
                     end if;
                  end if;

                  --  Methods inherited from parents other that the first one
                  --  are added to the interface's exports list by the
                  --  expander.

               end loop;
            end;

            if Implement and then Local (Node) then
               Gen_Local_Impl_Is_A
                 (Node, S.Impl (Unit_Spec), S.Impl (Unit_Body));
            end if;

            if Generate_Client_Code then
               Gen_Repository_Id (Node, S.Stubs (Unit_Spec));

               if not Local (Node) then
                  Gen_Is_A (Node, S.Stubs (Unit_Spec), S.Stubs (Unit_Body));
                  Gen_Local_Is_A (S.Stubs (Unit_Body), Node);
               end if;
            end if;

            if Intf_Repo then
               IR_Info.Gen_Node_Spec (S.IR_Info (Unit_Spec), Node);
               IR_Info.Gen_Node_Body (S.IR_Info (Unit_Body), Node);
            end if;

            if Generate_Client_Code then
               Gen_Convert_Forward_Declaration (S.Stubs (Unit_Spec), Node);
            end if;

            if not Abst (Node) then
               if not Local (Node)
                 and then Generate_Server_Code
               then
                  Skel.Gen_Body_Common_End
                    (S.Skel (Unit_Body), Node, Is_Delegate => False);
               end if;

               if Generate_Delegate then
                  Skel.Gen_Body_Common_End
                    (S.Delegate (Unit_Body), Node,
                     Is_Delegate => True);
               end if;
            end if;

         when others =>
            --  This never happens

            pragma Assert (False);
            null;
      end case;

      if In_Scope /= null then
         return;
      end if;

      Gen_Module_Init_Postlude (S.Helper (Unit_Body));

      --  Local objects do not have a skeleton

      if Skel_Required then
         Gen_Module_Init_Postlude (S.Stubs (Unit_Body));
         Gen_Module_Init_Postlude (S.Skel (Unit_Body));
         Add_Elaborate_Body (S.Skel (Unit_Spec), S.Skel (Unit_Body));
      end if;

      if Intf_Repo then
         IR_Info.Gen_Body_Postlude (S.IR_Info (Unit_Body));
      end if;

      if Kind (Node) = K_Ben_Idl_File
        and then Is_Unknown (Node)
      then
         --  Do not attempt to generate a 'file' scope if there was no actual
         --  IDL file (case of a tree that is synthetised from a DSA service
         --  specification, for example).
         return;
      end if;

      declare
         Is_Abstract_Node : Boolean := False;
         --  No skel and impl packages are generated for abstract interfaces

      begin
         if Kind (Node) = K_Interface then
            Is_Abstract_Node := Abst (Node);
         end if;

         if Implement then
            if not Is_Abstract_Node then
               Generate (S.Impl (Unit_Spec), False, To_Stdout);
               Generate (S.Impl (Unit_Body), False, To_Stdout);
            end if;
         else
            Generate (S.Stubs (Unit_Spec), False, To_Stdout);
            Generate (S.Stubs (Unit_Body), False, To_Stdout);
            Generate (S.Helper (Unit_Spec), False, To_Stdout);
            Generate (S.Helper (Unit_Body), False, To_Stdout);
            if Intf_Repo then
               Generate (S.IR_Info (Unit_Spec), False, To_Stdout);
               Generate (S.IR_Info (Unit_Body), False, To_Stdout);
            end if;
            if not Is_Abstract_Node then
               Generate (S.Skel (Unit_Spec), False, To_Stdout);
               Generate (S.Skel (Unit_Body), False, To_Stdout);
               if Generate_Delegate then
                  Generate (S.Delegate (Unit_Spec), False, To_Stdout);
                  Generate (S.Delegate (Unit_Body), False, To_Stdout);
               end if;
            end if;
         end if;
      end;

      Free (S);
   end Gen_Interface_Module_Scope;

   -------------------------
   --  Gen_Repository_Id  --
   -------------------------

   procedure Gen_Repository_Id
     (Node : Node_Id;
      CU   : in out Compilation_Unit)
   is
   begin
      Add_With (CU, "PolyORB.Std");
      NL (CU);
      PL (CU, Repository_Id_Name (Node)
          & " : constant PolyORB.Std.String");
      PL (CU, "  := """ & Idl_Repository_Id (Node) & """;");
   end Gen_Repository_Id;

   --------------
   -- Gen_Is_A --
   --------------

   procedure Gen_Is_A
     (Node       : Node_Id;
      Stubs_Spec : in out Compilation_Unit;
      Stubs_Body : in out Compilation_Unit) is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      pragma Assert (NK = K_Interface);
      --  Declaration

      Add_With (Stubs_Spec, "CORBA",
                Use_It => False,
                Elab_Control => Elaborate_All);

      NL (Stubs_Spec);
      PL (Stubs_Spec, "function Is_A");
      PL (Stubs_Spec, "  (Self : "
          & Ada_Type_Defining_Name (Mapping, Node)
          & ";");
      PL (Stubs_Spec, "   Logical_Type_Id : PolyORB.Std.String)");
      PL (Stubs_Spec, "  return CORBA.Boolean;");

      Divert (Stubs_Spec, Private_Declarations);
      NL (Stubs_Spec);
      PL (Stubs_Spec, "function Is_A");
      PL (Stubs_Spec, "  (Logical_Type_Id : PolyORB.Std.String)");
      PL (Stubs_Spec, "  return CORBA.Boolean;");
      Divert (Stubs_Spec, Visible_Declarations);

      --  Implementation

      NL (Stubs_Body);
      PL (Stubs_Body, "--  The visible Is_A object reference");
      PL (Stubs_Body, "--  operation (a dispatching operation");
      PL (Stubs_Body, "--  of all object reference types).");
      NL (Stubs_Body);
      PL (Stubs_Body, "function Is_A");
      PL (Stubs_Body, "  (Self : "
          & Ada_Type_Defining_Name (Mapping, Node)
          & ";");
      PL (Stubs_Body, "   Logical_Type_Id : PolyORB.Std.String)");
      PL (Stubs_Body, "  return CORBA.Boolean");
      PL (Stubs_Body, "is");
      PL (Stubs_Body, "begin");
      II (Stubs_Body);
      PL (Stubs_Body, "return False");
      II (Stubs_Body);
      PL (Stubs_Body,
          "--  Locally check class membership for this interface");
      DI (Stubs_Body);
      NL (Stubs_Body);
      PL (Stubs_Body, "  or else Is_A (Logical_Type_Id)");

      Add_With (Stubs_Body, "CORBA.Object");
      II (Stubs_Body);
      PL (Stubs_Body,
          "--  Fall back to a remote membership check (may involve");
      PL (Stubs_Body,
          "--  an actual request invocation on Self).");
      NL (Stubs_Body);
      PL (Stubs_Body, "  or else CORBA.Object.Is_A");
      PL (Stubs_Body,
          "           (CORBA.Object.Ref (Self), Logical_Type_Id);");
      DI (Stubs_Body);

      NL (Stubs_Body);
      DI (Stubs_Body);
      PL (Stubs_Body, "end Is_A;");
   end Gen_Is_A;

   --------------------------------
   -- Gen_Local_Is_A_Type_Checks --
   --------------------------------

   procedure Gen_Local_Is_A_Type_Checks
     (Node : Node_Id;
      CU   : in out Compilation_Unit)
   is
   begin
      --  An instance of a type verifies Is_A for that type...

      Add_With (CU, "CORBA");

      PL (CU, "return CORBA.Is_Equivalent");
      PL (CU, "  (Logical_Type_Id,");
      II (CU);
      PL (CU, Ada_Full_Name (Node) & "."
          & Repository_Id_Name (Node) & ")");
      DI (CU);
      PL (CU, "  or else CORBA.Is_Equivalent");
      PL (CU, "    (Logical_Type_Id,");

      --  ... and for CORBA::Object (if it is an interface) or
      --  CORBA::ValueBase (if it is a valuetype), either
      --  of which is at the root of the instance type's inheritance
      --  hierarchy

      if Kind (Node) = K_Interface then
         PL (CU, "     ""IDL:omg.org/CORBA/Object:1.0"")");
      else
         PL (CU, "     ""IDL:omg.org/CORBA/ValueBase:1.0"")");
      end if;

      --  ... and for all of its ancestor types.

      declare
         Parents : Node_List
           := All_Ancestors (Node);
         It : Node_Iterator;
         P_Node : Node_Id;
      begin
         Init (It, Parents);
         while not Is_End (It) loop
            Get_Next_Node (It, P_Node);

            Add_With (CU, Ada_Full_Name (P_Node));
            PL (CU, "  or else CORBA.Is_Equivalent");
            PL (CU, "     (Logical_Type_Id,");
            II (CU);
            PL (CU, Ada_Full_Name (P_Node)
                & "." & Repository_Id_Name (P_Node) & ")");
            DI (CU);
         end loop;
         Free (Parents);
      end;

      PL (CU, "  or else False;");
      NL (CU);
   end Gen_Local_Is_A_Type_Checks;

   -------------------------
   -- Gen_Local_Impl_Is_A --
   -------------------------

   procedure Gen_Local_Impl_Is_A
     (Node      : Node_Id;
      Impl_Spec : in out Compilation_Unit;
      Impl_Body : in out Compilation_Unit) is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      pragma Assert (NK = K_Interface);
      --  Declaration

      Divert (Impl_Spec, Visible_Declarations);
      NL (Impl_Spec);
      PL (Impl_Spec, "function Is_A");
      PL (Impl_Spec, "  (Self            : access Object;");
      PL (Impl_Spec, "   Logical_Type_Id : PolyORB.Std.String) "
                   & "return Boolean;");

      --  Implementation

      NL (Impl_Body);
      PL (Impl_Body, "function Is_A");
      PL (Impl_Body, "  (Self            : access Object;");
      PL (Impl_Body, "   Logical_Type_Id : PolyORB.Std.String) "
                   & "return Boolean");
      PL (Impl_Body, "is");
      PL (Impl_Body, "begin");
      II (Impl_Body);

      Gen_Local_Is_A_Type_Checks (Node, Impl_Body);

      DI (Impl_Body);
      PL (Impl_Body, "end Is_A;");
   end Gen_Local_Impl_Is_A;

   ----------------------
   --  Gen_Local_Is_A  --
   ----------------------

   procedure Gen_Local_Is_A
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      pragma Assert (NK = K_Interface or else NK = K_ValueType);

      --  Declaration

      NL (CU);
      PL (CU, "--  The internal Is_A implementation for");
      PL (CU, "--  this interface.");
      NL (CU);
      PL (CU, "function Is_A");
      PL (CU, "  (Logical_Type_Id : PolyORB.Std.String)");
      PL (CU, "  return CORBA.Boolean");
      PL (CU, "is");
      PL (CU, "begin");
      II (CU);

      Gen_Local_Is_A_Type_Checks (Node, CU);

      DI (CU);
      PL (CU, "end Is_A;");
   end Gen_Local_Is_A;

   --------------------------------------
   -- Gen_Client_Stub_Type_Declaration --
   --------------------------------------

   procedure Gen_Client_Stub_Type_Declaration
     (CU        : in out Compilation_Unit;
      Node      : Node_Id)
   is
      Primary_Parent : constant Node_Id
        := Idl_Fe.Tree.Synthetic.Primary_Parent (Node);
      Unit, Typ : ASU.Unbounded_String;
   begin
      pragma Assert (False
         or else Kind (Node) = K_Interface
         or else Kind (Node) = K_ValueType);

      NL (CU);
      Put (CU, "type "
           & Ada_Type_Defining_Name (Mapping, Node)
           & " is new ");

      if Primary_Parent = No_Node then

         case (Kind (Node)) is

            when K_Interface =>
               if Abst (Node) then
                  Add_With (CU, "CORBA.Object");
                  Put (CU, "CORBA.Object.Ref");
                  --  FIXME
                  --  Add_With (CU, "CORBA.AbstractBase");
                  --  Put (CU, "CORBA.AbstractBase.Ref");
                  --  See CORBA Spec v2.3, chapter 6 on abstract interface
                  --  semantics, it explains why abstract interfaces should
                  --  inherit directly from CORBA.AbstractBase.Ref and not from
                  --  CORBA.Object.Ref However, I leave it like that because it
                  --  requires a lot of code rewriting, all the current support
                  --  for abstract interfaces is wrong (mainly because abstract
                  --  interfaces can refer to valutypes).

               else
                  Add_With (CU, "CORBA.Object");
                  Put (CU, "CORBA.Object.Ref");
               end if;

            when K_ValueType =>
               Add_With (CU, "CORBA.Value");
               Put (CU, "CORBA.Value.Base");

            when others =>
               raise Program_Error;
               --  Never happens

         end case;

      else
         Map_Type_Name (Mapping, Primary_Parent, Unit, Typ);
         Add_With (CU, -Unit);
         Put (CU, -Typ);
      end if;

      PL (CU, " with null record;");
      --  The type is not produced as a private extension declaration, because
      --  we may need to use it as a generic actual parameter to instantiate
      --  CORBA.Forward.

   end Gen_Client_Stub_Type_Declaration;

   ---------------------------------------
   --  Gen_Convert_Forward_Declaration  --
   ---------------------------------------

   procedure Gen_Convert_Forward_Declaration
     (CU : in out Compilation_Unit;
      Node : Node_Id) is
      Forward_Node : Node_Id;
   begin
      pragma Assert ((Kind (Node) = K_Interface)
                     or else (Kind (Node) = K_ValueType));
      Forward_Node := Forward (Node);
      if Forward_Node /= No_Node then

         --  This interface has a forward declaration

         NL (CU);
         PL (CU, "package Convert_Forward is");
         Add_With (CU, Ada_Full_Name (Parent_Scope (Forward_Node)));
         Put (CU, "  new "
              & Ada_Full_Name (Forward_Node)
              & ".Convert ("
              & Ada_Type_Defining_Name (Mapping, Node)
              & ");");
      end if;
   end Gen_Convert_Forward_Declaration;

   ------------------------------------
   -- Gen_Object_Servant_Declaration --
   ------------------------------------

   procedure Gen_Object_Servant_Declaration
     (CU        : in out Compilation_Unit;
      Node      : Node_Id;
      Full_View : Boolean)
   is
      Primary_Parent : constant Node_Id :=
                         Idl_Fe.Tree.Synthetic.Primary_Parent (Node);
   begin

      --  No skel package is generated for abstract interfaces

      pragma Assert (Kind (Node) = K_Interface);
      pragma Assert (not Abst (Node));

      NL (CU);
      PL (CU, "type Object is");
      if Primary_Parent = No_Node then
         if Local (Node) then
            Add_With (CU, "CORBA.Local");
            Put (CU, "  new CORBA.Local.Object");
         else
            Add_With (CU, "PortableServer");
            Put (CU, "  new PortableServer.Servant_Base");
         end if;
      else
         declare
            It : Node_Iterator;
            P_Node : Node_Id;
         begin
            Put (CU, "  new "
                 & Ada_Full_Name (Primary_Parent)
                 & Impl.Suffix & ".Object");

            Init (It, Parents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, P_Node);

               if not Abst (Value (P_Node)) then
                  Add_With (CU, Ada_Full_Name (P_Node)
                            & Impl.Suffix,
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  --  Make it so that the skeleton unit for an interface is
                  --  elaborated after those of all its parents.
               end if;
            end loop;
         end;
      end if;

      if Full_View then
         PL (CU, " with record");
         II (CU);
         PL (CU, "--  Insert components for implementation object state");
         PL (CU, "null;");
         DI (CU);
         PL (CU, "end record;");
      else
         PL (CU, " with private;");
         NL (CU);
         PL (CU, "type Object_Ptr is access all Object'Class;");
      end if;

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
            --  The null label is the "default" one, and must have its own case

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

      --  All cases might already have been covered by explicit when clauses,
      --  in which case the compiler notes that this "when others" clause is
      --  redundant: disable warnings here.

      NL (CU);
      PL (CU, "pragma Warnings (Off);");
      PL (CU, "when others =>");
      II (CU);
      PL (CU, "null;");
      DI (CU);
      PL (CU, "pragma Warnings (On);");
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
           K_Module     =>
            null;

         when K_Interface  =>
            null;

         when K_Forward_Interface =>
            Add_With
              (CU, "CORBA.Forward", Elab_Control => Elaborate_All);
            NL (CU);
            PL (CU, "package " & Ada_Name (Node)
                & " is new CORBA.Forward;");

         -----------------
         -- Value types --
         -----------------

         when K_ValueType =>
            null;
         when K_Forward_ValueType =>
            Add_With (CU, "CORBA.Value.Forward");
            NL (CU);
            PL (CU, "package " & Ada_Name (Node)
                & " is new CORBA.Value.Forward;");

         when K_Boxed_ValueType =>
            Add_With (CU, "CORBA.Value.Box");
            NL (CU);
            PL (CU,
                "type "
                & Access_Type_Name (Boxed_Type (Node))
                & " is");
            PL (CU,
                "   access all "
                & Ada_Type_Name (Boxed_Type (Node))
                & ";");
            NL (CU);
            PL (CU,
                "package "
                & Ada_Name (Node)
                & "_Value_Box is new CORBA.Value.Box");
            PL (CU,
                "  ("
                & Ada_Type_Name (Boxed_Type (Node))
                & ",");
            PL (CU,
                "   "
                & Access_Type_Name (Boxed_Type (Node))
                & ");");
            NL (CU);
            PL (CU,
                "subtype "
                & Ada_Name (Node)
                & " is "
                & Ada_Name (Node)
                & "_Value_Box.Box_Ref;");
            --  XXX Using a derived type would require overriding primtives
            --  of CORBA.Value.Box, which was deemed "impractical". But are we
            --  allowed to use a subtype instead?

         when K_State_Member =>
            null;

         ----------------
         -- Operations --
         ----------------

         when K_Initializer =>
            Gen_Initializer_Profile
              (CU, "Value_Ref'Class", Node);
            PL (CU, ";");

         when K_Operation =>

            declare
               Implicit    : constant Boolean := Is_Implicit_Inherited (Node);
               Original_If : constant Node_Id := Original_Parent_Scope (Node);
            begin

               --  Generate operation declaration (commented out if it is
               --  implicitly inherited from parent type).

               Set_Comment_Out_Mode (CU, Implicit);
               Gen_Operation_Profile
                 (CU, Node, Ada_Type_Defining_Name
                              (Mapping, Parent_Scope (Node)));
               PL (CU, ";");
               Set_Comment_Out_Mode (CU, False);

               if not Implicit and then Original_Node (Node) = No_Node then

                  --  A real operation (coming from the IDL source)

                  Gen_Repository_Id (Node, CU);
               end if;

               if Original_If /= Parent_Scope (Node) then
                  Put (CU, "--  ");
                  if Implicit then
                     Put (CU, "Implicitly i");
                  else
                     Put (CU, "I");
                  end if;
                  PL (CU, "nherited from " & Ada_Full_Name (Original_If));
               end if;
            end;

         when K_Attribute =>
            declare
               It : Node_Iterator;
               Attr_Decl_Node : Node_Id;
            begin
               Init (It, Declarators (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Attr_Decl_Node);
                  Gen_Repository_Id (Attr_Decl_Node, CU);
               end loop;
            end;

         when K_Exception =>

            Add_With (CU, "Ada.Exceptions");
            Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

            --  Exception declaration

            NL (CU);
            PL (CU, Ada_Name (Node) & " : exception;");

            --  Repository id

            Gen_Repository_Id (Node, CU);

            --  Members accessor

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

            --  Type declaration

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
            Gen_Repository_Id (Node, CU);

         when K_Type_Declarator =>
            declare
               Is_Ref : constant Boolean
                 := Is_Interface_Type (T_Type (Node))
                 or else Kind (T_Type (Node)) = K_Object;
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
                        if Is_Ref and then not Is_Array then

                           --  A typedef where the <type_spec> denotes an
                           --  interface type, and which is not an array
                           --  declaration.

                           Put (CU, "subtype ");
                        else
                           Put (CU, "type ");
                        end if;

                        Gen_Node_Stubs_Spec (CU, Decl_Node);

                        PL (CU, " is");

                        if Is_Array then
                           Init (Bounds_It, Array_Bounds (Decl_Node));
                           while not Is_End (Bounds_It) loop
                              Get_Next_Node (Bounds_It, Bound_Node);

                              if First_Bound then
                                 Put (CU, "  array (");
                                 First_Bound := False;
                              else
                                 Put (CU, ", ");
                              end if;

                              Put (CU, "0 .. ");
                              Gen_Constant_Value
                                (CU, Expr => Bound_Node, Typ => No_Node);
                              Put (CU, " - 1");
                           end loop;
                           PL (CU, ") of");
                           II (CU);
                        else
                           Put (CU, "  ");
                           if not (Is_Ref or else Is_Fixed) then
                              Put (CU, "new ");
                           end if;
                        end if;

                        Gen_Node_Stubs_Spec (CU, T_Type (Node));
                        PL (CU, ";");

                        if Is_Array then
                           DI (CU);
                        end if;
                     end;

                     if Original_Node (Decl_Node) = No_Node then
                        Gen_Repository_Id (Decl_Node, CU);
                     end if;
                  end loop;
               end;
            end;

         when K_Union =>
            NL (CU);
            PL (CU, "type " & Ada_Name (Node));
            Put (CU, "  (Switch : ");
            Gen_Node_Stubs_Spec (CU, Switch_Type (Node));
            NL (CU);
            II (CU);
            Put (CU, "  := ");
            Gen_Node_Stubs_Spec (CU, Switch_Type (Node));
            PL (CU, "'First)");
            DI (CU);
            PL (CU, "is record");
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
            Gen_Repository_Id (Node, CU);

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
               Unit, Typ : ASU.Unbounded_String;
            begin
               Map_Type_Name (Mapping, Sequence_Type (S_Node),
                              Unit, Typ);
               Add_With (CU, -Unit);
               if B_Node /= No_Node then
                  Add_With (CU, "CORBA.Sequences.Bounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new CORBA.Sequences.Bounded");
                  PL (CU, "    ("
                      & (-Typ)
                      & ", " & Img (Integer_Value (B_Node))
                      & ");");
               else
                  Add_With (CU, "CORBA.Sequences.Unbounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new CORBA.Sequences.Unbounded");
                  PL (CU, "    ("
                      & (-Typ)
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
            if not Is_Exception_Members (Node) then
               Gen_Repository_Id (Node, CU);
            end if;

         when K_Const_Dcl =>

            NL (CU);
            Put (CU, Name (Node) & " : constant ");
            Gen_Node_Stubs_Spec (CU, Constant_Type (Node));
            NL (CU);
            Put (CU, "  := ");
            Gen_Constant_Value (CU,
              Expr => Expression (Node), Typ => Constant_Type (Node));
            PL (CU, ";");

         when K_ValueBase =>
            --  FIXME: Check that this is correct.
            null;

         when K_Native =>
            NL (CU);
            PL (CU, "--  Type " & Name (Declarator (Node))
                & " is implementation defined;");

         when K_Void =>
            --  FIXME: Probably cannot happen.
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

   -------------
   -- Justify --
   -------------

   function Justify (S : String; Max : Integer) return String
   is
      WS : String (1 .. 50);
   begin
      if S'Length >= Max or else Max > WS'Length then
         return S;
      end if;
      Move (S, WS, Pad => ' ');
      return Head (WS, Max);
   end Justify;

   ----------------------------
   -- Gen_Forward_Conversion --
   ----------------------------

   procedure Gen_Forward_Conversion
     (CU        : in out Compilation_Unit;
      T_Node    : Node_Id;
      Direction : String;
      What      : String)
   is
      NT : Node_Id := T_Node;
   begin
      --  XXX the following loop is dubious.
      --  Most likely, it runs exactly once every time.
      while Kind (NT) = K_Scoped_Name loop
         NT := Value (NT);
      end loop;

      if Kind (NT) = K_Forward_Interface
        or else Kind (NT) = K_Forward_ValueType
      then
         declare
            Prefix : constant String := Ada_Full_Name (Forward (NT));
         begin
            Add_With (CU, Prefix);
            PL (CU, Prefix & ".Convert_Forward." & Direction);
            Put (CU, "  (" & What & ")");
         end;
      else
         Put (CU, What);
      end if;
   end Gen_Forward_Conversion;

   -----------------------------
   -- Gen_Node_Stubs_Body_Dyn --
   -----------------------------

   procedure Gen_Node_Stubs_Body_Dyn
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

            --  Declare constant used by stubs for the name of the Result
            --  NamedValue

            if not Local (Node)
              and then Has_Non_Implicit_Inherited_Operations (Node)
            then
               NL (CU);
               Add_With (CU, "PolyORB.Types");
               PL (CU, T_Result_Name & " : constant PolyORB.Types.Identifier");
               PL (CU, "  := PolyORB.Types.To_PolyORB_String (""Result"");");
            end if;

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
               O_Name        : constant String  := Ada_Operation_Name (Node);
               O_Type        : constant Node_Id := Operation_Type (Node);
               Org_O_Type    : constant Node_Id
                                 := Original_Operation_Type (Node);
               Is_Function   : constant Boolean := Kind (O_Type) /= K_Void;
               Decls_Div     : constant Diversion := Current_Diversion (CU);

               Self_Expr : constant String :=
                             Self_For_Operation (Mapping, Node);

               procedure Gen_Object_Self_Nil_Check;
               --  Generate object reference nil check

               -------------------------------
               -- Gen_Object_Self_Nil_Check --
               -------------------------------

               procedure Gen_Object_Self_Nil_Check is
               begin
                  NL (CU);
                  PL (CU,
                      "if Is_Nil (" & Self_Expr & ") then");
                  II (CU);
                  PL (CU, "CORBA.Raise_Inv_Objref (Default_Sys_Member);");
                  DI (CU);
                  PL (CU, "end if;");
                  NL (CU);
               end Gen_Object_Self_Nil_Check;

            begin
               Add_With (CU, "CORBA",
                         Use_It       => True,
                         Elab_Control => Elaborate_All);

               Divert (CU, Operation_Body);
               Gen_Operation_Profile
                 (CU, Node,
                  Ada_Type_Defining_Name (Mapping, Parent_Scope (Node)));

               NL (CU);
               PL (CU, "is");
               II (CU);

               if Local (Parent_Scope (Node)) then

                  --  Case of a local interface

                  declare
                     Impl_U_Name : constant String
                       := Client_Stubs_Unit_Name (Mapping, Parent_Scope (Node))
                            & Impl.Suffix;

                  begin
                     Add_With (CU, Impl_U_Name);

                     DI (CU);
                     PL (CU, "begin");
                     II (CU);

                     Gen_Object_Self_Nil_Check;

                     if Is_Function then
                        Put (CU, "return ");
                     end if;
                     PL (CU, Impl_U_Name & '.' & O_Name);

                     --  XXX replace .Object_Ptr by ???
                     Put (CU, "  (" & Impl_U_Name
                          & ".Object_Ptr (Entity_Of ("
                          & Self_For_Operation (Mapping, Node) & "))");

                     declare
                        It   : Node_Iterator;
                        P_Node : Node_Id;
                     begin
                        Init (It, Parameters (Node));
                        while not Is_End (It) loop

                           Get_Next_Node (It, P_Node);

                           if not Is_Returns (P_Node) then
                              PL (CU, ",");
                              II (CU);
                              Put (CU, Ada_Name (Declarator (P_Node)));
                              DI (CU);
                           end if;

                        end loop;
                     end;

                     if not Is_Function
                       and then Kind (Org_O_Type) /= K_Void
                     then
                        PL (CU, ",");
                        II (CU);
                        Put (CU, "Returns");
                        DI (CU);
                     end if;

                     PL (CU, ");");
                  end;

               else

                  --  Case of an unconstrained interface

                  declare
                     Response_Expected : constant Boolean :=
                                           not Is_Oneway (Node);
                     Raise_Something   : constant Boolean :=
                                           not Is_Empty (Raises (Node));

                  begin
                     Add_With (CU, "PolyORB.CORBA_P.Exceptions");
                     Add_With (CU, "PolyORB.CORBA_P.Interceptors_Hooks");
                     Add_With (CU, "PolyORB.Any.NVList");
                     Add_With (CU, "PolyORB.Requests");

                     --  Prepare return Any

                     if Kind (Org_O_Type) /= K_Void then
                        Put (CU, T_Result & " : "
                             & Ada_Type_Name (Org_O_Type));

                        if Is_Function then
                           PL (CU, ";");

                           --  Kill warning about unreferenced variable (it is
                           --  accessed only through the wrapper below).

                           PL (CU, "pragma Warnings (Off, " & T_Result & ");");
                        else
                           PL (CU, " renames Returns;");

                           --  Kill warning on out arg that is never explicitly
                           --  assigned.

                           PL (CU, "pragma Warnings (Off, Returns);");
                        end if;

                        PL (CU, T_Arg_CC & T_Result
                            & " : aliased PolyORB.Any.Content'Class"
                            & " :=");
                        II (CU);
                        Helper.Gen_Wrap_Call (CU, Org_O_Type, T_Result);
                        DI (CU);
                        PL (CU, ";");
                     end if;

                     --  Prepare argument Anys

                     declare
                        It   : Node_Iterator;
                        P_Node : Node_Id;
                     begin
                        Init (It, Parameters (Node));
                        while not Is_End (It) loop
                           Get_Next_Node (It, P_Node);
                           if not Is_Returns (P_Node) then

                              declare
                                 Arg_Name    : constant String :=
                                               Ada_Name (Declarator (P_Node));
                                 P_Typ       : constant Node_Id :=
                                                 Param_Type (P_Node);
                                 Helper_Name : constant String :=
                                                 Helper_Unit (P_Typ);

                              begin
                                 Add_With (CU, Helper_Name);
                                 Divert (CU, Decls_Div);
                                 NL (CU);
                                 PL (CU, O_Name & T_Arg_Name & Arg_Name
                                     & " : constant PolyORB.Types.Identifier");
                                 PL (CU,
                                     "  := PolyORB.Types.To_PolyORB_String ("""
                                     & Arg_Name & """);");
                                 Divert (CU, Operation_Body);

                                 PL (CU, T_Arg_CC & Arg_Name
                                     & " : aliased PolyORB.Any.Content'Class"
                                     & " :=");
                                 II (CU);
                                 Helper.Gen_Wrap_Call (CU, P_Typ, Arg_Name);
                                 DI (CU);
                                 PL (CU, ";");

                                 Add_With (CU, TC_Unit (P_Typ));
                                 PL (CU, T_Arg_Any & Arg_Name
                                     & " : constant CORBA.Any :="
                                     & " CORBA.Internals.Get_Wrapper_Any ("
                                     & Ada_Full_TC_Name (P_Typ) & ", "
                                     & T_Arg_CC & Arg_Name
                                     & "'Unchecked_Access);");

                                 --  Kill warning on out arg that is never
                                 --  explicitly assigned.

                                 if Mode (P_Node) /= Mode_In then
                                    PL (CU, "pragma Warnings (Off, "
                                        & Arg_Name & ");");
                                 end if;
                              end;
                           end if;
                        end loop;
                     end;
                     NL (CU);

                     Add_With (CU, "CORBA.Object");
                     PL (CU, T_Request
                         & " : PolyORB.Requests.Request_Access;");

                     PL (CU, T_Arg_List & " : PolyORB.Any.NVList.Ref;");
                     PL (CU, T_Result & "_NV : PolyORB.Any.NamedValue;");

                     DI (CU);
                     PL (CU, "begin");
                     II (CU);

                     Gen_Object_Self_Nil_Check;

                     PL (CU, "--  Create argument list");
                     NL (CU);
                     PL (CU, "PolyORB.Any.NVList.Create");
                     PL (CU, "  (" & T_Arg_List & ");");

                     declare
                        It   : Node_Iterator;
                        P_Node : Node_Id;
                     begin
                        Init (It, Parameters (Node));
                        while not Is_End (It) loop

                           Get_Next_Node (It, P_Node);

                           if not Is_Returns (P_Node) then
                              declare
                                 Arg_Name : constant String :=
                                              Ada_Name (Declarator (P_Node));
                              begin
                                 PL (CU, "PolyORB.Any.NVList.Add_Item");
                                 PL (CU, "  (" & T_Arg_List & ",");
                                 II (CU);
                                 PL (CU, O_Name & T_Arg_Name & Arg_Name & ",");

                                 Put (CU,
                                      Conditional_Call
                                        (Func      => "PolyORB.Any.Copy_Any",
                                         Only_When => not Response_Expected,
                                         Expr      => "PolyORB.Any.Any ("
                                                      & T_Arg_Any & Arg_Name
                                                      & ")"));
                                 PL (CU, ",");
                              end;

                              case Mode (P_Node) is
                                 when Mode_In =>
                                    PL (CU, "PolyORB.Any.ARG_IN);");
                                 when Mode_Inout =>
                                    PL (CU, "PolyORB.Any.ARG_INOUT);");
                                 when Mode_Out =>
                                    PL (CU, "PolyORB.Any.ARG_OUT);");
                              end case;
                              DI (CU);
                           end if;

                        end loop;
                     end;

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

                           if First then
                              Divert (CU, Decls_Div);
                              NL (CU);

                              Add_With (CU, "CORBA.ExceptionList");
                              PL (CU, O_Name & T_Excp_List
                                  & " : CORBA.ExceptionList.Ref;");
                              Divert (CU, Deferred_Initialization);
                              NL (CU);
                              PL (CU, "--  Exceptions list for " & O_Name);
                              NL (CU);
                              PL (CU, "CORBA.ExceptionList.Create_List ("
                                  & O_Name & T_Excp_List & ");");
                              First := False;
                           end if;

                           Helper.Add_Helper_Dependency (CU, TC_Unit (E_Node));
                           PL (CU, "CORBA.ExceptionList.Add");
                           PL (CU, "  (" & O_Name & T_Excp_List & ",");
                           II (CU);
                           PL (CU, Ada_Full_TC_Name (E_Node) & ");");
                           DI (CU);
                        end loop;
                     end;
                     Divert (CU, Operation_Body);

                     NL (CU);
                     PL (CU, "--  Set result type (maybe void)");
                     NL (CU);
                     PL (CU, T_Result & "_NV :=");
                     PL (CU, " (Name     => " & T_Result_Name & ",");
                     PL (CU, "  Argument =>");

                     Add_With (CU, TC_Unit (Org_O_Type));
                     II (CU);
                     PL (CU, "CORBA.Internals.Get_Empty_Any ("
                         & Ada_Full_TC_Name (Org_O_Type) & "),");
                     PL (CU, "Arg_Modes => 0);");
                     DI (CU);
                     if Kind (Org_O_Type) /= K_Void then
                        PL (CU, "PolyORB.Any.Set_Value ("
                          & "PolyORB.Any.Get_Container ("
                          & T_Result & "_NV.Argument).all, "
                          & T_Arg_CC & T_Result & "'Unrestricted_Access);");
                     end if;
                     NL (CU);

                     PL (CU, "PolyORB.Requests.Create_Request");
                     PL (CU, "  (Target    => CORBA.Object.Internals."
                         & "To_PolyORB_Ref");
                     II (CU);
                     PL (CU, "  (CORBA.Object.Ref ("
                         & Self_For_Operation (Mapping, Node) & ")),");
                     PL (CU, "Operation => """ & Idl_Operation_Id (Node)
                         & """,");
                     PL (CU, "Arg_List  => " & T_Arg_List & ",");
                     PL (CU, "Result    => " & T_Result & "_NV,");

                     if Raise_Something then
                        PL (CU,
                            "Exc_List  => CORBA.ExceptionList.Internals."
                            & "To_PolyORB_Ref ("
                            & O_Name & T_Excp_List & "),");
                     end if;

                     if Response_Expected then
                        PL (CU, "Req       => " & T_Request & ");");
                     else
                        PL (CU, "Req       => " & T_Request & ",");
                        PL (CU, "Req_Flags => " &
                            "PolyORB.Requests.Sync_With_Transport);");
                     end if;
                     DI (CU);

                     NL (CU);
                     PL (CU,
                         "PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke");
                     PL (CU, "  (" & T_Request
                         & ", PolyORB.Requests.Flags (0));");

                     PL (CU, "PolyORB.CORBA_P.Exceptions."
                         & "Request_Raise_Occurrence (" & T_Request
                         & ".all);");

                     PL (CU, "PolyORB.Requests.Destroy_Request ("
                         & T_Request & ");");

                     if Response_Expected then

                        NL (CU);
                        PL (CU, "--  Request has been synchronously invoked");
                        NL (CU);

                        if Kind (Org_O_Type) /= K_Void
                          and then Is_Function
                        then
                           PL (CU, "return " & T_Result & ";");
                        end if;

                     end if;
                  end;
               end if;

               DI (CU);
               PL (CU, "end " & O_Name & ";");
               Divert (CU, Decls_Div);
               Undivert (CU, Operation_Body);
            end;

         when K_Exception =>
            Add_With (CU, "PolyORB.Exceptions");
            NL (CU);
            PL (CU, "procedure Get_Members");
            PL (CU, "  (From : Ada.Exceptions.Exception_Occurrence;");
            PL (CU, "   To   : out "
                & Ada_Name (Members_Type (Node))
                & ") is");
            PL (CU, "begin");
            II (CU);
            PL (CU, "PolyORB.Exceptions.User_Get_Members (From, To);");
            DI (CU);
            PL (CU, "end Get_Members;");

         when others =>
            null;
      end case;
   end Gen_Node_Stubs_Body_Dyn;

   -----------------------------
   -- Gen_Initializer_Profile --
   -----------------------------

   procedure Gen_Initializer_Profile
     (CU : in out Compilation_Unit;
      Return_Type : String;
      Node : Node_Id) is
   begin
      pragma Assert (Kind (Node) = K_Initializer);
      NL (CU);
      Put (CU, "function ");
      PL (CU, Ada_Name (Node));

      --  Parameters
      declare
         It   : Node_Iterator;
         P_Node : Node_Id;
      begin

         Init (It, Param_Decls (Node));
         if not Is_End (It) then
            --  First parameter
            Put (CU, "  (");
            Get_Next_Node (It, P_Node);
            Gen_Operation_Profile
              (CU, P_Node, "");
            II (CU);
            --  Next parameters
            while not Is_End (It) loop
               Get_Next_Node (It, P_Node);
               PL (CU, ";");
               Gen_Operation_Profile
                 (CU, P_Node, "");
            end loop;

            Put (CU, ")");
            DI (CU);
         end if;
         NL (CU);
         II (CU);
         Put (CU, "return " & Return_Type);
         DI (CU);
      end;
   end Gen_Initializer_Profile;

   ---------------------------
   -- Gen_Operation_Profile --
   ---------------------------

   procedure Gen_Operation_Profile
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Object_Type : String;
      With_Name   : Boolean          := True;
      Is_Delegate : Boolean          := False)
   is
      First : Boolean := True;
   begin
      case Kind (Node) is

         when K_Operation =>

            --  Subprogram name

            NL (CU);
            if Is_Delegate then
               Put (CU, "with ");
            end if;
            if Kind (Operation_Type (Node)) = K_Void then
               Put (CU, "procedure ");
            else
               Put (CU, "function ");
            end if;

            --  In Value_Skel, we need the profile of the subprogram without
            --  the name, to create an access to subprogram type

            if With_Name then
               Put (CU, Ada_Operation_Name (Node));
            end if;

            --  Formals

            if not Is_Explicit_Self (Node) then
               Put (CU, ASCII.LF & "  (Self : " & Object_Type);
               II (CU);
               First := False;
            end if;

            declare
               It   : Node_Iterator;
               P_Node : Node_Id;
            begin
               Init (It, Parameters (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, P_Node);

                  if First then
                     Put (CU, ASCII.LF & "  (");
                     II (CU);
                     First := False;
                  else
                     PL (CU, ";");
                  end if;
                  Gen_Operation_Profile
                    (CU, P_Node, Object_Type);
               end loop;

               if not First then
                  --  Non-empty profile
                  Put (CU, ")");
                  DI (CU);
               end if;
            end;

            --  Return type

            declare
               O_Type : constant Node_Id
                 := Operation_Type (Node);
               Unit, Typ : ASU.Unbounded_String;
            begin
               if Kind (O_Type) /= K_Void then
                  NL (CU);
                  Map_Type_Name (Mapping, O_Type, Unit, Typ);
                  Add_With (CU, -Unit);
                  Put (CU, "  return " & (-Typ));

                  if Kind (O_Type) = K_Scoped_Name
                    and then S_Type (O_Type) = Parent_Scope (Node)
                  then
                     --  An operation of an interface is a primitive operation
                     --  of the tagged type that maps this interface. If it has
                     --  other formal parameters that are object references of
                     --  the same interface type, then these formals must not
                     --  be controlling (Ada RTF issue #2459).

                     Put (CU, "'Class");
                  end if;
               end if;
            end;
            if Is_Delegate then
               Put (CU, " is <>");
            end if;

         when K_Param =>

            Gen_Operation_Profile
              (CU, Declarator (Node), Object_Type);
            case Mode (Node) is
               when Mode_In =>
                  Put (CU, " : ");
               when Mode_Out =>
                  Put (CU, " : out ");
               when Mode_Inout =>
                  Put (CU, " : in out ");
            end case;

            declare
               T_Node : constant Node_Id := Param_Type (Node);
               Unit, Typ : ASU.Unbounded_String;
            begin
               Map_Type_Name (Mapping, T_Node, Unit, Typ);
               Add_With (CU, -Unit);
               Put (CU, -Typ);

               if Kind (T_Node) = K_Scoped_Name
                 and then S_Type (T_Node) = Parent_Scope
                 (Parent_Scope (Declarator (Node)))
               then

                  --  An operation of an interface is primitive operation of
                  --  the tagged type that maps this interface. If it has other
                  --  formal parameters that are object references of the same
                  --  interface type, then these formals must not be
                  --  controlling. (Ada RTF issue #2459) (see above).
                  --  FIXME: code duplication.

                  Put (CU, "'Class");
               end if;
            end;

         when others =>
            Gen_Node_Default (CU, Node);

      end case;
   end Gen_Operation_Profile;

   procedure Gen_Node_Default
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Unit, Typ : Ada.Strings.Unbounded.Unbounded_String;
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is

         when K_Scoped_Name =>

            declare
               Denoted_Entity : constant Node_Id
                 := Value (Node);
            begin
               case Kind (Denoted_Entity) is
                  when K_Enumerator =>
                     Put (CU, Ada_Full_Name (Node));

                  when others =>
                     Map_Type_Name (Mapping, Node, Unit, Typ);
                     Add_With (CU, -Unit);
                     Put (CU, -Typ);
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
           K_Object             |
           K_Any                =>
            Map_Type_Name (Mapping, Node, Unit, Typ);
            Add_With (CU, -Unit);
            Put (CU, -Typ);

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

         when K_Lit_Character =>
            Put (CU, Img (Character_Value (Node)));

         when K_Lit_Integer =>
            Put (CU, Img (Integer_Value (Node)));

         when K_Lit_Boolean =>
            Put (CU, Img (Boolean_Value (Node)));

         when K_Lit_Enum =>
            Put (CU, Ada_Full_Name (Enum_Value (Node)));

         when K_Primary_Expr =>
            Gen_Node_Default (CU, Operand (Node));

         when others =>
            Error
              ("ada_be-idl2ada.Gen_Node_Default: "
               & "Don't know what to do with a "
               & Node_Kind'Image (NK) & " node.",
               Fatal, Get_Location (Node));
      end case;
   end Gen_Node_Default;

   --------------------
   -- Ada_Type_Name --
   --------------------

   function Ada_Type_Name
     (Node : Node_Id)
     return String
   is
      Unit, Typ : ASU.Unbounded_String;
   begin
      Map_Type_Name (Mapping, Node, Unit, Typ);
      return -Typ;
   end Ada_Type_Name;

   ----------------------
   -- Ada_Full_TC_Name --
   ----------------------

   function Ada_Full_TC_Name (Node : Node_Id) return String is
   begin
      return TC_Unit (Node) & "." & Ada_TC_Name (Node);
   end Ada_Full_TC_Name;

   -----------------
   -- Helper_Unit --
   -----------------

   function Helper_Unit
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin

      case NK is
         when K_Declarator =>
            declare
               P_T_Type : constant Node_Id := T_Type (Parent (Node));
               Is_Array : constant Boolean
                 := not Is_Empty (Array_Bounds (Node));
               Is_Ref   : constant Boolean
                 := not Is_Array
                      and then (Is_Interface_Type (P_T_Type)
                                or else Kind (P_T_Type) = K_Object);
            begin
               if Is_Ref then
                  --  This node is mapped to a subtype of the original
                  --  reference type: use that type's From_Any and To_Any.
                  return Helper_Unit (P_T_Type);
               else
                  return Helper_Unit (Parent_Scope (Node));
               end if;
            end;

         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            return Helper_Unit (Parent_Scope (Node));
            --  Different from Ada_Helper_Name (Node).

         when K_Scoped_Name =>
            return Helper_Unit (Value (Node));
            --  Potentially different from Ada_Helper_Name (Node).

         when others =>
            return Ada_Helper_Unit_Name (Mapping, Node);
      end case;
   end Helper_Unit;

   -------------
   -- TC_Unit --
   -------------

   function TC_Unit (Node : Node_Id) return String is
   begin
      return Ada_Helper_Unit_Name (Mapping, Node);
   end TC_Unit;

   ----------------------
   -- Access_Type_Name --
   ----------------------

   function Access_Type_Name (Node : Node_Id) return String is
      Name : String := Ada_Type_Name (Node);
   begin
      for I in Name'Range loop
         if Name (I) = '.' then
            Name (I) := '_';
         end if;
      end loop;
      return Name & "_Access";
   end Access_Type_Name;

   ---------------------
   -- Add_With_Entity --
   ---------------------

   procedure Add_With_Entity
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      Add_With (CU, Library_Unit_Name (Mapping, Node));
   end Add_With_Entity;

   ---------------------------------------------------------
   -- Ada_Operation_Name and Idl_Operation_Id differ      --
   -- for operations that are created by the expander and --
   -- represent attributes:                               --
   -- given an attribute Foo of an interface, the "get"   --
   -- and "set" operations will be generated with         --
   -- Ada_Operation_Names "Get_Foo" and "Set_Foo", and    --
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

   ------------------------
   -- Repository_Id_Name --
   ------------------------

   function Repository_Id_Name
     (Node : Node_Id)
     return String is
   begin
      return Ada_Name (Repository_Id_Identifier (Node));
   end Repository_Id_Name;

   ------------------------
   -- Gen_Constant_Value --
   ------------------------

   procedure Gen_Constant_Value
     (CU   : in out Compilation_Unit;
      Expr : Node_Id;
      Typ  : Node_Id)
   is
      Value : constant Constant_Value_Ptr := Expr_Value (Expr);

      To_CORBA_Prefix   : aliased String := "To_CORBA";
      To_Bounded_Prefix : aliased String := "To_Bounded";
      String_Cvt_Prefix : constant array (Boolean) of access String :=
                            (False => To_CORBA_Prefix'Access,
                             True  => To_Bounded_Prefix'Access);
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

            declare
               Int_Val : constant Idl_Integer := Integer_Value (Expr);
            begin
               --  If the value is negative, we use an expanded name for the
               --  "-" operator, because it might not be directly visible. For
               --  example, CORBA."-" (1234). If Typ is not provided, assume
               --  that the "-" operator is directly visible (case of the
               --  index type of an IDL array, which is Standard.Integer).

               if Present (Typ) and then Int_Val < 0 then
                  Put (CU, Library_Unit_Name (Mapping, Typ) & ".""-"" (");
                  Put (CU, Img (-Int_Val));
                  Put (CU, ")");
               else
                  Put (CU, Img (Int_Val));
               end if;
            end;

         when C_Char =>
            Put (CU, "'" & Value.Char_Value & "'");

         when C_WChar =>
            Put (CU, Ada.Characters.Conversions.To_String
                 ("'" & Value.WChar_Value & "'"));

         when C_Boolean =>
            Put (CU, Img (Boolean_Value (Expr)));

         when
           C_Float         |
           C_Double        |
           C_LongDouble    |
           C_General_Float =>
            Put (CU, Img (Float_Value (Expr)));

         when
           C_Fixed         |
           C_General_Fixed =>
            declare
               Value_Digits : constant String := Img (Value.Fixed_Value);
               Zeroes : constant String (1 .. Integer (Value.Scale)
                                                - Value_Digits'Length + 1) :=
                          (others => '0');

               All_Digits : constant String := Zeroes & Value_Digits;
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
            declare
               Bounded : constant Boolean := Present (Bound (Root_Type (Typ)));
            begin
               Put (CU, Library_Unit_Name (Mapping, Typ)
                    & "."
                    & String_Cvt_Prefix (Bounded).all
                    & "_String ("""
                    & String_Value (Expr) & """)");
            end;

         when C_WString =>
            declare
               Bounded : constant Boolean := Present (Bound (Root_Type (Typ)));
            begin
               Put (CU, Library_Unit_Name (Mapping, Typ)
                    & "."
                    & String_Cvt_Prefix (Bounded).all
                    & "_Wide_String ("""
                    & Ada.Characters.Conversions.To_String
                    (WString_Value (Expr)) & """)");
            end;

         when C_Enum =>
            Put (CU, Ada_Full_Name (Enum_Value (Expr)));

         when C_No_Kind =>
            Error
              ("Constant without a kind.",
               Fatal, Get_Location (Expr));
      end case;

   end Gen_Constant_Value;

   -----------------
   -- Ada_TC_Name --
   -----------------

   function Ada_TC_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);
      Prefix : constant String := "TC_";
   begin
      case NK is
         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            return Ada_TC_Name (Forward (Node));

         when
           K_Interface         |
           K_ValueType         |
           K_Sequence_Instance |
           K_String_Instance   |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Declarator        =>
            return Prefix & Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_TC_Name (Value (Node));

         when K_Short =>
            return Prefix & "Short";

         when K_Long =>
            return Prefix & "Long";

         when K_Long_Long =>
            return Prefix & "Long_Long";

         when K_Unsigned_Short =>
            return Prefix & "Unsigned_Short";

         when K_Unsigned_Long =>
            return Prefix & "Unsigned_Long";

         when K_Unsigned_Long_Long =>
            return Prefix & "Unsigned_Long_Long";

         when K_Char =>
            return Prefix & "Char";

         when K_Wide_Char =>
            return Prefix & "Wchar";

         when K_Boolean =>
            return Prefix & "Boolean";

         when K_Float =>
            return Prefix & "Float";

         when K_Double =>
            return Prefix & "Double";

         when K_Long_Double =>
            return Prefix & "Long_Double";

         when K_String =>
            return Prefix & "String";

         when K_Wide_String =>
            return Prefix & "Wide_String";

         when K_Octet =>
            return Prefix & "Octet";

         when K_Object =>
            return Prefix & "Object";

         when K_Any =>
            return Prefix & "Any";

         when K_Void =>
            return Prefix & "Void";

         when others =>
            --  Improper use: node N is not mapped to an Ada type

            Error
              ("No TypeCode for " & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy

            raise Program_Error;

      end case;
   end Ada_TC_Name;

   -----------------------------
   -- Gen_Module_Init_Prelude --
   -----------------------------

   procedure Gen_Module_Init_Prelude
     (CU              : in out Compilation_Unit;
      With_Dependency : String := "")
   is
   begin
      Set_Template_Mode (CU, True);
      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "procedure Deferred_Initialization is");
      PL (CU, "begin");
      II (CU);
      Divert (CU, Initialization_Dependencies);
      II (CU); II (CU); II (CU);

      if With_Dependency'Length /= 0 then
         PL (CU, "+""" & With_Dependency & """");
      else
         PL (CU, "PolyORB.Initialization.String_Lists.Empty");
      end if;

      Divert (CU, Visible_Declarations);
      Set_Template_Mode (CU, False);
   end Gen_Module_Init_Prelude;

   ------------------------------
   -- Gen_Module_Init_Postlude --
   ------------------------------

   procedure Gen_Module_Init_Postlude (CU : in out Compilation_Unit) is
   begin
      Set_Template_Mode (CU, True);
      Divert (CU, Deferred_Initialization);
      DI (CU);
      NL (CU);
      PL (CU, "end Deferred_Initialization;");
      Set_Template_Mode (CU, False);

      if Current_Diversion_Empty (CU) then
         return;
      end if;

      Divert (CU, Visible_Declarations);
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
      PL (CU, " Conflicts => PolyORB.Initialization.String_Lists.Empty,");
      PL (CU, " Depends   =>");
      Undivert (CU, Initialization_Dependencies);

      PL (CU, " ,");
      PL (CU, " Provides  => PolyORB.Initialization.String_Lists.Empty,");
      PL (CU, " Implicit  => False,");
      PL (CU, " Init      => Deferred_Initialization'Access,");
      PL (CU, " Shutdown  => null));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
   end Gen_Module_Init_Postlude;

   ----------------------
   -- New_Library_Unit --
   ----------------------

   procedure New_Library_Unit (Name : String; LU : out Library_Unit_Data) is
   begin
      New_Compilation_Unit (LU (Unit_Spec), Unit_Spec, Name);
      New_Compilation_Unit (LU (Unit_Body), Unit_Body, Name,
                            LU (Unit_Spec)'Unchecked_Access);
   end New_Library_Unit;

end Ada_Be.Idl2Ada;
