------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A D A _ B E . I D L 2 A D A                        --
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

--  This unit contains both generation routines that are
--  of general use to the whole Ada 95 back-end, and specialised
--  routines for the generation of calling stubs.

--  XXX The latter should be moved away to a Ada_Be.Idl2Ada.Stubs
--  child unit one day.

--  $Id$

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Idlac_Flags;           use Idlac_Flags;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Value_Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;
with Ada_Be.Idl2Ada.Skel;
with Ada_Be.Idl2Ada.IR_Info;

with Ada_Be.Mappings; use Ada_Be.Mappings;

with Errors;                use Errors;
with Utils;                 use Utils;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Ada_Be.Idl2Ada is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada");
   procedure O is new Ada_Be.Debug.Output (Flag);

   ------------------------------------------
   -- The current language mapping variant --
   ------------------------------------------

   type Mapping_Access is access Ada_Be.Mappings.Mapping_Type'Class;
   Mapping : Mapping_Access;

   ---------------------------------------------
   -- The current state of the code generator --
   ---------------------------------------------

   type Library_Unit_Data is array (Unit_Kind) of Compilation_Unit;

   type Scope_State is record
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
      Node : in Node_Id);

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   function Access_Type_Name (Node : in Node_Id)
     return String;
   --  Generates a name for an access to objet type.
   --  The rule used is to take the ada_type_name,
   --  replacing '.' with '_', and appending "_Access".
   --  Should be in expansion, but it would require too much work
   --  to do it now.

   procedure Gen_Repository_Id
     (Node : in     Node_Id;
      CU   : in out Compilation_Unit);
   --  Generate the RepositoryId for an entity.

   procedure Gen_Is_A
     (Node       : in Node_Id;
      Stubs_Spec : in out Compilation_Unit;
      Stubs_Body : in out Compilation_Unit);
   --  Generate code for Repository_Id and Is_A
   --  object reference operation.

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

   function TC_Name
     (Node : Node_Id)
      return String;

   procedure Gen_Convert_Forward_Declaration
     (CU : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate package Convert if necessary for
   --  valuetypes and interfaces.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   procedure Generate
     (Use_Mapping :    Ada_Be.Mappings.Mapping_Type'Class;
      Node        : in Node_Id;
      Implement   :    Boolean                            := False;
      Intf_Repo   :    Boolean                            := False;
      To_Stdout   :    Boolean                            := False)
   is
      S_Node : Node_Id;
      It : Node_Iterator;
   begin
      pragma Assert (Is_Repository (Node));

      Mapping := new Mappings.Mapping_Type'Class'(Use_Mapping);
      Init (It, Contents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, S_Node);
         Gen_Scope
           (S_Node, Implement, Intf_Repo, To_Stdout,
            Current_Scope => null);
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
   begin
      St :=
        (Stubs =>
           (Unit_Spec => New_Package (Stubs_Name, Unit_Spec),
            Unit_Body => New_Package (Stubs_Name, Unit_Body)),
         Skel =>
           (Unit_Spec => New_Package (Skel_Name, Unit_Spec),
            Unit_Body => New_Package (Skel_Name, Unit_Body)),
         Helper =>
           (Unit_Spec => New_Package (Helper_Name, Unit_Spec),
            Unit_Body => New_Package (Helper_Name, Unit_Body)),
         IR_Info =>
           (Unit_Spec => New_Package (IR_Info_Name, Unit_Spec),
            Unit_Body => New_Package (IR_Info_Name, Unit_Body)),
         Impl =>
           (Unit_Spec => New_Package (Impl_Name, Unit_Spec),
            Unit_Body => New_Package (Impl_Name, Unit_Body)),
         Value_Skel =>
           (Unit_Spec => New_Package (Value_Skel_Name, Unit_Spec),
            Unit_Body => New_Package (Value_Skel_Name, Unit_Body)),
         Delegate =>
           (Unit_Spec => New_Package (Delegate_Name, Unit_Spec),
            Unit_Body => New_Package (Delegate_Name, Unit_Body)));
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
         Add_With
           (S.Helper (Unit_Spec), "PolyORB.Any",
            Elab_Control => Elaborate_All,
            No_Warnings => True);
         --  Work-around for GNAT bug 9530-011.

         Helper.Gen_Body_Prelude (S.Helper (Unit_Body));

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

      Helper.Gen_Spec_Postlude (S.Helper (Unit_Spec));
      Helper.Gen_Body_Postlude (S.Helper (Unit_Body));
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
      Node : in Node_Id) is
   begin
      case Kind (Node) is
         when K_Operation =>
            declare
               Op_Name : constant String
                 := Ada_Operation_Name (Node);
               Is_Function : constant Boolean
                 := Kind (Operation_Type (Node)) /= K_Void;
               Original_VT_Name : constant String
                 := Ada_Full_Name
                 (Oldest_Supporting_ValueType (Node));
               Self_Expr : constant String
                 := Self_For_Operation (Mapping, Node);
            begin
               if not Is_Implicit_Inherited (Node) then
                  Add_With (CU, Parent_Scope_Name (Node)
                            & Value_Skel.Suffix);
                  Add_With (CU, "CORBA.Impl");
                  Gen_Operation_Profile
                    (CU, Node,
                     Ada_Type_Defining_Name
                     (Parent_Scope (Node)));
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
                  Add_With (CU, "PolyORB.CORBA_P.Exceptions");
                  PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Inv_Objref;");
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

         Add_With
           (S.Helper (Unit_Spec), "PolyORB.Any",
            Elab_Control => Elaborate_All,
            No_Warnings => True);
         --  Work-around for GNAT bug 9530-011.

         Helper.Gen_Body_Prelude (S.Helper (Unit_Body));
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
                     --  Ensure current unit has a non-empty
                     --  spec, if it has child packages.
                     if Kind (Node) /= K_Repository then
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
                              --  Does not happen.
                              raise Program_Error;
                        end case;
                        PL (S.Stubs (Unit_Spec), Name (Decl_Node));
                     end if;

                     Gen_Scope
                       (Decl_Node, Implement, Intf_Repo, To_Stdout,
                        Current_Scope => S);
                  else
                     if Kind (Decl_Node) = K_Forward_Interface then
                        --  in case of a forward declaration
                        Helper.Gen_Forward_Interface_Spec
                          (S.Helper (Unit_Spec), Decl_Node);
                        Helper.Gen_Forward_Interface_Body
                          (S.Helper (Unit_Body), Decl_Node);
                     end if;

                     Gen_Node_Stubs_Spec
                       (S.Stubs (Unit_Spec), Decl_Node);
                     Gen_Node_Stubs_Body_Dyn
                       (S.Stubs (Unit_Body), Decl_Node);

                     --  Exception declarations cause
                     --  generation of a Get_Members procedure.

                     Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Decl_Node);
                     Helper.Gen_Node_Body (S.Helper (Unit_Body), Decl_Node);

                     if Intf_Repo then
                        IR_Info.Gen_Node_Spec
                          (S.IR_Info (Unit_Spec), Decl_Node);
                        IR_Info.Gen_Node_Body
                          (S.IR_Info (Unit_Body), Decl_Node);
                     end if;
                  end if;

               end loop;
            end;

         when K_Interface =>

            --  Object reference type

            Gen_Client_Stub_Type_Declaration
              (S.Stubs (Unit_Spec), Node);

            if not Abst (Node) then

               Skel.Gen_Node_Spec
                 (S.Skel (Unit_Spec), Node, Is_Delegate => False);
               Skel.Gen_Node_Body
                 (S.Skel (Unit_Body), Node, Is_Delegate => False);

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
                      "  new PortableServer.Servant_Base"
                      & " with private;");
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

                  Add_With (S.Impl (Unit_Body), Skel_Name,
                            Use_It => False,
                            Elab_Control => Elaborate,
                            No_Warnings => True);
               end if;
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
                     Gen_Node_Stubs_Spec
                       (S.Stubs (Unit_Spec), Export_Node);

                     Gen_Node_Stubs_Body_Dyn
                       (S.Stubs (Unit_Body), Export_Node);

                     --  No code produced per-node
                     --  in skeleton spec.
                     if not Abst (Node) then
                        Skel.Gen_Node_Body
                          (S.Skel (Unit_Body), Export_Node,
                           Is_Delegate => False);

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

                     Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Export_Node);
                     Helper.Gen_Node_Body (S.Helper (Unit_Body), Export_Node);

                     if Intf_Repo then
                        IR_Info.Gen_Node_Spec
                          (S.IR_Info (Unit_Spec), Export_Node);
                        IR_Info.Gen_Node_Body
                          (S.IR_Info (Unit_Body), Export_Node);
                     end if;
                  end if;

                  --  Methods inherited from parents other that
                  --  the first one are added to the interface's
                  --  exports list by the expander.

               end loop;
            end;

            Gen_Repository_Id (Node, S.Stubs (Unit_Spec));
            Gen_Is_A (Node, S.Stubs (Unit_Spec), S.Stubs (Unit_Body));
            Gen_Local_Is_A (S.Stubs (Unit_Body), Node);

            --  CORBA 2.3
            Helper.Gen_Node_Spec (S.Helper (Unit_Spec), Node);
            Helper.Gen_Node_Body (S.Helper (Unit_Body), Node);

            if Intf_Repo then
               IR_Info.Gen_Node_Spec (S.IR_Info (Unit_Spec), Node);
               IR_Info.Gen_Node_Body (S.IR_Info (Unit_Body), Node);
            end if;

            Gen_Convert_Forward_Declaration (S.Stubs (Unit_Spec), Node);

            if not Abst (Node) then
               Skel.Gen_Body_Common_End
                 (S.Skel (Unit_Body), Node, Is_Delegate => False);

               if Generate_Delegate then
                  Skel.Gen_Body_Common_End
                    (S.Delegate (Unit_Body), Node,
                     Is_Delegate => True);
               end if;
            end if;

         when others =>
            pragma Assert (False);
            --  This never happens.

            null;
      end case;

      if In_Scope /= null then
         return;
      end if;

      Helper.Gen_Spec_Postlude (S.Helper (Unit_Spec));
      Helper.Gen_Body_Postlude (S.Helper (Unit_Body));
      if Intf_Repo then
         IR_Info.Gen_Body_Postlude (S.IR_Info (Unit_Body));
      end if;

      if Kind (Node) = K_Ben_Idl_File
        and then Is_Unknown (Node) then
         return;
         --  Do not attempt to generate a 'file' scope if
         --  there was no actual IDL file (case of a tree that
         --  is synthetised from a DSA service specification,
         --  for example.)
      end if;

      declare
         Is_Abstract_Node : Boolean := False;
         --  No skel and impl packages are generated
         --  for abstract interfaces.
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
     (Node : in     Node_Id;
      CU   : in out Compilation_Unit)
   is
   begin
      NL (CU);
      PL (CU, Repository_Id_Name (Node)
          & " : constant Standard.String");
      PL (CU, "  := """ & Idl_Repository_Id (Node) & """;");
   end Gen_Repository_Id;

   --------------
   -- Gen_Is_A --
   --------------

   procedure Gen_Is_A
     (Node       : in Node_Id;
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
          & Ada_Type_Defining_Name (Node)
          & ";");
      PL (Stubs_Spec, "   Logical_Type_Id : Standard.String)");
      PL (Stubs_Spec, "  return CORBA.Boolean;");

      Divert (Stubs_Spec, Private_Declarations);
      NL (Stubs_Spec);
      PL (Stubs_Spec, "function Is_A");
      PL (Stubs_Spec, "  (Logical_Type_Id : Standard.String)");
      PL (Stubs_Spec, "  return CORBA.Boolean;");
      Divert (Stubs_Spec, Visible_Declarations);

      --  Implementation

      Add_With (Stubs_Body, "CORBA.Object");

      NL (Stubs_Body);
      PL (Stubs_Body, "--  The visible Is_A object reference");
      PL (Stubs_Body, "--  operation (a dispatching operation");
      PL (Stubs_Body, "--  of all object reference types).");
      NL (Stubs_Body);
      PL (Stubs_Body, "function Is_A");
      PL (Stubs_Body, "  (Self : "
          & Ada_Type_Defining_Name (Node)
          & ";");
      PL (Stubs_Body, "   Logical_Type_Id : Standard.String)");
      PL (Stubs_Body, "  return CORBA.Boolean");
      PL (Stubs_Body, "is");
      PL (Stubs_Body, "begin");
      II (Stubs_Body);
      PL (Stubs_Body, "return False");
      NL (Stubs_Body);
      PL (Stubs_Body, "  or else Is_A (Logical_Type_Id)");
      II (Stubs_Body);
      PL (Stubs_Body,
          "--  Locally check class membership for this interface");
      DI (Stubs_Body);
      NL (Stubs_Body);
      PL (Stubs_Body, "  or else CORBA.Object.Is_A");
      PL (Stubs_Body,
          "           (CORBA.Object.Ref (Self), Logical_Type_Id);");

      II (Stubs_Body);
      PL (Stubs_Body,
          "--  Fall back to a remote membership check (may involve");
      PL (Stubs_Body,
          "--  an actual request invocation on Self).");
      DI (Stubs_Body);

      NL (Stubs_Body);
      DI (Stubs_Body);
      PL (Stubs_Body, "end Is_A;");
   end Gen_Is_A;

   ----------------------
   --  Gen_Local_Is_A  --
   ----------------------

   procedure Gen_Local_Is_A
     (CU   : in out Compilation_Unit;
      Node : in Node_Id)
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
      PL (CU, "  (Logical_Type_Id : Standard.String)");
      PL (CU, "  return CORBA.Boolean");
      PL (CU, "is");
      PL (CU, "begin");
      II (CU);

      --  An instance of a type verifies Is_A for that type...

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
           & Ada_Type_Defining_Name (Node)
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
                  --  semantics, it explains why abstract interfaces
                  --  should inherit directly from CORBA.AbstractBase.Ref
                  --  and not from CORBA.Object.Ref
                  --  However, I leave it like that because
                  --  it requires a lot of code rewriting,
                  --  all the current support for abstract interfaces is wrong
                  --  (mainly because abstract interfaces can refer
                  --  to valutypes).
               else
                  Add_With (CU, "CORBA.Object");
                  Put (CU, "CORBA.Object.Ref");
               end if;

            when K_ValueType =>
               Add_With (CU, "CORBA.Value");
               Put (CU, "CORBA.Value.Base");

            when others =>
               raise Program_Error;
               --  Never happens.

         end case;

      else
         Map_Type_Name (Mapping, Primary_Parent, Unit, Typ);
         Add_With (CU, -Unit);
         Put (CU, -Typ);
      end if;

      PL (CU, " with null record;");
      --  The type is not produced as a private extension
      --  declaration, because we may need to use it as
      --  a generic actual parameter to instanciate
      --  CORBA.Forward.

   end Gen_Client_Stub_Type_Declaration;

   ---------------------------------------
   --  Gen_Convert_Forward_Declaration  --
   ---------------------------------------

   procedure Gen_Convert_Forward_Declaration
     (CU : in out Compilation_Unit;
      Node : in Node_Id) is
      Forward_Node : Node_Id;
   begin
      pragma Assert ((Kind (Node) = K_Interface)
                     or else (Kind (Node) = K_ValueType));
      Forward_Node := Forward (Node);
      if Forward_Node /= No_Node then
         --  This interface has a forward declaration.

         NL (CU);
         PL (CU, "package Convert_Forward is");
         Put (CU, "  new "
              & Ada_Full_Name (Forward_Node)
              & ".Convert ("
              & Ada_Type_Defining_Name (Node)
              & ");");
         Add_With (CU, Ada_Full_Name
                   (Definition (Node).Parent_Scope));
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
      Primary_Parent : constant Node_Id
        := Idl_Fe.Tree.Synthetic.Primary_Parent (Node);
   begin
      pragma Assert (Kind (Node) = K_Interface);
      pragma Assert (not Abst (Node));
      --  No skel package is generated for abstract interfaces.

      NL (CU);
      PL (CU, "type Object is");
      if Primary_Parent = No_Node then
         Add_With (CU, "PortableServer");
         Put (CU, "  ");
         Put (CU, "new PortableServer.Servant_Base");
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
                  --  Make it so that the skeleton unit for
                  --  an interface is elaborated after those
                  --  of all its parents.
               end if;
            end loop;
         end;
      end if;
      if Full_View then
         PL (CU, " with record");
         II (CU);
         PL (CU, "--  Insert components to hold the state");
         PL (CU, "--  of the implementation object.");
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
            --  I tried to put a "with null record", but
            --  primitives of CORBA.Value.Box have to be overriden.
            --  More simple with a subtype.

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

            if not Is_Implicit_Inherited (Node) then
               Gen_Operation_Profile
                 (CU, Node, "in " & Ada_Type_Defining_Name
                  (Parent_Scope (Node)));
               PL (CU, ";");
               if Original_Node (Node) = No_Node then
                  --  A real (not expanded) operation
                  Gen_Repository_Id (Node, CU);
               end if;
            end if;

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
            NL (CU);
            PL (CU, Ada_Name (Node) & " : exception;");
--             PL (CU, Repository_Id_Name (Node)
--                 & " : constant CORBA.RepositoryId");
--             PL (CU, "  := CORBA.To_CORBA_String ("""
--                 & Idl_Repository_Id (Node) & """);");
            Gen_Repository_Id (Node, CU);
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
                        if Is_Ref
                          and then not Is_Array then
                           --  A typedef where the <type_spec>
                           --  denotes an interface type, and
                           --  which is not an array declaration.
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
                              Gen_Node_Stubs_Spec (CU, Bound_Node);
                              Put (CU, " - 1");
                           end loop;
                           PL (CU, ") of");
                        else
                           Put (CU, "  ");
                           if not (Is_Ref or else Is_Fixed) then
                              Put (CU, "new ");
                           end if;
                        end if;

                        Gen_Node_Stubs_Spec (CU, T_Type (Node));
                        PL (CU, ";");
                     end;

                     if Original_Node (Decl_Node) = No_Node then
                        begin
                           Gen_Repository_Id (Decl_Node, CU);
                        exception
                           when Constraint_Error =>
                              PL (CU, "XXX NO REPO ID for decl:"
                                  & Name (Decl_Node));
                           when others =>
                              raise;
                        end;
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
                  Add_With (CU, "PolyORB.Sequences.Bounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new PolyORB.Sequences.Bounded");
                  PL (CU, "    ("
                      & (-Typ)
                      & ", " & Img (Integer_Value (B_Node))
                      & ");");
               else
                  Add_With (CU, "PolyORB.Sequences.Unbounded",
                            Use_It => False,
                            Elab_Control => Elaborate_All);
                  PL (CU, "  new PolyORB.Sequences.Unbounded");
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
            Gen_Constant_Value (CU, Expression (Node));
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

   -------------------------
   -- Gen_Node_Stubs_Body --
   -------------------------

   function Justify (S : in String; Max : in Integer) return String
   is
      WS : String (1 .. 50);
   begin
      if S'Length >= Max or else Max > WS'Length then
         return S;
      end if;
      Move (S, WS, Pad => ' ');
      return Head (WS, Max);
   end Justify;

   procedure Gen_Forward_Conversion
     (CU        : in out Compilation_Unit;
      T_Node    : in     Node_Id;
      Direction : in     String;
      What      : in     String)
   is
      NT : Node_Id := T_Node;
   begin
      --  XXX the following loop is dubious.
      --  Most likely, it runs exactly once every
      --  time.
      while (Kind (NT) = K_Scoped_Name) loop
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
               Is_Function : constant Boolean
                 := Kind (O_Type) /= K_Void;
               Raise_Something : constant Boolean
                 := not Is_Empty (Raises (Node));

            begin
               Add_With (CU, "CORBA",
                         Use_It    => True,
                         Elab_Control => Elaborate_All);

               Add_With (CU, "PolyORB.CORBA_P.Exceptions");
               Add_With (CU, "PolyORB.Any.NVList");
               Add_With (CU, "PolyORB.Requests");
               Add_With (CU, "PolyORB.Types");

               Add_With (CU, "CORBA.Object");

               Gen_Operation_Profile
                 (CU, Node,
                  Ada_Type_Defining_Name (Parent_Scope (Node)));

               NL (CU);
               PL (CU, "is");
               II (CU);

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     if not Is_Returns (P_Node) then

                        declare
                           Arg_Name : constant String
                             := Ada_Name (Declarator (P_Node));
                           Prefix   : constant String
                             := Helper_Unit (Param_Type (P_Node));
                        begin
                           PL (CU, T_Arg_Name & Arg_Name
                               & " : PolyORB.Types.Identifier");
                           PL (CU, "  := PolyORB.Types.To_PolyORB_String ("""
                               & Arg_Name & """);");

                           Add_With (CU, Prefix);

                           PL (CU, T_Argument & Arg_Name & " : CORBA.Any");
                           PL (CU, "  := " & Prefix & ".To_Any");
                           Put (CU, "  (");
                           Gen_Forward_Conversion
                             (CU, Param_Type (P_Node),
                              "From_Forward", Arg_Name);
                           PL (CU, ");");
                        end;
                     end if;
                  end loop;
               end;

               NL (CU);
               PL (CU, T_Operation_Name & " : constant Standard.String");
               PL (CU, "  := """ & Idl_Operation_Id (Node) & """;");
               PL (CU, T_Self_Ref & " : CORBA.Object.Ref");
               PL (CU, "  := CORBA.Object.Ref ("
                   & Self_For_Operation (Mapping, Node) & ");");
               NL (CU);
               PL (CU, T_Request
                   & " : PolyORB.Requests.Request_Access;");

               PL (CU, T_Arg_List & " : PolyORB.Any.NVList.Ref;");

               if Raise_Something then
                  Add_With (CU, "CORBA.ExceptionList");
                  PL (CU, T_Excp_List
                    & " : CORBA.ExceptionList.Ref;");
               end if;

               PL (CU, T_Result & " : PolyORB.Any.NamedValue;");
               PL (CU, T_Result_Name
                   & " : CORBA.String := To_CORBA_String (""Result"");");

               DI (CU);
               PL (CU, "begin");
               II (CU);

               NL (CU);
               PL (CU, "if CORBA.Object.Is_Nil (" & T_Self_Ref & ") then");
               II (CU);
               PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Inv_Objref;");
               DI (CU);
               PL (CU, "end if;");
               NL (CU);

               PL (CU, "--  Create argument list");
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
                           Arg_Name : constant String
                             := Ada_Name (Declarator (P_Node));
                        begin
                           PL (CU, "PolyORB.Any.NVList.Add_Item");
                           PL (CU, "  (" & T_Arg_List & ",");
                           II (CU);
                           PL (CU, T_Arg_Name & Arg_Name & ",");
                           PL (CU, T_Argument & Arg_Name & ",");
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
                     Add_With_Entity (CU, E_Node);

                     if First then
                        NL (CU);
                        PL (CU, "--  Create exceptions list.");
                        NL (CU);
                        PL (CU, "CORBA.ExceptionList.Create_List ("
                            & T_Excp_List & ");");
                        First := False;
                     end if;

                     PL (CU, "CORBA.ExceptionList.Add");
                     PL (CU, "  (" & T_Excp_List & ",");
                     II (CU);
                     PL (CU, TC_Name (E_Node) & ");");
                     DI (CU);

                  end loop;
               end;

               PL (CU, "--  Set result type (maybe void)");
               PL (CU, T_Result);
               PL (CU, "  := (Name => PolyORB.Types.Identifier ("
                 & T_Result_Name & "),");
               PL (CU, "      Argument => Get_Empty_Any");
               PL (CU, "  ("
                   & TC_Name (Original_Operation_Type (Node)) & "),");
               II (CU);
               PL (CU, "Arg_Modes => 0);");
               DI (CU);
               NL (CU);

               PL (CU, "PolyORB.Requests.Create_Request");
               PL (CU, "  (Target    => CORBA.Object.To_PolyORB_Ref");
               II (CU);
               PL (CU, "(CORBA.Object.Ref ("
                   & Self_For_Operation (Mapping, Node) & ")),");
               PL (CU, "Operation => " & T_Operation_Name & ",");
               PL (CU, "Arg_List  => " & T_Arg_List & ",");
               PL (CU, "Result    => " & T_Result & ",");

               if Raise_Something then
                  PL (CU, "Exc_List  => CORBA.ExceptionList.To_PolyORB_Ref ("
                      & T_Excp_List & "),");
               end if;
               PL (CU, "Req       => " & T_Request & ");");
               DI (CU);

               NL (CU);
               PL (CU, "PolyORB.Requests.Invoke ("
                   & T_Request & ");");

               PL (CU, "if not Is_Empty (" & T_Request
                   & ".Exception_Info) then");
               II (CU);
               PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_From_Any");
               PL (CU, "  (" & T_Request & ".Exception_Info);");
               DI (CU);
               PL (CU, "end if;");

               PL (CU, "PolyORB.Requests.Destroy_Request");
               PL (CU, "  (" & T_Request & ");");

               if Response_Expected then

                  NL (CU);
                  PL (CU, "--  Request has been synchronously invoked.");

                  declare
                     It     : Node_Iterator;
                     P_Node : Node_Id;
                     First  : Boolean := True;
                  begin
                     if Kind (Original_Operation_Type (Node)) /= K_Void then
                        NL (CU);
                        PL (CU, "--  Retrieve return value.");

                        if Is_Function then
                           Put (CU, "return ");
                        else
                           Put (CU, "Returns := ");
                        end if;

                        declare
                           Prefix : constant String
                             := Helper_Unit
                             (Original_Operation_Type (Node));
                        begin
                           Add_With (CU, Prefix);

                           Gen_Forward_Conversion
                             (CU, Original_Operation_Type (Node),
                              "To_Forward",
                              Prefix & ".From_Any"
                              & ASCII.LF & "  ("
                              & T_Result & ".Argument)");
                           PL (CU, ";");
                        end;
                     end if;

                     Init (It, Parameters (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, P_Node);

                        if not Is_Returns (P_Node) then

                           if Mode (P_Node) =  Mode_Inout
                             or else Mode (P_Node) = Mode_Out
                           then
                              if First then
                                 NL (CU);
                                 PL (CU,
                                     "--  Retrieve 'out' argument values.");
                                 NL (CU);
                                 First := False;
                              end if;

                              declare
                                 Arg_Name : constant String
                                   := Ada_Name (Declarator (P_Node));
                              begin
                                 Put (CU, Arg_Name & " := ");
                                 Gen_Forward_Conversion
                                   (CU, Param_Type (P_Node),
                                    "To_Forward",
                                    Helper_Unit (Param_Type (P_Node))
                                    & ".From_Any"
                                    & ASCII.LF & "  ("
                                    & T_Argument
                                    & Arg_Name);
                                 PL (CU, ");");
                              end;
                           end if;
                        end if;
                     end loop;
                  end;
               end if;

               DI (CU);
               PL (CU, "end " & O_Name & ";");
            end;

         when K_Exception =>
            Add_With (CU, "PolyORB.CORBA_P.Exceptions");
            NL (CU);
            PL (CU, "procedure Get_Members");
            PL (CU, "  (From : Ada.Exceptions.Exception_Occurrence;");
            PL (CU, "   To   : out "
                & Ada_Name (Members_Type (Node))
                & ") is");
            PL (CU, "begin");
            II (CU);
            PL (CU, "PolyORB.CORBA_P.Exceptions.User_Get_Members (From, To);");
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
      Return_Type : in String;
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
         Put (CU,
              "return "
              & Return_Type);
         DI (CU);
      end;
   end Gen_Initializer_Profile;

   ---------------------------
   -- Gen_Operation_Profile --
   ---------------------------

   procedure Gen_Operation_Profile
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Object_Type : in     String;
      With_Name   : in     Boolean          := True;
      Is_Delegate : in     Boolean          := False)
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

            --  In .value_skel, we need the profile
            --  of the subprogram without the name, to create
            --  an access to subprogram type
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
            if Is_Delegate then
               Put (CU, " is <>");
            end if;

         when K_Param =>

            Gen_Operation_Profile
              (CU, Declarator (Node), Object_Type);
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
               Unit, Typ : ASU.Unbounded_String;
            begin
               Map_Type_Name (Mapping, T_Node, Unit, Typ);
               Add_With (CU, -Unit);
               Put (CU, -Typ);

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
                  --  (see above) --  FIXME: code duplication.

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
            Put (CU, Ada_Name (Enum_Value (Node)));

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

   ----------------------------
   -- Ada_Type_Defining_Name --
   ----------------------------

   function Ada_Type_Defining_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface =>
            return Calling_Stubs_Type (Mapping, Node);

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
              ("Improper call of Ada_Type_Defining_Name with a "
               & Node_Kind'Image (NK), Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_Type_Defining_Name;

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

   -------------
   -- TC_Name --
   -------------

   function TC_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin

      case NK is

         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            return TC_Name (Forward (Node));

         when
           K_Interface         |
           K_ValueType         |

           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Boxed_ValueType   |

           K_Sequence_Instance |
           K_String_Instance =>

            return Helper_Unit (Node) & ".TC_" & Ada_Name (Node);

         when K_Declarator =>
            declare
               P : constant Node_Id := Parent (Node);
            begin
               if Kind (P) = K_Type_Declarator then
                  declare
                     T_Node : constant Node_Id := T_Type (P);
                  begin
                     case Kind (T_Node) is
                        when
                          K_Interface         |
                          K_Forward_Interface |
                          K_ValueType         |
                          K_Scoped_Name       |
                          K_Forward_ValueType =>
                           return TC_Name (T_Node);
                        when others =>
                           return Helper_Unit (Node) & ".TC_"
                             & Ada_Name (Node);
                     end case;
                  end;
               else
                  return Helper_Unit (Node) & ".TC_" & Ada_Name (Node);
               end if;
            end;

         when K_Scoped_Name =>
            return TC_Name (Value (Node));

         when K_Void =>
            return "CORBA.TC_Void";

         when K_Short =>
            return "CORBA.TC_Short";

         when K_Long =>
            return "CORBA.TC_Long";

         when K_Long_Long =>
            return "CORBA.TC_Long_Long";

         when K_Unsigned_Short =>
            return "CORBA.TC_Unsigned_Short";

         when K_Unsigned_Long =>
            return "CORBA.TC_Unsigned_Long";

         when K_Unsigned_Long_Long =>
            return "CORBA.TC_Unsigned_Long_Long";

         when K_Char =>
            return "CORBA.TC_Char";

         when K_Wide_Char =>
            return "CORBA.TC_Wchar";

         when K_Boolean =>
            return "CORBA.TC_Boolean";

         when K_Float =>
            return "CORBA.TC_Float";

         when K_Double =>
            return "CORBA.TC_Double";

         when K_Long_Double =>
            return "CORBA.TC_Long_Double";

         when K_String =>
            return "CORBA.TC_String";

         when K_Wide_String =>
            return "CORBA.TC_Wide_String";

         when K_Octet =>
            return "CORBA.TC_Octet";

         when K_Any =>
            return "CORBA.TC_Any";

         when K_Object =>
            return "CORBA.Object.Helper.TC_Object";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("This is TC_Name : A " & Node_Kind'Image (NK)
               & " does not denote a type.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end TC_Name;

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
         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            return Helper_Unit (Forward (Node));
            --  Different from Ada_Helper_Name (Node).

         when K_Scoped_Name =>
            return Helper_Unit (Value (Node));
            --  Potentially different from Ada_Helper_Name (Node).

         when others =>
            return Ada_Helper_Name (Node);
      end case;
   end Helper_Unit;

   ----------------------
   -- Access_Type_Name --
   ----------------------

   function Access_Type_Name (Node : in Node_Id) return String is
      Name : String := Ada_Type_Name (Node);
   begin
      for I in Name'Range loop
         if Name (I) = '.' then
            Name (I) := '_';
         end if;
      end loop;
      return Name & "_Access";
   end Access_Type_Name;

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

   ------------------------
   -- Repository_Id_Name --
   ------------------------

   function Repository_Id_Name
     (Node : Node_Id)
     return String is
   begin
      return Ada_Name (Repository_Id_Identifier (Node));
   end Repository_Id_Name;

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

   -----------------
   -- Ada_TC_Name --
   -----------------

   function Ada_TC_Name
     (Node : Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
      Prefix : constant String := "TC_";
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface |
            --          K_ValueType         |
            --          K_Forward_ValueType |
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
            return Prefix & "Wide_Char";

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

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("No TypeCode for " & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_TC_Name;

   ---------------------
   -- Ada_Helper_Name --
   ---------------------

   function Ada_Helper_Name
     (Node : in     Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Interface | K_ValueType =>
            return Client_Stubs_Unit_Name (Mapping, Node)
              & Helper.Suffix;

         when
           K_Forward_Interface | K_Forward_ValueType =>
            return Parent_Scope_Name (Node) & Helper.Suffix;

         when
           K_Sequence_Instance |
           K_String_Instance   |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Declarator        =>

            return Client_Stubs_Unit_Name
              (Mapping, Parent_Scope (Node))
              & Helper.Suffix;

         when K_Scoped_Name =>
            return Ada_Helper_Name (Value (Node));

         when K_Short           |
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
            return "CORBA";

         when K_Object =>
            return "CORBA.Object.Helper";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("No helpers for " & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;
      end case;
   end Ada_Helper_Name;

   ----------------------
   -- Ada_Full_TC_Name --
   ----------------------

   function Ada_Full_TC_Name
     (Node : Node_Id)
     return String is
   begin
      return Ada_Helper_Name (Node) & "." & Ada_TC_Name (Node);
   end Ada_Full_TC_Name;

end Ada_Be.Idl2Ada;
