------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               A D A _ B E . I D L 2 A D A . I R _ I N F O                --
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

--  $Id$

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Utils;                 use Utils;
with Errors;                use Errors;

package body Ada_Be.Idl2Ada.IR_Info is

   Flag : constant Natural := Ada_Be.Debug.Is_Active
     ("ada_be.idl2ada.ir_info");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   CRR : constant String := "CORBA.Repository_Root";

   procedure Gen_IR_Function_Prologue
     (CU       : in out Compilation_Unit;
      Node     :        Node_Id;
      For_Body :        Boolean);

   function Ada_IR_Name (Node : Node_Id) return String;
   --  The defining name for the IR object corresponding to Node.

   function Ada_IR_Info_Name (Node : Node_Id) return String;
   --  The name of the package that contains the IR object for Node.

   function Ada_Full_IR_Name (Node : Node_Id) return String;
   --  The fully qualified name for the IR object corresponding
   --  to Node.

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Gen_Parent_Container_Dcl
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the declaration of a Container_Ref corresponding
   --  to the container corresponding to Node's parent scope.

   procedure Gen_Parent_Container_Lookup
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the code to look up Node's name in the container
   --  object that describes its parent scope.

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an interface declaration

   procedure Gen_Operation_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an operation declaration

   procedure Gen_Module_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a module declaration

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
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id;
      Decl_Node : in     Node_Id);
   --  generate lines to fill in an array typecode
   --  only used in the type_declarator part of gen_node_body

   function Raise_From_Any_Name (Node : in Node_Id) return String;
   --  Return the name of a procedure that raises that exception
   --  from an occurrence stored in an Any.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   function Ada_IR_Name (Node : Node_Id) return String is
      Prefix : constant String := "IR_";
      NK : constant Node_Kind := Kind (Node);

      function Get_Primitive (S : String) return String;
      --  Return a call to Get_Primitive for the named
      --  PrimitiveDef kind.

      function Get_Primitive (S : String) return String is
      begin
         return "Get_Primitive" & ASCII.LF
           & "  (Root_Repo_Ref, CORBA.Repository_Root." & S & ")";
      end Get_Primitive;

   begin
      case NK is
         when
           K_Module            |
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
           K_Operation         |
           K_Declarator        =>
            return Prefix & Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_IR_Name (Value (Node));

         when K_Short =>
            return Get_Primitive ("pk_short");

         when K_Long =>
            return Get_Primitive ("pk_long");

         when K_Long_Long =>
            return Get_Primitive ("pk_longlong");

         when K_Unsigned_Short =>
            return Get_Primitive ("pk_ushort");

         when K_Unsigned_Long =>
            return Get_Primitive ("pk_ulong");

         when K_Unsigned_Long_Long =>
            return Get_Primitive ("pk_ulonglong");

         when K_Char =>
            return Get_Primitive ("pk_char");

         when K_Wide_Char =>
            return Get_Primitive ("pk_widechar");

         when K_Boolean =>
            return Get_Primitive ("pk_boolean");

         when K_Float =>
            return Get_Primitive ("pk_float");

         when K_Double =>
            return Get_Primitive ("pk_double");

         when K_Long_Double =>
            return Get_Primitive ("pk_longdouble");

         when K_String =>
            return Get_Primitive ("pk_string");

         when K_Wide_String =>
            return Get_Primitive ("pk_widestring");

         when K_Octet =>
            return Get_Primitive ("pk_octet");

         when K_Object =>
            return Get_Primitive ("pk_objref");

         when K_Any =>
            return Get_Primitive ("pk_any");

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("No IR object for "
               & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_IR_Name;

   function Ada_IR_Info_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Module            |
           K_Interface         =>
            return Ada_Full_Name (Node) & Suffix;

         when
           K_Forward_Interface =>
            return Parent_Scope_Name (Node) & Suffix;

            --          K_ValueType         |
            --          K_Forward_ValueType |
         when
           K_Sequence_Instance |
           K_String_Instance   |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Declarator        =>
            return Parent_Scope_Name (Node) & Suffix;

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
           K_Object             |
           K_Any                =>
            return CRR;

         when others =>
            --  Improper use: node N does not have a correspoding
            --  IR object.

            Error
              ("No IR info for " & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;
      end case;
   end Ada_IR_Info_Name;

   function Ada_Full_IR_Name (Node : Node_Id) return String is
   begin
      return Ada_IR_Info_Name (Node) & "." & Ada_IR_Name (Node);
   end Ada_Full_IR_Name;

   -------------------------
   -- Raise_From_Any_Name --
   -------------------------

   function Raise_From_Any_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_Exception);
      return "Raise_" & Ada_Name (Node) & "_From_Any";
   end Raise_From_Any_Name;

   --------------------
   -- Gen_Scope_Spec --
   --------------------

   procedure Gen_Node_Spec
     (CU        : in out Compilation_Unit;
      Node      :        Node_Id) is
   begin
      case Kind (Node) is

         when
           K_Interface |
           K_Operation |
           K_Module    =>
            Gen_IR_Function_Prologue (CU, Node, For_Body => False);

         when K_Enum =>
            Gen_Enum_Spec (CU, Node);

         when K_Type_Declarator =>
            if Is_Interface_Type (T_Type (Node)) then
               null;
            elsif Kind (T_Type (Node)) = K_Fixed then
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
            null;

         when K_Union =>
            Gen_Union_Spec (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Spec (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Spec (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Spec (CU, Node);
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
         when K_Operation =>
            if Original_Node (Node) = No_Node then
               --  Not for synthetic operations.
               Gen_Operation_Body (CU, Node);
            end if;

         when K_Module =>
            Gen_Module_Body (CU, Node);

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
            null;

         when K_Union =>
            Gen_Union_Body (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Body (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Body (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Body (CU, Node);
         when others =>
            null;

      end case;
   end Gen_Node_Body;

   ------------------------
   -- Gen_Interface_Spec --
   ------------------------

--    procedure Gen_Interface_Spec
--      (CU        : in out Compilation_Unit;
--       Node      : in     Node_Id) is
--    begin
--       Gen_IR_Function_Prologue (CU, Node, For_Body => False);
--    end Gen_Interface_Spec;

   ------------------------
   -- Gen_ValueType_Spec --
   ------------------------

   procedure Gen_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : in Node_Id)
   is
   begin
      null;
   end Gen_ValueType_Spec;

   ------------------------
   -- Gen_ValueType_Body --
   ------------------------

   procedure Gen_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : in Node_Id)
   is
   begin
      null;
   end Gen_ValueType_Body;

   ------------------------------
   -- Gen_IR_Function_Prologue --
   ------------------------------

   procedure Gen_IR_Function_Prologue
     (CU       : in out Compilation_Unit;
      Node     :        Node_Id;
      For_Body :        Boolean)
   is
      Name : constant String := Ada_IR_Name (Node);
   begin
      NL (CU);
      if not For_Body then
         Add_With (CU, CRR & ".IRObject");
      else
         PL (CU, "Cached_" & Name & " :");
         PL (CU, "  CORBA.Repository_Root.IRObject.Ref;");
         NL (CU);
      end if;
      PL (CU, "function " & Name);
      Put (CU, "  return CORBA.Repository_Root.IRObject.Ref");
      if not For_Body then
         PL (CU, ";");
      else
         NL (CU);
         PL (CU, "is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "if not CORBA.Repository_Root.IRObject.Is_Nil");
         PL (CU, "  (Cached_" & Name & ")");
         PL (CU, "then");
         II (CU);
         PL (CU, "return Cached_" & Name & ";");
         DI (CU);
         PL (CU, "end if;");
         NL (CU);
      end if;
   end Gen_IR_Function_Prologue;

   ------------------------------
   -- Gen_Parent_Container_Dcl --
   ------------------------------

   procedure Gen_Parent_Container_Dcl
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      PS_Node : constant Node_Id := Parent_Scope (Node);
   begin
      Add_With (CU, CRR & ".Container.Helper");
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "Container_Ref : "
          & "constant CORBA.Repository_Root.Container.Ref");

      case Kind (PS_Node) is
         when K_Interface | K_ValueType | K_Module =>
            --  The PS_Node corresponds to a container in
            --  the IR sense.
            Add_With (CU, Ada_IR_Info_Name (PS_Node));
            PL (CU, "  := CORBA.Repository_Root.Container.Helper.To_Ref");
            PL (CU, "  (" & Ada_Full_IR_Name (PS_Node) & ");");
         when others =>
            PL (CU, "  := The_Repository_Root_Ref;");
      end case;

   end Gen_Parent_Container_Dcl;

   ---------------------------------
   -- Gen_Parent_Container_Lookup --
   ---------------------------------

   procedure Gen_Parent_Container_Lookup
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Cached_Name : constant String := "Cached_" & Ada_IR_Name (Node);
   begin

      PL (CU, Cached_Name & " :=");
      PL (CU, "  CORBA.Repository_Root.IRObject.Helper.To_Ref");
      PL (CU, "  (CORBA.Repository_Root.Container.Lookup");
      II (CU);
      PL (CU, "(Container_Ref));");
      DI (CU);
      PL (CU, "if not CORBA.Repository_Root.IRObject.Is_Nil");
      PL (CU, "  (" & Cached_Name & ")");
      PL (CU, "then");
      II (CU);
      PL (CU, "return " & Cached_Name & ";");
      DI (CU);
      PL (CU, "end if;");
   end Gen_Parent_Container_Lookup;

   ------------------------
   -- Gen_Interface_Body --
   ------------------------

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      It : Node_Iterator;
      IRN : constant String := Ada_IR_Name (Node);
   begin
      Add_With (CU, CRR & ".InterfaceDef.Helper");

      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Base_Ifs : InterfaceDefSeq;");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      Init (It, Parents (Node));
      while not Is_End (It) loop
         declare
            A_Node : Node_Id;
         begin
            Get_Next_Node (It, A_Node);
            Add_With (CU, Ada_IR_Info_Name (A_Node));
            PL (CU, "Append");
            PL (CU, "  (Base_Ifs,");
            II (CU);
            PL (CU, "InterfaceDef.Helper.To_Ref");
            PL (CU, "  (" & Ada_Full_IR_Name (A_Node) & ");");
            DI (CU);
         end;
      end loop;

      PL (CU, "Cached_" & IRN);
      PL (CU, "  := CORBA.Repository_Root."
          & "Repository.Create_Interface");
      PL (CU, "  (Interface_Repository,");
      --  XXX WRONG must create the interface in the
      --  proper container!
      II (CU);
      PL (CU, "To_CORBA_String");
      PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      PL (CU, """" & Name (Node) & """,");
      PL (CU, """" & Image (Version (Node)) & """,");
      PL (CU, "Base_Ifs,");
      PL (CU, Boolean'Image (Abst (Node)) & ");");
      DI (CU);
      NL (CU);
      PL (CU, "return Cached_" & IRN);
      DI (CU);
      PL (CU, "end " & IRN & ";");
   end Gen_Interface_Body;

   ------------------------
   -- Gen_Operation_Body --
   ------------------------

   procedure Gen_Operation_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      It : Node_Iterator;
      IRN : constant String := Ada_IR_Name (Node);
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Dcl (CU, Node);
      Gen_Parent_Container_Lookup (CU, Node);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Params : ParDescriptionSeq;");
      PL (CU, "Exceptions : ExceptionDefSeq;");
      PL (CU, "Contexts : ContextIdSeq;");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      Init (It, Parameters (Node));
      while not Is_End (It) loop
         declare
            P_Node, T_Node : Node_Id;
         begin
            Get_Next_Node (It, P_Node);
            T_Node := Param_Type (P_Node);
            PL (CU, "Append");
            PL (CU, "  (Params,");
            II (CU);
            PL (CU, "(name => "
                & Ada_Name (Declarator (P_Node)) & ",");

            Add_With (CU, Ada_Helper_Name (T_Node));
            Add_With (CU, Ada_IR_Info_Name (T_Node));

            PL (CU, " type =>");
            II (CU);
            PL (CU, Ada_Full_TC_Name (T_Node) & ",");
            DI (CU);
            PL (CU, " type_def =>");
            II (CU);
            PL (CU, Ada_Full_IR_Name (T_Node) & ",");
            DI (CU);
            Put (CU, " mode => ");
            case Mode (P_Node) is
               when Mode_In =>
                  Put (CU, "PARAM_IN");
               when Mode_Inout =>
                  Put (CU, "PARAM_INOUT");
               when Mode_Out =>
                  Put (CU, "PARAM_OUT");
            end case;
            PL (CU, "));");
            DI (CU);
         end;
      end loop;

      --  XXX TODO!
--             Init (It, Exceptions (Node));
--             while not Is_End (It) loop

--                Next (It);
--             end loop;

      Add_With (CU, CRR & ".InterfaceDef.Helper");
      PL (CU, "Cached_" & IRN);
      PL (CU, "  := Create_Operation");
      PL (CU, "  (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref");
      II (CU);
      PL (CU, "(Container_Ref),");
      PL (CU, Repository_Id_Name (Node) & ",");
      PL (CU, Name (Node) & ",");
      PL (CU, """" & Image (Version (Node)) & """,");
      PL (CU, "Params,");
      PL (CU, "Exceptions,");
      PL (CU, "Contexts);");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
      PL (CU, "return Cached_" & IRN);
      DI (CU);
      PL (CU, "end " & IRN & ";");

   end Gen_Operation_Body;

   ------------------------
   -- Gen_Module_Body --
   ------------------------

   procedure Gen_Module_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      IRN : constant String := Ada_IR_Name (Node);
   begin
      Add_With (CU, CRR & ".ModuleDef.Helper");

      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);

      PL (CU, "Cached_" & IRN);
      PL (CU, "  := CORBA.Repository_Root."
          & "Repository.Create_Module");
      PL (CU, "  (Interface_Repository,");
      --  XXX WRONG must create the module in the proper container
      II (CU);
      PL (CU, "To_CORBA_String");
      PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      PL (CU, """" & Name (Node) & """,");
      PL (CU, """" & Image (Version (Node)) & """);");
      DI (CU);
      NL (CU);
      PL (CU, "return Cached_" & IRN);
      DI (CU);
      PL (CU, "end " & IRN & ";");
   end Gen_Module_Body;

   -------------------
   -- Gen_Enum_Spec --
   -------------------

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Enum_Spec;

   -------------------
   -- Gen_Enum_body --
   -------------------

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Enum_Body;

   -------------------------------
   -- Gen_Struct_Exception_Spec --
   -------------------------------

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Struct_Exception_Spec;

   -------------------------------
   -- Gen_Struct_Exception_Body --
   -------------------------------

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Struct_Exception_Body;

   --------------------
   -- Gen_Union_Spec --
   --------------------

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Union_Spec;

   --------------------
   -- Gen_Union_Body --
   --------------------

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Union_Body;

   ------------------------------
   -- Gen_Type_Declarator_Spec --
   ------------------------------

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
   begin
      null;
   end Gen_Type_Declarator_Spec;

   ------------------------------
   -- Gen_Type_Declarator_Body --
   ------------------------------

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
   begin
      null;
   end Gen_Type_Declarator_Body;

   -----------------------
   -- Gen_Sequence_Spec --
   -----------------------

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Sequence_Spec;

   -----------------------
   -- Gen_Sequence_Body --
   -----------------------

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Sequence_Body;

   --------------------
   -- Gen_Fixed_Spec --
   --------------------

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Fixed_Spec;

   --------------------
   -- Gen_Fixed_Body --
   --------------------

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      null;
   end Gen_Fixed_Body;

   ------------------
   -- Gen_Array_TC --
   ------------------

   procedure Gen_Array_TC
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id;
      Decl_Node : in     Node_Id)
   is

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id);

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id) is
         Bound_Node : Node_Id;
         Last_Bound : Boolean := False;
      begin
         Get_Next_Node (It, Bound_Node);
         if not Is_End (It) then
            Rec_Gen_Array_TC (CU, It, False, Index + 1, Type_Node, Decl_Node);
         else
            Last_Bound := True;
         end if;
         Add_With (CU, "CORBA");
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         Add_With (CU, "CORBA");
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
                 & Ada_Full_TC_Name (Type_Node));
         else
            Put (CU, ", To_Any (TC_"
                 & Img (Index + 1));
         end if;
         PL (CU, "));");
      end Rec_Gen_Array_TC;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Rec_Gen_Array_TC (CU, Bounds_It, True, 0, Type_Node, Decl_Node);
   end Gen_Array_TC;

end Ada_Be.Idl2Ada.IR_Info;
