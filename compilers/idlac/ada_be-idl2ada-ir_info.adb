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

   procedure Gen_Standard_Create_Parameters
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the actual parameters that are common to
   --  all create_* operations: id, name, and version.

   procedure Gen_Parent_Container_Dcl
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the declaration of a Container_Ref corresponding
   --  to the container corresponding to Node's parent scope.

   procedure Gen_IDLType
     (CU     : in out Compilation_Unit;
      T_Node : in     Node_Id;
      D_Node : in     Node_Id);
   --  Generate an IDLType object reference corresponding
   --  to the entity declared by declarator D_Node with the type
   --  denoted by T_Node (note, for arrays T_Node is the element
   --  type.) If D_Node is No_Node, no array bounds are assumed.

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

--    procedure Gen_Sequence_Spec
--      (CU        : in out Compilation_Unit;
--       Node      : in     Node_Id);
--    --  Generate the spec of the helper package for a sequence declaration

--    procedure Gen_Sequence_Body
--      (CU        : in out Compilation_Unit;
--       Node      : in     Node_Id);
--    --  Generate the body of the helper package for a sequence declaration

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a fixed type declaration

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a fixed type declaration

   procedure Gen_Array_IR
     (CU                : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node         : in     Node_Id);
   --  Generate code to create an ArrayDef IRObject
   --  (only used in the type_declarator part of gen_node_body).

   procedure Gen_Fixed_IR
     (CU   : in out Compilation_Unit;
      Node : in     Node_Id);
   --  Generate code to create a FixedDef IRObject
   --  (only used in the type_declarator part of gen_node_body).

   procedure Gen_Sequence_IR
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate code to create a SequenceDef IRObject
   --  (only used in the type_declarator part of gen_node_body).

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
           & "  (Get_IR_Root, CORBA.Repository_Root." & S & ")";
      end Get_Primitive;

   begin
      case NK is
         when K_Declarator =>
            if Original_Node (Node) /= No_Node
              and then Kind (Original_Node (Node)) /= K_Fixed
            then
               return Ada_IR_Name (Original_Node (Node));
            end if;
            return Prefix & Ada_Name (Node);

         when
           K_Module            |
           K_Interface         |
           K_Forward_Interface |
            --          K_ValueType         |
            --          K_Forward_ValueType |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Operation         =>
            return Prefix & Ada_Name (Node);

         when K_Scoped_Name =>
            return Ada_IR_Name (Value (Node));

         when K_Void =>
            return Get_Primitive ("pk_void");

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
            --  K_Sequence_Instance |
            --  K_String_Instance   |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Declarator        |
           K_Exception         =>
            return Parent_Scope_Name (Node) & Suffix;

         when K_Scoped_Name =>
            return Ada_IR_Info_Name (Value (Node));

         when
           K_Void               |
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
            return CRR & ".Repository";

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

   function Ada_Full_IR_Name (Node : Node_Id) return String
   is
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
         when K_Operation =>
            if Original_Node (Node) /= No_Node then
               return;
            end if;

            Gen_IR_Function_Prologue
              (CU, Node, For_Body => False);

         when
           K_Interface |
           K_Module    =>
            Gen_IR_Function_Prologue
              (CU, Node, For_Body => False);

         when K_Enum =>
            Gen_Enum_Spec (CU, Node);

         when K_Type_Declarator =>

            if  Original_Node (Node) /= No_Node then
               return;
            end if;

            if Kind (T_Type (Node)) /= K_Fixed then
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

--          when K_Sequence_Instance =>
--             Gen_Sequence_Spec (CU, Node);

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
            if Original_Node (Node) /= No_Node then
               return;
            end if;
            Gen_Operation_Body (CU, Node);

         when K_Module =>
            Gen_Module_Body (CU, Node);

         when K_Enum =>
            Gen_Enum_Body (CU, Node);

         when K_Type_Declarator =>

            if Original_Node (Node) /= No_Node then
               return;
            end if;

            if Kind (T_Type (Node)) /= K_Fixed then
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

--          when K_Sequence_Instance =>
--             Gen_Sequence_Body (CU, Node);

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
         II (CU);
         Gen_Parent_Container_Dcl (CU, Node);
         DI (CU);
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

   ------------------------------------
   -- Gen_Standard_Create_Parameters --
   ------------------------------------

   procedure Gen_Standard_Create_Parameters
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
   begin
      begin
         PL (CU, "id => CORBA.To_CORBA_String");
         PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      exception
         when Constraint_Error =>
            Error
              ("Repository Id failed for " & Name (Node),
               Fatal, Get_Location (Node));
         when others =>
            raise;
      end;
      PL (CU, "name => CORBA.To_CORBA_String");
      PL (CU, "  (""" & Name (Node) & """),");
      PL (CU, "version => CORBA.Repository_Root.To_CORBA_String");
      PL (CU, "  (""" & Image (Version (Node)) & """),");
   end Gen_Standard_Create_Parameters;

   ------------------------------
   -- Gen_Parent_Container_Dcl --
   ------------------------------

   procedure Gen_Parent_Container_Dcl
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      PS_Node : constant Node_Id := Parent_Scope (Node);
      NK : constant Node_Kind := Kind (Node);
      PSNK : constant Node_Kind := Kind (PS_Node);
   begin
      Add_With (CU, CRR & ".Container.Helper");
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "Container_Ref : "
          & "constant CORBA.Repository_Root.Container.Ref");

      if NK = K_Sequence_Instance
        or else not
        (False
         or else PSNK = K_Interface
         or else PSNK = K_ValueType
         or else PSNK = K_Module)
      then
         PL (CU, "  := " & CRR
             & ".Container.Helper.To_Ref (Get_IR_Root);");
      else
         --  The PS_Node corresponds to a container in
         --  the IR sense.
         Add_With (CU, Ada_IR_Info_Name (PS_Node));
         PL (CU, "  := CORBA.Repository_Root.Container.Helper.To_Ref");
         PL (CU, "  (" & Ada_Full_IR_Name (PS_Node) & ");");
      end if;
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
      PL (CU, "(Container_Ref,");
      PL (CU, " CORBA.To_CORBA_String ("""
          & Name (Node) & """)));");
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
            PL (CU, "CORBA.Repository_Root.Append");
            PL (CU, "  (Base_Ifs,");
            II (CU);
            PL (CU, "InterfaceDef.Helper.To_Ref");
            PL (CU, "  (" & Ada_Full_IR_Name (A_Node) & ");");
            DI (CU);
         end;
      end loop;

      PL (CU, "Cached_" & IRN);
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (CORBA.Repository_Root."
          & "Container.Create_Interface");
      PL (CU, "  (Container_Ref,");
      II (CU);
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (""" & Name (Node) & """),");
      PL (CU, "To_CORBA_String (""" & Image (Version (Node))
          & """),");
      PL (CU, "Base_Ifs,");
      PL (CU, Boolean'Image (Abst (Node)) & "));");
      DI (CU);
      NL (CU);
      PL (CU, "return Cached_" & IRN & ";");
      DI (CU);
      PL (CU, "end;");
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
      OT_Node : Node_Id := Operation_Type (Node);
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
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

            PL (CU, "CORBA.Repository_Root.Append");
            PL (CU, "  (Params,");
            II (CU);
            PL (CU, "ParameterDescription'");
            PL (CU, "(name => CORBA.To_CORBA_String ("""
                & Name (Declarator (P_Node)) & """),");

            Add_With (CU, Ada_Helper_Name (T_Node));
            Add_With (CU, Ada_IR_Info_Name (T_Node));

            PL (CU, " IDL_type =>");
            II (CU);
            PL (CU, Ada_Full_TC_Name (T_Node) & ",");
            DI (CU);
            PL (CU, " type_def =>");
            Add_With (CU, CRR & ".IDLType");
            Add_With (CU, CRR & ".IDLType.Helper");
            II (CU);
            PL (CU, "IDLType.Convert_Forward.To_Forward");
            PL (CU, "  (IDLType.Helper.To_Ref");
            PL (CU, "   (" & Ada_Full_IR_Name (T_Node) & ")),");
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
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "Cached_" & IRN);
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (" & CRR & ".InterfaceDef.Create_Operation");
      PL (CU, "  (" & CRR & ".InterfaceDef.Helper.To_Ref");
      II (CU);
      PL (CU, "(Container_Ref),");
      Gen_Standard_Create_Parameters (CU, Node);

      Add_With (CU, Ada_IR_Info_Name (OT_Node));
      Add_With (CU, CRR & ".IDLType.Helper");
      PL (CU, "IDL_result => IDLType.Helper.To_Ref");
      PL (CU, "  (" & Ada_Full_IR_Name (OT_Node) & "),");
      Put (CU, "mode => ");
      if Is_Oneway (Node) then
         Put (CU, "OP_ONEWAY");
      else
         Put (CU, "OP_NORMAL");
      end if;
      PL (CU, ",");
      PL (CU, "params => Params,");
      PL (CU, "exceptions => Exceptions,");
      PL (CU, "contexts => Contexts));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
      PL (CU, "return Cached_" & IRN & ";");
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
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (" & CRR & ".Container.Create_Module");
      PL (CU, "  (Container_Ref,");
      II (CU);
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (""" & Name (Node) & """),");
      PL (CU, "CORBA.Repository_Root.To_CORBA_String");
      PL (CU, "  (""" & Image (Version (Node)) & """)));");
      DI (CU);
      NL (CU);
      PL (CU, "return Cached_" & IRN & ";");
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
      Gen_IR_Function_Prologue (CU, Node, For_Body => False);
   end Gen_Enum_Spec;

   -------------------
   -- Gen_Enum_body --
   -------------------

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      It : Node_Iterator;
      IRN : constant String := Ada_IR_Name (Node);
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);


      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Members : EnumMemberSeq;");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      Init (It, Enumerators (Node));
      while not Is_End (It) loop
         declare
            E_Node : Node_Id;
         begin
            Get_Next_Node (It, E_Node);

            PL (CU, "CORBA.Repository_Root.Append");
            PL (CU, "  (Members,");
            II (CU);
            PL (CU, "CORBA.To_CORBA_String ("""
                & Name (E_Node) & """));");
            DI (CU);
         end;
      end loop;

      PL (CU, "Cached_" & IRN);
      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (CORBA.Repository_Root.Container.Create_Enum");
      PL (CU, "  (Container_Ref,");
      II (CU);
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (" & Repository_Id_Name (Node) & "),");
      PL (CU, "CORBA.To_CORBA_String");
      PL (CU, "  (""" & Name (Node) & """),");
      PL (CU, "CORBA.Repository_Root.To_CORBA_String");
      PL (CU, "  (""" & Image (Version (Node)) & """),");
      PL (CU, "Members));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
      NL (CU);
      PL (CU, "return Cached_" & IRN & ";");
      DI (CU);
      PL (CU, "end " & IRN & ";");

   end Gen_Enum_Body;

   -------------------------------
   -- Gen_Struct_Exception_Spec --
   -------------------------------

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => False);
   end Gen_Struct_Exception_Spec;

   -------------------------------
   -- Gen_Struct_Exception_Body --
   -------------------------------

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      It, It2 : Node_Iterator;
      IRN : constant String := Ada_IR_Name (Node);
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);

      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Members : StructMemberSeq;");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      Init (It, Members (Node));
      while not Is_End (It) loop
         declare
            M_Node, T_Node : Node_Id;
         begin
            Get_Next_Node (It, M_Node);
            T_Node := M_Type (M_Node);

            Add_With (CU, Ada_Helper_Name (T_Node));

            Init (It2, Decl (M_Node));
            while not Is_End (It2) loop
               declare
                  D_Node : Node_Id;
               begin
                  Get_Next_Node (It2, D_Node);

                  PL (CU, "CORBA.Repository_Root.Append");
                  PL (CU, "  (Members,");
                  II (CU);
                  PL (CU, "StructMember'");
                  PL (CU, "(name => CORBA.To_CORBA_String ("""
                      & Name (D_Node) & """),");

                  PL (CU, " IDL_type =>");
                  II (CU);
                  PL (CU, Ada_Full_TC_Name (T_Node) & ",");
                  DI (CU);
                  Add_With (CU, CRR & ".IDLType");
                  Add_With (CU, CRR & ".IDLType.Helper");
                  PL (CU, " type_def => "
                      & "IDLType.Convert_Forward.To_Forward");
                  II (CU);
                  Put (CU, "(");
                  Gen_IDLType
                    (CU, T_Node => T_Node, D_Node => D_Node);
                  PL (CU, ")));");
                  DI (CU);
                  DI (CU);
               end;
            end loop;
         end;
      end loop;

      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "Cached_" & IRN);
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      if Is_Struct (Node) then
         PL (CU, "  (" & CRR & ".Container.Create_Struct");
      else
         PL (CU, "  (" & CRR & ".Container.Create_Exception");
      end if;
      II (CU);
      PL (CU, "(Container_Ref,");
      Gen_Standard_Create_Parameters (CU, Node);

      PL (CU, "members => Members));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
      PL (CU, "return Cached_" & IRN & ";");
      DI (CU);
      PL (CU, "end " & IRN & ";");
   end Gen_Struct_Exception_Body;

   --------------------
   -- Gen_Union_Spec --
   --------------------

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => False);
   end Gen_Union_Spec;

   --------------------
   -- Gen_Union_Body --
   --------------------

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      It, It2 : Node_Iterator;
      IRN : constant String := Ada_IR_Name (Node);
      Case_Index : Long_Integer := -1;
      ST_Node : constant Node_Id := Switch_Type (Node);
      ST_Helper : constant String := Ada_Helper_Name (ST_Node);
   begin
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);

      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Members : UnionMemberSeq;");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      Add_With (CU, ST_Helper);
      Add_With (CU, CRR & ".IDLType");
      Add_With (CU, CRR & ".IDLType.Helper");
      Init (It, Cases (Node));
      while not Is_End (It) loop
         declare
            M_Node, T_Node : Node_Id;
         begin
            Case_Index := Case_Index + 1;
            Get_Next_Node (It, M_Node);
            T_Node := Case_Type (M_Node);

            Add_With (CU, Ada_Helper_Name (T_Node));
            Add_With (CU, Ada_IR_Info_Name (T_Node));

            Init (It2, Labels (M_Node));
            while not Is_End (It2) loop
               declare
                  L_Node : Node_Id;
               begin
                  Get_Next_Node (It2, L_Node);

                  PL (CU, "CORBA.Repository_Root.Append");
                  PL (CU, "  (Members,");
                  II (CU);
                  PL (CU, "UnionMember'");
                  PL (CU, "(name => CORBA.To_CORBA_String ("""
                      & Name (Case_Decl (M_Node)) & """),");
                  Put (CU, " label => ");
                  if Case_Index = Default_Index (Node) then
                     PL (CU, "CORBA.To_Any (CORBA.Octet'(0)),");
                  else
                     PL (CU, ST_Helper & ".To_Any");
                     Put (CU, " (");
                     Gen_Node_Stubs_Spec (CU, ST_Node);
                     Put (CU, "'(");
                     Gen_Constant_Value (CU, L_Node);
                     PL (CU, ")),");
                  end if;

                  PL (CU, " IDL_type =>");
                  II (CU);
                  PL (CU, Ada_Full_TC_Name (T_Node) & ",");
                  DI (CU);
                  PL (CU, " type_def =>");
                  II (CU);
                  PL (CU, "IDLType.Convert_Forward.To_Forward");
                  PL (CU, "  (IDLType.Helper.To_Ref");
                  PL (CU, "   (" & Ada_Full_IR_Name (T_Node) & "))));");
                  DI (CU);
                  DI (CU);
               end;
            end loop;
         end;
      end loop;

      Add_With (CU, CRR & ".IRObject.Helper");
      PL (CU, "Cached_" & IRN);
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (" & CRR & ".Container.Create_Union");
      II (CU);
      PL (CU, "(Container_Ref,");
      Gen_Standard_Create_Parameters (CU, Node);

      Add_With (CU, CRR & ".IDLType");
      Add_With (CU, CRR & ".IDLType.Helper");
      PL (CU, "discriminator_type => "
          & "IDLType.Convert_Forward.To_Forward");
      PL (CU, "  (IDLType.Helper.To_Ref");
      II (CU);
      PL (CU, "(" & Ada_Full_IR_Name (ST_Node) & ")),");
      DI (CU);
      PL (CU, "members => Members));");
      DI (CU);
      DI (CU);
      PL (CU, "end;");
      PL (CU, "return Cached_" & IRN & ";");
      DI (CU);
      PL (CU, "end " & IRN & ";");
   end Gen_Union_Body;

   ------------------------------
   -- Gen_Type_Declarator_Spec --
   ------------------------------

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
   begin
      pragma Assert (Kind (Node) = K_Declarator);
      Gen_IR_Function_Prologue (CU, Node, For_Body => False);
   end Gen_Type_Declarator_Spec;

   -----------------
   -- Gen_IDLType --
   -----------------

   procedure Gen_IDLType
     (CU     : in out Compilation_Unit;
      T_Node : in     Node_Id;
      D_Node : in     Node_Id)
   is
      function Get_Original (Node : Node_Id) return Node_Id;
      function Get_Original (Node : Node_Id) return Node_Id is
         O_Node : Node_Id;
      begin
         if Node /= No_Node then
            O_Node := Original_Node (Node);
         end if;

         if O_Node /= No_Node then
            return O_Node;
         end if;
         return Node;
      end Get_Original;

      OT_Node : constant Node_Id := Get_Original (T_Node);
      OD_Node : Node_Id;

      Is_Array : Boolean;
   begin
      if T_Node /= OT_Node
        and then Kind (T_Node) = K_Scoped_Name
      then
         OD_Node := Value (T_Node);
      else
         OD_Node := D_Node;
      end if;

      Is_Array := OD_Node /= No_Node
        and then Kind (OD_Node) = K_Declarator
        and then Length (Array_Bounds (OD_Node)) > 0;
      --  OD_Node may also be a K_Sequence_Instance.

      if Is_Array then
         Gen_Array_IR
           (CU,
            Element_Type_Node => OT_Node,
            Decl_Node => OD_Node);
      else
         case Kind (OT_Node) is

            when K_Fixed =>
               Gen_Fixed_IR (CU, OT_Node);
            when K_Sequence =>
               Gen_Sequence_IR (CU, OT_Node);

            when others =>
               Add_With (CU, Ada_IR_Info_Name (T_Node));
               PL (CU, "IDLType.Helper.To_Ref");
               Put (CU, "(" & Ada_Full_IR_Name (T_Node) & ")");
         end case;
      end if;
   end Gen_IDLType;

   ------------------------------
   -- Gen_Type_Declarator_Body --
   ------------------------------

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      T_Node : constant Node_Id := T_Type (Parent (Node));
      IRN : constant String := Ada_IR_Name (Node);

   begin
      Add_With (CU, CRR & ".IRObject.Helper");
      Gen_IR_Function_Prologue (CU, Node, For_Body => True);
      Gen_Parent_Container_Lookup (CU, Node);
      NL (CU);

      PL (CU, "Cached_" & IRN);
      PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
      PL (CU, "  (" & CRR & ".Container.Create_Alias");
      II (CU);
      PL (CU, "(Container_Ref,");
      Gen_Standard_Create_Parameters (CU, Node);

      Add_With (CU, CRR & ".IDLType");
      Add_With (CU, CRR & ".IDLType.Helper");
      PL (CU, "original_type => "
          & "IDLType.Convert_Forward.To_Forward");
      Put (CU, "  (");
      II (CU);
      Gen_IDLType (CU, T_Node => T_Node, D_Node => Node);
      PL (CU, ")));");
      DI (CU);
      DI (CU);
      PL (CU, "return Cached_" & IRN & ";");
      DI (CU);
      PL (CU, "end " & IRN & ";");
   end Gen_Type_Declarator_Body;

--    -----------------------
--    -- Gen_Sequence_Spec --
--    -----------------------

--    procedure Gen_Sequence_Spec
--      (CU        : in out Compilation_Unit;
--       Node      : in     Node_Id) is
--    begin
--       Gen_IR_Function_Prologue (CU, Node, For_Body => False);
--    end Gen_Sequence_Spec;

--    -----------------------
--    -- Gen_Sequence_Body --
--    -----------------------

--    procedure Gen_Sequence_Body
--      (CU        : in out Compilation_Unit;
--       Node      : in     Node_Id)
--    is
--       S_Node  : constant Node_Id := Sequence (Node);
--       ET_Node : constant Node_Id := Sequence_Type (S_Node);
--       B_Node  : constant Node_Id := Bound (S_Node);
--       IRN : constant String := Ada_IR_Name (Node);
--    begin
--       Add_With (CU, CRR & ".IRObject.Helper");
--       Add_With (CU, CRR & ".Repository");
--       Add_With (CU, CRR & ".Repository.Helper");
--       Gen_IR_Function_Prologue (CU, Node, For_Body => True);
--       Gen_Parent_Container_Lookup (CU, Node);
--       NL (CU);
--       PL (CU, "Cached_" & IRN);
--       PL (CU, "  := " & CRR & ".IRObject.Helper.To_Ref");
--       PL (CU, "  (" & CRR & ".Repository.Create_Sequence");
--       II (CU);
--       PL (CU, "(" & CRR & ".Repository.Helper.To_Ref (Container_Ref),");

--       Put (CU, " bound => ");
--       if B_Node = No_Node then
--          Put (CU, "0");
--       else
--          Gen_Constant_Value (CU, B_Node);
--       end if;
--       PL (CU, ",");
--       Add_With (CU, Ada_IR_Info_Name (ET_Node));
--       Add_With (CU, CRR & ".IDLType");
--       Add_With (CU, CRR & ".IDLType.Helper");
--       PL (CU, "element_type => IDLType.Helper.To_Ref");
--       PL (CU, "  (" & Ada_Full_IR_Name (ET_Node) & ")));");
--       DI (CU);
--       PL (CU, "return Cached_" & IRN & ";");
--       DI (CU);
--       PL (CU, "end " & IRN & ";");
--    end Gen_Sequence_Body;

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
   -- Gen_Array_IR --
   ------------------

   procedure Gen_Array_IR
     (CU                : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node         : in     Node_Id)
   is

      procedure Rec_Gen_Array_IR
        (CU                : in out Compilation_Unit;
         It                : in out Node_Iterator;
         Element_Type_Node : in     Node_Id;
         Decl_Node         : in     Node_Id);

      procedure Rec_Gen_Array_IR
        (CU                : in out Compilation_Unit;
         It                : in out Node_Iterator;
         Element_Type_Node : in     Node_Id;
         Decl_Node         : in     Node_Id)
      is
         Bound_Node : Node_Id;
      begin
         Get_Next_Node (It, Bound_Node);

         PL (CU, CRR & ".IDLType.Helper.To_Ref");
         PL (CU, "  (Repository.Create_Array");
         II (CU);
         PL (CU, "(Get_IR_Root,");
         II (CU);
         Put (CU, " length => ");
         Gen_Constant_Value (CU, Bound_Node);
         PL (CU, ",");
         Put (CU, " element_type => ");
         if not Is_End (It) then
            Rec_Gen_Array_IR (CU, It, Element_Type_Node, Decl_Node);
         else
            Add_With (CU, Ada_IR_Info_Name (Element_Type_Node));
            PL (CU, "IDLType.Helper.To_Ref");
            II (CU);
            Put (CU, "(" & Ada_Full_IR_Name (Element_Type_Node) & ")");
            DI (CU);
         end if;
         DI (CU);
         DI (CU);
         PL (CU, "))");
      end Rec_Gen_Array_IR;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Add_With (CU, CRR & ".IDLType.Helper");
      Rec_Gen_Array_IR (CU, Bounds_It, Element_Type_Node, Decl_Node);
   end Gen_Array_IR;

   ------------------
   -- Gen_Fixed_IR --
   ------------------

   procedure Gen_Fixed_IR
     (CU   : in out Compilation_Unit;
      Node : in     Node_Id)
   is
   begin
      PL (CU, CRR & ".IDLType.Helper.To_Ref");
      PL (CU, "  (Repository.Create_Fixed");
      II (CU);
      PL (CU, "(Get_IR_Root,");
      II (CU);
      Put (CU, " IDL_digits => ");
      Gen_Constant_Value (CU, Digits_Nb (Node));
      PL (CU, ",");
      Put (CU, " scale => ");
      Gen_Constant_Value (CU, Scale (Node));
      Put (CU, "))");
      DI (CU);
      DI (CU);
   end Gen_Fixed_IR;

   ---------------------
   -- Gen_Sequence_IR --
   ---------------------

   procedure Gen_Sequence_IR
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      ET_Node : constant Node_Id := Sequence_Type (Node);
      B_Node  : constant Node_Id := Bound (Node);
   begin
      Add_With (CU, CRR & ".IDLType.Helper");
      Add_With (CU, CRR & ".Repository");

      PL (CU, CRR & ".IDLType.Helper.To_Ref");
      PL (CU, "  (" & CRR & ".Repository.Create_Sequence");
      II (CU);
      PL (CU, "(Get_IR_Root,");

      Put (CU, " bound => ");
      if B_Node = No_Node then
         Put (CU, "0");
      else
         Gen_Constant_Value (CU, B_Node);
      end if;
      PL (CU, ",");
      Put (CU, "element_type => ");
      Gen_IDLType (CU, T_Node => ET_Node, D_Node => No_Node);
      Put (CU, "))");
      DI (CU);
   end Gen_Sequence_IR;

   ----------------------
   -- Gen_Body_Prelude --
   ----------------------

   procedure Gen_Body_Prelude (CU : in out Compilation_Unit) is
   begin
      Add_With (CU, CRR, Use_It => True);
      Add_With (CU, "PolyORB.CORBA_P.IR_Tools", Use_It => True);
      NL (CU);
      PL (CU, "pragma Warnings (Off);");
   end Gen_Body_Prelude;

end Ada_Be.Idl2Ada.IR_Info;
