--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_Io;
with Types; use Types;
with Tokens;

package body Be_Ada is
   --  If set, Any functions and typecode are generated.
   Flag_Emit_Any : Boolean := True;

   type Be_Container;
   type Be_Container_Acc is access all Be_Container'Class;

   --  be extension for a named node.
   --  ADA_ID is the mapping of the identifier.
   --  FULL_NAME is ADA_ID, prefixed with packages name.
   type Be_Named_Node is new N_Back_End with record
      Ada_Id : String_Cacc;
      Parent : Be_Container_Acc;
   end record;
   type Be_Named_Node_Acc is access all Be_Named_Node;

   --  BE for an operator.
   --  Contains the kind of subprogram.
   type Be_Operation is new Be_Named_Node with record
      Is_Function : Boolean;
   end record;
   type Be_Operation_Acc is access all Be_Operation;

   --  BE for a declarator.
   --  TYPE_NAME is the name of the type.  This is the same name as the name
   --  of its type, except for a complex declarator (since in this case an
   --  middle type is used).
   type Be_Declarator_Node is new Be_Named_Node with record
      Type_Name : String_Cacc;
   end record;
   type Be_Declarator_Node_Acc is access all Be_Declarator_Node;

   --  Every container is mapped to an Ada package.
   --  An Ada package contains with clauses.  In order to define them
   --  uniquely, they are contained in a list.
   type With_Cell;
   type With_Cell_Acc is access With_Cell;
   type With_Cell is record
      Name : String_Cacc;
      Next : With_Cell_Acc;
   end record;
   type Unit_Decl is record
      With_List : With_Cell_Acc := null;
      Decls : Unbounded_String := Null_Unbounded_String;
   end record;

   --  A record for package declaration, its private part and its body.
   --  Note: the with_list of p_priv *must* be empty.
   type Package_Unit is record
      P_Spec : Unit_Decl;
      P_Priv : Unit_Decl;
      P_Body : Unit_Decl;
   end record;

   type Be_Container is new Be_Named_Node with record
      --  The FULL_NAME is the name of this container prefixed with the
      --  full name of its parent.  This is aimed at with'ing.
      Full_Name : String_Cacc;
      --  Client part.  This is specified by Ada mapping.
      Client : Package_Unit;
      Marshal : Package_Unit;
      --  Used to create uniq identifier.
      Id_Number : Natural := 0;
   end record;
   --  type Be_Container_Acc is access all Be_Container;

   --  Seen is used as a flag for inheritance.
   type Be_Interface is new Be_Container with record
      Impl : Package_Unit;
      Seen : Boolean := False;
   end record;
   type Be_Interface_Acc is access all Be_Interface;

   procedure Pre_Generate_Idl_Type_Spec
     (A_Type : Node_Acc;
      Declarator : N_Declarator_Acc;
      Container_Be : Be_Container_Acc);

   procedure Generate_Idl_For_Scope
     (List : Node_List;
      Container : Be_Container_Acc;
      Is_Repository : Boolean := False);

   procedure Error (Msg : String; Kind : Node_Kind) is
   begin
      Ada.Text_Io.Put_Line
        ("Cannot handle " & Node_Kind'Image (Kind) & " in subprg " & Msg);
      raise Internal_Error;
   end Error;

   --  Append a with clause to a be_container, iff it doesn't exist yet.
   --  Note: the first version should disappear.
   procedure Add_With (Decl : in out Unit_Decl; Name : String);
   procedure Add_With (Decl : in out Unit_Decl; Name : String_Cacc);

   --  Return the Ada name of the node.  This name should have already been
   --  created by create_ada_identifier for example.
   function Get_Ada_Name (Node : N_Root'Class) return String is
      Be : Be_Named_Node_Acc;
   begin
      Be := Be_Named_Node_Acc (Get_Back_End (Node));
      return Be.Ada_Id.all;
   end Get_Ada_Name;

   --  Create an Ada identifier from an IDL indentifier.
   --  Rules are given in 3.1 "Mapping of identifiers"
   --    o  Remove any leading underscore
   --    o  Where "_" is followed by another underscore, replace the second
   --       underscore with the character "U".
   --    o  Where "_" is at the end of an identifier, add the character "U"
   --       after the underscore.
   --    o  When an IDL identifier collides with an Ada reserved words, insert
   --       the string "IDL_" before the identifier.
   --
   --  FIXME.
   --  Note: of course, only rules (1) and (4) are clear.
   --  However, rule (1) is stupid, because CORBA V2.2, section 3.2.3 says:
   --   "An identifier is an arbitrarily long sequence of alphabetics, digit,
   --   and underscore ("_") characters.  The first character must be
   --   an alphabetic character."
   --  Rule (2) is ambiguous: should "A____B" be replaced by "A_U_UB" or
   --    by "A_UUUB" ?  I really prefer the second translation.
   --  Rule (3) collides with rule (2): should "A__" be replaced by "A_U"
   --    or by "A_UU" ?  I choose the first one.
   --  Length of the mapped identifier: is at most 4 characters longer (with
   --   rule 4) or 1 character longer (with tule 3) if the IDL identifier is
   --   not an Ada reserved word.
   --  Last question:  what is a conforming implementation? :-)
   function Map_Identifier (N : String) return String is
      use Ada.Characters.Handling;
      use Tokens;
      Id : Uniq_Id;
      Str : String (1 .. N'Length + 1);
      Index : Positive;
      Capitalize : Boolean;

      --  Append a character to STR.
      procedure Append (C : Character) is
      begin
         Str (Index) := C;
         Index := Index + 1;
      end Append;
   begin
      --  Check for collision with an Ada reserved word.
      --  List of keyword is given by LRM 2.9, reserved words.
      --  FIXME: very, very inefficient code...
      --  Note: case, exception, in, out are IDL keywords too.
      Id := Get_Identifier (N);

      --  FIXME : to be rewrote without the use of Idents
--       if Id in Id_First_Ada_Word .. Id_Last_Ada_Word
--         or else Id = Id_Pragma
--       then
--          return "IDL_" & N;
--       end if;

      Index := Str'First;
      for I in N'Range loop
         if N (I) = '_' then
            if I /= N'First and then N (I - 1) = '_' then
               --  Rule (2)
               Append ('U');
               Capitalize := False;
            else
               Append ('_');
               Capitalize := True;
            end if;
            if I = N'Last then
               --  Rule (3).
               Append ('U');
               Capitalize := False;
            end if;
         else
            if Capitalize then
               Append (To_Upper (N (I)));
               Capitalize := False;
            else
               Append (N (I));
            end if;
         end if;
      end loop;
      return Str (Str'First .. Index - 1);
   end Map_Identifier;

   procedure Create_Ada_Identifier
     (Node : in out Named_Node'Class; Parent : Be_Container_Acc)
   is
      Res : String_Cacc;
      Be : Be_Named_Node_Acc;
   begin
      Res := new String'(Map_Identifier (Get_Name (Node)));
      Be := Be_Named_Node_Acc (Get_Back_End (Node));
      Be.Ada_Id := Res;
      Be.Parent := Parent;
   end Create_Ada_Identifier;

   procedure Create_Ada_Uniq_Identifier
     (Node : in out N_Root'Class;
      Prefix : String;
      Container : Be_Container_Acc)
   is
      Res : String_Cacc;
      N : String := Natural'Image (Container.Id_Number);
   begin
      --  Huh, what about overflow ???
      Container.Id_Number := Container.Id_Number + 1;
      --  Replace the blank with a underscore.
      N (N'First) := '_';
      Res := new String'(Map_Identifier (Prefix & N));
      Be_Named_Node (Get_Back_End (Node).all).Ada_Id := Res;
      Be_Named_Node (Get_Back_End (Node).all).Parent := Container;
   end Create_Ada_Uniq_Identifier;

   procedure Create_Full_Name (Node : in out N_Root'Class) is
      Be : Be_Container_Acc;
   begin
      Be := Be_Container_Acc (Get_Back_End (Node));
      if Be.Parent /= null then
         Be.Full_Name :=
          new String'(Be.Parent.Full_Name.all & '.' & Be.Ada_Id.all);
      else
         Be.Full_Name := Be.Ada_Id;
      end if;
   end Create_Full_Name;

   procedure Add_With (Decl : in out Unit_Decl; Name : String) is
      Cell, Prev : With_Cell_Acc;
   begin
      Prev := null;
      Cell := Decl.With_List;
      while Cell /= null loop
         if Tokens.Idl_Compare (Cell.Name.all, Name) then
            return;
         end if;
         Prev := Cell;
         Cell := Cell.Next;
      end loop;
      Cell := new With_Cell'(new String'(Name), null);
      if Prev = null then
         Decl.With_List := Cell;
      else
         Prev.Next := Cell;
      end if;
   end Add_With;

   procedure Add_With (Decl : in out Unit_Decl; Name : String_Cacc) is
      Cell, Prev : With_Cell_Acc;
   begin
      Prev := null;
      Cell := Decl.With_List;
      while Cell /= null loop
         if Tokens.Idl_Compare (Cell.Name.all, Name.all) then
            return;
         end if;
         Prev := Cell;
         Cell := Cell.Next;
      end loop;
      Cell := new With_Cell'(Name, null);
      if Prev = null then
         Decl.With_List := Cell;
      else
         Prev.Next := Cell;
      end if;
   end Add_With;

   procedure Add_Text (Decl : in out Unit_Decl; Text : String) is
   begin
      Append (Decl.Decls, Text);
   end Add_Text;

   procedure Add_Newline (Decl : in out Unit_Decl) is
   begin
      Append (Decl.Decls, Ada.Characters.Latin_1.Lf);
   end Add_Newline;

   procedure Add_Full_Ada_Name (Decl : in out Unit_Decl; Node : N_Root'Class)
   is
      Be : Be_Named_Node_Acc;
   begin
      Be := Be_Named_Node_Acc (Get_Back_End (Node));
      if Be.Parent /= null and then Be.Parent.Full_Name /= null then
         Add_Text (Decl, Be.Parent.Full_Name.all);
         Add_Text (Decl, ".");
      end if;
      Add_Text (Decl, Get_Ada_Name (Node));
   end Add_Full_Ada_Name;

   procedure Disp_Ada (Node : access N_Root'Class);
   procedure Disp_With_List (List : With_Cell_Acc) is
      use Ada.Text_IO;
      With_El : With_Cell_Acc;
   begin
      With_El := List;
      while With_El /= null loop
         Put ("with ");
         Put (With_El.Name.all);
         Put_Line (";");
         With_El := With_El.Next;
      end loop;
      if List /= null then
         New_Line;
      end if;
   end Disp_With_List;

   procedure Disp_Ada (Unit : Package_Unit; Name : String) is
      use Ada.Text_Io;
   begin
      Put_Line ("--  specification of " & Name);
      --  Ada mapping § 1.6
      --   All Ada compilation units generated from an IDL specification shall
      --   have (non-direct) visibility to the CORBA subsystem (through a
      --   with clause).
      Put_Line ("with CORBA;");
      Disp_With_List (Unit.P_Spec.With_List);
      if Unit.P_Spec.With_List = null then
         --  Skip a line, even if there is no additionnal with clauses.
         New_Line;
      end if;
      Put_line ("package " & Name & " is");
      Put (To_String (Unit.P_Spec.Decls));
      if Length (Unit.P_Priv.Decls) > 0 then
         Put_Line ("private");
         Put (To_String (Unit.P_Priv.Decls));
      end if;
      Put_Line ("end " & Name & ";");
      New_Line;
      if Length (Unit.P_Body.Decls) > 0 then
         Put_Line ("--  body of " & Name);
         Disp_With_List (Unit.P_Body.With_List);
         Put_line ("package body " & Name & " is");
         Put (To_String (Unit.P_Body.Decls));
         Put_Line ("end " & Name & ";");
         New_Line;
      end if;
   end Disp_Ada;

   procedure Disp_Ada (List : Node_List) is
      It : Node_Iterator;
      Node : Node_Acc;
   begin
      Init (It, List);
      while not Is_End (It) loop
         Node := Get_Node (It);
         Disp_Ada (Node);
         Next (It);
      end loop;
   end Disp_Ada;

   procedure Disp_Ada (Node : access N_Root'Class) is
   begin
      case Get_Kind (Node.all) is
         when K_Repository =>
            declare
               Be : Be_Container_Acc;
            begin
               Be := Be_Container_Acc (Get_Back_End (Node.all));
               Disp_Ada (Be.Client, Be.Full_Name.all);
            end;
            Disp_Ada (N_Repository_Acc (Node).Contents);
         when K_Interface =>
            declare
               Be : Be_Interface_Acc;
            begin
               Be := Be_Interface_Acc (Get_Back_End (Node.all));
               Disp_Ada (Be.Client, Be.Full_Name.all);
               Disp_Ada (Be.Impl, Be.Full_Name.all & ".Impl");
            end;
         when K_Module =>
            declare
               Be : Be_Container_Acc;
            begin
               Be := Be_Container_Acc (Get_Back_End (Node.all));
               Disp_Ada (Be.Client, Be.Full_Name.all);
            end;
            Disp_Ada (N_Module_Acc (Node).Contents);
         when K_Forward_Interface =>
            declare
               use Ada.Text_Io;
               Be : Be_Container_Acc;
            begin
               Be := Be_Container_Acc (Get_Back_End (Node.all));
               --  FIXME.
               --  Very, very special case.
               Put_Line ("--  specification of " & Be.Full_Name.all);
               Put_Line ("with CORBA;");
               Put_Line ("with CORBA.Forward;");
               New_Line;
               Put (To_String (Be.Client.P_Spec.Decls));
               New_Line;
            end;
         when K_Type_Declarator | K_Enum | K_Struct | K_Union | K_Const
           | K_Exception =>
            null;
         when others =>
            Error ("disp_ada (n_root)", Get_Kind (Node.all));
      end case;
   end Disp_Ada;

   --  Type of package for generation of names.
   type Package_Kind is (Package_Client, Package_Impl);

   --  Generate code for a scoped name.
   --  Code generated it the name of the scoped name, possibly prefixed
   --  if the node is not defined in the current scope.
   --  Also, ".Ref" or ".Object" can be appended for an interface
   --  (full declaration or forward declaration for .Ref only).
   procedure Generate_Idl
     (Node : N_Scoped_Name_Acc;
      Unit : in out Unit_Decl;
      Container : Be_Container_Acc;
      Package_Type : Package_Kind := Package_Client)
   is
      Kind : Node_Kind;
      N : Named_Node_Acc;
      Parent : Be_Container_Acc;
   begin
      N := Node.Value;
      Kind := Get_Kind (N.all);
      case Kind is
         when K_Interface | K_Forward_Interface =>
            Parent := Be_Container_Acc (Get_Back_End (N.all));
         when others =>
            Parent := Be_Named_Node_Acc (Get_Back_End (N.all)).Parent;
      end case;
      if Parent /= Container then
         if Kind = K_Interface and then Package_Type = Package_Impl then
            Add_With (Unit, Parent.Full_Name.all & ".Impl");
         else
            Add_With (Unit, Parent.Full_Name);
         end if;
         Add_Full_Ada_Name (Unit, N.all);
      else
         Add_Text (Unit, Get_Ada_Name (N.all));
      end if;
      case Kind is
         when K_Interface | K_Forward_Interface =>
            case Package_Type is
               when Package_Client =>
                  Add_Text (Unit, ".Ref");
               when Package_Impl =>
                  --  Note kind = k_forward_interface
                  --   and package_type = package_impl is NOT possible.
                  Add_Text (Unit, ".Object");
            end case;
         when others =>
            null;
      end case;
   end Generate_Idl;

   --  Convert a type node into Ada.
   --  See syntax § 1.9
   procedure Generate_Idl_For_Type
     (Node : Node_Acc;
      Unit : in out Unit_Decl;
      Container : Be_Container_Acc)
   is
   begin
      case Get_Kind (Node.all) is
         when K_Void =>
            raise Internal_Error;
         when K_Float =>
            Add_Text (Unit, "CORBA.Float");
         when K_Double =>
            Add_Text (Unit, "CORBA.Double");
         when K_Long_Double =>
            Add_Text (Unit, "CORBA.Long_Double");
         when K_Short =>
            Add_Text (Unit, "CORBA.Short");
         when K_Long =>
            Add_Text (Unit, "CORBA.Long");
         when K_Long_Long =>
            Add_Text (Unit, "CORBA.Long_Long");
         when K_Unsigned_Short =>
            Add_Text (Unit, "CORBA.Unsigned_Short");
         when K_Unsigned_Long =>
            Add_Text (Unit, "CORBA.Unsigned_Long");
         when K_Unsigned_Long_Long =>
            Add_Text (Unit, "CORBA.Unsigned_Long_Long");
         when K_Char =>
            Add_Text (Unit, "CORBA.Char");
         when K_Wchar =>
            Add_Text (Unit, "CORBA.Wchar");
         when K_Octet =>
            Add_Text (Unit, "CORBA.Octet");
         when K_Boolean =>
            Add_Text (Unit, "CORBA.Boolean");
         when K_Sequence =>
            Add_Text (Unit, Get_Ada_Name (Node.all) & ".Sequence");
         when K_String =>
            if N_String_Acc (Node).Bound /= null then
               Add_Text (Unit, Get_Ada_Name (Node.all) & ".Bounded_String");
            else
               Add_Text (Unit, "CORBA.String");
            end if;
         when K_Scoped_Name =>
            Generate_Idl (N_Scoped_Name_Acc (Node), Unit, Container);
         when K_Enum =>
            Add_Text (Unit, Get_Ada_Name (Node.all));
         when K_Object =>
            Add_With (Unit, "CORBA.Object");
            Add_Text (Unit, "CORBA.Object.Ref");
         when others =>
            Error ("generate_idl_for_type", Get_Kind (Node.all));
      end case;
   end Generate_Idl_For_Type;

   procedure Generate_Idl_For_Const
     (Node : Node_Acc;
      Unit : in out Unit_Decl;
      Container : Be_Container_Acc)
   is
   begin
      case Get_Kind (Node.all) is
         when K_Lit_Integer | K_Lit_Floating_Point =>
            Add_Text (Unit, N_Literal_Acc (Node).Lit.all);
         when Binary_Node_Kind  =>
            Generate_Idl_For_Const
              (N_Binary_Expr_Acc (Node).Left, Unit, Container);
            case Get_Kind (Node.all) is
               when K_Add =>
                  Add_Text (Unit, " + ");
               when K_Mul =>
                  Add_Text (Unit, " * ");
               when others =>
                  Error ("generate_idl_for_const (binary)",
                         Get_Kind (Node.all));
            end case;
            Generate_Idl_For_Const
              (N_Binary_Expr_Acc (Node).Right, Unit, Container);
         when K_Scoped_Name =>
            Generate_Idl (N_Scoped_Name_Acc (Node), Unit, Container);
         when others =>
            Error ("generate_idl_for_const", Get_Kind (Node.all));
      end case;
   end Generate_Idl_For_Const;

   procedure Generate_Idl_For_Complex_Type
     (Type_Spec : Node_Acc;
      Declarator : N_Declarator_Acc;
      Container : Be_Container_Acc)
   is
      Spec : Unit_Decl renames Container.Client.P_Spec;
      It : Node_Iterator;
      Node : Node_Acc;
   begin
      if Declarator.Array_Bounds /= Nil_List then
         Add_Text (Spec, "array (");
         Init (It, Declarator.Array_Bounds);
         while not Is_End (It) loop
            Node := Get_Node (It);
            Add_Text (Spec, "0 .. ");
            Generate_Idl_For_Const (Node, Spec, Container);
            Add_Text (Spec, " - 1");
            Next (It);
            if not Is_End (It) then
               Add_Text (Spec, ", ");
            end if;
         end loop;
         Add_Text (Spec, ") of ");
      end if;
      Generate_Idl_For_Type (Type_Spec, Spec, Container);
   end Generate_Idl_For_Complex_Type;

   procedure Generate_Any
     (Type_Name : String; Unit : in out Unit_Decl; Is_From : Boolean) is
   begin
      if not Flag_Emit_Any then
         return;
      end if;
      if Is_From then
         Add_Text (Unit, "  function From_Any (Item : in Any) return "
                   & Type_Name);
      else
         Add_Text (Unit, "  function To_Any (Item : in " & Type_Name
                     & ") return Any");
      end if;
   end Generate_Any;

   --  Generate declarations to handle type codes and any for type TYPE_NAME.
   procedure Generate_Any (Type_Name : String; Unit : in out Unit_Decl) is
   begin
      if not Flag_Emit_Any then
         return;
      end if;
      --  Ada mapping § 1.22.1
      --   For each distinct type T in an IDL specification, pre-defined or
      --   IDL-defined, conforming implementations shall be capable of
      --   generating functions to insert and extract values of that type
      --   to and from type Any.  The form of these functions shall be: [...]
      Generate_Any (Type_Name, Unit, True);
      Add_Text (Unit, ";");
      Add_Newline (Unit);
      Generate_Any (Type_Name, Unit, False);
      Add_Text (Unit, ";");
      Add_Newline (Unit);
      --  Ada mapping § 1.21
      --   All conforming implementations shall be capable of generating
      --   constants of type CORBA.TypeCode.Object for all pre-defined
      --   and IDL-defined types.  The name of the constant shall be "TC_"
      --   prepended to the mapped type name.
      Add_Text (Unit,
                "  TC_" & Type_Name & " : constant CORBA.TypeCode.Object;");
      Add_Newline (Unit);
   end Generate_Any;

   procedure Generate_Idl_For_Decls
     (Type_Spec : Node_Acc;
      Declarator : N_Declarator_Acc;
      Container_Be : Be_Container_Acc)
   is
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Be : Be_Declarator_Node_Acc;
   begin
      Be := new Be_Declarator_Node;
      Set_Back_End (Declarator.all, Be);
      Create_Ada_Identifier (Declarator.all, Container_Be);
      if Declarator.Array_Bounds /= Nil_List then
         Be.Type_Name :=
          new String'(Map_Identifier (Be.Ada_Id.all & "_Array"));
         Add_Text (Spec, "  type " & Be.Type_Name.all & " is ");
         Generate_Idl_For_Complex_Type (Type_Spec, Declarator, Container_Be);
         Add_Text (Spec, ";");
         Add_Newline (Spec);
         Generate_Any (Be.Type_Name.all, Spec);
      else
         Be.Type_Name := null;
      end if;
   end Generate_Idl_For_Decls;

   procedure Generate_Idl (Sequence : N_Sequence_Acc;
                           Container_Be : Be_Container_Acc)
   is
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Be : Be_Named_Node_Acc;
   begin
      --  Ada mapping § 1.15
      --   Each sequence type declaration shall correspond to an instantiation
      --   of CORBA.Sequence.Bounded or CORBA.Sequence.Unbounded, as
      --   appropriate.  The formal of the generic packages and the actual
      --   arguments provided are implementation defined.  The name and scope
      --   of the instantiation is left implementation defined.
      --
      --  The wording is interesting.
      --  An high cost (mainly memory) of generic package instantiation is
      --  assumed.
      --  However, it is not allowed to factorize sequence type, since sentence
      --  number 1.  Shame.
      --  The current implementation numbers instantiation names; this is
      --  a little bit worse than naming them, as used in the spec, but, at
      --  least, constantes are not computed.
      Pre_Generate_Idl_Type_Spec (Sequence.S_Type, null, Container_Be);
      Be := new Be_Named_Node;
      Set_Back_End (Sequence.all, Be);
      Create_Ada_Uniq_Identifier (Sequence.all, "IDL_Sequence", Container_Be);
      if Sequence.Bound = null then
         Add_With (Spec, "CORBA.Sequence.Unbounded");
         Add_Text (Spec, "  package " & Get_Ada_Name (Sequence.all)
                   & " is new CORBA.Sequence.Unbounded");
         Add_Newline (Spec);
         Add_Text (Spec, "    (");
         Generate_Idl_For_Type (Sequence.S_Type, Spec, Container_Be);
         Add_Text (Spec, ");");
         Add_Newline (Spec);
      else
         Add_With (Spec, "CORBA.Sequence.Bounded");
         Add_Text (Spec, "  package " & Get_Ada_Name (Sequence.all)
                   & " is new CORBA.Sequence.Bounded");
         Add_Newline (Spec);
         Add_Text (Spec, "    (");
         Generate_Idl_For_Type (Sequence.S_Type, Spec, Container_Be);
         Add_Text (Spec, ", ");
         Generate_Idl_For_Const (Sequence.Bound, Spec, Container_Be);
         Add_Text (Spec, ");");
         Add_Newline (Spec);
      end if;
      Generate_Any (Get_Ada_Name (Sequence.all), Spec);
   end Generate_Idl;

   procedure Generate_Idl
     (Str : N_String_Acc; Container_Be : Be_Container_Acc)
   is
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Be : Be_Named_Node_Acc;
   begin
      --  Ada mapping § 1.16
      --   The IDL bounded and unbounded strings types are mapped to Ada's
      --   predefined string packages or functional equivalent.
      --
      --   Conforming implementations shall provide an unbounded string type
      --   in the package CORBA.  The CORBA.String type shall be a derivation
      --   of Ada.Strings.Unbounded.Unbounded_String or a functionally
      --   equivalent package with equivalent primitive operations. [...]
      --
      --   An unbounded IDL string shall be mapped to the type CORBA.String.
      --
      --   Conforming implementations shall provide a CORBA.Bounded_Strings
      --   package with the same specification and semantics as
      --   Ada.Strings.Bounded.Geneeric_Bounded_Length.
      --
      --   The CORBA.Bounded_Strings package has a generic formal parameter
      --   "Max" declared as type Positive and establishes the maximum length
      --   of the bounded string at instantiation.  A generic instantiation
      --   of the package shall be created using the bound for the IDL string
      --   as the associated parameter.  The name and scope of the
      --   instantiation is left implementation defined.
      if Str.Bound = null then
         --  Nothing to do.
         return;
      end if;
      Be := new Be_Named_Node;
      Set_Back_End (Str.all, Be);
      Create_Ada_Uniq_Identifier (Str.all, "IDL_String", Container_Be);
      Add_With (Spec, "CORBA.Bounded_Strings");
      Add_Text (Spec, "  package " & Get_Ada_Name (Str.all)
                & " is new CORBA.Bounded_Strings (");
      Generate_Idl_For_Const (Str.Bound, Spec, Container_Be);
      Add_Text (Spec, ");");
      Add_Newline (Spec);
      Generate_Any (Get_Ada_Name (Str.all), Spec);
   end Generate_Idl;

   procedure Generate_Idl (Enum_Decl : N_Enum_Acc;
                           Container_Be : Be_Container_Acc)
   is
      It : Node_Iterator;
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Node : N_Enumerator_Acc;
   begin
      --  Ada mapping § 1.12
      --   An IDL enum type shall map directly to an Ada enumerated type with
      --   name mapped from the IDL identifier and values mapped from and in
      --   the order of the IDL member list.
      Be := new Be_Named_Node;
      Set_Back_End (Enum_Decl.all, Be);
      Create_Ada_Identifier (Enum_Decl.all, Container_Be);
      Add_Text (Spec, "  type " & Get_Ada_Name (Enum_Decl.all) & " is (");

      Init (It, Enum_Decl.Enumerators);
      loop
         Node := N_Enumerator_Acc (Get_Node (It));
         Be := new Be_Named_Node;
         Set_Back_End (Node.all, Be);
         Create_Ada_Identifier (Node.all, null);
         Add_Text (Spec, Get_Ada_Name (Node.all));
         Next (It);
         if not Is_End (It) then
            Add_Text (Spec, ", ");
         else
            exit;
         end if;
      end loop;
      Add_Text (Spec, ");");
      Add_Newline (Spec);
      Generate_Any (Get_Ada_Name (Enum_Decl.all), Spec);
   end Generate_Idl;

   procedure Pre_Generate_Idl_Type_Spec
     (A_Type : Node_Acc;
      Declarator : N_Declarator_Acc;
      Container_Be : Be_Container_Acc)
   is
   begin
      case Get_Kind (A_Type.all) is
         when K_Sequence =>
            Generate_Idl (N_Sequence_Acc (A_Type), Container_Be);
         when K_String =>
            Generate_Idl (N_String_Acc (A_Type), Container_Be);
         when K_Enum =>
            Generate_Idl (N_Enum_Acc (A_Type), Container_Be);
         when K_Octet | K_Boolean | K_Short | K_Long | K_Long_Long
           | K_Unsigned_Short | K_Unsigned_Long | K_Unsigned_Long_Long
           | K_Float | K_Double | K_Long_Double | K_Char | K_Wchar =>
            null;
         when K_Scoped_Name =>
            null;
         when others =>
            Error ("pre_generate_idl_type_spec", Get_Kind (A_Type.all));
      end case;
   end Pre_Generate_Idl_Type_Spec;

   procedure Generate_Idl
     (Type_Spec : Node_Acc;
      Declarator : N_Declarator_Acc;
      Container : Be_Container_Acc)
   is
      Spec : Unit_Decl renames Container.Client.P_Spec;
      Be : Be_Declarator_Node_Acc;
   begin
      if Declarator.Array_Bounds /= Nil_List then
         Be := Be_Declarator_Node_Acc (Get_Back_End (Declarator.all));
         Add_Text (Spec, Be.Type_Name.all);
      else
         Generate_Idl_For_Type (Type_Spec, Spec, Container);
      end if;
   end Generate_Idl;

   procedure Add_Definition
     (Node : N_Operation_Acc;
      Be : Be_Interface_Acc;
      Unit : in out Unit_Decl;
      Package_Type : Package_Kind)
   is
      Op_Be : Be_Operation_Acc;
      Param_Be : Be_Named_Node_Acc;
      It : Node_Iterator;
      Param : N_Param_Acc;
   begin
      --  An operation is mapped to an Ada subprogram with a non-empty list
      --  of parameters iff the operation has parameters.
      --  Note: an operation with an empty list of parameters and a
      --  non-void types is always mapped to an Ada function.

      --  Create BE for this node, if not already created.
      --  Possible due to multiple inheritance.
      Op_Be := Be_Operation_Acc (Get_Back_End (Node.all));

      if Op_Be.Is_Function then
         Add_Text (Unit, "  function ");
      else
         Add_Text (Unit, "  procedure ");
      end if;
      Add_Text (Unit, Get_Ada_Name (Node.all));

      --  Ada mapping § 1.42
      --   The parameters passed to an implementation subprogram parallel those
      --   passed to the side stub except that the type of the Self
      --   parameter is access Object, where Object is described above,
      --   rather than the reference type declared in the stub package.
      case Package_Type is
         when Package_Client =>
            Add_Text (Unit, " (Self : in Ref");
         when Package_Impl =>
            Add_Text (Unit, " (Self : access Object");
      end case;
      Init (It, Node.Parameters);
      while not Is_End (It) loop
         Param := N_Param_Acc (Get_Node (It));
         Param_Be := Be_Named_Node_Acc (Get_Back_End (Param.all));
         Add_Text (Unit, "; ");
         Add_Text (Unit, Get_Ada_Name (Param.all));
         case Param.Mode is
            when Mode_In =>
               Add_Text (Unit, ": in ");
            when Mode_Out =>
               Add_Text (Unit, ": out ");
            when Mode_Inout =>
               Add_Text (Unit, ": in out ");
         end case;
         Generate_Idl_For_Type (Param.P_Type, Unit, Be_Container_Acc (Be));
         Next (It);
      end loop;

      --  Ada mapping 1.25
      --   If an operation in an IDL specification has a context specification
      --   then an additional argument with name "In_Context", of in mode and
      --   of type CORBA.Context.Object shall be added after all IDL
      --   specified arguments and before the Returns argument if any.  The
      --   In_Context argumenet shall have a default value of
      --   CORBA.ORB.Get_Default_Context.
      if Node.Contexts /= Nil_List then
         Add_With (Unit, "CORBA.Context");
         Add_With (Unit, "CORBA.ORB");
         Add_Text (Unit, "; In_Context : in CORBA.Context.Object := "
                   & "CORBA.ORB.Get_Default_Context");
      end if;

      if Op_Be.Is_Function then
         Add_Text (Unit, ") return ");
         Generate_Idl_For_Type (Node.Op_type, Unit, Be_Container_Acc (Be));
      else
         if Get_Kind (Node.Op_Type.all) /= K_Void then
            Add_Text (Unit, "; Returns: out ");
            Generate_Idl_For_Type (Node.Op_type, Unit, Be_Container_Acc (Be));
         end if;
         Add_Text (Unit, ")");
      end if;
   end Add_Definition;

   procedure Generate_Idl (Node : N_Operation_Acc; Be : Be_Interface_Acc)
   is
      function Is_Mapped_To_Function return Boolean is
         It : Node_Iterator;
         Param : N_Param_Acc;
      begin
         --  Ada mapping § 1.25:
         --  IDL interface operations with non-void result types that have only
         --  in mode parameters shall be mapped to Ada functions returning an
         --  Ada type mapped from the operation result type.  Otherwise, [...]
         --  operations shall be mapped to Ada procedures.
         if Get_Kind (Node.Op_Type.all) = K_Void then
            return False;
         end if;
         Init (It, Node.Parameters);
         while not Is_End (It) loop
            Param := N_Param_Acc (Get_Node (It));
            if Param.Mode /= Mode_In then
               return False;
            end if;
            Next (It);
         end loop;
         return True;
      end Is_Mapped_To_Function;

      Spec : Unit_Decl renames Be.Client.P_Spec;
      Ispec : Unit_Decl renames Be.Impl.P_Spec;
      Op_Be : Be_Operation_Acc;
      Param_Be : Be_Named_Node_Acc;
      It : Node_Iterator;
      Param : N_Param_Acc;
   begin
      --  An operation is mapped to an Ada subprogram with a non-empty list
      --  of parameters iff the operation has parameters.
      --  Note: an operation with an empty list of parameters and a
      --  non-void types is always mapped to an Ada function.

      --  Create BE for this node, if not already created.
      --  Possible due to multiple inheritance.
      Op_Be := Be_Operation_Acc (Get_Back_End (Node.all));
      if Op_Be = null then
         Op_Be := new Be_Operation;
         Set_Back_End (Node.all, Op_Be);
         Create_Ada_Identifier (Node.all, Be_Container_Acc (Be));
         Op_Be.Is_Function := Is_Mapped_To_Function;

         Init (It, Node.Parameters);
         while not Is_End (It) loop
            Param := N_Param_Acc (Get_Node (It));
            Param_Be := new Be_Named_Node;
            Set_Back_End (Param.all, Param_Be);
            Create_Ada_Identifier (Param.all, null);
            Next (It);
         end loop;
      end if;

      Add_Definition (Node, Be, Spec, Package_Client);
      Add_Text (Spec, ";");
      Add_Newline (Spec);
      Add_Definition (Node, Be, Ispec, Package_Impl);
      Add_Text (Ispec, ";");
      Add_Newline (Ispec);
   end Generate_Idl;

   procedure Pre_Generate_Idl_For_Members
     (Members : Node_List; Container_Be : Be_Container_Acc)
   is
      Decl : N_Declarator_Acc;
      It_Memb, It_Decl : Node_Iterator;
      Member : N_Member_Acc;
   begin
      --  Generate type definition, if any.
      Init (It_Memb, Members);
      while not Is_End (It_Memb) loop
         Member := N_Member_Acc (Get_Node (It_Memb));
         Init (It_Decl, Member.Decl);
         while not Is_End (It_Decl) loop
            Decl := N_Declarator_Acc (Get_Node (It_Decl));
            Generate_Idl_For_Decls (Member.M_Type, Decl, Container_Be);
            Next (It_Decl);
         end loop;
         Next (It_Memb);
      end loop;
   end Pre_Generate_Idl_For_Members;

   procedure Generate_Idl_For_Members
     (Members : Node_List; Container_Be : Be_Container_Acc)
   is
      Decl : N_Declarator_Acc;
      It_Memb, It_Decl : Node_Iterator;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Member : N_Member_Acc;
   begin
      Init (It_Memb, Members);
      while not Is_End (It_Memb) loop
         Member := N_Member_Acc (Get_Node (It_Memb));
         Init (It_Decl, Member.Decl);
         while not Is_End (It_Decl) loop
            Decl := N_Declarator_Acc (Get_Node (It_Decl));
            Add_Text (Spec, "    " & Get_Ada_Name (Decl.all) & " : ");
            Generate_Idl (Member.M_Type, Decl, Container_Be);
            Add_Text (Spec, ";");
            Add_Newline (Spec);
            Next (It_Decl);
         end loop;
         Next (It_Memb);
      end loop;
   end Generate_Idl_For_Members;

   procedure Generate_Idl (Struct : N_Struct_Acc;
                           Container_Be : Be_Container_Acc)
   is
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
   begin
      --  Ada mapping § 1.13
      --   An IDL struct type shall map directly to an Ada record type with
      --   type name mapped from the struct identifier and each component
      --   formed from each declarator in the member list as follows:
      --   - If the declarator is a simple_declarator, the component name
      --     shall be mapped from the identifier in the declarator and the
      --     type shall be mapped from the type_spec.
      --   - if the declarator is a complex_declarator, a preceding type
      --     definition shall define an array type.  The array type name
      --     shall be mapped from the identifier contained in the
      --     array_declarator prepended to "_Array".  The type definition
      --     shall be an array, over the range(s) 0 to one less then the
      --     fixed_array_size(s). specified in the array declarator, of the
      --     type mapped from the IDL, type contained in the type
      --     specification.  If multiple bounds are declared, a multiple
      --     dimensional array shall be created that preserves the indexing
      --     order specified in the IDL declaration.  In the component
      --     definition, the name shall be mapped from the identifier contained
      --     in the array_declarator and the type shall be the array type.
      Be := new Be_Named_Node;
      Set_Back_End (Struct.all, Be);
      Create_Ada_Identifier (Struct.all, Container_Be);

      --  Generate type definition, if any.
      Pre_Generate_Idl_For_Members (Struct.Members, Container_Be);

      Add_Text (Spec, "  type " & Get_Ada_Name (Struct.all) & " is record");
      Add_Newline (Spec);
      Generate_Idl_For_Members (Struct.Members, Container_Be);
      Add_Text (Spec, "  end record;");
      Add_Newline (Spec);
      Generate_Any (Get_Ada_Name (Struct.all), Spec);
   end Generate_Idl;

   procedure Generate_Idl (A_Case : N_Case_Acc;
                           Container_Be : Be_Container_Acc)
   is
      A_Label : Node_Acc;
      It_Label : Node_Iterator;
      Be_Decl : Be_Declarator_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
   begin
      Init (It_Label, A_Case.Labels);
      Add_Text (Spec, "      when ");
      loop
         A_Label := Get_Node (It_Label);
         if A_Label = null then
            Add_Text (Spec, "others");
         else
            Generate_Idl_For_Const (A_Label, Spec, Container_Be);
         end if;
         Next (It_Label);
         if Is_End (It_Label) then
            Add_Text (Spec, " =>");
            Add_Newline (Spec);
            exit;
         else
            Add_Text (Spec, " | ");
         end if;
      end loop;
      Be_Decl := Be_Declarator_Node_Acc (Get_Back_End (A_Case.C_Decl.all));
      Add_Text
        (Spec, "        " & Get_Ada_Name (A_Case.C_Decl.all) & " : ");
      Generate_Idl (A_Case.C_Type, A_Case.C_Decl, Container_Be);
      Add_Text (Spec, ";");
      Add_Newline (Spec);
   end Generate_Idl;

   procedure Generate_Idl (Union : N_Union_Acc;
                           Container_Be : Be_Container_Acc)
   is
      Be : Be_Named_Node_Acc;
      It_Case, It_Label : Node_Iterator;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      A_Case : N_Case_Acc;

      --  Case containing a default label.
      Default_Case : N_Case_Acc;
   begin
      --  Ada mapping §1.14
      --   An IDL union type shall map to an Ada discriminated record type.
      --   The type name shall be mapped from the IDL identifier.  The
      --   discriminant shall be formed with name "Switch" and shall be of
      --   type mapped from the IDL switch_type_spec.  A default value for
      --   the discriminant shall be formed from the 'first value of the mapped
      --   switch_type_spec.  A variant shall be formed from each case
      --   contained in the switch_body as follow:
      --   - Discrete_choice_list: for case_labels specified by "case"
      --     followed by a const_exp, the const_exp defines a discrete_choice.
      --     For the "default" case_label, the discrete_choice is "others".
      --     If more than one case_label is associated with a case, they shall
      --     be "or"ed ogether.
      --   - Variant_component_list: The component_list of each variant shall
      --     contain one component formed from the element_spec using the
      --     mapping in section 1.13.
      --
      --  Additionnal rules: default case is the last one.
      --  Add a default case if none ?
      Be := new Be_Named_Node;
      Set_Back_End (Union.all, Be);
      Create_Ada_Identifier (Union.all, Container_Be);

      --  Pregenerate types.
      --  Also remember of a default case, in order to generate it at the
      --  end of the discriminated record.
      Init (It_Case, Union.Cases);
      Default_Case := null;
      while not Is_End (It_Case) loop
         A_Case := N_Case_Acc (Get_Node (It_Case));
         Generate_Idl_For_Decls (A_Case.C_Type, A_Case.C_Decl, Container_Be);
         if Default_Case = null then
            Init (It_Label, A_Case.Labels);
            while not Is_End (It_Label) loop
               if Get_Node (It_Label) = null then
                  Default_Case := A_Case;
                  exit;
               end if;
               Next (It_Label);
            end loop;
         end if;
         Next (It_Case);
      end loop;

      --  Generate union declaration.
      Add_Text (Spec, "  type " & Get_Ada_Name (Union.all) & " (Switch : ");
      Generate_Idl_For_Type (Union.Switch_Type, Spec, Container_Be);
      Add_Text (Spec, " := ");
      Generate_Idl_For_Type (Union.Switch_Type, Spec, Container_Be);
      Add_Text (Spec, "'First) is record");
      Add_Newline (Spec);
      Add_Text (Spec, "    case Switch is");
      Add_Newline (Spec);

      Init (It_Case, Union.Cases);
      while not Is_End (It_Case) loop
         A_Case := N_Case_Acc (Get_Node (It_Case));
         if A_Case /= Default_Case then
            Generate_Idl (A_Case, Container_Be);
         end if;
         Next (It_Case);
      end loop;
      if Default_Case /= null then
         Generate_Idl (Default_Case, Container_Be);
      else
         Add_Text (Spec, "      when others =>");
         Add_Newline (Spec);
         Add_Text (Spec, "        null;");
         Add_Newline (Spec);
      end if;
      Add_Text (Spec, "    end case;");
      Add_Newline (Spec);
      Add_Text (Spec, "  end record;");
      Add_Newline (Spec);
      Generate_Any (Get_Ada_Name (Union.all), Spec);
   end Generate_Idl;

   procedure Generate_Idl (Type_Decl : N_Type_Declarator_Acc;
                           Container_Be : Be_Container_Acc)
   is
      It : Node_Iterator;
      Declarator : N_Declarator_Acc;
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
      Is_Subtype : Boolean;
   begin
      --  Ada mapping § 1.20
      --   IDL typedefs introduce new names for types.  An IDL-typedef is
      --   formed from the keyword "typedef", a type specification, and one
      --   or more declarators.  A declarator may be a simple declarator
      --   consisting of an identifier, or an array declarator consisting
      --   of an identifier and one or mode fixed array size.  An IDL typedef
      --   maps to an Ada derived type.
      --
      --   Each array_declarator in a typedef shall be mapped to an array
      --   type.  The array type name shall be the identifier contained in
      --   the array_declarator.  The type definition shall be an array over
      --   the range(s) from 0 to one less than the fixed_array_size(s).
      --   specified in the array declarator of the type mapped from the IDL
      --   type contained in the type specification.  If multiple bounds are
      --   declared, a multiple dimensional array shall be created that
      --   preserves the indexing order specified in the IDL declaration.
      --
      --   Each simple declarator for a non-reference type (ie, a type not in
      --   CORBA.Object.Ref'Class) shall be mapped to a derived type
      --   declaration.  Each simple declarator for a reference type shall
      --   be mapped to a subtype declaration.  The type name shall be the
      --   identifier provided in the simple declarator.  The type definition
      --   shall be the mapping of the typespec, as specified elsewhere in this
      --   section.
      Init (It, Type_decl.Declarators);
      while not Is_End (It) loop
         Declarator := N_Declarator_Acc (Get_Node (It));
         Be := new Be_Named_Node;
         Set_Back_End (Declarator.all, Be);
         Create_Ada_Identifier (Declarator.all, Container_Be);
         Pre_Generate_Idl_Type_Spec
           (Type_Decl.T_Type, Declarator, Container_Be);
         if Declarator.Array_Bounds /= Nil_List then
            Add_Text
              (Spec, "  type " & Get_Ada_Name (Declarator.all) & " is ");
            Is_Subtype := False;
         elsif Get_Kind (Type_Decl.T_Type.all) = K_Interface then
            Add_Text
              (Spec, "  subtype " & Get_Ada_Name (Declarator.all) & " is ");
            Is_Subtype := True;
         else
            Add_Text
              (Spec, "  type " & Get_Ada_Name (Declarator.all) & " is new ");
            Is_Subtype := False;
         end if;
         Generate_Idl_For_Complex_Type
           (Type_Decl.T_Type, Declarator, Container_Be);
         Add_Text (Spec, ";");
         Add_Newline (Spec);
         Next (It);
         if not Is_Subtype then
            Generate_Any (Get_Ada_Name (Declarator.all), Spec);
         end if;
      end loop;
   end Generate_Idl;

   procedure Generate_Idl (Const : N_Const_Acc;
                           Container_Be : Be_Container_Acc)
   is
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
   begin
      --  Ada mapping § 1.19
      --   An IDL constant shall map directly to an Ada constant.  The Ada
      --   constant name shall be mapped from the identifier in the IDL
      --   declaration.  The type of the Ada constant shall be mapped from the
      --   IDL const_type as specified elsewhere in this section.  The value
      --   of the Ada constant shall be mapped from the IDL constant
      --   expression as specified in section 1.3.3 "Mapping of Constant
      --   Expressions".  This mapping may yield a semantically equivalent
      --   literal of the correct type or a syntactically equivalent Ada
      --   expression that evaluates to the correct type and value.
      Be := new Be_Named_Node;
      Set_Back_End (Const.all, Be);
      Create_Ada_Identifier (Const.all, Container_Be);
      Pre_Generate_Idl_Type_Spec (Const.C_Type, null, Container_Be);
      Add_Text (Spec, "  " & Get_Ada_Name (Const.all) & " : constant ");
      Generate_Idl_For_Type (Const.C_Type, Spec, Container_Be);
      Add_Text (Spec, " := ");
      Generate_Idl_For_Const (Const.Expr, Spec, Container_Be);
      Add_Text (Spec, ";");
      Add_Newline (Spec);
   end Generate_Idl;

   procedure Generate_Idl (Except : N_Exception_Acc;
                           Container_Be : Be_Container_Acc)
   is
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container_Be.Client.P_Spec;
   begin
      --  Ada mapping § 1.23
      --   The IDL exception declaration shall map direclty to an Ada exception
      --   declaration where the name of the Ada exception is mapped from the
      --   IDL exception identifier.
      --
      --  § 1.23.2.2
      --   For an application-specific exception declaration, a type
      --   extended from the abstract type, IDL_Exception_Members, shall be
      --   declared where the type name will be the concatenation of the
      --   exception identifier with "_Members".  Each members shall be mapped
      --   to a component of the extension.  The name used for each
      --   component shall be mapped from the member name.  The type of each
      --   exception member shall be mapped form the IDL member type as
      --   specified elsewhere in this document.
      --
      --   The mapping shall also provide a concrete function, Get_Members,
      --   that returns the exception member from an object of type:
      --    Ada.Exceptions.Exception_Occurrence.
      Pre_Generate_Idl_For_Members (Except.Members, Container_be);
      Be := new Be_Named_Node;
      Set_Back_End (Except.all, Be);
      Create_Ada_Identifier (Except.all, Container_Be);
      Add_With (Spec, "Ada.Exceptions");
      Add_Text (Spec, "  " & Get_Ada_Name (Except.all) & " : exception;");
      Add_Newline (Spec);
      Add_Text
        (Spec, "  type " & Get_Ada_Name (Except.all) & "_Members is new");
      Add_Newline (Spec);
      Add_Text (Spec, "    CORBA.IDL_Exception_Members with ");
      if Except.Members = Nil_List then
         Add_Text (Spec, "null record;");
         Add_Newline (Spec);
      else
         Add_Text (Spec, "record");
         Add_Newline (Spec);
         Generate_Idl_For_Members (Except.Members, Container_Be);
         Add_Text (Spec, "  end record;");
         Add_Newline (Spec);
      end if;
      Generate_Any (Get_Ada_Name (Except.all) & "_Members", Spec);
      Add_Text (Spec, "  procedure Get_Members (" &
                  "From : in Ada.Exceptions.Exception_Occurrence;");
      Add_Newline (Spec);
      Add_Text (Spec, "                         To : out "
                & Get_Ada_Name (Except.all) & "_Members);");
      Add_Newline (Spec);
   end Generate_Idl;

   --  Append the definition of function/procedure to get or set an attribute.
   --  If IS_GET is true, the function to get is appended, else the
   --  procedure to set is appended.
   --  Only the definition is appended, no semi-colon, no newline.
   --  Be careful: do not call this subprogram with IS_GET false, for a
   --  readonly attribute, since it will generate the procedure.
   procedure Add_definition
     (Attr : N_Attribute_Acc;
      Container : Be_Interface_Acc;
      Unit : in out Unit_Decl;
      Package_Type : Package_Kind;
      Is_Get : boolean)
   is
      Be : Be_Named_Node_Acc;
   begin
      --  Ada mapping § 1.24
      --   Read-only attributes shall be mapped to an Ada functin with name
      --   formed by prepending "Get_" to the mapped attribute name.
      --   Read-write attributes shall be mapped to an Ada function with name
      --   formed by prepending "Get_" to the mapped attribute name and an
      --   Ada procedure with name formed by prepending "Set_" to the mapped
      --   attribute name.  The Set procedure takes a controlling parameter
      --   of object reference type and name "Self", and a parameter with
      --   name "To".  The type of the To parameter shall be mapped from the
      --   attribute type, except for an attribute of the enclosing interface
      --   type, which shall be mapped as Ref'Class.  The Get function takes
      --   a controlling parameter only, of object reference type and name
      --   "Self".  The Get function returns the type mapped from the
      --   attribute type, except for an attribute of the enclosing interface
      --   type, which shall be mapped as Ref'Class.
      Be := Be_Named_Node_Acc (Get_Back_End (Attr.all));
      if Is_Get then
         Add_Text (Unit, "  function Get_" & Get_Ada_Name (Attr.all));
         case Package_Type is
            when Package_Client =>
               Add_Text (Unit, " (Self : in Ref) return ");
            when Package_Impl =>
               Add_Text (Unit, " (Self : access Object) return ");
         end case;

         Generate_Idl_For_Type
           (Attr.A_Type, Unit, Be_Container_Acc (Container));
      else
         Add_Text (Unit, "  procedure Set_" & Get_Ada_Name (Attr.all));
         case Package_Type is
            when Package_Client =>
               Add_Text (Unit, " (Self : in Ref; To : in ");
            when Package_Impl =>
               Add_Text (Unit, " (Self : access Object; To : in ");
         end case;

         Generate_Idl_For_Type
           (Attr.A_Type, Unit, Be_Container_Acc (Container));
         Add_Text (Unit, ")");
      end if;
   end Add_Definition;

   --  Generate the get and set (if not readonly) subprogram declaration
   --  for attribute ATTR.
   procedure Add_definition
     (Attr : N_Attribute_Acc;
      Container : Be_Interface_Acc;
      Unit : in out Unit_Decl;
      Package_Type : Package_Kind)
   is
   begin
      Add_Definition (Attr, Container, Unit, Package_Type, True);
      Add_Text (Unit, ";");
      Add_Newline (Unit);
      if not Attr.Is_Readonly then
         Add_Definition (Attr, Container, Unit, Package_Type, False);
         Add_Text (Unit, ";");
         Add_Newline (Unit);
      end if;
   end Add_Definition;

   procedure Generate_Idl
     (Attr : N_Attribute_Acc; Container : Be_Interface_Acc)
   is
      Be : Be_Named_Node_Acc;
      Spec : Unit_Decl renames Container.Client.P_Spec;
      Ispec : Unit_Decl renames Container.Impl.P_Spec;
   begin
      --  Ada mapping § 1.24
      --   Read-only attributes shall be mapped to an Ada functin with name
      --   formed by prepending "Get_" to the mapped attribute name.
      --   Read-write attributes shall be mapped to an Ada function with name
      --   formed by prepending "Get_" to the mapped attribute name and an
      --   Ada procedure with name formed by prepending "Set_" to the mapped
      --   attribute name.  The Set procedure takes a controlling parameter
      --   of object reference type and name "Self", and a parameter with
      --   name "To".  The type of the To parameter shall be mapped from the
      --   attribute type, except for an attribute of the enclosing interface
      --   type, which shall be mapped as Ref'Class.  The Get function takes
      --   a controlling parameter only, of object reference type and name
      --   "Self".  The Get function returns the type mapped from the
      --   attribute type, except for an attribute of the enclosing interface
      --   type, which shall be mapped as Ref'Class.
      Be := Be_Named_Node_Acc (Get_Back_End (Attr.all));
      if Be = null then
         Be := new Be_Named_Node;
         Set_Back_End (Attr.all, Be);
         Create_Ada_Identifier (Attr.all, Be_Container_Acc (Container));
      end if;
      Pre_Generate_Idl_Type_Spec
        (Attr.A_Type, null, Be_Container_Acc (Container));

      Add_Definition (Attr, Container, Spec, Package_Client);
      Add_Definition (Attr, Container, Ispec, Package_Client);
   end Generate_Idl;

   --  Mark Node and its parents.
   procedure Mark_Nodes (Node : N_Interface_Acc)
   is
      Parent_It : Node_Iterator;
      Parent : N_Interface_Acc;
      Be : Be_Interface_Acc;
   begin
      Be := Be_Interface_Acc (Get_Back_End (Node.all));
      Be.Seen := True;
      Init (Parent_It, Node.Parents);
      while not Is_End (Parent_It) loop
         Parent :=
           N_Interface_Acc (N_Scoped_Name_Acc (Get_Node (Parent_It)).Value);
         Mark_Nodes (Parent);
         Next (Parent_It);
      end loop;
   end Mark_Nodes;

   --  Unmark Node and its parents.
   procedure Unmark_Nodes (Node : N_Interface_Acc)
   is
      Parent_It : Node_Iterator;
      Parent : N_Interface_Acc;
      Be : Be_Interface_Acc;
   begin
      Be := Be_Interface_Acc (Get_Back_End (Node.all));
      Be.Seen := False;
      Init (Parent_It, Node.Parents);
      while not Is_End (Parent_It) loop
         Parent :=
           N_Interface_Acc (N_Scoped_Name_Acc (Get_Node (Parent_It)).Value);
         Mark_Nodes (Parent);
         Next (Parent_It);
      end loop;
   end Unmark_Nodes;

   --  Generate code for operations inherited by N_interface.
   procedure Generate_Inherited_Operations
     (Node : N_Interface_Acc; Container : Be_Interface_Acc)
   is
      Parent_It : Node_Iterator;
      Op_It : Node_Iterator;
      Parent : N_Interface_Acc;
      Be : Be_Interface_Acc;
      Sub_Node : Node_Acc;
   begin
      Be := Be_Interface_Acc (Get_Back_End (Node.all));
      if Be.Seen then
         return;
      else
         Be.Seen := True;
      end if;

      --  Generate code for this interface operations.
      Init (Op_It, Node.Contents);
      while not Is_End (Op_It) loop
         Sub_Node := Get_Node (Op_It);
         case Get_Kind (Sub_Node.all) is
            when K_Operation =>
               Generate_Idl (N_Operation_Acc (Sub_Node), Container);
            when K_Attribute =>
               Generate_Idl (N_Attribute_Acc (Sub_Node), Container);
            when others =>
               null;
         end case;
         Next (Op_It);
      end loop;

      Init (Parent_It, Node.Parents);
      while not Is_End (Parent_It) loop
         Parent :=
           N_Interface_Acc (N_Scoped_Name_Acc (Get_Node (Parent_It)).Value);
         Generate_Inherited_Operations (Parent, Container);
         Next (Parent_It);
      end loop;
   end Generate_Inherited_Operations;

   procedure Generate_Idl
     (Node : N_Interface_Acc; Container : Be_Container_Acc)
   is
      Be : Be_Interface_Acc;
      It : Node_Iterator;
      Parent : N_Scoped_Name_Acc;
      Nbr_Parents : Natural;
   begin
      --  Ada mapping, § 1.8
      --   An IDL interface will be mapped to a child package of the package
      --   associated with its enclosing name scope (if any) or to a root
      --   library package (if there is no enclosing name scope).
      --  FIXME.
      Be := new Be_Interface;
      Set_Back_End (Node.all, Be);
      Create_Ada_Identifier (Node.all, Container);
      Create_Full_Name (Node.all);

      declare
         Spec : Unit_Decl renames Be.Client.P_Spec;
         Ispec : Unit_Decl renames Be.Impl.P_Spec;
         Ipriv : Unit_Decl renames Be.Impl.P_Priv;
      begin
         --  These packages are needed at least for To_Ref.
         Add_With (Spec, "CORBA.Object");

         --  Ada mapping, § 1.8
         --   This "interface package" shall define a new controlled tagged
         --   type, with name "Ref", used to represent object references for
         --   the mapped interface.  This reference type shall be derived
         --   from an implementation-specific type named "CORBA.Object.Ref"
         --   or from its parent Ref type as specified in Section 1.8.2,
         --   "Interface and Inheritance".

         --  Ada mapping § 1.41
         --   If the interface has no parents, the type Object shall be
         --   declared as an extension of PortableServer.Servant_Base.  If the
         --   interface has multiple parent, the type object shall be an
         --   extension of the Object type mapped from the parent interface.
         --   If the interface has multiple parent the type Object shall be
         --   an extension of the Object type mapped from the first-named
         --   parent interface.
         if Node.Parents = Nil_List then
            --  FIXME:  tgi: can "with private" be used instead ???
            Add_Text (Spec,
                      "  type Ref is new CORBA.Object.Ref with null record;");
            Add_Newline (Spec);
            Add_Text (Ispec, "  type Object is new PortableServer.Servant_Base"
                      & " with private;");
            Add_Newline (Ispec);
            Add_Text (Ipriv, "  type Object is new PortableServer.Servant_Base"
                      & " with record");
            Add_Newline (Ipriv);
            Nbr_Parents := 0;
         else
            Init (It, Node.Parents);
            Parent := N_Scoped_Name_Acc (Get_Node (It));

            Add_Text (Spec, "  type Ref is new ");
            Generate_Idl (Parent, Spec, Be_Container_Acc (Be));
            Add_Text (Spec, " with null record;");
            Add_Newline (Spec);

            Add_Text (Ispec, "  type Object is new ");
            Generate_Idl (Parent, Ispec, Be_Container_Acc (Be), Package_Impl);
            Add_Text (Ispec, " with private;");
            Add_Newline (Ispec);

            Add_Text (Ipriv, "  type Object is new ");
            Generate_Idl (Parent, Ipriv, Be_Container_Acc (Be), Package_Impl);
            Add_Text (Ipriv, " with record");
            Add_Newline (Ipriv);

            Next (It);
            if Is_End (It) then
               Nbr_Parents := 1;
            else
               --  2 or more, in fact.
               Nbr_Parents := 2;
            end if;
         end if;
         Add_Text (Ipriv, "    null;");
         Add_Newline (Ipriv);
         Add_Text (Ipriv, "    -- (implementation data)");
         Add_Newline (Ipriv);
         Add_Text (Ipriv, "  end record;");
         Add_Newline (Ipriv);

         --  Ada mapping § 1.8.5
         --   An all purpose widening and narrowing method, To_Ref, is defined
         --   for all interfaces that provide object reference operations.
         --   This function shall support widening (and narrowing) along all
         --   lines of descent.
         --  FIXME: todo

         --  Ada mapping § 1.8.6
         --   Conforming implementations must provide a To_Ref primitive
         --   subprogram in each interface package to perform and check the
         --   narrowing operation.  Unlike widening, narrowing canno be
         --   accomplished via normal Ada language mechanism.
         --
         --   Each interface mapping shall include a function with
         --   specification: [...]
         Add_Text (Spec, "  function To_Ref "
                   & "(The_Ref : in CORBA.Object.Ref'Class) return Ref;");
         Add_Newline (Spec);

         --  [...]
         --   For cases where the application programmer wishes to avoid the
         --   possibility of this remote invocation, conforming implementations
         --   must provide a primitive subprogram in each interface package to
         --   perform an unchecked narrow operation.  Each interface mapping
         --   shall include a function with specifications: [...]
         Add_Text (Spec, "  function Unchecked_To_Ref " &
                   "(The_Ref : in CORBA.Object.Ref'Class) return Ref;");
         Add_Newline (Spec);

         --  Ada mapping, § 1.8.3 (mapping forward declarations).
         --   Within the full declaration of the forward declared interface,
         --   the nested Convert package shall be instantiated with the
         --   actual Ref type.  The name of the instantiation shall be
         --   Convert_Forward;
         if Node.Forward /= null then
            Add_With
              (Spec,
               Be_Container_Acc (Get_Back_End (Node.Forward.all)).Full_Name);
            Add_Text (Spec, "  package Convert_Forward is new ");
            Add_Full_Ada_Name (Spec, Node.Forward.all);
            Add_Text (Spec, ".Convert (Ref);");
            Add_Newline (Spec);
         end if;

         Generate_Any ("Ref", Spec);

         --  Ada mapping, § 1.8
         --   The declaration of constants, exceptions, and types scoped within
         --   interfaces shall be mapped to declarations with the mapped Ada
         --   package.
         Generate_Idl_For_Scope (Node.Contents, Be_Container_Acc (Be));
      end;

      --  Generate operation from 'other parents' (ie, other that the first
      --  named parent).
      --  FIXME: Is there a special rule ????
      if Nbr_Parents > 1 then
         Init (It, Node.Parents);
         Parent := N_Scoped_Name_Acc (Get_Node (It));
         --  Operations from first named parent has inherited via Ada type
         --  extension rule.
         Mark_Nodes (N_Interface_Acc (Parent.Value));
         Next (It);
         --  Generate code for other parents operations.
         while not Is_End (It) loop
            Parent := N_Scoped_Name_Acc (Get_Node (It));
            Generate_Inherited_Operations (N_Interface_Acc (Parent.Value), Be);
            Next (It);
         end loop;
         --  Unmark all parents.
         Unmark_Nodes (Node);
      end if;
   end Generate_Idl;

   procedure Generate_Idl
     (Fw : N_Forward_Interface_Acc; Container : Be_Container_Acc)
   is
      Be : Be_Container_Acc;
   begin
      --  Ada mapping § 1.8.3
      --   An instantiation of CORBA.Forward shall be performed for every
      --   forward declaration of an interface.  The name of the
      --   instantiation shall be the interface name appended by "_Forward".
      --   All references to the forward declared interface before the full
      --   declaration of the interface shall be mapped to the Ref type
      --   in this instantiated package.
      Be := new Be_Container;
      Set_Back_End (Fw.all, Be);
      Be.Ada_Id :=
       new String'(Map_Identifier (Get_Name (Fw.Forward.all) & "_Forward"));
      Be.Parent := Container;
      Create_Full_Name (Fw.all);
      Add_With (Be.Client.P_Spec, "CORBA.Forward");
      Add_Text (Be.Client.P_Spec,
                "package " & Get_Ada_Name (Fw.all) & " is new CORBA.Forward;");
      Add_Newline (Be.Client.P_Spec);
   end Generate_Idl;

   procedure Generate_Idl (Module : N_Module_Acc; Container : Be_Container_Acc)
   is
      Be : Be_Container_Acc;
   begin
      Be := new Be_Container;
      Set_Back_End (Module.all, Be);
      Create_Ada_Identifier (Module.all, Container);
      Create_Full_Name (Module.all);
      Generate_Idl_For_Scope (Module.Contents, Be);
   end Generate_Idl;

   procedure Generate_Idl_For_Scope
     (List : Node_List;
      Container : Be_Container_Acc;
      Is_Repository : Boolean := False)
   is
      It : Node_Iterator;
      N : Node_Acc;
   begin
      Init (It, List);
      while not Is_End (It) loop
         N := Get_Node (It);
         case Get_Kind (N.all) is
            when K_Module =>
               if Is_Repository then
                  Generate_Idl (N_Module_Acc (N), null);
               else
                  Generate_Idl (N_Module_Acc (N), Container);
               end if;
            when K_Interface =>
               if Is_Repository then
                  Generate_Idl (N_Interface_Acc (N), null);
               else
                  Generate_Idl (N_Interface_Acc (N), Container);
               end if;
            when K_Forward_Interface =>
               if Is_Repository then
                  Generate_Idl (N_Forward_Interface_Acc (N), null);
               else
                  Generate_Idl (N_Forward_Interface_Acc (N), Container);
               end if;
            when K_Operation =>
               Generate_Idl
                 (N_Operation_Acc (N), Be_Interface_Acc (Container));
            when K_Attribute =>
               Generate_Idl
                 (N_Attribute_Acc (N), Be_Interface_Acc (Container));
            when K_Type_Declarator =>
               Generate_Idl (N_Type_Declarator_Acc (N), Container);
            when K_Enum =>
               Generate_Idl (N_Enum_Acc (N), Container);
            when K_Struct =>
               Generate_Idl (N_Struct_Acc (N), Container);
            when K_Union =>
               Generate_Idl (N_Union_Acc (N), Container);
            when K_Const =>
               Generate_Idl (N_Const_Acc (N), Container);
            when K_Exception =>
               Generate_Idl (N_Exception_Acc (N), Container);
            when others =>
               Error ("generate_idl_for_scope", Get_Kind (N.all));
         end case;
         Next (It);
      end loop;
   end Generate_Idl_For_Scope;

   --  Extract base_name of STR.
   --  STR is supposed to be a filename, and more precisely an UNIX filename.
   --  Get_Base_Name remove any leading directories, with is '/', and any
   --  extensions (parts beginning with a '.').
   function Get_Base_Name (Str : String) return String is
      S, E : Positive;
   begin
      --  Remove leading directories.
      S := Str'First;
      for I in Str'Range loop
         if Str (I) = '/' then
            S := I + 1;
         end if;
      end loop;
      --  Remove extension.
      E := Str'Last;
      for I in S .. Str'Last loop
         if Str (I) = '.' then
            E := I - 1;
            exit;
         end if;
      end loop;
      return Str (S .. E);
   end Get_Base_Name;

   procedure Generate_Idl (Repository : N_Repository_Acc; Filename : String) is
      Need_File_Package : Boolean;
      Be : Be_Container_Acc;
      It : Node_Iterator;
      N : Node_Acc;
   begin
      --  Ada mappping, 1.5.4:
      --   If all the IDL statements in a file are enclosed by a single module
      --   or interface definition, the generation of this 'file package'
      --   is optional.
      Be := new Be_Container;
      Be.Ada_id := new String'
        (Map_Identifier (Get_Base_Name (Filename) & "_IDL_File"));
      Be.Parent := null;
      Be.Full_Name := Be.Ada_Id;
      Set_Back_End (Repository.all, Be);

      --  Generate all the contents.
      Generate_Idl_For_Scope (Repository.Contents, Be, True);

      --  Count number of interface and module.
      --  Also, remember of a module or interface.  If there is only one
      --   interface or module, ROOT will point to it, and only code for this
      --   will be generated.
      Need_File_Package := False;
      Init (It, Repository.Contents);
      while not Is_End (It) loop
         N := Get_Node (It);
         case Get_Kind (N.all) is
            when K_Interface | K_Module | K_Forward_Interface =>
               null;
            when others =>
               Need_File_Package := True;
               exit;
         end case;
         Next (It);
      end loop;

      --  Display it.
      if Need_File_Package then
         Disp_Ada (Node_Acc (Repository));
      else
         Disp_Ada (Repository.Contents);
      end if;
   end Generate_Idl;

end Be_Ada;
