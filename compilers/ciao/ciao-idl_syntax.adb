----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  IDL syntactic information
--  $Id: //depot/ciao/main/ciao-idl_syntax.adb#23 $

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis;

with CIAO.IDL_Syntax.Scoped_Names; use CIAO.IDL_Syntax.Scoped_Names;
with CIAO.Namet;  use CIAO.Namet;
with CIAO.Nlists; use CIAO.Nlists;

package body CIAO.IDL_Syntax is

   use CIAO.IDL_Tree;

   -------------------------
   -- Allocator functions --
   -------------------------

   function New_Specification
     return Node_Id is
   begin
      return New_Node (N_Specification);
   end New_Specification;

   function New_Include_Directive
     return Node_Id is
   begin
      return New_Node (N_Preprocessor_Include);
   end New_Include_Directive;

   function New_Name
     (Source_Val : Asis.Program_Text)
     return Name_Id is

   begin
      for I in Source_Val'Range loop
         Namet.Name_Buffer
           (I - Source_Val'First + Name_Buffer'First) :=
           To_Character (Source_Val (I));
      end loop;
      Namet.Name_Len := Source_Val'Length;

      return Name_Enter;
   end New_Name;

   function New_Constructed_Type_Identifier
     (Type_Declarator_Identifier : Name_Id;
      Suffix                     : ASIS.Program_Text)
     return Name_Id is
   begin
      Get_Name_String (Type_Declarator_Identifier);
      Name_Buffer (1 .. Name_Len + Suffix'Length + 1) :=
        Name_Buffer (1 .. Name_Len) & "_" & To_String (Suffix);
      Name_Len := Name_Len + Suffix'Length + 1;

      return Name_Enter;
   end New_Constructed_Type_Identifier;

   function New_Simple_Type_Spec return Node_Id is
      Type_Spec_Node        : Node_Id;
      Simple_Type_Spec_Node : Node_Id;
   begin
      Type_Spec_Node := New_Node (N_Type_Spec);

      Simple_Type_Spec_Node := New_Node (N_Simple_Type_Spec);
      Set_Parent (Simple_Type_Spec_Node, Type_Spec_Node);
      Set_Specific_Type_Spec (Type_Spec_Node,
                              Simple_Type_Spec_Node);

      return Type_Spec_Node;
   end New_Simple_Type_Spec;

   function New_Opaque_Type return Node_Id is
      Type_Spec_Node        : Node_Id;
      Simple_Type_Spec_Node : Node_Id;
      Member_Type_Spec_Node : Node_Id;
      Sequence_Node         : Node_Id;
      Octet_Node            : Node_Id;
   begin
      Type_Spec_Node := New_Simple_Type_Spec;
      Simple_Type_Spec_Node := Specific_Type_Spec (Type_Spec_Node);

      Sequence_Node := New_Node (N_Sequence_Type);
      Set_Template_Type_Spec (Simple_Type_Spec_Node, Sequence_Node);
      Set_Parent (Sequence_Node, Simple_Type_Spec_Node);

      Member_Type_Spec_Node := New_Node (N_Simple_Type_Spec);
      Set_Specific_Type_Spec (Sequence_Node, Member_Type_Spec_Node);
      Set_Parent (Member_Type_Spec_Node, Sequence_Node);

      Octet_Node := New_Node (N_Base_Type_Octet);
      Set_Base_Type_Spec (Member_Type_Spec_Node, Octet_Node);
      Set_Parent (Octet_Node, Member_Type_Spec_Node);

      return Type_Spec_Node;
   end New_Opaque_Type;

   function New_Standard_Name
     (Source_Val : Asis.Program_Text)
     return Node_Id is
      Norm_Source_Val : Asis.Program_Text (Source_Val'Range);
      Uppercase_Next : Boolean := True;
   begin
      --  Ensure consistent spelling: capitalize initial of each word.
      for I in Source_Val'Range loop
         if Uppercase_Next then
            Norm_Source_Val (I) := To_Wide_Character
              (To_Upper (To_Character (Source_Val (I))));
            Uppercase_Next := False;
         else
            Norm_Source_Val (I) := To_Wide_Character
              (To_Lower (To_Character (Source_Val (I))));
            if Source_Val (I) = '_' then
               Uppercase_Next := True;
            end if;
         end if;
      end loop;

      return New_Scoped_Name ("::CIAO::Standard::Ada_" & Norm_Source_Val);
   end New_Standard_Name;

   function New_Base_Type (Base_Type_Kind : N_Base_Type_Spec)
     return Node_Id is
   begin
      return New_Node (Base_Type_Kind);
   end New_Base_Type;

   function New_Scoped_Name
     (Source_Val : Asis.Program_Text)
   return Node_Id is
      Identifier_First,
        Identifier_Last : Asis.ASIS_Integer;
      Scoped_Name_Node : Node_Id := Empty;
   begin
      Identifier_Last := Source_Val'Last;
      while Identifier_Last >= Source_Val'First loop
         Identifier_First := Identifier_Last;
         while Identifier_First > Source_Val'First
           and then Source_Val (Identifier_First) /= ':' loop
            Identifier_First := Identifier_First - 1;
         end loop;
         if Source_Val (Identifier_First) = ':' then
            Identifier_First := Identifier_First + 1;
         end if;

         declare
            N : constant Node_Id := New_Node (N_Scoped_Name);
         begin
            Set_Name (N, New_Name (Source_Val (Identifier_First .. Identifier_Last)));
            if No (Scoped_Name_Node) then
               Scoped_Name_Node := N;
            else
               Add_Prefix (Scoped_Name_Node, N);
            end if;
         end;

         if Identifier_First >= Source_Val'First + 2 then
            pragma Assert (True
                and then Source_Val (Identifier_First - 1) = ':'
                and then Source_Val (Identifier_First - 2) = ':');
            Identifier_Last := Identifier_First - 3;
            if Identifier_Last < Source_Val'First then
               Add_Absolute (Scoped_Name_Node);
            end if;
         else
            pragma Assert (Identifier_First = Source_Val'First);
            exit;
         end if;
      end loop;
      Chain_Prefixes (Scoped_Name_Node);
      return Scoped_Name_Node;
   end New_Scoped_Name;

   function New_Constructed_Type return Node_Id is
      Type_Spec_Node        : Node_Id;
      Constr_Type_Spec_Node : Node_Id;
   begin
      Type_Spec_Node := New_Node (N_Type_Spec);

      Constr_Type_Spec_Node := New_Node (N_Constr_Type_Spec);
      Set_Parent (Constr_Type_Spec_Node, Type_Spec_Node);
      Set_Specific_Type_Spec (Type_Spec_Node,
                              Constr_Type_Spec_Node);
      return Type_Spec_Node;
   end New_Constructed_Type;

   function New_Interface return Node_Id is
      Interface_Node        : Node_Id;
      Interface_Dcl_Node    : Node_Id;
      Interface_Header_Node : Node_Id;
   begin
      Interface_Node := New_Node (N_Interface);

      Interface_Dcl_Node := New_Node (N_Interface_Dcl);
      Set_Parent (Interface_Dcl_Node, Interface_Node);
      Set_Specific_Interface (Interface_Node,
                              Interface_Dcl_Node);

      Interface_Header_Node := New_Node (N_Interface_Header);
      Set_Parent (Interface_Header_Node, Interface_Dcl_Node);
      Set_Interface_Header (Interface_Dcl_Node, Interface_Header_Node);

      return Interface_Node;
   end New_Interface;

   function New_Forward_Interface return Node_Id is
      Interface_Node   : Node_Id;
      Forward_Dcl_Node : Node_Id;
   begin
      Interface_Node := New_Node (N_Interface);

      Forward_Dcl_Node := New_Node (N_Forward_Dcl);
      Set_Parent (Forward_Dcl_Node, Interface_Node);
      Set_Specific_Interface (Interface_Node,
                              Forward_Dcl_Node);

      return Interface_Node;
   end New_Forward_Interface;

   function New_Operation return Node_Id is
      Op_Dcl_Node       : Node_Id;
      Op_Type_Spec_Node : Node_Id;
   begin
      Op_Dcl_Node := New_Node (N_Op_Dcl);

      Op_Type_Spec_Node := New_Node (N_Op_Type_Spec);
      Set_Parent (Op_Type_Spec_Node, Op_Dcl_Node);
      Set_Op_Type_Spec (Op_Dcl_Node, Op_Type_Spec_Node);

      return Op_Dcl_Node;
   end New_Operation;

   function New_Parameter return Node_Id is
      Param_Dcl_Node       : Node_Id;
      Param_Type_Spec_Node : Node_Id;
   begin
      Param_Dcl_Node := New_Node (N_Param_Dcl);

      Param_Type_Spec_Node := New_Node (N_Param_Type_Spec);
      Set_Parent (Param_Type_Spec_Node, Param_Dcl_Node);
      Set_Param_Type_Spec (Param_Dcl_Node, Param_Type_Spec_Node);

      return Param_Dcl_Node;
   end New_Parameter;

   function New_Keyword_Node (Keyword : Nkind)
     return Node_Id is
      N  : Node_Id;
      KN : constant Node_Id
        := New_Node (Keyword);
   begin
      case Keyword is
         when N_Keyword_Void =>
            return KN;
         when
           N_Keyword_In    |
           N_Keyword_Inout |
           N_Keyword_Out   =>
            N := New_Node (N_Param_Attribute);
         when others =>
            raise Program_Error;
      end case;

      Set_Parent (KN, N);
      -- XXX WARNING abstraction violation,
      -- this use of Node1 in N_Keyword_* is UNDOCUMENTED!
      Set_Node1 (N, KN);
      return N;
   end New_Keyword_Node;

   function New_In_Attribute return Node_Id is
   begin
      return New_Keyword_Node (N_Keyword_In);
   end New_In_Attribute;

   function New_Out_Attribute return Node_Id is
   begin
      return New_Keyword_Node (N_Keyword_Out);
   end New_Out_Attribute;

   function New_Inout_Attribute return Node_Id is
   begin
      return New_Keyword_Node (N_Keyword_Inout);
   end New_Inout_Attribute;

   function New_Void return Node_Id is
   begin
      return New_Keyword_Node (N_Keyword_Void);
   end New_Void;

   ---------------------------------------------------------------
   -- Allocators that insert the newly created node in the tree --
   ---------------------------------------------------------------

   function Insert_New_Member (Parent : Node_Id)
     return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Node (N_Member);
      Set_Parent (Node, Parent);
      Add_Member (Parent, Node);

      return Node;
   end Insert_New_Member;

   function Insert_New_Enumerator (Parent : Node_Id)
     return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Node (N_Enumerator);
      Set_Parent (Node, Parent);
      Add_Enumerator (Parent, Node);

      return Node;
   end Insert_New_Enumerator;

   function Insert_New_Simple_Type_Spec (Parent : Node_Id)
     return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Simple_Type_Spec;
      Set_Parent (Node, Parent);
      Set_Type_Spec (Parent, Node);

      return Node;
   end Insert_New_Simple_Type_Spec;

   function Insert_New_Constructed_Type (Parent : Node_Id)
     return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Constructed_Type;
      Set_Parent (Node, Parent);
      Set_Type_Spec (Parent, Node);

      return Node;
   end Insert_New_Constructed_Type;

   function Insert_New_Opaque_Type (Parent : Node_Id)
     return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Opaque_Type;
      Set_Parent (Node, Parent);
      Set_Type_Spec (Parent, Node);

      return Node;
   end Insert_New_Opaque_Type;

   -------------------------------------
   -- Syntactic information accessors --
   -------------------------------------

   function Directives (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Specification);
      return List1 (N);
   end Directives;

   function Name
     (N : in Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include
        or else Node_Kind (N) = N_Module
        or else Node_Kind (N) = N_Forward_Dcl
        or else Node_Kind (N) = N_Interface_Header
        or else Node_Kind (N) = N_Scoped_Name
        or else Node_Kind (N) = N_Simple_Declarator
        or else Node_Kind (N) = N_Struct_Type
        or else Node_Kind (N) = N_Union_Type
        or else Node_Kind (N) = N_Enum_Type
        or else Node_Kind (N) = N_Enumerator
        or else Node_Kind (N) = N_Array_Declarator
        or else Node_Kind (N) = N_Op_Dcl
        or else Node_Kind (N) = N_Param_Dcl);
      return Name1 (N);
   end Name;

   function Get_Name (N : Node_Id) return Wide_String is
   begin
      Get_Name_String (CIAO.IDL_Syntax.Name (N));
      return To_Wide_String (Name_Buffer (1 .. Name_Len));
   end Get_Name;

   function Declarators
     (N : in Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Declarator
        or else Node_Kind (N) = N_Member);
      return List1 (N);
   end Declarators;

   function Type_Declarator
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Dcl);
      return Node1 (N);
   end Type_Declarator;

   function Type_Spec
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Declarator
        or else Node_Kind (N) = N_Member
        or else Node_Kind (N) = N_Element_Spec);
      return Node2 (N);
   end Type_Spec;

   function Specific_Type_Spec
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Spec
        or else Node_Kind (N) = N_Sequence_Type);
      return Node1 (N);
   end Specific_Type_Spec;

   function Structure
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Constr_Type_Spec);
      return Node1 (N);
   end Structure;

   function Scoped_Name
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec
        or else Node_Kind (N) = N_Param_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Node1 (N)) = N_Scoped_Name);
      return Node1 (N);
   end Scoped_Name;

   function Template_Type_Spec
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec);
      if Node_Kind (Node1 (N)) = N_Sequence_Type then
         return Node1 (N);
      else
         return Empty;
      end if;
   end Template_Type_Spec;

   function Base_Type_Spec
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec
        or else Node_Kind (N) = N_Param_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Node1 (N)) in N_Base_Type_Spec);
      return Node1 (N);
   end Base_Type_Spec;

   function Specific_Declarator
     (N : in Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Declarator);
        return Node1 (N);
   end Specific_Declarator;

   function Definitions (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface
        or else Node_Kind (N) = N_Specification
        or else Node_Kind (N) = N_Module);
      return List2 (N);
   end Definitions;

   function Members (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Struct_Type);
      return List2 (N);
   end Members;

   function Enumerators (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Enum_Type);
      return List2 (N);
   end Enumerators;

   function Prefix        (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Scoped_Name);
      return Node2 (N);
   end Prefix;

   function Specific_Interface (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface);
      return Node1 (N);
   end Specific_Interface;

   function Interface_Header (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      return Node1 (N);
   end Interface_Header;

   function Inheritance_Spec (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Header);
      return List2 (N);
   end Inheritance_Spec;

   function Interface_Body  (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      return List2 (N);
   end Interface_Body;

   function Operation_Value_Type (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Type_Spec);
      return Node1 (N);
   end Operation_Value_Type;

   function Op_Type_Spec (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Dcl);
      return Node2 (N);
   end Op_Type_Spec;

   function Param_Dcls (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Dcl);
      return List3 (N);
   end Param_Dcls;

   function Param_Type_Spec (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Param_Dcl);
      return Node2 (N);
   end Param_Type_Spec;

   function Parameter_Attribute (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Param_Dcl);
      return Node3 (N);
   end Parameter_Attribute;

   function Fixed_Array_Sizes (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Array_Declarator);
      return List2 (N);
   end Fixed_Array_Sizes;

   function Size_Value (N : Node_Id)
     return Unbiased_Uint is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Fixed_Array_Size);
      return From_Uint (Uint1 (N));
   end Size_Value;

   ------------------------------------
   -- Semantic information accessors --
   ------------------------------------

   function Constants_Interface (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Module);
      return Node3 (N);
   end Constants_Interface;

   function Interfaces (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Specification
        or else Node_Kind (N) = N_Module
        or else Node_Kind (N) = N_Interface_Dcl);
      return List4 (N);
   end Interfaces;

   function Unit_Used (N : Node_Id)
     return Boolean is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include);
      return Flag1 (N);
   end Unit_Used;

   function Is_Remote_Subprograms (N : Node_Id)
     return Boolean is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      return Flag1 (N);
   end Is_Remote_Subprograms;

   function Translated_Unit (N : in Node_Id)
     return Node_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include);
      return Node2 (N);
   end Translated_Unit;

   ------------------------------------
   -- Corresponding Set_* procedures --
   ------------------------------------

   procedure Set_Name
     (N   : in Node_Id;
      Val : in Name_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include
        or else Node_Kind (N) = N_Module
        or else Node_Kind (N) = N_Forward_Dcl
        or else Node_Kind (N) = N_Interface_Header
        or else Node_Kind (N) = N_Scoped_Name
        or else Node_Kind (N) = N_Simple_Declarator
        or else Node_Kind (N) = N_Struct_Type
        or else Node_Kind (N) = N_Union_Type
        or else Node_Kind (N) = N_Enum_Type
        or else Node_Kind (N) = N_Enumerator
        or else Node_Kind (N) = N_Array_Declarator
        or else Node_Kind (N) = N_Op_Dcl
        or else Node_Kind (N) = N_Param_Dcl);
      Set_Name1 (N, Val);
   end Set_Name;

   procedure Set_Type_Declarator
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N)   = N_Type_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Type_Declarator);
      Set_Node1 (N, Val);
   end Set_Type_Declarator;

   procedure Set_Type_Spec
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Declarator
        or else Node_Kind (N) = N_Member
        or else Node_Kind (N) = N_Element_Spec);
      pragma Assert (False
        or else Node_Kind (Val) = N_Type_Spec);
      Set_Node2 (N, Val);
   end Set_Type_Spec;

   procedure Set_Specific_Type_Spec
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Spec
        or else Node_Kind (N) = N_Sequence_Type);
      pragma Assert (False
        or else Node_Kind (Val) = N_Simple_Type_Spec
        or else (Node_Kind (N) = N_Type_Spec
                 and then Node_Kind (Val) = N_Constr_Type_Spec));
      Set_Node1 (N, Val);
   end Set_Specific_Type_Spec;

   procedure Set_Structure
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Constr_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Val) = N_Struct_Type
        or else Node_Kind (Val) = N_Union_Type
        or else Node_Kind (Val) = N_Enum_Type);
      Set_Node1 (N, Val);
   end Set_Structure;

   procedure Set_Scoped_Name
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec
        or else Node_Kind (N) = N_Param_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Val) = N_Scoped_Name);
      Set_Node1 (N, Val);
   end Set_Scoped_Name;

   procedure Set_Template_Type_Spec
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Val) = N_Sequence_Type);
      Set_Node1 (N, Val);
   end Set_Template_Type_Spec;

   procedure Set_Base_Type_Spec
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Simple_Type_Spec
        or else Node_Kind (N) = N_Param_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Val) in N_Base_Type_Spec);
      Set_Node1 (N, Val);
   end Set_Base_Type_Spec;

   procedure Set_Specific_Declarator
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N)   = N_Declarator);
      pragma Assert (False
        or else Node_Kind (Val) = N_Simple_Declarator
        or else Node_Kind (Val) = N_Array_Declarator);
      Set_Node1 (N, Val);
   end Set_Specific_Declarator;

   procedure Set_Prefix
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (True
        and then Node_Kind (N) = N_Scoped_Name);
      pragma Assert (False
        or else Node_Kind (Val) = N_Absolute
        or else Node_Kind (Val) = N_Scoped_Name);
      Set_Node2 (N, Val);
   end Set_Prefix;

   procedure Set_Specific_Interface
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface);
      pragma Assert (False
        or else Node_Kind (Val) = N_Interface_Dcl
        or else Node_Kind (Val) = N_Forward_Dcl);
      Set_Node1 (N, Val);
   end Set_Specific_Interface;

   procedure Set_Interface_Header
     (N   : in Node_Id;
      Val : in Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Interface_Header);
      Set_Node1 (N, Val);
   end Set_Interface_Header;

   procedure Set_Operation_Value_Type
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Type_Spec);
      pragma Assert (False
        or else Node_Kind (Val) = N_Keyword_Void
        or else Node_Kind (Val) = N_Param_Type_Spec);
      Set_Node1 (N, Val);
   end Set_Operation_Value_Type;

   procedure Set_Op_Type_Spec
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Op_Type_Spec);
      Set_Node2 (N, Val);
   end Set_Op_Type_Spec;

   procedure Set_Param_Type_Spec
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Param_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Param_Type_Spec);
      Set_Node2 (N, Val);
   end Set_Param_Type_Spec;

   procedure Set_Parameter_Attribute
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Param_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Param_Attribute);
      Set_Node3 (N, Val);
   end Set_Parameter_Attribute;

   procedure Set_Constants_Interface
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Module);
      pragma Assert (False
        or else Node_Kind (Val) = N_Interface_Dcl);
      Set_Node3 (N, Val);
   end Set_Constants_Interface;

   procedure Set_Size_Value
     (N   : Node_Id;
      Val : Unbiased_Uint) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Fixed_Array_Size);
      Set_Uint1 (N, To_Uint (Val));
   end Set_Size_Value;

   procedure Set_Unit_Used
     (N   : Node_Id;
      Val : Boolean) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include);
      Set_Flag1 (N, Val);
   end Set_Unit_Used;

   procedure Set_Is_Remote_Subprograms
     (N   : Node_Id;
      Val : Boolean) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      Set_Flag1 (N, Val);
   end Set_Is_Remote_Subprograms;

   procedure Set_Translated_Unit
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Preprocessor_Include);
      Set_Node2 (N, Val);
   end Set_Translated_Unit;

   --------------------------------
   -- Add_* procedures for lists --
   --------------------------------

   procedure Add_Directive
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Specification);
      pragma Assert (False
        or else Node_Kind (Val) = N_Preprocessor_Include);

      Add_List1 (N, Val);
   end Add_Directive;

   procedure Add_Definition
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Specification
        or else Node_Kind (N) = N_Module
        or else Node_Kind (N) = N_Interface_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Type_Dcl
        --  or else Node_Kind (Val) = N_Const_Dcl
        --  or else Node_Kind (Val) = N_Except_Dcl
        or else Node_Kind (Val) = N_Interface
        or else Node_Kind (Val) = N_Module);

      Add_List2 (N, Val);
   end Add_Definition;

   procedure Add_Declarator
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Type_Declarator
        or else Node_Kind (N) = N_Member);
      pragma Assert (False
        or else Node_Kind (Val) = N_Declarator);

      Add_List1 (N, Val);
   end Add_Declarator;

   procedure Add_Member
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Struct_Type);
      pragma Assert (False
        or else Node_Kind (Val) = N_Member);

      Add_List2 (N, Val);
   end Add_Member;

   procedure Add_Enumerator
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Enum_Type);
      pragma Assert (False
        or else Node_Kind (Val) = N_Enumerator);

      Add_List2 (N, Val);
   end Add_Enumerator;

   procedure Add_Inherited_Interface
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Header);
      pragma Assert (False
        or else Node_Kind (Val) = N_Scoped_Name);

      Add_List2 (N, Val);
   end Add_Inherited_Interface;

   procedure Add_Export
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Interface_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Op_Dcl);

      Add_List2 (N, Val);
   end Add_Export;

   procedure Add_Param_Dcl
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Op_Dcl);
      pragma Assert (False
        or else Node_Kind (Val) = N_Param_Dcl);

      Add_List3 (N, Val);
   end Add_Param_Dcl;

   procedure Add_Interface
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Module
        or else Node_Kind (N) = N_Interface_Dcl
        or else Node_Kind (N) = N_Specification);
      pragma Assert (False
        or else Node_Kind (Val) = N_Interface);

      Add_List4 (N, Val);
   end Add_Interface;

   procedure Add_Fixed_Array_Size
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Array_Declarator);
      pragma Assert (False
        or else Node_Kind (Val) = N_Fixed_Array_Size);

      Add_List2 (N, Val);
   end Add_Fixed_Array_Size;

end CIAO.IDL_Syntax;
