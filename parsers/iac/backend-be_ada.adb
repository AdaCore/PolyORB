with GNAT.Command_Line; use GNAT.Command_Line;

with Charset;   use Charset;
with Errors;    use Errors;
with Lexer;     use Lexer;
with Locations; use Locations;
with Namet;     use Namet;
with Output;    use Output;
with Types;     use Types;

with Flags;     use Flags;

with Utils;     use Utils;


with Frontend.Nodes;           use Frontend.Nodes;

with Backend.BE_Ada.Debug;     use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Generator; use Backend.BE_Ada.Generator;
with Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;    use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada is

   Ada_Packages : List_Id;
   D_Tree       : Boolean := False;


   package BEN renames Backend.BE_Ada.Nodes;

   function All_Interfaces_Abstract (L : List_Id) return Boolean;
   --  Return False when L is empty or when one interface is concrete

   procedure Attribute_Getter_Function_Spec (N : Node_Id; L : in out List_Id);
   --  XXX comments

   procedure Attribute_Setter_Procedure_Spec (N : Node_Id; L : in out List_Id);
   --  XXX comments

   procedure Declare_CORBA_Type (K : BEN.Node_Kind; S : String := "");
   --  Declare CORBA type as predefined Ada type.

   function Generate_Package (E : Node_Id) return Node_Id;
   --  XXX comments

   function Generate_IDL_File (File_Name : Name_Id) return Node_Id;
   --  Generate the package, implementing the IDL File.

   function Get_Mapped_Type (E : Node_Id) return Node_Id;
   --  XXX comments

   procedure Insert_Type_Ref (L : in out List_Id; Inheritance_List : List_Id;
                                           Interface_Is_Abstract : Boolean);
   --  XXX comments

   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Spec : Node_Id) return Node_Id;
   --  XXX comments

   function Visit_Constant_Declaration (E : Node_Id) return Node_Id;
   --  XXX comments

   function Visit_Enumeration_Type (E : Node_Id) return Node_Id;
   --  XXX comments

   procedure Visit_Specification (E : Node_Id);
   --  XXX comments

   function Visit_Interface (E : Node_Id)return Node_Id;
   --  XXX comments

   function Visit_Module (E : Node_Id) return Node_Id;
   --  XXX comments

   function Visit_Operation_Declaration (E : Node_Id) return Node_Id;
   --  XXX comments

   function Visit_Type_Declaration (E : Node_Id) return List_Id;
   --  XXX comments

   function Image (N : Node_Kind) return String;

   -----------------------------
   -- All_Interfaces_Abstract --
   -----------------------------

   function All_Interfaces_Abstract (L : List_Id) return Boolean
   is
      N : Node_Id;
   begin
      if Is_Empty (L) then
         return False;
      end if;
      N := First_Node (L);
      while Present (N)  loop
         if Is_Abstract (N) then
            return False;
         end if;
         N := Next_Node (N);
      end loop;
      return True;
   end All_Interfaces_Abstract;

   ------------------------------------
   -- Attribute_Getter_Function_Spec --
   ------------------------------------

   procedure Attribute_Getter_Function_Spec
     (N : in     Node_Id;
      L : in out List_Id)
   is
      Param_Node  : Node_Id;
      Param_List  : List_Id := No_List;
      Param_Name  : Node_Id;
      Param_Type  : Node_Id;
      Subprogram  : Node_Id;
      Attr_Type   : Node_Id;
      Attribute   : Node_Id;
      Attr_Name   : Name_Id;

   begin
      pragma Assert (Kind (N) = K_Attribute_Declaration);

      --  Expected result :
      --  function Get_<A> (Self : Ref) return <T>;

      Param_Name := Make_Ada_Identifier ("Self");
      Param_Type := Make_Ada_Identifier ("Ref");
      Param_Node := Make_Ada_Parameter (Param_Name, Param_Type);
      Append_Node_To_List (Param_Node, Param_List);

      --  Define <T>
      --
      --  XXX Note that this incorrect as soon as declarators are complex

      Attr_Type := Get_Mapped_Type (Type_Spec (N));

      --  For all <A> create a getter

      Attribute := First_Node (Declarators (N));
      while Present (Attribute) loop
         Attr_Name  := IDL_Name (Identifier (Attribute));
         Subprogram :=
           Make_Ada_Identifier ("Get_" & Get_Name_String (Attr_Name));
         Append_Node_To_List
           (Make_Subprogram_Spec (Subprogram, Param_List, Attr_Type), L);
         Attribute := Next_Node (Attribute);
      end loop;
   end Attribute_Getter_Function_Spec;

   -------------------------------------
   -- Attribute_Setter_Procedure_Spec --
   -------------------------------------

   procedure Attribute_Setter_Procedure_Spec
     (N : in     Node_Id;
      L : in out List_Id)
   is
      Param_Node : Node_Id;
      Param_List : List_Id := No_List;
      Param_Name : Node_Id;
      Param_Type : Node_Id;
      Subprogram : Node_Id;
      Attribute  : Node_Id;
      Attr_Name   : Name_Id;

   begin
      pragma Assert (Kind (N) = K_Attribute_Declaration);

      --  Expected result :
      --  procedure Set_<A> (Self : in Ref, To : <T>);

      Param_Name := Make_Ada_Identifier ("Self");
      Param_Type := Make_Ada_Identifier ("Ref");
      Param_Node := Make_Ada_Parameter  (Param_Name, Param_Type);
      Append_Node_To_List (Param_Node, Param_List);

      Param_Name := Make_Ada_Identifier ("To");

      --  Define <T>
      --
      --  XXX Note that this incorrect as soon as declarators are complex

      Param_Type := Get_Mapped_Type    (Type_Spec (N));
      Param_Node := Make_Ada_Parameter (Param_Name, Param_Type);
      Append_Node_To_List (Param_Node, Param_List);

      Attribute := First_Node (Declarators (N));
      while Present (Attribute) loop
         Attr_Name  := IDL_Name (Identifier (Attribute));
         Subprogram :=
           Make_Ada_Identifier ("Set_" & Get_Name_String (Attr_Name));
         Append_Node_To_List
           (Make_Subprogram_Spec (Subprogram, Param_List), L);
         Attribute := Next_Node (Attribute);
      end loop;
   end Attribute_Setter_Procedure_Spec;

   ------------------------
   -- Declare_CORBA_Type --
   ------------------------

   procedure Declare_CORBA_Type (K : BEN.Node_Kind; S : String := "") is
      E : Node_Id;
      N : Name_Id;

   begin

      --  Create a fake node located at the beginning of the
      --  specification (current token location).

      E := New_Node (K, No_Location);

      --  Accumulate token names and store node id as table info

      if S'Length = 0 then
         Name_Len := 4;
         Add_Str_To_Name_Buffer (BEN.Node_Kind'Image (K));
         Name_Buffer (1 .. 6) := "CORBA.";
         Capitalize (Name_Buffer (7 .. Name_Len));
      else
         Set_Str_To_Name_Buffer (S);
      end if;
      N := Name_Find;
      Set_Name_Table_Info (N, Int (E));
      BEN.Set_Image (Base_Type (E), N);
   end Declare_CORBA_Type;

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      loop
         case Getopt ("t") is
            when ASCII.NUL =>
               exit;

            when 't' =>
               D_Tree := True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Configure;

   -----------------------
   -- Generate_IDL_File --
   -----------------------

   function Generate_IDL_File (File_Name : Name_Id) return Node_Id is
      pragma Unreferenced (File_Name);
   begin
      --   Write_Name (File_Name);
      return No_Node;
   end Generate_IDL_File;

   ----------------------
   -- Generate_Package --
   ----------------------

   function Generate_Package (E : Node_Id) return Node_Id is
      Pkg_Decl   : Node_Id;
      Pkg_Name   : Node_Id;
      Pkg_Parent : Node_Id;

   begin
      Pkg_Decl   := New_Node (BEN.K_Package_Declaration, No_Location);
      Pkg_Name   := Make_Ada_Identifier (IDL_Name (Identifier (E)));
      Pkg_Parent := Current_Package;
      BEN.Set_Identifier (Pkg_Decl, Pkg_Name);
      BEN.Set_Parent (Pkg_Decl, Pkg_Parent);
      return Pkg_Decl;
   end Generate_Package;

   ---------------
   --  Generate --
   ---------------

   procedure Generate (E : Node_Id) is
      N : Node_Id;
   begin
      Declare_CORBA_Type (BEN.K_Float);
      Declare_CORBA_Type (BEN.K_Double);
      Declare_CORBA_Type (BEN.K_Long_Double);
      Declare_CORBA_Type (BEN.K_Short);
      Declare_CORBA_Type (BEN.K_Long);
      Declare_CORBA_Type (BEN.K_Long_Long);
      Declare_CORBA_Type (BEN.K_Unsigned_Short);
      Declare_CORBA_Type (BEN.K_Unsigned_Long);
      Declare_CORBA_Type (BEN.K_Unsigned_Long_Long);
      Declare_CORBA_Type (BEN.K_Char);
      Declare_CORBA_Type (BEN.K_Wide_Char, "CORBA.WChar");
      Declare_CORBA_Type (BEN.K_String);
      Declare_CORBA_Type (BEN.K_Wide_String);
      Declare_CORBA_Type (BEN.K_Boolean);
      Declare_CORBA_Type (BEN.K_Octet);

      N := Generate_IDL_File (Main_Source);
      pragma Unreferenced (N);
      case Kind (E) is
         when K_Specification =>
            Visit_Specification (E);

         when others =>
            Write_Line ("Others");
      end case;

      if D_Tree then
         W_List_Id (Ada_Packages);
      else
         Generate (Ada_Packages);
      end if;
   end Generate;

   ----------------------
   --  Get_Mapped_Type --
   ----------------------

   function Get_Mapped_Type (E : Node_Id) return Node_Id is
      N : Node_Id := No_Node;

   begin
      case Kind (E) is
         when K_Float .. K_Octet =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            N := Node_Id (Get_Name_Table_Info (Name_Find));

         when K_Scoped_Name =>
            N := BE_Node (Reference (E));
         when K_Enumeration_Type =>
            N := No_Node;
         when others =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            Error_Name (1) := Name_Find;
            DE ("Type Mapping not implemented yet: %");
            N := No_Node;
      end case;
      return N;
   end Get_Mapped_Type;

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      S : String := Node_Kind'Image (N);

   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (3 .. S'Last);
   end Image;

   ---------------------
   -- Insert_Type_Ref --
   ---------------------

   procedure Insert_Type_Ref
     (L : in out List_Id;
      Inheritance_List : List_Id;
      Interface_Is_Abstract : Boolean)
   is
      Type_Ref_Node : Node_Id;
      Type_Ref_Name : Node_Id;
      Ref_Str : constant String := "Ref";
      Abstract_Ref_Str : constant String := "Abstract_Ref";
      Local_Ref_Str   : constant String := "Local_Ref";
      Ancestor_Type_Abstract : constant String := "CORBA.AbstractBase.Ref";
      Ancestor_Type : constant String := "CORBA.Object.Ref";
      Type_Spec_Node : Node_Id;
      pragma Unreferenced (Local_Ref_Str, Type_Ref_Name);

   begin
      Type_Spec_Node := New_Node (BEN.K_Derived_Type_Definition, No_Location);
      BEN.Set_Identifier (Type_Spec_Node,
                          New_Node (BEN.K_Ada_Identifier, No_Location));
      BEN.Set_Record_Extention_Part
        (Type_Spec_Node, New_Node (BEN.K_Record_Type_Spec, No_Location));
      BEN.Set_Is_Null_Record
        (BEN.Record_Extention_Part (Type_Spec_Node), True);

      if (Inheritance_List = No_List)
        or
        (All_Interfaces_Abstract (Inheritance_List))
      then
         if Interface_Is_Abstract then
            Set_Str_To_Name_Buffer (Ancestor_Type_Abstract);
            BEN.Set_Name (BEN.Identifier (Type_Spec_Node), Name_Find);
            Type_Ref_Node :=
              Make_Ada_Typedef_Node (Abstract_Ref_Str, Type_Spec_Node);
         else
            Set_Str_To_Name_Buffer (Ancestor_Type);
            BEN.Set_Name (BEN.Identifier (Type_Spec_Node), Name_Find);
            Type_Ref_Node := Make_Ada_Typedef_Node (Ref_Str, Type_Spec_Node);
            Append_Node_To_List (Type_Ref_Node, L);
         end if;
      else
         null; --   in contruction.
      end if;
   end Insert_Type_Ref;

   ---------------------------
   -- Make_Ada_Typedef_Node --
   ---------------------------

   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Spec : Node_Id) return Node_Id is
      --  Declaration
      Id_Node : Node_Id;
      Type_Def : Node_Id;
   begin
      Id_Node := New_Node (BEN.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer (Identifier_Name);
      BEN.Set_Name (Id_Node, Name_Find);
      Type_Def := New_Node (BEN.K_Type_Declaration, No_Location);
      BEN.Set_Identifier (Type_Def, Id_Node);
      BEN.Set_Type_Spec (Type_Def, Type_Spec);

      return Type_Def;
   end Make_Ada_Typedef_Node;

   -----------
   -- Usage --
   -----------

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Str (Hdr);
      Write_Str ("-t       Dump Ada tree");
      Write_Eol;
   end Usage;

   --------------------------------
   -- Visit_Constant_Declaration --
   --------------------------------

   function Visit_Constant_Declaration (E : Node_Id) return Node_Id is
      pragma Unreferenced (E);
   begin
      return No_Node;
   end Visit_Constant_Declaration;

   ----------------------------
   -- Visit_Enumeration_Type --
   ----------------------------

   function Visit_Enumeration_Type (E : Node_Id) return Node_Id is
      L : constant List_Id
        := Enumerators (E); --  IDL Enumerators List.
      D : Node_Id;
      Enumerator_Node : Node_Id;
      Enumerators_List : List_Id := No_List;
      Type_Spec_Node : Node_Id;
      Type_Identifier : Node_Id;
      Type_Declaration : Node_Id;
   begin
      D := First_Node (L);
      while Present (D) loop
         Enumerator_Node :=
           Make_Ada_Identifier
           (To_Ada_Name (IDL_Name (Identifier (D))));
         Append_Node_To_List
           (Enumerator_Node, Enumerators_List);
         D := Next_Node (D);
      end loop;
      Type_Spec_Node := Make_Enumeration_Type (Enumerators_List);
      Type_Identifier := Make_Ada_Identifier
        (To_Ada_Name (IDL_Name (Identifier (E))));
      Type_Declaration :=
        Make_Type_Declaration (Type_Identifier, Type_Spec_Node);
      Set_BE_Node (E, Type_Declaration);
      return Type_Declaration;
   end Visit_Enumeration_Type;

   ---------------------
   -- Visit_Interface --
   ---------------------

   function Visit_Interface (E : Node_Id) return Node_Id is
      Pkg : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Public_Decl : List_Id;
      I_Spec : List_Id;
      I_Body : List_Id;
      N : Node_Id;
      Ada_Public_Node : Node_Id;
      Ada_Public_List : List_Id;
      Package_With_Node : List_Id;
      pragma Unreferenced (Pkg_Body, Package_With_Node);
   begin
      Pkg := Generate_Package (E);
      --   Package creation
      Pkg_Spec := New_Node (BEN.K_Package_Specification, No_Location);
      I_Spec := Interface_Spec (E);
      Public_Decl := New_List (BEN.K_Declaration_List, No_Location);
      BEN.Set_Visible_Part (Pkg_Spec, Public_Decl);
      I_Body := Interface_Body (E);
      Insert_Type_Ref (Public_Decl, I_Spec, False);
      if I_Body /= No_List then
         N := First_Node (I_Body);
         while Present (N) loop
            case Kind (N) is
               when K_Attribute_Declaration =>
                  Attribute_Getter_Function_Spec (N, Public_Decl);
                  if not Is_Readonly (N) then
                     Attribute_Setter_Procedure_Spec (N, Public_Decl);
                  end if;
               when K_Operation_Declaration =>
                  Ada_Public_Node := Visit_Operation_Declaration (N);
                  Append_Node_To_List (Ada_Public_Node, Public_Decl);
               when K_Type_Declaration =>
                  Ada_Public_List := Visit_Type_Declaration (N);
                  Append_Node_To_List
                    (BEN.First_Node (Ada_Public_List), Public_Decl);
               when K_Enumeration_Type =>
                  Ada_Public_Node := Visit_Enumeration_Type (N);
                  Append_Node_To_List (Ada_Public_Node, Public_Decl);
               when K_Constant_Declaration =>
                  Ada_Public_Node := Visit_Constant_Declaration (N);
                  Append_Node_To_List (Ada_Public_Node, Public_Decl);
               when others =>
                  Set_Str_To_Name_Buffer (Image (Kind (N)));
                  Error_Name (1) := Name_Find;
                  DE ("Visit_Interface : Pas encore implemente! %");
            end case;
            N := Next_Node (N);
         end loop;
      end if;
      BEN.Set_Package_Specification (Pkg, Pkg_Spec);
      return Pkg;
   end Visit_Interface;

   ---------------------------------
   -- Visit_Operation_Declaration --
   ---------------------------------

   function Visit_Operation_Declaration (E : Node_Id) return Node_Id is
      Operation_Identifier : Node_Id;
      Return_Type_Node : Node_Id;
      Ada_Argument_List : List_Id := No_List;
      Ada_Argument : Node_Id := No_Node;
      Param_Id : Node_Id;
      Param_Type : Node_Id;
      Param_Mode : Mode_Id;
      Declarator_Node : Node_Id;
      L : List_Id;
      N : Node_Id;
   begin
      Operation_Identifier :=
        Make_Ada_Identifier (IDL_Name (Identifier (E)));
      Return_Type_Node := Get_Mapped_Type (Type_Spec (E));
      L := Parameters (E);
      --   Argument Identifier
      Param_Id := Make_Ada_Identifier ("Self");
      --   Argument Type
      Param_Type := Make_Ada_Identifier ("Ref");
      --   Argument Mode
      Param_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Ada_Argument := Make_Ada_Parameter (Param_Id, Param_Type, Param_Mode);
      --   Argument List
      Append_Node_To_List (Ada_Argument, Ada_Argument_List);
      if L /= No_List then
         N := First_Node (L);
         while Present (N) loop
            Declarator_Node := Identifier (Declarator (N));
            Param_Id :=
              Make_Ada_Identifier (IDL_Name  (Declarator_Node));
            Param_Type := Get_Mapped_Type (Type_Spec (N));
            Param_Mode := Parameter_Mode (N);
            Ada_Argument :=
              Make_Ada_Parameter (Param_Id, Param_Type, Param_Mode);
            Append_Node_To_List (Ada_Argument, Ada_Argument_List);
            N := Next_Node (N);
            if Is_Oneway (E) and
              Param_Mode /= Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In))
            then
               raise Fatal_Error; --  Validity check of the IDL Tree.
            end if;
         end loop;
      end if;
      if Return_Type_Node = No_Node then
         if Kind (Type_Spec (E)) = K_Void then
            return Make_Subprogram_Spec
              (Operation_Identifier, Ada_Argument_List);
         else
            raise Fatal_Error; -- Type mapping Error;
         end if;
      else
         return Make_Subprogram_Spec
           (Operation_Identifier, Ada_Argument_List, Return_Type_Node);
      end if;

   end Visit_Operation_Declaration;

   ------------------
   -- Visit_Module --
   ------------------

   function Visit_Module (E : Node_Id) return Node_Id is
      Package_Node : Node_Id;
   begin
      Package_Node := Generate_Package (E);
      return Package_Node;
   end Visit_Module;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      D : Node_Id;
      N : Node_Id;
   begin

      Ada_Packages := New_List (BEN.K_Declaration_List, No_Location);
      D := First_Node (Definitions (E));
      while Present (D) loop
         case Kind (D) is
            when K_Module =>
               N := Visit_Module (D);
               Append_Node_To_List (N, Ada_Packages);
               Push_Package (N);
               Visit_Specification (D); -- Visit  definitions of the module
               Pop_Package;

            when K_Type_Declaration =>
               --    Ada_List := Visit_Type_Declaration (D);
               null;

            when K_Interface_Declaration =>
               N := Visit_Interface (D);
               Append_Node_To_List (N, Ada_Packages);

            when others =>
               Display_Error ("Definition not recongnized");

         end case;
         D := Next_Node (D);
      end loop;
   end Visit_Specification;

   ----------------------------
   -- Visit_Type_Declaration --
   ----------------------------

   function Visit_Type_Declaration (E : Node_Id) return List_Id
   is
      Type_Spec_Node : Node_Id;
      Declarators_List : List_Id;
      D : Node_Id;
      Result_List : List_Id := No_List;
      Ada_Type_Declaration : Node_Id;

   begin
      Type_Spec_Node := Get_Mapped_Type (Type_Spec (E));
      Declarators_List := Declarators (E);
      D := First_Node (Declarators_List);
      while Present (D) loop
         Ada_Type_Declaration :=
           Make_Derived_Type_Declaration
           (Make_Ada_Identifier
            (IDL_Name (Identifier (D))), Type_Spec_Node);
         Set_BE_Node (D, Ada_Type_Declaration);
         --   Link Idl node  with Ada node.
         Append_Node_To_List
           (Ada_Type_Declaration, Result_List);
         D := Next_Node (D);
      end loop;
      return Result_List;
   end Visit_Type_Declaration;

end Backend.BE_Ada;
