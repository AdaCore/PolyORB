with GNAT.Command_Line; use GNAT.Command_Line;
with Types; use Types;
with Nodes; use Nodes;
with Backend.BE_Ada.Nodes;
with Output; use Output;
with Errors; use Errors;
with Locations; use Locations;
with Namet; use Namet;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Debug; use Backend.BE_Ada.Debug;
with Backend.BE_Ada.FILES_Generation; use Backend.BE_Ada.FILES_Generation;
with Lexer;
with Charset; use Charset;


package body Backend.BE_Ada is

   Ada_Packages : List_Id;
   D_Tree   : Boolean := False;

   package BE renames Backend.BE_Ada.Nodes;

   function All_Inheritance_Interface_Abstract (L : List_Id) return Boolean;
   procedure Attribute_Getter_Function_Spec (N : Node_Id; L : in out List_Id);
   procedure Attribute_Setter_Procedure_Spec (N : Node_Id; L : in out List_Id);
   procedure Declare_Base_Type (Type_Str : String; K : BE.Node_Kind);
   function G_Package (E : Node_Id) return Node_Id;
   function Get_Mapped_Type (E : Node_Id) return Node_Id;
   procedure Insert_Base_Type;
   procedure Insert_Type_Ref (L : in out List_Id; Inheritance_List : List_Id;
                                           Interface_Is_Abstract : Boolean);
   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Spec : Node_Id) return Node_Id;
   procedure Visite_Specification (E : Node_Id);

   function Visit_Interface (E : Node_Id)return Node_Id;
   function Visite_Module (E : Node_Id) return Node_Id;
   function Visite_Operation_Declaration (E : Node_Id) return Node_Id;
   function Visite_Type_Declaration (E : Node_Id) return List_Id;

   function Image (N : Node_Kind) return String;


   function All_Inheritance_Interface_Abstract (L : List_Id) return Boolean is
      N : Node_Id;
      All_Abstract : Boolean := True;
   begin
      if L /= No_List then
         N := First_Node (L);
         while Present (N)  loop
            if Is_Abstract (N) then
               All_Abstract := False;
               exit;
            end if;
            N := Next_Node (N);
         end loop;
      end if;
      return All_Abstract;

   end All_Inheritance_Interface_Abstract;

   ----------------------------------
   --   Attribute Getter Function : --
   ----------------------------------
   --   N : (in)  Interface Attribute
   --   Return : ADA Getter Function Specification Node
   ----------------------------------------
   procedure Attribute_Getter_Function_Spec
     (N : Node_Id; L : in out List_Id) is
      --   Need Assert to controle N kind
      --   N must be of K_Ada_Attribute kind;
      Argument_Node : Node_Id;
      Argument_List : List_Id := No_List;
      Arg_Id : Node_Id;
      --   Id_Name : Name_Id;
      Arg_Type : Node_Id;
      Arg_Mode : Mode_Id;
      Function_Id : Node_Id;
      Return_Type : Node_Id;
      Declarator : Node_Id;
      Declarators_List : List_Id;

   begin

      --   Argument Identifier
      Arg_Id := Mk_Node_Ada_Identifier ("Self");
      --   Argument Type
      Arg_Type := Mk_Node_Ada_Identifier ("Ref");
      --   Argument Mode
      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);

      --   Return Type
      Return_Type := Get_Mapped_Type (Type_Spec (N));
      --   Generate getter function for each declarator
      Declarators_List := Declarators (N);
      Declarator := First_Node (Declarators_List);
      while Present (Declarator) loop

         Function_Id :=
           Mk_Node_Ada_Identifier
           ("get_" & Get_Name_String (IDL_Name (Identifier (Declarator))));
         Append_Node_To_List
           (Mk_Node_Ada_Function_Spec
            (Function_Id, Argument_List, Return_Type), L);
         Declarator := Next_Node (Declarator);
      end loop;
   end Attribute_Getter_Function_Spec;

   -----------------------------------------
   --   Attribute Setter Function Spec: --
   ----------------------------------
   --   N : (in)  Interface Attribute
   --   Return : ADA Setter Procedure Specification Node
   ----------------------------------------
   procedure Attribute_Setter_Procedure_Spec
     (N : Node_Id; L : in out List_Id)  is
      --   Need Assert to controle N kind
      --   N must be of K_Ada_Attribute kind;
      Argument_Node : Node_Id;
      Argument_List : List_Id := No_List;
      Arg_Id : Node_Id;
      --   Id_Name : Name_Id;
      Arg_Type : Node_Id;
      Arg_Mode : Mode_Id;
      Procedure_Id : Node_Id;
      Declarator : Node_Id;
      Declarators_List : List_Id;
   begin
      --   Argument Identifier
      Arg_Id := Mk_Node_Ada_Identifier ("Self");
      --   Argument Type
      Arg_Type := Mk_Node_Ada_Identifier ("Ref");
      --   Argument Mode
      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);
      --   Argument Identifier
      Arg_Id := Mk_Node_Ada_Identifier ("To");
      --   Argument Type
      Arg_Type := Get_Mapped_Type (Type_Spec (N));
      --   Argument Mode
      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);

      --   Procedure Identifier
      Declarators_List := Declarators (N);
      Declarator := First_Node (Declarators_List);
      while Present (Declarator) loop

         Procedure_Id :=
           Mk_Node_Ada_Identifier
           ("set_" & Get_Name_String (IDL_Name (Identifier (Declarator))));
         Append_Node_To_List
           (Mk_Node_Ada_Procedure_Spec
            (Procedure_Id, Argument_List), L);
         Declarator := Next_Node (Declarator);
      end loop;
   end Attribute_Setter_Procedure_Spec;


   procedure Declare_Base_Type (Type_Str : String; K : BE.Node_Kind) is
      E : Node_Id;
      N : Name_Id;
   begin

      --  Create a fake node located at the beginning of the
      --  specification (current token location).

      E := New_Node (K, No_Location);

      --  Accumulate token names and store node id as table info

      Set_Str_To_Name_Buffer (Type_Str);
      N := Name_Find;
      Set_Name_Table_Info (N, Int (E));
      BE.Set_Image (Base_Type (E), N);
   end Declare_Base_Type;



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
   --  Gererate Package  --
   -----------------------

   function G_Package (E : Node_Id) return Node_Id is

      Node : Node_Id;
      Id : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Parent_Node : Node_Id;

   begin
      Node := New_Node (BE.K_Ada_Packages, No_Location);
      Id := Mk_Node_Ada_Identifier (IDL_Name (Identifier (E)));
      BE.Set_Identifier (Node, Id);
      Parent_Node := Current_Package;
      BE.Set_Parent (Node, Parent_Node);
      case Kind (E) is
         when K_Module =>
            Pkg_Spec := No_Node;
            BE.Set_Package_Spec (Node, Pkg_Spec);
            Pkg_Body := No_Node;
            BE.Set_Package_Body (Node, Pkg_Body);
         when K_Interface_Declaration =>
            null;
         when others =>
            Write_Line ("In progress....G_Package");
      end case;
      return Node;
   end G_Package;


   ---------------
   --  Generate --
   ---------------
   procedure Generate (E : Node_Id) is

   begin
      --    BE_Namet.Initialize;
      Insert_Base_Type;
      Set_Standard_Output;
      Ada_Packages := New_List (BE.K_Ada_Package_List, No_Location);
      case Kind (E) is
         when K_Specification =>
            Visite_Specification (E);
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
      Node : Node_Id := No_Node;
   begin

      case Kind (E) is
         when K_Float .. K_Octet =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            Node := Node_Id (Get_Name_Table_Info (Name_Find));

         when K_Scoped_Name =>
            Node := Ada_Node (Reference (E));
         when others =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            Error_Name (1) := Name_Find;
            DE ("Type Mapping not implemented yet: %");
      end case;
      return Node;
   end Get_Mapped_Type;



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


   procedure Insert_Base_Type is
   begin
      Declare_Base_Type ("CORBA.Float", BE.K_Float);
      Declare_Base_Type ("CORBA.Double", BE.K_Double);
      Declare_Base_Type ("CORBA.Long_Double", BE.K_Long_Double);
      Declare_Base_Type ("CORBA.Short", BE.K_Short);
      Declare_Base_Type ("CORBA.Long", BE.K_Long);
      Declare_Base_Type ("CORBA.Long_Long", BE.K_Long_Long);
      Declare_Base_Type ("CORBA.Unsigned_Short", BE.K_Unsigned_Short);
      Declare_Base_Type ("CORBA.Unsigned_Long", BE.K_Unsigned_Long);
      Declare_Base_Type ("CORBA.Unsigned_Long_Long", BE.K_Unsigned_Long_Long);
      Declare_Base_Type ("CORBA.Char", BE.K_Char);
      Declare_Base_Type ("CORBA.WChar", BE.K_Wide_Char);
      Declare_Base_Type ("CORBA.String", BE.K_String);
      Declare_Base_Type ("CORBA.Wide_String", BE.K_Wide_String);
      Declare_Base_Type ("CORBA.Boolean", BE.K_Boolean);
      Declare_Base_Type ("CORBA.Octet", BE.K_Octet);


   end Insert_Base_Type;


   ------------------------------
   --   Insert Type Reference ---
   ------------------------------
   procedure Insert_Type_Ref
     (L : in out List_Id;
      Inheritance_List : List_Id;
      Interface_Is_Abstract : Boolean) is


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
      Type_Spec_Node := New_Node (BE.K_Derived_Type_Definition, No_Location);
      BE.Set_Identifier (Type_Spec_Node,
                         New_Node (BE.K_Ada_Identifier, No_Location));
      BE.Set_Record_Extention_Part
        (Type_Spec_Node, New_Node (BE.K_Record_Type_Spec, No_Location));
      BE.Set_Is_Null_Record
        (BE.Record_Extention_Part (Type_Spec_Node), True);

      if (Inheritance_List = No_List)
        or
        (All_Inheritance_Interface_Abstract (Inheritance_List))
      then
         if Interface_Is_Abstract then
            Set_Str_To_Name_Buffer (Ancestor_Type_Abstract);
            BE.Set_Name (BE.Identifier (Type_Spec_Node), Name_Find);
            Type_Ref_Node :=
              Make_Ada_Typedef_Node (Abstract_Ref_Str, Type_Spec_Node);
         else
            Set_Str_To_Name_Buffer (Ancestor_Type);
            BE.Set_Name (BE.Identifier (Type_Spec_Node), Name_Find);
            Type_Ref_Node := Make_Ada_Typedef_Node (Ref_Str, Type_Spec_Node);
            Append_Node_To_List (Type_Ref_Node, L);
         end if;
      else
         null; --   in contruction.
      end if;
   end Insert_Type_Ref;


   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Spec : Node_Id) return Node_Id is
      --  Declaration
      Id_Node : Node_Id;
      Type_Def : Node_Id;
   begin
      Id_Node := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer (Identifier_Name);
      BE.Set_Name (Id_Node, Name_Find);
      Type_Def := New_Node (BE.K_Type_Declaration, No_Location);
      BE.Set_Identifier (Type_Def, Id_Node);
      BE.Set_Type_Spec (Type_Def, Type_Spec);

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
      Pkg := G_Package (E);
      --   Package creation
      Pkg_Spec := New_Node (BE.K_Ada_Package_Spec, No_Location);
      I_Spec := Interface_Spec (E);
      Public_Decl := New_List (BE.K_List_Id, No_Location);
      BE.Set_Ada_Public (Pkg_Spec, Public_Decl);
      --   Package_With_Node := New_List (BE.K_Ada_With_List, No_Location);
      --   BE.Set_Package_With (Pkg_Spec, Package_With_Node);
      Set_Str_To_Name_Buffer ("--   None ");
      BE.Set_Prologue (Pkg_Spec, Name_Find);
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
                  Ada_Public_Node := Visite_Operation_Declaration (N);
                  Append_Node_To_List (Ada_Public_Node, Public_Decl);
               when K_Type_Declaration =>
                  Ada_Public_List := Visite_Type_Declaration (N);
                  Append_List_To_List (Ada_Public_List, Public_Decl);
               when others =>
                  Set_Str_To_Name_Buffer (Image (Kind (N)));
                  Error_Name (1) := Name_Find;
                  DE ("Visit_Interface : Pas encore implemente! %");
            end case;
            N := Next_Node (N);
         end loop;
      end if;
      BE.Set_Package_Spec (Pkg, Pkg_Spec);
      return Pkg;
   end Visit_Interface;


   ------------------------------------
   --   Visite_Operation_Declaration --
   ------------------------------------
   function Visite_Operation_Declaration (E : Node_Id) return Node_Id is
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
        Mk_Node_Ada_Identifier (IDL_Name (Identifier (E)));
      Return_Type_Node := Get_Mapped_Type (Type_Spec (E));
      L := Parameters (E);
      --   Argument Identifier
      Param_Id := Mk_Node_Ada_Identifier ("Self");
      --   Argument Type
      Param_Type := Mk_Node_Ada_Identifier ("Ref");
      --   Argument Mode
      Param_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Ada_Argument := Mk_Node_Ada_Argument (Param_Id, Param_Type, Param_Mode);
      --   Argument List
      Append_Node_To_List (Ada_Argument, Ada_Argument_List);
      if L /= No_List then
         N := First_Node (L);
         while Present (N) loop
            Declarator_Node := Identifier (Declarator (N));
            Param_Id :=
              Mk_Node_Ada_Identifier (IDL_Name  (Declarator_Node));
            Param_Type := Get_Mapped_Type (Type_Spec (N));
            Param_Mode := Parameter_Mode (N);
            Ada_Argument :=
              Mk_Node_Ada_Argument (Param_Id, Param_Type, Param_Mode);
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
            return Mk_Node_Ada_Procedure_Spec
              (Operation_Identifier, Ada_Argument_List);
         else
            raise Fatal_Error; -- Type mapping Error;
         end if;
      else
         return Mk_Node_Ada_Function_Spec
           (Operation_Identifier, Ada_Argument_List, Return_Type_Node);
      end if;

   end Visite_Operation_Declaration;


   ----------------------
   --   Visite_Module  --
   ----------------------
   function Visite_Module (E : Node_Id) return Node_Id is
      Package_Node : Node_Id;
   begin
      Package_Node := G_Package (E);
      return Package_Node;
   end Visite_Module;


   -------------------------------
   -- Visite Specification Node --
   -------------------------------
   procedure Visite_Specification (E : Node_Id) is
      List_Def : List_Id;
      D      : Node_Id;
      Ada_Node : Node_Id;
      Ada_List : List_Id;
      pragma Unreferenced (Ada_List);
   begin
      List_Def := Definitions (E);
      D := First_Node (List_Def);
      while Present (D) loop
         case Kind (D) is

            when K_Module =>
               Ada_Node := Visite_Module (D);
               Append_Node_To_List (Ada_Node, Ada_Packages);
               Push_Package (Ada_Node);
               Visite_Specification (D); -- Visit  definitions of the module
               Pop_Package;

            when K_Type_Declaration =>
               --    Ada_List := Visite_Type_Declaration (D);
               null;
            when K_Interface_Declaration =>
               Ada_Node := Visit_Interface (D);
               Append_Node_To_List (Ada_Node, Ada_Packages);
            when others =>
               Display_Error ("Definition not recongnized");

         end case;
         D := Next_Node (D);
      end loop;
   end Visite_Specification;

   -------------------------------
   --   Visite_Type_Declaration --
   -------------------------------
   function Visite_Type_Declaration (E : Node_Id) return List_Id is
      Type_Spec_Node : Node_Id;
      Declarators_List : List_Id;  --    := Declarators (E);
      D : Node_Id;
      Result_List : List_Id := No_List; -- Ada Type Definition List
      Ada_Type_Declaration : Node_Id;
   begin

      Type_Spec_Node := Get_Mapped_Type (Type_Spec (E));
      Declarators_List := Declarators (E);
      D := First_Node (Declarators_List);
      while Present (D) loop
         Ada_Type_Declaration :=
           Mk_Node_Simple_Derived_Type_Def
           (Mk_Node_Ada_Identifier
            (IDL_Name (Identifier (D))), Type_Spec_Node);
         Set_Ada_Node (D, Ada_Type_Declaration);
         --   Link Idl node  with Ada node.
         Append_Node_To_List
           (Ada_Type_Declaration, Result_List);
         D := Next_Node (D);
      end loop;
      return Result_List;
   end Visite_Type_Declaration;

end Backend.BE_Ada;


