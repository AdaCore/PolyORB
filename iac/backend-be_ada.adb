with Types; use Types;
with Nodes; use Nodes;

pragma Warnings (Off);
with Backend.BE_Ada.Nodes;

pragma Warnings (On);


with Output; use Output;
with Errors; use Errors;
with Locations; use Locations;
with Namet; use Namet;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
--   with Backend.BE_Ada.Debug; use Backend.BE_Ada.Debug;
with Backend.BE_Ada.FILES_Generation; use Backend.BE_Ada.FILES_Generation;
--   with Backend.BE_Ada.Namet;
with Lexer;
with Charset; use Charset;
package body Backend.BE_Ada is

   Ada_Packages : List_Id;

   package BE renames Backend.BE_Ada.Nodes;


   procedure Visite_Type_Declaration (E : Node_Id);
   procedure Visite_Specification (E : Node_Id);
   function G_Package (E : Node_Id) return Node_Id;
   function Visit_Interface (E : Node_Id)return Node_Id;
   function Visite_Module (E : Node_Id) return Node_Id;
   function All_Inheritance_Interface_Abstract (L : List_Id) return Boolean;
   procedure Insert_Type_Ref (L : in out List_Id; Inheritance_List : List_Id;
                                           Interface_Is_Abstract : Boolean);
   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Spec : Node_Id) return Node_Id;
   procedure Insert_Base_Type;
   procedure Declare_Base_Type (Type_Str : String; K : BE.Node_Kind);
   function Attribute_Getter_Function_Spec (N : Node_Id) return Node_Id;
   function Get_Mapped_Type (E : Node_Id) return Node_Id;
   function Image (N : Node_Kind) return String;
   function Attribute_Setter_Procedure_Spec (N : Node_Id) return Node_Id;


   --------------
   --  Generate --
   --------------

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

      Generate (Ada_Packages);
   end Generate;


   procedure Visite_Type_Declaration (E : Node_Id)  is
      pragma Unreferenced (E);
   begin
      --   Write_Line ("Type Definition");
      null;
   end Visite_Type_Declaration;


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
               Visite_Type_Declaration (D);

            when K_Interface_Declaration =>
               Ada_Node := Visit_Interface (D);
               Append_Node_To_List (Ada_Node, Ada_Packages);
            when others =>
               Display_Error ("Definition not recongnized");

         end case;
         D := Next_Node (D);
      end loop;

   end Visite_Specification;

   -----------------------
   --  Gererate Package  --
   -----------------------



   function G_Package (E : Node_Id) return Node_Id is

      Node : Node_Id;
      Id : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Ada_Name : Name_Id;
      Parent_Node : Node_Id;

   begin
      Node := New_Node (BE.K_Ada_Packages, No_Location);
      Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Ada_Name := Map_Identifier_Name_2Ada (Name (Identifier (E)));
      BE.Set_Name (Id, Ada_Name);
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


   -------------------
   -- Current_Scope --
   -------------------




   function Visit_Interface (E : Node_Id) return Node_Id is
      Pkg : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Public_Decl : List_Id;
      I_Spec : List_Id;
      I_Body : List_Id;
      N : Node_Id;
      Public_Declaration_Node : Node_Id;
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
                  Public_Declaration_Node :=
                    Attribute_Getter_Function_Spec (N);
                  Append_Node_To_List (Public_Declaration_Node, Public_Decl);
                  if not Is_Readonly (N) then
                     Public_Declaration_Node :=
                       Attribute_Setter_Procedure_Spec (N);
                     Append_Node_To_List
                       (Public_Declaration_Node, Public_Decl);
                  end if;
               when others =>
                  DE ("Visit_Interface : Fonctionnalite pas encore imp!");
            end case;
            N := Next_Node (N);
         end loop;
      end if;


      --   Type reference insertion
      BE.Set_Package_Spec (Pkg, Pkg_Spec);



      return Pkg;
   end Visit_Interface;


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
      Declare_Base_Type ("CORBA.Octet", BE.K_Octet);
      Declare_Base_Type ("CORBA.Boolean", BE.K_Boolean);
      Declare_Base_Type ("CORBA.String", BE.K_String);

   end Insert_Base_Type;

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

   ----------------------------------
   --   Attribute Getter Function : --
   ----------------------------------
   --   N : (in)  Interface Attribute
   --   Return : ADA Getter Function Specification Node
   ----------------------------------------
   function Attribute_Getter_Function_Spec (N : Node_Id) return Node_Id is
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

   begin

      --   Argument Identifier
      Arg_Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Self");
      BE.Set_Name (Arg_Id, Name_Find);
      --   Argument Type
      Arg_Type := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Ref");
      BE.Set_Name (Arg_Type, Name_Find);
      --   Argument Mode

      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);
      --   Function Identifier
      Function_Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Get_" & Get_Name_String
                              (Name (Identifier
                                     (First_Node (Declarators (N))))));
      BE.Set_Name (Function_Id, Name_Find);
      --   Return Type
      Return_Type := Get_Mapped_Type (Type_Spec (N));
      return Mk_Node_Ada_Function_Spec
        (Function_Id, Argument_List, Return_Type);
   end Attribute_Getter_Function_Spec;

   -----------------------------------------
   --   Attribute Setter Function Spec: --
   ----------------------------------
   --   N : (in)  Interface Attribute
   --   Return : ADA Setter Procedure Specification Node
   ----------------------------------------
   function Attribute_Setter_Procedure_Spec (N : Node_Id) return Node_Id is
      --   Need Assert to controle N kind
      --   N must be of K_Ada_Attribute kind;
      Argument_Node : Node_Id;
      Argument_List : List_Id := No_List;
      Arg_Id : Node_Id;
      --   Id_Name : Name_Id;
      Arg_Type : Node_Id;
      Arg_Mode : Mode_Id;
      Procedure_Id : Node_Id;
   begin

      --   Argument Identifier
      Arg_Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Self");
      BE.Set_Name (Arg_Id, Name_Find);
      --   Argument Type
      Arg_Type := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Ref");
      BE.Set_Name (Arg_Type, Name_Find);
      --   Argument Mode

      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);
      --   Argument Identifier
      Arg_Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("To");
      BE.Set_Name (Arg_Id, Name_Find);
      --   Argument Type
      Arg_Type := Get_Mapped_Type (Type_Spec (N));
      --   Argument Mode

      Arg_Mode := Mode_Id (Lexer.Token_Type'Pos (Lexer.T_In));
      Argument_Node := Mk_Node_Ada_Argument (Arg_Id, Arg_Type, Arg_Mode);
      --   Argument List
      Append_Node_To_List (Argument_Node, Argument_List);

      --   Function Identifier
      Procedure_Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Set_Str_To_Name_Buffer ("Set_" & Get_Name_String
                              (Name (Identifier
                                     (First_Node (Declarators (N))))));
      BE.Set_Name (Procedure_Id, Name_Find);

      return Mk_Node_Ada_Procedure_Spec
        (Procedure_Id, Argument_List);
   end Attribute_Setter_Procedure_Spec;


   function Get_Mapped_Type (E : Node_Id) return Node_Id is
      Node : Node_Id := No_Node;
   begin

      case Kind (E) is
         when K_Float .. K_Octet =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            Node := Node_Id (Get_Name_Table_Info (Name_Find));
         when others =>
            DE ("Type Mapping not implemented yet");
      end case;
      return Node;
   end Get_Mapped_Type;


end Backend.BE_Ada;


