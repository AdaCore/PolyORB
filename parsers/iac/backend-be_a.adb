with GNAT.Command_Line; use GNAT.Command_Line;

with Namet;     use Namet;
with Output;    use Output;
with Types;     use Types;

with Frontend.Nodes;           use Frontend.Nodes;

with Backend.BE_A.Debug;     use Backend.BE_A.Debug;
with Backend.BE_A.Generator; use Backend.BE_A.Generator;
with Backend.BE_A.Nodes;     use Backend.BE_A.Nodes;
with Backend.BE_A.Nutils;    use Backend.BE_A.Nutils;

package body Backend.BE_A is

   D_Tree   : Boolean := False;

   Interface_Ref          : Name_Id;
   Object_Ref             : Name_Id;

   Abstract_Interface_Ref : Name_Id;
   pragma Unreferenced (Abstract_Interface_Ref);
   Local_Interface_Ref    : Name_Id;
   pragma Unreferenced (Local_Interface_Ref);
   Abstract_Base_Ref      : Name_Id;
   pragma Unreferenced (Abstract_Base_Ref);

   Self_Parameter_Name    : Name_Id;
   To_Parameter_Name      : Name_Id;

   package FEN renames Frontend.Nodes;

   procedure Visit (E : Node_Id);
   procedure Visit_Attribute_Declaration (E : Node_Id);
   procedure Visit_Constant_Declaration (E : Node_Id);
   procedure Visit_Enumeration_Type (E : Node_Id);
   procedure Visit_Interface_Declaration (E : Node_Id);
   procedure Visit_Module (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);
   procedure Visit_Type_Declaration (E : Node_Id);

   function Make_IDL_Unit (E : Node_Id) return Node_Id;
   function Make_Package_Declaration (N : Name_Id) return Node_Id;

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

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
   begin
      Declare_CORBA_Type (FEN.K_Float);
      Declare_CORBA_Type (FEN.K_Double);
      Declare_CORBA_Type (FEN.K_Long_Double);
      Declare_CORBA_Type (FEN.K_Short);
      Declare_CORBA_Type (FEN.K_Long);
      Declare_CORBA_Type (FEN.K_Long_Long);
      Declare_CORBA_Type (FEN.K_Unsigned_Short);
      Declare_CORBA_Type (FEN.K_Unsigned_Long);
      Declare_CORBA_Type (FEN.K_Unsigned_Long_Long);
      Declare_CORBA_Type (FEN.K_Char);
      Declare_CORBA_Type (FEN.K_Wide_Char, "WChar");
      Declare_CORBA_Type (FEN.K_String);
      Declare_CORBA_Type (FEN.K_Wide_String);
      Declare_CORBA_Type (FEN.K_Boolean);
      Declare_CORBA_Type (FEN.K_Octet);

      Set_Str_To_Name_Buffer ("Ref");
      Interface_Ref := Name_Find;
      Set_Str_To_Name_Buffer ("Abstract_Ref");
      Abstract_Interface_Ref := Name_Find;
      Set_Str_To_Name_Buffer ("Local_Ref");
      Local_Interface_Ref := Name_Find;
      Set_Str_To_Name_Buffer ("CORBA.AbstractBase.Ref");
      Abstract_Base_Ref := Name_Find;
      Set_Str_To_Name_Buffer ("CORBA.Object.Ref");
      Object_Ref := Name_Find;
      Set_Str_To_Name_Buffer ("Self");
      Self_Parameter_Name := Name_Find;
      Set_Str_To_Name_Buffer ("To");
      To_Parameter_Name := Name_Find;

      Visit_Specification (E);
      if D_Tree then
         W_Node_Id (BE_Node (E));
      end if;
      Generator.Generate (BE_Node (E));
   end Generate;

   -------------------
   -- Make_IDL_Unit --
   -------------------

   function Make_IDL_Unit (E : Node_Id) return Node_Id is
      P : Node_Id;
      N : Name_Id;
      D : Node_Id;
      L : List_Id;

   begin
      P := New_Node (K_IDL_Unit, E);

      L := New_List (K_Packages);
      Set_Packages (P, L);

      N := IDL_Name (Identifier (E));
      if Kind (E) = K_Specification then
         Get_Name_String (N);
         Add_Str_To_Name_Buffer ("_IDL_File");
         N := Name_Find;
      end if;

      --  Main package

      D := Make_Package_Declaration (N);
      Set_Main_Package (P, D);
      Append_Node_To_List (D, L);

      --  Helper package

      Get_Name_String (N);
      Add_Str_To_Name_Buffer (".Helper");
      D := Make_Package_Declaration (Name_Find);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (E) = K_Interface_Declaration then

         --  Skeleton package

         Get_Name_String (N);
         Add_Str_To_Name_Buffer (".Skel");
         D := Make_Package_Declaration (Name_Find);
         Set_Skeleton_Package (P, D);
         Append_Node_To_List (D, L);

         --  Implementation package

         Get_Name_String (N);
         Add_Str_To_Name_Buffer (".Impl");
         D := Make_Package_Declaration (Name_Find);
         Set_Implementation_Package (P, D);
         Append_Node_To_List (D, L);
      end if;

      return P;
   end Make_IDL_Unit;

   ------------------------------
   -- Make_Package_Declaration --
   ------------------------------

   function Make_Package_Declaration (N : Name_Id) return Node_Id is
      D : Node_Id;
      P : Node_Id;

   begin
      D := New_Node (K_Package_Declaration);
      Set_Defining_Identifier (D, Make_Defining_Identifier (N));
      P := New_Node (K_Package_Specification);
      Set_Withed_Packages (P, New_List (K_Withed_Packages));
      Set_Visible_Part (P, New_List (K_Declaration_List));
      Set_Private_Part (P, New_List (K_Declaration_List));
      Set_Package_Declaration (P, D);
      Set_Package_Specification (D, P);
      P := New_Node (K_Package_Implementation);
      Set_Withed_Packages (P, New_List (K_Withed_Packages));
      Set_Declarations (P, New_List (K_Declaration_List));
      Set_Statements (P, New_List (K_Statement_List));
      Set_Package_Declaration (P, D);
      Set_Package_Implementation (D, P);
      return D;
   end Make_Package_Declaration;

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

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case FEN.Kind (E) is
         when K_Specification =>
            Visit_Specification (E);

         when K_Constant_Declaration =>
            Visit_Constant_Declaration (E);

         when K_Enumeration_Type =>
            Visit_Enumeration_Type (E);

         when K_Interface_Declaration =>
            Visit_Interface_Declaration (E);

         when K_Attribute_Declaration =>
            Visit_Attribute_Declaration (E);

         when K_Type_Declaration =>
            Visit_Type_Declaration (E);

         when K_Module =>
            Visit_Module (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Attribute_Declaration --
   ---------------------------------

   procedure Visit_Attribute_Declaration (E : Node_Id) is
      N : Node_Id;
      T : Node_Id;
      L : List_Id;
      P : Node_Id;
      A : Node_Id;
      X : Name_Id;

   begin
      Set_Main_Spec;

      A := First_Entity (Declarators (E));
      while Present (A) loop
         X := To_Ada_Name (FEN.IDL_Name (Identifier (A)));
         Set_Str_To_Name_Buffer ("Get_");
         Get_Name_String_And_Append (X);
         N := Make_Defining_Identifier (Name_Find);
         L := New_List (K_Parameter_Profile);
         P := Make_Parameter_Specification
           (Make_Defining_Identifier (Self_Parameter_Name),
            Make_Defining_Identifier (Interface_Ref));
         Append_Node_To_List (P, L);
         T := Make_Defining_Identifier (Type_Spec (E));
         N := Make_Subprogram_Specification (N, L, T);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         if not Is_Readonly (E) then
            Set_Str_To_Name_Buffer ("Set_");
            Get_Name_String_And_Append (X);
            N := Make_Defining_Identifier (Name_Find);
            L := New_List (K_Parameter_Profile);
            P := Make_Parameter_Specification
              (Make_Defining_Identifier (Self_Parameter_Name),
               Make_Defining_Identifier (Interface_Ref));
            Append_Node_To_List (P, L);
            P := Make_Parameter_Specification
              (Make_Defining_Identifier (To_Parameter_Name),
               Make_Defining_Identifier (Type_Spec (E)));
            Append_Node_To_List (P, L);
            N := Make_Subprogram_Specification (N, L);
            Append_Node_To_List (N, Visible_Part (Current_Package));
         end if;

         A := Next_Entity (A);
      end loop;
   end Visit_Attribute_Declaration;

   --------------------------------
   -- Visit_Constant_Declaration --
   --------------------------------

   procedure Visit_Constant_Declaration (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Constant_Declaration;

   ----------------------------
   -- Visit_Enumeration_Type --
   ----------------------------

   procedure Visit_Enumeration_Type (E : Node_Id) is
      Enumerator     : Node_Id;
      Enum_Literals  : List_Id;
      Enum_Literal   : Node_Id;
      Enum_Type_Decl : Node_Id;

   begin
      Set_Main_Spec;
      Enum_Literals := New_List (K_Enumeration_Literals);
      Enumerator := First_Entity (Enumerators (E));
      while Present (Enumerator) loop
         Enum_Literal := Make_Defining_Identifier (Enumerator);
         Append_Node_To_List (Enum_Literal, Enum_Literals);
         Enumerator := Next_Entity (Enumerator);
      end loop;

      Enum_Type_Decl :=
        Make_Full_Type_Declaration
          (Make_Defining_Identifier (E),
           Make_Enumeration_Type_Definition (Enum_Literals));

      Set_BE_Node (E, Enum_Type_Decl);
      Append_Node_To_List (Enum_Type_Decl, Visible_Part (Current_Package));
   end Visit_Enumeration_Type;

   ---------------------------------
   -- Visit_Interface_Declaration --
   ---------------------------------

   procedure Visit_Interface_Declaration (E : Node_Id) is
      P : Node_Id;
      N : Node_Id;
      L : List_Id;

   begin
      P := Make_IDL_Unit (E);
      Append_Node_To_List (P, Packages (BE_Node (Scope (Identifier (E)))));

      Push_Entity (P);
      Set_Main_Spec;
      L := Interface_Spec (E);
      if Is_Empty (L) then
         N := Make_Defining_Identifier (Object_Ref);
      else
         N := Make_Defining_Identifier (IDL_Name (Identifier (E)));
      end if;
      N :=
        Make_Full_Type_Declaration
          (Make_Defining_Identifier (Interface_Ref),
           Make_Derived_Type_Definition
             (Subtype_Indication    => N,
              Record_Extension_Part =>
                Make_Record_Type_Definition
                  (Record_Definition => Make_Record_Definition (No_List))));
      Append_Node_To_List (N, Visible_Part (Current_Package));

      N := First_Entity (Interface_Body (E));
      while Present (N) loop
         Visit (N);
         N := Next_Entity (N);
      end loop;
      Pop_Entity;
   end Visit_Interface_Declaration;

   ------------------
   -- Visit_Module --
   ------------------

   procedure Visit_Module (E : Node_Id) is
      D : Node_Id;
      S : Node_Id;

   begin
      S := Make_IDL_Unit (E);
      Push_Entity (S);
      D := First_Entity (Definitions (E));
      while Present (D) loop
         Visit (D);
         D := Next_Entity (D);
      end loop;
      Pop_Entity;
   end Visit_Module;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      D : Node_Id;
      S : Node_Id;

   begin
      S := Make_IDL_Unit (E);
      Push_Entity (S);
      D := First_Entity (Definitions (E));
      while Present (D) loop
         Visit (D);
         D := Next_Entity (D);
      end loop;
      Pop_Entity;
   end Visit_Specification;

   ----------------------------
   -- Visit_Type_Declaration --
   ----------------------------

   procedure Visit_Type_Declaration (E : Node_Id) is
      D : Node_Id;
      T : Node_Id;
      N : Node_Id;

   begin
      Set_Main_Spec;
      T := Make_Designator (Type_Spec (E));
      D := First_Entity (Declarators (E));
      while Present (D) loop
         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier (D),
            Type_Definition     => Make_Derived_Type_Definition
              (Subtype_Indication    => T,
               Record_Extension_Part => No_Node));
         Append_Node_To_List (N, Visible_Part (Current_Package));
         D := Next_Entity (D);
      end loop;
   end Visit_Type_Declaration;

end Backend.BE_A;
