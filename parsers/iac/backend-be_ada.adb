with GNAT.Command_Line; use GNAT.Command_Line;

with Namet;     use Namet;
with Output;    use Output;
with Types;     use Types;

with Frontend.Nodes;           use Frontend.Nodes;

with Backend.BE_Ada.Debug;     use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Generator; use Backend.BE_Ada.Generator;
with Backend.BE_Ada.Nodes;     use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;    use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada is

   D_Tree   : Boolean := False;

   Interface_Reference         : Node_Id;
   Interface_Ref_Designator    : Node_Id;
   CORBA_Designator            : Node_Id;
   CORBA_Object_Designator     : Node_Id;
   CORBA_Object_Ref_Designator : Node_Id;

   Return_Parameter_Name  : Name_Id;
   Self_Parameter_Name    : Name_Id;
   To_Parameter_Name      : Name_Id;

   package FEN renames Frontend.Nodes;

   procedure Visit (E : Node_Id);
   procedure Visit_Attribute_Declaration (E : Node_Id);
   procedure Visit_Constant_Declaration (E : Node_Id);
   procedure Visit_Enumeration_Type (E : Node_Id);
   procedure Visit_Interface_Declaration (E : Node_Id);
   procedure Visit_Module (E : Node_Id);
   procedure Visit_Operation_Declaration (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);
   procedure Visit_Structure_Type (E : Node_Id);
   procedure Visit_Type_Declaration (E : Node_Id);

   function Make_IDL_Unit (E : Node_Id) return Node_Id;
   function Make_Package_Declaration (E : Node_Id) return Node_Id;

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
      Interface_Reference := Make_Defining_Identifier (Name_Find);

      Interface_Ref_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("Ref");
      Set_Defining_Identifier
        (Interface_Ref_Designator, Make_Defining_Identifier (Name_Find));

      CORBA_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("CORBA");
      Set_Defining_Identifier
        (CORBA_Designator, Make_Defining_Identifier (Name_Find));

      CORBA_Object_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("Object");
      Set_Defining_Identifier
        (CORBA_Object_Designator, Make_Defining_Identifier (Name_Find));
      Set_Parent_Unit_Name
        (CORBA_Object_Designator, CORBA_Designator);

      CORBA_Object_Ref_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("Ref");
      Set_Defining_Identifier
        (CORBA_Object_Ref_Designator, Make_Defining_Identifier (Name_Find));
      Set_Parent_Unit_Name
        (CORBA_Object_Ref_Designator, CORBA_Object_Designator);

      Set_Str_To_Name_Buffer ("Return");
      Return_Parameter_Name := Name_Find;
      Set_Str_To_Name_Buffer ("Self");
      Self_Parameter_Name := Name_Find;
      Set_Str_To_Name_Buffer ("To");
      To_Parameter_Name := Name_Find;

      Set_Space_Increment (3);
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
      N : Node_Id;
      D : Node_Id;
      L : List_Id;
      I : Node_Id;

   begin
      P := New_Node (K_IDL_Unit, E);

      L := New_List (K_Packages);
      Set_Packages (P, L);

      Get_Name_String (IDL_Name (FEN.Identifier (E)));
      if Kind (E) = K_Specification then
         Add_Str_To_Name_Buffer ("_IDL_File");
      end if;
      I := Make_Defining_Identifier (Name_Find);

      --  Main package

      D := Make_Package_Declaration (I);
      Set_Main_Package (P, D);
      Append_Node_To_List (D, L);

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Defining_Identifier (Name_Find);
      Set_Parent_Unit_Name (N, I);
      D := Make_Package_Declaration (N);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (E) = K_Interface_Declaration then

         --  Skeleton package

         Set_Str_To_Name_Buffer ("Skel");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Skeleton_Package (P, D);
         Append_Node_To_List (D, L);

         --  Implementation package

         Set_Str_To_Name_Buffer ("Impl");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Implementation_Package (P, D);
         Append_Node_To_List (D, L);
      end if;

      return P;
   end Make_IDL_Unit;

   ------------------------------
   -- Make_Package_Declaration --
   ------------------------------

   function Make_Package_Declaration (E : Node_Id) return Node_Id is
      D : Node_Id;
      P : Node_Id;

   begin
      D := New_Node (K_Package_Declaration);
      Set_Defining_Identifier (D, E);
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

         when K_Operation_Declaration =>
            Visit_Operation_Declaration (E);

         when K_Structure_Type =>
            Visit_Structure_Type (E);

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
         X := To_Ada_Name (IDL_Name (FEN.Identifier (A)));
         Set_Str_To_Name_Buffer ("Get_");
         Get_Name_String_And_Append (X);
         N := Make_Defining_Identifier (Name_Find);
         L := New_List (K_Parameter_Profile);
         P := Make_Parameter_Specification
           (Make_Defining_Identifier (Self_Parameter_Name),
            Interface_Ref_Designator);
         Append_Node_To_List (P, L);
         T := Make_Designator (Type_Spec (E));
         N := Make_Subprogram_Specification (N, L, T);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         if not Is_Readonly (E) then
            Set_Str_To_Name_Buffer ("Set_");
            Get_Name_String_And_Append (X);
            N := Make_Defining_Identifier (Name_Find);
            L := New_List (K_Parameter_Profile);
            P := Make_Parameter_Specification
              (Make_Defining_Identifier (Self_Parameter_Name),
               Interface_Ref_Designator);
            Append_Node_To_List (P, L);
            P := Make_Parameter_Specification
              (Make_Defining_Identifier (To_Parameter_Name),
               Make_Designator (Type_Spec (E)));
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
      Append_Node_To_List (P, Packages (Current_Entity));

      Push_Entity (P);
      Set_Main_Spec;
      L := Interface_Spec (E);
      if Is_Empty (L) then
         N := Copy_Node (CORBA_Object_Ref_Designator);
      else
         N := Make_Designator (First_Entity (L));
      end if;
      N :=
        Make_Full_Type_Declaration
          (Copy_Node (Interface_Reference),
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
      Append_Node_To_List (S, Packages (Current_Entity));

      Push_Entity (S);
      D := First_Entity (Definitions (E));
      while Present (D) loop
         Visit (D);
         D := Next_Entity (D);
      end loop;
      Pop_Entity;
   end Visit_Module;

   ---------------------------------
   -- Visit_Operation_Declaration --
   ---------------------------------

   procedure Visit_Operation_Declaration (E : Node_Id) is
      O : Node_Id;
      L : List_Id;
      P : Node_Id;
      N : Node_Id;
      M : Mode_Id := Mode_In;
      R : Node_Id;

   begin
      L := New_List (K_Parameter_Profile);
      P := Make_Parameter_Specification
        (Make_Defining_Identifier (Self_Parameter_Name),
         Copy_Node (Interface_Ref_Designator));
      Append_Node_To_List (P, L);

      N := First_Entity (Parameters (E));
      while Present (N) loop
         P := Make_Parameter_Specification
           (Make_Defining_Identifier (Declarator (N)),
            Make_Designator (Type_Spec (N)),
            FEN.Parameter_Mode (N));
         M := M or FEN.Parameter_Mode (N);
         Append_Node_To_List (P, L);
         N := Next_Entity (N);
      end loop;
      if FEN.Kind (Type_Spec (E)) = K_Void then
         M := Mode_Out;
      end if;
      if M = Mode_In then
         R := Make_Designator (Type_Spec (E));
      else
         P := Make_Parameter_Specification
           (Make_Defining_Identifier (Return_Parameter_Name),
            Make_Designator (Type_Spec (E)),
            Mode_Out);
         Append_Node_To_List (P, L);
      end if;
      O := Make_Subprogram_Specification
        (Make_Defining_Identifier (E), L, R);
      Append_Node_To_List (O, Visible_Part (Current_Package));
   end Visit_Operation_Declaration;

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

   --------------------------
   -- Visit_Structure_Type --
   --------------------------

   procedure Visit_Structure_Type (E : Node_Id) is
      N : Node_Id;
      M : Node_Id;
      L : List_Id;
      D : Node_Id;

   begin
      Set_Main_Spec;
      L := New_List (K_Component_List);
      M := First_Entity (Members (E));
      while Present (M) loop
         D := First_Entity (Declarators (M));
         while Present (D) loop
            N := Make_Component_Declaration
              (Make_Defining_Identifier (D),
               Make_Designator (Type_Spec (M)));
            Append_Node_To_List (N, L);
            D := Next_Entity (D);
         end loop;
         M := Next_Entity (M);
      end loop;

      N := Make_Full_Type_Declaration
        (Make_Defining_Identifier (E),
         Make_Record_Type_Definition
           (Make_Record_Definition (L)));
      Append_Node_To_List (N, Visible_Part (Current_Package));
   end Visit_Structure_Type;

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

end Backend.BE_Ada;
