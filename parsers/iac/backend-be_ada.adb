with GNAT.Command_Line; use GNAT.Command_Line;

with Namet;     use Namet;
with Output;    use Output;
with Types;     use Types;
with Values;    use Values;

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
   Standard_Designator         : Node_Id;
   Standard_String_Designator  : Node_Id;

   Returns_Parameter_Name : Name_Id;
   Self_Parameter_Name    : Name_Id;
   To_Parameter_Name      : Name_Id;

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   Getter : constant Character := 'G';
   Setter : constant Character := 'S';

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

   function Make_Accessor_Declaration
     (Accessor : Character; Attribute : Node_Id) return Node_Id;
   function Make_IDL_Unit (E : Node_Id) return Node_Id;
   function Make_Package_Declaration (E : Node_Id) return Node_Id;
   function Make_Repository_Declaration (N : Node_Id) return Node_Id;

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

      Standard_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("Standard");
      Set_Defining_Identifier
        (Standard_Designator, Make_Defining_Identifier (Name_Find));

      Standard_String_Designator := New_Node (K_Designator);
      Set_Str_To_Name_Buffer ("String");
      Set_Defining_Identifier
        (Standard_String_Designator, Make_Defining_Identifier (Name_Find));
      Set_Parent_Unit_Name
        (Standard_String_Designator, Standard_Designator);

      Set_Str_To_Name_Buffer ("Returns");
      Returns_Parameter_Name := Name_Find;
      Set_Str_To_Name_Buffer ("Self");
      Self_Parameter_Name := Name_Find;
      Set_Str_To_Name_Buffer ("To");
      To_Parameter_Name := Name_Find;

      Int0_Val := New_Integer_Value (0, 1, 10);

      Set_Space_Increment (3);
      Visit_Specification (E);
      if D_Tree then
         W_Node_Id (BE_Node (E));
      end if;
      Generator.Initialize;
      Generator.Generate (BE_Node (E));
   end Generate;

   ---------------------------------
   -- Make_Accessor_Specification --
   ---------------------------------

   function Make_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
     return Node_Id is
      P : Node_Id;
      L : List_Id;
      T : Node_Id;
      N : Name_Id;

   begin
      L := New_List (K_Parameter_Profile);
      P := Make_Parameter_Specification
        (Make_Defining_Identifier (Self_Parameter_Name),
         Interface_Ref_Designator);
      Append_Node_To_List (P, L);
      if Accessor = Setter then
         P := Make_Parameter_Specification
           (Make_Defining_Identifier (To_Parameter_Name),
            Make_Designator (Type_Spec (Declaration (Attribute))));
         Append_Node_To_List (P, L);
         T := No_Node;
      else
         T := Make_Designator (Type_Spec (Declaration (Attribute)));
      end if;
      N := To_Ada_Name (IDL_Name (FEN.Identifier (Attribute)));
      Set_Str_To_Name_Buffer ("Set_");
      Name_Buffer (1) := Accessor;
      Get_Name_String_And_Append (N);
      return Make_Subprogram_Specification
        (Make_Defining_Identifier (Name_Find), L, T);
   end Make_Accessor_Declaration;

   -------------------
   -- Make_IDL_Unit --
   -------------------

   function Make_IDL_Unit (E : Node_Id) return Node_Id is
      P : Node_Id;
      N : Node_Id;
      M : Node_Id;  -- Main Package;
      D : Node_Id;
      L : List_Id;
      I : Node_Id;

   begin
      P := New_Node (K_IDL_Unit, E);

      L := New_List (K_Packages);
      Set_Packages (P, L);

      I := Make_Fully_Qualified_Identifier (E);

      --  Main package

      M := Make_Package_Declaration (I);
      Set_Main_Package (P, M);
      Append_Node_To_List (M, L);

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Defining_Identifier (Name_Find);
      Set_Parent_Unit_Name (N, I);
      D := Make_Package_Declaration (N);
      Set_Parent (D, M);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (E) = K_Interface_Declaration then

         --  Skeleton package

         Set_Str_To_Name_Buffer ("Skel");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Parent (D, M);
         Set_Skeleton_Package (P, D);
         Append_Node_To_List (D, L);

         --  Implementation package

         Set_Str_To_Name_Buffer ("Impl");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Parent (D, M);
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
      if Present (Current_Entity) then
         Set_Parent (D, Main_Package (Current_Entity));
      end if;
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

   ---------------------------------
   -- Make_Repository_Declaration --
   ---------------------------------

   function Make_Repository_Declaration (N : Node_Id) return Node_Id is
      I : Name_Id;
      V : Value_Id;

   begin
      Get_Name_String (BEN.Name (Defining_Identifier (N)));
      Add_Str_To_Name_Buffer ("_Repository_Id");
      I := Name_Find;
      Set_Str_To_Name_Buffer ("IDL:");
      Get_Name_String_And_Append (BEN.Name (Defining_Identifier (N)));
      Add_Str_To_Name_Buffer (":1.0");
      V := New_String_Value (Name_Find, False);
      return Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (I),
         Constant_Present    => True,
         Object_Definition   => Copy_Designator (Standard_String_Designator),
         Expression          => Make_Literal (V));
   end Make_Repository_Declaration;

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
      A : Node_Id;
      P : Node_Id;

   begin

      A := First_Entity (Declarators (E));
      while Present (A) loop
         Set_Main_Spec;
         N := Make_Accessor_Declaration
           (Accessor => Getter, Attribute => A);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         Set_Main_Body;
         P := Make_Subprogram_Implementation
           (N, No_List, No_List);
         Append_Node_To_List (P, Statements (Current_Package));

         if not Is_Readonly (E) then
            Set_Main_Spec;
            N := Make_Accessor_Declaration
              (Accessor => Setter, Attribute => A);
            Append_Node_To_List (N, Visible_Part (Current_Package));

            Set_Main_Body;
            P := Make_Subprogram_Implementation
              (N, No_List, No_List);
            Append_Node_To_List (P, Statements (Current_Package));
         end if;

         A := Next_Entity (A);
      end loop;
   end Visit_Attribute_Declaration;

   --------------------------------
   -- Visit_Constant_Declaration --
   --------------------------------

   procedure Visit_Constant_Declaration (E : Node_Id) is
      N : Node_Id;

   begin
      Set_Main_Spec;
      N := Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (E),
         Constant_Present    => True,
         Object_Definition   => Make_Designator (Type_Spec (E)),
         Expression          => Make_Literal (FEN.Value (FEN.Expression (E))));
      Append_Node_To_List (N, Visible_Part (Current_Package));
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
      Append_Node_To_List
        (Enum_Type_Decl,
         Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (Enum_Type_Decl),
         Visible_Part (Current_Package));
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
         N := Copy_Designator (CORBA_Object_Ref_Designator);
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
      Append_Node_To_List
        (N, Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (N), Visible_Part (Current_Package));

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
      Subp_Body : Node_Id;
      Subp_Spec : Node_Id;
      Profile   : List_Id;
      IDL_Param : Node_Id;
      Ada_Param : Node_Id;
      Mode      : Mode_Id := Mode_In;
      Returns   : Node_Id := No_Node;

   begin
      Profile := New_List (K_Parameter_Profile);

      --  Create a dispatching parameter

      Ada_Param := Make_Parameter_Specification
        (Make_Defining_Identifier (Self_Parameter_Name),
         Copy_Designator (Interface_Ref_Designator));
      Append_Node_To_List (Ada_Param, Profile);

      --  Create an Ada subprogram parameter for each IDL subprogram
      --  parameter. Check whether there is one inout or out parameter.

      IDL_Param := First_Entity (Parameters (E));
      while Present (IDL_Param) loop
         Ada_Param := Make_Parameter_Specification
           (Make_Defining_Identifier (Declarator (IDL_Param)),
            Make_Designator (Type_Spec (IDL_Param)),
            FEN.Parameter_Mode (IDL_Param));
         if FEN.Parameter_Mode (IDL_Param) /= Mode_In then
            Mode := Mode_Out;
         end if;
         Append_Node_To_List (Ada_Param, Profile);
         IDL_Param := Next_Entity (IDL_Param);
      end loop;

      --  If the IDL subprogram is a function, then check whether it
      --  has inout and out parameters. In this case, map the IDL
      --  function as an Ada procedure and not an Ada function.

      if FEN.Kind (Type_Spec (E)) /= K_Void then
         if Mode = Mode_In then
            Returns := Make_Designator (Type_Spec (E));

         --  If the IDL function is mapped as an Ada procedure, add a
         --  new parameter Returns to pass the returned value.

         else
            Ada_Param := Make_Parameter_Specification
              (Make_Defining_Identifier (Returns_Parameter_Name),
               Make_Designator (Type_Spec (E)),
               Mode_Out);
            Append_Node_To_List (Ada_Param, Profile);
         end if;
      end if;

      --  Add subprogram to main specification

      Set_Main_Spec;
      Subp_Spec := Make_Subprogram_Specification
        (Make_Defining_Identifier (E), Profile, Returns);
      Append_Node_To_List (Specification, Visible_Part (Current_Package));

      --  Add subprogram to main implementation

      Set_Main_Body;
      Subp_Body := Make_Subprogram_Implementation
        (Subp_Spec, No_List, No_List);
      Append_Node_To_List (Subp_Body, Statements (Current_Package));
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
      T : Node_Id;

   begin
      Set_Main_Spec;
      L := New_List (K_Component_List);
      M := First_Entity (Members (E));
      while Present (M) loop
         D := First_Entity (Declarators (M));
         while Present (D) loop
            T := Make_Designator (Type_Spec (M));
            if Kind (D) = K_Complex_Declarator then
               Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (D))));
               Add_Str_To_Name_Buffer ("_Array");
               T := Make_Full_Type_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (Name_Find),
                  Type_Definition     => Make_Array_Type_Definition
                    (Make_Range_Constraints (FEN.Array_Sizes (D)), T));
               Append_Node_To_List (T, Visible_Part (Current_Package));
               Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (D))));
               Add_Str_To_Name_Buffer ("_Array");
               T := New_Node (K_Designator);
               Set_Defining_Identifier
                 (T, Make_Defining_Identifier (Name_Find));
            end if;
            N := Make_Component_Declaration
              (Make_Defining_Identifier (D), T);
            Append_Node_To_List (N, L);
            D := Next_Entity (D);
         end loop;
         M := Next_Entity (M);
      end loop;

      N := Make_Full_Type_Declaration
        (Make_Defining_Identifier (E),
         Make_Record_Type_Definition
           (Make_Record_Definition (L)));
      Append_Node_To_List
        (N, Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (N), Visible_Part (Current_Package));
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
         if Kind (D) = K_Complex_Declarator then
            N := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (D),
               Type_Definition     => Make_Array_Type_Definition
                 (Make_Range_Constraints (FEN.Array_Sizes (D)), T));
         else
            N := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (D),
               Type_Definition     => Make_Derived_Type_Definition
                 (Subtype_Indication    => T,
                  Record_Extension_Part => No_Node));
         end if;
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Append_Node_To_List
           (Make_Repository_Declaration (N), Visible_Part (Current_Package));
         D := Next_Entity (D);
      end loop;
   end Visit_Type_Declaration;

end Backend.BE_Ada;
