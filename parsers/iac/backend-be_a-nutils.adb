with GNAT.Table;

with Lexer;     use Lexer;
with Locations; use Locations;
with Namet;     use Namet;
with Utils;     use Utils;

with Backend.BE_A.Nodes; use Backend.BE_A.Nodes;
with Frontend.Nodes;

package body Backend.BE_A.Nutils is

   package BEN renames Backend.BE_A.Nodes;

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;

   CORBA_Type : array (FEN.K_Float .. FEN.K_Value_Base) of Name_Id;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   --------------------
   -- Current_Entity --
   --------------------

   function Current_Entity return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Entity;
      end if;
   end Current_Entity;

   ---------------------
   -- Current_Package --
   ---------------------

   function Current_Package return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Package;
      end if;
   end Current_Package;

   ------------------------
   -- Declare_CORBA_Type --
   ------------------------

   procedure Declare_CORBA_Type (K : FEN.Node_Kind; S : String := "") is
   begin
      if S'Length = 0 then
         Name_Len := 4;
         Add_Str_To_Name_Buffer (FEN.Node_Kind'Image (K));
      else
         Name_Len := 6;
         Add_Str_To_Name_Buffer (S);
      end if;
      Name_Buffer (1 .. 6) := "CORBA.";
      Capitalize (Name_Buffer (7 .. Name_Len));
      CORBA_Type (K) := Name_Find;
   end Declare_CORBA_Type;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Node : Node_Id) return Node_Id is
      use FEN;

      N : Name_Id;
      K : constant FEN.Node_Kind := FEN.Kind (Node);

   begin
      if K = FEN.K_Identifier then
         N := FEN.IDL_Name (Node);

      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         N := CORBA_Type (K);

      else
         N := FEN.IDL_Name (FEN.Identifier (Node));
      end if;
      return Make_Defining_Identifier (N);
   end Make_Defining_Identifier;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      Set_Name (N, To_Ada_Name (Name));
      return N;
   end Make_Defining_Identifier;

   ----------------------------------
   -- Make_Derived_Type_Definition --
   ----------------------------------

   function Make_Derived_Type_Definition
     (Is_Abstract_Type      : Boolean;
      Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Derived_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Record_Extension_Part (N, Record_Extension_Part);
      return N;
   end Make_Derived_Type_Definition;

   --------------------------------------
   -- Make_Enumeration_Type_Definition --
   --------------------------------------

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Enumeration_Type_Definition);
      Set_Enumeration_Literals (N, Enumeration_Literals);
      return N;
   end Make_Enumeration_Type_Definition;

   --------------------------------
   -- Make_Full_Type_Declaration --
   --------------------------------

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Full_Type_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Type_Definition (N, Type_Definition);
      return N;
   end Make_Full_Type_Declaration;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := 0)
      return                Node_Id
   is
      P : Node_Id;

   begin
      P := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Parameter_Type (P, Subtype_Mark);
      if Parameter_Mode = 0 then
         Set_Parameter_Mode (P, Token_Type'Pos (T_In));
      else
         Set_Parameter_Mode (P, Parameter_Mode);
      end if;
      return P;
   end Make_Parameter_Specification;

   ----------------------------
   -- Make_Record_Definition --
   ----------------------------

   function Make_Record_Definition
     (Component_List : List_Id)
     return Node_Id is
      pragma Unreferenced (Component_List);
   begin
      return No_Node;
   end Make_Record_Definition;

   ---------------------------------
   -- Make_Record_Type_Definition --
   ---------------------------------

   function Make_Record_Type_Definition
     (Is_Abstract_Type  : Boolean;
      Is_Tagged_Type    : Boolean;
      Is_Limited_Type   : Boolean;
      Record_Definition : Node_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Is_Tagged_Type (N, Is_Tagged_Type);
      Set_Is_Limited_Type (N, Is_Limited_Type);
      Set_Record_Definition (N, Record_Definition);
      return N;
   end Make_Record_Type_Definition;

   -----------------------------------
   -- Make_Subprogram_Specification --
   -----------------------------------

   function Make_Subprogram_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Profile   : List_Id;
      Return_Type         : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Specification);
      Set_Defining_Identifier  (N, Defining_Identifier);
      Set_Parameter_Profile    (N, Parameter_Profile);
      Set_Return_Type          (N, Return_Type);
      return N;
   end Make_Subprogram_Specification;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return List_Id is
      N : Node_Id;

   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         Set_Loc  (N, Loc (From));
      else
         Set_Loc  (N, No_Location);
      end if;

      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         BEN.Set_FE_Node (N, From);
         FEN.Set_BE_Node (From, N);
         Set_Loc  (N, FEN.Loc (From));
      else
         Set_Loc  (N, No_Location);
      end if;

      return N;
   end New_Node;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (E : Node_Id) return Mode_Type is
      M : Mode_Id;

   begin
      M := Nodes.Parameter_Mode (E);
      return Mode_Type'Val (M);
   end Parameter_Mode;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity is
   begin
      if Last > No_Depth then
         Decrement_Last;
      end if;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Current_Entity := E;
   end Push_Entity;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;

   begin
      C := First_Node (L);
      if C = E then
         Set_First_Node (L, Next_Node (E));
         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));
               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   -------------------
   -- Set_Impl_Body --
   -------------------

   procedure Set_Impl_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Implementation_Package (X));
   end Set_Impl_Body;

   -------------------
   -- Set_Impl_Spec --
   -------------------

   procedure Set_Impl_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Implementation_Package (X));
   end Set_Impl_Spec;

   -------------------
   -- Set_Main_Body --
   -------------------

   procedure Set_Main_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Main_Package (X));
   end Set_Main_Body;

   -------------------
   -- Set_Main_Spec --
   -------------------

   procedure Set_Main_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Main_Package (X));
   end Set_Main_Spec;

   ------------------------
   -- Set_Parameter_Mode --
   ------------------------

   procedure Set_Parameter_Mode (E : Node_Id; M : Mode_Type) is
      B : Byte;

   begin
      B := Mode_Type'Pos (M);
      Nodes.Set_Parameter_Mode (E, Mode_Id (B));
   end Set_Parameter_Mode;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (N : Name_Id) return Name_Id is
      First : Natural := 1;

   begin
      Get_Name_String (N);
      while First <= Name_Len
        and then Name_Buffer (First) = '_'
      loop
         First := First + 1;
      end loop;

      for I in First .. Name_Len loop
         if Name_Buffer (I) = '_'
           and then I < Name_Len
           and then Name_Buffer (I + 1) = '_'
         then
            Name_Buffer (I + 1) := 'U';
         end if;
      end loop;

      if Name_Buffer (Name_Len) = '_' then
         Add_Char_To_Name_Buffer ('U');
      end if;

      return Name_Find;
   end To_Ada_Name;

end Backend.BE_A.Nutils;
