with GNAT.Table;

with Locations; use Locations;
with Namet;     use Namet;
with Output;    use Output;
with Utils;     use Utils;
with Values;    use Values;

with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
with Frontend.Nodes;

package body Backend.BE_Ada.Nutils is

   package BEN renames Backend.BE_Ada.Nodes;

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;

   procedure Add_With_Package (P : Node_Id);

   CORBA_Type : array (FEN.K_Float .. FEN.K_Value_Base) of Name_Id;

   ----------------------
   -- Add_With_Package --
   ----------------------

   procedure Add_With_Package (P : Node_Id) is
      W : Node_Id;

      procedure Get_Defining_Identifier_Name (N : Node_Id);

      ----------------------------------
      -- Get_Defining_Identifier_Name --
      ----------------------------------

      procedure Get_Defining_Identifier_Name (N : Node_Id) is
         P : constant Node_Id := Parent_Unit_Name (N);

      begin
         if Present (P) then
            Get_Defining_Identifier_Name (P);
            Add_Char_To_Name_Buffer ('.');
         end if;
         Get_Name_String_And_Append (Name (N));
      end Get_Defining_Identifier_Name;

      B : Byte;
      N : Name_Id;

   begin
      Name_Len := 0;
      Get_Defining_Identifier_Name
        (Defining_Identifier (Package_Declaration (Current_Package)));
      Add_Char_To_Name_Buffer (' ');
      Get_Defining_Identifier_Name
        (Defining_Identifier (P));
      Add_Char_To_Name_Buffer ('%');
      if Kind (Current_Package) = K_Package_Specification then
         Add_Char_To_Name_Buffer ('s');
      else
         Add_Char_To_Name_Buffer ('b');
      end if;
      N := To_Lower (Name_Find);
      B := Get_Name_Table_Byte (N);
      if B /= 0 then
         return;
      end if;
      Set_Name_Table_Byte (N, 1);
      W := New_Node (K_Withed_Package);
      Set_Defining_Identifier (W, P);
      Append_Node_To_List (W, Withed_Packages (Current_Package));
   end Add_With_Package;

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

   ---------------------
   -- Copy_Designator --
   ---------------------

   function Copy_Designator
     (Designator : Node_Id)
     return Node_Id
   is
      D : Node_Id;
      P : Node_Id := Parent_Unit_Name (Designator);

   begin
      D := Copy_Node (Designator);
      if Present (P) then
         P := Copy_Designator (P);
         Set_Parent_Unit_Name (D, P);
         Add_With_Package (P);
      end if;
      return D;
   end Copy_Designator;


   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (N : Node_Id) return Node_Id is
      C : Node_Id;

   begin
      case Kind (N) is
         when K_Designator =>
            C := New_Node (K_Designator);
            Set_Defining_Identifier (C, Defining_Identifier (N));
            Set_Parent_Unit_Name (C, Parent_Unit_Name (N));

         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            Set_Name (C, Name (N));
            Set_Parent_Unit_Name (C, Parent_Unit_Name (N));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

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
         Set_Str_To_Name_Buffer (FEN.Node_Kind'Image (K));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
      else
         Set_Str_To_Name_Buffer (S);
      end if;
      Capitalize (Name_Buffer (1 .. Name_Len));
      CORBA_Type (K) := Name_Find;
   end Declare_CORBA_Type;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   --------------------------------
   -- Make_Array_Type_Definition --
   --------------------------------

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (BEN.K_Array_Type_Definition);
      Set_Range_Constraints (N, Range_Constraints);
      Set_Component_Definition (N, Component_Definition);
      return N;
   end Make_Array_Type_Definition;

   --------------------------------
   -- Make_Component_Declaration --
   --------------------------------

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Component_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Subtype_Indication (N, Subtype_Indication);
      return N;
   end Make_Component_Declaration;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Entity : Node_Id) return Node_Id is
      use FEN;

      I : Node_Id := Entity;

   begin
      if FEN.Kind (Entity) /= FEN.K_Identifier then
         I := FEN.Identifier (Entity);
      end if;
      return Make_Defining_Identifier (IDL_Name (I));
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
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id;
      Is_Abstract_Type      : Boolean := False)
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

   ---------------------
   -- Make_Designator --
   ---------------------

   function Make_Designator (Entity : Node_Id) return Node_Id is
      use FEN;
      I : Node_Id;
      P : Node_Id;
      N : Node_Id;
      K : FEN.Node_Kind;
      R : Node_Id;

   begin
      K := FEN.Kind (Entity);
      if K = FEN.K_Scoped_Name then
         R := Reference (Entity);
         if Kind (BE_Node (R)) = K_Specification then
            return No_Node;
         end if;
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, Make_Defining_Identifier (R));
         P := Parent_Entity (Entity);
         if Present (P) then
            Set_Parent_Unit_Name (N, Make_Designator (P));
         end if;

      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         Set_Str_To_Name_Buffer ("CORBA");
         I := Make_Defining_Identifier (Name_Find);
         P := New_Node (K_Designator);
         Set_Defining_Identifier (P, I);
         I := Make_Defining_Identifier (CORBA_Type (K));
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, I);
         Set_Parent_Unit_Name (N, P);

      else
         raise Program_Error;
      end if;

      P := Parent_Unit_Name (N);
      if Present (P) then
         Add_With_Package (P);
      end if;

      return N;
   end Make_Designator;

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

   -------------------------------------
   -- Make_Fully_Qualified_Identifier --
   -------------------------------------

   function Make_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id is
      use FEN;

      N : Node_Id;
      P : Node_Id;
      I : Node_Id;

   begin
      I := FEN.Identifier (Entity);
      Get_Name_String (IDL_Name (I));
      if Kind (Entity) = K_Specification then
         Add_Str_To_Name_Buffer ("_IDL_File");
      end if;
      N := Make_Defining_Identifier (Name_Find);
      P := FEN.Scope_Entity (I);
      if Present (P)
        and then FEN.Kind (P) /= FEN.K_Specification
      then
         Set_Parent_Unit_Name (N, Make_Fully_Qualified_Identifier (P));
      end if;
      return N;
   end Make_Fully_Qualified_Identifier;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal
     (Value : Value_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Literal);
      Set_Value (N, Value);
      return N;
   end Make_Literal;

   -----------------------------
   -- Make_Object_Declaration --
   -----------------------------

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean;
      Object_Definition   : Node_Id;
      Expression          : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Object_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Constant_Present (N, Constant_Present);
      Set_Object_Definition (N, Object_Definition);
      Set_Expression (N, Expression);
      return N;
   end Make_Object_Declaration;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In)
      return                Node_Id
   is
      P : Node_Id;

   begin
      P := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Parameter_Type (P, Subtype_Mark);
      Set_Parameter_Mode (P, Parameter_Mode);
      return P;
   end Make_Parameter_Specification;

   ----------------------------
   -- Make_Range_Constraints --
   ----------------------------

   function Make_Range_Constraints
     (Array_Sizes : List_Id)
     return List_Id
   is
      L : List_Id;
      S : Node_Id;
      R : Node_Id;
      V : Value_Type;

   begin
      L := New_List (K_Range_Constraints);
      S := FEN.First_Entity (Array_Sizes);
      while Present (S) loop
         R := New_Node (K_Range_Constraint);
         Set_First (R, Int0_Val);
         V := Value (FEN.Value (S));
         V.IVal := V.IVal - 1;
         Set_Last (R, New_Value (V));
         Append_Node_To_List (R, L);
         S := FEN.Next_Entity (S);
      end loop;
      return L;
   end Make_Range_Constraints;

   ----------------------------
   -- Make_Record_Definition --
   ----------------------------

   function Make_Record_Definition
     (Component_List : List_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Definition);
      Set_Component_List (N, Component_List);
      return N;
   end Make_Record_Definition;

   ---------------------------------
   -- Make_Record_Type_Definition --
   ---------------------------------

   function Make_Record_Type_Definition
     (Record_Definition : Node_Id;
      Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False)
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

   ------------------------------------
   -- Make_Subprogram_Implementation --
   ------------------------------------

   function Make_Subprogram_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id)

     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Implementation);
      Set_Specification (N, Specification);
      Set_Declarations (N, Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Subprogram_Implementation;

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

end Backend.BE_Ada.Nutils;
