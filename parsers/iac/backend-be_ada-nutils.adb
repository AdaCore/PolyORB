with Lexer;     use Lexer;
with Locations; use Locations;
with Namet;     use Namet;

with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;

package body Backend.BE_Ada.Nutils is

   use Inheritance_Stack;

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
   -- Current_Package --
   ---------------------

   function Current_Package return Node_Id is
   begin
      if Last = No_Inheritance_Depth then
         return No_Node;
      else
         return Table (Last).Node;
      end if;
   end Current_Package;

   -----------------------
   -- Full_Package_Name --
   -----------------------

   function Full_Package_Name (E : Node_Id) return String is
      P : Node_Id;
   begin
      P := Parent (E);
      if Present (P) then
         Set_Str_To_Name_Buffer (Full_Package_Name (P));
         Add_Char_To_Name_Buffer ('.');
         Get_Name_String_And_Append (Name (Identifier (E)));
         return Name_Buffer (1 .. Name_Len);
      else
         return Get_Name_String (Name (Identifier (E)));
      end if;
   end Full_Package_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Init;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ------------------------
   -- Make_Ada_Parameter --
   ------------------------

   function Make_Ada_Parameter
     (N : Node_Id; T : Node_Id; M : Mode_Id := 0) return Node_Id
   is
      P : Node_Id;
   begin
      P := New_Node (K_Ada_Parameter, No_Location);
      Set_Identifier (P, N);
      Set_Type_Spec  (P, T);
      if M = 0 then
         Set_Parameter_Mode (P, Token_Type'Pos (T_In));
      else
         Set_Parameter_Mode (P, M);
      end if;
      return P;
   end Make_Ada_Parameter;

   -------------------------
   -- Make_Ada_Identifier --
   -------------------------

   function Make_Ada_Identifier (N : Name_Id) return Node_Id is
      I : Node_Id;
   begin
      I := New_Node (K_Ada_Identifier, No_Location);
      Set_Name (I, To_Ada_Name (N));
      return I;
   end Make_Ada_Identifier;

   function Make_Ada_Identifier (S : String) return Node_Id is
   begin
      Set_Str_To_Name_Buffer (S);
      return Make_Ada_Identifier (Name_Find);
   end Make_Ada_Identifier;

   ---------------------
   -- Make_Array_Type --
   ---------------------

   function Make_Array_Type (T : Node_Id; L : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Array_Type, No_Location);
      Set_Type_Spec (N, T);
      Set_Array_Sizes (N, L);
      return N;
   end Make_Array_Type;

   --------------------------------
   -- Make_Empty_Enumerator_List --
   --------------------------------

   function Make_Empty_Enumerator_List return List_Id is
   begin
      return New_List (K_Enumerator_List, No_Location);
   end Make_Empty_Enumerator_List;

   ------------------------
   -- Make_Empty_List_Id --
   ------------------------

   function Make_Empty_List_Id return List_Id is
   begin
      return New_List (K_List_Id, No_Location);
   end Make_Empty_List_Id;

   --------------------------------------
   -- Make_Empty_Package_Specification --
   --------------------------------------

   function Make_Empty_Package_Spec return Node_Id is
   begin
      return New_Node (K_Package_Specification, No_Location);
   end Make_Empty_Package_Spec;

   ---------------------------
   -- Make_Enumeration_Type --
   ---------------------------

   function Make_Enumeration_Type (L : List_Id) return Node_Id is
      Enum_Node : Node_Id;
   begin
      Enum_Node := New_Node (K_Enumeration_Type, No_Location);
      Set_Enumerators (Enum_Node, L);
      return Enum_Node;
   end Make_Enumeration_Type;

   -----------------------------------
   -- Make_Derived_Type_Declaration --
   -----------------------------------

   function Make_Derived_Type_Declaration
     (Identifier_Node : Node_Id;
      Type_Spec_Node : Node_Id)
     return Node_Id
   is
      Node : Node_Id;
      Nested_Node : Node_Id;
   begin
      Nested_Node := New_Node (K_Derived_Type_Definition, No_Location);
      Set_Identifier (Nested_Node, Type_Spec_Node);
      Set_Is_Abstract (Nested_Node, False);
      Node := Make_Type_Declaration (Identifier_Node, Nested_Node);
      return Node;
   end Make_Derived_Type_Declaration;

   --------------------------
   -- Make_Integer_Literal --
   --------------------------

   function Make_Integer_Literal (V : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Integer_Literal, No_Location);
      Set_Value (N, V);
      return N;
   end Make_Integer_Literal;

   ------------------------------
   -- Make_Package_Declaration --
   ------------------------------

   function Make_Package_Declaration
     (I : Node_Id; P : Node_Id; S : Node_Id; Im : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Package_Declaration, No_Location);
      Set_Identifier (N, I);
      Set_Parent (N, P);
      if S = No_Node then
         Set_Package_Specification (N, Make_Empty_Package_Spec);
      else
         Set_Package_Specification (N, S);
      end if;
      Set_Package_Implementation (N, Im);
      return N;
   end Make_Package_Declaration;

   --------------------------
   -- Make_Subprogram_Spec --
   --------------------------

   function Make_Subprogram_Spec
     (S : Node_Id;
      P : List_Id;
      T : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Ada_Subprogram_Spec, No_Location);
      Set_Identifier (N, S);
      Set_Parameters (N, P);
      Set_Type_Spec (N, T);
      return N;
   end Make_Subprogram_Spec;

   ---------------------------
   -- Make_Type_Declaration --
   ---------------------------

   function Make_Type_Declaration
     (Type_Identifier : Node_Id;
      Type_Spec : Node_Id)
     return Node_Id
   is
      Node : Node_Id;
   begin
      Node := New_Node (K_Type_Declaration, No_Location);
      Set_Identifier (Node, Type_Identifier);
      Set_Type_Spec (Node, Type_Spec);
      return Node;
   end Make_Type_Declaration;

   --------------------------------
   -- Make_Package_Specification --
   --------------------------------

   function Make_Package_Specification
     (D : Node_Id; W : List_Id; V : List_Id; P : List_Id) return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Package_Specification, No_Location);
      Set_Package_Declaration (N, D);
      Set_Withed_Packages (N, W);
      Set_Visible_Part (N, V);
      Set_Private_Part (N, P);

      return N;
   end Make_Package_Specification;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      Loc : Location)
     return Node_Id is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      Loc  : Location)
     return List_Id is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (E : Node_Id) return String is
   begin
      return Get_Name_String (Name (Identifier (E)));
   end Package_Name;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (E : Node_Id) return Mode_Type is
      M : Mode_Id;
   begin
      M := Nodes.Parameter_Mode (E);
      return Mode_Type'Val (M);
   end Parameter_Mode;

   ------------------
   -- Push_Package --
   ------------------

   procedure Push_Package (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Node := E;
   end Push_Package;

   -----------------
   -- Pop_Package --
   -----------------

   procedure Pop_Package is
   begin
      if Last > No_Inheritance_Depth then
         Decrement_Last;
      else
         null;  --  maybe it's better to raise an exception.
      end if;
   end Pop_Package;

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

   function To_Ada_Name (N : Name_Id) return Name_Id
   is
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

   function To_Ada_Name (N : String) return String is
   begin
      Set_Str_To_Name_Buffer (N);
      return Get_Name_String (To_Ada_Name (Name_Find));
   end To_Ada_Name;

   ---------------------
   -- To_Library_Name --
   ---------------------

   function To_Library_Name (N : Name_Id) return Name_Id is
      First : constant Natural := 1;
      Lib_Name : Name_Id;
   begin
      Get_Name_String (N);
      Name_Len := Name_Len - 4;
      for I in First .. Name_Len loop
         if Name_Buffer (I) = ' ' then
            Name_Buffer (I) := '_';
         end if;
      end loop;
      Add_Str_To_Name_Buffer ("_IDL_File");
      Lib_Name := To_Ada_Name (Name_Find);
      return Lib_Name;
   end To_Library_Name;

end Backend.BE_Ada.Nutils;
