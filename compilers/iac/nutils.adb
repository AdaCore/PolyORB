package body Nutils is

   ---------------------------
   -- Append_Entity_To_List --
   ---------------------------

   procedure Append_Entity_To_List (E : Entity_Id; L : List_Id) is
      Last : Entity_Id;
   begin
      Last := Last_Entity (L);
      if No (Last) then
         Set_First_Entity (L, E);
      else
         Set_Next_Entity (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Entity (L, Last);
         Last := Next_Entity (Last);
      end loop;
   end Append_Entity_To_List;

   ---------------
   -- Associate --
   ---------------

   procedure Associate (E : Entity_Id; N : Node_Id) is
   begin
      Set_Identifier (E, N);
      Set_Entity     (N, E);
   end Associate;

   ---------------------
   -- Is_A_Forward_Of --
   ---------------------

   function Is_A_Forward_Of (X, Y : Entity_Id) return Boolean is
      KX : Node_Kind := Kind (X);
      KY : Node_Kind := Kind (Y);
   begin
      case KY is
         when K_Interface_Declaration
           | K_Forward_Interface_Declaration =>
            return KX = K_Forward_Interface_Declaration;

         when K_Structure_Type
            | K_Forward_Structure_Type =>
            return KX = K_Forward_Structure_Type;

         when K_Union_Type
            | K_Forward_Union_Type =>
            return KX = K_Forward_Union_Type;

         when K_Value_Declaration
           | K_Abstract_Value_Declaration
           | K_Value_Box_Declaration
           | K_Value_Forward_Declaration =>
            if KX /= K_Value_Forward_Declaration then
               return False;

            elsif Is_Abstract (X) then
               return KY = K_Abstract_Value_Declaration
                 or else (KY = K_Value_Forward_Declaration
                          and then Is_Abstract (Y));

            else
               return KY /= K_Abstract_Value_Declaration
                 and then (KY /= K_Value_Forward_Declaration
                           or else not Is_Abstract (Y));
            end if;

         when others =>
            return False;
      end case;
   end Is_A_Forward_Of;

   ----------------
   -- Is_A_Scope --
   ----------------

   function Is_A_Scope (E : Entity_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Module
           | K_Enumeration_Type
           | K_Structure_Type
           | K_Union_Type
           | K_Exception_Declaration
           | K_Interface_Declaration =>
            return True;

         when others =>
            return False;
      end case;
   end Is_A_Scope;

   ---------------
   -- Is_A_Type --
   ---------------

   function Is_A_Type (E : Entity_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Type_Declaration
           | K_Forward_Structure_Type
           | K_Structure_Type
           | K_Forward_Union_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Native_Type
           | K_Sequence_Type
           | K_String
           | K_Wide_String
           | K_Fixed_Point_Type
           | K_Float
           | K_Double
           | K_Long_Double
           | K_Short
           | K_Long
           | K_Long_Long
           | K_Unsigned_Short
           | K_Unsigned_Long
           | K_Unsigned_Long_Long
           | K_Char
           | K_Wide_Char
           | K_Boolean
           | K_Octet
           | K_Object
           | K_Any
           | K_Void
           | K_Value_Base =>
            return True;
         when others =>
            return False;
      end case;
   end Is_A_Type;

   -------------------------------
   -- Is_Attribute_Or_Operation --
   -------------------------------

   function Is_Attribute_Or_Operation (E : Entity_Id) return Boolean
   is
      K : Node_Kind := Kind (E);
   begin
      return K = K_Attribute_Declaration
        or else K = K_Operation_Declaration;
   end Is_Attribute_Or_Operation;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Entity (L));
   end Is_Empty;

   -------------------------------------
   -- Is_Interface_Redefinable_Entity --
   -------------------------------------

   function Is_Interface_Redefinable_Entity (E : Entity_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Type_Declaration
           | K_Constant_Declaration
           | K_Forward_Structure_Type
           | K_Structure_Type
           | K_Forward_Union_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Native_Type
           | K_Sequence_Type
           | K_String
           | K_Wide_String
           | K_Fixed_Point_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Interface_Redefinable_Entity;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (E : Entity_Id) return Entity_Id is
   begin
      Entries.Increment_Last;
      Entries.Table (Entries.Last) := Entries.Table (Node_Id (E));
      Set_Next_Entity (E, No_Entity);
      return Entity_Id (Entries.Last);
   end New_Copy;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (N : Node_Id) return Node_Id is
   begin
      Entries.Increment_Last;
      Entries.Table (Entries.Last) := Entries.Table (N);
      Set_Homonym (N, No_Node);
      return Entries.Last;
   end New_Copy;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (Kind : Node_Kind;
      Loc  : Location)
     return Entity_Id is
   begin
      return Entity_Id (New_Node (Kind, Loc));
   end New_Entity;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      Loc  : Location)
     return List_Id
   is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      Loc  : Location)
     return Node_Id
   is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   -----------------------------
   -- Remove_Entity_From_List --
   -----------------------------

   procedure Remove_Entity_From_List (E : Entity_Id; L : List_Id) is
      C : Entity_Id;
   begin
      C := First_Entity (L);
      if C = E then
         Set_First_Entity (L, Next_Entity (E));
         if Last_Entity (L) = E then
            Set_Last_Entity (L, No_Entity);
         end if;
      else
         while Present (C) loop
            if Next_Entity (C) = E then
               Set_Next_Entity (C, Next_Entity (E));
               if Last_Entity (L) = E then
                  Set_Last_Entity (L, C);
               end if;
               exit;
            end if;
            C := Next_Entity (C);
         end loop;
      end if;
   end Remove_Entity_From_List;

end Nutils;
