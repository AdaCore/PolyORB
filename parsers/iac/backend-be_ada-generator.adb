with Backend.BE_Ada.Debug;  use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;

with Errors; use Errors;
with Lexer;  use Lexer;
with Namet;  use Namet;
with Output; use Output;
with Types;  use Types;
with Values;  use Values;

package body Backend.BE_Ada.Generator is

   procedure Generate_Array_Sizes (L : List_Id);
   procedure Generate_Array_Type (E : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Identifier (E : Node_Id);
   procedure Generate_Package (E : Node_Id);
   procedure Generate_Package_Body (E : Node_Id);
   procedure Generate_Package_Spec (E : Node_Id);
   procedure Generate_Package_With (L : List_Id);
   procedure Generate_Parameter (E : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Private_Part (L : List_Id);
   procedure Generate_Record_Type_Spec (E : Node_Id);
   procedure Generate_Subprogram_Header (E : Node_Id);
   procedure Generate_Subprogram_Spec (E : Node_Id);
   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Type_Extension (E : Node_Id);
   procedure Generate_Type_Header  (E : Node_Id);
   procedure Generate_Type_Spec (E : Node_Id);
   procedure Generate_Visible_Part (L : List_Id);

   procedure W_Small_Indent;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : List_Id) is
      N : Node_Id;
   begin
      Set_Space_Increment (3);
      if E = No_List then
         return;
      end if;
      N := First_Node (E);
      while Present (N) loop
         case Kind (N) is
            when K_Package_Declaration =>
               Generate_Package (N);

            when others =>
               Set_Str_To_Name_Buffer ("Ada Package Node");
               Error_Name (1) := Name_Find;
               DE ("% Expected.");

         end case;
         Write_Eol;
         N := Next_Node (N);
      end loop;
   end Generate;

   --------------------------
   -- Generate_Array_Sizes --
   --------------------------

   procedure Generate_Array_Sizes (L : List_Id) is
      D : Node_Id;
      V : Value_Id;
      VT : Value_Type;
   begin
      Write_Str ("(");
      D := First_Node (L);
      while Present (D) loop
         Write_Str ("0 .. ");
         V := Value (D);
         VT := Value (V);
         VT.IVal := VT.IVal - 1;
         Set_Value (V, VT);
         Write_Str (Values.Image (V));
         D := Next_Node (D);
         if Present (D) then
            Write_Str (",");
            Write_Space;
         end if;
      end loop;
      Write_Str (")");
   end Generate_Array_Sizes;

   -------------------------
   -- Generate_Array_Type --
   -------------------------

   procedure Generate_Array_Type (E : Node_Id) is
   begin

      Write_Str ("array");
      Write_Space;
      Generate_Array_Sizes (Array_Sizes (E));
      Write_Space;
      Write_Str ("of");
      Write_Space;
      Generate_Type_Spec (Type_Spec (E));
   end Generate_Array_Type;

   ------------------------------
   -- Generate_Subprogram_Spec --
   ------------------------------

   procedure Generate_Subprogram_Spec (E : Node_Id) is
      P : List_Id;
      T : Node_Id;
   begin
      Generate_Subprogram_Header (E);
      Write_Eol;
      Increment_Indentation;
      P := Parameters (E);
      if not Is_Empty (P) then
         Generate_Parameter_List (P);
      end if;
      T := Type_Spec (E);
      if Present (T) then
         Write_Eol;
         N_Space := N_Space - 1;
         Write_Indentation;
         Write_Str ("return ");
         N_Space := N_Space + 1;
         Generate_Type_Spec (T);
      end if;
      Write_Line (";");
      Decrement_Indentation;
   end Generate_Subprogram_Spec;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (E : Node_Id) is
   begin
      Write_Name (Name (Identifier (E)));
      Write_Str  (" : ");
      case Nutils.Parameter_Mode (E) is
         when T_In =>
            Write_Str ("in ");
         when T_Out =>
            Write_Str ("out ");
         when T_Inout =>
            Write_Str ("in out ");
      end case;
      Generate_Type_Spec (Type_Spec (E));
   end Generate_Parameter;

   -----------------------------
   -- Generate_Parameter_List --
   -----------------------------

   procedure Generate_Parameter_List (L : List_Id) is
      N : Node_Id;
   begin
      N_Space := N_Space - 1;
      Write_Indentation;
      N_Space := N_Space + 1;
      N := First_Node (L);
      Write_Str ("(");
      while Present (N) loop
         Generate_Parameter (N);
         N := Next_Node (N);
         if Present (N) then
            Write_Line (";");
            Write_Indentation;
         end if;
      end loop;
      Write_Str (")");
   end Generate_Parameter_List;

   ---------------------------
   -- Generate_Private_Part --
   ---------------------------

   procedure Generate_Private_Part (L : List_Id) is
      pragma Unreferenced (L);
   begin
      null;
   end Generate_Private_Part;

   ---------------------------
   -- Generate_Visible_Part --
   ---------------------------

   procedure Generate_Visible_Part (L : List_Id) is
      N : Node_Id;
   begin
      if Is_Empty (L) then
         return;
      end if;

      N := First_Node (L);
      while Present (N) loop
         case Kind (N) is
            when K_Type_Declaration =>
               Generate_Type_Declaration (N);

            when K_Ada_Subprogram_Spec =>
               Generate_Subprogram_Spec (N);

            when others =>
               DE (Image (Kind (N)) & " not supported");
         end case;
         N := Next_Node (N);
      end loop;
   end Generate_Visible_Part;

   ------------------------------
   -- Generate_Type_Extension --
   ------------------------------

   procedure Generate_Type_Extension (E : Node_Id) is
      Ext : Node_Id;
      Id : Node_Id;
   begin
      Write_Str ("new ");
      if Is_Abstract (E) then
         Write_Str ("abstract ");
      end if;
      Id := Identifier (E);
      case Kind (Id) is
         when K_Ada_Identifier =>
            Generate_Identifier (Id);
         when K_Float .. K_Octet =>
            Write_Str (Image (Image (Base_Type (Id))));
         when K_Type_Declaration =>
            Write_Str (Full_Package_Name (Current_Package) & ".");
            Generate_Identifier (Identifier (Id));
         when others =>
            null;
      end case;
      Ext := Record_Extention_Part (E);
      if Present (Ext) then
         Write_Str (" with ");
         Generate_Record_Type_Spec (Ext);
      end if;
   end Generate_Type_Extension;

   -------------------------------
   -- Generate_Enumeration_Type --
   -------------------------------

   procedure Generate_Enumeration_Type (E : Node_Id) is
      D : Node_Id;
   begin
      Write_Str ("(");
      D := First_Node (Enumerators (E));
      while Present (D) loop
         Generate_Identifier (D);
         D := Next_Node (D);
         if D /= No_Node then
            Write_Str (",");
            W_Eol;
            Write_Indentation;
         end if;
      end loop;
      Write_Str (")");
   end Generate_Enumeration_Type;

   -------------------------
   -- Generate_Identifier --
   -------------------------

   procedure Generate_Identifier (E : Node_Id) is
   begin
      Write_Name (Name (E));
   end Generate_Identifier;

   ----------------------
   -- Generate_Package --
   ----------------------

   procedure Generate_Package (E : Node_Id) is
   begin
      Push_Package (E);
      Generate_Package_Spec (Package_Specification (E));
      Generate_Package_Body (Package_Implementation (E));
      Pop_Package;
   end Generate_Package;

   ---------------------------
   -- Generate_Package_Spec --
   ---------------------------

   procedure Generate_Package_Spec (E : Node_Id) is
   begin
      if E = No_Node then
         return;
      end if;
      Generate_Package_With (Withed_Packages (E));
      Write_Indentation;
      Write_Str  ("package ");
      Write_Str  (Full_Package_Name (Current_Package));
      Write_Line (" is");
      Increment_Indentation;
      Generate_Visible_Part (Visible_Part (E));
      Generate_Private_Part (Private_Part (E));
      Decrement_Indentation;
      Write_Indentation;
      Write_Str  ("end ");
      Write_Str  (Full_Package_Name (Current_Package));
      Write_Line (";");
   end Generate_Package_Spec;

   ---------------------------
   -- Generate_Package_Body --
   ---------------------------

   procedure Generate_Package_Body (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Generate_Package_Body;

   ---------------------------
   -- Generate_Package_With --
   ---------------------------

   procedure Generate_Package_With (L : List_Id) is
      pragma Unreferenced (L);
   begin
      null;
   end Generate_Package_With;

   -------------------------------
   -- Generate_Record_Type_Spec --
   -------------------------------

   procedure Generate_Record_Type_Spec (E : Node_Id) is
   begin
      if Is_Null_Record (E) then
         Write_Str ("null record");
      else
         null; --   In progress...
      end if;
   end Generate_Record_Type_Spec;

   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------

   procedure Generate_Type_Declaration (E : Node_Id) is
   begin
      Generate_Type_Header (Identifier (E));
      Increment_Indentation;
      W_Small_Indent;
      Generate_Type_Spec   (Type_Spec (E));
      Write_Line (";");
      Decrement_Indentation;
   end Generate_Type_Declaration;

   ------------------------
   -- Generate_Type_Spec --
   ------------------------

   procedure Generate_Type_Spec (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Float .. K_Octet =>
            Write_Name (Image (Base_Type (E)));

         when K_Derived_Type_Definition =>
            Generate_Type_Extension (E);

         when K_Ada_Identifier =>
            Generate_Identifier (E);

         when K_Type_Declaration =>
            Write_Str (Full_Package_Name (Current_Package) & ".");
            Write_Name (Name (Identifier (E)));

         when K_Enumeration_Type =>
            Generate_Enumeration_Type (E);

         when K_Array_Type =>
            Generate_Array_Type (E);

         when others =>
            DE (Image (Kind (E)) & " not supported");
      end case;
   end Generate_Type_Spec;

   --------------------------------
   -- Generate_Subprogram_Header --
   --------------------------------

   procedure Generate_Subprogram_Header (E : Node_Id) is
   begin
      Write_Indentation;
      if No (Type_Spec (E)) then
         Write_Str ("procedure ");
      else
         Write_Str ("function ");
      end if;
      Write_Name (Name (Identifier (E)));
   end Generate_Subprogram_Header;

   --------------------------
   -- Generate_Type_Header --
   --------------------------

   procedure Generate_Type_Header (E : Node_Id) is
   begin
      Write_Indentation;
      Write_Str  ("type ");
      Write_Name (Name (E));
      Write_Str  (" is");
   end Generate_Type_Header;

   --------------------
   -- W_Small_Indent --
   --------------------

   procedure W_Small_Indent is
   begin
      W_Eol;
      N_Space := N_Space - 1;
      Write_Indentation;
      N_Space := N_Space + 1;
   end W_Small_Indent;

end Backend.BE_Ada.Generator;
