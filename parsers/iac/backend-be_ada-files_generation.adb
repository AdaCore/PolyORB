with Types; use Types;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
pragma Warnings (off);
with Backend.BE_Ada.Debug; use Backend.BE_Ada.Debug;
pragma Warnings (on);
with Namet; use Namet;
with Errors; use Errors;
with Output; use Output;
with Lexer;
package body Backend.BE_Ada.FILES_Generation is


   procedure Generate_Ada_Function_Spec (E : Node_Id);
   procedure Generate_Ada_Public (L : List_Id);
   procedure Generate_Ada_Private (L : List_Id);
   procedure Generate_Ada_Procedure_Spec (E : Node_Id);
   procedure Generate_Argument (E : Node_Id);
   procedure Generate_Argument_List (L : List_Id);
   procedure Generate_Derived_Type (E : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Identifier (E : Node_Id);
   procedure Generate_Package (E : Node_Id);
   procedure Generate_Package_Spec (E : Node_Id);
   procedure Generate_Package_Body (E : Node_Id);
   procedure Generate_Package_With (L : List_Id);
   procedure Generate_Record_Type_Spec (E : Node_Id);
   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Type_Spec (E : Node_Id);
   procedure Write_Package_Spec_Header (E : Node_Id);
   procedure Write_Function_Spec_Header (E : Node_Id);
   procedure Write_Procedure_Spec_Header (E : Node_Id);
   procedure Write_Type_Declaration  (E : Node_Id);



   -----------------
   --   Generate  --
   -----------------

   procedure Generate (E : List_Id) is
      N : Node_Id;
   begin
      if E = No_List then
         return;
      end if;
      N := First_Node (E);
      while Present (N) loop
         case Kind (N) is
            when K_Ada_Packages =>
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

   -------------------------------------------
   --   Generate Ada Function Specification --
   -------------------------------------------
   procedure Generate_Ada_Function_Spec (E : Node_Id) is
      Arg_List : List_Id;
   begin
      N_Small_Indents := 1;
      Write_Function_Spec_Header (E);
      Arg_List := Argument_List (E);
      if Arg_List /= No_List then
         W_Small_Indents;
         Generate_Argument_List (Arg_List);
      end if;
      W_Small_Indents;
      Write_Str ("return ");
      Generate_Type_Spec (Type_Spec (E));
      Write_Line (";");
   end Generate_Ada_Function_Spec;

   ------------------------------------------
   -- Generate Ada Procedure Specification --
   ------------------------------------------
   procedure Generate_Ada_Procedure_Spec (E : Node_Id) is
      Arg_List : List_Id;
   begin
      N_Small_Indents := 1;
      Write_Procedure_Spec_Header (E);
      Arg_List := Argument_List (E);
      if Arg_List /= No_List then
         W_Small_Indents;
         Generate_Argument_List (Arg_List);
      end if;
      Write_Line (";");

   end Generate_Ada_Procedure_Spec;

   --------------------------
   --   Generate Argument  --
   --------------------------
   procedure Generate_Argument (E : Node_Id) is
      Type_Spec_Node  : Node_Id;
      Arg_Mode        : Mode_Id;
   begin
      Write_Str (Get_Name_String (Name (Identifier (E))) & " : ");
      Arg_Mode := Argument_Mode (E);
      case Arg_Mode is
         when Lexer.Token_Type'Pos (Lexer.T_In) =>
            Write_Str ("in ");
         when Lexer.Token_Type'Pos (Lexer.T_Out) =>
            Write_Str ("out ");
         when Lexer.Token_Type'Pos (Lexer.T_Inout) =>
            Write_Str ("in out ");
         when others =>
            Error_Int (1) := Int (Arg_Mode);
            DE ("Generate Argument : Argument Mode not recognized $");
      end case;
      Type_Spec_Node := Type_Spec (E);
      Generate_Type_Spec (Type_Spec_Node);
   end Generate_Argument;


   ------------------------------
   --   Generate Argument List --
   ------------------------------
   procedure Generate_Argument_List (L : List_Id) is
      N : Node_Id;
   begin
      N_Small_Indents := N_Small_Indents + 1;
      N := First_Node (L);
      Write_Str ("(");
      while Present (N) loop
         Generate_Argument (N);
         N := Next_Node (N);
         if Present (N) then
            Write_Str (";");
            W_Small_Indents;
         end if;
      end loop;
      Write_Str (")");
      N_Small_Indents := N_Small_Indents - 1;
   end Generate_Argument_List;



   -----------------------------
   --   Generate Ada Private  --
   -----------------------------
   procedure Generate_Ada_Private (L : List_Id) is
   begin
      if L = No_List then
         return;
      end if;
      null;
   end Generate_Ada_Private;


   ---------------------------
   --   Generate Ada Public --
   ---------------------------
   procedure Generate_Ada_Public (L : List_Id) is
      N : Node_Id;
   begin
      if L = No_List then
         return;
      end if;
      N := First_Node (L);
      while Present (N) loop
         Write_Eol;
         case Kind (N) is
            when K_Type_Declaration =>
               Generate_Type_Declaration (N);
            when K_Ada_Function_Spec =>
               Generate_Ada_Function_Spec (N);
            when K_Ada_Procedure_Spec =>
               Generate_Ada_Procedure_Spec (N);
            when others =>
               Set_Str_To_Name_Buffer (Image (Kind (N)));
               Error_Name (1) := Name_Find;
               DE ("Generate Ada Public : Not supporting %");
         end case;
         N := Next_Node (N);
      end loop;
   end Generate_Ada_Public;

   -----------------------------
   --   Generate Derived Type --
   -----------------------------
   procedure Generate_Derived_Type (E : Node_Id) is
      Record_Extenstion_Node : Node_Id;
   begin
      Write_Str ("new ");
      if Is_Abstract (E) then
         Write_Str ("abstract ");
      end if;
      Generate_Type_Spec (Identifier (E));
      Record_Extenstion_Node := Record_Extention_Part (E);
      if Record_Extenstion_Node /= No_Node then
         Write_Str (" with ");
         Generate_Record_Type_Spec (Record_Extenstion_Node);
      end if;
   end Generate_Derived_Type;

   --------------------------------
   --  Generate_Enumeration_Type --
   --------------------------------
   procedure Generate_Enumeration_Type (E : Node_Id) is
      Enumerators_List : constant List_Id := Enumerators (E);
      D : Node_Id;
   begin

      if Enumerators_List = No_List then
         return;  -- problem not fixed
      end if;
      N_Small_Indents := N_Small_Indents + 1;
      Write_Str ("(");
      D := First_Node (Enumerators_List);
      while Present (D) loop
         Generate_Identifier (D);
         D := Next_Node (D);
         if D /= No_Node then
            Write_Str (",");
            W_Small_Indents;
         end if;
      end loop;
      Write_Str (")");
      N_Small_Indents := N_Small_Indents - 1;
   end Generate_Enumeration_Type;

   --------------------------
   --  Generate_Identifier --
   --------------------------
   procedure Generate_Identifier (E : Node_Id) is
   begin
      Write_Str
        (Get_Name_String (Name (E)));
   end Generate_Identifier;

   ------------------------
   --   Generate Package --
   ------------------------
   procedure Generate_Package (E : Node_Id) is
      Package_Spec_Node : Node_Id;
      Package_Body_Node : Node_Id;
   begin
      Push_Package (E);
      Package_Spec_Node := Package_Spec (E);
      Generate_Package_Spec (Package_Spec_Node);
      Write_Eol;
      Package_Body_Node := Package_Body (E);
      Generate_Package_Body (Package_Body_Node);
      Pop_Package;
   end Generate_Package;

   ------------------------------
   --   Generate Package Spec  --
   ------------------------------
   procedure Generate_Package_Spec (E : Node_Id) is

      Package_With_List : List_Id := No_List;
      Ada_Public_List : List_Id := No_List;
      Ada_Private_List : List_Id := No_List;
   begin

      if E /= No_Node then
         Package_With_List :=  Package_With (E);
         Ada_Public_List := Ada_Public (E);
         Ada_Private_List := Ada_Private (E);
      end if;
      N_Indents := 0;
      N_Small_Indents := 1;
      Generate_Package_With (Package_With_List);
      Write_Package_Spec_Header (Current_Package);
      N_Indents := N_Indents + 1;
      Generate_Ada_Public (Ada_Public_List);
      Generate_Ada_Private (Ada_Private_List);
      N_Indents := N_Indents - 1;
      W_Indents;
      Write_Line ("end "
                  & Full_Package_Name (Current_Package) & ";");
   end Generate_Package_Spec;


   -----------------------------
   --   Generate Package Body --
   -----------------------------
   procedure Generate_Package_Body (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
      --  W_Indents;
      --  Write_Line ("package body "
      --             & Full_Package_Name (Current_Package) & " is");
      --  W_Indents;
      --  Write_Line ("end "
      --            & Full_Package_Name (Current_Package) & ";");
   end Generate_Package_Body;


   -----------------------------
   --   Generate Package With --
   -----------------------------
   procedure Generate_Package_With (L : List_Id) is
   begin
      if L = No_List then
         return;
      end if;
      null;
   end Generate_Package_With;

   -------------------------------
   -- Generate Record Type Spec --
   -------------------------------
   procedure Generate_Record_Type_Spec (E : Node_Id) is
   begin
      if Is_Null_Record (E) then
         Write_Str ("null record");
      else
         null; --   In progress...
      end if;
   end Generate_Record_Type_Spec;


   ----------------------------------
   --   Generate Type Declaration  --
   ----------------------------------
   procedure Generate_Type_Declaration (E : Node_Id) is
      Type_Identifier : Node_Id;
      Type_Spec_Node : Node_Id;
   begin
      if E = No_Node then
         return;
      end if;
      Type_Identifier := Identifier (E);
      Type_Spec_Node := Type_Spec (E);
      Write_Type_Declaration (Type_Identifier);
      --   This code is juste added to make code
      --   generated identical to idlac output
      Set_Str_To_Name_Buffer ("Ref");
      if Name (Type_Identifier) /= Name_Find then
         W_Small_Indents;
      else
         Write_Str (" ");
      end if;
      Generate_Type_Spec (Type_Spec_Node);
      Write_Line (";");
   end Generate_Type_Declaration;


   --------------------------
   --   Generate Type Spec --
   --------------------------
   procedure Generate_Type_Spec (E : Node_Id) is
   begin
      pragma Warnings (off);
      case Kind (E) is
         when K_Float .. K_Octet =>
            Write_Str (Get_Name_String (Image (Base_Type (E))));
         when K_Derived_Type_Definition =>
            Generate_Derived_Type (E);
         when K_Ada_Identifier =>
            Write_Str (Get_Name_String (Name (E)));
         when K_Type_Declaration =>
            --   Temp solutions.
            Write_Str (Full_Package_Name (Current_Package) & ".");
            Write_Str (Get_Name_String (Name (Identifier (E))));
         when K_Enumeration_Type =>
            Generate_Enumeration_Type (E);
         when others =>
            Set_Str_To_Name_Buffer (Image (Kind (E)));
            Error_Name (1) := Name_Find;
            DE ("Generate Type Spec : Not supporting %");
      end case;
      pragma Warnings (on);
   end Generate_Type_Spec;


   --------------------------------
   --  Write_Package_Spec_Header --
   --------------------------------
   procedure Write_Package_Spec_Header (E : Node_Id) is
   begin
      W_Indents;
      Write_Line ("package "
                  & Full_Package_Name (E) & " is");
   end Write_Package_Spec_Header;

   --------------------------------
   --  Write_Package_Spec_Header --
   --------------------------------
   procedure Write_Function_Spec_Header (E : Node_Id) is
   begin
      W_Indents;
      Write_Str ("function "
                 & Get_Name_String (Name (Identifier (E))));
   end Write_Function_Spec_Header;

   ----------------------------------
   --  Write_Procedure_Spec_Header --
   ----------------------------------
   procedure Write_Procedure_Spec_Header (E : Node_Id) is
   begin
      W_Indents;
      Write_Str ("procedure "
                 & Get_Name_String (Name (Identifier (E))));
   end Write_Procedure_Spec_Header;

   ----------------------------
   -- Write_Type_Declaration --
   ----------------------------
   procedure Write_Type_Declaration  (E : Node_Id) is
   begin
      W_Indents;
      Write_Str ("type " & Get_Name_String (Name (E)) & " is");
   end Write_Type_Declaration;
end Backend.BE_Ada.FILES_Generation;
