with Backend.BE_A.Debug;  use Backend.BE_A.Debug;
with Backend.BE_A.Nodes;  use Backend.BE_A.Nodes;
with Backend.BE_A.Nutils; use Backend.BE_A.Nutils;

with Errors; use Errors;
with Lexer;  use Lexer;
with Namet;  use Namet;
with Output; use Output;
with Types;  use Types;

package body Backend.BE_A.Generator is

   procedure Generate_Component_Declaration (N : Node_Id);
   procedure Generate_Defining_Identifier (E : Node_Id);
   procedure Generate_Derived_Type_Definition (N : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Enumeration_Type_Definition (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_IDL_Unit_Packages (N : Node_Id);
   procedure Generate_Package_Declaration (N : Node_Id);
   procedure Generate_Package_Implementation (N : Node_Id);
   procedure Generate_Package_Specification (N : Node_Id);
   procedure Generate_Parameter (E : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Record_Definition (N : Node_Id);
   procedure Generate_Record_Type_Definition (N : Node_Id);
   procedure Generate_Subprogram_Specification (N : Node_Id);
   procedure Generate_Type_Spec (E : Node_Id);

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_IDL_Unit =>
            Generate_IDL_Unit_Packages (N);

         when K_Package_Declaration =>
            Generate_Package_Declaration (N);

         when K_Package_Specification =>
            Generate_Package_Specification (N);

         when K_Package_Implementation =>
            Generate_Package_Implementation (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_Enumeration_Type_Definition =>
            Generate_Enumeration_Type_Definition (N);

         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Record_Type_Definition =>
            Generate_Record_Type_Definition (N);

         when K_Record_Definition =>
            Generate_Record_Definition (N);

         when K_Component_Declaration =>
            Generate_Component_Declaration (N);

         when K_Subprogram_Specification =>
            Generate_Subprogram_Specification (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Float .. K_Octet =>
            Write_Name (Image (Base_Type (N)));

         when others =>
            null;
      end case;
   end Generate;

   ------------------------------------
   -- Generate_Component_Declaration --
   ------------------------------------

   procedure Generate_Component_Declaration (N : Node_Id) is
   begin
      Write_Indentation;
      Generate (Defining_Identifier (N));
      Write_Str (" : ");
      Generate (Subtype_Indication (N));
      Write_Line (";");
   end Generate_Component_Declaration;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (E : Node_Id) is
   begin
      Write_Name (Name (E));
   end Generate_Defining_Identifier;

   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------

   procedure Generate_Derived_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write_Str ("abstract ");
      end if;
      Write_Str ("new ");
      Generate (Subtype_Indication (N));
      R := Record_Extension_Part (N);
      if Present (R) then
         Write_Str (" with ");
         Generate (Record_Extension_Part (N));
      end if;
   end Generate_Derived_Type_Definition;

   -------------------------------
   -- Generate_Enumeration_Type --
   -------------------------------

   procedure Generate_Enumeration_Type (E : Node_Id) is
      N : Node_Id;

   begin
      Write_Indentation;
      Write_Str ("(");
      N := First_Node (Enumeration_Literals (E));
      while Present (N) loop
         Generate_Defining_Identifier (N);
         N := Next_Node (N);
         if Present (N) then
            Write_Str (",");
            Write_Indentation;
         end if;
      end loop;
      Write_Str (")");
   end Generate_Enumeration_Type;

   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------

   procedure Generate_Enumeration_Type_Definition (N : Node_Id) is
      E : Node_Id;

   begin
      Write_Str ("(");
      E := First_Node (Enumeration_Literals (N));
      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Str (", ");
      end loop;
      Write_Str (")");
   end Generate_Enumeration_Type_Definition;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
   begin
      Write_Indentation;
      Write_Str ("type ");
      Write_Name (Name (Defining_Identifier (N)));
      Write_Str (" is ");
      Generate  (Type_Definition (N));
   end Generate_Full_Type_Declaration;

   --------------------------------
   -- Generate_IDL_Unit_Packages --
   --------------------------------

   procedure Generate_IDL_Unit_Packages (N : Node_Id) is
      P : Node_Id := First_Node (Packages (N));

   begin
      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;
   end Generate_IDL_Unit_Packages;

   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------

   procedure Generate_Package_Declaration (N : Node_Id) is
   begin
      Generate (Package_Specification (N));
      Generate (Package_Implementation (N));
   end Generate_Package_Declaration;

   -------------------------------------
   -- Generate_Package_Implementation --
   -------------------------------------

   procedure Generate_Package_Implementation (N : Node_Id) is
      pragma Unreferenced (N);
   begin
      null;
   end Generate_Package_Implementation;

   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------

   procedure Generate_Package_Specification (N : Node_Id) is
      P : Node_Id;
   begin
      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;
      Write_Indentation;
      Write_Str  ("package ");
      Write_Name (Name (Defining_Identifier (Package_Declaration (N))));
      Write_Line (" is");
      Increment_Indentation;
      P := First_Node (Visible_Part (N));
      while Present (P) loop
         Generate (P);
         Write_Line (";");
         P := Next_Node (P);
      end loop;
      if not Is_Empty (Private_Part (N)) then
         Write_Indentation;
         Write_Line ("private");
         P := First_Node (Private_Part (N));
         while Present (P) loop
            Generate (P);
            Write_Line (";");
            P := Next_Node (P);
         end loop;
      end if;
      Decrement_Indentation;
      Write_Indentation;
      Write_Str  ("end ");
      Write_Name (Name (Defining_Identifier (Package_Declaration (N))));
      Write_Line (";");
   end Generate_Package_Specification;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (E : Node_Id) is
   begin
      Write_Name (Name (Defining_Identifier (E)));
      Write_Str  (" : ");
      case Nutils.Parameter_Mode (E) is
         when T_In =>
            Write_Str ("in ");
         when T_Out =>
            Write_Str ("out ");
         when T_Inout =>
            Write_Str ("in out ");
      end case;
      Generate_Type_Spec (Parameter_Type (E));
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

   --------------------------------
   -- Generate_Record_Definition --
   --------------------------------

   procedure Generate_Record_Definition (N : Node_Id) is
      L : constant List_Id := Component_List (N);
      C : Node_Id;

   begin
      if Is_Empty (L) then
         Write_Str ("null record");
      else
         Increment_Indentation;
         Write_Indentation;
         Write_Line ("record");
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Generate (C);
            C := Next_Node (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
         Write_Line ("end record");
         Decrement_Indentation;
      end if;
   end Generate_Record_Definition;

   -------------------------------------
   -- Generate_Record_Type_Definition --
   -------------------------------------

   procedure Generate_Record_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write_Str ("abstract ");
      end if;
      if Is_Tagged_Type (N) then
         Write_Str ("tagged ");
      end if;
      if Is_Limited_Type (N) then
         Write_Str ("limited ");
      end if;
      R := Record_Definition (N);
      if Present (R) then
         Generate (R);
      end if;
   end Generate_Record_Type_Definition;

   ---------------------------------------
   -- Generate_Subprogram_Specification --
   ---------------------------------------

   procedure Generate_Subprogram_Specification (N : Node_Id) is
      P : List_Id;
      T : Node_Id;

   begin
      T := Return_Type (N);
      Write_Indentation;
      if Present (T) then
         Write_Str ("function ");
      else
         Write_Str ("procedure ");
      end if;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Eol;
      Increment_Indentation;
      P := Parameter_Profile (N);
      if not Is_Empty (P) then
         Generate_Parameter_List (P);
      end if;
      T := Return_Type (N);
      if Present (T) then
         Write_Eol;
         Write_Indentation;
         Write_Str ("return ");
         Generate_Type_Spec (T);
      end if;
      Decrement_Indentation;
   end Generate_Subprogram_Specification;

   ------------------------
   -- Generate_Type_Spec --
   ------------------------

   procedure Generate_Type_Spec (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (E);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (E);

         when K_Full_Type_Declaration =>
            Write_Name (Name (Defining_Identifier (E)));

         when K_Enumeration_Type =>
            Generate_Enumeration_Type (E);

         when others =>
            DE (Image (Kind (E)) & " not supported");
      end case;
   end Generate_Type_Spec;

end Backend.BE_A.Generator;
