with GNAT.Command_Line; use GNAT.Command_Line;

with Types;     use Types;
with Output;    use Output;

with Backend.BE_Types.Utils; use Backend.BE_Types.Utils;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;

package body Backend.BE_Types is

   Print : Boolean := False;

   procedure Generate (E : Node_Id; L : in out List);
   procedure Generate_Abstract_Value_Declaration
      (E : Node_Id; L : in out List);
   procedure Generate_Attribute_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Base_Type (E : Node_Id; L : in out List);
   procedure Generate_Complex_Declarator (E : Node_Id; L : in out List);
   procedure Generate_Constant_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Element (E : Node_Id; L : in out List);
   procedure Generate_Enumeration_Type (E : Node_Id; L : in out List);
   procedure Generate_Exception_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Fixed_Point_Type (E : Node_Id; L : in out List);
   procedure Generate_Initializer_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Interface_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Member (E : Node_Id; L : in out List);
   procedure Generate_Module (E : Node_Id; L : in out List);
   procedure Generate_Operation_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Parameter_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Simple_Declarator (E : Node_Id; L : in out List);
   procedure Generate_Sequence_Type (E : Node_Id; L : in out List);
   procedure Generate_State_Member (E : Node_Id; L : in out List);
   procedure Generate_String_Type (E : Node_Id; L : in out List);
   procedure Generate_WString_Type (E : Node_Id; L : in out List);
   procedure Generate_Structure_Type (E : Node_Id; L : in out List);
   procedure Generate_Switch_Alternative (E : Node_Id; L : in out List);
   procedure Generate_Type_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Union_Type (E : Node_Id; L : in out List);
   procedure Generate_Value_Declaration (E : Node_Id; L : in out List);
   procedure Generate_Value_Box_Declaration (E : Node_Id; L : in out List);

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      loop
         case Getopt ("p") is
            when 'p' =>
               Print := True;

            when ASCII.NUL =>
               exit;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Configure;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
      List_Of_Types : List;
   begin
      Generate (E, List_Of_Types);
      if Print then
         Print_List (List_Of_Types);
      end if;
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id; L : in out List) is
   begin
      case Kind (E) is
         when K_Abstract_Value_Declaration =>
            Generate_Abstract_Value_Declaration (E, L);

         when K_Attribute_Declaration =>
            Generate_Attribute_Declaration (E, L);

         when K_Complex_Declarator =>
            Generate_Complex_Declarator (E, L);

         when K_Constant_Declaration =>
            Generate_Constant_Declaration (E, L);

         when K_Element =>
            Generate_Element (E, L);

         when K_Enumeration_Type =>
            Generate_Enumeration_Type (E, L);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (E, L);

         when K_Fixed_Point_Type =>
            Generate_Fixed_Point_Type (E, L);

         when K_Initializer_Declaration =>
            Generate_Initializer_Declaration (E, L);

         when K_Interface_Declaration =>
            Generate_Interface_Declaration (E, L);

         when K_Member =>
            Generate_Member (E, L);

         when K_Module =>
            Generate_Module (E, L);

         when K_Operation_Declaration =>
            Generate_Operation_Declaration (E, L);

         when K_Parameter_Declaration =>
            Generate_Parameter_Declaration (E, L);

         when K_Simple_Declarator =>
            Generate_Simple_Declarator (E, L);

         when K_Sequence_Type =>
            Generate_Sequence_Type (E, L);

         when K_Specification =>
            Generate_Module (E, L);

         when K_State_Member =>
            Generate_State_Member (E, L);

         when K_String_Type =>
            Generate_String_Type (E, L);

         when K_Wide_String_Type =>
            Generate_WString_Type (E, L);

         when K_Structure_Type =>
            Generate_Structure_Type (E, L);

         when K_Switch_Alternative =>
            Generate_Switch_Alternative (E, L);

         when K_Type_Declaration =>
            Generate_Type_Declaration (E, L);

         when K_Union_Type =>
            Generate_Union_Type (E, L);

         when K_Value_Declaration =>
            Generate_Value_Declaration (E, L);

         when K_Value_Box_Declaration =>
            Generate_Value_Box_Declaration (E, L);

         when K_Float .. K_Value_Base =>
            Generate_Base_Type (E, L);

         when others =>
            Dummy (E);
      end case;
   end Generate;

   ----------------------------------------
   -- Generate_Abstract_Value_Declaration --
   ----------------------------------------

   procedure Generate_Abstract_Value_Declaration
      (E : Node_Id; L : in out List) is
   begin
      Generate_Value_Declaration (E, L);
   end Generate_Abstract_Value_Declaration;

   -----------------------------------
   -- Generate_Attribute_Declaration --
   -----------------------------------

   procedure Generate_Attribute_Declaration (E : Node_Id; L : in out List) is
   begin
      Generate (Type_Spec (E), L);
   end Generate_Attribute_Declaration;

   ------------------------
   -- Generate_Base_Type --
   ------------------------

   procedure Generate_Base_Type (E : Node_Id; L : in out List) is
   begin
      case Kind (E) is
         when K_Float =>
            Insert (L, Tk_Float);
         when K_Double =>
            Insert (L, Tk_Double);
         when K_Long_Double =>
            Insert (L, Tk_Longdouble);
         when K_Short =>
            Insert (L, Tk_Short);
         when K_Long =>
            Insert (L, Tk_Long);
         when K_Long_Long =>
            Insert (L, Tk_Longlong);
         when K_Unsigned_Short =>
            Insert (L, Tk_Ushort);
         when K_Unsigned_Long =>
            Insert (L, Tk_Ulong);
         when K_Unsigned_Long_Long =>
            Insert (L, Tk_Ulonglong);
         when K_Char =>
            Insert (L, Tk_Char);
         when K_Wide_Char =>
            Insert (L, Tk_Widechar);
         when K_String =>
            Insert (L, Tk_String);
         when K_Wide_String =>
            Insert (L, Tk_Wstring);
         when K_Boolean =>
            Insert (L, Tk_Boolean);
         when K_Octet =>
            Insert (L, Tk_Octet);
         when K_Object =>
            Insert (L, Tk_Objref);
         when K_Any =>
            Insert (L, Tk_Any);
         when K_Void =>
            Insert (L, Tk_Void);
         when K_Value_Base =>
            Insert (L, Tk_Value);
         when others =>
            raise Program_Error;
      end case;
   end Generate_Base_Type;

   ---------------------------------
   -- Generate_Complex_Declarator --
   ---------------------------------

   procedure Generate_Complex_Declarator (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Array);
   end Generate_Complex_Declarator;

   -----------------------------------
   -- Generate_Constant_Declaration --
   -----------------------------------

   procedure Generate_Constant_Declaration (E : Node_Id; L : in out List) is
   begin
      Generate (Type_Spec (E), L);
   end Generate_Constant_Declaration;

   ----------------------
   -- Generate_Element --
   ----------------------

   procedure Generate_Element (E : Node_Id; L : in out List) is
   begin
      Generate (Type_Spec (E), L);
   end Generate_Element;

   -------------------------------
   -- Generate_Enumeration_Type --
   -------------------------------

   procedure Generate_Enumeration_Type (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Enum);
   end Generate_Enumeration_Type;

   ------------------------------------
   -- Generate_Exception_Declaration --
   ------------------------------------

   procedure Generate_Exception_Declaration (E : Node_Id; L : in out List) is
      C : Node_Id;
      LL : List_Id;
   begin
      Insert (L, Tk_Except);
      LL := Members (E);
      C := First_Entity (LL);
      while Present (C) loop
         Generate (C, L);
         C := Next_Entity (C);
      end loop;
   end Generate_Exception_Declaration;

   -------------------------------
   -- Generate_Fixed_Point_Type --
   -------------------------------

   procedure Generate_Fixed_Point_Type (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Fixed);
   end Generate_Fixed_Point_Type;

   --------------------------------------
   -- Generate_Initializer_Declaration --
   --------------------------------------

   procedure Generate_Initializer_Declaration (E : Node_Id; L : in out List) is
   begin
      Generate_Operation_Declaration (E, L);
   end Generate_Initializer_Declaration;

   ------------------------------------
   -- Generate_Interface_Declaration --
   ------------------------------------

   procedure Generate_Interface_Declaration (E : Node_Id; L : in out List) is
      F : Node_Id := No_Node;
      B : List_Id;
   begin
      B := Interface_Body (E);
      if not Is_Empty (B) then
         F := First_Entity (B);
         while Present (F) loop
            Generate (F, L);
            F := Next_Entity (F);
         end loop;
      end if;
   end Generate_Interface_Declaration;

   ---------------------
   -- Generate_Member --
   ---------------------

   procedure Generate_Member (E : Node_Id; L : in out List) is
   begin
      Generate (Type_Spec (E), L);
   end Generate_Member;

   ---------------------
   -- Generate_Module --
   ---------------------

   procedure Generate_Module (E : Node_Id; L : in out List) is
      C : Node_Id;
      LL : List_Id;
   begin
      LL := Definitions (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         while Present (C) loop
            Generate (C, L);
            C := Next_Entity (C);
         end loop;
      end if;
   end Generate_Module;

   ------------------------------------
   -- Generate_Operation_Declaration --
   ------------------------------------

   procedure Generate_Operation_Declaration (E : Node_Id; L : in out List) is
      C : Node_Id;
      LL : List_Id;
   begin
      if Kind (E) /= K_Initializer_Declaration then
         Generate (Type_Spec (E), L);
      end if;

      LL := Parameters (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         loop
            Generate (C, L);
            C := Next_Entity (C);
            exit when No (C);
         end loop;
      end if;
   end Generate_Operation_Declaration;

   ------------------------------------
   -- Generate_Parameter_Declaration --
   ------------------------------------

   procedure Generate_Parameter_Declaration (E : Node_Id; L : in out List) is
   begin
      Generate (Type_Spec (E), L);
   end Generate_Parameter_Declaration;

   ---------------------------
   -- Generate_Sequence_Type --
   ---------------------------

   procedure Generate_Sequence_Type (E : Node_Id; L : in out List) is
   begin
      Insert (L, Tk_Sequence);
      Generate (Type_Spec (E), L);
   end Generate_Sequence_Type;

   --------------------------------
   -- Generate_Simple_Declarator --
   --------------------------------

   procedure Generate_Simple_Declarator (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Alias);
   end Generate_Simple_Declarator;

   ---------------------------
   -- Generate_State_Member --
   ---------------------------

   procedure Generate_State_Member (E : Node_Id; L : in out List) is
   begin
      Generate_Member (E, L);
   end Generate_State_Member;

   --------------------------
   -- Generate_String_Type --
   --------------------------

   procedure Generate_String_Type (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_String);
   end Generate_String_Type;

   ---------------------------
   -- Generate_WString_Type --
   ---------------------------

   procedure Generate_WString_Type (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Wstring);
   end Generate_WString_Type;

   -----------------------------
   -- Generate_Structure_Type --
   -----------------------------

   procedure Generate_Structure_Type (E : Node_Id; L : in out List) is
      LL : List_Id;
      C : Node_Id;

   begin
      Insert (L, Tk_Struct);
      LL := Members (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         while Present (C) loop
            Generate (C, L);
            C := Next_Entity (C);
         end loop;
      end if;
   end Generate_Structure_Type;

   ---------------------------------
   -- Generate_Switch_Alternative --
   ---------------------------------

   procedure Generate_Switch_Alternative (E : Node_Id; L : in out List) is
      LL : Node_Id := First_Entity (Labels (E));

   begin
      while Present (LL) loop
         Generate (LL, L);
         LL := Next_Entity (LL);
      end loop;
      Generate (Element (E), L);
   end Generate_Switch_Alternative;

   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------

   procedure Generate_Type_Declaration (E : Node_Id; L : in out List) is
      D : Node_Id := First_Entity (Declarators (E));

   begin
      Generate (Type_Spec (E), L);
      loop
         Generate (D, L);
         D := Next_Entity (D);
         exit when No (D);
      end loop;
   end Generate_Type_Declaration;

   -------------------------
   -- Generate_Union_Type --
   -------------------------

   procedure Generate_Union_Type (E : Node_Id; L : in out List) is
      N : Node_Id := First_Entity (Switch_Type_Body (E));

   begin
      Insert (L, Tk_Union);
      Generate (Switch_Type_Spec (E), L);
      while Present (N) loop
         Generate (N, L);
         N := Next_Entity (N);
      end loop;
   end Generate_Union_Type;

   ------------------------------------
   -- Generate_Value_Box_Declaration --
   ------------------------------------

   procedure Generate_Value_Box_Declaration (E : Node_Id; L : in out List) is
      pragma Unreferenced (E);
   begin
      Insert (L, Tk_Valuebox);
   end Generate_Value_Box_Declaration;

   --------------------------------
   -- Generate_Value_Declaration --
   --------------------------------

   procedure Generate_Value_Declaration (E : Node_Id; L : in out List) is
      S : constant Node_Id := Value_Spec (E);
      N : Node_Id;
      LL : List_Id;

   begin
      Generate (Identifier (E), L);
      LL := Value_Names (S);
      if not Is_Empty (LL) then
         N := First_Entity (LL);
         loop
            Generate (N, L);
            N := Next_Entity (N);
            exit when No (N);
         end loop;
      end if;
      LL := Interface_Names (S);
      if not Is_Empty (LL) then
         N := First_Entity (LL);
         loop
            Generate (N, L);
            N := Next_Entity (N);
            exit when No (N);
         end loop;
      end if;

      LL := Value_Body (E);
      if not Is_Empty (LL) then
         N := First_Entity (LL);
         while Present (N) loop
            Generate (N, L);
            N := Next_Entity (N);
         end loop;
      end if;
   end Generate_Value_Declaration;

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Str (Hdr);
      Write_Str ("-p       Print the list generate");
      Write_Eol;
   end Usage;

end Backend.BE_Types;
