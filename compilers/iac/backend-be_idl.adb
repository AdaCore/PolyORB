------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       B A C K E N D . B E _ I D L                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Lexer;     use Lexer;
with Namet;     use Namet;
with Output;    use Output;
with Values;    use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;
with Frontend.Debug;

with Backend.BE_CORBA_Ada.Expand;

package body Backend.BE_IDL is

   Already_Expanded    : Boolean := False;

   procedure Generate (V : Value_Id);
   procedure Generate_Abstract_Value_Declaration (E : Node_Id);
   procedure Generate_Attribute_Declaration (E : Node_Id);
   procedure Generate_Base_Type (E : Node_Id);
   procedure Generate_Case_Label (E : Node_Id);
   procedure Generate_Complex_Declarator (E : Node_Id);
   procedure Generate_Constant_Declaration (E : Node_Id);
   procedure Generate_Element (E : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Enumerator (E : Node_Id);
   procedure Generate_Exception_Declaration (E : Node_Id);
   procedure Generate_Expression (E : Node_Id);
   procedure Generate_Fixed_Point_Type (E : Node_Id);
   procedure Generate_Forward_Interface_Declaration (E : Node_Id);
   procedure Generate_Forward_Structure_Type (E : Node_Id);
   procedure Generate_Forward_Union_Type (E : Node_Id);
   procedure Generate_Identifier (E : Node_Id);
   procedure Generate_Import (E : Node_Id);
   procedure Generate_Initializer_Declaration (E : Node_Id);
   procedure Generate_Interface_Declaration (E : Node_Id);
   procedure Generate_Literal (E : Node_Id);
   procedure Generate_Member (E : Node_Id);
   procedure Generate_Module (E : Node_Id);
   procedure Generate_Operation_Declaration (E : Node_Id);
   procedure Generate_Native_Type (E : Node_Id);
   procedure Generate_Parameter_Declaration (E : Node_Id);
   procedure Generate_Pragma (E : Node_Id);
   procedure Generate_Scoped_Name (E : Node_Id);
   procedure Generate_Simple_Declarator (E : Node_Id);
   procedure Generate_Sequence_Type (E : Node_Id);
   procedure Generate_State_Member (E : Node_Id);
   procedure Generate_String_Type (E : Node_Id);
   procedure Generate_Structure_Type (E : Node_Id);
   procedure Generate_Switch_Alternative (E : Node_Id);
   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Type_Id_Declaration (E : Node_Id);
   procedure Generate_Type_Prefix_Declaration (E : Node_Id);
   procedure Generate_Union_Type (E : Node_Id);
   procedure Generate_Value_Declaration (E : Node_Id);
   procedure Generate_Value_Box_Declaration (E : Node_Id);
   procedure Generate_Value_Forward_Declaration (E : Node_Id);

   --  This procedure generates a semi-colon in general cases. After a #pragma
   --  the semi-colon is not generated
   procedure Generate_Statement_Delimiter (E : Node_Id);

   procedure Write_Line (T : Token_Type);

   --------------
   -- Generate --
   --------------

   procedure Generate (V : Value_Id)
   is
      Val  : Value_Type := Value (V);
      Base : Unsigned_Short_Short := 0;
   begin
      if Val.K in K_Short .. K_Unsigned_Long_Long
        or else Val.K = K_Octet
      then
         Base := Val.Base;
         if Default_Base /= 0 then
            Val.Base := Unsigned_Short_Short (Default_Base);
         end if;
         Set_Value (V, Val);
      end if;
      Write_Str (Image (V));
      if Base /= 0 then
         Val.Base := Base;
         Set_Value (V, Val);
      end if;
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
   begin
      if Expand_Tree and then not Already_Expanded then
         Backend.BE_CORBA_Ada.Expand.Expand (E);
         Already_Expanded := True;
      end if;
      if Print_IDL_Tree then
         Frontend.Debug.W_Node_Id (E);
      else
         case Kind (E) is
            when K_Abstract_Value_Declaration =>
               Generate_Abstract_Value_Declaration (E);

            when K_Attribute_Declaration =>
               Generate_Attribute_Declaration (E);

            when K_Case_Label =>
               Generate_Case_Label (E);

            when K_Complex_Declarator =>
               Generate_Complex_Declarator (E);

            when K_Constant_Declaration =>
               Generate_Constant_Declaration (E);

            when K_Element =>
               Generate_Element (E);

            when K_Enumerator =>
               Generate_Enumerator (E);

            when K_Enumeration_Type =>
               Generate_Enumeration_Type (E);

            when K_Exception_Declaration =>
               Generate_Exception_Declaration (E);

            when K_Expression =>
               Generate_Expression (E);

            when K_Fixed_Point_Type =>
               Generate_Fixed_Point_Type (E);

            when K_Forward_Interface_Declaration =>
               Generate_Forward_Interface_Declaration (E);

            when K_Forward_Structure_Type =>
               Generate_Forward_Structure_Type (E);

            when K_Forward_Union_Type =>
               Generate_Forward_Union_Type (E);

            when K_Import =>
               Generate_Import (E);

            when K_Initializer_Declaration =>
               Generate_Initializer_Declaration (E);

            when K_Interface_Declaration =>
               Generate_Interface_Declaration (E);

            when K_Integer_Literal .. K_Boolean_Literal =>
               Generate_Literal (E);

            when K_Member =>
               Generate_Member (E);

            when K_Module =>
               Generate_Module (E);

            when K_Operation_Declaration =>
               Generate_Operation_Declaration (E);

            when K_Native_Type =>
               Generate_Native_Type (E);

            when K_Parameter_Declaration =>
               Generate_Parameter_Declaration (E);

            when K_Pragma =>
               Generate_Pragma (E);

            when K_Scoped_Name =>
               Generate_Scoped_Name (E);

            when K_Simple_Declarator =>
               Generate_Simple_Declarator (E);

            when K_Sequence_Type =>
               Generate_Sequence_Type (E);

            when K_Specification =>
               Generate_Module (E);

            when K_State_Member =>
               Generate_State_Member (E);

            when K_String_Type | K_Wide_String_Type =>
               Generate_String_Type (E);

            when K_Structure_Type =>
               Generate_Structure_Type (E);

            when K_Switch_Alternative =>
               Generate_Switch_Alternative (E);

            when K_Type_Declaration =>
               Generate_Type_Declaration (E);

            when K_Type_Id_Declaration =>
               Generate_Type_Id_Declaration (E);

            when K_Type_Prefix_Declaration =>
               Generate_Type_Prefix_Declaration (E);

            when K_Union_Type =>
               Generate_Union_Type (E);

            when K_Value_Declaration =>
               Generate_Value_Declaration (E);

            when K_Value_Box_Declaration =>
               Generate_Value_Box_Declaration (E);

            when K_Value_Forward_Declaration =>
               Generate_Value_Forward_Declaration (E);

            when K_Float .. K_Value_Base =>
               Generate_Base_Type (E);

            when K_Identifier =>
               Generate_Identifier (E);

            when others =>
               Dummy (E);
         end case;
      end if;
   end Generate;

   ----------------------------------------
   -- Generate_Abstract_Value_Declaration --
   ----------------------------------------

   procedure Generate_Abstract_Value_Declaration (E : Node_Id) is
   begin
      Write (T_Abstract);
      Write_Space;
      Generate_Value_Declaration (E);
   end Generate_Abstract_Value_Declaration;

   -----------------------------------
   -- Generate_Attribute_Declaration --
   -----------------------------------

   procedure Generate_Attribute_Declaration (E : Node_Id) is
      D : Node_Id := First_Entity (Declarators (E));
      L : List_Id;
      C : Node_Id;

   begin
      if Is_Readonly (E) then
         Write (T_Readonly);
         Write_Space;
      end if;
      Write (T_Attribute);
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (Identifier (D));
         D := Next_Entity (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;

      L := Getter_Exceptions (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Get_Raises);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Entity (L);
         loop
            Generate (C);
            C := Next_Entity (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;

      L := Setter_Exceptions (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Set_Raises);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Entity (L);
         loop
            Generate (C);
            C := Next_Entity (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;
   end Generate_Attribute_Declaration;

   ------------------------
   -- Generate_Base_Type --
   ------------------------

   procedure Generate_Base_Type (E : Node_Id) is
   begin
      Write_Name (Image (Base_Type (E)));
   end Generate_Base_Type;

   -------------------------
   -- Generate_Case_Label --
   -------------------------

   procedure Generate_Case_Label (E : Node_Id) is
      X : constant Node_Id := Expression (E);

   begin
      if No (X) then
         Write (T_Default);
      else
         Write (T_Case);
         Write_Space;
         if Kind (X) = K_Scoped_Name then
            Generate (X);
         else
            Generate (Value (E));
         end if;
      end if;
      Write_Space;
      Write (T_Colon);
   end Generate_Case_Label;

   ---------------------------------
   -- Generate_Complex_Declarator --
   ---------------------------------

   procedure Generate_Complex_Declarator (E : Node_Id) is
      C : Node_Id;

   begin
      Generate (Identifier (E));

      --  The array sizes attribute is never empty

      Write (T_Left_Bracket);
      C := First_Entity (Array_Sizes (E));
      loop
         Generate (C);
         C := Next_Entity (C);
         exit when No (C);
         Write (T_Right_Bracket);
         Write (T_Left_Bracket);
      end loop;
      Write (T_Right_Bracket);
   end Generate_Complex_Declarator;

   -----------------------------------
   -- Generate_Constant_Declaration --
   -----------------------------------

   procedure Generate_Constant_Declaration (E : Node_Id) is
   begin
      Write (T_Const);
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write (T_Equal);
      Write_Space;
      Generate (Value (E));
   end Generate_Constant_Declaration;

   ----------------------
   -- Generate_Element --
   ----------------------

   procedure Generate_Element (E : Node_Id) is
   begin
      Generate (Type_Spec (E));
      Write_Space;
      Generate (Declarator (E));
   end Generate_Element;

   -------------------------------
   -- Generate_Enumeration_Type --
   -------------------------------

   procedure Generate_Enumeration_Type (E : Node_Id) is
      C : Node_Id;

   begin
      Write (T_Enum);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write_Line (T_Left_Brace);
      Increment_Indentation;
      C := First_Entity (Enumerators (E));
      loop
         Write_Indentation;
         Generate (C);
         C := Next_Entity (C);
         exit when No (C);
         Write_Line (T_Comma);
      end loop;
      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write (T_Right_Brace);
   end Generate_Enumeration_Type;

   -------------------------
   -- Generate_Enumerator --
   -------------------------

   procedure Generate_Enumerator (E : Node_Id) is
   begin
      Generate (Identifier (E));
   end Generate_Enumerator;

   ------------------------------------
   -- Generate_Exception_Declaration --
   ------------------------------------

   procedure Generate_Exception_Declaration (E : Node_Id) is
      C : Node_Id;
      L : List_Id;

   begin
      Write (T_Exception);
      Write_Space;
      Generate (Identifier (E));
      L := Members (E);
      Write_Space;
      Write_Line (T_Left_Brace);
      Increment_Indentation;
      C := First_Entity (L);
      while Present (C) loop
         Write_Indentation;
         Generate (C);
         Generate_Statement_Delimiter (C);
         C := Next_Entity (C);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (T_Right_Brace);
   end Generate_Exception_Declaration;

   ------------------------
   -- Generate_Expression --
   ------------------------

   procedure Generate_Expression (E : Node_Id) is
      C : Node_Id;

   begin
      C := Left_Expr (E);
      if Present (C) then
         Generate (C);
      end if;
      Write_Space;
      Write (Token_Type'Val (Operator (E)));
      Write_Space;
      C := Right_Expr (E);
      if Present (C) then
         Generate (C);
      end if;
   end Generate_Expression;

   -------------------------------
   -- Generate_Fixed_Point_Type --
   -------------------------------

   procedure Generate_Fixed_Point_Type (E : Node_Id) is
   begin
      Write (T_Fixed);
      Write (T_Less);
      Write_Int (Int (N_Total (E)));
      Write (T_Comma);
      Write_Int (Int (N_Scale (E)));
      Write (T_Greater);
   end Generate_Fixed_Point_Type;

   --------------------------------------------
   -- Generate_Forward_Interface_Declaration --
   --------------------------------------------

   procedure Generate_Forward_Interface_Declaration (E : Node_Id) is
   begin
      Write (T_Interface);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Interface_Declaration;

   -------------------------------------
   -- Generate_Forward_Structure_Type --
   -------------------------------------

   procedure Generate_Forward_Structure_Type (E : Node_Id) is
   begin
      Write (T_Struct);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Structure_Type;

   ---------------------------------
   -- Generate_Forward_Union_Type --
   ---------------------------------

   procedure Generate_Forward_Union_Type (E : Node_Id) is
   begin
      Write (T_Union);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Union_Type;

   -------------------------
   -- Generate_Identifier --
   -------------------------

   procedure Generate_Identifier (E : Node_Id) is
   begin
      Write_Name (IDL_Name (E));
   end Generate_Identifier;

   ---------------------
   -- Generate_Import --
   ---------------------

   procedure Generate_Import (E : Node_Id) is
      procedure Generate_Imported_Scope (Entity : Node_Id);

      -----------------------------
      -- Generate_Imported_Scope --
      -----------------------------

      procedure Generate_Imported_Scope (Entity : Node_Id) is
         pragma Assert (Kind (Entity) = K_Scoped_Name);

         Parent : constant Node_Id := Parent_Entity (Entity);
      begin
         if Present (Parent)
           and then IDL_Name (Identifier (Parent)) /= No_Name
         then
            Generate_Imported_Scope (Parent);
            Write (T_Colon_Colon);
         end if;
         Write_Name (IDL_Name (Identifier (Entity)));
      end Generate_Imported_Scope;

      S : Node_Id;
   begin
      Write (T_Import);
      Write_Space;
      S := Imported_Scope (E);
      Write (T_Colon_Colon);
      Generate_Imported_Scope (S);
   end Generate_Import;

   --------------------------------------
   -- Generate_Initializer_Declaration --
   --------------------------------------

   procedure Generate_Initializer_Declaration (E : Node_Id) is
   begin
      Write (T_Factory);
      Write_Space;
      Generate_Operation_Declaration (E);
   end Generate_Initializer_Declaration;

   ------------------------------------
   -- Generate_Interface_Declaration --
   ------------------------------------

   procedure Generate_Interface_Declaration (E : Node_Id) is
      F : Node_Id := No_Node;
      I : Node_Id;
      S : List_Id;
      B : List_Id;

   begin
      if Is_Abstract_Interface (E) then
         Write (T_Abstract);
         Write_Space;
      end if;

      if Is_Local_Interface (E) then
         Write (T_Local);
         Write_Space;
      end if;

      Write (T_Interface);
      Write_Space;
      Generate (Identifier (E));

      --  Generate interface names, enter them in scope and make them
      --  visible.

      S := Interface_Spec (E);
      if not Is_Empty (S) then
         Write_Space;
         Write (T_Colon);
         Write_Space;
         I := First_Entity (S);
         loop
            Generate (I);
            I := Next_Entity (I);
            exit when No (I);
            Write (T_Comma);
            Write_Space;
         end loop;
      end if;

      --  Prepare to inherit operations and attributes from parent
      --  interfaces. Preserve entities from current interface in F.
      --  Empty body of current interface in order to enter attributes
      --  and operations of parent interfaces.

      Write_Space;
      Write_Line (T_Left_Brace);

      B := Interface_Body (E);
      if not Is_Empty (B) then
         Increment_Indentation;
         F := First_Entity (B);
         while Present (F) loop
            Write_Indentation;
            Generate (F);
            Generate_Statement_Delimiter (F);
            F := Next_Entity (F);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (T_Right_Brace);
   end Generate_Interface_Declaration;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (E : Node_Id) is
   begin
      Generate (Value (E));
   end Generate_Literal;

   ---------------------
   -- Generate_Member --
   ---------------------

   procedure Generate_Member (E : Node_Id) is
      D : Node_Id := First_Entity (Declarators (E));

   begin
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (D);
         D := Next_Entity (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;
   end Generate_Member;

   ---------------------
   -- Generate_Module --
   ---------------------

   procedure Generate_Module (E : Node_Id) is
      M : constant Boolean := (Kind (E) = K_Module);
      C : Node_Id;
      L : List_Id;
      I : Node_Id;

   begin
      if M then
         Write (T_Module);
         Write_Space;
         Generate (Identifier (E));
      elsif Kind (E) = K_Specification then

         --  Generate the "import" declarations

         if not Is_Empty (Imports (E)) then
            I := First_Entity (Imports (E));
            while Present (I) loop
               if not Imported (I) or else
                 Generate_Imported then
                  Generate (I);
                  Generate_Statement_Delimiter (I);
               end if;
               I := Next_Entity (I);
            end loop;
         end if;
      end if;

      L := Definitions (E);
      if not Is_Empty (L) then
         if M then
            Write_Space;
            Write_Line (T_Left_Brace);
            Increment_Indentation;
         end if;
         C := First_Entity (L);
         while Present (C) loop
            if not Imported (C) or else
              Generate_Imported then
               Write_Indentation;
               Generate (C);
               Generate_Statement_Delimiter (C);
            end if;
            C := Next_Entity (C);
         end loop;
         if M then
            Decrement_Indentation;
            Write_Indentation;
            Write (T_Right_Brace);
         end if;
      end if;
   end Generate_Module;

   --------------------------
   -- Generate_Native_Type --
   --------------------------

   procedure Generate_Native_Type (E : Node_Id) is
   begin
      Write (T_Native);
      Write_Space;
      Generate (Declarator (E));
   end Generate_Native_Type;

   ------------------------------------
   -- Generate_Operation_Declaration --
   ------------------------------------

   procedure Generate_Operation_Declaration (E : Node_Id) is
      C : Node_Id;
      L : List_Id;

   begin
      if Kind (E) /= K_Initializer_Declaration then
         if Is_Oneway (E) then
            Write (T_Oneway);
            Write_Space;
         end if;
         Generate (Type_Spec (E));
         Write_Space;
      end if;

      Generate (Identifier (E));
      Write (T_Left_Paren);

      L := Parameters (E);
      if not Is_Empty (L) then
         C := First_Entity (L);
         loop
            Generate (C);
            C := Next_Entity (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
      end if;
      Write (T_Right_Paren);

      L := Exceptions (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Raises);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Entity (L);
         loop
            Generate (C);
            C := Next_Entity (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;

      L := Contexts (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Context);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Entity (L);
         loop
            Write_Str (Image (Value (C)));
            C := Next_Entity (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;
   end Generate_Operation_Declaration;

   ------------------------------------
   -- Generate_Parameter_Declaration --
   ------------------------------------

   procedure Generate_Parameter_Declaration (E : Node_Id) is
   begin
      Write (Parameter_Mode (Parameter_Mode (E)));
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      Generate (Declarator (E));
   end Generate_Parameter_Declaration;

   ---------------------
   -- Generate_Pragma --
   ---------------------

   procedure Generate_Pragma (E : Node_Id) is
   begin
      Write_Str ("#");
      Write (T_Pragma);
      Write_Space;

      if Pragma_Kind (E) = Pragma_Unrecognized then
         Write_Eol;
         return;
      end if;

      Write (Get_Pragma_Type (Pragma_Kind (E)));
      Write_Space;

      case Pragma_Kind (E) is
         when Pragma_Id =>
            Generate (Target (E));
            Write_Space;
            Write_Char ('"');
            Write_Name (Data (E));
            Write_Char ('"');

         when Pragma_Prefix =>
            Write_Char ('"');
            Write_Name (Data (E));
            Write_Char ('"');

         when Pragma_Version =>
            Generate (Target (E));
            Write_Space;
            Write_Name (Data (E));

         when Pragma_Unrecognized =>
            --  Extract from the CORBA 3.0 ($10.7.5) :
            --  "Conforming IDL compilers may support additional non-standard
            --   pragmas, but must not refuse to compile IDL source containing
            --   non-standard pragmas that are not understood by the compiler"

            --  So, we just indicate that a non recognized pragma is
            --  encountered

            null;

      end case;

      Write_Eol;
   end Generate_Pragma;

   --------------------------
   -- Generate_Scoped_Name --
   --------------------------

   procedure Generate_Scoped_Name (E : Node_Id) is

      procedure Generate_Reference_Name (E : Node_Id);

      -----------------------------
      -- Generate_Reference_Name --
      -----------------------------

      procedure Generate_Reference_Name (E : Node_Id) is
         S : constant Node_Id := Scope_Entity (E);

      begin
         if Kind (S) /= K_Specification then
            Generate_Reference_Name (Identifier (S));
            Write (T_Colon_Colon);
         end if;
         Write_Name (IDL_Name (E));
      end Generate_Reference_Name;

      R : constant Node_Id := Reference (E);
      V : Value_Id;

   begin
      if Kind (R) = K_Constant_Declaration then
         V := Value (R);
         if Value (V).K = K_Enumerator then
            Generate_Reference_Name (Identifier (R));
         else
            Generate (V);
         end if;

      else
         Generate_Reference_Name (Identifier (R));
      end if;
   end Generate_Scoped_Name;

   ---------------------------
   -- Generate_Sequence_Type --
   ---------------------------

   procedure Generate_Sequence_Type (E : Node_Id) is
      S : constant Node_Id := Max_Size (E);

   begin
      Write (T_Sequence);
      Write (T_Less);
      Generate (Type_Spec (E));
      if Present (S) then
         Write (T_Comma);
         Write_Space;
         Generate (S);
      end if;
      Write (T_Greater);
   end Generate_Sequence_Type;

   --------------------------------
   -- Generate_Simple_Declarator --
   --------------------------------

   procedure Generate_Simple_Declarator (E : Node_Id) is
   begin
      Generate (Identifier (E));
   end Generate_Simple_Declarator;

   ---------------------------
   -- Generate_State_Member --
   ---------------------------

   procedure Generate_State_Member (E : Node_Id) is
   begin
      if Is_Public (E) then
         Write (T_Public);
      else
         Write (T_Private);
      end if;
      Write_Space;
      Generate_Member (E);
   end Generate_State_Member;

   --------------------------
   -- Generate_String_Type --
   --------------------------

   procedure Generate_String_Type (E : Node_Id) is
   begin
      if Kind (E) = K_String_Type then
         Write (T_String);
      else
         Write (T_Wstring);
      end if;
      Write (T_Less);
      Generate (Value (Max_Size (E)));
      Write (T_Greater);
   end Generate_String_Type;

   -----------------------------
   -- Generate_Structure_Type --
   -----------------------------

   procedure Generate_Structure_Type (E : Node_Id) is
      L : List_Id;
      C : Node_Id;

   begin
      Write (T_Struct);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write_Line (T_Left_Brace);
      L := Members (E);
      if not Is_Empty (L) then
         Increment_Indentation;
         C := First_Entity (L);
         while Present (C) loop
            Write_Indentation;
            Generate (C);
            Generate_Statement_Delimiter (C);
            C := Next_Entity (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (T_Right_Brace);
   end Generate_Structure_Type;

   ---------------------------------
   -- Generate_Switch_Alternative --
   ---------------------------------

   procedure Generate_Switch_Alternative (E : Node_Id) is
      L : Node_Id := First_Entity (Labels (E));

   begin
      while Present (L) loop
         Write_Indentation;
         Generate (L);
         Write_Eol;
         L := Next_Entity (L);
      end loop;
      Increment_Indentation;
      Write_Indentation;
      Generate (Element (E));
      Decrement_Indentation;
   end Generate_Switch_Alternative;

   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------

   procedure Generate_Type_Declaration (E : Node_Id) is
      D : Node_Id := First_Entity (Declarators (E));

   begin
      Write (T_Typedef);
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (D);
         D := Next_Entity (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;
   end Generate_Type_Declaration;

   ----------------------------------
   -- Generate_Type_Id_Declaration --
   ----------------------------------

   procedure Generate_Type_Id_Declaration (E : Node_Id) is
   begin
      Write (T_Type_Id);
      Write_Space;
      Generate (Target (E));
      Write_Space;
      Write_Char ('"');
      Write_Name (Data (E));
      Write_Char ('"');

   end Generate_Type_Id_Declaration;

   --------------------------------------
   -- Generate_Type_Prefix_Declaration --
   --------------------------------------

   procedure Generate_Type_Prefix_Declaration (E : Node_Id) is
   begin
      Write (T_Type_Prefix);
      Write_Space;
      Generate (Target (E));
      Write_Space;
      Write_Char ('"');
      Write_Name (Data (E));
      Write_Char ('"');
   end Generate_Type_Prefix_Declaration;

   -------------------------
   -- Generate_Union_Type --
   -------------------------

   procedure Generate_Union_Type (E : Node_Id) is
      N : Node_Id := First_Entity (Switch_Type_Body (E));

   begin
      Write (T_Union);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write (T_Switch);
      Write_Space;
      Write (T_Left_Paren);
      Generate (Switch_Type_Spec (E));
      Write (T_Right_Paren);
      Write_Space;
      Write (T_Left_Brace);
      Write_Eol;
      Increment_Indentation;
      while Present (N) loop
         Write_Indentation;
         Generate (N);
         Generate_Statement_Delimiter (N);
         N := Next_Entity (N);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (T_Right_Brace);
   end Generate_Union_Type;

   ------------------------------------
   -- Generate_Value_Box_Declaration --
   ------------------------------------

   procedure Generate_Value_Box_Declaration (E : Node_Id) is
   begin
      Write (T_Value_Type);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Generate (Type_Spec (E));
   end Generate_Value_Box_Declaration;

   --------------------------------
   -- Generate_Value_Declaration --
   --------------------------------

   procedure Generate_Value_Declaration (E : Node_Id) is
      S : constant Node_Id := Value_Spec (E);
      N : Node_Id;
      L : List_Id;

   begin
      Write (T_Value_Type);
      Write_Space;
      Generate (Identifier (E));
      L := Value_Names (S);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Colon);
         if Is_Truncatable (S) then
            Write_Space;
            Write (T_Truncatable);
         end if;
         N := First_Entity (L);
         loop
            Write_Space;
            Generate (N);
            N := Next_Entity (N);
            exit when No (N);
            Write (T_Comma);
         end loop;
      end if;
      L := Interface_Names (S);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Supports);
         N := First_Entity (L);
         loop
            Write_Space;
            Generate (N);
            N := Next_Entity (N);
            exit when No (N);
            Write (T_Comma);
         end loop;
      end if;
      Write_Space;
      Write_Line (T_Left_Brace);

      L := Value_Body (E);
      if not Is_Empty (L) then
         Increment_Indentation;
         N := First_Entity (L);
         while Present (N) loop
            Write_Indentation;
            Generate (N);
            Generate_Statement_Delimiter (N);
            N := Next_Entity (N);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (T_Right_Brace);
   end Generate_Value_Declaration;

   ----------------------------------------
   -- Generate_Value_Forward_Declaration --
   ----------------------------------------

   procedure Generate_Value_Forward_Declaration (E : Node_Id) is
   begin
      if Is_Abstract_Value (E) then
         Write (T_Abstract);
         Write_Space;
      end if;
      Write (T_Value_Type);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Value_Forward_Declaration;

   -----------
   -- Usage --
   -----------

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Line
        (Hdr & "-b n     Base to output integer literal");
      Write_Line
        (Hdr & "         As a default (zero) use base from input");
      Write_Line
        (Hdr & "-e       Expand IDL Tree");
      Write_Line
        (Hdr & "-df      Dump IDL Tree (may be used in conjunction with -e");
      Write_Line
        (Hdr & "         to dump the expanded IDL tree)");
      Write_Line
        (Hdr & "-di      Output IDL code of imported entities  (may be");
      Write_Line
        (Hdr & "         used in conjunction with -e to output the");
      Write_Line
        (Hdr & "         expanded IDL code)");
   end Usage;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   ----------------------------------
   -- Generate_Statement_Delimiter --
   ----------------------------------

   procedure Generate_Statement_Delimiter (E : Node_Id) is
   begin
      if Kind (E) /= K_Pragma then
         Write_Line (T_Semi_Colon);
      end if;
   end Generate_Statement_Delimiter;

end Backend.BE_IDL;
