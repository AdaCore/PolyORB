with Backend.BE_Ada.Debug;  use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;

with Charset; use Charset;
with Errors;  use Errors;
with Namet;   use Namet;
with Output;  use Output;
with Types;   use Types;

package body Backend.BE_Ada.Generator is

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Mod,             -- MOD   **** First Keyword
      Tok_Rem,             -- REM
      Tok_New,             -- NEW
      Tok_Abs,             -- ABS
      Tok_Others,          -- OTHERS
      Tok_Null,            -- NULL
      Tok_Delta,           -- DELTA
      Tok_Digits,          -- DIGITS
      Tok_Range,           -- RANGE
      Tok_And,             -- AND
      Tok_Or,              -- OR
      Tok_Xor,             -- XOR
      Tok_In,              -- IN
      Tok_Not,             -- NOT
      Tok_Abstract,        -- ABSTRACT
      Tok_Access,          -- ACCESS
      Tok_Aliased,         -- ALIASED
      Tok_All,             -- ALL
      Tok_Array,           -- ARRAY
      Tok_At,              -- AT
      Tok_Body,            -- BODY
      Tok_Constant,        -- CONSTANT
      Tok_Do,              -- DO
      Tok_Is,              -- IS
      Tok_Limited,         -- LIMITED
      Tok_Of,              -- OF
      Tok_Out,             -- OUT
      Tok_Record,          -- RECORD
      Tok_Renames,         -- RENAMES
      Tok_Reverse,         -- REVERSE
      Tok_Tagged,          -- TAGGED
      Tok_Then,            -- THEN
      Tok_Abort,           -- ABORT
      Tok_Accept,          -- ACCEPT
      Tok_Case,            -- CASE
      Tok_Delay,           -- DELAY
      Tok_Else,            -- ELSE
      Tok_Elsif,           -- ELSIF
      Tok_End,             -- END
      Tok_Exception,       -- EXCEPTION
      Tok_Exit,            -- EXIT
      Tok_Goto,            -- GOTO
      Tok_If,              -- IF
      Tok_Pragma,          -- PRAGMA
      Tok_Raise,           -- RAISE
      Tok_Requeue,         -- REQUEUE
      Tok_Return,          -- RETURN
      Tok_Select,          -- SELECT
      Tok_Terminate,       -- TERMINATE
      Tok_Until,           -- UNTIL
      Tok_When,            -- WHEN

      Tok_Begin,           -- BEGIN
      Tok_Declare,         -- DECLARE
      Tok_For,             -- FOR
      Tok_Loop,            -- LOOP
      Tok_While,           -- WHILE

      Tok_Entry,           -- ENTRY
      Tok_Protected,       -- PROTECTED
      Tok_Task,            -- TASK
      Tok_Type,            -- TYPE
      Tok_Subtype,         -- SUBTYPE
      Tok_Use,             -- USE

      Tok_Function,        -- FUNCTION
      Tok_Generic,         -- GENERIC
      Tok_Package,         -- PACKAGE
      Tok_Procedure,       -- PROCEDURE

      Tok_Private,         -- PRIVATE
      Tok_With,            -- WITH
      Tok_Separate,        -- SEPARATE **** Last Keyword

      --  Graphic Characters
      Tok_Double_Asterisk, -- **
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Box,             -- <>
      Tok_Colon_Equal,     -- :=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- =>
      Tok_Vertical_Bar,    -- |
      Tok_Dot_Dot);        -- ..

   Token_Image : array (Token_Type) of Name_Id;
   subtype Keyword_Type is Token_Type
     range Tok_Mod .. Tok_Separate;


   procedure Generate_Component_Declaration (N : Node_Id);
   procedure Generate_Defining_Identifier (E : Node_Id);
   procedure Generate_Derived_Type_Definition (N : Node_Id);
   procedure Generate_Designator (N : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Enumeration_Type_Definition (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_IDL_Unit_Packages (N : Node_Id);
   procedure Generate_Package_Declaration (N : Node_Id);
   procedure Generate_Package_Implementation (N : Node_Id);
   procedure Generate_Package_Specification (N : Node_Id);
   procedure Generate_Parameter (N : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Qualified_Name (N : Node_Id);
   procedure Generate_Record_Definition (N : Node_Id);
   procedure Generate_Record_Type_Definition (N : Node_Id);
   procedure Generate_Subprogram_Implementation (N : Node_Id);
   procedure Generate_Subprogram_Specification (N : Node_Id);
   procedure Generate_Type_Spec (N : Node_Id);
   procedure Generate_Withed_Package (N : Node_Id);

   function Image (T : Token_Type) return String;
   procedure New_Token
     (Token : Token_Type;
      I : String := "");
   procedure Write_Token (T : Token_Type);

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Component_Declaration =>
            Generate_Component_Declaration (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Designator =>
            Generate_Designator (N);

         when K_Enumeration_Type_Definition =>
            Generate_Enumeration_Type_Definition (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_IDL_Unit =>
            Generate_IDL_Unit_Packages (N);

         when K_Package_Declaration =>
            Generate_Package_Declaration (N);

         when K_Package_Implementation =>
            Generate_Package_Implementation (N);

         when K_Package_Specification =>
            Generate_Package_Specification (N);

         when K_Record_Definition =>
            Generate_Record_Definition (N);

         when K_Record_Type_Definition =>
            Generate_Record_Type_Definition (N);

         when K_Subprogram_Specification =>
            Generate_Subprogram_Specification (N);

         when K_Subprogram_Implementation =>
            Generate_Subprogram_Implementation (N);

         when K_Withed_Package =>
            Generate_Withed_Package (N);

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
      Write_Space;
      Write_Token (Tok_Colon);
      Write_Space;
      Generate (Subtype_Indication (N));
      Write_Token (Tok_Semicolon);
   end Generate_Component_Declaration;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (E : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (E);
      if Present (P) then
         Generate (P);
         Write_Token (Tok_Dot);
      end if;
      Write_Name (Name (E));
   end Generate_Defining_Identifier;

   -------------------------
   -- Generate_Designator --
   -------------------------

   procedure Generate_Designator (N : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (N);
      if Present (P) then
         Generate (P);
         Write_Token (Tok_Dot);
      end if;
      Write_Name (Name (Defining_Identifier (N)));
   end Generate_Designator;

   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------

   procedure Generate_Derived_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write_Space;
         Write_Token (Tok_Abstract);
      end if;
      Write_Space;
      Write_Token (Tok_New);
      Write_Space;
      Generate (Subtype_Indication (N));
      R := Record_Extension_Part (N);
      if Present (R) then
         Write_Space;
         Write_Token (Tok_With);
         Write_Space;
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
      Write_Token (Tok_Left_Paren);
      N := First_Node (Enumeration_Literals (E));
      while Present (N) loop
         Generate_Defining_Identifier (N);
         N := Next_Node (N);
         if Present (N) then
            Write_Token (Tok_Comma);
            Write_Indentation;
         end if;
      end loop;
      Write_Token (Tok_Right_Paren);
   end Generate_Enumeration_Type;

   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------

   procedure Generate_Enumeration_Type_Definition (N : Node_Id) is
      E : Node_Id;

   begin
      Write_Token (Tok_Left_Paren);
      E := First_Node (Enumeration_Literals (N));
      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Token (Tok_Comma);
         Write_Space;
      end loop;
      Write_Token (Tok_Right_Paren);
   end Generate_Enumeration_Type_Definition;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
   begin
      Write_Indentation;
      Write_Token (Tok_Type);
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write_Token (Tok_Is);
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
      P : Node_Id;
   begin
      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;
      Write_Indentation;
      Write_Token  (Tok_Package);
      Write_Space;
      Write_Token (Tok_Body);
      Write_Space;
      Generate_Qualified_Name (Package_Declaration (N));
      Write_Space;
      Write_Token (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      P := First_Node (Statements (N));
      while Present (P) loop
         Generate (P);
         Write_Token (Tok_Semicolon);
         Write_Eol;
         P := Next_Node (P);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write_Token  (Tok_End);
      Write_Space;
      Generate_Qualified_Name (Package_Declaration (N));
      Write_Token (Tok_Semicolon);
      Write_Eol;
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
      Write_Token  (Tok_Package);
      Write_Space;
      Generate_Qualified_Name (Package_Declaration (N));
      Write_Space;
      Write_Token (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      P := First_Node (Visible_Part (N));
      while Present (P) loop
         Generate (P);
         Write_Token (Tok_Semicolon);
         Write_Eol;
         P := Next_Node (P);
      end loop;
      if not Is_Empty (Private_Part (N)) then
         Write_Indentation;
         Write_Token (Tok_Private);
         Write_Space;
         P := First_Node (Private_Part (N));
         while Present (P) loop
            Generate (P);
            Write_Token (Tok_Semicolon);
            P := Next_Node (P);
         end loop;
      end if;
      Decrement_Indentation;
      Write_Indentation;
      Write_Token  (Tok_End);
      Write_Space;
      Generate_Qualified_Name (Package_Declaration (N));
      Write_Token (Tok_Semicolon);
      Write_Eol;
   end Generate_Package_Specification;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (N : Node_Id) is
   begin
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write_Token  (Tok_Colon);
      Write_Space;
      case Parameter_Mode (N) is
         when Mode_In =>
            Write_Token (Tok_In);
         when Mode_Out =>
            Write_Token (Tok_Out);
         when Mode_Inout =>
            Write_Token (Tok_In);
            Write_Space;
            Write_Token (Tok_Out);
      end case;
      Write_Space;
      Generate_Type_Spec (Parameter_Type (N));
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
      Write_Token (Tok_Left_Paren);
      while Present (N) loop
         Generate_Parameter (N);
         N := Next_Node (N);
         if Present (N) then
            Write_Token (Tok_Semicolon);
            Write_Eol;
            Write_Indentation;
         end if;
      end loop;
      Write_Token (Tok_Right_Paren);
   end Generate_Parameter_List;

   -----------------------------
   -- Generate_Qualified_Name --
   -----------------------------

   procedure Generate_Qualified_Name (N : Node_Id) is
   begin
      pragma Assert (Kind (N) = K_Package_Declaration);


      if Present (Parent (N)) then
         Generate_Qualified_Name (Parent (N));
         Write_Token (Tok_Dot);
      end if;
      Write_Name (Name (Defining_Identifier (N)));

   end Generate_Qualified_Name;

   --------------------------------
   -- Generate_Record_Definition --
   --------------------------------

   procedure Generate_Record_Definition (N : Node_Id) is
      L : constant List_Id := Component_List (N);
      C : Node_Id;

   begin
      if Is_Empty (L) then
         Write_Token (Tok_Null);
         Write_Space;
         Write_Token (Tok_Record);
      else
         Write_Space;
         Write_Token (Tok_Record);
         Write_Eol;
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Generate (C);
            C := Next_Node (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
         Write_Token (Tok_End);
         Write_Space;
         Write_Token (Tok_Record);
      end if;
   end Generate_Record_Definition;

   -------------------------------------
   -- Generate_Record_Type_Definition --
   -------------------------------------

   procedure Generate_Record_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write_Token (Tok_Abstract);
         Write_Space;
      end if;
      if Is_Tagged_Type (N) then
         Write_Token (Tok_Tagged);
         Write_Space;
      end if;
      if Is_Limited_Type (N) then
         Write_Token (Tok_Limited);
         Write_Space;
      end if;
      R := Record_Definition (N);
      if Present (R) then
         Generate (R);
      end if;
   end Generate_Record_Type_Definition;

   ----------------------------------------
   -- Generate_Subprogram_Implementation --
   ----------------------------------------

   procedure Generate_Subprogram_Implementation (N : Node_Id) is
      D : List_Id;
      S : List_Id;
   begin
      D := Declarations (N);
      S := Statements (N);
      Generate_Subprogram_Specification (Specification (N));
      Write_Token (Tok_Is);
      Write_Eol;
      if not Is_Empty (D)  then
         null; -- Generate Declarations
      end if;
      Write_Indentation;
      Write_Token (Tok_Begin);
      Write_Eol;

      if not Is_Empty (S) then
         null; -- Generate statements
      end if;
      Write_Indentation;
      Write_Token (Tok_End);
      Write_Space;
   end Generate_Subprogram_Implementation;

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
         Write_Token (Tok_Function);
      else
         Write_Token (Tok_Procedure);
      end if;
      Write_Space;
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
         Write_Token (Tok_Return);
         Write_Space;
         Generate_Type_Spec (T);
      end if;
      Decrement_Indentation;
   end Generate_Subprogram_Specification;

   ------------------------
   -- Generate_Type_Spec --
   ------------------------

   procedure Generate_Type_Spec (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Full_Type_Declaration =>
            Write_Name (Name (Defining_Identifier (N)));

         when K_Enumeration_Type =>
            Generate_Enumeration_Type (N);

         when K_Designator =>
            Generate_Designator (N);

         when others =>
            DE (Image (Kind (N)) & " not supported");
      end case;
   end Generate_Type_Spec;

   -----------------------------
   -- Generate_Withed_Package --
   -----------------------------

   procedure Generate_Withed_Package (N : Node_Id) is
   begin
      Write_Token (Tok_With);
      Write_Space;
      Generate (Defining_Identifier (N));
      Write_Token (Tok_Semicolon);
   end Generate_Withed_Package;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
   end Image;

   procedure Initialize is
   begin

      --  Keywords.
      for I in Keyword_Type loop
         New_Token (I);
      end loop;

      --  Graphic Characters
      New_Token (Tok_Double_Asterisk, "**");
      New_Token (Tok_Ampersand, "&");
      New_Token (Tok_Minus, "-");
      New_Token (Tok_Plus, "+");
      New_Token (Tok_Asterisk, "*");
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Dot, ".");
      New_Token (Tok_Apostrophe, "'");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Comma, ",");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "/=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Box, "<>");
      New_Token (Tok_Colon_Equal, ":=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Arrow, "=>");
      New_Token (Tok_Vertical_Bar, "|");
      New_Token (Tok_Dot_Dot, "..");
   end Initialize;
   ---------------
   -- New_Token --
   ---------------

   procedure New_Token
     (Token : Token_Type;
      I : String := "") is
   begin
      if Token in Keyword_Type then
         Set_Str_To_Name_Buffer (Image (Token));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (Token)));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (Token) := Name_Find;
   end New_Token;

   -----------------
   -- Write_Token --
   -----------------

   procedure Write_Token (T : Token_Type) is
      pragma Inline (Write_Token);
   begin
      Write_Name (Token_Image (T));
   end Write_Token;

end Backend.BE_Ada.Generator;
