with Ada.Text_IO; use Ada.Text_IO;

with Charset;     use Charset;
with Locations;   use Locations;
with Lexer;       use Lexer;
with Namet;       use Namet;
with Nodes;       use Nodes;
with Output;      use Output;
--  with Scopes;      use Scopes;
with Types;       use Types;
with Utils;       use Utils;

package body Debug is

   procedure Print_Component_Category (Category : Byte);

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      S : String := Node_Kind'Image (N);
   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (3 .. S'Last);
   end Image;

   function Image (N : Name_Id) return String is
   begin
      if N = No_Name then
         return No_Str;
      else
         return Get_Name_String (N);
      end if;
   end Image;

   function Image (N : Node_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Mode_Id) return String is
   begin
      return Quoted (Image (Token_Type'Val (N)));
   end Image;

   function Image (N : Operator_Id) return String is
   begin
      return Quoted (Image (Token_Type'Val (N)));
   end Image;

   function Image (N : Boolean) return String is
   begin
      return Boolean'Image (N);
   end Image;

   function Image (N : Byte) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Int) return String is
      S : constant String := Int'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   ------------------------------
   -- Print_Component_Category --
   ------------------------------

   procedure Print_Component_Category (Category : Byte) is
   begin
      case Category is
         when  1 => Write_Str ("data");
         when  2 => Write_Str ("subprogram");
         when  3 => Write_Str ("thread");
         when  4 => Write_Str ("threadgroup");
         when  5 => Write_Str ("process");
         when  6 => Write_Str ("memory");
         when  7 => Write_Str ("processor");
         when  8 => Write_Str ("bus");
         when  9 => Write_Str ("device");
         when 10 => Write_Str ("system");

         when others
           => Write_Str ("[UNKNOWN]");
      end case;
   end Print_Component_Category;

   ----------------
   -- Print_Node --
   ----------------

   procedure Print_Node (N : Node_Id) is
      Ident : Node_Id;
      Node  : Node_Id;

   begin
      N_Indents := N_Indents + 1;

      case Kind (N) is
         when K_Node_Id =>
            W_Indents;
            Write_Line ("Node_Id = " & Image (N));

         when K_List_Id
           | K_AADL_Declaration_List
           | K_Identifiers_List =>
            Node := First_Node (List_Id (N));
            if Present (Node) then
               while Present (Node) loop
                  Print_Node (Node);
                  Node := Next_Node (Node);
               end loop;
            else
               W_Indents;
               Write_Str ("[EMPTY]");
            end if;
            Write_Eol;

         when K_Identifier =>
            Write_Name (Display_Name (N));

         when K_AADL_Specification =>
            W_Indents;
            Write_Line ("AADL_Specification:");
            Print_Node (Node_Id (Declarations (N)));

         when K_Package_Items =>
            Node := Node_Id (Items (N));
            if Present (Node) then
               Print_Node (Node);
            end if;
            Node := Node_Id (Properties (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Properties:");
               Print_Node (Node);
            end if;

         when K_Package_Name =>
            Ident := First_Node (List_Id (N));
            while Present (Ident) loop
               Print_Node (Ident);
               Ident := Next_Node (Ident);
               if Present (Ident) then
                  Write_Char ('.');
               end if;
            end loop;

         when K_Package_Spec =>
            W_Indents;
            Write_Str ("Package: ");
            Print_Node (Node_Id (Package_Name (N)));
            Write_Eol;
            Node := Node_Id (Public_Package_Items (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Public:");
               Print_Node (Node);
            end if;
            Node := Node_Id (Private_Package_Items (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Private:");
               Print_Node (Node);
            end if;

         when K_Component_Type =>
            W_Indents;
            Write_Str ("Component_Type: ");
            Print_Component_Category (Category (N));
            Write_Char (' ');
            Print_Node (Identifier (N));
            Write_Eol;

            Node := Node_Id (Provides (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Provides");
               Print_Node (Node);
            end if;
            Node := Node_Id (Requires (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Requires");
               Print_Node (Node);
            end if;
            Node := Node_Id (Parameters (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Parameters");
               Print_Node (Node);
            end if;
            Node := Node_Id (Properties (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Properties");
               Print_Node (Node);
            end if;
            Node := Node_Id (Annexes (N));
            if Present (Node) then
               W_Indents;
               Write_Line ("Annexes");
               Print_Node (Node);
            end if;

         when K_Port_Spec =>
            W_Indents;
            if Is_Refinement (N) then
               Write_Str ("Port_Refinement: ");
            else
               Write_Str ("Port: ");
            end if;
            Print_Node (Node_Id (Identifiers_List (N)));
            W_Indents;
            if Is_In (N) then
               Write_Str ("in ");
            end if;
            if Is_Out (N) then
               Write_Str ("out ");
            end if;
            Print_Node (Port_Type (N));
            Node := Node_Id (Properties (N));
            if Present (Node) then
               Write_Eol;
               Write_Str ("Properties:");
               Print_Node (Node);
            end if;
            Write_Eol;

         when K_Port_Type =>
            if Is_Event (N) then
               Write_Str ("event ");
            end if;
            if Present (Data_Ref (N)) then
               Write_Str ("data ");
            end if;

         when others =>
            W_Indents;
            Put ("Other node");
            Write_Eol;
      end case;

      N_Indents := N_Indents - 1;
   end Print_Node;

   ---------------
   -- W_Boolean --
   ---------------

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   ------------
   -- W_Byte --
   ------------

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   -----------------
   -- W_Full_Tree --
   -----------------

   procedure W_Full_Tree (N : Node_Id) is
      D : Node_Id := First_Node (List_Id (N));
   begin
      N_Indents := 0;
      while Present (D) loop
         W_Node_Id (D);
         D := Next_Node (D);
      end loop;
   end W_Full_Tree;

   ---------------
   -- W_Indents --
   ---------------

   procedure W_Indents is
   begin
      for I in 1 .. N_Indents loop
         Write_Str (" ");
      end loop;
   end W_Indents;

   ---------------
   -- W_List_Id --
   ---------------

   procedure W_List_Id (L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Node (L);
      while E /= No_Node loop
         W_Node_Id (E);
         E := Next_Node (E);
      end loop;
   end W_List_Id;

   ----------------------
   -- W_Node_Attribute --
   ----------------------

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0)
   is
      C : Node_Id;
   begin
      if A = "Next_Node"
        or else A = "Homonym"
        or else A = "Name"
        or else A = "Scoped_Identifiers"
--          or else A = "Explicitely_Visible"
--          or else A = "Implicitely_Visible"
        or else A = "Next_Identifier"
      then
         return;
      end if;
      N_Indents := N_Indents + 1;
      W_Indents;
      Write_Str  (A);
      Write_Char (' ');
      Write_Str  (K);
      Write_Char (' ');
      C := Node_Id (N);
      if K = "Name_Id" then
         Write_Line (Quoted (V));

      elsif K = "Node_Id"
        and then Present (C)
      then
--         case Kind (C) is
--            when K_Float .. K_Value_Base =>
--               Write_Line ('(' & Image (Kind (Node_Id (N))) & ')');
--            when others =>
               Write_Line (V);
--         end case;

      else
         Write_Line (V);
      end if;

      if A /= "Node"
        and then A /= "Scope"
        and then A /= "Reference"
        and then A /= "Base_Interface"
        and then A /= "Declaration"
      then
         if K = "Node_Id" then
            W_Node_Id (Node_Id (N));
         elsif K = "List_Id" then
            W_List_Id (List_Id (N));
         end if;
      end if;

      N_Indents := N_Indents - 1;
   end W_Node_Attribute;

   -------------------
   -- W_Node_Header --
   -------------------

   procedure W_Node_Header (N : Node_Id) is
   begin
      W_Indents;
      Write_Int  (Int (N));
      Write_Char (' ');
      Write_Str  (Image (Kind (N)));
      Write_Char (' ');
      Write_Line (Image (Loc (N)));
   end W_Node_Header;

   ---------------
   -- W_Node_Id --
   ---------------

   procedure W_Node_Id (N : Node_Id) is
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (N);
   end W_Node_Id;

   ---------
   -- wni --
   ---------

   procedure wni (N : Node_Id) is
      I : constant Natural := N_Indents;
   begin
      N_Indents := 1;
      W_Node_Id (N);
      N_Indents := I;
   end wni;

end Debug;
