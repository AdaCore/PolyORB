with Locations;   use Locations;
with Lexer;       use Lexer;
with Namet;       use Namet;
with Nodes;       use Nodes;
with Output;      use Output;
with Scopes;      use Scopes;
with Types;       use Types;
with Utils;       use Utils;

package body Debug is

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      I : String := Node_Kind'Image (N);
   begin
      Capitalize (I);
      return I (3 .. I'Last);
   end Image;

   procedure W_Indentation (N : Natural) is
   begin
      for I in 1 .. N loop
         Write_Str ("   ");
      end loop;
   end W_Indentation;

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   procedure W_List_Id (I : Natural; L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Node (L);
      while E /= No_Node loop
         W_Node_Id (I, E);
         E := Next_Node (E);
      end loop;
   end W_List_Id;

   procedure W_Node_Attribute
     (I : Natural;
      A : String;
      T : String;
      V : String;
      N : Int := 0)
   is
      C : Node_Id;
   begin
      if A = "Next_Node"
        or else A = "Homonym"
        or else A = "Name"
      then
         return;
      end if;
      W_Indentation (I + 1);
      Write_Str  (A);
      Write_Char (' ');
      Write_Str  (T);
      Write_Char (' ');
      C := Node_Id (N);
      if T = "Name_Id" then
         Write_Line (Quoted (V));
      elsif T = "Node_Id"
        and then Present (C)
      then
         case Kind (C) is
            when K_Float .. K_Value_Base =>
               Write_Line ('(' & Image (Kind (Node_Id (N))) & ')');
            when others =>
               Write_Line (V);
         end case;
      else
         Write_Line (V);
      end if;
      if A = "Node"
        or else A = "Scope"
      then
         return;
      end if;
      if T = "Node_Id" then
         W_Node_Id (I + 1, Node_Id (N));
      elsif T = "List_Id" then
         W_List_Id (I + 1, List_Id (N));
      end if;
   end W_Node_Attribute;

   procedure W_Node_Id (I : Natural; N : Node_Id) is
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (I, N);
   end W_Node_Id;

   procedure W_Node_Header (I : Natural; N : Node_Id) is
   begin
      W_Indentation (I);
      Write_Int   (Int (N));
      Write_Char  (' ');
      Write_Str   (Image (Kind (N)));
      Write_Char  (' ');
      Write_Line  (Image (Loc (N)));
   end W_Node_Header;

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

   procedure wni (N : Node_Id) is
   begin
      W_Node_Id (1, N);
   end wni;

   procedure W_Full_Tree is
      D : Node_Id := First_Node (Definitions (Root));
   begin
      while Present (D) loop
         W_Node_Id (0, D);
         D := Next_Node (D);
      end loop;
   end W_Full_Tree;

end Debug;
