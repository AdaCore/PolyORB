with Nodes;  use Nodes;
with Output; use Output;
with Types;  use Types;

package Debug is

   procedure W_Eol                       renames Output.Write_Eol;
   procedure W_Int         (N : Int)     renames Output.Write_Int;
   procedure W_Indentation (N : Natural);
   procedure W_Line        (N : String)  renames Output.Write_Line;
   procedure W_Str         (N : String)  renames Output.Write_Str;

   procedure W_Boolean     (N : Boolean);
   procedure W_Byte        (N : Byte);
   procedure W_List_Id     (I : Natural; L : List_Id);
   procedure W_Node_Id     (I : Natural; N : Node_Id);
   procedure W_Node_Header (I : Natural; N : Node_Id);
   procedure W_Full_Tree;

   procedure W_Node_Attribute
     (I : Natural;
      A : String;
      T : String;
      V : String;
      N : Int := 0);

   function Image (N : Node_Kind) return String;
   function Image (N : Name_Id) return String;
   function Image (N : Node_Id) return String;
   function Image (N : List_Id) return String;
   function Image (N : Mode_Id) return String;
   function Image (N : Value_Id) return String;
   function Image (N : Operator_Id) return String;
   function Image (N : Boolean) return String;
   function Image (N : Byte) return String;
   function Image (N : Int) return String;

   procedure wni (N : Node_Id);
   pragma Export (C, wni, "wni");

end Debug;
