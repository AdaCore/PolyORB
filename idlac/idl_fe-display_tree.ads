with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Types; use Idl_Fe.Types;

package Idl_Fe.Display_Tree is

   Offset : constant Natural := 2;

   procedure Disp_Tree (Tree : Node_Id);

   --  display a node list
   procedure Disp_List (List : Node_List; Indent : Natural; Full : Boolean);

private

   --  display the indentation
   procedure Disp_Indent (Indent : Natural; S : String := "");

   --  displays a binary operator
   procedure Disp_Binary (N : Node_Id;
                          Indent : Natural;
                          Full : Boolean;
                          Op : String);

   --  displays a unary operator
   procedure Disp_Unary (N : Node_Id;
                         Indent : Natural;
                         Full : Boolean;
                         Op : String);

end Idl_Fe.Display_Tree;
