with Debug;     use Debug;
with Lexer;     use Lexer;
with Nodes;     use Nodes;
with Types;     use Types;
with Utils;     use Utils;

package body AADLDebug is

   -----------
   -- Image --
   -----------

   function Image (N : Component_Category) return String is
   begin
      return Quoted (Image (Token_Type'Val (N)), ''');
   end Image;

   function Image (N : Package_Items) return String is
   begin
      return "Items: " & Image (Items (Node_Id (N))) &
             "Properties: " & Image (Properties (Node_Id (N)));
   end Image;

end AADLDebug;
