   with Lexer;      use Lexer;
with Types;      use Types;

package body Parser is

   --  Component categories

   Cat_Data        : constant Natural :=  1;
   Cat_Subprogram  : constant Natural :=  2;
   Cat_Thread      : constant Natural :=  3;
   Cat_Threadgroup : constant Natural :=  4;
   Cat_Process     : constant Natural :=  5;
   Cat_Memory      : constant Natural :=  6;
   Cat_Processor   : constant Natural :=  7;
   Cat_Bus         : constant Natural :=  8;
   Cat_Device      : constant Natural :=  9;
   Cat_System      : constant Natural := 10;

   function P_Specification return Node_Id;

   ---------------------
   -- P_Specification --
   ---------------------

   function P_Specification return Node_Id is
      Node : Node_Id;
   begin
      return Node;
   end P_Specification;

   -------------
   -- Process --
   -------------

   procedure Process (Root : out Node_Id) is
   begin
      Root := P_Specification;
   end Process;

end Parser;
