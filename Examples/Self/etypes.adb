with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Etypes is

   procedure Print (N : access New_Node_Type) is
   begin
      Ada.Text_IO.Put ("Data : ");
      Ada.Integer_Text_IO.Put (N.Data);
      Ada.Text_IO.New_Line;
   end Print;

   Local : New_Node_Type;

begin

   Local.Data := 1996;
   Register (Local.My.Self);

end Etypes;
