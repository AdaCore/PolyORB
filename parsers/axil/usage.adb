with Ada.Command_Line;   use Ada.Command_Line;

with Output;      use Output;

procedure Usage is
begin
   Write_Str  ("Usage: ");
   Write_Str  (Command_Name);
   Write_Line (" source_file");
end Usage;
