with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Parser;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;

procedure Idlac is

   procedure Help;

   procedure Help is
   begin
      Put_Line ("Usage: " & Command_Name & " idl_file");
      Set_Exit_Status (Failure);
   end Help;

   File_Name : Idl_Fe.Types.String_Cacc;
   Rep : Idl_Fe.Types.Node_Id;

begin
   if Argument_Count /= 1 then
      Help;
      return;
   end if;

   File_Name := new String'(Argument (1));
   Idl_Fe.Parser.Initialize (File_Name.all,
                             True,
                             False);
   Rep := Idl_Fe.Parser.Parse_Specification;
   Ada_Be.Expansion.Expand_Repository (Rep);
   Ada_Be.Idl2Ada.Generate (Rep, Implement => True);
end Idlac;
