with GNAT.Command_Line;
with GNAT.OS_Lib;
--  with Ada.Text_IO;     use Ada.Text_IO;

with Debug;           use Debug;
with Errors;          use Errors;
with Lexer;
with Namet;           use Namet;
with Parser;
with Types;           use Types;
with Usage;

procedure Axil is
   File_Desc   : GNAT.OS_Lib.File_Descriptor;
   Source_File : Name_Id := No_Name;
   Root        : Node_Id;

begin

   --  Initialization step
   Namet.Initialize;
   Errors.Initialize;

   Set_Str_To_Name_Buffer (GNAT.Command_Line.Get_Argument);
   if Name_Len /= 0 then
      Source_File := Name_Find;
   else
      Usage;
      return;
   end if;

   Lexer.Preprocess (Source_File, File_Desc);
   Lexer.Process (File_Desc, Source_File);
   Parser.Process (Root);

   Print_Node (Root);
end Axil;
