with GNAT.Command_Line;
with GNAT.OS_Lib;
with Ada.Text_IO;     use Ada.Text_IO;

with Errors;          use Errors;
with Lexer;           use Lexer;
with Namet;           use Namet;
with Types;           use Types;
with Usage;

procedure AADL is
   File_Desc   : GNAT.OS_Lib.File_Descriptor;
   Source_File : Name_Id := No_Name;
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

   Preprocess (Source_File, File_Desc);
   Process (File_Desc, Source_File);

   loop
      Scan_Token;
      Put (Image (Token));
      Put (' ');
      if Token = T_EOF then
         exit;
      end if;
   end loop;
end AADL;
