with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
--  with Tree;
with Tokens; use Tokens;
--  with Parse;
--  with Disp;
--  with Types;

procedure testlexer is

   Idl_File : File_Type;
   File_Name : String_Access;
--   Rep : Tree.N_Repository_Acc;
begin
   File_Name := new String'(Get_Argument);
   if File_Name.all'Length = 0 then
      return;
   end if;

   Open (Idl_File, In_File, File_Name.all);
   Set_Input (Idl_File);

   loop
      Tokens.Next_Token;
      Ada.Text_Io.Put_Line (Idl_Token'Image (Token));
      exit when Token = T_Eof;
   end loop;
--   Rep := Parse.Parse_Specification;
--   Disp.Disp_Tree (Rep.all);
end testlexer;
