--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Command_Line; use Ada.Command_Line;
--  with Types; use Types;
--  with Tree;
--  with Tokens; use Tokens;
--  with Parse;

with Ada_Be;
pragma Warnings (Off, Ada_Be);

procedure Idlac is

   procedure Help is
   begin
      Put_Line ("usage: " & Command_Name & " idl_file");
      Set_Exit_Status (Failure);
   end Help;

--
--     Idl_File: File_Type;
--     File_Name: String_Cacc;
--     Rep : Tree.N_Repository_Acc;
--  begin
--     if Argument_Count /= 1 then
--        Help;
--        return;
--     end if;
--
--     File_Name := new String'(Argument (1));
--     Open (Idl_File, In_File, File_Name.all);
--     Set_Input (Idl_File);
--
--     Rep := Parse.Parse_Specification;
--
--     -- Tree.Disp_Tree (Rep.all, P);
--
--     Be_Ada.Generate_Idl (Rep, File_Name.all);
--
--  --    loop
--  --       Tokens.Next_Token;
--  --       Ada.Text_Io.Put (Idl_Token'Image (Token));
--  --       Ada.Text_Io.Put (' ');
--  --       exit when Token = T_Eof;
--  --       exit when Token = T_Error;
--  --    end loop;
--     exception
--     when Name_Error =>
--        Put_Line (Command_Name & ": cannot open `" & File_Name.all & ''');
--        Set_Exit_Status (Failure);
--        return;
begin
   null;
end Idlac;
