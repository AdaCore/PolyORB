--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Idents;
with Tree;
with Tokens;
with Parse;
with Disp;
with Types;
with Be_Ada;

procedure Gidlac is
   procedure Help is
   begin
      Put_Line
        ("usage: " & Command_Name & " [OPTIONS] idl_file");
      Put_Line ("Options are:");
      Put_Line (" -p            enable proprocessor");
      Put_Line (" -save-temps   do not remove temporary files");
      Put_Line (" -g            disp internal tree");
      Set_Exit_Status (Failure);
   end Help;

   Args : Argument_List (1 .. 64);
   Arg_Count : Positive := 1;
   procedure Add_Argument (Str : String) is
   begin
      Args (Arg_Count) := new String'(Str);
      Arg_Count := Arg_Count + 1;
   end Add_Argument;

   Use_Preprocessor : Boolean := False;
   Flag_Save_Temps : Boolean := False;
   Flag_Disp_Tree : Boolean := False;

   Preprocessor : String_Access := null;
   Idl_File : File_Type;
   File_Name : String_Access;
   Rep : Tree.N_Repository_Acc;
begin
   loop
      case Getopt ("p save-temps g I: D: U: A:") is
         when Ascii.NUL =>
            exit;
         when 'p' =>
            Use_Preprocessor := True;
         when 's' =>
            Flag_Save_Temps := True;
         when 'g' =>
            Flag_Disp_Tree := True;
         when 'I' | 'D' | 'U' | 'A' =>
            Add_Argument (Full_Switch & Parameter);
         when others =>
            raise Program_Error;
      end case;
   end loop;

   File_Name := new String'(Get_Argument);
   if File_Name.all'Length = 0 then
      Help;
      return;
   end if;

   if Use_Preprocessor then
      declare
         use GNAT.OS_Lib;
         Spawn_Res : Boolean;
         Fd : File_Descriptor;
         Tmp_File_Name : Temp_File_Name;
      begin
         if Preprocessor = null then
            --  Use the default processor.
            Preprocessor := Locate_Exec_On_Path ("gcc");
            --  Use default options:
            --  -E           only preprocess
            --  -C           do not discard comments
            --  -x c++       use c++ preprocessor semantic
            Add_Argument ("-E");
            Add_Argument ("-C");
            Add_Argument ("-x");
            Add_Argument ("c++");
            Add_Argument ("-ansi");
         end if;
         Create_Temp_File (Fd, Tmp_File_Name);
         if Fd = Invalid_FD then
            Put_Line (Command_Name & ": cannot create temporary file name");
            Set_Exit_Status (Failure);
            return;
         end if;
         --  We don't need the fd.
         Close (Fd);

         Add_Argument ("-o");
         Add_Argument (Tmp_File_Name);
         Args (Arg_Count) := File_Name;
         Spawn (Preprocessor.all, Args (1 .. Arg_Count), Spawn_Res);
         if not Spawn_Res then
            Put_Line (Command_Name & ": preprocessor failed");
            Set_Exit_Status (Failure);
            return;
         end if;
         Open (Idl_File, In_File, Tmp_File_Name);
         Set_Input (Idl_File);
         --  FIXME: Huh, this is perhaps to early.
         if not Flag_Save_Temps then
            Delete_File (Tmp_File_Name'Address, Spawn_Res);
         end if;
      end;
   else
      Open (Idl_File, In_File, File_Name.all);
      Set_Input (Idl_File);
   end if;

   Idents.Initialize;
   Tokens.Initialize;

   Rep := Parse.Parse_Specification;

   if Flag_Disp_Tree then
      Disp.Disp_Tree (Rep.all);
   end if;

   Be_Ada.Generate_Idl (Rep, File_Name.all);

--    loop
--       Tokens.Next_Token;
--       Ada.Text_Io.Put (Idl_Token'Image (Token));
--       Ada.Text_Io.Put (' ');
--       exit when Token = T_Eof;
--       exit when Token = T_Error;
--    end loop;
exception
   when Invalid_Switch =>
      Put_Line ("Invalid Switch " & Full_Switch);
      Help;

   when Invalid_Parameter =>
      Put_Line ("No parameter for " & Full_Switch);
      Help;

   when Name_Error =>
      Put_Line (Command_Name & ": cannot open `" & File_Name.all & ''');
      Set_Exit_Status (Failure);
      return;

   when Types.Fatal_Error =>
      Set_Exit_Status (Failure);
      return;
end Gidlac;
