------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                I D L A C                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.IO_Aux;       use GNAT.IO_Aux;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Idl_Fe.Types;
with Idl_Fe.Parser;
with Idl_Fe.Lexer;
with Errors;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;

procedure Idlac is

   procedure Usage;
   --  Display a general help message describing the
   --  idlac command line, and exit to the operating
   --  system with a failure indication.

   procedure Usage is
   begin
      Put_Line (Current_Error, "Usage: " & Command_Name
                & " [-i] [-k] idl_file [-cppargs ...]");
      Put_Line (Current_Error, "  -E    Preprocess only.");
      Put_Line (Current_Error, "  -i    Generate implementation template.");
      Put_Line (Current_Error, "  -k    Keep temporary files.");
      Put_Line (Current_Error, "  -p    Produce source on standard output.");
      Put_Line (Current_Error, "  -q    Be quiet.");
      Put_Line (Current_Error, "  -cppargs ARGS");
      Put_Line (Current_Error, "        Pass ARGS to the C++ preprocessor.");
      Put_Line (Current_Error, "  -I dir is a shortcut for -cppargs -I dir.");
      OS_Exit (1);
   end Usage;

   File_Name : Idl_Fe.Types.String_Cacc;
   Rep       : Idl_Fe.Types.Node_Id;

   Generate_Impl_Template : Boolean := False;
   Keep_Temporary_Files   : Boolean := False;
   Preprocess_Only        : Boolean := False;
   To_Stdout              : Boolean := False;
   Verbose                : Boolean := True;

begin
   begin
      Initialize_Option_Scan
        ('-', False, "cppargs");

      loop
         case Getopt ("E I: i k p q") is
            when ASCII.Nul => exit;

            when 'E' =>
               Preprocess_Only := True;

            when 'I' =>
               Idl_Fe.Lexer.Add_Argument ("-I");
               Idl_Fe.Lexer.Add_Argument (Parameter);

            when 'i' =>
               Generate_Impl_Template
                 := True;

            when 'k' =>
               Keep_Temporary_Files
                 := True;

            when 'p' =>
               To_Stdout := True;

            when 'q' =>
               Verbose := False;

            when others =>
               --  This never happens.
               raise Program_Error;
         end case;
      end loop;

      File_Name := new String'(Get_Argument);
      if File_Name.all'Length = 0 then
         Put_Line (Current_Error, "No file name specified.");
         Usage;
      end if;

      --  The "cppargs" section is processed in
      --  Idl_Fe.Lexer.Initialize.

   exception
      when Invalid_Switch    =>
         Put_Line (Current_Error, "Invalid Switch " & Full_Switch);
         Usage;
      when Invalid_Parameter =>
         Put_Line (Current_Error, "No parameter for " & Full_Switch);
         Usage;
   end;

   --  If file does not exist, issue an error message unless it works after
   --  adding an "idl" extension.

   if not File_Exists (File_Name.all)
     or else not Is_Regular_File (File_Name.all)
   then
      if File_Exists (File_Name.all & ".idl")
        and then Is_Regular_File (File_Name.all & ".idl")
      then
         File_Name := new String'(File_Name.all & ".idl");
      else
         Put_Line (Current_Error, "No such file: " & File_Name.all);
         Usage;
      end if;
   end if;

   if Preprocess_Only then

      --  If we only want to preprocess, let's preprocess, print the content
      --  of the file and exit.

      Idl_Fe.Lexer.Preprocess_File (File_Name.all);
      declare
         use Ada.Text_IO;
         Line : String (1 .. 1024);
         Last : Natural;
      begin
         while not End_Of_File loop
            Get_Line (Line, Last);
            Put_Line (Line (1 .. Last));
         end loop;
      end;

      if not Keep_Temporary_Files then
         Idl_Fe.Lexer.Remove_Temporary_Files;
      end if;

   else

      --  Setup parser
      Idl_Fe.Parser.Initialize
        (File_Name.all,
         True,
         Keep_Temporary_Files);

      --  Parse input
      Rep := Idl_Fe.Parser.Parse_Specification;

      if Errors.Is_Error then

         Put (Current_Error,
              Natural'Image (Errors.Error_Number)
              & " error(s)");
         if Errors.Is_Warning then
            Put_Line
              (Current_Error,
               " and "
               & Natural'Image (Errors.Warning_Number)
               & " warning(s)");
         end if;
         Put_Line (Current_Error, " during parsing.");

         return;

      elsif Verbose then
         if Errors.Is_Warning then
            Put_Line
              (Current_Error,
               Natural'Image (Errors.Warning_Number)
               & " warning(s) during parsing.");
         else
            Put_Line (Current_Error, "Successfully parsed.");
         end if;
      end if;

      --  Expand tree
      Ada_Be.Expansion.Expand_Repository (Rep);

      --  Generate code
      Ada_Be.Idl2Ada.Generate
        (Rep,
         Implement => Generate_Impl_Template,
         To_Stdout => To_Stdout);
   end if;

end Idlac;
