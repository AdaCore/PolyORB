with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

with Idl_Fe.Types;
with Idl_Fe.Parser;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;

procedure Idlac is

   procedure Usage;
   --  Display a general help message describing the
   --  idlac command line, and exit to the operating
   --  system with a failure indication.

   procedure Usage is
   begin
      Put_Line ("Usage: " & Command_Name
                & " [-i] [-k] idl_file [-cppargs ...]");
      Put_Line ("  -i    Generate implementation template.");
      Put_Line ("  -k    Keep temporary files.");
      Put_Line ("  -cppargs ARGS");
      Put_Line ("        Pass ARGS to the C++ preprocessor.");
      GNAT.OS_Lib.OS_Exit (1);
   end Usage;

   File_Name : Idl_Fe.Types.String_Cacc;
   Rep : Idl_Fe.Types.Node_Id;

   Generate_Impl_Template : Boolean := False;
   Keep_Temporary_Files : Boolean := False;

begin
   begin
      Initialize_Option_Scan
        ('-', False, "cppargs");

      loop
         case Getopt ("i k") is
            when Ascii.NUL => exit;

            when 'i' =>
               Generate_Impl_Template
                 := True;

            when 'k' =>
               Keep_Temporary_Files
                 := True;

            when others =>
               --  This never happens.
               raise Program_Error;
         end case;
      end loop;

      File_Name := new String'(Get_Argument);
      if File_Name.all'Length = 0 then
         Put_Line ("No file name specified.");
         Usage;
      end if;

      Goto_Section ("cppargs");
      while Getopt ("*") /= Ascii.NUL loop
         --  FIXME: Support passing arguments to CPP.
         --  Add_Preproc_Argument (Full_Switch);
         Put_Line ("Ignoring CPP arg: " & Full_Switch);
      end loop;

   exception
      when Invalid_Switch    =>
         Put_Line ("Invalid Switch " & Full_Switch);
         Usage;
      when Invalid_Parameter =>
         Put_Line ("No parameter for " & Full_Switch);
         Usage;
   end;

   --  Setup parser
   Idl_Fe.Parser.Initialize
     (File_Name.all,
      True,
      Keep_Temporary_Files);

   --  Parse input
   Rep := Idl_Fe.Parser.Parse_Specification;

   --  Expand tree
   Ada_Be.Expansion.Expand_Repository (Rep);

   --  Generate code
   Ada_Be.Idl2Ada.Generate
     (Rep, Implement => Generate_Impl_Template);

end Idlac;
