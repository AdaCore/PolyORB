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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

with Idl_Fe.Types;
with Idl_Fe.Parser;
with Idl_Fe.Errors;

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
      Put_Line ("  -q    Be quiet.");
      Put_Line ("  -cppargs ARGS");
      Put_Line ("        Pass ARGS to the C++ preprocessor.");
      GNAT.OS_Lib.OS_Exit (1);
   end Usage;

   File_Name : Idl_Fe.Types.String_Cacc;
   Rep : Idl_Fe.Types.Node_Id;

   Generate_Impl_Template : Boolean := False;
   Keep_Temporary_Files : Boolean := False;
   Verbose : Boolean := True;

begin
   begin
      Initialize_Option_Scan
        ('-', False, "cppargs");

      loop
         case Getopt ("i k q") is
            when Ascii.NUL => exit;

            when 'i' =>
               Generate_Impl_Template
                 := True;

            when 'k' =>
               Keep_Temporary_Files
                 := True;

            when 'q' =>
               Verbose := False;

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

      --  The "cppargs" section is processed in
      --  Idl_Fe.Lexer.Initialize.

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

   if Idl_Fe.Errors.Is_Error then

      Ada.Text_IO.Put
        (Natural'Image (Idl_Fe.Errors.Error_Number) &
         " error(s)");
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line
           (" and " &
            Natural'Image (Idl_Fe.Errors.Warning_Number) &
            " warning(s)");
      end if;
      Ada.Text_IO.Put_Line (" during parsing.");

      return;

   elsif Verbose then
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line
           (Natural'Image (Idl_Fe.Errors.Warning_Number) &
            " warning(s) during parsing.");
      else
         Ada.Text_IO.Put_Line ("Successfully parsed.");
      end if;
   end if;

   --  Expand tree
   Ada_Be.Expansion.Expand_Repository (Rep);

   --  Generate code
   Ada_Be.Idl2Ada.Generate
     (Rep, Implement => Generate_Impl_Template);

end Idlac;
