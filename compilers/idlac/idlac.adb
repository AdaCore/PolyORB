------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                I D L A C                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Idlac_Flags;       use Idlac_Flags;

with Idl_Fe.Files;
with Idl_Fe.Types;
with Idl_Fe.Parser;
with Errors;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;
with Ada_Be.Mappings.CORBA;

procedure Idlac is

   procedure Usage;
   --  Display a general help message describing the
   --  idlac command line, and exit to the operating
   --  system with a failure indication.

   procedure Usage is
   begin
      Put_Line (Current_Error, "Usage: " & Command_Name
                & " [-E] [-d] [-i] [-k] [-p] [-q] [-noir]"
                & " idl_file [-cppargs ...]");
      Put_Line (Current_Error, "  -E     Preprocess only.");
      Put_Line (Current_Error, "  -d     Generate delegation package.");
      Put_Line (Current_Error, "  -i     Generate implementation template.");
      Put_Line (Current_Error, "  -k     Keep temporary files.");
      Put_Line (Current_Error, "  -p     Produce source on standard output.");
      Put_Line (Current_Error, "  -q     Be quiet.");
      Put_Line (Current_Error, "  -noir  Don't generate code for "
                & "interface repository.");
      Put_Line (Current_Error, "  -gnatW8");
      Put_Line (Current_Error, "         Use UTF8 character encoding");
      Put_Line (Current_Error, "  -cppargs ARGS");
      Put_Line (Current_Error, "         Pass ARGS to the C++ preprocessor.");
      Put_Line (Current_Error, "  -I dir is a shortcut for -cppargs -I dir.");
      OS_Exit (1);
   end Usage;

   File_Name : Idl_Fe.Types.String_Cacc;
   Rep       : Idl_Fe.Types.Node_Id;

begin
   begin
      Initialize_Option_Scan
        ('-', False, "cppargs");

      loop
         case Getopt ("E I: d i k p q noir gnatW8") is
            when ASCII.Nul => exit;

            when 'E' =>
               Preprocess_Only := True;

            when 'I' =>
               declare
                  Success : Boolean;
               begin
                  Idl_Fe.Files.Add_Search_Path (Parameter, Success);
               end;

            when 'd' =>
               Generate_Delegate := True;

            when 'i' =>
               Generate_Impl_Template := True;

            when 'n' =>
               if Full_Switch = "noir" then
                  Generate_IR := False;
               end if;

            when 'k' =>
               Keep_Temporary_Files := True;

            when 'p' =>
               To_Stdout := True;

            when 'q' =>
               Verbose := False;

            when 'g' =>
               Character_Encoding := UTF_8;

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

   File_Name := new String'(Idl_Fe.Files.Locate_IDL_File (File_Name.all));
   if File_Name.all'Length = 0 then
      Put_Line (Current_Error, "No such file: " & File_Name.all);
      Usage;
   end if;

   if Preprocess_Only then

      --  If we only want to preprocess, let's preprocess, print the content
      --  of the file and exit.

      declare
         use Ada.Text_IO;

         Idl_File : File_Type;
         Line     : String (1 .. 1024);
         Last     : Natural;
      begin
         Open
           (Idl_File, In_File, Idl_Fe.Files.Preprocess_File (File_Name.all));
         Set_Input (Idl_File);

         while not End_Of_File loop
            Get_Line (Line, Last);
            Put_Line (Line (1 .. Last));
         end loop;

         Delete (Idl_File);
      end;

   else

      --  Setup parser

      Idl_Fe.Parser.Initialize (File_Name.all);

      --  Parse input

      Rep := Idl_Fe.Parser.Parse_Specification;

      if Errors.Is_Error then
         Put (Current_Error,
              Natural'Image (Errors.Error_Number)
              & " error(s)");
         if Errors.Is_Warning then
            Put
              (Current_Error,
               " and "
               & Natural'Image (Errors.Warning_Number)
               & " warning(s)");
         end if;
         Put_Line (Current_Error, " during parsing.");

      else
         if Verbose then
            if Errors.Is_Warning then
               Put_Line
                 (Current_Error,
                  Natural'Image (Errors.Warning_Number)
                  & " warning(s) during parsing.");
            else
               Put_Line (Current_Error, "Successfully parsed.");
            end if;
         end if;

         --  Expand tree. This should not cause any errors!

         Ada_Be.Expansion.Expand_Repository (Rep);
         pragma Assert (not Errors.Is_Error);

         --  Generate code

         Ada_Be.Idl2Ada.Generate
           (Use_Mapping => Ada_Be.Mappings.CORBA.The_CORBA_Mapping,
            Node        => Rep,
            Implement   => Generate_Impl_Template,
            Intf_Repo   => Generate_IR,
            To_Stdout   => To_Stdout);

         Idl_Fe.Parser.Finalize;
      end if;
   end if;
end Idlac;
