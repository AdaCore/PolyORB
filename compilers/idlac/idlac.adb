------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                I D L A C                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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
with Idlac_Errors;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;
with Ada_Be.Mappings.CORBA;
with Ada_Be.Source_Streams;

with Platform;

procedure Idlac is

   procedure Usage;
   --  Display a general help message describing the
   --  idlac command line, and exit to the operating
   --  system with a failure indication.

   procedure Usage is
   begin
      Put_Line (Current_Error, "IDLAC from PolyORB " & Platform.Version);
      Put_Line (Current_Error, "Usage: " & Command_Name
                & " [-Edikpqv] [-[no]ir] [-gnatW8] [-o DIR]"
                & " idl_file [-cppargs ...]");
      Put_Line (Current_Error, "  -E     Preprocess only.");
      Put_Line (Current_Error, "  -d     Generate delegation package.");
      Put_Line (Current_Error, "  -i     Generate implementation template.");
      Put_Line (Current_Error, "  -s     Generate server side code.");
      Put_Line (Current_Error, "  -c     Generate client side code.");
      Put_Line (Current_Error, "  -k     Keep temporary files.");
      Put_Line (Current_Error, "  -p     Produce source on standard output.");
      Put_Line (Current_Error, "  -q     Be quiet (default).");
      Put_Line (Current_Error, "  -v     Be verbose.");
      Put_Line (Current_Error, "  -ir    Generate code for "
                & "interface repository.");
      Put_Line (Current_Error, "  -noir  Don't generate code for "
                & "interface repository (default).");
      Put_Line (Current_Error, "  -gnatW8");
      Put_Line (Current_Error, "         Use UTF8 character encoding.");
      Put_Line (Current_Error, "  -o DIR Specify output directory.");
      Put_Line (Current_Error, "  -cppargs ARGS");
      Put_Line (Current_Error, "         Pass ARGS to the C++ preprocessor.");
      Put_Line (Current_Error, "  -I dir is a shortcut for -cppargs -I dir.");
      OS_Exit (1);
   end Usage;

   File_Name        : Idl_Fe.Types.String_Cacc;
   Rep              : Idl_Fe.Types.Node_Id;

begin
   begin
      Initialize_Option_Scan ('-', False, "cppargs");

      loop
         case Getopt ("E I: c d i k p q s v ir noir o: gnatW8") is
            when ASCII.NUL => exit;

            when 'E' =>
               Preprocess_Only := True;

            when 'd' =>
               Generate_Delegate := True;

            when 'i' =>
               if Full_Switch = "i" then
                  Generate_Impl_Template := True;

               elsif Full_Switch = "ir" then
                  Generate_IR := True;
               end if;

            when 'c' =>
               Generate_Client_Code := True;

            when 's' =>
               Generate_Server_Code := True;

            when 'k' =>
               Keep_Temporary_Files := True;

            when 'p' =>
               To_Stdout := True;

            when 'q' =>
               --  For backward compatibility we just ignore this switch
               Verbose := False;

            when 'v' =>
               Verbose := True;

            when 'g' =>
               Character_Encoding := UTF_8;

            when 'n' =>
               if Full_Switch = "noir" then
                  --  For backward compatibility we just ignore this switch
                  Generate_IR := False;
               end if;

            when 'o' =>
               if not Ada_Be.Source_Streams.Set_Output_Directory
                        (Parameter)
               then
                  raise Invalid_Parameter;
               end if;

            when 'I' =>
               declare
                  Success : Boolean;
               begin
                  Idl_Fe.Files.Add_Search_Path (Parameter, Success);
                  if not Success then
                     raise Program_Error
                       with Parameter & ": directory not found";
                  end if;
               end;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      --  Force generation of client and server side code if at least one
      --  from client, server or implementation template not selected.

      if not Generate_Client_Code
        and then not Generate_Server_Code
        and then not Generate_Impl_Template
      then
         Generate_Client_Code := True;
         Generate_Server_Code := True;
      end if;

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

   declare
      File_Loc : constant String :=
                   Idl_Fe.Files.Locate_IDL_File (File_Name.all);
   begin
      if File_Loc'Length = 0 then
         Put_Line (Current_Error, "No such file: " & File_Name.all);
         Usage;
      end if;
      File_Name := new String'(File_Loc);
   end;

   if Preprocess_Only then

      --  If we only want to preprocess, let's preprocess, print the content
      --  of the file and exit.

      declare
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

      if Idlac_Errors.Is_Error then
         Put (Current_Error,
              Natural'Image (Idlac_Errors.Error_Number)
              & " error(s)");
         if Idlac_Errors.Is_Warning then
            Put
              (Current_Error,
               " and "
               & Natural'Image (Idlac_Errors.Warning_Number)
               & " warning(s)");
         end if;
         Put_Line (Current_Error, " during parsing.");

      else
         if Verbose then
            if Idlac_Errors.Is_Warning then
               Put_Line
                 (Current_Error,
                  Natural'Image (Idlac_Errors.Warning_Number)
                  & " warning(s) during parsing.");
            else
               Put_Line (Current_Error, "Successfully parsed.");
            end if;
         end if;

         --  Expand tree. This should not cause any errors!

         Ada_Be.Expansion.Expand_Repository (Rep);
         pragma Assert (not Idlac_Errors.Is_Error);

         --  Generate code

         Ada_Be.Idl2Ada.Generate
           (Use_Mapping => Ada_Be.Mappings.CORBA.The_CORBA_Mapping,
            Node        => Rep,
            Implement   => Generate_Impl_Template,
            Intf_Repo   => Generate_IR,
            To_Stdout   => To_Stdout);

      end if;

      Idl_Fe.Parser.Finalize;

      if Idlac_Errors.Is_Error then
         OS_Exit (2);
      end if;
   end if;
end Idlac;
