------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                  I A C                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2011, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Command_Line;  use Ada.Command_Line;

with GNAT.Command_Line;            use GNAT.Command_Line;
with GNAT.Directory_Operations;    use GNAT;
with GNAT.OS_Lib;                  use GNAT.OS_Lib;
with GNAT.Perfect_Hash_Generators; use GNAT.Perfect_Hash_Generators;

with Analyzer;     use Analyzer;
with Backend;      use Backend;
with Errors;       use Errors;
with Flags;        use Flags;
with Lexer;
with Namet;        use Namet;
with Output;       use Output;
with Parser;       use Parser;
with Scopes;
with Source_Input; use Source_Input;
with Types;        use Types;
with Usage;
with Utils;        use Utils;

with Frontend.Debug;

with Backend.Config;
with Backend.BE_CORBA_Ada;
with Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_IDL;
with Backend.BE_Types;

procedure IAC is

   procedure Command_Line_Error (Template : Message_Template);
   procedure Command_Line_Error (Template : Message_Template; S : String);
   --  Print out the error message and exit the program with non-zero status

   procedure Scan_Switches;
   --  Scan command line switches and update Flags accordingly. If the help
   --  option (-h) is given, this does not return, but calls OS_Exit.

   ------------------------
   -- Command_Line_Error --
   ------------------------

   procedure Command_Line_Error (Template : Message_Template) is
   begin
      DE (Template);
      DE ("\type ""iac -h"" for help");
      OS_Exit (1);
   end Command_Line_Error;

   ------------------------
   -- Command_Line_Error --
   ------------------------

   procedure Command_Line_Error (Template : Message_Template; S : String) is
   begin
      DE (Template, S);
      DE ("\type ""iac -h"" for help");
      OS_Exit (1);
   end Command_Line_Error;

   -------------------
   -- Scan_Switches --
   -------------------

   procedure Scan_Switches is
      package BEA renames Backend.BE_CORBA_Ada;
      package BEI renames Backend.BE_IDL;
      package BET renames Backend.BE_Types;

      Found_Language : Boolean := False;
   begin

      --  Add the current directory to the search path, it will be added
      --  automatically to the preprocessor search path

      Lexer.Add_IAC_Search_Path (".");

      --  The command line parsing in IAC is a bit complicated. The
      --  structure of the command line is as follows :

      --  %iac [general_switches] [-<backend>] [backend_switches] file
      --       [-cppargs preprocessor_flags]

      --  Check whether the user specified the Backend to use ...

      Initialize_Option_Scan;
      for I in 1 .. Argument_Count loop
         Set_Str_To_Name_Buffer (Argument (I));
         if Name_Buffer (1) = '-'
           and then Name_Len > 1
           and then Backend.Is_Valid_Language (Name_Buffer (2 .. Name_Len))
         then
            if Found_Language then
               raise Invalid_Switch;
            end if;
            Backend.Set_Current_Language (Name_Buffer (2 .. Name_Len));
            Found_Language := True;
         end if;
      end loop;

      Initialize_Option_Scan ('-', False, "cppargs");
      loop
         case Getopt ("b: c d da db df di dm ds dt dw "
                      & "E e h hc hm I: i k o: p q r! s "
                      & "ada gnatW8 idl ir noir nocpp types") is

            when ASCII.NUL =>
               exit;

            when 'b' =>
               begin
                  BEI.Default_Base := Natural'Value (Parameter);
               exception
                  when Constraint_Error =>
                     raise Invalid_Parameter;
               end;

            when 'c' =>
               BEA.Disable_Server_Code_Gen := True;

            when 'd' =>
               if Full_Switch = "d" then
                  --  XXX should support the generation of delegate package
                  null;
               elsif Full_Switch = "da" then
                  BEA.Print_Ada_Tree := True;

               elsif Full_Switch = "db" then
                  BEA.Disable_Pkg_Spec_Gen := True;

               elsif Full_Switch = "df" then
                  Print_Full_Tree := True;
                  BEI.Print_IDL_Tree := True;

               elsif Full_Switch = "di" then
                  BEA.Generate_Imported := True;
                  BEI.Generate_Imported := True;

               elsif Full_Switch = "dm" then
                  Scopes.D_Scopes := True;

               elsif Full_Switch = "ds" then
                  BEA.Disable_Pkg_Body_Gen := True;

               elsif Full_Switch = "dt" then
                  BEA.Output_Tree_Warnings := True;

               elsif Full_Switch = "dw" then
                  BEA.Output_Unit_Withing := True;

               else
                  raise Invalid_Switch;
               end if;

            when 'E' =>
               Preprocess_Only := True;

            when 'e' =>
               BEI.Expand_Tree := True;

            when 'g' =>
               if Full_Switch = "gnatW8" then
                  BEA.Nutils.Set_UTF_8_Encoding;
               else
                  raise Program_Error;
               end if;

            when 'I' =>

               --  We add the parameter WITHOUT the ending
               --  directory separator

               if Is_Dir_Separator (Parameter (Parameter'Last)) then
                  Lexer.Add_IAC_Search_Path
                    (Parameter (Parameter'First .. Parameter'Last - 1));
               else
                  Lexer.Add_IAC_Search_Path (Parameter);
               end if;

            when 'h' =>
               --  Help switch (-h) prints usage message and exits the program
               --  without doing anything else.

               if Full_Switch = "h" then
                  Usage;
                  OS_Exit (0);

               elsif Full_Switch = "hc" then
                  BEA.Optimization_Mode := CPU_Time;

               elsif Full_Switch = "hm" then
                  BEA.Optimization_Mode := Memory_Space;

               else
                  --  Can't get here, because we already processed the '-h'
                  --  switch.

                  raise Program_Error;
               end if;

            when 'i' =>
               if Full_Switch = "i" then
                  BEA.Impl_Packages_Gen := True;

               elsif Full_Switch = "ir" then
                  BEA.IR_Info_Packages_Gen := True;
               end if;

            when 'k' =>
               Lexer.Keep_TMP_Files := True;

            when 'n' =>
               if Full_Switch = "noir" then
                  BEA.IR_Info_Packages_Gen := False;

               elsif Full_Switch = "nocpp" then
                  No_Preprocess := True;
               end if;

            when 'o' =>
               if GNAT.OS_Lib.Is_Directory (Parameter) then
                  if Is_Dir_Separator (Parameter (Parameter'Last)) then
                     Output_Directory := new String'(Parameter);
                  else
                     Output_Directory := new String'
                       (Parameter & Directory_Operations.Dir_Separator);
                  end if;
               else
                  Command_Line_Error ("%: directory not found", Parameter);
               end if;

            when 'p' =>
               if Backend.Current_Language = "types" then
                  BET.Print_Types := True;
               end if;
               Use_Stdout := True;

            when 'q' =>
               Quiet := True;

            when 'r' =>
               --  ??? Needs documentation

               declare
                  S : constant String := Parameter;
               begin
                  case S (S'First) is
                     when 's' =>
                        BEA.Use_SII := True;

                     when 'd' =>
                        BEA.Use_SII := False;

                     when 'o' =>
                        --  Buffers allocation optimization can be
                        --  used only with SII/SSI invocation

                        BEA.Use_SII := True;
                        BEA.Use_Optimized_Buffers_Allocation := True;

                     when 'a' =>
                        --  Marshalling optimization can be
                        --  used only with SII/SSI invocation

                        BEA.Use_Compiler_Alignment := True;
                        BEA.Use_SII := True;
                        BEA.Use_Optimized_Buffers_Allocation := True;

                     when others =>
                        raise Invalid_Switch;
                  end case;
               end;

            when 's' =>
               BEA.Disable_Client_Code_Gen := True;

            when others =>
               if Full_Switch /= "ada"
                 and then Full_Switch /= "idl"
                 and then Full_Switch /= "types"
               then
                  raise Invalid_Switch;
               end if;
         end case;
      end loop;

      --  When the user gives both "-s" and "-c", we generate the code
      --  for both client side and server side

      if BEA.Disable_Client_Code_Gen
        and then BEA.Disable_Server_Code_Gen
      then
         BEA.Disable_Client_Code_Gen := False;
         BEA.Disable_Server_Code_Gen := False;
      end if;

      --  When the user gives both "-db" and "-ds" we generate the
      --  code for both specs and bodies

      if BEA.Disable_Pkg_Body_Gen
        and then BEA.Disable_Pkg_Spec_Gen
      then
         BEA.Disable_Pkg_Body_Gen := False;
         BEA.Disable_Pkg_Spec_Gen := False;
      end if;

      Set_Str_To_Name_Buffer (Get_Argument);
      if Name_Len /= 0 then
         Main_Source := Name_Find;

         if Get_Argument /= "" then
            Command_Line_Error ("only one file name allowed");
         end if;
      end if;

   --  These exceptions can come from Getopt, or be raised explicitly above

   exception
      when Invalid_Switch =>
         Command_Line_Error ("invalid switch: %", Full_Switch);
      when Invalid_Parameter =>
         Command_Line_Error
           ("invalid or missing parameter for switch: %", Full_Switch);
   end Scan_Switches;

   Preprocessed_File : Source_File_Ptr;

--  Start of processing for IAC

begin
   --  Initialization step

   Namet.Initialize;
   Backend.Config.Initialize;
   Scan_Switches;
   Scopes.Initialize;

   if Main_Source = No_Name then
      Command_Line_Error ("missing .idl input file");
   end if;

   Get_Name_String (Main_Source);
   if not Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
      Add_Str_To_Name_Buffer (".idl");
      if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         Main_Source := Name_Find;
      else
         Error_Name (1) := Main_Source;
         Command_Line_Error ("%not found");
      end if;
   end if;

   declare
      First, Last : Natural;
   begin
      Get_Name_String (Main_Source);

      --  Remove any prefix

      First := 1;
      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '/'
           or else Name_Buffer (J) = '\'
         then
            First := J + 1;
            exit;
         end if;
      end loop;

      --  Remove any suffix.
      --
      --  Implementation note: we do not want any '.' character left
      --  in the specification unit name since this would require to
      --  define the parent unit as well.

      Last := Name_Len;
      for J in First .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Last := J - 1;
            exit;
         end if;
      end loop;

      Set_Str_To_Name_Buffer (Name_Buffer (First .. Last));
      Scopes.IDL_Spec_Name := Name_Find;
   end;

   --  The "cppargs" section is processed in Lexer.Preprocess

   --  Preprocessor step

   Preprocessed_File := Lexer.Preprocess (Main_Source);

   if Preprocess_Only then
      Copy_To_Standard_Output (Preprocessed_File.all);
      return;
   end if;

   --  Lexer step

   Lexer.Process (Preprocessed_File);

   --  Parser step

   Parser.Process (Scopes.IDL_Spec);

   --  Analyzer step

   Analyze (Scopes.IDL_Spec);

   if Print_Full_Tree then
      Frontend.Debug.W_Full_Tree;
   end if;

   if N_Errors > 0 then
      Error_Int (1) := N_Errors;

      if N_Warnings > 0 then
         Error_Int (2) := N_Warnings;
         DE ("$ error(s) and $ warning(s)");
      else
         DE ("$ error(s)");
      end if;

      OS_Exit (2);

   elsif N_Warnings > 0 then
      Error_Int (1) := N_Warnings;
      DE ("$ warning(s)");
   end if;

   Generate (Scopes.IDL_Spec);

exception
   --  We don't print a bug box on Fatal_Error, because an error message has
   --  already been issued. We just set the exit status to non-zero.

   when Fatal_Error =>
      OS_Exit (2);

   --  Other exceptions are considered bugs. Print a "bug box", and exit with
   --  non-zero exit status.

   when E : others =>
      declare
         Exception_String : constant String :=
           "| Detected exception: "
           & Ada.Exceptions.Exception_Name (E);
         Error_String : constant String :=
           "| Error: "
           & Ada.Exceptions.Exception_Message (E);
      begin
         --  To avoid generating the bug box inside a file

         Set_Standard_Error;

         Write_Line ("+============================ IAC BUG DETECTED"
                     & " ===========================+");

         Write_Str (Exception_String);
         for J in Exception_String'Length .. 72 loop
            Write_Str (" ");
         end loop;
         Write_Str (" |");
         Write_Eol;

         Write_Str (Error_String);
         for J in Error_String'Length .. 72 loop
            Write_Str (" ");
         end loop;
         Write_Str (" |");
         Write_Eol;

         Write_Str ("| Please include the files listed below when submitting"
                    & " your report.      |");
         Write_Eol;
         Write_Str ("| Please refer to the User's Guide for more details."
                    & "                      |");
         Write_Eol;
         Write_Line ("+============================================="
                     & "============================+");
         Write_Eol;
         Write_Name (Main_Source);
         Write_Eol;

         OS_Exit (3);
      end;

end IAC;
