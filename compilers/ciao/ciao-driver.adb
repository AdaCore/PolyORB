----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  Main subprogram for the CIAO generation tool.
--  Some code is taken from display-source, gnatstub and gnatelim.
--  $Id: //depot/ciao/main/ciao-driver.adb#12 $

with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Fixed;     use Ada.Strings.Wide_Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Wide_Text_IO;          use Ada.Wide_Text_IO;

with Asis;
with Asis.Exceptions;
with Asis.Errors;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Declarations;
with Asis.Text;

with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with CIAO.Filenames;             use CIAO.Filenames;
with CIAO.IDL_Tree;
with CIAO.Namet;
with CIAO.Nlists;
with CIAO.Options;               use CIAO.Options;
with CIAO.Types;                 use CIAO.Types;
with CIAO.Translator;            use CIAO.Translator;
with CIAO.Generator;             use CIAO.Generator;
with CIAO.Generator.IDL;
with CIAO.Generator.Proxy;

with CIAO.Generator.Broca;

procedure CIAO.Driver is

   package Proxy_Generator is new CIAO.Generator.Proxy
     (CIAO.Generator.Broca.ORB_Deps);

   Parameter_Error : exception;

   IDL_File    : Ada.Text_IO.File_Type;
   Tree_File   : Ada.Text_IO.File_Type;
   Spec_File   : Ada.Text_IO.File_Type;
   Form        : String := "";

   CIAO_Context : Asis.Context;

   -------------------------
   --  Local subprograms  --
   -------------------------

   procedure Clean;
   --  Does the ASIS finalization steps (Close->Dissociate->Finalize)
   --  and possibly delete the tree file. Called on all exits,
   --  erroneous or not.

   procedure Put_CIAO_Version;
   --  Displays the version information for CIAO and
   --  the underlying ASIS implementation.

   procedure Brief_Help;
   --  Displays a help message describing the command line
   --  options.

   procedure Initialize;
   --  Scan command line arguments and set up the
   --  context for the translation.

   procedure Make_Unit_Name (CU_Name : in out String);
   --  Change a GNAT source file name to an Ada
   --  compilation unit name by replacing all '-'s to '.'s.

   procedure Check_Parameters;
   --  Checks that command-line options and files existing in the file
   --  system fit each other. If the check fails, generates the diagnostic
   --  message and raises Parameter_Error

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

   -------------
   --  Clean  --
   -------------

   procedure Clean is
   begin
      if Asis.Ada_Environments.Is_Open (CIAO_Context) then
         Asis.Ada_Environments.Close (CIAO_Context);
      end if;
      Asis.Ada_Environments.Dissociate (CIAO_Context);
      Asis.Implementation.Finalize;

      if Delete_Tree and then Tree_Exists then
         Ada.Text_IO.Open (Tree_File, Ada.Text_IO.In_File,
                           Tree_Name.all, Form);
         Ada.Text_IO.Delete (Tree_File);
      end if;

      --  CIAO.IDL_Tree.Finalize;
      CIAO.Namet.Finalize;
      --  CIAO.Nlists.Finalize;
   end Clean;

   ----------------------
   -- Put_CIAO_Version --
   ----------------------

   procedure Put_CIAO_Version is
   begin
      Put_Line ("CIAO version " & CIAO.Version);
      Put_Line (CIAO.Copyright);
      New_Line;
      Put_Line (Asis.Implementation.ASIS_Version & " by "
                & Asis.Implementation.ASIS_Implementor & ":");
      Put_Line (Asis.Implementation.ASIS_Implementor_Version);
   end Put_CIAO_Version;

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      Put_Line ("Usage: ciao [opts] filename [directory]");
      New_Line;
      Put_Line ("  filename  source file");
      Put      ("  directory directory to place a sample body");
      Put_Line (" (default is the current directory)");
      New_Line;
      Put_Line ("CIAO options:");
      New_Line;
      Put_Line ("  -f     replace existing generated files");
      Put_Line ("  -Idir  source search dir, has the same meaning as for "
                       & "gcc and gnatmake");
      Put_Line ("  -I-    do not look for the sources in the default "
                       & "directory");
      Put_Line ("  -in    (n in 1 .. 9) number of spaces used for identation "
                       & "in a generated file");
      Put_Line ("  -q     quiet mode - do not confirm creating a body");
      Put_Line ("  -r     reuse existing tree file");
      Put_Line ("  -k     do not delete tree file");
      Put_Line ("         (-r also implies -k)");
      Put_Line ("  -t     overwrite the existing tree file");
      Put_Line ("  -v     verbose mode - output the version information");
   end Brief_Help;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      use GNAT.Command_Line;

      Switch_Parameter : Natural;

      function Get_Switch_Parameter (Val : String) return Natural;
      --  computes a natural parameter for switch from its string
      --  representation. Raises Parameter_Error if Val can not be considered
      --  as a string image of a natural number. This function supposes that
      --  Val is not an empty string.

      function Get_Switch_Parameter (Val : String) return Natural is
         Result : Natural := 0;
      begin
         for I in Val'Range loop

            if Val (I) not in '0' .. '9' then
               Ada.Text_IO.Put_Line
                 ("CIAO: wrong switch integer parameter " & Val);
               raise Parameter_Error;
            else
               Result := Result * 10 +
                  Character'Pos (Val (I)) - Character'Pos ('0');
            end if;

         end loop;

         return Result;

      end Get_Switch_Parameter;

   begin
      if Argument_Count = 0 then
         Brief_Help;
         --  Initialized remains False here!
      else
         --  first, scanning the command line parameters:
         loop
            case Getopt ("f I: i: k l: q r t v") is
               when Ascii.NUL =>
                  exit;

               when 'f' =>
                  Overwrite_Body := True;
               when 'I' =>
                  Free (I_Options_Tmp);
                  I_Options_Tmp :=
                    new String'(I_Options.all & " " & Parameter);
                  Free (I_Options);
                  I_Options := new String'(I_Options_Tmp.all);
                  Dir_Count := Dir_Count + 1;

               when 'i' =>
                  Switch_Parameter :=
                    Get_Switch_Parameter (Parameter);

                  if Switch_Parameter < Min_Indent_Level then
                     Ada.Text_IO.Put ("CIAO: identation level is too small (");
                     Ada.Text_IO.Put (Parameter);
                     Ada.Text_IO.Put_Line (")");
                     raise Parameter_Error;
                  elsif Switch_Parameter > Max_Indent_Level  then
                     Ada.Text_IO.Put ("CIAO: identation level is too large (");
                     Ada.Text_IO.Put      (Parameter);
                     Ada.Text_IO.Put_Line (")");
                     raise Parameter_Error;
                  else
                     Indent_Level := Switch_Parameter;
                  end if;

               when 'k' =>
                  Delete_Tree := False;
               when 'q' =>
                  Quiet_Mode := True;
               when 'r' =>
                  Reuse_Tree := True;
               when 't' =>
                  Overwrite_Tree := True;
               when 'v' =>
                  Verbose_Mode := True;

               when others =>
                  Ada.Text_IO.Put_Line ("CIAO: unknown option " & Parameter);
                  raise Parameter_Error;
            end case;
         end loop;

         declare
            S : constant String := Get_Argument (Do_Expansion => True);

         begin
            if S'Length > 0 then
               File_Name := new String'(S);
            end if;
         end;

         --  then, checking, that parameters are valid and that they
         --  corresponds to the situation in the file system
         Check_Parameters;

         --  Open output files
         Ada.Text_IO.Create (IDL_File, Ada.Text_IO.Out_File,
                             IDL_Name.all, Form);

         Initialized := True;
      end if;

   exception
      when Parameter_Error =>
         Initialized := False;
         --  nothing else to do!
      when others =>
         Initialized := False;
         raise;
   end Initialize;

   --------------------
   -- Make_Unit_Name --
   --------------------

   procedure Make_Unit_Name (CU_Name : in out String) is
   begin
      for I in CU_Name'Range loop
         if CU_Name (I) = '-' then
            CU_Name (I) := '.';
         end if;
      end loop;
   end Make_Unit_Name;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is
      Ind : Integer;

      I_Len : Natural;
      Next_Dir_Start : Natural := 2;
      Next_Dir_End : Natural := 2;
      --  "2 is because of the leading ' '  "
   begin

      --  First, check if the source file is set
      if File_Name = null then
         Brief_Help;
         raise Parameter_Error;
      end if;

      --  Then, checking if the argument file follows the GNAT file name
      --  conventions:
      File_Name_Len := File_Name'Length;
      File_Name_First := File_Name'First;
      File_Name_Last := File_Name'Last;

      if not (File_Name_Len  >= 5 and then
              File_Name (File_Name_Last - 3 .. File_Name_Last) = ".ads")
      then
         Ada.Text_IO.Put_Line ("CIAO: " & File_Name.all &
                  " is not a name of a spec file");
         raise Parameter_Error;
      end if;

      --  checking if the file to process really exists:
      if not Is_Regular_File (File_Name.all) then
         Ada.Text_IO.Put_Line ("CIAO: cannot find " & File_Name.all);
         raise Parameter_Error;
      end if;

      --  if destination is set, check if the destination directory exists:
      if Destination_Dir /= null then
         if not Is_Directory (Destination_Dir.all) then
            Ada.Text_IO.Put_Line ("CIAO: " & Destination_Dir.all &
                                  " does not exist");
            raise Parameter_Error;
         end if;
      end if;

      --  and now, we have to compute some names before continuing checking:
      Ind := File_Name_First;

      for I in reverse File_Name_First .. File_Name_Last loop
         if File_Name (I) = Directory_Separator then
            Ind := I + 1;
            exit;
         end if;
      end loop;

      Short_File_Name := new String'(File_Name (Ind .. File_Name_Last));
      Short_File_Name_Len   := Short_File_Name'Length;
      Short_File_Name_First := Short_File_Name'First;
      Short_File_Name_Last  := Short_File_Name'Last;

      if Destination_Dir = null then
         IDL_Name := new String'(IDL_File_Name (Short_File_Name.all));
      else
         IDL_Name := new String'
                         (Destination_Dir.all &
                          Directory_Separator &
                          IDL_File_Name (Short_File_Name.all));
      end if;

      --  checking if a body already exists:

      if Is_Regular_File (IDL_Name.all) then

         if Overwrite_Body then
            Ada.Text_IO.Open (IDL_File, Ada.Text_IO.Out_File,
                              IDL_Name.all, Form);
            Ada.Text_IO.Delete (IDL_File);
         else
            Ada.Text_IO.Put_Line ("CIAO: the body for " & File_Name.all
                   & " already exists");
            Ada.Text_IO.Put_Line ("          use -f to overwrite it");
            raise Parameter_Error;
         end if;

      end if;

      --  now, checking the situation with the tree file:
      Tree_Name := new String'(Short_File_Name.all);

      --  This was for ASIS-for-GNAT 3.11p
      --  Tree_Name (Tree_Name'Last - 1) := 't';

      --  This is for ASIS-for-GNAT 3.12p
      Tree_Name (Tree_Name'Last) := 't';

      if Is_Regular_File (Tree_Name.all) then
         Tree_Exists := True;
         if not (Reuse_Tree or else Overwrite_Tree) then
            Ada.Text_IO.Put_Line
              ("CIAO: " & Tree_Name.all & " already exists");
            Ada.Text_IO.Put_Line
              ("           use -r or -t to reuse or to overwrite it");

            raise Parameter_Error;
         end if;
      else
         if Reuse_Tree then
            Ada.Text_IO.Put_Line ("CIAO: cannot find " & Tree_Name.all
                   & " (-r is set)");
            raise Parameter_Error;
         end if;
      end if;

      if Reuse_Tree then
         Delete_Tree := False;
         Overwrite_Tree := False;
      end if;

      --  now, converting '-I' options from a string into argument list

      if Dir_Count = 0 then
         Arg_List := new Argument_List (1 .. 0);
      else
         Arg_List := new Argument_List (1 .. Dir_Count);
         I_Len := I_Options'Length;
         for I in 1 .. Dir_Count loop
            while (Next_Dir_End <= I_Len and then
                   I_options (Next_Dir_End) /= ' ')
            loop
               Next_Dir_End := Next_Dir_End + 1;
            end loop;
            Next_Dir_End := Next_Dir_End - 1;
            Arg_list (I) :=
               new String'(I_options (Next_Dir_Start .. Next_Dir_End));
               Next_Dir_Start := Next_Dir_End + 2;
               Next_Dir_End := Next_Dir_Start;
         end loop;
      end if;

      --  Cleaning up - freeing what we will not need any more
      Free (Destination_Dir);
      Free (I_Options);
      Free (I_Options_Tmp);

   end Check_Parameters;

   ---------------------
   -- Local variables --
   ---------------------

   Library_Unit : Asis.Compilation_Unit;
   IDL_Tree     : Node_Id;

begin  --  CIAO.Driver's body.

   --  CIAO initializations
   CIAO.Namet.Initialize;
   CIAO.Nlists.Initialize;
   CIAO.IDL_Tree.Initialize;
   Initialize;

   if not Initialized then
      --  Cannot do much...
      return;
   end if;

   --  ASIS Initialization
   Asis.Implementation.Initialize;
   declare
      Param : String_Ptr;
   begin
      if Overwrite_Tree then
         Param := new String'("-CA -FS");
      elsif Reuse_Tree then
         Param := new String'("-C1 " & Tree_Name.all);
      else
         Param := new String'("-CA -FM");
      end if;
      Asis.Ada_Environments.Associate
        (The_Context => CIAO_Context,
         Name        => "CIAO_Context",
         Parameters  => To_Wide_String (Param.all));
      Free (Param);
   end;
   Asis.Ada_Environments.Open (CIAO_Context);

   declare
      Library_Unit_Name_Len : Positive := Short_File_Name_Len - 4;
      --  "- 4" stands for ".ads"
      Library_Unit_Name : String (1 .. Library_Unit_Name_Len) :=
        Short_File_Name.all
          (Short_File_Name_First .. Short_File_Name_Last - 4);
   begin
      --  Open requested library unit.
      Make_Unit_Name (Library_Unit_Name);
      Library_Unit := Asis.Compilation_Units.Library_Unit_Declaration
        (To_Wide_String (Library_Unit_Name), CIAO_Context);
   end;

   if Asis.Compilation_Units.Is_Nil (Library_Unit) then
      --  this may be the case if the file name was krunched.
      --  This is the case for the GNAT RTL components.
      --  In this case we have to iterate through the context
      declare
         C_Units : Asis.Compilation_Unit_List :=
           Asis.Compilation_Units.Compilation_Units (CIAO_Context);
      begin
         --  to be 100% honest, we should go through C_Units list and
         --  to compare the result of Asis.Compilation_Units.Text_Name
         --  applied to a unit with File_Name. But here we use the
         --  fact that in every tree a unit for which the tree is
         --  created is always processed first when ASIS opens a
         --  Context, and in gnatstub we have C1 context. So
         --  the needed unit is the second in the list (just after Standard)

         --  In CIAO we have C1 context only if Reuse_Tree.
         --  XXX for the other cases we should implement the
         --     behaviour described above.
         if Reuse_Tree and then C_Units'Length > 1 then
            Library_Unit := C_Units (2);
         end if;

      end;
   end if;

   IDL_Tree := Translate (Library_Unit);
   --  Translate service specification to IDL syntax tree.

   IDL.Generate (IDL_Tree, IDL_File);
   --  Produce IDL_Source_File.

   Proxy_Generator.Generate (IDL_Tree);
   --  Generate proxy packages.

   Clean;

exception

   when Ex : Asis.Exceptions.Asis_Inappropriate_Context
          |  Asis.Exceptions.Asis_Inappropriate_Container
          |  Asis.Exceptions.Asis_Inappropriate_Compilation_Unit
          |  Asis.Exceptions.Asis_Inappropriate_Element
          |  Asis.Exceptions.Asis_Inappropriate_Line
          |  Asis.Exceptions.Asis_Inappropriate_Line_Number
          |  Asis.Exceptions.Asis_Failed
     =>
      Set_Output (Standard_Error);
      New_Line;


      Put ("Unexpected bug in ");
      Put_CIAO_Version;
      New_Line;
      Put (To_Wide_String (Exception_Name (Ex)));
      Put_Line (" raised");
      Put ("CIAO: ASIS Diagnosis is " &  Asis.Implementation.Diagnosis);
      New_Line;
      Put ("CIAO: Status Value   is ");
      Put_Line (Asis.Errors.Error_Kinds'Wide_Image
                (Asis.Implementation.Status));
      New_Line;
      Put_Line ("Please report to ciao-report@adabroker.eu.org.");

      --  Exit cleanly.
      Set_Output (Standard_Output);
      Set_Exit_Status (Failure);
      Clean;

   when CIAO.Translator.Translation_Error =>
      Clean;

   when Ex : others =>
      Set_Output (Standard_Error);
      New_Line;
      Put ("Unexpected exception in ");
      Put_CIAO_Version;
      New_Line;
      Put (To_Wide_String (Exception_Name (Ex)));
      Put (" was raised: ");

      if Exception_Message (Ex)'Length = 0 then
         Put_Line ("(no exception message)");
      else
         Put_Line (To_Wide_String (Exception_Message (Ex)));
      end if;

      Put_Line ("Please report to ciao-report@adabroker.eu.org");

      --  Exit cleanly.
      Set_Output (Standard_Output);
      Set_Exit_Status (Failure);
      Clean;
end CIAO.Driver;
