------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ L I S T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Table;

with GPR2.ALI.Definition;
with GPR2.ALI.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Tree;
with GPR2.Project.View;

with XE;
with XE_Front;    use XE_Front;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Units;    use XE_Units;
with XE_Utils;    use XE_Utils;

package body XE_List is

   Monolithic_Src_File : File_Descriptor;

   -----------------------
   -- Source File Stack --
   -----------------------

   package Sources is new GNAT.Table
     (Table_Component_Type => With_Record,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100);

   ------------
   -- Parser --
   ------------

   package Parser is

      subtype Token_Type is Natural range 0 .. 27;

      T_No_ALI         : constant Token_Type := 00;
      T_ALI            : constant Token_Type := 01;
      T_Unit           : constant Token_Type := 02;
      T_With           : constant Token_Type := 03;
      T_Source         : constant Token_Type := 04;
      T_Afile          : constant Token_Type := 05;
      T_Ofile          : constant Token_Type := 06;
      T_Sfile          : constant Token_Type := 07;
      T_Name           : constant Token_Type := 08;
      T_Main           : constant Token_Type := 09;
      T_Kind           : constant Token_Type := 10;
      T_Flags          : constant Token_Type := 11;
      T_Preelaborated  : constant Token_Type := 12;
      T_Pure           : constant Token_Type := 13;
      T_Has_RACW       : constant Token_Type := 14;
      T_Remote_Types   : constant Token_Type := 15;
      T_Shared_Passive : constant Token_Type := 16;
      T_RCI            : constant Token_Type := 17;
      T_Predefined     : constant Token_Type := 18;
      T_Internal       : constant Token_Type := 19;
      T_Is_Generic     : constant Token_Type := 20;
      T_Procedure      : constant Token_Type := 21;
      T_Function       : constant Token_Type := 22;
      T_Package        : constant Token_Type := 23;
      T_Subprogram     : constant Token_Type := 24;
      T_Spec           : constant Token_Type := 25;
      T_Body           : constant Token_Type := 26;

      subtype Valid_Token_Type is Token_Type
        range Token_Type'First .. Token_Type'Last - 1;

      Image : constant array (Valid_Token_Type) of GNAT.OS_Lib.String_Access :=
        (T_No_ALI          => new String'("No_ALI"),
         T_ALI             => new String'("ALI"),
         T_Unit            => new String'("Unit"),
         T_With            => new String'("With"),
         T_Source          => new String'("Source"),
         T_Afile           => new String'("Afile"),
         T_Ofile           => new String'("Ofile"),
         T_Sfile           => new String'("Sfile"),
         T_Name            => new String'("Name"),
         T_Main            => new String'("Main"),
         T_Kind            => new String'("Kind"),
         T_Flags           => new String'("Flags"),
         T_Preelaborated   => new String'("Preelaborated"),
         T_Pure            => new String'("Pure"),
         T_Has_RACW        => new String'("Has_RACW"),
         T_Remote_Types    => new String'("Remote_Types"),
         T_Shared_Passive  => new String'("Shared_Passive"),
         T_RCI             => new String'("RCI"),
         T_Predefined      => new String'("Predefined"),
         T_Internal        => new String'("Internal"),
         T_Is_Generic      => new String'("Is_Generic"),
         T_Procedure       => new String'("procedure"),
         T_Function        => new String'("function"),
         T_Package         => new String'("package"),
         T_Subprogram      => new String'("subprogram"),
         T_Spec            => new String'("spec"),
         T_Body            => new String'("body"));

   end Parser;

   procedure Dump_ALI (My_ALI : ALI_Id);
   --  Dump content of ALI record

   procedure Load_ALI
     (Afile, Sfile : File_Name_Type; View : GPR2.Project.View.Object);
   --  Read from Output all the ALI files available

   --------------
   -- Dump_ALI --
   --------------

   procedure Dump_ALI (My_ALI : ALI_Id) is
      use Parser;

      A : constant ALIs_Record := ALIs.Table (My_ALI);
      U : Unit_Record;
      N : Natural := 0;

      procedure Write_Token (T : Token_Type);

      -----------------
      -- Write_Token --
      -----------------

      procedure Write_Token (T : Token_Type) is
         Img : String renames Image (T).all;

      begin
         case T is
            when T_No_ALI .. T_Flags =>
               for J in 1 .. N loop
                  Write_Str ("   ");
               end loop;

               Write_Str (Img);

               for J in Img'Length .. 12 loop
                  Write_Char (' ');
               end loop;

               Write_Str ("=>");

               if T in T_Source .. T_Name then
                  Write_Char (' ');
               end if;

            when T_Preelaborated .. T_Body =>
               Write_Char (' ');
               Write_Str  (Img);

            when others =>
               Write_Str  (Img);

         end case;
      end Write_Token;

   begin
      Write_Token (T_ALI);
      Write_Str   (" (");
      Write_Int   (Int (My_ALI));
      Write_Str   (")");
      Write_Eol;
      N := N + 1;

      Write_Token (T_Name);
      Write_Name  (A.Uname);
      Write_Str   (" (");
      Write_Int   (Get_Name_Table_Info (A.Uname));
      Write_Str   (")");
      Write_Eol;

      if Present (A.Afile) then
         Write_Token (T_Afile);
         Write_Name  (A.Afile);
         Write_Str   (" (");
         Write_Int   (Get_Name_Table_Info (A.Afile));
         Write_Str   (")");
         Write_Eol;
      end if;

      if Present (A.Ofile) then
         Write_Token (T_Ofile);
         Write_Name  (A.Ofile);
         Write_Eol;
      end if;

      if Present (A.Sfile) then
         Write_Token (T_Sfile);
         Write_Name  (A.Sfile);
         Write_Str   (" (");
         Write_Int   (Get_Name_Table_Info (A.Sfile));
         Write_Str   (")");
         Write_Eol;
      end if;

      for J in A.First_Unit .. A.Last_Unit loop
         U := Units.Table (J);
         Write_Token (T_Unit);
         Write_Str   (" (");
         Write_Int   (Int (J));
         Write_Str   (")");
         Write_Eol;

         N := N + 1;
         Write_Token (T_Name);
         Write_Name  (U.Uname);
         Write_Str   (" (");
         Write_Int   (Get_Name_Table_Info (U.Uname));
         Write_Str   (")");
         Write_Eol;

         if Present (U.Sfile) then
            Write_Token (T_Sfile);
            Write_Name  (U.Sfile);
            Write_Str   (" (");
            Write_Int   (Get_Name_Table_Info (U.Sfile));
            Write_Str   (")");
            Write_Eol;
         end if;

         Write_Token (T_Flags);
         if U.Has_RACW then
            Write_Token (T_Has_RACW);
         end if;
         if U.Remote_Types then
            Write_Token (T_Remote_Types);
         end if;
         if U.Shared_Passive then
            Write_Token (T_Shared_Passive);
         end if;
         if U.RCI then
            Write_Token (T_RCI);
         end if;
         if U.Preelaborated then
            Write_Token (T_Preelaborated);
         end if;
         if U.Pure then
            Write_Token (T_Pure);
         end if;
         if U.Predefined then
            Write_Token (T_Predefined);
         end if;
         if U.Internal then
            Write_Token (T_Internal);
         end if;
         if U.Is_Generic then
            Write_Token (T_Is_Generic);
         end if;
         Write_Eol;

         for K in U.First_With .. U.Last_With loop
            Write_Token (T_With);
            Write_Eol;
            N := N + 1;

            if Present (Withs.Table (K).Afile) then
               Write_Token (T_Afile);
               Write_Name  (Withs.Table (K).Afile);
               Write_Str   (" (");
               Write_Int   (Get_Name_Table_Info (Withs.Table (K).Afile));
               Write_Str   (")");
               Write_Eol;
            end if;

            if Present (Withs.Table (K).Sfile) then
               Write_Token (T_Sfile);
               Write_Name  (Withs.Table (K).Sfile);
               Write_Str   (" (");
               Write_Int   (Get_Name_Table_Info (Withs.Table (K).Sfile));
               Write_Str   (")");
               Write_Eol;
            end if;
            N := N - 1;
         end loop;
         N := N - 1;
      end loop;

      for J in A.First_Sdep .. A.Last_Sdep loop
         Write_Token (T_Source);
         Write_Name  (Sdep.Table (J).Sfile);
         Write_Eol;
      end loop;
      Write_Eol;
   end Dump_ALI;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Create main body for monolithic application as a temporary file

      Register_Temp_File (Monolithic_Src_File, Monolithic_Src_Name);
      Set_Output (Monolithic_Src_File);
      Write_Warnings_Pragmas;

      --  Record the associated object and ALI files as temporary files to
      --  be cleaned up eventually.

      Register_Temp_File (Monolithic_ALI_Name);
      Register_Temp_File (Monolithic_Obj_Name);
   end Initialize;

   ---------------
   -- Load_ALIs --
   ---------------

   procedure Load_ALI
     (Afile, Sfile : File_Name_Type; View : GPR2.Project.View.Object)
   is
      use GPR2;

      Src_NR : constant Path_Name.Object :=
                 Path_Name.Create_File
                   (Name_Type (Get_Name_String (Sfile)),
                    Path_Name.No_Resolution);
      Src_PR   : Path_Name.Object;
      Afacts   : Project.Source.Artifact.Object :=
                   View.Source (Src_NR).Artifacts;
      ALI_File : Path_Name.Object;
      ALI_Def  : ALI.Definition.Object;

      My_ALI  : ALI_Id;
      My_Unit : Unit_Id;
      My_With : With_Id;
      My_Sdep : Sdep_Id;

      Unit_Name_Id : Name_Id := XE_Types.No_Name;

      procedure Find_Existing_ALI;

      -----------------------
      -- Find_Existing_ALI --
      -----------------------

      procedure Find_Existing_ALI is
      begin
         for L in GPR2.Project.Source.Artifact.Dependency_Location loop
            if Afacts.Has_Dependency (1, L)
              and then Afacts.Dependency (1, L).Exists
            then
               ALI_File := Afacts.Dependency (1, L);
               Src_PR   := Afacts.Source.Source.Path_Name;
               exit;
            end if;
         end loop;
      end Find_Existing_ALI;

   --  Start of processing for Load_ALIs

   begin
      Find_Existing_ALI;

      if not ALI_File.Is_Defined then
         for V of View.Tree.all loop
            declare
               Source : constant Project.Source.Object := V.Source (Src_NR);
            begin
               if Source.Is_Defined then
                  Afacts := Source.Artifacts;
                  Find_Existing_ALI;
                  exit when ALI_File.Is_Defined;
               end if;
            end;
         end loop;
      end if;

      ALI_Def := ALI.Definition.Scan_ALI (ALI_File);

      --  Allocate ALI and initialize ALI entry

      ALIs.Increment_Last;
      My_ALI := ALIs.Last;
      Set_ALI_Id (Afile, My_ALI);

      ALIs.Table (My_ALI) := Default_ALI;
      ALIs.Table (My_ALI).Afile := Afile;
      Set_Str_To_Name_Buffer (Src_PR.Value);
      ALIs.Table (My_ALI).Sfile := Name_Find;
      ALIs.Table (My_ALI).Main_Program :=
        (if ALI_Def.Is_Main
         then (case ALI_Def.Main_Kind is
                  when ALI.Definition.Func => Func,
                  when ALI.Definition.Proc => Proc)
         else No_Main);
      Set_Str_To_Name_Buffer (ALI_File.Value);
      ALIs.Table (My_ALI).Ofile := To_Ofile (Name_Find);
      --  ??? Do we need to take object file from project tree,
      --  depend on how did we take ali file.

      for U of ALI_Def.Units loop
         --  Allocate Unit and initialize Unit entry

         Units.Increment_Last;
         My_Unit := Units.Last;

         if ALIs.Table (My_ALI).Last_Unit = No_Unit_Id then
            ALIs.Table (My_ALI).First_Unit := My_Unit;
         end if;
         ALIs.Table (My_ALI).Last_Unit := My_Unit;

         Units.Table (My_Unit)        := Default_Unit;
         Units.Table (My_Unit).My_ALI := My_ALI;

         Set_Str_To_Name_Buffer
           (GPR2.Path_Name.Create_File
              (U.Sfile, Name_Type (Src_PR.Dir_Name)).Value);
         Units.Table (My_Unit).Sfile := Name_Find;
         Set_Unit_Id (Units.Table (My_Unit).Sfile, My_Unit);

         --  Is it a subprogram or a package

         Units.Table (My_Unit).Unit_Kind :=
           (case U.Kind is
               when ALI.Unit.Kind_Subprogram => 's',
               when ALI.Unit.Kind_Package    => 'p');

         --  Prepare to set unit name info

         Set_Str_To_Name_Buffer (String (U.Uname));
         Unit_Name_Id := Name_Find;
         Add_Char_To_Name_Buffer ('%');

         --  Is it a spec or a body

         case U.Utype is
            when ALI.Unit.Is_Spec =>
               Add_Char_To_Name_Buffer ('s');
               Units.Table (My_Unit).Utype := Is_Spec;
            when ALI.Unit.Is_Spec_Only =>
               Add_Char_To_Name_Buffer ('s');
               Units.Table (My_Unit).Utype := Is_Spec_Only;
            when ALI.Unit.Is_Body =>
               Add_Char_To_Name_Buffer ('b');
               Units.Table (My_Unit).Utype := Is_Body;
            when ALI.Unit.Is_Body_Only =>
               Add_Char_To_Name_Buffer ('b');
               Units.Table (My_Unit).Utype := Is_Body_Only;
         end case;

         --  Set unit name info

         Units.Table (My_Unit).Uname := Name_Find;
         Set_Unit_Id (Units.Table (My_Unit).Uname, My_Unit);

         for W of U.Withs loop
            Withs.Increment_Last;
            My_With := Withs.Last;
            Withs.Table (My_With) := Default_With;

            if Units.Table (My_Unit).Last_With = No_With_Id then
               Units.Table (My_Unit).First_With := My_With;
            end if;
            Units.Table (My_Unit).Last_With := My_With;

            declare
               AF  : constant String := String (W.Afile);
               SF  : constant Optional_Name_Type := W.Sfile;
            begin
               if AF /= "" then
                  Set_Str_To_Name_Buffer (AF);
                  Withs.Table (My_With).Afile := Name_Find;
               end if;

               if SF /= "" then
                  Set_Str_To_Name_Buffer (String (SF));
                  Withs.Table (My_With).Sfile := Name_Find;
               end if;
            end;

            Set_Str_To_Name_Buffer (String (W.Uname));
            case W.Ukind is
               when GPR2.S_Spec     => Add_Str_To_Name_Buffer ("%s");
               when GPR2.S_Body     => Add_Str_To_Name_Buffer ("%b");
               when GPR2.S_Separate => null;
            end case;
            Withs.Table (My_With).Uname := Name_Find;
         end loop;

         declare
            use GPR2.ALI.Unit;
            Flags : constant Flag_Array := U.Flags;
         begin
            Units.Table (My_Unit).Has_RACW       := Flags (Has_RACW);
            Units.Table (My_Unit).Remote_Types   := Flags (Remote_Types);
            Units.Table (My_Unit).Shared_Passive := Flags (Shared_Passive);
            Units.Table (My_Unit).RCI            := Flags (RCI);
            Units.Table (My_Unit).Predefined     := Flags (Predefined);
            Units.Table (My_Unit).Is_Generic     := Flags (Is_Generic);
            Units.Table (My_Unit).Preelaborated  := Flags (Preelab);
            Units.Table (My_Unit).Internal       := False;
            --  Internal flag is not assigned in gpr1.
         end;
      end loop;

      ALIs.Table (My_ALI).Uname := Unit_Name_Id; -- Units.Table (My_Unit).Uname
      Set_ALI_Id (ALIs.Table (My_ALI).Uname, My_ALI);

      for SD of ALI_Def.Sdeps loop
         Sdep.Increment_Last;
         My_Sdep := Sdep.Last;
         Set_Str_To_Name_Buffer (String (SD.Sfile));
         Sdep.Table (My_Sdep).Sfile := Name_Find;

         if ALIs.Table (My_ALI).Last_Sdep = No_Sdep_Id then
            ALIs.Table (My_ALI).First_Sdep := My_Sdep;
         end if;
         ALIs.Table (My_ALI).Last_Sdep := My_Sdep;

         Get_Name_String (Strip_Directory (Sdep.Table (My_Sdep).Sfile));

         if Name_Len > 8 and then Name_Buffer (1 .. 8) = "s-taskin" then
            ALIs.Table (My_ALI).Tasking := XE.User_Tasking;
         end if;
      end loop;
   end Load_ALI;

   -------------------------------
   -- Load_All_Registered_Units --
   -------------------------------

   procedure Load_All_Registered_Units is
      use GPR2;

      Comp_Flags, List_Flags, Make_Flags : Argument_Vec;

      Tree       : Project.Tree.Object;
      View       : Project.View.Object;
      Sfile      : File_Name_Type;
      Afile      : File_Name_Type;
      ALI        : ALI_Id;
      Partition  : Partition_Id;

   begin
      Push (Comp_Flags, Semantic_Only_Flag);
      Push (Comp_Flags, Project_File_Flag);
      Push (Comp_Flags, Project_File_Name);

      Push (List_Flags, GLADE_List_Flag);
      Push (List_Flags, Project_File_Flag);
      Push (List_Flags, Project_File_Name);

      Push (Make_Flags, Compile_Only_Flag);
      Push (Make_Flags, Project_File_Flag);
      Push (Make_Flags, Project_File_Name);

      --  Finish up main library procedure with a dummy body

      Write_Str  ("procedure ");
      Write_Name (Monolithic_App_Unit_Name);
      Write_Line (" is");
      Write_Line ("begin");
      Write_Line ("   null;");
      Write_Str  ("end ");
      Write_Name (Monolithic_App_Unit_Name);
      Write_Line (";");
      Close (Monolithic_Src_File);
      Set_Standard_Output;

      --  Build the monolithic application with a fake main subprogram
      --  Monolithic_App. Note that we must pass the bare file name (without
      --  directory information) to gnat make, Monolithic_Src_Base_Name,
      --  not Monolithic_Src_Name.

      Build
        (Monolithic_Src_Base_Name, Make_Flags, not Keep_Going,
         Display_Compilation_Progress);

      Project.Tree.Load_Autoconf
        (Self             => Tree,
         Context          => Context.Empty,
         Filename         => Path_Name.Create_File
                               (Name_Type (To_String (Project_File_Name))),
         Absent_Dir_Error => True);
      Tree.Update_Sources;

      --  Load the info from its ALI file

      Load_ALI (Monolithic_ALI_Name, Monolithic_Src_Name, Tree.Root_Project);
      ALI := Get_ALI_Id (Monolithic_ALI_Name);

      --  Do not delete the source file for the fake main subprogram,
      --  it is needed by List later on.

      Remove_Temp_File (Part_Main_ALI_Name);
      Remove_Temp_File (Part_Main_Obj_Name);

      --  The compilation of monolithic_app.adb failed. There is no way to
      --  rescue this situation.

      if ALI = No_ALI_Id then
         raise Compilation_Error;
      end if;

      --  Load in the sources stack all the withed units or in other
      --  words the configured units.

      for J in
        ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit
      loop
         for K in
           Units.Table (J).First_With .. Units.Table (J).Last_With
         loop
            Sfile := Withs.Table (K).Sfile;

            if Present (Sfile) then
               Set_Name_Table_Byte (Sfile, 1);
               Sources.Append (Withs.Table (K));
            end if;
         end loop;
      end loop;

      while Sources.First <= Sources.Last loop
         declare
            Last  : Natural := Sources.Last + 1 - Sources.First;
            W_Arr : array (1 .. Last) of With_Record;

         begin
            --  Load in Args the sources whose corresponding ALI file is not
            --  yet available.

            Last := 0;
            for J in Sources.First .. Sources.Last loop
               --  We never tried to download this ALI file. Its info
               --  is not a valid ALI id (not even No_ALI_Id).

               if Get_Name_Table_Info (Sources.Table (J).Afile) = 0 then
                  Last := Last + 1;
                  W_Arr (Last) := Sources.Table (J);
               end if;
            end loop;
            Sources.Init;

            for J in 1 .. Last loop
               declare
                  Unit : constant String := Get_Name_String (W_Arr (J).Uname);
                  Last : constant Natural :=
                           (if Unit'Length > 2
                              and then Unit (Unit'Last - 1) = '%'
                            then Unit'Last - 2
                            else Unit'Last);
               begin
                  View := Tree.Get_View (Name_Type (Unit (1 .. Last)));

                  if not View.Is_Defined then
                     raise Constraint_Error with
                       "Can't find project for unit " & Unit (1 .. Last);
                  end if;
               end;

               Sfile := W_Arr (J).Sfile;
               Afile := W_Arr (J).Afile;
               Load_ALI (Afile, Sfile, View);
               ALI   := Get_ALI_Id (Afile);

               --  The ALI file does not exist. It may come from a missing
               --  body file although the spec file is available (the main
               --  subprogram is compiled with the -k (keep going) flag).
               --  Therefore compile the spec file with the -gnatc
               --  (semantics only) flag in order to obtain an ALI file
               --  anyway. Then check, this operation was successul, i.e.
               --  that the unit is an RCI. The missing body file is not an
               --  issue as long as the unit is not assigned to a partition
               --  to build.

               if ALI = No_ALI_Id then
                  if Debug_Mode then
                     Write_Str ("No ALI yet for ");
                     Write_Name (Sfile);
                  end if;

                  Compile (Sfile, Comp_Flags, Fatal => False);
                  Load_ALI (Afile, Sfile, View);

                  --  If the ALI file is still missing, then we have a real
                  --  problem.

                  ALI := Get_ALI_Id (Afile);
                  if ALI = No_ALI_Id then
                     Get_Name_String (Sfile);
                     raise Fatal_Error with "failed to load ALI for "
                       & Name_Buffer (1 .. Name_Len);
                  end if;

                  --  Check that the unit was really assigned to a partition
                  --  we are not going to build.

                  Partition := Get_Partition_Id (ALIs.Table (ALI).Uname);
                  if not Units.Table (ALIs.Table (ALI).Last_Unit).RCI
                    or else Partition = No_Partition_Id
                    or else Partitions.Table (Partition).To_Build
                  then
                     Get_Name_String (ALIs.Table (ALI).Uname);
                     raise Fatal_Error with "invalid partition for "
                       & Name_Buffer (1 .. Name_Len);
                  end if;
               end if;

               if Debug_Mode then
                  Dump_ALI (ALI);
               end if;

               --  Check that the withed units are present.

               for J in
                 ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit
               loop
                  for K in
                    Units.Table (J).First_With .. Units.Table (J).Last_With
                  loop
                     Sfile := Withs.Table (K).Sfile;

                     --  We can ignore the sources that have already
                     --  been loaded and the predefined ones (they are
                     --  not defined as configured units at this stage
                     --  and they cannot be categorized).

                     if Present (Sfile)
                       and then not Is_Predefined_File (Sfile)
                       and then Get_Name_Table_Byte (Sfile) = 0
                     then
                        Set_Name_Table_Byte (Sfile, 1);
                        Sources.Append (Withs.Table (K));
                     end if;
                  end loop;
               end loop;
            end loop;
         end;
      end loop;
   end Load_All_Registered_Units;

   ---------------------------
   -- Register_Unit_To_Load --
   ---------------------------

   procedure Register_Unit_To_Load (Uname : Unit_Name_Type) is
   begin
      Write_Str  ("with ");
      Write_Name (Uname);
      Write_Line (";");
   end Register_Unit_To_Load;

end XE_List;
