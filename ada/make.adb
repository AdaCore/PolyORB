------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M A K E                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;      use GNAT.OS_Lib;

with ALI;              use ALI;
with ALI.Util;         use ALI.Util;
with Csets;
with Debug;
with Fname;            use Fname;
with Fname.SF;         use Fname.SF;
with Fname.UF;         use Fname.UF;
with Gnatvsn;          use Gnatvsn;
with Hostparm;         use Hostparm;
with Makeusg;
with Namet;            use Namet;
with Opt;              use Opt;
with Osint;            use Osint;
with Gnatvsn;
with Output;           use Output;
with Prj;              use Prj;
with Prj.Env;
with Prj.Ext;
with Prj.Pars;
with Prj.Util;
with SFN_Scan;
with Snames;           use Snames;
with Stringt;          use Stringt;
with Table;
with Types;            use Types;
with Switch;           use Switch;

with System.WCh_Con;   use System.WCh_Con;

package body Make is

   use ASCII;
   --  Make control characters visible

   -------------------------------------
   -- Queue (Q) Manipulation Routines --
   -------------------------------------

   --  The Q is used in Compile_Sources below. Its implementation uses the
   --  GNAT generic package Table (basically an extensible array). Q_Front
   --  points to the first valid element in the Q, whereas Q.First is the first
   --  element ever enqueued, while Q.Last - 1 is the last element in the Q.
   --
   --        +---+--------------+---+---+---+-----------+---+--------
   --    Q   |   |  ........    |   |   |   | .......   |   |
   --        +---+--------------+---+---+---+-----------+---+--------
   --          ^                  ^                       ^
   --       Q.First             Q_Front               Q.Last - 1
   --
   --  The elements comprised between Q.First and Q_Front - 1 are the
   --  elements that have been enqueued and then dequeued, while the
   --  elements between Q_Front and Q.Last - 1 are the elements currently
   --  in the Q. When the Q is intialized Q_Front = Q.First = Q.Last.
   --  After Compile_Sources has terminated its execution, Q_Front = Q.Last
   --  and the elements contained between Q.Front and Q.Last-1 are those that
   --  were explored and thus marked by Compile_Sources. Whenever the Q is
   --  reinitialized, the elements between Q.First and Q.Last - 1 are unmarked.

   procedure Init_Q;
   --  Must be called to (re)initialize the Q.

   procedure Insert_Q
     (Source_File : File_Name_Type;
      Source_Unit : Unit_Name_Type := No_Name);
   --  Inserts Source_File at the end of Q. Provide Source_Unit when
   --  possible for external use (gnatdist).

   function Empty_Q return Boolean;
   --  Returns True if Q is empty.

   procedure Extract_From_Q
     (Source_File : out File_Name_Type;
      Source_Unit : out Unit_Name_Type);
   --  Extracts the first element from the Q.

   First_Q_Initialization : Boolean := True;
   --  Will be set to false after Init_Q has been called once.

   Q_Front : Natural;
   --  Points to the first valid element in the Q.

   Unique_Compile : Boolean := False;

   type Q_Record is record
      File : File_Name_Type;
      Unit : Unit_Name_Type;
   end record;
   --  File is the name of the file to compile. Unit is for gnatdist
   --  use in order to easily get the unit name of a file to compile
   --  when its name is krunched or declared in gnat.adc.

   package Q is new Table.Table (
     Table_Component_Type => Q_Record,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => 4000,
     Table_Increment      => 100,
     Table_Name           => "Make.Q");
   --  This is the actual Q.

   --  The following instantiations and variables are necessary to save what
   --  is found on the command line, in case there is a project file specified.

   package Saved_Gcc_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Saved_Gcc_Switches");

   package Saved_Binder_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Saved_Binder_Switches");

   package Saved_Linker_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Make.Saved_Linker_Switches");

   package Saved_Make_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Make.Saved_Make_Switches");

   Saved_Maximum_Processes : Natural := 0;
   Saved_WC_Encoding_Method : WC_Encoding_Method := WC_Encoding_Method'First;
   Saved_WC_Encoding_Method_Set : Boolean := False;

   type Arg_List_Ref is access Argument_List;
   The_Saved_Gcc_Switches : Arg_List_Ref;

   Project_File_Name : String_Access := null;
   Current_Verbosity : Prj.Verbosity := Prj.Default;
   Main_Project      : Prj.Project_Id;

   procedure Add_Source_Dir (N : String);
   --  Call Add_Src_Search_Dir.
   --  Output one line when in verbose mode.

   procedure Add_Source_Directories is
     new Prj.Env.For_All_Source_Dirs (Action => Add_Source_Dir);

   procedure Add_Object_Dir (N : String);
   --  Call Add_Lib_Search_Dir.
   --  Output one line when in verbose mode.

   procedure Add_Object_Directories is
     new Prj.Env.For_All_Object_Dirs (Action => Add_Object_Dir);

   type Bad_Compilation_Info is record
      File  : File_Name_Type;
      Unit  : Unit_Name_Type;
      Found : Boolean;
   end record;
   --  File is the name of the file for which a compilation failed.
   --  Unit is for gnatdist use in order to easily get the unit name
   --  of a file when its name is krunched or declared in gnat.adc.
   --  Found is False if the compilation failed because the file could
   --  not be found.

   package Bad_Compilation is new Table.Table (
     Table_Component_Type => Bad_Compilation_Info,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Bad_Compilation");
   --  Full name of all the source files for which compilation fails.

   type Special_Argument is record
      File : String_Access;
      Args : Argument_List_Access;
   end record;
   --  File is the name of the file for which a special set of compilation
   --  arguments (Args) is required.

   package Special_Args is new Table.Table (
     Table_Component_Type => Special_Argument,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Special_Args");
   --  Compilation arguments of all the source files for which an entry has
   --  been found in the project file.

   Original_Ada_Include_Path : constant String_Access :=
                                 Getenv ("ADA_INCLUDE_PATH");
   Original_Ada_Objects_Path : constant String_Access :=
                                 Getenv ("ADA_OBJECTS_PATH");
   Current_Ada_Include_Path  : String_Access := null;
   Current_Ada_Objects_Path  : String_Access := null;

   ----------------------
   -- Marking Routines --
   ----------------------

   procedure Mark (Source_File : File_Name_Type);
   --  Mark Source_File. Marking is used to signal that Source_File has
   --  already been inserted in the Q.

   function Is_Marked (Source_File : File_Name_Type) return Boolean;
   --  Returns True if Source_File was previously marked.

   procedure Unmark (Source_File : File_Name_Type);
   --  Unmarks Source_File.

   -------------------
   -- Misc Routines --
   -------------------

   procedure List_Depend;
   --  Prints to standard output the list of object dependencies. This list
   --  can be used directly in a Makefile. A call to Compile_Sources must
   --  precede the call to List_Depend. Also because this routine uses the
   --  ALI files that were originally loaded and scanned by Compile_Sources,
   --  no additional ALI files should be scanned between the two calls (i.e.
   --  between the call to Compile_Sources and List_Depend.)

   procedure Inform (N : Name_Id := No_Name; Msg : String);
   --  Prints out the program name followed by a colon, N and S.

   procedure List_Bad_Compilations;
   --  Prints out the list of all files for which the compilation failed.

   procedure Verbose_Msg
     (N1     : Name_Id;
      S1     : String;
      N2     : Name_Id := No_Name;
      S2     : String  := "";
      Prefix : String  := "  -> ");
   --  If the verbose flag (Verbose_Mode) is set then print Prefix to standard
   --  output followed by N1 and S1. If N2 /= No_Name then N2 is then printed
   --  after S1. S2 is printed last. Both N1 and N2 are printed in quotation
   --  marks.

   -----------------------
   -- Gnatmake Routines --
   -----------------------

   subtype Lib_Mark_Type is Byte;

   Ada_Lib_Dir  : constant Lib_Mark_Type := 1;
   GNAT_Lib_Dir : constant Lib_Mark_Type := 2;

   --  Note that the notion of GNAT lib dir is no longer used. The code
   --  related to it has not been removed to give an idea on how to use
   --  the directory prefix marking mechanism.

   --  An Ada library directory is a directory containing ali and object
   --  files but no source files for the bodies (the specs can be in the
   --  same or some other directory). These directories are specified
   --  in the Gnatmake command line with the switch "-Adir" (to specify the
   --  spec location -Idir cab be used).  Gnatmake skips the missing sources
   --  whose ali are in Ada library directories. For an explanation of why
   --  Gnatmake behaves that way, see the spec of Make.Compile_Sources.
   --  The directory lookup penalty is incurred every single time this
   --  routine is called.

   function Is_External_Assignment (Argv : String) return Boolean;
   --  Verify that an external assignment switch is syntactically correct.
   --  Correct forms are
   --      -Xname=value
   --      -X"name=other value"
   --  Assumptions: 'First = 1, Argv (1 .. 2) = "-X"
   --  When this function returns True, the external assignment has
   --  been entered by a call to Prj.Ext.Add, so that in a project
   --  file, External ("name") will return "value".

   function In_Ada_Lib_Dir  (File : File_Name_Type) return Boolean;
   --  Get directory prefix of this file and get lib mark stored in name
   --  table for this directory. Then check if an Ada lib mark has been set.

   procedure Mark_Dir_Path
     (Path : in String_Access;
      Mark : in Lib_Mark_Type);
   --  Invoke Mark_Directory on each directory of the path.

   procedure Mark_Directory
     (Dir  : in String;
      Mark : in Lib_Mark_Type);
   --  Store Dir in name table and set lib mark as name info to identify
   --  Ada libraries.

   function Object_File_Name (Source : String) return String;
   --  Returns the object file name suitable for switch -o.

   procedure Set_Ada_Paths (For_Project : Prj.Project_Id);
   --  Set, if necessary, env. variables ADA_INCLUDE_PATH and
   --  ADA_OBJECTS_PATH.
   --  Note: this will modify these environment variables only
   --  for the current gnatmake process and all of its children
   --  (invocations of the compiler, the binder and the linker).
   --  The caller process ADA_INCLUDE_PATH and ADA_OBJECTS_PATH are
   --  not affected.

   ----------------------------------------------------
   -- Compiler, Binder & Linker Data and Subprograms --
   ----------------------------------------------------

   Gcc             : String_Access := Program_Name ("gcc");
   Gnatbind        : String_Access := Program_Name ("gnatbind");
   Gnatlink        : String_Access := Program_Name ("gnatlink");
   --  Default compiler, binder, linker programs

   Saved_Gcc       : String_Access := null;
   Saved_Gnatbind  : String_Access := null;
   Saved_Gnatlink  : String_Access := null;
   --  Given by the command line. Will be used, if non null.

   Gcc_Path        : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
   Gnatbind_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
   Gnatlink_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);
   --  Path for compiler, binder, linker programs, defaulted now for gnatdist.
   --  Changed later if overridden on command line.

   Comp_Flag         : constant String_Access := new String'("-c");
   Output_Flag       : constant String_Access := new String'("-o");
   Ada_Flag_1        : constant String_Access := new String'("-x");
   Ada_Flag_2        : constant String_Access := new String'("ada");
   GNAT_Flag         : constant String_Access := new String'("-gnatpg");
   Do_Not_Check_Flag : constant String_Access := new String'("-x");

   Object_Suffix     : constant String := Get_Object_Suffix.all;
   Executable_Suffix : constant String := Get_Executable_Suffix.all;

   Display_Executed_Programs : Boolean := True;
   --  Set to True if name of commands should be output on stderr.

   Output_Filename_Seen : Boolean := False;
   --  Set to True after having scanned the file_name for
   --  switch "-o file_name"

   File_Name_Seen : Boolean := False;
   --  Set to true after having seen at least one file name.
   --  Used in Scan_Make_Arg only, but must be a global variable.

   type Make_Program_Type is (None, Compiler, Binder, Linker);

   Program_Args : Make_Program_Type := None;
   --  Used to indicate if we are scanning gcc, gnatbind, or gnatbl
   --  options within the gnatmake command line.
   --  Used in Scan_Make_Arg only, but must be a global variable.

   procedure Add_Switches
     (The_Package : Package_Id;
      File_Name   : String;
      Program     : Make_Program_Type);
   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True);
   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True);
   --  Make invokes one of three programs (the compiler, the binder or the
   --  linker). For the sake of convenience, some program specific switches
   --  can be passed directly on the gnatmake commande line. This procedure
   --  records these switches so that gnamake can pass them to the right
   --  program.  S is the switch to be added at the end of the command line
   --  for Program if Append_Switch is True. If Append_Switch is False S is
   --  added at the beginning of the command line.

   procedure Check
     (Lib_File  : File_Name_Type;
      ALI       : out ALI_Id;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type);
   --  Determines whether the library file Lib_File is up-to-date or not. The
   --  full name (with path information) of the object file corresponding to
   --  Lib_File is returned in O_File. Its time stamp is saved in O_Stamp.
   --  ALI is the ALI_Id corresponding to Lib_File. If Lib_File in not
   --  up-to-date, then the corresponding source file needs to be recompiled.
   --  In this case ALI = No_ALI_Id.

   procedure Check_Linker_Options
     (E_Stamp : Time_Stamp_Type;
      O_File  : out File_Name_Type;
      O_Stamp : out Time_Stamp_Type);
   --  Checks all linker options for linker files that are newer
   --  than E_Stamp. If such objects are found, the youngest object
   --  is returned in O_File and its stamp in O_Stamp.
   --
   --  If no obsolete linker files were found, the first missing
   --  linker file is returned in O_File and O_Stamp is empty.
   --  Otherwise O_File is No_File.

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args if variable
   --  Display_Executed_Programs is set. The lower bound of Args must be 1.

   --------------------
   -- Add_Object_Dir --
   --------------------

   procedure Add_Object_Dir (N : String) is
   begin
      Add_Lib_Search_Dir (N);

      if Opt.Verbose_Mode then
         Write_Str ("Adding object directory """);
         Write_Str (N);
         Write_Str (""".");
         Write_Eol;
      end if;
   end Add_Object_Dir;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (N : String) is
   begin
      Add_Src_Search_Dir (N);

      if Opt.Verbose_Mode then
         Write_Str ("Adding source directory """);
         Write_Str (N);
         Write_Str (""".");
         Write_Eol;
      end if;
   end Add_Source_Dir;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True)
   is
      generic
         with package T is new Table.Table (<>);
      function Generic_Position return Integer;
      --  Generic procedure that adds S at the end or beginning of T depending
      --  of the value of the boolean Append_Switch.

      ----------------------
      -- Generic_Position --
      ----------------------

      function Generic_Position return Integer is
      begin
         T.Increment_Last;

         if Append_Switch then
            return Integer (T.Last);
         else
            for J in reverse T.Table_Index_Type'Succ (T.First) .. T.Last loop
               T.Table (J) := T.Table (T.Table_Index_Type'Pred (J));
            end loop;

            return Integer (T.First);
         end if;
      end Generic_Position;

      function Gcc_Switches_Pos    is new Generic_Position (Gcc_Switches);
      function Binder_Switches_Pos is new Generic_Position (Binder_Switches);
      function Linker_Switches_Pos is new Generic_Position (Linker_Switches);

      function Saved_Gcc_Switches_Pos is new
        Generic_Position (Saved_Gcc_Switches);

      function Saved_Binder_Switches_Pos is new
        Generic_Position (Saved_Binder_Switches);

      function Saved_Linker_Switches_Pos is new
        Generic_Position (Saved_Linker_Switches);

   --  Start of processing for Add_Switch

   begin
      if And_Save then
         case Program is
            when Compiler =>
               Saved_Gcc_Switches.Table (Saved_Gcc_Switches_Pos) := S;

            when Binder   =>
               Saved_Binder_Switches.Table (Saved_Binder_Switches_Pos) := S;

            when Linker   =>
               Saved_Linker_Switches.Table (Saved_Linker_Switches_Pos) := S;

            when None =>
               raise Program_Error;
         end case;

      else
         case Program is
            when Compiler =>
               Gcc_Switches.Table (Gcc_Switches_Pos) := S;

            when Binder   =>
               Binder_Switches.Table (Binder_Switches_Pos) := S;

            when Linker   =>
               Linker_Switches.Table (Linker_Switches_Pos) := S;

            when None =>
               raise Program_Error;
         end case;
      end if;
   end Add_Switch;

   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True)
   is
   begin
      Add_Switch (S             => new String'(S),
                  Program       => Program,
                  Append_Switch => Append_Switch,
                  And_Save      => And_Save);
   end Add_Switch;

   ------------------
   -- Add_Switches --
   ------------------

   procedure Add_Switches
     (The_Package : Package_Id;
      File_Name   : String;
      Program     : Make_Program_Type)
   is
      Switches      : Variable_Value;
      Switch_List   : String_List_Id;
      Element       : String_Element;

   begin
      if File_Name'Length > 0 then
         Name_Len := File_Name'Length;
         Name_Buffer (1 .. Name_Len) := File_Name;
         Switches :=
           Prj.Util.Value_Of
             (Name                   => Name_Find,
              Variable_Or_Array_Name => Name_Switches,
              In_Package             => The_Package);

         case Switches.Kind is
            when Undefined =>
               null;

            when List =>
               Program_Args := Program;

               Switch_List := Switches.Values;

               while Switch_List /= Nil_String loop
                  Element := String_Elements.Table (Switch_List);
                  String_To_Name_Buffer (Element.Value);

                  if Name_Len > 0 then
                     if Opt.Verbose_Mode then
                        Write_Str ("   Adding ");
                        Write_Line (Name_Buffer (1 .. Name_Len));
                     end if;

                     Scan_Make_Arg
                       (Name_Buffer (1 .. Name_Len),
                        And_Save => False);
                  end if;

                  Switch_List := Element.Next;
               end loop;

            when Single =>
               Program_Args := Program;
               String_To_Name_Buffer (Switches.Value);

               if Name_Len > 0 then
                  if Opt.Verbose_Mode then
                     Write_Str ("   Adding ");
                     Write_Line (Name_Buffer (1 .. Name_Len));
                  end if;

                  Scan_Make_Arg
                    (Name_Buffer (1 .. Name_Len), And_Save => False);
               end if;
         end case;
      end if;
   end Add_Switches;

   ----------
   -- Bind --
   ----------

   procedure Bind (ALI_File : File_Name_Type; Args : Argument_List) is
      Bind_Args : Argument_List (1 .. Args'Last + 2);
      Bind_Last : Integer;
      Success   : Boolean;

   begin
      pragma Assert (Args'First = 1);

      --  Optimize the simple case where the gnatbind command line looks like
      --     gnatbind -aO. -I- file.ali   --into->   gnatbind file.adb

      if Args'Length = 2
        and then Args (Args'First).all = "-aO" & Normalized_CWD
        and then Args (Args'Last).all = "-I-"
        and then ALI_File = Strip_Directory (ALI_File)
      then
         Bind_Last := Args'First - 1;

      else
         Bind_Last := Args'Last;
         Bind_Args (Args'Range) := Args;
      end if;

      --  It is completely pointless to re-check source file time stamps.
      --  This has been done already by gnatmake

      Bind_Last := Bind_Last + 1;
      Bind_Args (Bind_Last) := Do_Not_Check_Flag;

      Get_Name_String (ALI_File);

      Bind_Last := Bind_Last + 1;
      Bind_Args (Bind_Last) := new String'(Name_Buffer (1 .. Name_Len));

      Display (Gnatbind.all, Bind_Args (Args'First .. Bind_Last));

      if Gnatbind_Path = null then
         Osint.Fail ("error, unable to locate " & Gnatbind.all);
      end if;

      GNAT.OS_Lib.Spawn
        (Gnatbind_Path.all, Bind_Args (Args'First .. Bind_Last), Success);

      if not Success then
         raise Bind_Failed;
      end if;
   end Bind;

   -----------
   -- Check --
   -----------

   procedure Check
     (Lib_File  : File_Name_Type;
      ALI       : out ALI_Id;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type)
   is
      function First_New_Spec (A : ALI_Id) return File_Name_Type;
      --  Looks in the with table entries of A and returns the spec file name
      --  of the first withed unit (subprogram) for which no spec existed when
      --  A was generated but for which there exists one now, implying that A
      --  is now obsolete. If no such unit is found No_File is returned.
      --  Otherwise the spec file name of the unit is returned.
      --
      --  **WARNING** in the event of Uname format modifications, one *MUST*
      --  make sure this function is also updated.
      --
      --  Note: This function should really be in ali.adb and use Uname
      --  services, but this causes the whole compiler to be dragged along
      --  for gnatbind and gnatmake.

      --------------------
      -- First_New_Spec --
      --------------------

      function First_New_Spec (A : ALI_Id) return File_Name_Type is
         Spec_File_Name : File_Name_Type := No_File;

         function New_Spec (Uname : Unit_Name_Type) return Boolean;
         --  Uname is the name of the spec or body of some ada unit.
         --  This function returns True if the Uname is the name of a body
         --  which has a spec not mentioned inali file A. If True is returned
         --  Spec_File_Name above is set to the name of this spec file.

         --------------
         -- New_Spec --
         --------------

         function New_Spec (Uname : Unit_Name_Type) return Boolean is
            Spec_Name : Unit_Name_Type;
            File_Name : File_Name_Type;

         begin
            --  Test whether Uname is the name of a body unit (ie ends with %b)

            Get_Name_String (Uname);
            pragma
              Assert (Name_Len > 2 and then Name_Buffer (Name_Len - 1) = '%');

            if Name_Buffer (Name_Len) /= 'b' then
               return False;
            end if;

            --  Convert unit name into spec name

            --  ??? this code seems dubious in presence of pragma
            --  Source_File_Name since there is no more direct relationship
            --  between unit name and file name.

            --  ??? Further, what about alternative subunit naming

            Name_Buffer (Name_Len) := 's';
            Spec_Name := Name_Find;
            File_Name := Get_File_Name (Spec_Name, Subunit => False);

            --  Look if File_Name is mentioned in A's sdep list.
            --  If not look if the file exists. If it does return True.

            for D in
              ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
            loop
               if Sdep.Table (D).Sfile = File_Name then
                  return False;
               end if;
            end loop;

            if Full_Source_Name (File_Name) /= No_File then
               Spec_File_Name := File_Name;
               return True;
            end if;

            return False;
         end New_Spec;

      --  Start of processing for First_New_Spec

      begin
         U_Chk : for U in
           ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit
         loop
            exit U_Chk when Units.Table (U).Utype = Is_Body_Only
               and then New_Spec (Units.Table (U).Uname);

            for W in Units.Table (U).First_With
                       ..
                     Units.Table (U).Last_With
            loop
               exit U_Chk when
                 Withs.Table (W).Afile /= No_File
                 and then New_Spec (Withs.Table (W).Uname);
            end loop;
         end loop U_Chk;

         return Spec_File_Name;
      end First_New_Spec;

      ---------------------------------
      -- Data declarations for Check --
      ---------------------------------

      Full_Lib_File    : File_Name_Type;
      --  Full name of current library file

      Full_Obj_File    : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File.

      Lib_Stamp        : Time_Stamp_Type;
      --  Time stamp of the current ada library file.

      Obj_Stamp        : Time_Stamp_Type;
      --  Time stamp of the current object file.

      Modified_Source  : File_Name_Type;
      --  The first source in Lib_File whose current time stamp differs
      --  from that stored in Lib_File.

      New_Spec         : File_Name_Type;
      --  If Lib_File contains in its W (with) section a body (for a
      --  subprogram) for which there exists a spec and the spec did not
      --  appear in the Sdep section of Lib_File, New_Spec contains the file
      --  name of this new spec.

      Source_Name : Name_Id;
      Text : Text_Buffer_Ptr;

      Prev_Switch : Character;
      --  First character of previous switch processed

      Arg : Arg_Id;
      --  Current index in Args.Table for a given unit

      Switch_Found : Boolean;
      --  True if a given switch has been found

      Num_Args : Integer;
      --  Number of compiler arguments processed

      Special_Arg : Argument_List_Access;
      --  Special arguments if any of a given compilation file

   --  Start of processing for Check

   begin
      pragma Assert (Lib_File /= No_File);

      Text := Read_Library_Info (Lib_File);
      Full_Lib_File := Full_Library_Info_Name;
      Full_Obj_File := Full_Object_File_Name;
      Lib_Stamp     := Current_Library_File_Stamp;
      Obj_Stamp     := Current_Object_File_Stamp;

      if Full_Lib_File = No_File then
         Verbose_Msg (Lib_File, "being checked ...", Prefix => "  ");
      else
         Verbose_Msg (Full_Lib_File, "being checked ...", Prefix => "  ");
      end if;

      ALI     := No_ALI_Id;
      O_File  := Full_Obj_File;
      O_Stamp := Obj_Stamp;

      if Text = null then
         if Full_Lib_File = No_File then
            Verbose_Msg (Lib_File, "missing.");

         elsif Obj_Stamp (Obj_Stamp'First) = ' ' then
            Verbose_Msg (Full_Obj_File, "missing.");

         else
            Verbose_Msg
              (Full_Lib_File, "(" & String (Lib_Stamp) & ") newer than",
               Full_Obj_File, "(" & String (Obj_Stamp) & ")");
         end if;

      else
         ALI := Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);
         Free (Text);

         if ALI = No_ALI_Id then
            Verbose_Msg (Full_Lib_File, "incorrectly formatted ALI file");
            return;

         elsif ALIs.Table (ALI).Ver (1 .. ALIs.Table (ALI).Ver_Len) /=
           Library_Version
         then
            Verbose_Msg (Full_Lib_File, "compiled with old GNAT version");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Don't take Ali file into account if it was generated without
         --  object.

         if Opt.Operating_Mode /= Opt.Check_Semantics
           and then ALIs.Table (ALI).No_Object
         then
            Verbose_Msg (Full_Lib_File, "has no corresponding object");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Check for matching compiler switches if needed

         if Opt.Check_Switches then
            Prev_Switch := ASCII.Nul;
            Num_Args    := 0;

            Get_Name_String (ALIs.Table (ALI).Sfile);

            for J in 1 .. Special_Args.Last loop
               if Special_Args.Table (J).File.all =
                 Name_Buffer (1 .. Name_Len)
               then
                  Special_Arg := Special_Args.Table (J).Args;
                  exit;
               end if;
            end loop;

            if Main_Project /= No_Project then
               null;
            end if;

            if Special_Arg = null then
               for J in Gcc_Switches.First .. Gcc_Switches.Last loop
                  --  Skip non switches, -I and -o switches

                  if (Gcc_Switches.Table (J) (1) = '-'
                        or else
                      Gcc_Switches.Table (J) (1) = Switch_Character)
                    and then Gcc_Switches.Table (J) (2) /= 'o'
                    and then Gcc_Switches.Table (J) (2) /= 'I'
                  then
                     Num_Args := Num_Args + 1;

                     --  Comparing switches is delicate because gcc reorders
                     --  a number of switches, according to lang-specs.h, but
                     --  gnatmake doesn't have the sufficient knowledge to
                     --  perform the same reordering.
                     --  Instead, ignore orders between different
                     --  "first letter" switches, but keep orders between same
                     --  switches, e.g -O -O2 is different than -O2 -O, but
                     --  -g -O is equivalent to -O -g.

                     if Gcc_Switches.Table (J) (2) /= Prev_Switch then
                        Prev_Switch := Gcc_Switches.Table (J) (2);
                        Arg :=
                          Units.Table (ALIs.Table (ALI).First_Unit).First_Arg;
                     end if;

                     Switch_Found := False;

                     for K in Arg ..
                       Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg
                     loop
                        if Gcc_Switches.Table (J).all = Args.Table (K).all then
                           Arg := K + 1;
                           Switch_Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Switch_Found then
                        if Opt.Verbose_Mode then
                           Verbose_Msg (ALIs.Table (ALI).Sfile,
                             "switch mismatch");
                        end if;

                        ALI := No_ALI_Id;
                        return;
                     end if;
                  end if;
               end loop;

            else
               for J in Special_Arg'Range loop

                  --  Skip non switches, -I and -o switches

                  if (Special_Arg (J) (1) = '-'
                    or else Special_Arg (J) (1) = Switch_Character)
                    and then Special_Arg (J) (2) /= 'o'
                    and then Special_Arg (J) (2) /= 'I'
                  then
                     Num_Args := Num_Args + 1;

                     if Special_Arg (J) (2) /= Prev_Switch then
                        Prev_Switch := Special_Arg (J) (2);
                        Arg :=
                          Units.Table (ALIs.Table (ALI).First_Unit).First_Arg;
                     end if;

                     Switch_Found := False;

                     for K in Arg ..
                       Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg
                     loop
                        if Special_Arg (J).all = Args.Table (K).all then
                           Arg := K + 1;
                           Switch_Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Switch_Found then
                        if Opt.Verbose_Mode then
                           Verbose_Msg (ALIs.Table (ALI).Sfile,
                             "switch mismatch");
                        end if;

                        ALI := No_ALI_Id;
                        return;
                     end if;
                  end if;
               end loop;
            end if;

            if Num_Args /=
              Integer (Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg -
                Units.Table (ALIs.Table (ALI).First_Unit).First_Arg + 1)
            then
               if Opt.Verbose_Mode then
                  Verbose_Msg (ALIs.Table (ALI).Sfile,
                    "different number of switches");
               end if;

               ALI := No_ALI_Id;
               return;
            end if;
         end if;

         --  Get the source files and their time stamps. Note that some
         --  sources may be missing if ALI is out-of-date.

         Set_Source_Table (ALI);

         Modified_Source := Time_Stamp_Mismatch (ALI);

         if Modified_Source /= No_File then
            ALI := No_ALI_Id;

            if Opt.Verbose_Mode then
               Source_Name := Full_Source_Name (Modified_Source);

               if Source_Name /= No_File then
                  Verbose_Msg (Source_Name, "time stamp mismatch");
               else
                  Verbose_Msg (Modified_Source, "missing");
               end if;
            end if;

         else
            New_Spec := First_New_Spec (ALI);

            if New_Spec /= No_File then
               ALI := No_ALI_Id;

               if Opt.Verbose_Mode then
                  Source_Name := Full_Source_Name (New_Spec);

                  if Source_Name /= No_File then
                     Verbose_Msg (Source_Name, "new spec");
                  else
                     Verbose_Msg (New_Spec, "old spec missing");
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Check;

   --------------------------
   -- Check_Linker_Options --
   --------------------------

   procedure Check_Linker_Options
     (E_Stamp   : Time_Stamp_Type;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type)
   is
      procedure Check_File (File : File_Name_Type);
      --  Update O_File and O_Stamp if the given file is younger than E_Stamp
      --  and O_Stamp, or if O_File is No_File and File does not exist.

      function Get_Library_File (Name : String) return File_Name_Type;
      --  Return the full file name including path of a library based
      --  on the name specified with the -l linker option, using the
      --  Ada object path. Return No_File if no such file can be found.

      type Char_Array is array (Natural) of Character;
      type Char_Array_Access is access constant Char_Array;

      Template : Char_Array_Access;
      pragma Import (C, Template, "_gnat_library_template");

      ----------------
      -- Check_File --
      ----------------

      procedure Check_File (File : File_Name_Type) is
         Stamp : Time_Stamp_Type;
         Name  : File_Name_Type := File;

      begin
         Get_Name_String (Name);

         --  Remove any trailing NUL characters

         while Name_Len >= Name_Buffer'First
           and then Name_Buffer (Name_Len) = NUL
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if Name_Len <= 0 then
            return;

         elsif Name_Buffer (1) = Get_Switch_Character
           or else Name_Buffer (1) = '-'
         then
            --  Do not check if File is a switch other than "-l"

            if Name_Buffer (2) /= 'l' then
               return;
            end if;

            --  The argument is a library switch, get actual name. It
            --  is necessary to make a copy of the relevant part of
            --  Name_Buffer as Get_Library_Name uses Name_Buffer as well.

            declare
               Base_Name : constant String := Name_Buffer (3 .. Name_Len);

            begin
               Name := Get_Library_File (Base_Name);
            end;

            if Name = No_File then
               return;
            end if;
         end if;

         Stamp := File_Stamp (Name);

         --  Find the youngest object file that is younger than the
         --  executable. If no such file exist, record the first object
         --  file that is not found.

         if (O_Stamp < Stamp and then E_Stamp < Stamp)
           or else (O_File = No_File and then Stamp (Stamp'First) = ' ')
         then
            O_Stamp := Stamp;
            O_File := Name;

            --  Strip the trailing NUL if present

            Get_Name_String (O_File);

            if Name_Buffer (Name_Len) = NUL then
               Name_Len := Name_Len - 1;
               O_File := Name_Find;
            end if;
         end if;
      end Check_File;

      ----------------------
      -- Get_Library_Name --
      ----------------------

      --  See comments in a-adaint.c about template syntax

      function Get_Library_File (Name : String) return File_Name_Type is
         File : File_Name_Type := No_File;

      begin
         Name_Len := 0;

         for Ptr in Template'Range loop
            case Template (Ptr) is
               when '*'    =>
                  Add_Str_To_Name_Buffer (Name);

               when ';'    =>
                  File := Full_Lib_File_Name (Name_Find);
                  exit when File /= No_File;
                  Name_Len := 0;

               when NUL    =>
                  exit;

               when others =>
                  Add_Char_To_Name_Buffer (Template (Ptr));
            end case;
         end loop;

         --  The for loop exited because the end of the template
         --  was reached. File contains the last possible filename
         --  for the library.

         if File = No_File and then Name_Len > 0 then
            File := Full_Lib_File_Name (Name_Find);
         end if;

         return File;
      end Get_Library_File;

   --  Start of processing for Check_Linker_Options

   begin
      O_File  := No_File;
      O_Stamp := (others => ' ');

      --  Process linker options from the ALI files.

      for Opt in 1 .. Linker_Options.Last loop
         Check_File (Linker_Options.Table (Opt).Name);
      end loop;

      --  Process options given on the command line.

      for Opt in Linker_Switches.First .. Linker_Switches.Last loop

         --  Check if the previous Opt has one of the two switches
         --  that take an extra parameter. (See GCC manual.)

         if Opt = Linker_Switches.First
           or else (Linker_Switches.Table (Opt - 1).all /= "-u"
                      and then
                    Linker_Switches.Table (Opt - 1).all /= "-Xlinker")
         then
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Linker_Switches.Table (Opt).all);
            Check_File (Name_Find);
         end if;
      end loop;

   end Check_Linker_Options;

   ---------------------
   -- Compile_Sources --
   ---------------------

   procedure Compile_Sources
     (Main_Source           : File_Name_Type;
      Args                  : Argument_List;
      First_Compiled_File   : out Name_Id;
      Most_Recent_Obj_File  : out Name_Id;
      Most_Recent_Obj_Stamp : out Time_Stamp_Type;
      Main_Unit             : out Boolean;
      Compilation_Failures  : out Natural;
      Check_Readonly_Files  : Boolean  := False;
      Do_Not_Execute        : Boolean  := False;
      Force_Compilations    : Boolean  := False;
      Keep_Going            : Boolean  := False;
      In_Place_Mode         : Boolean  := False;
      Initialize_ALI_Data   : Boolean  := True;
      Max_Process           : Positive := 1)
   is
      function Compile (S : Name_Id; L : Name_Id; Args : Argument_List)
        return Process_Id;
      --  Compiles S using Args. If S is a GNAT predefined source
      --  "-gnatpg" is added to Args. Non blocking call. L corresponds to the
      --  expected library filename. Process_Id of the process spawned to
      --  execute the compile.

      type Compilation_Data is record
         Pid              : Process_Id;
         Full_Source_File : File_Name_Type;
         Lib_File         : File_Name_Type;
         Source_Unit      : Unit_Name_Type;
      end record;

      Running_Compile : array (1 .. Max_Process) of Compilation_Data;
      --  Used to save information about outstanding compilations.

      Outstanding_Compiles : Natural := 0;
      --  Current number of outstanding compiles

      Source_Unit : Unit_Name_Type;
      --  Current source unit

      Source_File : File_Name_Type;
      --  Current source file

      Full_Source_File : File_Name_Type;
      --  Full name of the current source file

      Lib_File : File_Name_Type;
      --  Current library file

      Full_Lib_File : File_Name_Type;
      --  Full name of the current library file

      Obj_File : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File.

      Obj_Stamp : Time_Stamp_Type;
      --  Time stamp of the current object file.

      Sfile : File_Name_Type;
      --  Contains the source file of the units withed by Source_File

      ALI : ALI_Id;
      --  ALI Id of the current ALI file

      Compilation_OK  : Boolean;
      Need_To_Compile : Boolean;

      Pid  : Process_Id;
      Text : Text_Buffer_Ptr;

      Data : Prj.Project_Data;
      Gnatmake_Element : Package_Element;

      Arg_Index : Natural;
      --  Index in Special_Args.Table of a given compilation file

      procedure Add_Process
        (Pid   : Process_Id;
         Sfile : File_Name_Type;
         Afile : File_Name_Type;
         Uname : Unit_Name_Type);
      --  Adds process Pid to the current list of outstanding compilation
      --  processes and record the full name of the source file Sfile that
      --  we are compiling, the name of its library file Afile and the
      --  name of its unit Uname.

      procedure Await_Compile
        (Sfile : out File_Name_Type;
         Afile : out File_Name_Type;
         Uname : out Unit_Name_Type;
         OK    : out Boolean);
      --  Awaits that an outstanding compilation process terminates. When
      --  it does set Sfile to the name of the source file that was compiled
      --  Afile to the name of its library file and Uname to the name of its
      --  unit. Note that this time stamp can be used to check whether the
      --  compilation did generate an object file. OK is set to True if the
      --  compilation succeeded. Note that Sfile, Afile and Uname could be
      --  resp. No_File, No_File and No_Name  if there were no compilations
      --  to wait for.

      procedure Collect_Arguments_And_Compile;
      --  Collect arguments from project file (if any) and compile

      package Good_ALI is new Table.Table (
        Table_Component_Type => ALI_Id,
        Table_Index_Type     => Natural,
        Table_Low_Bound      => 1,
        Table_Initial        => 50,
        Table_Increment      => 100,
        Table_Name           => "Make.Good_ALI");
      --  Contains the set of valid ALI files that have not yet been scanned.

      procedure Record_Good_ALI (A : ALI_Id);
      --  Records in the previous set the Id of an ALI file.

      function Good_ALI_Present return Boolean;
      --  Returns True if any ALI file was recorded in the previous set.

      function Get_Next_Good_ALI return ALI_Id;
      --  Returns the next good ALI_Id record;

      procedure Record_Failure
        (File  : File_Name_Type;
         Unit  : Unit_Name_Type;
         Found : Boolean := True);
      --  Records in the previous table that the compilation for File failed.
      --  If Found is False then the compilation of File failed because we
      --  could not find it. Records also Unit when possible.

      function Bad_Compilation_Count return Natural;
      --  Returns the number of compilation failures.

      procedure Debug_Msg (S : String; N : Name_Id);
      --  If Debug.Debug_Flag_W is set outputs string S followed by name N.

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process
        (Pid   : Process_Id;
         Sfile : File_Name_Type;
         Afile : File_Name_Type;
         Uname : Unit_Name_Type)
      is
         OC1 : constant Positive := Outstanding_Compiles + 1;

      begin
         pragma Assert (OC1 <= Max_Process);
         pragma Assert (Pid /= Invalid_Pid);

         Running_Compile (OC1).Pid              := Pid;
         Running_Compile (OC1).Full_Source_File := Sfile;
         Running_Compile (OC1).Lib_File         := Afile;
         Running_Compile (OC1).Source_Unit      := Uname;

         Outstanding_Compiles := OC1;
      end Add_Process;

      --------------------
      -- Await_Compile --
      -------------------

      procedure Await_Compile
        (Sfile  : out File_Name_Type;
         Afile  : out File_Name_Type;
         Uname  : out File_Name_Type;
         OK     : out Boolean)
      is
         Pid : Process_Id;

      begin
         pragma Assert (Outstanding_Compiles > 0);

         Sfile := No_File;
         Afile := No_File;
         Uname := No_Name;
         OK    := False;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         for J in Running_Compile'First .. Outstanding_Compiles loop
            if Pid = Running_Compile (J).Pid then
               Sfile := Running_Compile (J).Full_Source_File;
               Afile := Running_Compile (J).Lib_File;
               Uname := Running_Compile (J).Source_Unit;

               --  To actually remove this Pid and related info from
               --  Running_Compile replace its entry with the last valid
               --  entry in Running_Compile.

               if J = Outstanding_Compiles then
                  null;

               else
                  Running_Compile (J) :=
                    Running_Compile (Outstanding_Compiles);
               end if;

               Outstanding_Compiles := Outstanding_Compiles - 1;
               return;
            end if;
         end loop;

         raise Program_Error;
      end Await_Compile;

      ---------------------------
      -- Bad_Compilation_Count --
      ---------------------------

      function Bad_Compilation_Count return Natural is
      begin
         return Bad_Compilation.Last - Bad_Compilation.First + 1;
      end Bad_Compilation_Count;

      -----------------------------------
      -- Collect_Arguments_And_Compile --
      -----------------------------------

      procedure Collect_Arguments_And_Compile is
      begin
         --  If no project file is used, then just call Compile with
         --  the specified Args.

         if Main_Project = No_Project then
            Pid := Compile (Full_Source_File, Lib_File, Args);

         --  A project file was used.

         else
            --  First check if the current source is an immediate
            --  source of a project file.

            if Opt.Verbose_Mode then
               Write_Eol;
               Write_Line ("Establishing Project context.");
            end if;

            declare
               Source_File_Name : constant String :=
                 Name_Buffer (1 .. Name_Len);
               Current_Project : Prj.Project_Id;
               Path_Name : File_Name_Type := Source_File;
               Gnatmake : Prj.Package_Id;
               Compiler_Package : Prj.Package_Id;
               Switches : Prj.Variable_Value;
               Object_File : String_Access;

            begin
               if Opt.Verbose_Mode then
                  Write_Str ("Checking if the Project File exists for """);
                  Write_Str (Source_File_Name);
                  Write_Line (""".");
               end if;

               Prj.Env.
                 Get_Reference
                 (Source_File_Name => Source_File_Name,
                  Project          => Current_Project,
                  Path             => Path_Name);

               if Current_Project = No_Project then

                  --  The current source is not an immediate source of
                  --  any project file.
                  --  Call Compile with the specified Args plus the saved
                  --  gcc switches.

                  if Opt.Verbose_Mode then
                     Write_Str ("No Project File.");
                     Write_Eol;
                  end if;

                  Pid := Compile
                    (Full_Source_File,
                     Lib_File,
                     Args & The_Saved_Gcc_Switches.all);

               --  We now know the project of the current source.

               else
               --  Set ADA_INCLUDE_PATH and ADA_OBJECTS_PATH if the project
               --  has changed.

               --  Note: this will modify these environment variables only
               --  for the current gnatmake process and all of its children
               --  (invocations of the compiler, the binder and the linker).

               --  The caller's ADA_INCLUDE_PATH and ADA_OBJECTS_PATH are
               --  not affected.

                  Set_Ada_Paths (Current_Project);

                  Data := Projects.Table (Current_Project);
                  Object_File :=
                    new String'
                    (Get_Name_String (Data.Object_Directory) &
                     Directory_Separator &
                     Object_File_Name (Source_File_Name));

                  if Opt.Verbose_Mode then
                     Write_Str ("Project file is """);
                     Write_Str (Get_Name_String (Data.Name));
                     Write_Str (""".");
                     Write_Eol;
                  end if;

                  if Opt.Verbose_Mode then
                     Write_Str ("Checking package gnatmake.");
                     Write_Eol;
                  end if;

                  --  We know look for package Gnatmake.Compiler
                  --  and get the switches from this package.

                  Gnatmake :=
                    Prj.Util.Value_Of
                    (Name        => Name_Gnatmake,
                     In_Packages => Data.Decl.Packages);

                  --  Package Gnatmake exists

                  if Gnatmake /= No_Package then

                     --  We now look for package Gnatmake.Compiler.

                     if Opt.Verbose_Mode then
                        Write_Str ("Checking package Compiler.");
                        Write_Eol;
                     end if;

                     Gnatmake_Element :=
                       Packages.Table (Gnatmake);

                     Compiler_Package :=
                       Prj.Util.Value_Of
                       (Name        => Name_Compiler,
                        In_Packages => Gnatmake_Element.Decl.Packages);

                     if Opt.Verbose_Mode then
                        Write_Str ("Getting the switches.");
                        Write_Eol;
                     end if;

                     --  If package Gnatmake.Compiler exists, we get
                     --  the specific switches for the current source,
                     --  or the global switches, if any.

                     Switches :=
                       Prj.Util.Value_Of
                       (Name                   => Source_File,
                        Variable_Or_Array_Name => Name_Switches,
                        In_Package             => Compiler_Package);
                  end if;

                  case Switches.Kind is

                     --  We have a list of switches. We add to Args
                     --  these switches, plus the saved gcc switches.

                     when List =>

                        declare
                           Current : String_List_Id := Switches.Values;
                           Element : String_Element;
                           Number  : Natural := 0;

                        begin
                           while Current /= Nil_String loop
                              Element := String_Elements.Table (Current);
                              Number  := Number + 1;
                              Current := Element.Next;
                           end loop;

                           declare
                              New_Args : Argument_List (1 .. Number);

                           begin
                              Current := Switches.Values;

                              for Index in New_Args'Range loop
                                 Element := String_Elements.Table (Current);
                                 String_To_Name_Buffer (Element.Value);
                                 New_Args (Index) :=
                                   new String' (Name_Buffer (1 .. Name_Len));
                                 Current := Element.Next;
                              end loop;

                              Pid := Compile
                                (Path_Name,
                                 Lib_File,
                                 Args & Output_Flag & Object_File &
                                 New_Args & The_Saved_Gcc_Switches.all);
                           end;
                        end;

                     --  We have a single switch. We add to Args
                     --  this switch, plus the saved gcc switches.

                     when Single =>

                        String_To_Name_Buffer (Switches.Value);
                        declare
                           New_Args : constant Argument_List :=
                             (1 => new String' (Name_Buffer (1 .. Name_Len)));

                        begin
                           Pid := Compile
                             (Path_Name,
                              Lib_File,
                              Args &
                              Output_Flag &
                              Object_File &
                              New_Args &
                              The_Saved_Gcc_Switches.all);
                        end;

                     --  We have no switches from Gnatmake.Compiler.
                     --  We add to Args the saved gcc switches.

                     when Undefined =>

                        if Opt.Verbose_Mode then
                           Write_Str ("There are no switches.");
                           Write_Eol;
                        end if;

                        Pid := Compile
                          (Path_Name,
                           Lib_File,
                           Args & Output_Flag & Object_File &
                           The_Saved_Gcc_Switches.all);

                  end case;
               end if;
            end;
         end if;
      end Collect_Arguments_And_Compile;

      -------------
      -- Compile --
      -------------

      function Compile (S : Name_Id; L : Name_Id; Args : Argument_List)
        return Process_Id
      is
         Comp_Args : Argument_List (Args'First .. Args'Last + 7);
         Comp_Next : Integer := Args'First;
         Comp_Last : Integer;

         function Ada_File_Name (Name : Name_Id) return Boolean;
         --  Returns True if Name is the name of an ada source file
         --  (i.e. suffix is .ads or .adb)

         function Ada_File_Name (Name : Name_Id) return Boolean is
         begin
            Get_Name_String (Name);
            return
              Name_Len > 4
                and then Name_Buffer (Name_Len - 3 .. Name_Len - 1) = ".ad"
                and then (Name_Buffer (Name_Len) = 'b'
                            or else
                          Name_Buffer (Name_Len) = 's');
         end Ada_File_Name;

      --  Start of processing for Compile

      begin
         Comp_Args (Comp_Next) := Comp_Flag;
         Comp_Next := Comp_Next + 1;

         --  Optimize the simple case where the gcc command line looks like
         --     gcc -c -I. ... -I- file.adb  --into->  gcc -c ... file.adb

         if Args (Args'First).all = "-I" & Normalized_CWD
           and then Args (Args'Last).all = "-I-"
           and then S = Strip_Directory (S)
         then
            Comp_Last := Comp_Next + Args'Length - 3;
            Comp_Args (Comp_Next .. Comp_Last) :=
              Args (Args'First + 1 .. Args'Last - 1);

         else
            Comp_Last := Comp_Next + Args'Length - 1;
            Comp_Args (Comp_Next .. Comp_Last) := Args;
         end if;

         --  Set -gnatpg for predefined files (for this purpose the renamings
         --  such as Text_IO do not count as predefined). Note that we strip
         --  the directory name from the source file name becase the call to
         --  Fname.Is_Predefined_File_Name cannot deal with directory prefixes.

         if Is_Predefined_File_Name (Strip_Directory (S), False) then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := GNAT_Flag;
         end if;

         --  Now check if the filename has one of the suffixes familiar to
         --  the gcc driver. If this is not the case then add the ada flag
         --  "-x ada".

         if not Ada_File_Name (S) then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Ada_Flag_1;
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Ada_Flag_2;
         end if;

         if L /= Strip_Directory (L) then

            --  Build -o argument.

            Get_Name_String (L);

            for J in reverse 1 .. Name_Len loop
               if Name_Buffer (J) = '.' then
                  Name_Len := J + Object_Suffix'Length - 1;
                  Name_Buffer (J .. Name_Len) := Object_Suffix;
                  exit;
               end if;
            end loop;

            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Output_Flag;
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := new String'(Name_Buffer (1 .. Name_Len));

         end if;

         Get_Name_String (S);

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'(Name_Buffer (1 .. Name_Len));

         Display (Gcc.all, Comp_Args (Args'First .. Comp_Last));

         if Gcc_Path = null then
            Osint.Fail ("error, unable to locate " & Gcc.all);
         end if;

         return
           GNAT.OS_Lib.Non_Blocking_Spawn
             (Gcc_Path.all, Comp_Args (Args'First .. Comp_Last));
      end Compile;

      ---------------
      -- Debug_Msg --
      ---------------

      procedure Debug_Msg (S : String; N : Name_Id) is
      begin
         if Debug.Debug_Flag_W then
            Write_Str ("   ... ");
            Write_Str (S);
            Write_Str (" ");
            Write_Name (N);
            Write_Eol;
         end if;
      end Debug_Msg;

      -----------------------
      -- Get_Next_Good_ALI --
      -----------------------

      function Get_Next_Good_ALI return ALI_Id is
         ALI : ALI_Id;

      begin
         pragma Assert (Good_ALI_Present);
         ALI := Good_ALI.Table (Good_ALI.Last);
         Good_ALI.Decrement_Last;
         return ALI;
      end Get_Next_Good_ALI;

      ----------------------
      -- Good_ALI_Present --
      ----------------------

      function Good_ALI_Present return Boolean is
      begin
         return Good_ALI.First <= Good_ALI.Last;
      end Good_ALI_Present;

      --------------------
      -- Record_Failure --
      --------------------

      procedure Record_Failure
        (File  : File_Name_Type;
         Unit  : Unit_Name_Type;
         Found : Boolean := True)
      is
      begin
         Bad_Compilation.Increment_Last;
         Bad_Compilation.Table (Bad_Compilation.Last) := (File, Unit, Found);
      end Record_Failure;

      ---------------------
      -- Record_Good_ALI --
      ---------------------

      procedure Record_Good_ALI (A : ALI_Id) is
      begin
         Good_ALI.Increment_Last;
         Good_ALI.Table (Good_ALI.Last) := A;
      end Record_Good_ALI;

   --  Start of processing for Compile_Sources

   begin
      pragma Assert (Args'First = 1);

      --  Package and Queue initializations.

      Good_ALI.Init;
      Bad_Compilation.Init;
      Output.Set_Standard_Error;
      Init_Q;

      if Initialize_ALI_Data then
         Initialize_ALI;
         Initialize_ALI_Source;
      end if;

      --  The following two flags affect the behavior of ALI.Set_Source_Table.
      --  We set Opt.Check_Source_Files to True to ensure that source file
      --  time stamps are checked, and we set Opt.All_Sources to False to
      --  avoid checking the presence of the source files listed in the
      --  source dependency section of an ali file (which would be a mistake
      --  since the ali file may be obsolete).

      Opt.Check_Source_Files := True;
      Opt.All_Sources        := False;

      Insert_Q (Main_Source);
      Mark (Main_Source);

      First_Compiled_File       := No_File;
      Most_Recent_Obj_File      := No_File;
      Main_Unit                 := False;

      --  Keep looping until there is no more work to do (the Q is empty)
      --  and all the outstanding compilations have terminated

      Make_Loop : while not Empty_Q or else Outstanding_Compiles > 0 loop

         --  If the user does not want to keep going in case of errors then
         --  wait for the remaining outstanding compiles and then exit.

         if Bad_Compilation_Count > 0 and then not Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile
                 (Full_Source_File, Lib_File, Source_Unit, Compilation_OK);

               if not Compilation_OK then
                  Record_Failure (Full_Source_File, Source_Unit);
               end if;
            end loop;

            exit Make_Loop;
         end if;

         --  PHASE 1: Check if there is more work that we can do (ie the Q
         --  is non empty). If there is, do it only if we have not yet used
         --  up all the available processes.

         if not Empty_Q and then Outstanding_Compiles < Max_Process then
            Extract_From_Q (Source_File, Source_Unit);
            Full_Source_File := Osint.Full_Source_Name (Source_File);
            Lib_File         := Osint.Lib_File_Name (Source_File);
            Full_Lib_File    := Osint.Full_Lib_File_Name (Lib_File);

            --  If the library file is an Ada library skip it

            if Full_Lib_File /= No_File
              and then In_Ada_Lib_Dir (Full_Lib_File)
            then
               Verbose_Msg (Lib_File, "is in an Ada library", Prefix => "  ");

            --  If the library file is a read-only library skip it

            elsif Full_Lib_File /= No_File
              and then not Check_Readonly_Files
              and then Is_Readonly_Library (Full_Lib_File)
            then
               Verbose_Msg
                 (Lib_File, "is a read-only library", Prefix => "  ");

            --  The source file that we are checking cannot be located

            elsif Full_Source_File = No_File then
               Record_Failure (Source_File, Source_Unit, False);

            --  Source and library files can be located but are internal
            --  files

            elsif not Check_Readonly_Files
              and then Full_Lib_File /= No_File
              and then Is_Internal_File_Name (Source_File)
            then
               Verbose_Msg
                 (Lib_File, "is an internal library", Prefix => "  ");

            --  The source file that we are checking can be located

            else
               --  Don't waste any time if we have to recompile anyway

               Obj_Stamp       := Empty_Time_Stamp;
               Need_To_Compile := Force_Compilations;

               if not Force_Compilations then
                  Check (Lib_File, ALI, Obj_File, Obj_Stamp);
                  Need_To_Compile := (ALI = No_ALI_Id);
               end if;

               if not Need_To_Compile then
                  --  The ALI file is up-to-date. Record its Id.

                  Record_Good_ALI (ALI);

                  --  Record the time stamp of the most recent object file
                  --  as long as no (re)compilations are needed.

                  if First_Compiled_File = No_File
                    and then (Most_Recent_Obj_File = No_File
                              or else Obj_Stamp > Most_Recent_Obj_Stamp)
                  then
                     Most_Recent_Obj_File  := Obj_File;
                     Most_Recent_Obj_Stamp := Obj_Stamp;
                  end if;

               else
                  --  Is this the first file we have to compile?

                  if First_Compiled_File = No_File then
                     First_Compiled_File  := Full_Source_File;
                     Most_Recent_Obj_File := No_File;

                     if Do_Not_Execute then
                        exit Make_Loop;
                     end if;
                  end if;

                  if In_Place_Mode then

                     --  If the library file was not found, then save the
                     --  library file near the source file.

                     if Full_Lib_File = No_File then
                        Get_Name_String (Full_Source_File);

                        for J in reverse 1 .. Name_Len loop
                           if Name_Buffer (J) = '.' then
                              Name_Buffer (J + 1 .. J + 3) := "ali";
                              Name_Len := J + 3;
                              exit;
                           end if;
                        end loop;

                        Lib_File := Name_Find;

                     --  If the library file was found, then save the
                     --  library file in the same place.

                     else
                        Lib_File := Full_Lib_File;
                     end if;

                  end if;

                  --  Check for special compilation flags

                  Arg_Index := 0;
                  Get_Name_String (Source_File);

                  --  Start the compilation and record it. We can do this
                  --  because there is at least one free process.

                  Collect_Arguments_And_Compile;

                  --  Make sure we could successfully start the compilation

                  if Pid = Invalid_Pid then
                     Record_Failure (Full_Source_File, Source_Unit);
                  else
                     Add_Process
                       (Pid, Full_Source_File, Lib_File, Source_Unit);
                  end if;
               end if;
            end if;
         end if;

         --  PHASE 2: Now check if we should wait for a compilation to
         --  finish. This is the case if all the available processes are
         --  busy compiling sources or there is nothing else to do
         --  (that is the Q is empty and there are no good ALIs to process).

         if Outstanding_Compiles = Max_Process
           or else (Empty_Q
                     and then not Good_ALI_Present
                     and then Outstanding_Compiles > 0)
         then
            Await_Compile
              (Full_Source_File, Lib_File, Source_Unit, Compilation_OK);

            if not Compilation_OK then
               Record_Failure (Full_Source_File, Source_Unit);

            else
               --  Re-read the updated library file

               Text := Read_Library_Info (Lib_File);

               --  If no ALI file was generated by this compilation nothing
               --  more to do, otherwise scan the ali file and record it.
               --  If the scan fails, a previous ali file is inconsistent with
               --  the unit just compiled.

               if Text /= null then
                  ALI :=
                    Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);

                  if ALI = No_ALI_Id then
                     Inform
                       (Lib_File, "incompatible ALI file, please recompile");
                     Record_Failure (Full_Source_File, Source_Unit);
                  else
                     Free (Text);
                     Record_Good_ALI (ALI);
                  end if;

               --  If we could not read the ALI file that was just generated
               --  then there could be a problem reading either the ALI or the
               --  corresponding object file (if Opt.Check_Object_Consistency
               --  is set Read_Library_Info checks that the time stamp of the
               --  object file is more recent than that of the ALI). For an
               --  example of problems caught by this test see [6625-009].

               else
                  Inform
                    (Lib_File,
                     "WARNING: ALI or object file not found after compile");
                  Record_Failure (Full_Source_File, Source_Unit);
               end if;
            end if;
         end if;

         exit Make_Loop when Unique_Compile;

         --  PHASE 3: Check if we recorded good ALI files. If yes process
         --  them now in the order in which they have been recorded. There
         --  are two occasions in which we record good ali files. The first is
         --  in phase 1 when, after scanning an existing ALI file we realise
         --  it is up-to-date, the second instance is after a successful
         --  compilation.

         while Good_ALI_Present loop
            ALI := Get_Next_Good_ALI;

            --  If we are processing the library file corresponding to the
            --  main source file check if this source can be a main unit.

            if ALIs.Table (ALI).Sfile = Main_Source then
               Main_Unit := ALIs.Table (ALI).Main_Program /= None;
            end if;

            --  Now insert in the Q the unmarked source files (i.e. those
            --  which have neever been inserted in the Q and hence never
            --  considered).

            for J in
              ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit
            loop
               for K in
                 Units.Table (J).First_With .. Units.Table (J).Last_With
               loop
                  Sfile := Withs.Table (K).Sfile;

                  if Sfile = No_File then
                     Debug_Msg ("Skipping generic:", Withs.Table (K).Uname);

                  elsif Is_Marked (Sfile) then
                     Debug_Msg ("Skipping marked file:", Sfile);

                  elsif not Check_Readonly_Files
                    and then Is_Internal_File_Name (Sfile)
                  then
                     Debug_Msg ("Skipping internal file:", Sfile);

                  else
                     Insert_Q (Sfile, Withs.Table (K).Uname);
                     Mark (Sfile);
                  end if;
               end loop;
            end loop;
         end loop;

      end loop Make_Loop;

      Compilation_Failures := Bad_Compilation_Count;

      --  Compilation is finisshed. Restore the gnat.adc,
      --  if it had been modified.

      if Main_Project /= No_Project then
         Prj.Env.Restore_Gnat_Adc;
      end if;

   end Compile_Sources;

   -------------
   -- Display --
   -------------

   procedure Display (Program : String; Args : Argument_List) is
   begin
      pragma Assert (Args'First = 1);

      if Display_Executed_Programs then
         Write_Str (Program);

         for J in Args'Range loop
            Write_Str (" ");
            Write_Str (Args (J).all);
         end loop;

         Write_Eol;
      end if;
   end Display;

   ----------------------
   -- Display_Commands --
   ----------------------

   procedure Display_Commands (Display : Boolean := True) is
   begin
      Display_Executed_Programs := Display;
   end Display_Commands;

   -------------
   -- Empty_Q --
   -------------

   function Empty_Q return Boolean is
   begin
      if Debug.Debug_Flag_P then
         Write_Str ("   Q := [");

         for J in Q_Front .. Q.Last - 1 loop
            Write_Str (" ");
            Write_Name (Q.Table (J).File);
            Write_Eol;
            Write_Str ("         ");
         end loop;

         Write_Str ("]");
         Write_Eol;
      end if;

      return Q_Front >= Q.Last;
   end Empty_Q;

   ---------------------
   -- Extract_Failure --
   ---------------------

   procedure Extract_Failure
     (File  : out File_Name_Type;
      Unit  : out Unit_Name_Type;
      Found : out Boolean)
   is
   begin
      File  := Bad_Compilation.Table (Bad_Compilation.Last).File;
      Unit  := Bad_Compilation.Table (Bad_Compilation.Last).Unit;
      Found := Bad_Compilation.Table (Bad_Compilation.Last).Found;
      Bad_Compilation.Decrement_Last;
   end Extract_Failure;

   --------------------
   -- Extract_From_Q --
   --------------------

   procedure Extract_From_Q
     (Source_File : out File_Name_Type;
      Source_Unit : out Unit_Name_Type)
   is
      File : constant File_Name_Type := Q.Table (Q_Front).File;
      Unit : constant Unit_Name_Type := Q.Table (Q_Front).Unit;

   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q - [ ");
         Write_Name (File);
         Write_Str (" ]");
         Write_Eol;
      end if;

      Q_Front := Q_Front + 1;
      Source_File := File;
      Source_Unit := Unit;
   end Extract_From_Q;

   --------------
   -- Gnatmake --
   --------------

   procedure Gnatmake is
      Main_Source_File : File_Name_Type;
      --  The source file containing the main compilation unit

      Compilation_Failures : Natural;

      Is_Main_Unit : Boolean;
      --  Set to True by Compile_Sources if the Main_Source_File can be a
      --  main unit.

      Main_ALI_File : File_Name_Type;
      --  The ali file corresponding to Main_Source_File

   begin
      Make.Initialize;

      if Hostparm.Java_VM then
         Gcc := new String'("jgnat");
         Gnatbind := new String'("jgnatbind");
         Gnatlink := new String '("jgnatlink");

         --  Do not check for an object file (".o") when compiling to
         --  Java bytecode since ".class" files are generated instead.

         Opt.Check_Object_Consistency := False;
      end if;

      if Opt.Verbose_Mode then
         Write_Eol;
         Write_Str ("GNATMAKE ");
         Write_Str (Gnatvsn.Gnat_Version_String);
         Write_Str (" Copyright 1995-2001 Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      --  Output usage information if no files

      if Osint.Number_Of_Files = 0 then
         Makeusg;
         Exit_Program (E_Fatal);

      elsif Osint.Number_Of_Files > 1 then
         Osint.Fail ("error, only one input source file allowed.");
      end if;

      --  If -l was specified behave as if -n was specified

      if Opt.List_Dependencies then
         Opt.Do_Not_Execute := True;
      end if;

      --  Note that Osint.Next_Main_Source will always return the (possibly
      --  abbreviated file) without any directory information.

      Main_Source_File := Next_Main_Source;

      if Project_File_Name = null then
         Add_Switch ("-I-", Compiler, And_Save => True);
         Add_Switch ("-I-", Binder, And_Save => True);
      end if;

      if Opt.Look_In_Primary_Dir then

         Add_Switch
           ("-I" &
            Normalize_Directory_Name
              (Get_Primary_Src_Search_Directory.all).all,
            Compiler, Append_Switch => False,
            And_Save => False);

         Add_Switch ("-aO" & Normalized_CWD,
                     Binder,
                     Append_Switch => False,
                     And_Save => False);
      end if;

      --  If the user wants a program without a main subprogram, add the
      --  appropriate switch to the binder.

      if Opt.No_Main_Subprogram then
         Add_Switch ("-z", Binder, And_Save => True);
      end if;

      if Project_File_Name /= null then

         --  A project file was specified by a -P switch

         if Opt.Verbose_Mode then
            Write_Eol;
            Write_Str ("Parsing Project File """);
            Write_Str (Project_File_Name.all);
            Write_Str (""".");
            Write_Eol;
         end if;

         --  Set the project parsing verbosity to whatever was specified
         --  by a possible -vP switch.

         Prj.Pars.Set_Verbosity (To => Current_Verbosity);

         --  Parse the project file.
         --  If there is an error, Main_Project will still be No_Project.

         Prj.Pars.Parse
           (Project           => Main_Project,
            Project_File_Name => Project_File_Name.all,
            Package_Name      => "gnatmake");

         if Main_Project = No_Project then
            Fail ("""" & Project_File_Name.all &
                  """ processing failed");
         end if;

         if Opt.Verbose_Mode then
            Write_Eol;
            Write_Str ("Parsing of Project File """);
            Write_Str (Project_File_Name.all);
            Write_Str (""" is finished.");
            Write_Eol;
         end if;

         --  Find the file name of the main unit

         declare
            Main_Source_File_Name : constant String :=
                                      Get_Name_String (Main_Source_File);
            Main_Unit_File_Name   : constant String :=
                                      Prj.Env.File_Name_Of_Library_Unit_Body
                                        (Name    => Main_Source_File_Name,
                                         Project => Main_Project);

            Gnatmake : constant Prj.Package_Id :=
                         Prj.Util.Value_Of
                           (Name        => Name_Gnatmake,
                            In_Packages =>
                              Projects.Table (Main_Project).Decl.Packages);

         begin

            --  We fail if we cannot find the main source file
            --  as an immediate source of the main project file.

            if Main_Unit_File_Name = "" then
               Fail ('"' & Main_Source_File_Name  &
                     """ is not a unit of project " &
                     Project_File_Name.all & ".");
            else

               --  Remove any directory information from the main
               --  source file name.

               declare
                  Pos : Natural := Main_Unit_File_Name'Last;

               begin
                  loop
                     exit when Pos < Main_Unit_File_Name'First or else
                       Main_Unit_File_Name (Pos) = Directory_Separator;
                     Pos := Pos - 1;
                  end loop;

                  Name_Len := Main_Unit_File_Name'Last - Pos;

                  Name_Buffer (1 .. Name_Len) :=
                    Main_Unit_File_Name
                    (Pos + 1 .. Main_Unit_File_Name'Last);

                  Main_Source_File := Name_Find;

                  if Opt.Verbose_Mode then
                     Write_Str ("Main source file: """);
                     Write_Str (Main_Unit_File_Name
                                (Pos + 1 .. Main_Unit_File_Name'Last));
                     Write_Str (""".");
                     Write_Eol;
                  end if;
               end;
            end if;

            --  If there is a package gnatmake in the main project
            --  file, add the switches from it.
            --  We also add the switches from packages
            --  Gnatmake.Binder and Gnatmake.Linker, if any.

            if Gnatmake /= No_Package then
               declare
                  The_Packages : Package_Id :=
                    Packages.Table (Gnatmake).Decl.Packages;
                  Binder_Package : constant Prj.Package_Id :=
                    Prj.Util.Value_Of
                      (Name        => Name_Binder,
                       In_Packages => The_Packages);

                  Linker_Package : constant Prj.Package_Id :=
                    Prj.Util.Value_Of
                      (Name => Name_Linker,
                       In_Packages => The_Packages);

               begin
                  if Opt.Verbose_Mode then
                     Write_Str ("Adding gnatmake switches for """);
                     Write_Str (Main_Unit_File_Name);
                     Write_Str (""".");
                     Write_Eol;
                  end if;

                  Add_Switches
                    (File_Name   => Main_Unit_File_Name,
                     The_Package => Gnatmake,
                     Program     => None);

                  if Opt.Verbose_Mode then
                     Write_Str ("Adding binder switches for """);
                     Write_Str (Main_Unit_File_Name);
                     Write_Str (""".");
                     Write_Eol;
                  end if;

                  Add_Switches
                    (File_Name   => Main_Unit_File_Name,
                     The_Package => Binder_Package,
                     Program     => Binder);

                  if Opt.Verbose_Mode then
                     Write_Str ("Adding linker switches for""");
                     Write_Str (Main_Unit_File_Name);
                     Write_Str (""".");
                     Write_Eol;
                  end if;

                  Add_Switches
                    (File_Name   => Main_Unit_File_Name,
                     The_Package => Linker_Package,
                     Program     => Linker);
               end;
            end if;
         end;

         --  Generate (if necessary) gnat.adc

         Prj.Env.Create_Gnat_Adc (Main_Project);

      end if;

      Display_Commands (not Opt.Quiet_Output);

      --  We now put in the Binder_Switches and Linker_Switches tables,
      --  the binder and linker switches of the command line,
      --  that have been put in the Saved_ tables.
      --  If a project file was used, then the command line switches
      --  will follow the project file switches.

      for J in 1 .. Saved_Binder_Switches.Last loop
         Add_Switch
           (Saved_Binder_Switches.Table (J),
            Binder,
            And_Save => False);
      end loop;

      for J in 1 .. Saved_Linker_Switches.Last loop
         Add_Switch
           (Saved_Linker_Switches.Table (J),
            Linker,
            And_Save => False);
      end loop;

      --  If no project file is used, we just put the gcc switches
      --  from the command line in the Gcc_Switches table.

      if Main_Project = No_Project then
         for J in 1 .. Saved_Gcc_Switches.Last loop
            Add_Switch
              (Saved_Gcc_Switches.Table (J),
               Compiler,
              And_Save => False);
         end loop;

      else

         --  A project file is used.
         --  We add the source directories and the object directories
         --  to the search paths.

         Add_Source_Directories (Main_Project);
         Add_Object_Directories (Main_Project);

         --  And we put the command line gcc switches in the variable
         --  The_Saved_Gcc_Switches. They are going to be used later
         --  in procedure Compile_Sources.

         The_Saved_Gcc_Switches :=
           new Argument_List (1 .. Saved_Gcc_Switches.Last);

         for J in 1 .. Saved_Gcc_Switches.Last loop
            The_Saved_Gcc_Switches (J) := Saved_Gcc_Switches.Table (J);
         end loop;
      end if;

      --  If there was a --GCC, --GNATBIND or --GNATLINK switch on
      --  the command line, then we have to use it, even if there was
      --  another switch in the project file.

      if Saved_Gcc /= null then
         Gcc := Saved_Gcc;
      end if;

      if Saved_Gnatbind /= null then
         Gnatbind := Saved_Gnatbind;
      end if;

      if Saved_Gnatlink /= null then
         Gnatlink := Saved_Gnatlink;
      end if;

      Gcc_Path       := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
      Gnatbind_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
      Gnatlink_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);

      --  Here is where the make process is started

      Recursive_Compilation_Step : declare
         Args : Argument_List (1 .. Gcc_Switches.Last);

         First_Compiled_File : Name_Id;

         Youngest_Obj_File   : Name_Id;
         Youngest_Obj_Stamp  : Time_Stamp_Type;

         Executable          : File_Name_Type := No_File;
         Executable_Stamp    : Time_Stamp_Type;
         Executable_Obsolete : Boolean := True;
         --  Executable is the final executable program.

      begin
         for J in 1 .. Gcc_Switches.Last loop
            Args (J) := Gcc_Switches.Table (J);
         end loop;

         --  Look inside the linker switches to see if the name of the final
         --  executable program was specified.

         for J in Linker_Switches.First .. Linker_Switches.Last loop
            if Linker_Switches.Table (J).all = Output_Flag.all then
               pragma Assert (J < Linker_Switches.Last);

               Name_Len := Linker_Switches.Table (J + 1)'Length;
               Name_Buffer (1 .. Name_Len) :=
                 Linker_Switches.Table (J + 1).all;

               --  if target has an executable suffix and it has not been
               --  specified then it is added here.

               if Executable_Suffix'Length /= 0
                 and then Linker_Switches.Table (J + 1)
                    (Name_Len - Executable_Suffix'Length + 1
                     .. Name_Len) /= Executable_Suffix
               then
                  Name_Buffer (Name_Len + 1 ..
                               Name_Len + Executable_Suffix'Length) :=
                    Executable_Suffix;
                  Name_Len := Name_Len + Executable_Suffix'Length;
               end if;

               Executable := Name_Enter;

               Verbose_Msg (Executable, "final executable");

            end if;
         end loop;

         --  If the name of the final executable program was not specified
         --  then construct it from the main input file.

         if Executable = No_File then
            Executable := Executable_Name (Strip_Suffix (Main_Source_File));
         end if;

         Compile_Sources
           (Main_Source           => Main_Source_File,
            Args                  => Args,
            First_Compiled_File   => First_Compiled_File,
            Most_Recent_Obj_File  => Youngest_Obj_File,
            Most_Recent_Obj_Stamp => Youngest_Obj_Stamp,
            Main_Unit             => Is_Main_Unit,
            Compilation_Failures  => Compilation_Failures,
            Check_Readonly_Files  => Opt.Check_Readonly_Files,
            Do_Not_Execute        => Opt.Do_Not_Execute,
            Force_Compilations    => Opt.Force_Compilations,
            In_Place_Mode         => Opt.In_Place_Mode,
            Keep_Going            => Opt.Keep_Going,
            Initialize_ALI_Data   => True,
            Max_Process           => Opt.Maximum_Processes);

         if Opt.Verbose_Mode then
            Write_Str ("End of compilation");
            Write_Eol;
         end if;

         if Compilation_Failures /= 0 then
            List_Bad_Compilations;
            raise Compilation_Failed;
         end if;

         if Opt.List_Dependencies then
            if First_Compiled_File /= No_File then
               Inform (First_Compiled_File,
                       "must be recompiled. Can't generate dependence list.");
            else
               List_Depend;
            end if;

         elsif First_Compiled_File = No_File
           and then Opt.Compile_Only
           and then not Opt.Quiet_Output
         then
            if Unique_Compile then
               Inform (Msg => "object up to date.");
            else
               Inform (Msg => "objects up to date.");
            end if;

         elsif Opt.Do_Not_Execute
           and then First_Compiled_File /= No_File
         then
            Write_Name (First_Compiled_File);
            Write_Eol;
         end if;

         --  Stop after compile step if any of:

         --    1) -n (Do_Not_Execute) specified

         --    2) -l (List_Dependencies) specified (also sets Do_Not_Execute
         --       above, so this is probably superfluous).

         --    3) -c (Compile_Only) specified

         --    4) Made unit cannot be a main unit

         if (Opt.Do_Not_Execute
             or Opt.List_Dependencies
             or Opt.Compile_Only
             or not Is_Main_Unit)
           and then not No_Main_Subprogram
         then
            return;
         end if;

         --  If the objects were up-to-date check if the executable file
         --  is also up-to-date.
         --  For now always bind and link on the JVM since there is currently
         --  no simple way to check the up-to-date status of objects

         if not Hostparm.Java_VM and then First_Compiled_File = No_File then
            Executable_Stamp    := File_Stamp (Executable);

            Executable_Obsolete := Youngest_Obj_Stamp > Executable_Stamp;

            if not Executable_Obsolete then

               --  If no Ada object files obsolete the executable, check
               --  for younger or missing linker files.

               Check_Linker_Options
                 (Executable_Stamp, Youngest_Obj_File, Youngest_Obj_Stamp);

               Executable_Obsolete := Youngest_Obj_File /= No_File;

            end if;

            --  Return if the executable is up to date
            --  and otherwise motivate the relink/rebind.

            if not Executable_Obsolete then

               if not Opt.Quiet_Output then
                  Inform (Executable, "up to date.");
               end if;

               return;
            end if;

            if Executable_Stamp (1) = ' ' then
               Verbose_Msg (Executable, "missing.", Prefix => "  ");

            elsif Youngest_Obj_Stamp (1) = ' ' then
               Verbose_Msg (Youngest_Obj_File, "missing.", Prefix => "  ");

            else
               Verbose_Msg (Youngest_Obj_File,
                            "(" & String (Youngest_Obj_Stamp) & ") newer than",
                            Executable, "(" & String (Executable_Stamp) & ")");
            end if;
         end if;
      end Recursive_Compilation_Step;

      --  Following declare should have a label xxx_Step for consistency
      --  with blocks around it ???

      declare
         ALI_File : File_Name_Type;
         Src_File : File_Name_Type;

      begin
         Src_File      := Strip_Directory (Main_Source_File);
         ALI_File      := Lib_File_Name (Src_File);
         Main_ALI_File := Full_Lib_File_Name (ALI_File);

         --  When In_Place_Mode, the library file can be located in the
         --  Main_Source_File directory which may not be present in the
         --  library path. In this case, use the corresponding library file
         --  name.

         if Main_ALI_File = No_File and then Opt.In_Place_Mode then
            Get_Name_String (Get_Directory (Full_Source_Name (Src_File)));
            Get_Name_String_And_Append (ALI_File);
            Main_ALI_File := Name_Find;
            Main_ALI_File := Full_Lib_File_Name (Main_ALI_File);
         end if;

         pragma Assert (Main_ALI_File /= No_File);
      end;

      Bind_Step : declare
         Args : Argument_List (Binder_Switches.First .. Binder_Switches.Last);

      begin
         for J in Binder_Switches.First .. Binder_Switches.Last loop
            Args (J) := Binder_Switches.Table (J);
         end loop;

         if Main_Project /= No_Project then
            Set_Ada_Paths (Main_Project);
         end if;

         Bind (Main_ALI_File, Args);
      end Bind_Step;

      Link_Step : declare
         Args : Argument_List (Linker_Switches.First .. Linker_Switches.Last);

      begin
         for J in Linker_Switches.First .. Linker_Switches.Last loop
            Args (J) := Linker_Switches.Table (J);
         end loop;

         Link (Main_ALI_File, Args);
      end Link_Step;

      Exit_Program (E_Success);

   exception
      when Bind_Failed =>
         Osint.Fail ("*** bind failed.");

      when Compilation_Failed =>
         Exit_Program (E_Fatal);

      when Link_Failed =>
         Osint.Fail ("*** link failed.");

      when X : others =>
         Write_Line (Exception_Information (X));
         Osint.Fail ("INTERNAL ERROR. Please report.");

   end Gnatmake;

   --------------------
   -- In_Ada_Lib_Dir --
   --------------------

   function In_Ada_Lib_Dir (File : File_Name_Type) return Boolean is
      D : constant Name_Id := Get_Directory (File);
      B : constant Byte    := Get_Name_Table_Byte (D);

   begin
      return (B and Ada_Lib_Dir) /= 0;
   end In_Ada_Lib_Dir;

   ------------
   -- Inform --
   ------------

   procedure Inform (N : Name_Id := No_Name; Msg : String) is
   begin
      Osint.Write_Program_Name;

      Write_Str (": ");

      if N /= No_Name then
         Write_Str ("""");
         Write_Name (N);
         Write_Str (""" ");
      end if;

      Write_Str (Msg);
      Write_Eol;
   end Inform;

   ------------
   -- Init_Q --
   ------------

   procedure Init_Q is
   begin
      if not First_Q_Initialization then
         First_Q_Initialization := False;

         --  Unmark source files which were previously marked & enqueued.

         for J in Q.First .. Q.Last - 1 loop
            Unmark (Source_File => Q.Table (J).File);
         end loop;
      end if;

      Q_Front := Q.First;
      Q.Set_Last (Q.First);
   end Init_Q;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Next_Arg : Positive;

   begin
      --  Override default initialization of Check_Object_Consistency
      --  since this is normally False for GNATBIND, but is True for
      --  GNATMAKE since we do not need to check source consistency
      --  again once GNATMAKE has looked at the sources to check.

      Opt.Check_Object_Consistency := True;

      --  Package initializations. The order of calls is important here.

      Output.Set_Standard_Error;
      Osint.Initialize (Osint.Make);

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Namet.Initialize;

      Snames.Initialize;

      Prj.Initialize;

      Next_Arg := 1;
      Scan_Args : while Next_Arg <= Argument_Count loop
         Scan_Make_Arg (Argument (Next_Arg), And_Save => True);
         Next_Arg := Next_Arg + 1;
      end loop Scan_Args;

      if Usage_Requested then
         Makeusg;
      end if;

      --  Test for trailing -o switch

      if Opt.Output_Filename_Present
        and then not Output_Filename_Seen
      then
         Fail ("Output filename missing after -o");
      end if;

      Osint.Add_Default_Search_Dirs;

      --  Mark the GNAT libraries if needed.

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      Osint.Source_File_Data (Cache => True);

      --  Read gnat.adc file to initialize Fname.UF

      Fname.UF.Initialize;
      begin
         Fname.SF.Read_Source_File_Name_Pragmas;
      exception
         when SFN_Scan.Syntax_Error_In_GNAT_ADC =>
            Osint.Fail ("syntax error in gnat.adc.");
      end;

   end Initialize;

   --------------
   -- Insert_Q --
   --------------

   procedure Insert_Q
     (Source_File : File_Name_Type;
      Source_Unit : Unit_Name_Type := No_Name)
   is
   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q + [ ");
         Write_Name (Source_File);
         Write_Str (" ] ");
         Write_Eol;
      end if;

      Q.Table (Q.Last).File := Source_File;
      Q.Table (Q.Last).Unit := Source_Unit;
      Q.Increment_Last;
   end Insert_Q;

   ----------------------------
   -- Is_External_Assignment --
   ----------------------------

   function Is_External_Assignment (Argv : String) return Boolean is
      Start     : Positive := 3;
      Finish    : Natural := Argv'Last;
      Equal_Pos : Natural;

   begin
      if Argv'Last < 5 then
         return False;

      elsif Argv (3) = '"' then
         if Argv (Argv'Last) /= '"' or else Argv'Last < 7 then
            return False;
         else
            Start := 4;
            Finish := Argv'Last - 1;
         end if;
      end if;

      Equal_Pos := Start;

      while Equal_Pos <= Finish and then Argv (Equal_Pos) /= '=' loop
         Equal_Pos := Equal_Pos + 1;
      end loop;

      if Equal_Pos = Start
        or else Equal_Pos >= Finish
      then
         return False;

      else
         Prj.Ext.Add
           (External_Name => Argv (Start .. Equal_Pos - 1),
            Value         => Argv (Equal_Pos + 1 .. Finish));
         return True;
      end if;
   end Is_External_Assignment;

   ---------------
   -- Is_Marked --
   ---------------

   function Is_Marked (Source_File : File_Name_Type) return Boolean is
   begin
      return Get_Name_Table_Byte (Source_File) /= 0;
   end Is_Marked;

   ----------
   -- Link --
   ----------

   procedure Link (ALI_File : File_Name_Type; Args : Argument_List) is
      Link_Args : Argument_List (Args'First .. Args'Last + 1);
      Success   : Boolean;

   begin
      Link_Args (Args'Range) :=  Args;

      Get_Name_String (ALI_File);
      Link_Args (Args'Last + 1) := new String'(Name_Buffer (1 .. Name_Len));

      Display (Gnatlink.all, Link_Args);

      if Gnatlink_Path = null then
         Osint.Fail ("error, unable to locate " & Gnatlink.all);
      end if;

      GNAT.OS_Lib.Spawn (Gnatlink_Path.all, Link_Args, Success);

      if not Success then
         raise Link_Failed;
      end if;
   end Link;

   ---------------------------
   -- List_Bad_Compilations --
   ---------------------------

   procedure List_Bad_Compilations is
   begin
      for J in Bad_Compilation.First .. Bad_Compilation.Last loop
         if Bad_Compilation.Table (J).File = No_File then
            null;
         elsif not Bad_Compilation.Table (J).Found then
            Inform (Bad_Compilation.Table (J).File, "not found");
         else
            Inform (Bad_Compilation.Table (J).File, "compilation error");
         end if;
      end loop;
   end List_Bad_Compilations;

   -----------------
   -- List_Depend --
   -----------------

   procedure List_Depend is
      Lib_Name  : Name_Id;
      Obj_Name  : Name_Id;
      Src_Name  : Name_Id;

      Len       : Natural;
      Line_Pos  : Natural;
      Line_Size : constant := 77;

   begin
      Set_Standard_Output;

      for A in ALIs.First .. ALIs.Last loop
         Lib_Name := ALIs.Table (A).Afile;

         --  We have to provide the full library file name in In_Place_Mode

         if Opt.In_Place_Mode then
            Lib_Name := Full_Lib_File_Name (Lib_Name);
         end if;

         Obj_Name := Object_File_Name (Lib_Name);
         Write_Name (Obj_Name);
         Write_Str (" :");

         Get_Name_String (Obj_Name);
         Len := Name_Len;
         Line_Pos := Len + 2;

         for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
            Src_Name := Sdep.Table (D).Sfile;

            if Is_Internal_File_Name (Src_Name)
              and then not Check_Readonly_Files
            then
               null;
            else
               if not Opt.Quiet_Output then
                  Src_Name := Full_Source_Name (Src_Name);
               end if;

               Get_Name_String (Src_Name);
               Len := Name_Len;

               if Line_Pos + Len + 1 > Line_Size then
                  Write_Str (" \");
                  Write_Eol;
                  Line_Pos := 0;
               end if;

               Line_Pos := Line_Pos + Len + 1;

               Write_Str (" ");
               Write_Name (Src_Name);
            end if;
         end loop;

         Write_Eol;
      end loop;

      Set_Standard_Error;
   end List_Depend;

   ----------
   -- Mark --
   ----------

   procedure Mark (Source_File : File_Name_Type) is
   begin
      Set_Name_Table_Byte (Source_File, 1);
   end Mark;

   -------------------
   -- Mark_Dir_Path --
   -------------------

   procedure Mark_Dir_Path
     (Path : in String_Access;
      Mark : in Lib_Mark_Type)
   is
      Dir : String_Access;

   begin
      if Path /= null then
         Osint.Get_Next_Dir_In_Path_Init (Path);

         loop
            Dir := Osint.Get_Next_Dir_In_Path (Path);
            exit when Dir = null;
            Mark_Directory (Dir.all, Mark);
         end loop;
      end if;
   end Mark_Dir_Path;

   --------------------
   -- Mark_Directory --
   --------------------

   procedure Mark_Directory
     (Dir  : in String;
      Mark : in Lib_Mark_Type)
   is
      N : Name_Id;
      B : Byte;

   begin
      --  Dir last character is supposed to be a directory separator.

      Name_Len := Dir'Length;
      Name_Buffer (1 .. Name_Len) := Dir;

      if not Is_Directory_Separator (Name_Buffer (Name_Len)) then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Directory_Separator;
      end if;

      --  Add flags to the alredy existing flags.

      N := Name_Find;
      B := Get_Name_Table_Byte (N);
      Set_Name_Table_Byte (N, B or Mark);
   end Mark_Directory;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (Source : String) return String is
      Pos : Natural := Source'Last;

   begin
      while Pos >= Source'First and then
        Source (Pos) /= '.' loop
         Pos := Pos - 1;
      end loop;

      if Pos >= Source'First then
         Pos := Pos - 1;
      end if;

      return Source (Source'First .. Pos) & Object_Suffix;
   end Object_File_Name;

   -------------------
   -- Scan_Make_Arg --
   -------------------

   procedure Scan_Make_Arg (Argv : String; And_Save : Boolean) is
   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      --  If the previous switch has set the Output_Filename_Present
      --  flag (that is we have seen a -o), then the next argument is
      --  the name of the output executable.

      if Opt.Output_Filename_Present and then not Output_Filename_Seen then
         Output_Filename_Seen := True;

         if Argv (1) = Switch_Character or else Argv (1) = '-' then
            Fail ("Output filename missing after -o");
         else
            Add_Switch ("-o", Linker, And_Save => And_Save);

            --  add automatically the executable suffix if it has not been
            --  specified explicitly.

            if Executable_Suffix'Length /= 0
              and then Argv (Argv'Last - Executable_Suffix'Length + 1
                             .. Argv'Last) /= Executable_Suffix
            then
               Add_Switch
                 (Argv & Executable_Suffix,
                  Linker,
                  And_Save => And_Save);
            else
               Add_Switch (Argv, Linker, And_Save => And_Save);
            end if;
         end if;

      --  Then check if we are dealing with a -cargs, -bargs or -largs

      elsif (Argv (1) = Switch_Character or else Argv (1) = '-')
        and then (Argv (2 .. Argv'Last) = "cargs"
                   or else Argv (2 .. Argv'Last) = "bargs"
                   or else Argv (2 .. Argv'Last) = "largs")
      then
         if not File_Name_Seen then
            Fail ("-cargs, -bargs, -largs ",
                  "must appear after unit or file name");
         end if;

         case Argv (2) is
            when 'c' => Program_Args := Compiler;
            when 'b' => Program_Args := Binder;
            when 'l' => Program_Args := Linker;

            when others =>
               raise Program_Error;
         end case;

      --  A special test is needed for the -o switch within a -largs
      --  since that is another way to specify the name of the final
      --  executable.

      elsif Program_Args = Linker
        and then (Argv (1) = Switch_Character or else Argv (1) = '-')
        and then Argv (2 .. Argv'Last) = "o"
      then
         Fail ("Switch -o not allowed within a -largs. Use -o directly.");

      --  Check to see if we are reading switches after a -cargs,
      --  -bargs or -largs switch. If yes save it.

      elsif Program_Args /= None then

         --  Check to see if we are reading -I switches in order
         --  to take into account in the src & lib search directories.

         if Argv'Length > 2 and then Argv (1 .. 2) = "-I" then
            if Argv (3 .. Argv'Last) = "-" then
               Opt.Look_In_Primary_Dir := False;

            elsif Program_Args = Compiler then
               if Argv (3 .. Argv'Last) /= "-" then
                  Add_Src_Search_Dir (Argv (3 .. Argv'Last));

               end if;

            elsif Program_Args = Binder then
               Add_Lib_Search_Dir (Argv (3 .. Argv'Last));

            end if;
         end if;

         Add_Switch (Argv, Program_Args, And_Save => And_Save);

      --  Handle non-default compiler, binder, linker

      elsif Argv'Length > 2 and then Argv (1 .. 2) = "--" then
         if Argv'Length > 6
           and then Argv (1 .. 6) = "--GCC="
         then
            declare
               Program_Args : Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (7 .. Argv'Last));

            begin
               if And_Save then
                  Saved_Gcc := new String'(Program_Args.all (1).all);
               else
                  Gcc := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all,
                     Compiler,
                     And_Save => And_Save);
               end loop;
            end;

         elsif Argv'Length > 11
           and then Argv (1 .. 11) = "--GNATBIND="
         then
            declare
               Program_Args : Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (12 .. Argv'Last));

            begin
               if And_Save then
                  Saved_Gnatbind := new String'(Program_Args.all (1).all);
               else
                  Gnatbind := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all, Binder, And_Save => And_Save);
               end loop;
            end;

         elsif Argv'Length > 11
           and then Argv (1 .. 11) = "--GNATLINK="
         then
            declare
               Program_Args : Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (12 .. Argv'Last));
            begin
               if And_Save then
                  Saved_Gnatlink := new String'(Program_Args.all (1).all);
               else
                  Gnatlink := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch (Program_Args.all (J).all, Linker);
               end loop;
            end;

         else
            Fail ("Unknown switch: ", Argv);

         end if;

      --  If we have seen a regular switch process it

      elsif Argv (1) = Switch_Character or else Argv (1) = '-' then

         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");

         --  -I-

         elsif Argv (2 .. Argv'Last) = "I-" then
            Opt.Look_In_Primary_Dir := False;

         --  Forbid  -?-  or  -??-  where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Fail ("Trailing ""-"" at the end of ", Argv, " forbidden.");

         --  -Idir

         elsif Argv (2) = 'I' then
            Add_Src_Search_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Search_Dir (Argv (3 .. Argv'Last));
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch ("-aO" & Argv (3 .. Argv'Last),
                        Binder,
                        And_Save => And_Save);

            --  No need to pass any source dir to the binder
            --  since gnatmake call it with the -x flag
            --  (ie do not check source time stamp)

         --  -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Src_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch ("-I" & Argv (4 .. Argv'Last),
                        Compiler,
                        And_Save => And_Save);

         --  -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch (Argv, Binder, And_Save => And_Save);

         --  -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Mark_Directory (Argv (4 .. Argv'Last), Ada_Lib_Dir);
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch ("-aO" & Argv (4 .. Argv'Last),
                        Binder,
                        And_Save => And_Save);

         --  -Adir (to gnatbind this is like a -aO switch, to gcc like a -I)

         elsif Argv (2) = 'A' then
            Mark_Directory (Argv (3 .. Argv'Last), Ada_Lib_Dir);
            Add_Src_Search_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Search_Dir (Argv (3 .. Argv'Last));
            Add_Switch ("-I"  & Argv (3 .. Argv'Last),
                        Compiler,
                        And_Save => And_Save);
            Add_Switch ("-aO" & Argv (3 .. Argv'Last),
                        Binder,
                        And_Save => And_Save);

         --  -Ldir

         elsif Argv (2) = 'L' then
            Add_Switch (Argv, Linker, And_Save => And_Save);

         --  -g -pg (give the switch to both the compiler and the linker)

         elsif
           (Argv (2) = 'g' and then (Argv'Last = 2
                                       or else Argv (3) in '0' .. '3'))
             or else Argv (2 .. Argv'Last) = "pg"
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Linker, And_Save => And_Save);

         --  -j (need to save the result)

         elsif Argv (2) = 'j' then
            Scan_Switches (Argv);

            if And_Save then
               Saved_Maximum_Processes := Maximum_Processes;
            end if;

         --  -m

         elsif Argv (2) = 'm'
           and then Argv'Last = 2
         then
            Opt.Minimal_Recompilation := True;

         --  -u

         elsif Argv (2) = 'u'
           and then Argv'Last = 2
         then
            Unique_Compile   := True;
            Opt.Compile_Only := True;

         --  -Pprj (only once, and only on the command line)

         elsif Argv'Last > 2
           and then Argv (2) = 'P'
         then
            if Project_File_Name /= null then
               Fail ("cannot have several project files specified");

            elsif not And_Save then

               --  It could be a tool other than gnatmake (i.e, gnatdist)
               --  or a -P switch inside a project file.

               Fail
                 ("either the tool is not ""project-aware"" or " &
                  "a project file is specified inside a project file");

            else
               Project_File_Name := new String' (Argv (3 .. Argv'Last));
            end if;

         --  -S (Assemble)
         --  Since no object file is created, don't check object
         --  consistency.

         elsif Argv (2) = 'S'
           and then Argv'Last = 2
         then
            Opt.Check_Object_Consistency := False;
            Add_Switch (Argv, Compiler, And_Save => And_Save);

         --  -vPx  (verbosity of the parsing of the project files)

         elsif Argv'Last = 4
           and then Argv (2 .. 3) = "vP"
           and then Argv (4) in '0' .. '2'
         then
            if And_Save then
               case Argv (4) is
                  when '0' =>
                     Current_Verbosity := Prj.Default;
                  when '1' =>
                     Current_Verbosity := Prj.Medium;
                  when '2' =>
                     Current_Verbosity := Prj.High;
                  when others =>
                     null;
               end case;
            end if;

         --  -Wx (need to save the result)

         elsif Argv (2) = 'W' then
            Scan_Switches (Argv);

            if And_Save then
               Saved_WC_Encoding_Method := Wide_Character_Encoding_Method;
               Saved_WC_Encoding_Method_Set := True;
            end if;

         --  -Xext=val  (External assignment)

         elsif Argv (2) = 'X'
           and then Is_External_Assignment (Argv)
         then
            --  Is_External_Assignment has side effects
            --  when it returns True;

            null;

         --  If -gnath is present, then generate the usage information
         --  right now for the compiler, and do not pass this option
         --  on to the compiler calls.

         elsif Argv = "-gnath" then
            null;

         --  By default all switches with more than one character
         --  or one character switches which are not in 'a' .. 'z'
         --  are passed to the compiler, unless we are dealing
         --  with a -jnum switch or a debug switch (starts with 'd')

         elsif Argv'Length > 5
           and then Argv (2 .. 5) = "gnat"
           and then Argv (6) = 'c'
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Opt.Operating_Mode := Opt.Check_Semantics;
            Opt.Check_Object_Consistency := False;
            Opt.Compile_Only             := True;

         elsif Argv (2 .. Argv'Last) = "nostdlib" then

            --  Don't pass -nostdlib to gnatlink, it will disable
            --  linking with all standard library files.

            Opt.No_Stdlib := True;
            Add_Switch (Argv, Binder, And_Save => And_Save);

         elsif Argv (2 .. Argv'Last) = "nostdinc" then
            Opt.No_Stdinc := True;
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

         elsif Argv (2) /= 'd'
           and then Argv (2 .. Argv'Last) /= "M"
           and then (Argv'Length > 2 or else Argv (2) not in 'a' .. 'z')
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);

         --  All other options are handled by Scan_Switches

         else
            Scan_Switches (Argv);
         end if;

      --  If not a switch it must be a file name

      else
         File_Name_Seen := True;
         Set_Main_File_Name (Argv);
      end if;
   end Scan_Make_Arg;

   -------------------
   -- Set_Ada_Paths --
   -------------------

   procedure Set_Ada_Paths (For_Project : Prj.Project_Id) is
      New_Ada_Include_Path : constant String_Access :=
        Prj.Env.Ada_Include_Path (For_Project);

      New_Ada_Objects_Path : constant String_Access :=
        Prj.Env.Ada_Objects_Path (For_Project);

      --  Comments needed in following code ???

   begin
      if New_Ada_Include_Path /= Current_Ada_Include_Path then
         Current_Ada_Include_Path := New_Ada_Include_Path;

         if Original_Ada_Include_Path'Length = 0 then
            Setenv ("ADA_INCLUDE_PATH",
                    New_Ada_Include_Path.all);

         else
            Setenv ("ADA_INCLUDE_PATH",
                    Original_Ada_Include_Path.all &
                    Path_Separator &
                    New_Ada_Include_Path.all);
         end if;

         if Opt.Verbose_Mode then
            declare
               Include_Path : constant String_Access :=
                 Getenv ("ADA_INCLUDE_PATH");

            begin
               Write_Str ("ADA_INCLUDE_PATH = """);
               Write_Str (Include_Path.all);
               Write_Str ("""");
               Write_Eol;
            end;
         end if;
      end if;

      if New_Ada_Objects_Path /= Current_Ada_Objects_Path then
         Current_Ada_Objects_Path := New_Ada_Objects_Path;

         if Original_Ada_Objects_Path'Length = 0 then
            Setenv ("ADA_OBJECTS_PATH",
                    New_Ada_Objects_Path.all);

         else
            Setenv ("ADA_OBJECTS_PATH",
                    Original_Ada_Objects_Path.all &
                    Path_Separator &
                    New_Ada_Objects_Path.all);
         end if;

         if Opt.Verbose_Mode then
            declare
               Objects_Path : constant String_Access :=
                 Getenv ("ADA_OBJECTS_PATH");

            begin
               Write_Str ("ADA_OBJECTS_PATH = """);
               Write_Str (Objects_Path.all);
               Write_Str ("""");
               Write_Eol;
            end;
         end if;
      end if;

   end Set_Ada_Paths;

   ------------
   -- Unmark --
   ------------

   procedure Unmark (Source_File : File_Name_Type) is
   begin
      Set_Name_Table_Byte (Source_File, 0);
   end Unmark;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1     : Name_Id;
      S1     : String;
      N2     : Name_Id := No_Name;
      S2     : String  := "";
      Prefix : String := "  -> ")
   is
   begin
      if not Opt.Verbose_Mode then
         return;
      end if;

      Write_Str (Prefix);
      Write_Str ("""");
      Write_Name (N1);
      Write_Str (""" ");
      Write_Str (S1);

      if N2 /= No_Name then
         Write_Str (" """);
         Write_Name (N2);
         Write_Str (""" ");
      end if;

      Write_Str (S2);
      Write_Eol;
   end Verbose_Msg;

end Make;
