------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M A K E                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

with ALI;      use ALI;
with Csets;
with Debug;
with Fname;    use Fname;
with Gnatvsn;  use Gnatvsn;
with Hostparm; use Hostparm;
with Makeusg;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Gnatvsn;
with Output;   use Output;
with Switch;   use Switch;
with Table;
with Types;    use Types;

with Ada.Command_Line; use Ada.Command_Line;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Make is

   use Ascii;
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

   procedure Insert_Q (Source_File : File_Name_Type);
   --  Inserts Source_File at the end of Q.

   function Empty_Q return Boolean;
   --  Returns True if Q is empty.

   function Extract_From_Q return File_Name_Type;
   --  Extracts the first element from the Q.

   First_Q_Initialization : Boolean := True;
   --  Will be set to false after Init_Q has been called once.

   Q_Front : Natural;
   --  Points to the first valid element in the Q.

   package Q is new Table.Table (
     Table_Component_Type => File_Name_Type,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => 4000,
     Table_Increment      => 100,
     Table_Name           => "Make.Q");
   --  This is the actual Q.

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
   --  can be used directly in a Makefile. For this routine to work
   --  Compile_Sources must be called before hand. Also because this routine
   --  uses the ALI files that were originally loaded and scanned by
   --  Compile_Sources, no additional ALI files should be scanned between the
   --  call to Compile_Sources and List_Depend.

   procedure Inform (N : Name_Id := No_Name; Msg : String);
   --  Prints out the program name followed by a colon, N and S.
   --  What is S??? What is Msg???

   No_Indent : constant Boolean := False;
   procedure Verbose_Msg
     (N1  : Name_Id;
      S1  : String;
      N2  : Name_Id := No_Name;
      S2  : String  := "";
      Ind : Boolean := True);
   --  If the verbose flag is set prints the corresponding message.
   --  If Ind is set to No_Indent (False) the message is not,
   --  otherwise it is.
   --  What are the parameters here ???

   procedure Verbose_Msg
     (N1  : Name_Id;
      S1a : String;
      S1b : String;
      S1c : String;
      N2  : Name_Id;
      S2a : String;
      S2b : String;
      S2c : String);
   --  If the verbose flag is set prints the corresponding message.
   --  Created to print longer messages with time stamps information.
   --  What are the parameters here ???

   function "&" (Left, Right : String) return String;
   --  Concatenation routine

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

   function In_Ada_Lib_Dir  (File : in File_Name_Type) return Boolean;
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

   ----------------------------------------------------
   -- Compiler, Binder & Linker Data and Subprograms --
   ----------------------------------------------------

   Gcc             : String_Access := Program_Name ("gcc");
   Gnatbind        : String_Access := Program_Name ("gnatbind");
   Gnatlink        : String_Access := Program_Name ("gnatlink");
   --  Default compiler, binder, linker programs

   Gcc_Path        : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
   Gnatbind_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
   Gnatlink_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);
   --  Path for compiler, binder, linker programs, defaulted now for gnatdist.
   --  Changed later if overridden on command line.

   Output_Flag     : constant String_Access := new String'("-o");
   Comp_Flag       : constant String_Access := new String'("-c");
   Ada_Flag_1      : constant String_Access := new String'("-x");
   Ada_Flag_2      : constant String_Access := new String'("ada");
   GNAT_Flag       : constant String_Access := new String'("-gnatg");
   Dont_Check_Flag : constant String_Access := new String'("-x");

   Object_Suffix   : constant String := Get_Object_Suffix.all;

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

   procedure Add_Switch
     (S   : String;
      T   : Make_Program_Type);
   procedure Add_Switch
     (S1  : String;
      S2  : String;
      T   : Make_Program_Type;
      Pos : Integer);
   procedure Add_Switch
     (S1  : String;
      S2  : String;
      T   : Make_Program_Type);
   --  Make invokes one of three programs (the compiler, the binder or the
   --  linker). For the sake of convenience, some program specific switches
   --  can be passed directly on the gnatmake commande line, hence they need
   --  to be recorded so that gnamake can pass them to the right program.
   --  In the above calls, S is a switch to be added, or S1 and S2 are two
   --  separate switches to be added at the end of the command line for T.
   --  If Pos is set then the switch is inserted in position Pos in the
   --  command line (thus shifting the position of all other switches).
   --  Pos must be a valid switch position).
   --
   --  Note from RBKD, it would be cleaner to have one procedure with some
   --  default parameters (null for S2 and -1 for Pos for example) ???

   procedure Check
     (Lib_File  : File_Name_Type;
      ALI       : out ALI_Id;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type);
   --  Determines whether the library file Lib_File is up-to-date or not.
   --  The full name (with path information) of the object file
   --  corresponding to Lib_File is returned in O_File. Its time stamp is
   --  saved in O_Stamp. ALI is the ALI_Id corresponding to Lib_File. If
   --  Lib_File in not up-to-date, then the coresponding source file needs
   --  to be recompiled. In this case ALI = No_ALI_Id.

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args if variable
   --  Display_Executed_Programs is set. The lower bound of Args must be 1.

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : String) return String is
      Result : String (1 .. Left'Length + Right'Length);
   begin
      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Result'Last) := Right;
      return Result;
   end "&";

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch (S : String; T : Make_Program_Type) is
   begin
      case T is
         when Compiler =>
            Gcc_Switches.Increment_Last;
            Gcc_Switches.Table (Gcc_Switches.Last) := new String'(S);

         when Binder   =>
            Binder_Switches.Increment_Last;
            Binder_Switches.Table (Binder_Switches.Last) := new String'(S);

         when Linker   =>
            Linker_Switches.Increment_Last;
            Linker_Switches.Table (Linker_Switches.Last) := new String'(S);

         when None =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Add_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (S1  : String;
      S2  : String;
      T   : Make_Program_Type;
      Pos : Integer)
   is
      Tmp : String (1 .. S1'Length + S2'Length);

   begin
      Tmp (1 .. S1'Length) := S1;
      Tmp (S1'Length + 1 .. S1'Length + S2'Length) := S2;

      case T is
         when Compiler =>
            pragma Assert
              (Gcc_Switches.First <= Pos and Pos <= Gcc_Switches.Last);

            Gcc_Switches.Increment_Last;
            for J in reverse Pos + 1 .. Gcc_Switches.Last loop
               Gcc_Switches.Table (J) := Gcc_Switches.Table (J - 1);
            end loop;

            Gcc_Switches.Table (Pos) := new String'(Tmp);

         when Binder   =>
            pragma Assert
              (Binder_Switches.First <= Pos
               and Pos <= Binder_Switches.Last);

            Binder_Switches.Increment_Last;
            for J in reverse Pos + 1 .. Binder_Switches.Last loop
               Binder_Switches.Table (J) := Binder_Switches.Table (J - 1);
            end loop;

            Binder_Switches.Table (Pos) := new String'(Tmp);

            --  For the time being this facility is not available for the
            --  linker but can be trivially implemented.

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

   end Add_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (S1 : String;
      S2 : String;
      T  : Make_Program_Type)
   is
      Tmp : String (1 .. S1'Length + S2'Length);

   begin
      Tmp (1 .. S1'Length) := S1;
      Tmp (S1'Length + 1 .. S1'Length + S2'Length) := S2;
      Add_Switch (Tmp, T);
   end Add_Switch;

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
      Bind_Args (Bind_Last) := Dont_Check_Flag;

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

            Name_Buffer (Name_Len) := 's';
            Spec_Name := Name_Find;
            File_Name := Get_File_Name (Spec_Name);

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
            exit U_Chk when Unit.Table (U).Utype = Is_Body_Only
               and then New_Spec (Unit.Table (U).Uname);

            for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
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

   --  Start of processing for Check

   begin
      pragma Assert (Lib_File /= No_File);

      Text := Read_Library_Info (Lib_File);
      Full_Lib_File := Full_Library_Info_Name;
      Full_Obj_File := Full_Object_File_Name;
      Lib_Stamp     := Current_Library_File_Stamp;
      Obj_Stamp     := Current_Object_File_Stamp;

      if Full_Lib_File = No_File then
         Verbose_Msg (Lib_File, "being checked ...", Ind => No_Indent);
      else
         Verbose_Msg (Full_Lib_File, "being checked ...", Ind => No_Indent);
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
              (Full_Lib_File, "(", String (Lib_Stamp), ") newer than",
               Full_Obj_File, "(", String (Obj_Stamp), ")");
         end if;

      else
         ALI := Scan_ALI (Lib_File, Text, Err => True);

         if ALI = No_ALI_Id then
            Verbose_Msg (Full_Lib_File, "incorrectly formatted ALI file");
            return;

         elsif ALIs.Table (ALI).Ver /= Library_Version then
            Verbose_Msg (Full_Lib_File, "compiled with old GNAT version");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Don't take Ali file into account if it was generated without
         --  object.

         if  Opt.Operating_Mode /= Opt.Check_Semantics
           and then ALIs.Table (ALI).No_Object
         then
            Verbose_Msg (Full_Lib_File, "has no corresponding object");
            ALI := No_ALI_Id;
            return;
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
      Missing_ALIs          : out Boolean;
      Check_Readonly_Files  : Boolean  := False;
      Dont_Execute          : Boolean  := False;
      Force_Compilations    : Boolean  := False;
      Keep_Going            : Boolean  := False;
      In_Place_Mode         : Boolean  := False;
      Initialize_ALI_Data   : Boolean  := True;
      Max_Process           : Positive := 1)
   is
      function Compile (S : Name_Id; L : Name_Id) return Process_Id;
      --  Compiles S using Args above. If S is a GNAT predefined source
      --  "-gnatg" is added to Args. Non blocking call. L correponds to the
      --  expected library filename.  Process_Id of the process spawned to
      --  execute the compile.

      type Compilation_Data is record
         Pid              : Process_Id;
         Full_Source_File : File_Name_Type;
         Lib_File         : File_Name_Type;
      end record;

      Running_Compile : array (1 .. Max_Process) of Compilation_Data;
      --  Used to save information about outstanding compilations.

      Outstanding_Compiles : Natural := 0;
      --  Current number of outstanding compiles

      procedure Add_Process (Pid : Process_Id; S, L : File_Name_Type);
      --  Adds process Pid to the current list of outstanding compilation
      --  processes and record the full name of the source file S that we
      --  are compiling and the name of its library file L.

      procedure Await_Compile (S, L : out File_Name_Type; OK : out Boolean);
      --  Awaits that an outstanding compilation process terminates. When
      --  it does set S to the name of the Source file that was compiled and
      --  L the name of its library file. Note that this time stamp can be
      --  used to check whether the compilation did generate an object
      --  file. OK is set to True if the compilation succeeded. Note that S
      --  and L could be No_File if there were no compilations to wait for.

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

      type Bad_Compilation_Info is record
         File  : File_Name_Type;
         Found : Boolean;
      end record;
      --  File is the name of the file for which a compilation failed
      --  Found is False if the compilation failed because the file
      --  could not be found.

      package Bad_Compilation is new Table.Table (
        Table_Component_Type => Bad_Compilation_Info,
        Table_Index_Type     => Natural,
        Table_Low_Bound      => 1,
        Table_Initial        => 20,
        Table_Increment      => 100,
        Table_Name           => "Make.Bad_Compilation");
      --  Full name of all the source files for which compilation fails.

      procedure Record_Failure (F : File_Name_Type; Found : Boolean := True);
      --  Records in the previous table that the compilation for F failed.
      --  If Found is False then the compilation of F failed because we could
      --  not find it.

      function Compilations_Failed return Boolean;
      --  Returns True if a previous compilation failed.

      procedure List_Bad_Compilations;
      --  Prints out the list of all files for which the compilation failed.

      procedure Debug_Msg (S : String; N : Name_Id);
      --  If Debug.Debug_Flag_W is set outputs string S followed by name N.

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process (Pid : Process_Id; S, L : File_Name_Type) is
         OC1 : constant Positive := Outstanding_Compiles + 1;

      begin
         pragma Assert (OC1 <= Max_Process);
         pragma Assert (Pid /= Invalid_Pid);

         Running_Compile (OC1).Pid              := Pid;
         Running_Compile (OC1).Full_Source_File := S;
         Running_Compile (OC1).Lib_File         := L;

         Outstanding_Compiles := OC1;
      end Add_Process;

      --------------------
      -- Await_Compile --
      -------------------

      procedure Await_Compile (S, L : out File_Name_Type; OK : out Boolean) is
         Pid : Process_Id;

      begin
         pragma Assert (Outstanding_Compiles > 0);

         S  := No_File;
         L  := No_File;
         OK := False;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         for J in Running_Compile'First .. Outstanding_Compiles loop
            if Pid = Running_Compile (J).Pid then
               S := Running_Compile (J).Full_Source_File;
               L := Running_Compile (J).Lib_File;

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

         pragma Assert (False);
         raise Program_Error;
      end Await_Compile;

      -------------------------
      -- Compilations_Failed --
      -------------------------

      function Compilations_Failed return Boolean is
      begin
         return Bad_Compilation.First <= Bad_Compilation.Last;
      end Compilations_Failed;

      -------------
      -- Compile --
      -------------

      function Compile (S : Name_Id; L : Name_Id) return Process_Id is

         Comp_Args : Argument_List (Args'First .. Args'Last + 7);
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
         Comp_Args (Args'First) := Comp_Flag;

         --  Optimize the simple case where the gcc command line looks like
         --     gcc -c -I. ... -I- file.adb  --into->  gcc -c ... file.adb

         if Args (Args'First).all = "-I" & Normalized_CWD
           and then Args (Args'Last).all = "-I-"
           and then S = Strip_Directory (S)
         then
            Comp_Last := Args'Last - 1;
            Comp_Args (Args'First + 1 .. Comp_Last) :=
                                 Args (Args'First + 1 .. Args'Last - 1);

         else
            Comp_Last := Args'Last + 1;
            Comp_Args (Args'First + 1 .. Comp_Last) := Args;
         end if;

         --  Set -gnatg for predefined files (for this purpose the renamings
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

      --------------------
      -- Record_Failure --
      --------------------

      procedure Record_Failure (F : File_Name_Type; Found : Boolean := True) is
      begin
         Bad_Compilation.Increment_Last;
         Bad_Compilation.Table (Bad_Compilation.Last) := (F, Found);
      end Record_Failure;

      ---------------------
      -- Record_Good_ALI --
      ---------------------

      procedure Record_Good_ALI (A : ALI_Id) is
      begin
         Good_ALI.Increment_Last;
         Good_ALI.Table (Good_ALI.Last) := A;
      end Record_Good_ALI;

      --------------------------
      -- Compile_Sources Data --
      --------------------------

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

      First_Compiled_File   := No_File;
      Most_Recent_Obj_File  := No_File;
      Main_Unit             := False;
      Missing_ALIs          := False;

      --  Keep looping until there is no more work to do (the Q is empty)
      --  and all the outstanding compilations have terminated

      Make_Loop : while not Empty_Q or else Outstanding_Compiles > 0 loop

         --  If the user does not want to keep going in case of erros then
         --  wait for the remaining outstanding compiles and then exit.

         if Compilations_Failed and then not Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile (Full_Source_File, Lib_File, Compilation_OK);

               if not Compilation_OK then
                  Record_Failure (Full_Source_File);
               end if;
            end loop;

            exit Make_Loop;
         end if;

         --  PHASE 1: Check if there is more work that we can do (ie the Q
         --  is non empty). If there is, do it only if we have not yet used
         --  up all the available processes.

         if not Empty_Q and then Outstanding_Compiles < Max_Process then
            Source_File      := Extract_From_Q;
            Full_Source_File := Osint.Full_Source_Name (Source_File);
            Lib_File         := Osint.Lib_File_Name (Source_File);
            Full_Lib_File    := Osint.Full_Lib_File_Name (Lib_File);

            --  If the library file is an Ada library skip it

            if Full_Lib_File /= No_File
              and then In_Ada_Lib_Dir (Full_Lib_File)
            then
               Verbose_Msg
                 (Lib_File, "is in an Ada library", Ind => No_Indent);

            --  If the library file is a read-only library skip it

            elsif Full_Lib_File /= No_File
              and then not Check_Readonly_Files
              and then Is_Readonly_Library (Full_Lib_File)
            then
               Verbose_Msg
                 (Lib_File, "is a read-only library", Ind => No_Indent);

            --  The source file that we are checking cannot be located

            elsif Full_Source_File = No_File then
               Record_Failure (Source_File, Found => False);

            --  Source and library files can be located but are internal
            --  files

            elsif not Check_Readonly_Files
              and then Full_Lib_File /= No_File
              and then Is_Internal_File_Name (Source_File)
            then
               Verbose_Msg
                 (Lib_File, "is an internal library", Ind => No_Indent);

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

                     if Dont_Execute then
                        exit Make_Loop;
                     end if;
                  end if;

                  if In_Place_Mode then

                     --  If the library file was not found, then save the
                     --  library file near the source file.

                     if Full_Lib_File = No_File then
                        Get_Name_String (Full_Source_File);

                        for I in reverse 1 .. Name_Len loop
                           if Name_Buffer (I) = '.' then
                              Name_Buffer (I + 1 .. I + 3) := "ali";
                              Name_Len := I + 3;
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

                  --  Start the compilation and record it. We can do this
                  --  because there is at least one free process.

                  Pid := Compile (Full_Source_File, Lib_File);

                  --  Make sure we could successfully start the compilation

                  if Pid = Invalid_Pid then
                     Record_Failure (Full_Source_File);
                  else
                     Add_Process (Pid, Full_Source_File, Lib_File);
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
            Await_Compile (Full_Source_File, Lib_File, Compilation_OK);

            if not Compilation_OK then
               Record_Failure (Full_Source_File);

            else
               --  Re-read the updated library file.

               Text := Read_Library_Info (Lib_File);

               --  If no ALI file was generated by this compilation nothing
               --  more to do, otherwise scan the ali file and record it

               if Text /= null then
                  ALI := Scan_ALI (Lib_File, Text);
                  Record_Good_ALI (ALI);

               --  This should probably just be Assert (False) now. It is
               --  almost certainly junk code, dating from the time when
               --  generics could produce junk objects (with no error
               --  indication) but still did not generate an ali file ???

               else
                  Inform (Lib_File, "WARNING file not found after compile");
                  Missing_ALIs := True;
               end if;
            end if;
         end if;

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
                 Unit.Table (J).First_With .. Unit.Table (J).Last_With
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
                     Insert_Q (Sfile);
                     Mark (Sfile);
                  end if;
               end loop;
            end loop;
         end loop;

      end loop Make_Loop;

      --  If any compilation failed, report it

      if Compilations_Failed then
         List_Bad_Compilations;
         raise Compilation_Failed;
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
            Write_Name (Q.Table (J));
            Write_Eol;
            Write_Str ("         ");
         end loop;

         Write_Str ("]");
         Write_Eol;
      end if;

      return Q_Front >= Q.Last;
   end Empty_Q;

   --------------------
   -- Extract_From_Q --
   --------------------

   function Extract_From_Q return File_Name_Type is
      Elmt : constant File_Name_Type := Q.Table (Q_Front);

   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q - [ ");
         Write_Name (Elmt);
         Write_Str (" ]");
         Write_Eol;
      end if;

      Q_Front := Q_Front + 1;
      return Elmt;
   end Extract_From_Q;

   --------------------
   -- In_Ada_Lib_Dir --
   --------------------

   function In_Ada_Lib_Dir (File : in File_Name_Type) return Boolean is
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Next_Arg : Positive;

   begin
      --  Default initialization of the flags affecting gnatmake

      Opt.Check_Readonly_Files     := False;
      Opt.Check_Object_Consistency := True;
      Opt.Compile_Only             := False;
      Opt.Dont_Execute             := False;
      Opt.Force_Compilations       := False;
      Opt.Quiet_Output             := False;
      Opt.Minimal_Recompilation    := False;
      Opt.Verbose_Mode             := False;

      --  Package initializations. The order of calls is important here.

      Output.Set_Standard_Error;
      Osint.Initialize (Osint.Make); --  reads gnatmake switches

      if Switch.Usage_Requested then
         Makeusg;
      end if;

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Namet.Initialize;

      Next_Arg := 1;
      Scan_Args : loop
         exit when Next_Arg > Argument_Count;
         Scan_Make_Arg (Argument (Next_Arg));
         Next_Arg := Next_Arg + 1;
      end loop Scan_Args;

      Osint.Add_Default_Search_Dirs;

      --  Mark the GNAT libraries if needed.

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      Osint.Source_File_Data (Cache => True);

   end Initialize;

   --------------
   -- Gnatmake --
   --------------

   procedure Gnatmake is

      Main_Source_File : File_Name_Type;
      --  The source file containing the main compilation unit

      Has_Missing_ALIs : Boolean;
      --  Set True if there was a missing ali file (can this ever happen???)

      Is_Main_Unit     : Boolean;
      --  If True the Main_Source_File can be a main unit (is this used???
      --  It is not used here for sure???)

      Main_ALI_File : File_Name_Type;
      --  The ali file corresponding to Main_Source_File

   begin
      Initialize;

      if Opt.Verbose_Mode then
         Write_Eol;
         Write_Str ("GNATMAKE ");
         Write_Str (Gnatvsn.Gnat_Version_String);
         Write_Str (" Copyright 1995-1998 Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      --  Output usage information if more than one file

      if Osint.Number_Of_Files = 0 then
         Makeusg;
         Exit_Program (E_Fatal);

      elsif Osint.Number_Of_Files > 1 then
         Osint.Fail ("error, only one input source file allowed.");
      end if;

      --  If -l was specified behave as if -n was specified

      if Opt.List_Dependencies then
         Opt.Dont_Execute := True;
      end if;

      --  Note that Osint.Next_Main_Source will always return the (possibly
      --  abbreviated file) without any directory information.

      Main_Source_File := Next_Main_Source;

      Add_Switch ("-I-", Compiler);
      Add_Switch ("-I-", Binder);

      if Opt.Look_In_Primary_Dir then

         Add_Switch
           ("-I",
            Normalize_Directory_Name
              (Get_Primary_Src_Search_Directory.all).all,
            Compiler,
            Pos => Gcc_Switches.First);

         Add_Switch
           ("-aO",
            Normalized_CWD,
            Binder,
            Pos => Binder_Switches.First);
      end if;

      Display_Commands (not Opt.Quiet_Output);

      --  Here is where the make process is started

      Gcc_Path       := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
      Gnatbind_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
      Gnatlink_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);

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

               Executable := Name_Enter;
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
            Missing_ALIs          => Has_Missing_ALIs,
            Check_Readonly_Files  => Opt.Check_Readonly_Files,
            Dont_Execute          => Opt.Dont_Execute,
            Force_Compilations    => Opt.Force_Compilations,
            In_Place_Mode         => Opt.In_Place_Mode,
            Keep_Going            => Opt.Keep_Going,
            Initialize_ALI_Data   => True,
            Max_Process           => Opt.Maximum_Processes);

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
            Inform (Msg => "objects up to date.");

         elsif Opt.Dont_Execute and then First_Compiled_File /= No_File then
            Write_Name (First_Compiled_File);
            Write_Eol;
         end if;

         --  Stop after compile step if any of:
         --  1) -n (Dont_Execute) specified
         --  2) -l (List_Dependencies) specified (also sets Dont_Execute
         --     above, so this is probably superfluous).
         --  3) -c (Compile_Only) specified
         --  4) Made unit cannot be a main unit

         if Opt.Dont_Execute
           or Opt.List_Dependencies
           or Opt.Compile_Only
           or not Is_Main_Unit
         then
            return;
         end if;

         --  If the objects were up-to-date check if the executable file
         --  is also up-to-date.

         if First_Compiled_File = No_File then
            Executable_Stamp    := File_Stamp (Executable);
            Executable_Obsolete := Youngest_Obj_Stamp > Executable_Stamp;

            if not Executable_Obsolete and then not Opt.Quiet_Output then
               Inform (Executable, "up to date.");
            end if;

            if not Executable_Obsolete then
               return;
            else
               if Executable_Stamp (1) = ' ' then
                  Verbose_Msg (Executable, "missing.", Ind => No_Indent);

               else
                  Verbose_Msg (Executable, "obsolete.", Ind => No_Indent);
                  Verbose_Msg (Youngest_Obj_File,
                               "(", String (Youngest_Obj_Stamp),
                               ") newer than",
                               Executable,
                               "(", String (Executable_Stamp), ")");
               end if;
            end if;
         end if;
      end Recursive_Compilation_Step;

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
         for I in Binder_Switches.First .. Binder_Switches.Last loop
            Args (I) := Binder_Switches.Table (I);
         end loop;

         Bind (Main_ALI_File, Args);
      end Bind_Step;

      Link_Step : declare
         Args : Argument_List (Linker_Switches.First .. Linker_Switches.Last);

      begin
         for I in Linker_Switches.First .. Linker_Switches.Last loop
            Args (I) := Linker_Switches.Table (I);
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

      when others =>
         Osint.Fail ("INTERNAL ERROR. Please report.");

   end Gnatmake;

   ------------
   -- Init_Q --
   ------------

   procedure Init_Q is
   begin
      if not First_Q_Initialization then
         First_Q_Initialization := False;

         --  Unmark source files which were previously marked & enqueued.

         for J in Q.First .. Q.Last - 1 loop
            Unmark (Source_File => Q.Table (J));
         end loop;
      end if;

      Q_Front := Q.First;
      Q.Set_Last (Q.First);
   end Init_Q;

   --------------
   -- Insert_Q --
   --------------

   procedure Insert_Q (Source_File : File_Name_Type) is
   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q + [ ");
         Write_Name (Source_File);
         Write_Str (" ] ");
         Write_Eol;
      end if;

      Q.Table (Q.Last) := Source_File;
      Q.Increment_Last;
   end Insert_Q;

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

      if Name_Buffer (Name_Len) /= Directory_Separator
           or else
         Name_Buffer (Name_Len) /= '/'
      then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Directory_Separator;
      end if;

      --  Add flags to the alredy existing flags.

      N := Name_Find;
      B := Get_Name_Table_Byte (N);
      Set_Name_Table_Byte (N, B or Mark);
   end Mark_Directory;

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

   ------------
   -- Unmark --
   ------------

   procedure Unmark (Source_File : File_Name_Type) is
   begin
      Set_Name_Table_Byte (Source_File, 0);
   end Unmark;

   -------------------
   -- Scan_Make_Arg --
   -------------------

   procedure Scan_Make_Arg (Argv : String) is
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
            Add_Switch ("-o", Linker);
            Add_Switch (Argv, Linker);
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
               pragma Assert (False);
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
         Add_Switch (Argv, Program_Args);

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
               Gcc := new String'(Program_Args.all (1).all);

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch (Program_Args.all (J).all, Compiler);
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
               Gnatbind := new String'(Program_Args.all (1).all);

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch (Program_Args.all (J).all, Binder);
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
               Gnatlink := new String'(Program_Args.all (1).all);

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
            Add_Switch (Argv, Compiler);
            Add_Switch ("-aO", Argv (3 .. Argv'Last), Binder);

            --  No need to pass any source dir to the binder
            --  since gnatmake call it with the -x flag
            --  (ie do not check source time stamp)

         --  -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Src_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch ("-I", Argv (4 .. Argv'Last), Compiler);

         --  -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch (Argv, Binder);

         --  -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Mark_Directory (Argv (4 .. Argv'Last), Ada_Lib_Dir);
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch ("-aO", Argv (4 .. Argv'Last), Binder);

         --  -Adir (to gnatbind this is like a -aO switch, to gcc like a -I)

         elsif Argv (2) = 'A' then
            Mark_Directory (Argv (3 .. Argv'Last), Ada_Lib_Dir);
            Add_Src_Search_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Search_Dir (Argv (3 .. Argv'Last));
            Add_Switch ("-I", Argv (3 .. Argv'Last), Compiler);
            Add_Switch ("-aO", Argv (3 .. Argv'Last), Binder);

         --  -Ldir

         elsif Argv (2) = 'L' then
            Add_Switch (Argv, Linker);

         --  -g

         elsif Argv (2) = 'g'
           and then (Argv'Last = 2
                     or else Argv (3) in '0' .. '3')
         then
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Linker);

         --  -m

         elsif Argv (2) = 'm'
           and then Argv'Last = 2
         then
            Opt.Minimal_Recompilation := True;

         --  By default all switches with more than one character
         --  or one character switches which are not in 'a' .. 'z'
         --  are passed to the compiler, unless we are dealing
         --  with a -jnum switch or a debug switch (starts with 'd')

         elsif Argv'Length > 5
           and then Argv (2 .. 5) = "gnat"
           and then Argv (6) = 'c'
         then
            Add_Switch (Argv, Compiler);
            Opt.Operating_Mode := Opt.Check_Semantics;
            Opt.Check_Object_Consistency := False;
            Opt.Compile_Only             := True;

         elsif Argv (2) /= 'j'
           and then Argv (2) /= 'd'
           and then Argv (2 .. Argv'Last) /= "M"
           and then (Argv'Length > 2 or else Argv (2) not in 'a' .. 'z')
         then
            Add_Switch (Argv, Compiler);

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

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1  : Name_Id;
      S1  : String;
      N2  : Name_Id := No_Name;
      S2  : String  := "";
      Ind : Boolean := True)
   is
   begin
      if not Opt.Verbose_Mode then
         return;
      end if;

      Write_Str ("  ");

      if Ind then
         Write_Str ("-> ");
      end if;

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

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1  : Name_Id;
      S1a : String;
      S1b : String;
      S1c : String;
      N2  : Name_Id;
      S2a : String;
      S2b : String;
      S2c : String)
   is
   begin
      if not Opt.Verbose_Mode then
         return;
      end if;

      Write_Str ("  -> ");

      Write_Str ("""");
      Write_Name (N1);
      Write_Str (""" ");
      Write_Str (S1a);
      Write_Str (S1b);
      Write_Str (S1c);

      Write_Str (" """);
      Write_Name (N2);
      Write_Str (""" ");
      Write_Str (S2a);
      Write_Str (S2b);
      Write_Str (S2c);

      Write_Eol;
   end Verbose_Msg;

end Make;
