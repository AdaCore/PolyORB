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
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

with ALI;           use ALI;
with Csets;
with Debug;
with Fname;         use Fname;
with Namet;         use Namet;
with Opt;
with Osint;         use Osint;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Gnatvsn;
with Output;        use Output;
with Table;
with Types;         use Types;

package body Make is

   use Ascii;
   --  Make control characters visible

   -------------------------------------
   -- Queue (Q) Manipulation Routines --
   -------------------------------------

   --  The Q is used in Compile_Sources below. Its implementation uses the
   --  GNAT generic package Table (basically an extensible array).  Q_Front
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
   --  were explored and thus marked by Compile_Sources.  Whenever the Q is
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

   package Q is new Table (
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

   function Append (Name : Name_Id; Suffix : String) return Name_Id;
   --  Appends Suffix to Name and returns the new name.

   procedure List_Depend;
   --  Prints to standard output the list of object dependencies. This list
   --  can be used directly in a Makefile. For this routine to work
   --  Compile_Sources must be called before hand. Also because this routine
   --  uses the ALI files that were originally loaded and scanned by
   --  Compile_Sources, no additional ALI files should be scanned between the
   --  call to Compile_Sources and List_Depend.

   procedure Inform (N : Name_Id := No_Name; Msg : String);
   --  Prints out the program name followed by a colon, N and S.

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

   ----------------------------------------------------
   -- Compiler, Binder & Linker Variables & Routines --
   ----------------------------------------------------

   Gcc       : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gcc");
   Gnatbind  : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gnatbind");
   Gnatlink  : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gnatlink");

   Exec_Name_Flag  : constant String_Access := new String'("-o");
   Comp_Flag       : constant String_Access := new String'("-c");
   Ada_Flag_1      : constant String_Access := new String'("-x");
   Ada_Flag_2      : constant String_Access := new String'("ada");
   GNAT_Flag       : constant String_Access := new String'("-gnatg");
   Dont_Check_Flag : constant String_Access := new String'("-x");

   Display_Executed_Programs : Boolean := True;
   --  Set to True if name of commands should be output on stderr.

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args if
   --  variable Display_Executed_Programs is set.

   procedure Check
     (Lib_File  : File_Name_Type;
      Ali       : out ALI_Id;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type);
   --  Determines whether the library file Lib_File is up-to-date or not.
   --  The full name (with path information) of the object file
   --  corresponding to Lib_File is returned in O_File. Its time stamp is
   --  saved in O_Stamp. Ali is the ALI_Id corresponding to Lib_File.  If
   --  Lib_File in not up-to-date, then the coresponding source file needs
   --  to be recompiled. In this case Ali = No_ALI_Id.

   ------------
   -- Append --
   ------------

   function Append (Name : Name_Id; Suffix : String) return Name_Id is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;
      return Name_Find;
   end Append;

   ----------
   -- Bind --
   ----------

   procedure Bind (Ali_File : File_Name_Type; Args : Argument_List) is
      Bind_Args : Argument_List (Args'First .. Args'Last + 2);
      Bind_Last : Integer;
      Success   : Boolean;

   begin
      --  Optimize the simple case where the gnatbind command line looks like
      --     gnatbind -aO. -I- file.ali   --into->   gnatbind file.adb

      if Args'Length = 2
        and then Args (Args'First).all = "-aO."
        and then Args (Args'Last).all = "-I-"
        and then Ali_File = Strip_Directory (Ali_File)
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

      Get_Name_String (Ali_File);

      Bind_Last := Bind_Last + 1;
      Bind_Args (Bind_Last) := new String'(Name_Buffer (1 .. Name_Len));

      Display ("gnatbind", Bind_Args (Args'First .. Bind_Last));

      GNAT.OS_Lib.Spawn
        (Gnatbind.all, Bind_Args (Args'First .. Bind_Last), Success);

      if not Success then
         raise Bind_Failed;
      end if;
   end Bind;

   -----------
   -- Check --
   -----------

   procedure Check
     (Lib_File  : File_Name_Type;
      Ali       : out ALI_Id;
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
      --  ??? This function should really be in ali.adb and use Uname
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
         --  which has a spec not mentioned in ali file A. If True is returned
         --  Spec_File_Name above is set to the name of this spec file.

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
            exit U_Chk when New_Spec (Unit.Table (U).Uname);

            for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
               exit U_Chk when
                 Withs.Table (W).Afile /= No_File
                 and then New_Spec (Withs.Table (W).Uname);
            end loop;
         end loop U_Chk;

         return Spec_File_Name;
      end First_New_Spec;

      --  Local variables of Check

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

   --  Start processing of Check

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

      Ali     := No_ALI_Id;
      O_File  := Full_Obj_File;
      O_Stamp := Obj_Stamp;

      if Text = null then
         if Full_Lib_File = No_File then
            Verbose_Msg (Lib_File, "missing.");
         elsif Obj_Stamp (Obj_Stamp'First) = ' ' then
            Verbose_Msg (Full_Obj_File, "missing.");
         else
            Verbose_Msg (Full_Lib_File, "(", Lib_Stamp, ") newer than",
                         Full_Obj_File, "(", Obj_Stamp, ")");
         end if;

      else
         Ali := Scan_ALI (Lib_File, Text);

         --  get the source files and their time stamps. Note that some
         --  sources may be missing if Ali is out-of-date.

         Set_Source_Table (Ali);

         Modified_Source := Time_Stamp_Mismatch (Ali);
         if Modified_Source /= No_File then
            Ali := No_ALI_Id;

            if Opt.Verbose_Mode then
               Source_Name := Full_Source_Name (Modified_Source);
               if Source_Name /= No_File then
                  Verbose_Msg (Source_Name, "time stamp mismatch");
               else
                  Verbose_Msg (Modified_Source, "missing");
               end if;
            end if;

         else
            New_Spec := First_New_Spec (Ali);
            if New_Spec /= No_File then
               Ali := No_ALI_Id;

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
      Check_Internal_Files  : Boolean  := False;
      Dont_Execute          : Boolean  := False;
      Force_Compilations    : Boolean  := False;
      Keep_Going            : Boolean  := False;
      Initialize_Ali_Data   : Boolean  := True;
      Max_Process           : Positive := 1)
   is
      Empty_Time_Stamp : constant String := "            ";
      --  The empty time stamp

      function Compile (S : Name_Id) return Process_Id;
      --  Compiles S using Args above. If S is a GNAT predefined source
      --  "-gnatg" is added to Args.  Non blocking call. Returns The
      --  Process_Id of the process spawned to execute the compile.

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
      --  Awaits that an outstanding compilation process terminates.  When
      --  it does set S to the name of the Source file that was compiled and
      --  L the name of its library file. Note that this time stamp can be
      --  used to check whether the compilation did generate an object
      --  file. OK is set to True if the compilation succeeded. Note that S
      --  and L could be No_File if there were no compilations to wait for.

      package Good_Ali is new Table (
        Table_Component_Type => ALI_Id,
        Table_Index_Type     => Natural,
        Table_Low_Bound      => 1,
        Table_Initial        => 50,
        Table_Increment      => 100,
        Table_Name           => "Make.Good_Ali");
      --  Contains the set of valid Ali files that have not yet been scanned.

      procedure Record_Good_Ali (A : ALI_Id);
      --  Records in the previous set the Id of an Ali file.

      function Good_Ali_Present return Boolean;
      --  Returns True if any Ali file was recorded in the previous set.

      function Get_Next_Good_Ali return ALI_Id;
      --  Returns the next good ALI_Id record;

      type Bad_Compilation_Info is record
         File  : File_Name_Type;
         Found : Boolean;
      end record;
      --  File is the name of the file for which a compilation failed
      --  Found is False if the compilation failed because the file
      --  could not be found.

      package Bad_Compilation is new Table (
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
         I : constant Positive := Outstanding_Compiles + 1;

      begin
         pragma Assert (I <= Max_Process);
         pragma Assert (Pid /= Invalid_Pid);

         Running_Compile (I).Pid              := Pid;
         Running_Compile (I).Full_Source_File := S;
         Running_Compile (I).Lib_File         := L;

         Outstanding_Compiles := I;
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

      function Compile (S : Name_Id) return Process_Id is

         function Ada_File_Name (Name : Name_Id) return Boolean;
         --  Returns True if Name is the name of an ada source file (i.e. it
         --  has an extension recognized as Ada by default by the gcc driver)

         function Ada_File_Name (Name : Name_Id) return Boolean is
         begin
            Get_Name_String (Name);
            return
              Name_Len > 4
              and then Name_Buffer (Name_Len - 3 .. Name_Len - 1) = ".ad"
              and then (Name_Buffer (Name_Len) = 'b'
                        or else Name_Buffer (Name_Len) = 's'
                        or else Name_Buffer (Name_Len) = 'a');
         end Ada_File_Name;

         --  Variables local to Compile

         Comp_Args : Argument_List (Args'First .. Args'Last + 5);
         Comp_Last : Integer;

      --  Start of processing for Compile

      begin
         Comp_Args (Args'First) := Comp_Flag;

         --  Optimize the simple case where the gcc command line looks like
         --     gcc -c -I. ... -I- file.adb  --into->  gcc -c ... file.adb

         if Args (Args'First).all = "-I."
           and then Args (Args'Last).all = "-I-"
           and then S = Strip_Directory (S)
         then
            Comp_Last := Args'Last - 1;
            Comp_Args (Args'First + 1 .. Comp_Last)
              := Args (Args'First + 1 .. Args'Last - 1);
         else
            Comp_Last := Args'Last + 1;
            Comp_Args (Args'First + 1 .. Comp_Last) := Args;
         end if;

         --  The directory name needs to be stripped from the source file S
         --  because Fname.Is_Predefined_File_Name cannot deal with directory
         --  prefixes.

         if Is_Predefined_File_Name (Strip_Directory (S)) then
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

         Get_Name_String (S);

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'(Name_Buffer (1 .. Name_Len));

         Display ("gcc", Comp_Args (Args'First .. Comp_Last));

         return
           GNAT.OS_Lib.Non_Blocking_Spawn
             (Gcc.all, Comp_Args (Args'First .. Comp_Last));
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
      -- Get_Next_Good_Ali --
      -----------------------

      function Get_Next_Good_Ali return ALI_Id is
         Ali : ALI_Id;

      begin
         pragma Assert (Good_Ali_Present);
         Ali := Good_Ali.Table (Good_Ali.Last);
         Good_Ali.Decrement_Last;
         return Ali;
      end Get_Next_Good_Ali;

      ----------------------
      -- Good_Ali_Present --
      ----------------------

      function Good_Ali_Present return Boolean is
      begin
         return Good_Ali.First <= Good_Ali.Last;
      end Good_Ali_Present;

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
      -- Record_Good_Ali --
      ---------------------

      procedure Record_Good_Ali (A : ALI_Id) is
      begin
         Good_Ali.Increment_Last;
         Good_Ali.Table (Good_Ali.Last) := A;
      end Record_Good_Ali;

      --  Compile_Sources Variables

      Source_File      : File_Name_Type;
      --  Current source file

      Full_Source_File : File_Name_Type;
      --  Full name of the current source file

      Lib_File         : File_Name_Type;
      --  Current library file

      Ada_Lib_File     : File_Name_Type;
      --  Full name of current library file if it is in some Ada library

      Obj_File         : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File.

      Obj_Stamp        : Time_Stamp_Type;
      --  Time stamp of the current object file.

      Sfile            : File_Name_Type;
      --  Contains, in turn, the source file of the units withed by Source_File

      Ali              : ALI_Id;
      --  Ali Id of the current Ali file

      Compilation_OK  : Boolean;
      Need_To_Compile : Boolean;
      Missing_Alis    : Boolean := False;

      Pid  : Process_Id;
      Text : Text_Buffer_Ptr;

   --  Start of Processing for Compile_Sources

   begin
      --  Package and Queue initializations.

      Good_Ali.Init;
      Bad_Compilation.Init;
      Output.Set_Standard_Error;
      Init_Q;

      if Initialize_Ali_Data then
         Initialize_ALI;
      end if;

      --  The following two flags affect the behavior of Ali.Set_Source_Table.
      --  We set Opt.Check_Source_Files to True to ensure that source file
      --  time stamps are checked, and we set Opt.All_Sources to False to
      --  avoid checking the presence of the source files listed in the
      --  source dependency section of an ali file (which would be a mistake
      --  since the ali file may be obsolete).

      Opt.Check_Source_Files := True;
      Opt.All_Sources        := False;

      Insert_Q (Main_Source);
      Mark (Main_Source);

      First_Compiled_File    := No_File;
      Most_Recent_Obj_File   := No_File;
      Main_Unit              := False;

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
            Ada_Lib_File     := Osint.Ada_Library_Lookup (Lib_File);

            --  If the library file is is an Ada library skip it

            if Ada_Lib_File /= No_File then
               Verbose_Msg (Ada_Lib_File, "in Ada library", Ind => No_Indent);

            --  The source file that we are checking cannot be located

            elsif Full_Source_File = No_File then
               Record_Failure (Source_File, Found => False);

            --  The source file that we are checking can be located

            else
               --  Don't waste any time if we have to recompile anyway

               Obj_Stamp       := Empty_Time_Stamp;
               Need_To_Compile := Force_Compilations;
               if not Force_Compilations then
                  Check (Lib_File, Ali, Obj_File, Obj_Stamp);
                  Need_To_Compile := (Ali = No_ALI_Id);
               end if;

               if not Need_To_Compile then
                  --  The Ali file is up-to-date. Record its Id.

                  Record_Good_Ali (Ali);

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
                  --  Is this the first file we have to compile ?

                  if First_Compiled_File = No_File then
                     First_Compiled_File  := Full_Source_File;
                     Most_Recent_Obj_File := No_File;

                     if Dont_Execute then
                        exit Make_Loop;
                     end if;
                  end if;

                  --  Start the compilation and record it. We can do this
                  --  because there is at least one free process.

                  Pid := Compile (Full_Source_File);

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
         --  (that is the Q is empty and there are no good Alis to process).

         if Outstanding_Compiles = Max_Process
           or else (Empty_Q
                    and then not Good_Ali_Present
                    and then Outstanding_Compiles > 0)
         then
            Await_Compile (Full_Source_File, Lib_File, Compilation_OK);

            if not Compilation_OK then
               Record_Failure (Full_Source_File);

            else
               --  Re-read the updated library file.

               Text := Read_Library_Info (Lib_File);

               --  If no Ali file was generated by this compilation nothing
               --  more to do, otherwise scan the ali file and record it

               if Text /= null then
                  Ali := Scan_ALI (Lib_File, Text);
                  Record_Good_Ali (Ali);
               else
                  Inform (Lib_File, "WARNING file not found after compile");
                  Missing_Alis := True;
               end if;
            end if;
         end if;

         --  PHASE 3: Check if we recorded good Ali files. If yes process
         --  them now in the order in which they have been recorded.  There
         --  are two occasions in which we record good ali files. The first is
         --  in phase 1 when, after scanning an existing Ali file we realise
         --  it is up-to-date, the second instance is after a successful
         --  compilation.

         while Good_Ali_Present loop
            Ali := Get_Next_Good_Ali;

            --  If we are processing the library file corresponding to the
            --  main source file check if this source can be a main unit.

            if ALIs.Table (Ali).Sfile = Main_Source then
               Main_Unit := ALIs.Table (Ali).Main_Program /= None;
            end if;

            --  Now insert in the Q the unmarked source files (i.e. those
            --  which have neever been inserted in the Q and hence never
            --  considered).

            for J in
              ALIs.Table (Ali).First_Unit .. ALIs.Table (Ali).Last_Unit
            loop
               for K in
                 Unit.Table (J).First_With .. Unit.Table (J).Last_With
               loop
                  Sfile := Withs.Table (K).Sfile;

                  if Sfile = No_File then
                     Debug_Msg ("Skipping generic:", Withs.Table (K).Uname);

                  elsif Is_Marked (Sfile) then
                     Debug_Msg ("Skipping marked file:", Sfile);

                  elsif not Check_Internal_Files
                    and then Is_Predefined_File_Name (Sfile)
                  then
                     Debug_Msg ("Skipping language defined file:", Sfile);

                  else
                     Insert_Q (Sfile);
                     Mark (Sfile);
                  end if;
               end loop;
            end loop;
         end loop;

      end loop Make_Loop;

      --  if any compilation failed report it

      if Compilations_Failed then
         List_Bad_Compilations;
         raise Compilation_Failed;
      end if;

      --  Make sure no link step is performed if we had missing ali files

      if Missing_Alis then
         Main_Unit := False;
      end if;
   end Compile_Sources;

   -------------
   -- Display --
   -------------

   procedure Display (Program : String; Args : Argument_List) is
   begin
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

   --------------
   -- Gnatmake --
   --------------

   procedure Gnatmake is

      procedure Makeusg;
      --  Outputs gnatmake usage information.

      function To_Lower (Name : Name_Id) return Name_Id;
      --  If Name does not have upper case characters, Name is returned,
      --  otherwise this routine creates and returns a new lower case
      --  version of Name.

      -------------
      -- Makeusg --
      -------------

      procedure Makeusg is
         procedure Write_Switch_Char;
         --  Write two spaces followed by appropriate switch character

         procedure Write_Switch_Char is
         begin
            Write_Str ("  ");
            Write_Char (Switch_Character);
         end Write_Switch_Char;

      --  Start of processing for Makeusg

      begin
         --  Usage line

         Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Write_Str ("  opts  name  ");
         Write_Str ("{[-cargs opts] [-bargs opts] [-largs opts]}");
         Write_Eol;
         Write_Eol;
         Write_Str ("  name  is a file name from which you can omit the");
         Write_Str (" .adb or .ads suffix");
         Write_Eol;
         Write_Eol;

         --  GNATMAKE switches

         Write_Str ("gnatmake switches:");
         Write_Eol;

         --  Line for -a

         Write_Switch_Char;
         Write_Str ("a       Consider all files, even GNAT internal files");
         Write_Eol;

         --  Line for -c

         Write_Switch_Char;
         Write_Str ("c       Compile only, do not bind and link");
         Write_Eol;

         --  Line for -f

         Write_Switch_Char;
         Write_Str ("f       Force recompilations of non predefined units");
         Write_Eol;

         --  Line for -jnnn

         Write_Switch_Char;
         Write_Str ("jnum    Use nnn processes to compile");
         Write_Eol;

         --  Line for -k

         Write_Switch_Char;
         Write_Str ("k       Keep going after compilation errors");
         Write_Eol;

         --  Line for -M

         Write_Switch_Char;
         Write_Str ("M       List object file dependences for Makefile");
         Write_Eol;

         --  Line for -n

         Write_Switch_Char;
         Write_Str ("n       Check if objects are up to date");
         Write_Eol;

         --  Line for -o

         Write_Switch_Char;
         Write_Str ("o name  Choose an alternate executable name");
         Write_Eol;

         --  Line for -q

         Write_Switch_Char;
         Write_Str ("q       Be quiet/terse");
         Write_Eol;

         --  Line for -v

         Write_Switch_Char;
         Write_Str ("v       Motivate all (re)compilations");
         Write_Eol;
         Write_Eol;

         --  GCC switches (passed to gcc by gnatmake)

         Write_Str ("Gnat/Gcc switches such as -g, -O, -gnato, etc.");
         Write_Str ("are directly passed to gcc");
         Write_Eol;
         Write_Eol;

         --  Source & Library search path switches

         Write_Str ("Source & Library search path switches:");
         Write_Eol;

         --  Line for -aL

         Write_Switch_Char;
         Write_Str ("aLdir  Skip missing library sources if ali in dir");
         Write_Eol;

         --  Line for -A

         Write_Switch_Char;
         Write_Str ("Adir   like -aLdir -aIdir");
         Write_Eol;

         --  Line for -aO switch

         Write_Switch_Char;
         Write_Str ("aOdir  Specify library/object files search path");
         Write_Eol;

         --  Line for -aI switch

         Write_Switch_Char;
         Write_Str ("aIdir  Specify source files search path");
         Write_Eol;

         --  Line for -I switch

         Write_Switch_Char;
         Write_Str ("Idir   Like -aIdir -aOdir");
         Write_Eol;

         --  Line for -I- switch

         Write_Switch_Char;
         Write_Str ("I-     Don't look for sources & library files");
         Write_Str (" in the default directory");
         Write_Eol;

         --  Line for -L

         Write_Switch_Char;
         Write_Str ("Ldir   Look for program libraries also in dir");
         Write_Eol;
         Write_Eol;

         --  General Compiler, Binder, Linker switches

         Write_Str ("To pass an arbitrary switch to the Compiler, Binder or ");
         Write_Str ("Linker:");
         Write_Eol;

         --  Line for -cargs

         Write_Switch_Char;
         Write_Str ("cargs opts   opts are passed to the compiler");
         Write_Eol;

         --  Line for -bargs

         Write_Switch_Char;
         Write_Str ("bargs opts   opts are passed to the binder");
         Write_Eol;

         --  Line for -largs

         Write_Switch_Char;
         Write_Str ("largs opts   opts are passed to the linker");
         Write_Eol;

      end Makeusg;

      --------------
      -- To_Lower --
      --------------

      function To_Lower (Name : Name_Id) return Name_Id is
      begin
         Get_Name_String (Name);

         for I in 1 .. Name_Len loop
            if Csets.Is_Upper_Case_Letter (Name_Buffer (I)) then
               Name_Buffer (I) := Csets.Fold_Lower (Name_Buffer (I));
            end if;
         end loop;

         return Name_Enter;
      end To_Lower;

      ------------------------
      -- Gnatmake Variables --
      ------------------------

      Main_Name : Name_Id;
      --  The name of the input compilation unit or of the source containing it

      Main_Source_File : File_Name_Type;
      --  The actual source file corresponding to Main_Name

      Is_Main_Unit : Boolean;
      --  If True the Main_Source_File can be a main unit.

      Main_Ali_File : File_Name_Type;
      --  The ali file corresponding to Main_Source_File

   --  Start of processing for Gnatmake

   begin
      --  Default initialization of the flags affecting gnatmake

      Opt.Check_Internal_Files     := False;
      Opt.Check_Object_Consistency := True;
      Opt.Compile_Only             := False;
      Opt.Dont_Execute             := False;
      Opt.Force_Compilations       := False;
      Opt.Quiet_Output             := False;
      Opt.Smart_Compilations       := False;
      Opt.Verbose_Mode             := False;

      --  Package initializations. The order of calls is important here.

      Output.Set_Standard_Error;
      Osint.Initialize (Osint.Make); --  Reads gnatmake switches
      Csets.Initialize;
      Namet.Initialize;

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      Osint.Source_File_Data (Cache => True);

      if Opt.Verbose_Mode then
         Write_Eol;
         Write_Str ("GNATMAKE ");
         Write_Str (Gnatvsn.Gnat_Version_String);
         Write_Str (" Copyright 1995 Free Software Foundation, Inc.");
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

      --  Now check if the user input a full file name or he has omitted the
      --  suffix. If he omitted the suffix first check the existence of the
      --  source file for the compilation unit body. If the file for the
      --  body of the compilation unit does not exist try the spec.

      --  Note that Osint.Next_Main_Source will always return the (possibly
      --  abbreviated file) without any directory information.

      Main_Name := Osint.Next_Main_Source;

      --  If the input name to gnatmake has a suffix, then use it as is

      if Strip_Suffix (Main_Name) /= Main_Name then
         Main_Source_File := Main_Name;

         --  We cannot have a -I- flag present in the command line and a
         --  full file name or else gnatmake will not be able to reproduce
         --  the exact source search path in gcc.

         if not Opt.Look_In_Primary_Dir then
            Fail ("error, cannot use -I- if full file name is given.");
         end if;

      --  otherwise try to attach it an .adb or .ads suffix

      else
         Main_Source_File := Append (Main_Name, ".adb");

         if Full_Source_Name (Main_Source_File) = No_File then
            Main_Source_File := Append (Main_Name, ".ads");

            if Full_Source_Name (Main_Source_File) = No_File then
               Inform (Main_Name, "no corresponding source file found");
               Exit_Program (E_Fatal);
            end if;
         end if;
      end if;

      --  Consider GNAT predefined files only if -a switch is set.

      if Fname.Is_Predefined_File_Name (Main_Source_File)
        and then not Opt.Check_Internal_Files
      then
         Fail ("use the -a switch to compile GNAT predefined files");
      end if;

      Display_Commands (not Opt.Quiet_Output);

      --  Here is where the make process is started

      Recursive_Compilation_Step : declare
         Args : Argument_List (Gcc_Switches.First .. Gcc_Switches.Last);

         First_Compiled_File : Name_Id;

         Youngest_Obj_File   : Name_Id;
         Youngest_Obj_Stamp  : Time_Stamp_Type;

         Executable          : File_Name_Type := No_File;
         Executable_Stamp    : Time_Stamp_Type;
         Executable_Obsolete : Boolean := True;
         --  Executable is the final executable program.

      begin
         for J in Gcc_Switches.First .. Gcc_Switches.Last loop
            Args (J) := Gcc_Switches.Table (J);
         end loop;

         --  Look inside the linker switches to see if the name of the final
         --  executable program was specified.

         for I in Linker_Switches.First .. Linker_Switches.Last loop
            if Linker_Switches.Table (I).all = Exec_Name_Flag.all then
               pragma Assert (I < Linker_Switches.Last);

               Name_Len := Linker_Switches.Table (I + 1)'Length;
               Name_Buffer (1 .. Name_Len) :=
                 Linker_Switches.Table (I + 1).all;

               Executable := Name_Enter;
            end if;
         end loop;

         --  If the name of the final executable program was not specified
         --  then construct it from the main input file.

         if Executable = No_File then
            Executable := Executable_Name (Strip_Suffix (Main_Source_File));
         end if;

         Compile_Sources (Main_Source           => Main_Source_File,
                          Args                  => Args,
                          First_Compiled_File   => First_Compiled_File,
                          Most_Recent_Obj_File  => Youngest_Obj_File,
                          Most_Recent_Obj_Stamp => Youngest_Obj_Stamp,
                          Main_Unit             => Is_Main_Unit,
                          Check_Internal_Files  => Opt.Check_Internal_Files,
                          Dont_Execute          => Opt.Dont_Execute,
                          Force_Compilations    => Opt.Force_Compilations,
                          Keep_Going            => Opt.Keep_Going,
                          Initialize_Ali_Data   => True,
                          Max_Process           => Opt.Maximum_Processes);

         if Opt.List_Dependencies then
            if First_Compiled_File /= No_File then
               Inform (First_Compiled_File,
                       "must be recompiled. Can't generate dependence list.");
            else
               List_Depend;
            end if;

         elsif First_Compiled_File = No_File
           and then (Opt.Compile_Only or else not Is_Main_Unit)
           and then not Opt.Quiet_Output
         then
            Inform (Msg => "objects up to date.");

         elsif Opt.Dont_Execute and then First_Compiled_File /= No_File then
            Write_Name (First_Compiled_File);
            Write_Eol;
         end if;

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
                               "(", Youngest_Obj_Stamp, ") newer than",
                               Executable,
                               "(", Executable_Stamp, ")");
               end if;
            end if;
         end if;
      end Recursive_Compilation_Step;

      Main_Ali_File := Full_Lib_File_Name (Lib_File_Name (Main_Source_File));

      Bind_Step : declare
         Args : Argument_List (Binder_Switches.First .. Binder_Switches.Last);

      begin
         for I in Binder_Switches.First .. Binder_Switches.Last loop
            Args (I) := Binder_Switches.Table (I);
         end loop;

         Bind (Main_Ali_File, Args);
      end Bind_Step;

      Link_Step : declare
         Args : Argument_List (Linker_Switches.First .. Linker_Switches.Last);

      begin
         for I in Linker_Switches.First .. Linker_Switches.Last loop
            Args (I) := Linker_Switches.Table (I);
         end loop;

         Link (Main_Ali_File, Args);
      end Link_Step;

      Exit_Program (E_Success);

   exception
      when Bind_Failed        => Osint.Fail ("*** bind failed.");
      when Compilation_Failed => Exit_Program (E_Fatal);
      when Link_Failed        => Osint.Fail ("*** link failed.");
      when others             => Osint.Fail ("INTERNAL ERROR. Please report.");

   end Gnatmake;

   ------------
   -- Init_Q --
   ------------

   procedure Init_Q is
   begin
      if not First_Q_Initialization then
         First_Q_Initialization := False;

         --  unmark source files which were previously marked & enqueued.

         for I in Q.First .. Q.Last - 1 loop
            Unmark (Source_File => Q.Table (I));
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

   procedure Link (Ali_File : File_Name_Type; Args : Argument_List) is
      Link_Args : Argument_List (Args'First .. Args'Last + 1);
      Success   : Boolean;

   begin
      Link_Args (Args'Range) :=  Args;

      Get_Name_String (Ali_File);
      Link_Args (Args'Last + 1) := new String'(Name_Buffer (1 .. Name_Len));

      Display ("gnatlink", Link_Args);

      GNAT.OS_Lib.Spawn (Gnatlink.all, Link_Args, Success);

      if not Success then
         raise Link_Failed;
      end if;
   end Link;

   -----------------
   -- List_Depend --
   -----------------

   procedure List_Depend is
      Obj_Name  : Name_Id;
      Src_Name  : Name_Id;

      Len       : Natural;
      Line_Pos  : Natural;
      Line_Size : constant := 77;

   begin
      Set_Standard_Output;

      for A in ALIs.First .. ALIs.Last loop
         Obj_Name := Object_File_Name (ALIs.Table (A).Afile);
         Write_Name (Obj_Name);
         Write_Str (" :");

         Get_Name_String (Obj_Name);
         Len := Name_Len;
         Line_Pos := Len + 2;

         for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
            Src_Name := Sdep.Table (D).Sfile;

            if not Fname.Is_Predefined_File_Name (Src_Name)
              or else Opt.Check_Internal_Files
            then
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
