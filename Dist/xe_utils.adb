------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Unchecked_Deallocation;

with GNAT.Os_Lib;    use GNAT.Os_Lib;
with Namet;          use Namet;
with Osint;          use Osint;
with Output;         use Output;
with XE;             use XE;
with XE_Defs;        use XE_Defs;

package body XE_Utils is

   Path         : constant String_Access := GNAT.Os_Lib.Getenv ("PATH");

   GNAT_Verbose   : String_Access;
   XE_Gcc         : String_Access;
   Gcc            : String_Access;
   Mkdir          : String_Access;
   Copy           : String_Access;
   Link           : String_Access;
   Chmod          : String_Access;
   Rm             : String_Access;
   Gnatbind       : String_Access;
   Gnatlink       : String_Access;
   Gnatmake       : String_Access;

   EOL : constant String (1 .. 1) := (others => Ascii.LF);

   Output_Option         : constant String_Access := new String'("-o");
   Preserve              : constant String_Access := new String' ("-p");
   Symbolic              : constant String_Access := new String' ("-s");
   Force                 : constant String_Access := new String' ("-f");
   Compile_Flag          : constant String_Access := new String' ("-c");
   Exclude_File_Flag     : constant String_Access := new String' ("-x");
   Receiver_Build_Flag   : constant String_Access := new String' ("-gnatzr");
   Caller_Build_Flag     : constant String_Access := new String' ("-gnatzc");
   Receiver_Compile_Flag : constant String_Access := new String' ("-gnatzR");
   Caller_Compile_Flag   : constant String_Access := new String' ("-gnatzC");
   GNATLib_Compile_Flag  : constant String_Access := new String' ("-gnatg");

   I_GARLIC_Dir          : String_Access;
   L_GARLIC_Dir          : String_Access;

   Inc_Path_Flag         : Name_Id;
   Lib_Path_Flag         : Name_Id;
   Private_Id            : Name_Id;
   Caller_Id             : Name_Id;
   Receiver_Id           : Name_Id;

   Sem_Only_Flag    : constant String_Access := new String' ("-gnatc");
   --  Workaround : bad object file generated during stub generation

   I_Current_Dir    : String_Access;
   --  := new String' ("-I.");
   I_Caller_Dir     : String_Access;
   --  := new String' ("-I../../private/caller/");
   I_DSA_Caller_Dir : String_Access;
   --  := new String' ("-Idsa/private/caller/");
   I_Original_Dir   : String_Access;
   --  := new String' ("-I../../../");

   L_Current_Dir    : String_Access;
   --  := new String' ("-L.");
   L_Caller_Dir     : String_Access;
   --  := new String' ("-L../../private/caller");
   L_DSA_Caller_Dir : String_Access;
   --  := new String' ("-Ldsa/private/caller");
   L_Original_Dir   : String_Access;
   --  := new String' ("-L../../../");

   No_Args          : constant Argument_List (1 .. 0) := (others => null);

   ---------
   -- "&" --
   ---------

   function "&" (Prefix, Suffix : Name_Id) return Name_Id is
      Length : Natural := Strlen (Prefix) + Strlen (Suffix);
      Name   : String (1 .. Length);
   begin
      Get_Name_String (Prefix);
      Name (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      Length := Name_Len;
      Get_Name_String (Suffix);
      Name (Length + 1 .. Length + Name_Len) := Name_Buffer (1 .. Name_Len);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      return Name_Find;
   end "&";

   procedure Execute (Prog : String_Access; Args : Argument_List);
   --  Execute the command and raise Fatal Error if not successful

   procedure Execute_Bind
     (Lib  : in File_Name_Type;
      Args : in Argument_List);
   --  Execute gnatbind and add gnatdist flags

   procedure Execute_Gcc
     (File : in File_Name_Type;
      Args : in Argument_List);
   --  Execute gcc and add gnatdist compilation flags

   procedure Execute_Link
     (Lib  : in File_Name_Type;
      Exec : in File_Name_Type;
      Args : in Argument_List);
   --  Execute gnatlink and add gnatdist flags

   procedure Execute_XE_Gcc
     (Source : in String_Access;
      Target : in String_Access;
      Flags  : in Argument_List);
   --  Execute xe-gcc and add gnatdist compilation flags

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (To : in File_Name_Type) is

      C_Path : String (1 .. Strlen (To) + 1);

      function Chdir (Path : System.Address) return Int;
      pragma Import (C, Chdir, "chdir");

   begin

      if Debug_Mode then
         Write_Program_Name;
         Write_Str  (": change to dir ");
         Write_Name (To);
         Write_Eol;
      end if;

      Get_Name_String (To);
      C_Path (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      C_Path (Name_Len + 1) := Ascii.Nul;
      if Chdir (C_Path'Address) /= 0 then
         Write_Program_Name;
         Write_Str (": Cannot change dir to ");
         Write_Name (To);
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Building_Script then
         if Name_Len < 3 or else Name_Buffer (1 .. 3) /= "../" then
            Write_Str  (Standout, "if test ! -d ");
            Write_Name (Standout, To);
            Write_Str  (Standout, "; then mkdir -p ");
            Write_Name (Standout, To);
            Write_Str  (Standout, "; fi");
            Write_Eol  (Standout);
         end if;
         Write_Str  (Standout, "cd ");
         Write_Name (Standout, To);
         Write_Eol  (Standout);
      end if;

   end Change_Dir;

   ------------------------
   -- Compile_RCI_Caller --
   ------------------------

   procedure Compile_RCI_Caller (Source : in File_Name_Type) is
   begin
      Execute_Gcc
        (Source,
         (Caller_Compile_Flag,
          I_Original_Dir,
          I_GARLIC_Dir)
         );
   end Compile_RCI_Caller;

   --------------------------
   -- Compile_RCI_Receiver --
   --------------------------

   procedure Compile_RCI_Receiver (Source : in File_Name_Type) is
   begin
      Execute_Gcc
        (Source,
         (Receiver_Compile_Flag,
          I_Original_Dir,
          I_GARLIC_Dir)
         );
   end Compile_RCI_Receiver;

   --------------------------
   -- Copy_With_File_Stamp --
   --------------------------

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False) is
      S : String_Access := new String (1 .. Strlen (Source));
      T : String_Access := new String (1 .. Strlen (Target));
      procedure Free is new Unchecked_Deallocation (String, String_Access);
   begin
      Get_Name_String (Source);
      S.all := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Target);
      T.all := Name_Buffer (1 .. Name_Len);
      if Link = null then
         Execute (Copy, (Preserve, S, T));
      else
         Execute (Rm, (Force, T));
         if Maybe_Symbolic then
            Execute (Link, (Symbolic, S, T));
         else
            Execute (Link, (S, T));
         end if;
      end if;
      Free (S);
      Free (T);
   end Copy_With_File_Stamp;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Descriptor;
      Name : in File_Name_Type;
      Exec : in Boolean := False) is
      File_Name_Len : Natural := Strlen (Name);
      File_Name     : String (1 .. File_Name_Len + 1);
   begin
      Get_Name_String (Name);
      File_Name (1 .. File_Name_Len) := Name_Buffer (1 .. Name_Len);
      File_Name (File_Name_Len + 1) := Ascii.Nul;

      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": creating file ");
         Write_Name (Name);
         Write_Eol;
      end if;

      File := Create_File (File_Name'Address, Text);

      if File = Invalid_FD then
         Write_Program_Name;
         Write_Str  (": cannot create file ");
         Write_Name (Name);
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Exec then
         Execute
           (Chmod,
            (1 => new String'("u+x"),
             2 => new String'(File_Name (1 .. File_Name_Len))));
      end if;

   end Create;

   ----------------
   -- Create_Dir --
   ----------------

   procedure Create_Dir (To : in File_Name_Type) is
      Dir_Name_Len : Natural := Strlen (To);
      Dir_Name     : String (1 .. Dir_Name_Len);
   begin

      Get_Name_String (To);
      Dir_Name := Name_Buffer (1 .. Name_Len);
      for Index in Dir_Name'Range loop

         --  XXXXX
         if Dir_Name (Index) = Separator and then Index > 1 and then
            not Is_Directory (Dir_Name (1 .. Index - 1)) then
            Execute (Mkdir, (1 => new String'(Dir_Name (1 .. Index - 1))));
         elsif Index = Dir_Name'Last then
            Execute (Mkdir, (1 => new String'(Dir_Name)));
         end if;
      end loop;

   end Create_Dir;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in File_Name_Type) is
      Error : Boolean;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": deleting ");
         Write_Name (File);
         Write_Eol;
      end if;
      Get_Name_String (File);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Ascii.Nul;
      Delete_File (Name_Buffer'Address, Error);
   end Delete;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Prog : in String_Access;
      Args : in Argument_List) is
      Success : Boolean := False;
   begin

      if Verbose_Mode or else Building_Script then
         if Building_Script then
            Write_Str (Standout, Prog.all);
         else
            Write_Str (Prog.all);
         end if;
         for Index in Args'Range loop
            if Args (Index) /= null then
               if Building_Script then
                  Write_Str (Standout, " ");
                  Write_Str (Standout, Args (Index).all);
               else
                  Write_Str (" ");
                  Write_Str (Args (Index).all);
               end if;
            end if;
         end loop;
         if Building_Script then
            Write_Eol (Standout);
         else
            Write_Eol;
         end if;
      end if;

      Spawn (Prog.all, Args, Success);

      if not Success then
         Write_Program_Name;
         Write_Str (": ");
         Write_Str (Prog.all);
         Write_Str (" failed");
         Write_Eol;
         raise Fatal_Error;
      end if;

   end Execute;

   ------------------
   -- Execute_Bind --
   ------------------

   procedure Execute_Bind
     (Lib  : in File_Name_Type;
      Args : in Argument_List) is


      Length : constant Positive :=
        Args'Length + Binder_Switches.Last - Binder_Switches.First + 3;

      Bind_Flags   : Argument_List (1 .. Length);

      Lib_Name     : String (1 .. Strlen (Lib));

      N_Bind_Flags : Natural range 0 .. Length := 0;

   begin

      N_Bind_Flags := N_Bind_Flags + 1;
      Bind_Flags (N_Bind_Flags) := Exclude_File_Flag;

      --  various arguments

      for I in Args'Range loop
         N_Bind_Flags := N_Bind_Flags + 1;
         Bind_Flags (N_Bind_Flags) := Args (I);
      end loop;

      for I in Binder_Switches.First .. Binder_Switches.Last loop
         N_Bind_Flags := N_Bind_Flags + 1;
         Bind_Flags (N_Bind_Flags) := Binder_Switches.Table (I);
      end loop;

      --  <unit name>

      Get_Name_String (Lib);
      Lib_Name := Name_Buffer (1 .. Name_Len);
      N_Bind_Flags := N_Bind_Flags + 1;
      Bind_Flags (N_Bind_Flags) := new String'(Lib_Name);

      --  Call gnatbind

      Execute (Gnatbind, Bind_Flags (1 .. N_Bind_Flags));

   end Execute_Bind;

   -----------------
   -- Execute_Gcc --
   -----------------

   procedure Execute_Gcc
     (File : in File_Name_Type;
      Args : in Argument_List) is

      Length      : constant Natural
        := Gcc_Switches.Last - Gcc_Switches.First + 1 + Args'Length + 2;

      Gcc_Flags   : Argument_List (1 .. Length);

      File_Name   : String (1 .. Strlen (File));

      N_Gcc_Flags : Natural range 0 .. Length := 0;

   begin

      N_Gcc_Flags := N_Gcc_Flags + 1;
      Gcc_Flags (N_Gcc_Flags) := Compile_Flag;

      Get_Name_String (File);
      File_Name := Name_Buffer (1 .. Name_Len);

      N_Gcc_Flags := N_Gcc_Flags + 1;
      Gcc_Flags (N_Gcc_Flags) := new String'(File_Name);

      for I in Args'Range loop
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Args (I);
      end loop;

      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Gcc_Switches.Table (I);
      end loop;

      Execute (Gcc, Gcc_Flags);

   end Execute_Gcc;

   ------------------
   -- Execute_Link --
   ------------------

   procedure Execute_Link
     (Lib  : in File_Name_Type;
      Exec : in File_Name_Type;
      Args : in Argument_List) is


      Length : constant Positive :=
        2 +            --  -o <executable name>
        1 +            --  <unit_name>
        Args'Length +
        Linker_Switches.Last - Linker_Switches.First + 1;

      Link_Flags   : Argument_List (1 .. Length);

      Lib_Name     : String (1 .. Strlen (Lib));
      Exec_Name    : String (1 .. Strlen (Exec));

      N_Link_Flags : Natural range 0 .. Length := 0;

   begin

      --  -o <executable name>

      Get_Name_String (Exec);
      Exec_Name := Name_Buffer (1 .. Name_Len);
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := Output_Option;
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := new String'(Exec_Name);

      --  various arguments

      for I in Args'Range loop
         N_Link_Flags := N_Link_Flags + 1;
         Link_Flags (N_Link_Flags) := Args (I);
      end loop;

      --  <unit name>

      Get_Name_String (Lib);
      Lib_Name := Name_Buffer (1 .. Name_Len);
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := new String'(Lib_Name);

      for I in Linker_Switches.First .. Linker_Switches.Last loop
         N_Link_Flags := N_Link_Flags + 1;
         Link_Flags (N_Link_Flags) := Linker_Switches.Table (I);
      end loop;

      --  Call gnatmake

      Execute (Gnatlink, Link_Flags (1 .. N_Link_Flags));

   end Execute_Link;

   --------------------
   -- Execute_XE_Gcc --
   --------------------

   procedure Execute_XE_Gcc
     (Source : in String_Access;
      Target : in String_Access;
      Flags  : in Argument_List) is
      N : Natural := 1;
      L : constant Natural
        := Gcc_Switches.Last - Gcc_Switches.First + 5 + Flags'Length;
      A : Argument_List (1 .. L);
   begin
      A (N) := Target;
      N := N + 1;
      A (N) := Gcc;
      N := N + 1;
      A (N) := Compile_Flag;
      N := N + 1;
      for I in Flags'First .. Flags'Last loop
         A (N) := Flags (I);
         N := N + 1;
      end loop;
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         A (N) := Gcc_Switches.Table (I);
         N := N + 1;
      end loop;
      A (N) := Source;
      Execute (XE_Gcc, A);
   end Execute_XE_Gcc;

   -----------------------------------
   -- Expand_And_Compile_RCI_Caller --
   -----------------------------------

   procedure Expand_And_Compile_RCI_Caller
     (Source, Target : in File_Name_Type) is
      Source_Name_Len : Natural := Strlen (Source);
      Target_Name_Len : Natural := Strlen (Target);
      Source_Name     : String (1 .. Source_Name_Len);
      Target_Name     : String (1 .. Target_Name_Len);
   begin
      Get_Name_String (Source);
      Source_Name := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Target);
      Target_Name := Name_Buffer (1 .. Name_Len);
      Execute_XE_Gcc
        (new String'(Target_Name),
         new String'(Source_Name),
         (Sem_Only_Flag,
          Caller_Build_Flag,
          I_GARLIC_Dir));
   end Expand_And_Compile_RCI_Caller;

   -------------------------------------
   -- Expand_And_Compile_RCI_Receiver --
   -------------------------------------

   procedure Expand_And_Compile_RCI_Receiver
     (Source, Target : in File_Name_Type) is
      Source_Name_Len : Natural := Strlen (Source);
      Target_Name_Len : Natural := Strlen (Target);
      Source_Name     : String (1 .. Source_Name_Len);
      Target_Name     : String (1 .. Target_Name_Len);
   begin
      Get_Name_String (Source);
      Source_Name := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Target);
      Target_Name := Name_Buffer (1 .. Name_Len);
      Execute_XE_Gcc
        (new String'(Target_Name),
         new String'(Source_Name),
         (Sem_Only_Flag,
          Receiver_Build_Flag,
          I_GARLIC_Dir));
   end Expand_And_Compile_RCI_Receiver;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dir_Sep    : String (1 .. 1) := (others => Directory_Separator);
      Name       : Name_Id;
   begin

      XE_Gcc          := Locate ("xe-gcc");
      Gcc             := Locate ("gcc");
      Mkdir           := Locate ("mkdir");
      Copy            := Locate ("cp");
      Link            := Locate ("ln", False);
      Chmod           := Locate ("chmod");
      Rm              := Locate ("rm");
      Gnatbind        := Locate ("gnatbind");
      Gnatlink        := Locate ("gnatlink");
      Gnatmake        := Locate ("gnatmake");


      if Verbose_Mode then
         GNAT_Verbose := new String' ("-v");
      else
         GNAT_Verbose := new String' ("-q");
      end if;

      Inc_Path_Flag  := Str_To_Id ("-I");
      Lib_Path_Flag  := Str_To_Id ("-L");
      Private_Id     := Str_To_Id ("private");
      Caller_Id      := Str_To_Id ("caller");
      Receiver_Id    := Str_To_Id ("receiver");
      Parent_Dir     := Str_To_Id ("..");

      Obj_Suffix     := Str_To_Id (".o");
      ALI_Suffix     := Str_To_Id (".ali");
      ADS_Suffix     := Str_To_Id (".ads");
      ADB_Suffix     := Str_To_Id (".adb");

      Spec_Suffix    := Str_To_Id ("%s");
      Body_Suffix    := Str_To_Id ("%b");

      Dir_Sep_Id     := Str_To_Id (Dir_Sep);
      Dot_Sep_Id     := Str_To_Id (".");

      DSA_Dir        := Str_To_Id ("dsa");

      Caller_Dir     := DSA_Dir & Dir_Sep_Id & Private_Id &
                       Dir_Sep_Id & Caller_Id;
      Receiver_Dir   := DSA_Dir & Dir_Sep_Id & Private_Id &
                       Dir_Sep_Id & Receiver_Id;

      Original_Dir   := Parent_Dir & Dir_Sep_Id &
                        Parent_Dir & Dir_Sep_Id &
                        Parent_Dir;

      PWD_Id         := Str_To_Id ("`pwd`") & Dir_Sep_Id;

      Build_Stamp_File    := Str_To_Id ("glade.sta");
      Elaboration_File    := Str_To_Id ("s-garela");
      Elaboration_Name    := Str_To_Id ("System.Garlic.Elaboration");
      Partition_Main_File := Str_To_Id ("partition");
      Partition_Main_Name := Str_To_Id ("Partition");

      declare
         Dir  : constant String_Access := Get_GARLIC_Dir;
         Len  : Natural;
      begin
         Len := Dir'Length;
         Get_Name_String (Inc_Path_Flag);
         Name_Buffer (Name_Len + 1 .. Name_Len + Len) := Dir.all;
         I_GARLIC_Dir := new String'(Name_Buffer (1 .. Name_Len + Len));
         Get_Name_String (Lib_Path_Flag);
         Name_Buffer (Name_Len + 1 .. Name_Len + Len) := Dir.all;
         L_GARLIC_Dir := new String'(Name_Buffer (1 .. Name_Len + Len));
      end;

      Name := Inc_Path_Flag & Dot_Sep_Id;
      Get_Name_String (Name);
      I_Current_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name := Inc_Path_Flag & Parent_Dir & Dir_Sep_Id & Parent_Dir &
              Dir_Sep_Id & Private_Id & Dir_Sep_Id & Caller_Id & Dir_Sep_Id;
      Get_Name_String (Name);
      I_Caller_Dir  := new String'(Name_Buffer (1 .. Name_Len));

      Name := Inc_Path_Flag & DSA_Dir &
        Dir_Sep_Id & Private_Id & Dir_Sep_Id & Caller_Id & Dir_Sep_Id;
      Get_Name_String (Name);
      I_DSA_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name := Inc_Path_Flag & Parent_Dir & Dir_Sep_Id & Parent_Dir &
        Dir_Sep_Id & Parent_Dir & Dir_Sep_Id;
      I_Original_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name := Lib_Path_Flag & Dot_Sep_Id;
      L_Current_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name := Lib_Path_Flag & Parent_Dir & Dir_Sep_Id & Parent_Dir &
        Dir_Sep_Id & Private_Id & Dir_Sep_Id & Caller_Id & Dir_Sep_Id;
      Get_Name_String (Name);
      L_Caller_Dir  := new String'(Name_Buffer (1 .. Name_Len));

      Name := Lib_Path_Flag & DSA_Dir &
        Dir_Sep_Id & Private_Id & Dir_Sep_Id & Caller_Id & Dir_Sep_Id;
      Get_Name_String (Name);
      L_DSA_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name := Lib_Path_Flag & Parent_Dir & Dir_Sep_Id & Parent_Dir &
        Dir_Sep_Id & Parent_Dir & Dir_Sep_Id;
      L_Original_Dir := new String'(Name_Buffer (1 .. Name_Len));


   end Initialize;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return Is_Directory (Name_Buffer (1 .. Name_Len));
   end Is_Directory;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return GNAT.Os_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len));
   end Is_Regular_File;

   ---------------------
   -- Is_Relative_Dir --
   ---------------------

   function Is_Relative_Dir (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return Name_Len = 0 or else
        (Name_Buffer (1) /= Separator and then Name_Buffer (1) /= '/');
   end Is_Relative_Dir;

   ------------
   -- Locate --
   ------------

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access is
      Prog : String_Access;
   begin
      Prog := Locate_Regular_File (Exec_Name, Path.all);
      if Prog = null and then Show_Error then
         Write_Program_Name;
         Write_Str (": ");
         Write_Str (Exec_Name);
         Write_Str (" is not in your path");
         Write_Eol;
         raise Fatal_Error;
      end if;
      return Prog;
   end Locate;

   ---------
   -- ">" --
   ---------

   function ">" (File1, File2 : Name_Id) return Boolean is
   begin
      if Source_File_Stamp (File1) > Source_File_Stamp (File2) then
         if Debug_Mode then
            Write_Stamp_Comparison (File1, File2);
         end if;
         return True;
      else
         return False;
      end if;
   end ">";

   ----------------------------------
   -- Produce_Partition_Executable --
   ----------------------------------

   procedure Produce_Partition_Executable
     (Partition     : in Name_Id;
      Executable    : in File_Name_Type) is

      FD : File_Descriptor;

   begin

      Execute_Gcc
        (Elaboration_File & ADB_Suffix,
         (GNATLib_Compile_Flag,
          I_Original_Dir,
          I_GARLIC_Dir)
         );

      Execute_Gcc
        (Partition_Main_File & ADB_Suffix,
         (I_Current_Dir,
          I_Caller_Dir,
          I_Original_Dir)
         );

      --  I_Garlic_Dir is not included here because it was added by the
      --  gnatdist shell script.


      Execute_Bind
        (Partition_Main_File & ALI_Suffix,
         (I_Current_Dir,
          I_Caller_Dir,
          I_Original_Dir)
            );

      Execute_Link
        (Partition_Main_File & ALI_Suffix,
         Executable,
         (L_Current_Dir,
          L_Caller_Dir,
          L_Original_Dir,
          L_GARLIC_Dir)
         );

      Create (FD, Build_Stamp_File);
      Close  (FD);

   end Produce_Partition_Executable;

   ---------------
   -- Str_To_Id --
   ---------------

   function Str_To_Id (S : String) return Name_Id is
   begin
      Name_Buffer (1 .. S'Length) := S;
      Name_Len := S'Length;
      return Name_Find;
   end Str_To_Id;

   ------------
   -- Strlen --
   ------------

   function Strlen (Name : in Name_Id) return Natural is
   begin
      Get_Name_String (Name);
      return Name_Len;
   end Strlen;

   -----------------
   -- Unlink_File --
   -----------------

   procedure Unlink_File (File : in File_Name_Type) is
      File_Name : String_Access := new String (1 .. Strlen (File));
      procedure Free is new Unchecked_Deallocation (String, String_Access);
   begin
      Get_Name_String (File);
      File_Name.all := Name_Buffer (1 .. Name_Len);
      Execute (Rm, (Force, File_Name));
      Free (File_Name);
   end Unlink_File;

   ---------------------------
   -- Write_Compile_Command --
   ---------------------------

   procedure Write_Compile_Command (Name : in File_Name_Type) is
   begin
      Write_Str  (Standout, Gnatmake.all);
      Write_Str  (Standout, " -c ");
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         Write_Str (Standout, Gcc_Switches.Table (I).all);
         Write_Str (Standout, " ");
      end loop;
      Write_Name (Standout, Name);
      Write_Eol  (Standout);
   end Write_Compile_Command;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol
     (File   : in File_Descriptor;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if EOL'Length /= Write (File, EOL'Address, EOL'Length) then
         Write_Str ("error : disk full");
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Stdout then
         Write_Eol (Standout);
      end if;

   end Write_Eol;

   ----------------------
   -- Write_File_Stamp --
   ----------------------

   procedure Write_File_Stamp
     (File : in File_Name_Type) is
   begin
      Write_Str (" (");
      Write_Str (String (Source_File_Stamp (File)));
      Write_Str (")");
   end Write_File_Stamp;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Message : in String) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Str (Message);
      Write_Eol;
   end Write_Message;

   ------------------------
   -- Write_Missing_File --
   ------------------------

   procedure Write_Missing_File
     (File  : in File_Name_Type) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (File);
      Write_Str (" does not exist");
      Write_Eol;
   end Write_Missing_File;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name
     (File   : in File_Descriptor;
      Name   : in Name_Id;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if Name /= No_Name then
         Get_Name_String (Name);
         if Name_Buffer (Name_Len - 1) = '%' then --  %
            Name_Len := Name_Len - 2;
         end if;
         if Write (File, Name_Buffer'Address, Name_Len) /= Name_Len then
            Write_Str ("error : disk full");
            Write_Eol;
            raise Fatal_Error;
         end if;

         if Stdout then
            Write_Name (Standout, Name);
         end if;

      end if;

   end Write_Name;

   ----------------------------
   -- Write_Stamp_Comparison --
   ----------------------------

   procedure Write_Stamp_Comparison
     (Newer, Older   : in File_Name_Type) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (Newer);
      Write_File_Stamp (Newer);
      Write_Str (" > ");
      Write_Name (Older);
      Write_File_Stamp (Older);
      Write_Eol;
   end Write_Stamp_Comparison;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (File   : in File_Descriptor;
      Line   : in String;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if Write (File, Line'Address, Line'Length) /= Line'Length then
         Write_Str ("error : disk full");
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Stdout then
         Write_Str (Standout, Line);
      end if;

   end Write_Str;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (U : in Unit_Name_Type) is
   begin
      Get_Decoded_Name_String (U);
      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '-' then
            Name_Buffer (J) := '.';
         end if;
      end loop;
      Name_Len := Name_Len - 2;
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Write_Unit_Name;

end XE_Utils;
