------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                            X E _ U T I L S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.24                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with ALI;
with Fname;
with System;
with Unchecked_Deallocation;
with Namet;          use Namet;
with Casing;         use Casing;
with Opt;
with Osint;          use Osint;
with Output;         use Output;
with Sinput;         use Sinput;
with Types;          use Types;
with GNAT.Os_Lib;    use GNAT.Os_Lib;
with XE_Defs;        use XE_Defs;
with XE;             use XE;

package body XE_Utils is

   Path         : constant String_Access := GNAT.Os_Lib.Getenv ("PATH");

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access;

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

   GNAT_Verbose   : String_Access;
   Output_Option  : String_Access := new String'("-o");
   XE_Gcc         : String_Access := Locate ("xe-gcc");
   Gcc            : String_Access := Locate ("gcc");
   Gnatmake       : String_Access := Locate ("gnatmake");
   Gnatbind       : String_Access := Locate ("gnatbind");
   Gnatlink       : String_Access := Locate ("gnatlink");
   Mkdir          : String_Access := Locate ("mkdir");
   Copy           : String_Access := Locate ("cp");
   Link           : String_Access := Locate ("ln", False);
   Chmod          : String_Access := Locate ("chmod");
   Rm             : String_Access := Locate ("rm");

   EOL : constant String (1 .. 1) := (others => Ascii.LF);

   Preserve              : constant String_Access := new String' ("-p");
   Symbolic              : constant String_Access := new String' ("-s");
   Force                 : constant String_Access := new String' ("-f");
   Compile_Flag          : constant String_Access := new String' ("-c");
   Receiver_Build_Flag   : constant String_Access := new String' ("-gnatzr");
   Caller_Build_Flag     : constant String_Access := new String' ("-gnatzc");
   Receiver_Compile_Flag : constant String_Access := new String' ("-gnatzR");
   Caller_Compile_Flag   : constant String_Access := new String' ("-gnatzC");

   I_GARLIC_Dir          : String_Access;
   L_GARLIC_Dir          : String_Access;

   Cargs_Flag       : constant String_Access := new String' ("-cargs");
   Bargs_Flag       : constant String_Access := new String' ("-bargs");
   Largs_Flag       : constant String_Access := new String' ("-largs");

   Sem_Only_Flag    : constant String_Access := new String' ("-gnatc");
   --  Workaround : bad object file generated during stub generation

   I_Current_Dir    : constant String_Access := new String' ("-I.");
   I_Caller_Dir     : constant String_Access := new String' ("-I../caller");
   I_DSA_Caller_Dir : constant String_Access := new String' ("-Idsa/caller");
   I_G_Parent_Dir   : constant String_Access := new String' ("-I../..");

   L_Current_Dir    : constant String_Access := new String' ("-L.");
   L_DSA_Caller_Dir : constant String_Access := new String' ("-Ldsa/caller");

   No_Args          : constant Argument_List (1 .. 0) := (others => null);

   procedure Execute (Prog : String_Access; Args : Argument_List);
   --  Execute the command and raise Fatal Error if not successful

   procedure Execute_XE_Gcc
     (Source : String_Access;
      Target : String_Access;
      Flags  : Argument_List);
   --  Execute xe-gcc and add gnatdist compilation flags

   procedure Execute_Gcc
     (Flags : Argument_List;
      File : String_Access);
   --  Execute gcc and add gnatdist compilation flags

   procedure Execute_Gnatmake
     (Prog  : File_Name_Type;
      Exec  : File_Name_Type;
      Margs : Argument_List;
      Cargs : Argument_List;
      Bargs : Argument_List;
      Largs : Argument_List);
   --  Execute gnatmake and add gnatdist flags

   -----------
   -- Later --
   -----------

   function Later (File1, File2 : Name_Id) return Boolean is
   begin
      return Source_File_Stamp (File1) > Source_File_Stamp (File2);
   end Later;

   --------------
   -- Register --
   --------------

   function Register (Name : String) return Name_Id is
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      return Name_Find;
   end Register;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Prog : String_Access;
      Args : Argument_List) is
      Success : Boolean := False;
   begin

      if Verbose_Mode then
         Write_Str (Prog.all);
         for Index in Args'Range loop
            if Args (Index) = null then
               Write_Str (" <null>");
            else
               Write_Str (" ");
               Write_Str (Args (Index).all);
            end if;
         end loop;
         Write_Eol;
      end if;

      Spawn (Prog.all, Args, Success);

      if not Success then
         Write_Program_Name;
         Write_Str (": ");
         Write_Str (Prog.all);
         Write_Str (" failed");
         Write_Eol;
         raise XE.Fatal_Error;
      end if;

   end Execute;

   -----------------
   -- Unlink_File --
   -----------------

   procedure Unlink_File (File : File_Name_Type) is
      File_Name : String_Access := new String (1 .. Strlen (File));
   procedure Free is new Unchecked_Deallocation (String, String_Access);
   begin
      Get_Name_String (File);
      File_Name.all := Name_Buffer (1 .. Name_Len);
      Execute (Rm, (Force, File_Name));
      Free (File_Name);
   end Unlink_File;

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

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (To : in File_Name_Type) is

      C_Path : String (1 .. Strlen (To) + 1);

      function Chdir (Path : System.Address) return Int;
      pragma Import (C, Chdir, "chdir");

   begin

      Get_Name_String (To);
      C_Path (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      C_Path (Name_Len + 1) := Ascii.Nul;
      if Chdir (C_Path'Address) /= 0 then
         Write_Program_Name;
         Write_Str (": Cannot change dir to ");
         Write_Name (To);
         Write_Eol;
         raise XE.Fatal_Error;
      end if;

   end Change_Dir;

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

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (File   : in File_Descriptor;
      Line : in String) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if Write (File, Line'Address, Line'Length) /= Line'Length then
         Write_Str ("error : disk full");
         Write_Eol;
         raise XE.Fatal_Error;
      end if;

   end Write_Str;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name
     (File : in File_Descriptor;
      Name : in Name_Id) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      Get_Name_String (Name);
      if Name_Buffer (Name_Len - 1) = '%' then --  %
         Name_Len := Name_Len - 2;
      end if;
      if Write (File, Name_Buffer'Address, Name_Len) /= Name_Len then
         Write_Str ("error : disk full");
         Write_Eol;
         raise XE.Fatal_Error;
      end if;

   end Write_Name;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol
     (File : in File_Descriptor) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if EOL'Length /= Write (File, EOL'Address, EOL'Length) then
         Write_Str ("error : disk full");
         Write_Eol;
         raise XE.Fatal_Error;
      end if;

   end Write_Eol;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   : in out File_Descriptor;
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
         raise XE.Fatal_Error;
      end if;

      if Exec then
         Execute
           (Chmod,
            (1 => new String'("u+x"),
             2 => new String'(File_Name (1 .. File_Name_Len))));
      end if;

   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : File_Name_Type) is
      Error : Boolean;
   begin
      Get_Name_String (File);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Ascii.Nul;
      Delete_File (Name_Buffer'Address, Error);
   end Delete;

   --------------------
   -- Execute_XE_Gcc --
   --------------------

   procedure Execute_XE_Gcc
     (Source : String_Access;
      Target : String_Access;
      Flags  : Argument_List) is
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

   -----------------
   -- Execute_Gcc --
   -----------------

   procedure Execute_Gcc
     (Flags : Argument_List;
      File  : String_Access) is
      N : Natural := 1;
      L : constant Natural
        := Gcc_Switches.Last - Gcc_Switches.First + 1 + Flags'Length + 1;
      A : Argument_List (1 .. L);
   begin
      for I in Flags'Range loop
         A (N) := Flags (I);
         N := N + 1;
      end loop;
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         A (N) := Gcc_Switches.Table (I);
         N := N + 1;
      end loop;
      A (N) := File;
      Execute (Gcc, A);
   end Execute_Gcc;

   ----------------------
   -- Execute_Gnatmake --
   ----------------------

   procedure Execute_Gnatmake
     (Prog  : File_Name_Type;
      Exec  : File_Name_Type;
      Margs : Argument_List;
      Cargs : Argument_List;
      Bargs : Argument_List;
      Largs : Argument_List) is

      ALI_File : constant File_Name_Type := Prog & ALI_Suffix;
      ALI_Name : String (1 .. Strlen (ALI_File));

   begin

      if not Opt.Force_Compilations and then
        Is_Regular_File (Exec) and then
        (not Sources_Modified) and then
        Source_File_Stamp (Exec) > Most_Recent_Stamp then
         return;
      end if;

      Gnatmake_Execution :
      declare
         Gnatmake_Length : constant Natural :=
           1 + Margs'Length + 1 + 1 + Cargs'Length +
           Gcc_Switches.Last - Gcc_Switches.First + 1;
         Gnatmake_Args   : Argument_List (1 .. Gnatmake_Length);
         N               : Positive := 1;
         Prog_Name       : String (1 .. Strlen (Prog));
      begin
         Get_Name_String (Prog);
         Prog_Name := Name_Buffer (1 .. Name_Len);
         Gnatmake_Args (N) := new String'(Prog_Name);
         N := N + 1;
         Gnatmake_Args (N) := Compile_Flag;
         N := N + 1;
         if Opt.Force_Compilations then
            Gnatmake_Args (N) := Force;
            N := N + 1;
         end if;
         for I in Margs'Range loop
            Gnatmake_Args (N) := Margs (I);
            N := N + 1;
         end loop;
         if Gcc_Switches.First <= Gcc_Switches.Last or else
           0 < Cargs'Length then
            Gnatmake_Args (N) := Cargs_Flag;
            N := N + 1;
            for I in Cargs'Range loop
               Gnatmake_Args (N) := Cargs (I);
               N := N + 1;
            end loop;
            for I in Gcc_Switches.First .. Gcc_Switches.Last loop
               Gnatmake_Args (N) := Gcc_Switches.Table (I);
               N := N + 1;
            end loop;
         end if;
         Execute (Gnatmake, Gnatmake_Args (1 .. N - 1));
      end Gnatmake_Execution;

      Gnatbind_Execution :
      declare
         Gnatbind_Length : constant Natural :=
           1 + Bargs'Length + 1 +
           Binder_Switches.Last - Binder_Switches.First + 1;
         Gnatbind_Args   : Argument_List (1 .. Gnatbind_Length);
         N               : Positive := 1;
      begin
         Get_Name_String (ALI_File);
         ALI_Name := Name_Buffer (1 .. Name_Len);
         for I in Bargs'Range loop
            Gnatbind_Args (N) := Bargs (I);
            N := N + 1;
         end loop;
         if Verbose_Mode then
            Gnatbind_Args (N) := GNAT_Verbose;
            N := N + 1;
         end if;
         for I in Binder_Switches.First .. Binder_Switches.Last loop
            Gnatbind_Args (N) := Binder_Switches.Table (I);
            N := N + 1;
         end loop;
         Gnatbind_Args (N) := new String'(ALI_Name);
         N := N + 1;
         Execute (Gnatbind, Gnatbind_Args (1 .. N - 1));
      end Gnatbind_Execution;

      Gnatlink_Execution :
      declare
         Gnatlink_Length : constant Natural :=
           1 + Largs'Length + 3 +
           Linker_Switches.Last - Linker_Switches.First + 1;
         Gnatlink_Args   : Argument_List (1 .. Gnatlink_Length);
         Exec_Name       : String (1 .. Strlen (Exec));
         N               : Positive := 1;
      begin
         Get_Name_String (Exec);
         Exec_Name := Name_Buffer (1 .. Name_Len);
         Gnatlink_Args (N) := Output_Option;
         N := N + 1;
         Gnatlink_Args (N) := new String'(Exec_Name);
         N := N + 1;
         for I in Largs'Range loop
            Gnatlink_Args (N) := Largs (I);
            N := N + 1;
         end loop;
         if Verbose_Mode then
            Gnatlink_Args (N) := GNAT_Verbose;
            N := N + 1;
         end if;
         for I in Linker_Switches.First .. Linker_Switches.Last loop
            Gnatlink_Args (N) := Linker_Switches.Table (I);
            N := N + 1;
         end loop;
         Gnatlink_Args (N) := new String'(ALI_Name);
         N := N + 1;
         Execute (Gnatlink, Gnatlink_Args (1 .. N - 1));
      end Gnatlink_Execution;

   end Execute_Gnatmake;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (N : Unit_Name_Type) is
   begin
      Get_Decoded_Name_String (N);
      Set_Casing (Mixed_Case, Mixed_Case);
      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '-' then
            Name_Buffer (J) := '.';
         end if;
      end loop;
      Name_Len := Name_Len - 2;
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Write_Unit_Name;

   ------------
   -- Strlen --
   ------------

   function Strlen (Name : in Name_Id) return Natural is
   begin
      Get_Name_String (Name);
      return Name_Len;
   end Strlen;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return GNAT.Os_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len));
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory    (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return Is_Directory (Name_Buffer (1 .. Name_Len));
   end Is_Directory;

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

   ---------------------
   -- Build_Partition --
   ---------------------

   procedure Build_Partition (Partition : Name_Id; Exec : File_Name_Type) is
   begin
      Change_Dir (DSA_Dir & Dir_Sep_Id & Partition);
      Execute_Gnatmake
        (Partition,
         Exec,
         (GNAT_Verbose,
          I_Current_Dir,
          I_Caller_Dir,
          I_G_Parent_Dir,
          I_GARLIC_Dir),
         No_Args,
         (I_Current_Dir,
          I_Caller_Dir,
          I_G_Parent_Dir,
          I_GARLIC_Dir),
         (1 => L_GARLIC_Dir));
      Change_Dir (G_Parent_Dir);
   end Build_Partition;

   ------------------------
   -- Compile_RCI_Caller --
   ------------------------

   procedure Compile_RCI_Caller (Source : File_Name_Type) is
      Source_Name_Len : Natural := Strlen (Source);
      Source_Name     : String (1 .. Source_Name_Len);
   begin
      Maybe_Most_Recent_Stamp (Source_File_Stamp (Source));
      Get_Name_String (Source);
      Source_Name := Name_Buffer (1 .. Name_Len);
      Execute_Gcc
        ((Compile_Flag,
          Caller_Compile_Flag,
          I_G_Parent_Dir,
          I_GARLIC_Dir),
         new String' (Source_Name));
   end Compile_RCI_Caller;

   --------------------------
   -- Compile_RCI_Receiver --
   --------------------------

   procedure Compile_RCI_Receiver (Source : File_Name_Type) is
      Source_Name_Len : Natural := Strlen (Source);
      Source_Name     : String (1 .. Source_Name_Len);
   begin
      Maybe_Most_Recent_Stamp (Source_File_Stamp (Source));
      Get_Name_String (Source);
      Source_Name := Name_Buffer (1 .. Name_Len);
      Execute_Gcc
        ((Compile_Flag,
          Receiver_Compile_Flag,
          I_G_Parent_Dir,
          I_GARLIC_Dir),
         new String' (Source_Name));
   end Compile_RCI_Receiver;

   ----------------------
   -- Build_RCI_Caller --
   ----------------------

   procedure Build_RCI_Caller     (Source, Target : File_Name_Type) is
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
   end Build_RCI_Caller;

   ------------------------
   -- Build_RCI_Receiver --
   ------------------------

   procedure Build_RCI_Receiver   (Source, Target : File_Name_Type) is
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
   end Build_RCI_Receiver;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dir_Sep : String (1 .. 1) := (others => Directory_Separator);
   begin
      if Verbose_Mode then
         GNAT_Verbose := new String' ("-v");
      else
         GNAT_Verbose := new String' ("-q");
      end if;

      Obj_Suffix     := Register (".o");
      ALI_Suffix     := Register (".ali");
      ADS_Suffix     := Register (".ads");
      ADB_Suffix     := Register (".adb");

      Dir_Sep_Id     := Register (Dir_Sep);
      Dot_Sep_Id     := Register (".");

      DSA_Dir        := Register ("dsa");

      Caller_Dir     := DSA_Dir & Dir_Sep_Id & Register ("caller");
      Receiver_Dir   := DSA_Dir & Dir_Sep_Id & Register ("receiver");

      Parent_Dir     := Register ("..");
      G_Parent_Dir   := Parent_Dir & Dir_Sep_Id & Parent_Dir;

      Output_Option  := new String'("-o");
      XE_Gcc         := Locate ("xe-gcc");
      Gcc            := Locate ("gcc");
      Gnatmake       := Locate ("gnatmake");
      Gnatbind       := Locate ("gnatbind");
      Gnatlink       := Locate ("gnatlink");
      Mkdir          := Locate ("mkdir");
      Copy           := Locate ("cp");
      Link           := Locate ("ln", False);
      Chmod          := Locate ("chmod");

      declare
         GARLIC_Dir  : constant String_Access := Get_GARLIC_Dir;
         GARLIC_Flag : String (1 .. GARLIC_Dir'Length + 2);
      begin
         GARLIC_Flag (3 .. GARLIC_Flag'Length) := GARLIC_Dir.all;
         GARLIC_Flag (1 .. 2) := "-I";
         I_GARLIC_Dir := new String'(GARLIC_Flag);
         GARLIC_Flag (2) := 'L';
         L_GARLIC_Dir := new String'(GARLIC_Flag);
      end;

   end Initialize;

end XE_Utils;

