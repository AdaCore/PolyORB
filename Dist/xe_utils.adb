------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                            X E _ U T I L S                               --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
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
   Output_Option  : constant String_Access := new String'("-o");
   XE_Gcc         : constant String_Access := Locate ("xe-gcc");
   Gcc            : constant String_Access := Locate ("gcc");
   Gnatmake       : constant String_Access := Locate ("gnatmake");
   Mkdir          : constant String_Access := Locate ("mkdir");
   Copy           : constant String_Access := Locate ("cp");
   Link           : constant String_Access := Locate ("ln", False);
   Chmod          : constant String_Access := Locate ("chmod");
   Rm             : constant String_Access := Locate ("rm");
   Gnatbind       : constant String_Access := Locate ("gnatbind");
   Gnatlink       : constant String_Access := Locate ("gnatlink");

   EOL : constant String (1 .. 1) := (others => Ascii.LF);

   Preserve              : constant String_Access := new String' ("-p");
   Symbolic              : constant String_Access := new String' ("-s");
   Force                 : constant String_Access := new String' ("-f");
   Compile_Flag          : constant String_Access := new String' ("-c");
   Exclude_File_Flag     : constant String_Access := new String' ("-x");
   Receiver_Build_Flag   : constant String_Access := new String' ("-gnatzr");
   Caller_Build_Flag     : constant String_Access := new String' ("-gnatzc");
   Receiver_Compile_Flag : constant String_Access := new String' ("-gnatzR");
   Caller_Compile_Flag   : constant String_Access := new String' ("-gnatzC");

   I_GARLIC_Dir          : String_Access;
   L_GARLIC_Dir          : String_Access;

   Sem_Only_Flag    : constant String_Access := new String' ("-gnatc");
   --  Workaround : bad object file generated during stub generation

   I_Current_Dir    : constant String_Access := new String' ("-I.");
   I_Caller_Dir     : constant String_Access := new String' ("-I../caller");
   I_DSA_Caller_Dir : constant String_Access := new String' ("-Idsa/caller");
   I_G_Parent_Dir   : constant String_Access := new String' ("-I../..");

   L_Current_Dir    : constant String_Access := new String' ("-L.");
   L_Caller_Dir     : constant String_Access := new String' ("-L../caller");
   L_DSA_Caller_Dir : constant String_Access := new String' ("-Ldsa/caller");
   L_G_Parent_Dir   : constant String_Access := new String' ("-L../..");

   No_Args          : constant Argument_List (1 .. 0) := (others => null);

   procedure Execute (Prog : String_Access; Args : Argument_List);
   --  Execute the command and raise Fatal Error if not successful

   procedure Execute_XE_Gcc
     (Source : String_Access;
      Target : String_Access;
      Flags  : Argument_List);
   --  Execute xe-gcc and add gnatdist compilation flags

   procedure Execute_Gcc
     (File : File_Name_Type;
      Args : Argument_List);
   --  Execute gcc and add gnatdist compilation flags

   procedure Execute_Bind
     (Lib  : File_Name_Type;
      Args : Argument_List);
   --  Execute gnatbind and add gnatdist flags

   procedure Execute_Link
     (Lib  : File_Name_Type;
      Exec : File_Name_Type;
      Args : Argument_List);
   --  Execute gnatlink and add gnatdist flags

   -----------------
   -- More_Recent --
   -----------------

   function More_Recent (File1, File2 : Name_Id) return Boolean is
   begin
      return Source_File_Stamp (File1) > Source_File_Stamp (File2);
   end More_Recent;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Prog : String_Access;
      Args : Argument_List) is
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

   -----------------
   -- Write_Stamp --
   -----------------

   procedure Write_Stamp (File : Name_Id) is
   begin
      Write_Str (" (");
      Write_Str (String (Source_File_Stamp (File)));
      Write_Str (")");
   end Write_Stamp;

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
         raise XE.Fatal_Error;
      end if;

      if Stdout then
         Write_Str (Standout, Line);
      end if;

   end Write_Str;

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
            raise XE.Fatal_Error;
         end if;

         if Stdout then
            Write_Name (Standout, Name);
         end if;

      end if;

   end Write_Name;

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
         raise XE.Fatal_Error;
      end if;

      if Stdout then
         Write_Eol (Standout);
      end if;

   end Write_Eol;

   ---------------------------
   -- Build_Compile_Command --
   ---------------------------

   procedure Build_Compile_Command (Name : in File_Name_Type)
   is
   begin
      Write_Str  (Standout, Gnatmake.all);
      Write_Str  (Standout, " -c ");
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         Write_Str (Standout, Gcc_Switches.Table (I).all);
         Write_Str (Standout, " ");
      end loop;
      Write_Name (Standout, Name);
      Write_Eol  (Standout);
   end Build_Compile_Command;

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
     (File : File_Name_Type;
      Args : Argument_List) is

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
   -- Execute_Bind --
   ------------------

   procedure Execute_Bind
     (Lib  : File_Name_Type;
      Args : Argument_List) is


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

   ------------------
   -- Execute_Link --
   ------------------

   procedure Execute_Link
     (Lib  : File_Name_Type;
      Exec : File_Name_Type;
      Args : Argument_List) is


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

      if Opt.Force_Compilations or else
        More_Recent (Configuration & ADB_Suffix,
                     Configuration & ALI_Suffix) then

         Execute_Gcc
           (Configuration & ADB_Suffix,
            (I_Current_Dir,
             I_Caller_Dir,
             I_G_Parent_Dir)
            );

      end if;

      if Opt.Force_Compilations or else
        Most_Recent_Stamp > Source_File_Stamp (Exec) then

         --  I_Garlic_Dir is not included here because it was added by the
         --  gnatdist shell script.


         Execute_Bind
           (Configuration & ALI_Suffix,
            (I_Current_Dir,
             I_Caller_Dir,
             I_G_Parent_Dir)
            );

         Execute_Link
           (Configuration & ALI_Suffix,
            Exec,
            (L_Current_Dir,
             L_Caller_Dir,
             L_G_Parent_Dir,
             L_GARLIC_Dir)
            );

      end if;

      Change_Dir (G_Parent_Dir);

   end Build_Partition;

   ------------------------
   -- Compile_RCI_Caller --
   ------------------------

   procedure Compile_RCI_Caller (Source : File_Name_Type) is
   begin
      Maybe_Most_Recent_Stamp (Source_File_Stamp (Source));
      Execute_Gcc
        (Source,
         (Caller_Compile_Flag,
          I_G_Parent_Dir,
          I_GARLIC_Dir)
         );
   end Compile_RCI_Caller;

   --------------------------
   -- Compile_RCI_Receiver --
   --------------------------

   procedure Compile_RCI_Receiver (Source : File_Name_Type) is
   begin
      Maybe_Most_Recent_Stamp (Source_File_Stamp (Source));
      Execute_Gcc
        (Source,
         (Receiver_Compile_Flag,
          I_G_Parent_Dir,
          I_GARLIC_Dir)
         );
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

      Obj_Suffix     := Str_To_Id (".o");
      ALI_Suffix     := Str_To_Id (".ali");
      ADS_Suffix     := Str_To_Id (".ads");
      ADB_Suffix     := Str_To_Id (".adb");

      Spec_Suffix    := Str_To_Id ("%s");
      Body_Suffix    := Str_To_Id ("%b");

      Dir_Sep_Id     := Str_To_Id (Dir_Sep);
      Dot_Sep_Id     := Str_To_Id (".");

      DSA_Dir        := Str_To_Id ("dsa");

      Caller_Dir     := DSA_Dir & Dir_Sep_Id & Str_To_Id ("caller");
      Receiver_Dir   := DSA_Dir & Dir_Sep_Id & Str_To_Id ("receiver");

      Parent_Dir     := Str_To_Id ("..");
      G_Parent_Dir   := Parent_Dir & Dir_Sep_Id & Parent_Dir;

      PWD_Id         := Str_To_Id ("`pwd`/");

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

