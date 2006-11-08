------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ L I S T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2006 Free Software Foundation, Inc.           --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Table;

with XE_Front;    use XE_Front;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Units;    use XE_Units;
with XE_Utils;    use XE_Utils;
pragma Elaborate_All (XE_Utils);

package body XE_List is

   ------------------------
   --  Source File Stack --
   ------------------------

   package Sources is new GNAT.Table
     (Table_Component_Type => File_Name_Type,
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
      No_Such_Token    : constant Token_Type := 27;

      subtype Valid_Token_Type is Token_Type
        range Token_Type'First .. Token_Type'Last - 1;

      Image : constant array (Valid_Token_Type) of String_Access :=
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

      procedure Open (Fname : String);
      --  Open Fname in order to parse it. Initialize the parser by
      --  loading file in a buffer.

      procedure Get_Line;
      --  Read one line from current buffer and evaluate fields

      function  Field (N : Natural) return String;
      --  Return Nth field. N has to be in the range of 0 and
      --  Number_Of_Fields. When N is zero, return the full line.

      function  Number_Of_Fields return Natural;
      --  Return number of fields in the current line

      function  End_Of_File return Boolean;
      --  Return True when there is nothing else to read

      function  Token (N : Positive) return Token_Type;
      --  Return the token corresponding to field N. When there is no
      --  such corresponding token return No_Such_Token. Note that N
      --  cannot be zero.

      procedure Close;
      --  Close current file and free buffer

   end Parser;

   procedure Dump_ALI (My_ALI : ALI_Id);
   --  Dump content of ALI record

   procedure Load_ALIs (Output : File_Name_Type);
   --  Read from Output all the ALI files available

   ------------
   -- Parser --
   ------------

   package body Parser is

      use ASCII;

      type Field_Record is record
         First, Last : Natural;
      end record;

      Chars    : String (1 .. 256);
      N_Chars  : Natural;
      Fields   : array (1 .. 32) of Field_Record;
      N_Fields : Natural := 0;

      Buffer : Text_Buffer_Ptr;
      First  : Text_Ptr;
      Index  : Text_Ptr;
      Last   : Text_Ptr;

      --  These values are automatically generated for the set of
      --  tokens from Token_Type. Do not modify them.

      P  : constant array (0 ..  2) of Natural := (01, 02, 04);
      T1 : constant array (0 ..  2) of Byte    := (54, 27, 20);
      T2 : constant array (0 ..  2) of Byte    := (45, 24, 26);
      G  : constant array (0 .. 54) of Byte    :=
        (03, 00, 00, 00, 13, 11, 00, 00, 00, 00,
         13, 00, 03, 00, 13, 00, 00, 00, 06, 01,
         00, 00, 11, 00, 04, 13, 08, 26, 00, 00,
         24, 00, 22, 00, 00, 02, 00, 00, 00, 00,
         00, 05, 25, 03, 00, 26, 26, 14, 10, 00,
         06, 08, 12, 00, 00);

      function Hash (S : String) return Natural;

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         Free (Buffer);
         Index    := First;
         Last     := First;
         N_Fields := 0;
      end Close;

      -----------------
      -- End_Of_File --
      -----------------

      function End_Of_File return Boolean is
      begin
         return Last = Index;
      end End_Of_File;

      -----------
      -- Field --
      -----------

      function  Field (N : Natural) return String is
      begin
         if N = 0 then
            return Chars (1 .. N_Chars);
         end if;
         if N > N_Fields then
            return "";
         end if;
         return Chars (Fields (N).First .. Fields (N).Last);
      end Field;

      --------------
      -- Get_Line --
      --------------

      procedure Get_Line is
         C : Character;

      begin
         if Last <= Index then
            raise Fatal_Error;
         end if;
         N_Fields := 0;
         Fields   := (others => (0, 0));
         N_Chars  := 0;
         while Index < Last loop
            C := Buffer (Index);
            Index := Index + 1;
            case C is
               when LF | FF | CR | VT =>
                  if N_Fields > 0
                    and then Fields (N_Fields).Last = 0
                  then
                     Fields (N_Fields).Last := N_Chars;
                  end if;
                  return;

               when ' ' | HT =>
                  if N_Fields > 0
                    and then Fields (N_Fields).Last = 0
                  then
                     Fields (N_Fields).Last := N_Chars;
                  end if;
                  N_Chars := N_Chars + 1;
                  Chars (N_Chars) := C;

               when others =>
                  N_Chars := N_Chars + 1;
                  Chars (N_Chars) := C;
                  if N_Fields = 0
                    or else Fields (N_Fields).Last /= 0
                  then
                     N_Fields := N_Fields + 1;
                     Fields (N_Fields).First := N_Chars;
                  end if;
            end case;
         end loop;

         if N_Fields > 0
           and then Fields (N_Fields).Last = 0
         then
            Fields (N_Fields).Last := N_Chars;
         end if;
      end Get_Line;

      ----------
      -- Hash --
      ----------

      function Hash (S : String) return Natural is
         F : constant Natural := S'First - 1;
         L : constant Natural := S'Length;
         F1, F2 : Natural := 0;
         J : Natural;
      begin
         for K in P'Range loop
            exit when L < P (K);
            J  := Character'Pos (S (P (K) + F));
            F1 := (F1 + Natural (T1 (K)) * J) mod 55;
            F2 := (F2 + Natural (T2 (K)) * J) mod 55;
         end loop;
         return (Natural (G (F1)) + Natural (G (F2))) mod 27;
      end Hash;

      ----------------------
      -- Number_Of_Fields --
      ----------------------

      function Number_Of_Fields return Natural is
      begin
         if Buffer = null then
            raise Fatal_Error;
         end if;
         return N_Fields;
      end Number_Of_Fields;

      ----------
      -- Open --
      ----------

      procedure Open (Fname : String) is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Fname);
         Read_File (Name_Find, First, Last, Buffer);
         if Buffer = null then
            raise Fatal_Error;
         end if;
         Index := First;
      end Open;

      -----------
      -- Token --
      -----------

      function Token (N : Positive) return Token_Type is
         F : constant String  := Field (N);
         T : constant Natural := Hash (F);

      begin
         if T in Valid_Token_Type
           and then F = Image (T).all
         then
            return T;
         end if;
         return No_Such_Token;
      end Token;

   end Parser;

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
         Img : String_Access renames Image (T);

      begin
         if T in T_No_ALI .. T_Flags then
            for J in 1 .. N loop
               Write_Str ("   ");
            end loop;

            Write_Str (Img.all);

            for J in Img'Length .. 12 loop
               Write_Char (' ');
            end loop;

            Write_Str ("=>");

            if T in T_No_ALI .. T_With then
               null;

            elsif T in T_Source .. T_Name then
               Write_Char (' ');
            end if;

         elsif T in T_Preelaborated .. T_Body then
            Write_Char (' ');
            Write_Str  (Img.all);

         else
            Write_Str  (Img.all);
         end if;
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
      File : File_Descriptor;

   begin
      Register_Temp_File (File, Part_Main_Src_Name);
      Register_Temp_File (Part_Main_ALI_Name);
      Register_Temp_File (Part_Main_Obj_Name);
      Set_Output (File);
      Write_Line ("pragma Warnings (Off);");
   end Initialize;

   ---------------
   -- Load_ALIs --
   ---------------

   procedure Load_ALIs (Output : File_Name_Type) is
      use Parser;

      My_ALI      : ALI_Id         := No_ALI_Id;
      My_Unit     : Unit_Id        := No_Unit_Id;
      My_With     : With_Id        := No_With_Id;
      My_Sdep     : Sdep_Id        := No_Sdep_Id;
      Afile       : File_Name_Type;
      Sfile       : File_Name_Type;

      function File_Name (N : Natural) return File_Name_Type;
      --  Get the Nth field and return it as a file name type

      ---------------
      -- File_Name --
      ---------------

      function File_Name (N : Natural) return File_Name_Type is
      begin
         return Id (Format_Pathname (Parser.Field (N), UNIX));
      end File_Name;

   begin
      Parser.Open (Get_Name_String (Output));
      while not Parser.End_Of_File loop
         Parser.Get_Line;

         case Parser.Token (1) is
            when T_No_ALI =>
               My_ALI  := No_ALI_Id;
               My_Unit := No_Unit_Id;
               My_With := No_With_Id;
               My_Sdep := No_Sdep_Id;

            when T_ALI =>

               --  Allocate ALI and initialize ALI entry

               ALIs.Increment_Last;
               My_ALI              := ALIs.Last;
               ALIs.Table (My_ALI) := Default_ALI;
               My_Sdep             := No_Sdep_Id;
               My_Unit             := No_Unit_Id;
               My_With             := No_With_Id;

            when T_Unit =>

               --  Allocate Unit and initialize Unit entry

               Units.Increment_Last;
               My_Unit                      := Units.Last;
               Units.Table (My_Unit)        := Default_Unit;
               Units.Table (My_Unit).My_ALI := My_ALI;
               My_With                      := No_With_Id;

               --  Add it to ALI unit list

               if ALIs.Table (My_ALI).Last_Unit = No_Unit_Id then
                  ALIs.Table (My_ALI).First_Unit := My_Unit;
               end if;
               ALIs.Table (My_ALI).Last_Unit := My_Unit;

            when T_With =>

               --  Allocate With and initialize With entry

               Withs.Increment_Last;
               My_With                      := Withs.Last;
               Withs.Table (My_With)        := Default_With;

               --  Add it to unit with list

               if Units.Table (My_Unit).Last_With = No_With_Id then
                  Units.Table (My_Unit).First_With := My_With;
               end if;
               Units.Table (My_Unit).Last_With := My_With;

            when T_Source =>

               Sdep.Increment_Last;
               My_Sdep := Sdep.Last;
               Sdep.Table (My_Sdep).Sfile := File_Name (3);

               --  Add it to ALI sdep list

               if ALIs.Table (My_ALI).Last_Sdep = No_Sdep_Id then
                  ALIs.Table (My_ALI).First_Sdep := My_Sdep;
               end if;
               ALIs.Table (My_ALI).Last_Sdep := My_Sdep;

               --  Detect use of tasking

               Get_Name_String (Strip_Directory (Sdep.Table (My_Sdep).Sfile));

               if Name_Len > 8
                 and then Name_Buffer (1 .. 8) = "s-taskin"
               then
                  ALIs.Table (My_ALI).Tasking := 'U';
               end if;

            when T_Afile =>

               --  If My_With is not null, then this attribute belongs
               --  to a With entry.

               if My_With /= No_With_Id then
                  Withs.Table (My_With).Afile := File_Name (3);

               else
                  Afile := File_Name (3);
                  Set_ALI_Id (Afile, My_ALI);

                  if My_ALI /= No_ALI_Id then
                     ALIs.Table (My_ALI).Afile := Afile;
                  end if;
               end if;

            when T_Ofile =>
               ALIs.Table (My_ALI).Ofile := File_Name (3);

            when T_Sfile =>

               --  If My_With is not null, then this attribute belongs
               --  to a Withs entry.

               if My_With /= No_With_Id then
                  Withs.Table (My_With).Sfile := File_Name (3);

               --  If My_Unit is not null, then this attribute belongs
               --  to a Units entry.

               elsif My_Unit /= No_Unit_Id then
                  Units.Table (My_Unit).Sfile := File_Name (3);
                  Set_Unit_Id (Units.Table (My_Unit).Sfile, My_Unit);

               elsif My_ALI /= No_ALI_Id then
                  ALIs.Table (My_ALI).Sfile := File_Name (3);

               else
                  Sfile := File_Name (3);
                  Afile := To_Afile (Sfile);
                  Set_ALI_Id (Afile, No_ALI_Id);
               end if;

            when T_Name =>

               --  If My_With is not null, then this attribute belongs
               --  to a With entry.

               if My_With /= No_With_Id then
                  Withs.Table (My_With).Uname := File_Name (3);

               else
                  Units.Table (My_Unit).Uname := File_Name (3);

                  --  When Uname is unknown in ALI, update it and
                  --  associate unit name with ALI id. Note that the
                  --  unit name is not yet encoded.

                  if No (ALIs.Table (My_ALI).Uname) then
                     ALIs.Table (My_ALI).Uname := Units.Table (My_Unit).Uname;
                     Set_ALI_Id (ALIs.Table (My_ALI).Uname, My_ALI);
                  end if;
               end if;

            when T_Main =>

               if Parser.Token (3) = T_Procedure then
                  ALIs.Table (My_ALI).Main_Program := Proc;
               else
                  ALIs.Table (My_ALI).Main_Program := Func;
               end if;

            when T_Kind =>

               --  If My_With is not null, then this attribute belongs
               --  to a With entry.

               if My_With /= No_With_Id then

                  Get_Name_String (Withs.Table (My_With).Uname);
                  Add_Char_To_Name_Buffer ('%');

                  if Parser.Token (3) = T_Spec then
                     Add_Char_To_Name_Buffer ('s');
                  else
                     Add_Char_To_Name_Buffer ('b');
                  end if;

                  Withs.Table (My_With).Uname := Name_Find;

               else

                  --  Is it a subprogram or a package

                  if Parser.Token (3) = T_Subprogram then
                     Units.Table (My_Unit).Unit_Kind := 's';

                  else
                     Units.Table (My_Unit).Unit_Kind := 'p';
                  end if;

                  --  Prepare to set unit name info

                  Get_Name_String (Units.Table (My_Unit).Uname);
                  Add_Char_To_Name_Buffer ('%');

                  --  Is it a spec or a body

                  if Parser.Token (4) = T_Spec then
                     Add_Char_To_Name_Buffer ('s');
                     if ALIs.Table (My_ALI).First_Unit = My_Unit then
                        Units.Table (My_Unit).Utype := Is_Spec_Only;

                     else
                        Units.Table (ALIs.Table (My_ALI).First_Unit).Utype :=
                          Is_Body;
                        Units.Table (My_Unit).Utype := Is_Spec;
                     end if;

                  else
                     Add_Char_To_Name_Buffer ('b');
                     Units.Table (My_Unit).Utype := Is_Body_Only;
                  end if;

                  --  Set unit name info

                  Units.Table (My_Unit).Uname := Name_Find;
                  Set_Unit_Id (Units.Table (My_Unit).Uname, My_Unit);
               end if;

            when T_Flags =>
               for F in 3 .. Parser.Number_Of_Fields loop
                  case Parser.Token (F) is
                     when T_Has_RACW =>
                        Units.Table (My_Unit).Has_RACW := True;

                     when T_Remote_Types =>
                        Units.Table (My_Unit).Remote_Types := True;

                     when T_Shared_Passive =>
                        Units.Table (My_Unit).Shared_Passive := True;

                     when T_RCI =>
                        Units.Table (My_Unit).RCI := True;

                     when T_Predefined =>
                        Units.Table (My_Unit).Predefined := True;

                     when T_Internal =>
                        Units.Table (My_Unit).Internal := True;

                     when T_Is_Generic =>
                        Units.Table (My_Unit).Is_Generic := True;

                     when T_Preelaborated =>
                        Units.Table (My_Unit).Preelaborated := True;

                     when others =>
                        null;
                  end case;
               end loop;

            when others =>
               null;
         end case;
      end loop;
      Parser.Close;
      Remove_Temp_File (Output);
   end Load_ALIs;

   -------------------------------
   -- Load_All_Registered_Units --
   -------------------------------

   procedure Load_All_Registered_Units is
      Comp_Flags : constant Argument_List :=
        (1 => Semantic_Only_Flag);

      List_Args : constant Argument_List :=
        (GLADE_List_Flag,
         Project_File_Flag,
         Project_File_Name);

      Make_Args : constant Argument_List :=
        (Compile_Only_Flag,
         Keep_Going_Flag,
         Project_File_Flag,
         Project_File_Name);

      List_Args_Length, Make_Args_Length : Natural;

      Sfile      : File_Name_Type;
      Afile      : File_Name_Type;
      ALI        : ALI_Id;
      Partition  : Partition_Id;
      Output     : File_Name_Type;
   begin
      --  Only use the project flags if a project has been set

      if Project_File_Name /= null then
         List_Args_Length := 3;
         Make_Args_Length := 4;
      else
         List_Args_Length := 1;
         Make_Args_Length := 2;
      end if;

      declare
         Make_Flags : Argument_List
           renames Make_Args (1 .. Make_Args_Length);

         List_Flags : Argument_List
           renames List_Args (1 .. List_Args_Length);
      begin

         Write_Line ("procedure Partition is");
         Write_Line ("begin");
         Write_Line ("   null;");
         Write_Line ("end Partition;");
         Set_Standard_Output;

         --  Build the monolithic application with a fake main subprogram
         --  Partition. Load the info from its ALI file.

         Sfile := Part_Main_Src_Name;
         Afile := To_Afile (Sfile);
         Build (Sfile, Make_Flags, Fatal => False, Silent => False);
         List ((1 => Afile), List_Flags, Output);
         Load_ALIs (Output);
         ALI := Get_ALI_Id (Afile);

         Remove_Temp_File (Part_Main_Src_Name);
         Remove_Temp_File (Part_Main_ALI_Name);
         Remove_Temp_File (Part_Main_Obj_Name);

         --  The compilation of partition.adb failed. There is no way to
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
                  Sources.Append (Sfile);
               end if;
            end loop;
         end loop;

         while Sources.First <= Sources.Last loop
            declare
               Last   : Natural := Sources.Last + 1 - Sources.First;
               Afiles : File_Name_List (1 .. Last);
               Sfiles : File_Name_List (1 .. Last);

            begin
               --  Load in Args the sources which corresponding ALI file
               --  is not yet available.

               Last := 0;
               for J in Sources.First .. Sources.Last loop
                  Sfile := Sources.Table (J);
                  Afile := To_Afile (Sfile);

                  --  We never tried to download this ALI file. Its info
                  --  is not a valid ALI id (not even No_ALI_Id).

                  if Get_Name_Table_Info (Afile) = 0 then
                     Last := Last + 1;
                     Afiles (Last) := Afile;
                     Sfiles (Last) := Sfile;
                  end if;
               end loop;
               Sources.Init;

               List (Afiles (1 .. Last), List_Flags, Output);
               Load_ALIs (Output);

               for J in 1 .. Last loop
                  Sfile := Sfiles (J);
                  Afile := Afiles (J);
                  ALI   := Get_ALI_Id (Afile);

                  --  The ALI file does not exist. It may come from a
                  --  missing body file although the spec file is
                  --  available (we compiled the main subprogram with
                  --  keep-going flag). So compile the spec file with
                  --  semantic only flag in order to obtain the ALI file
                  --  anyway. Then check this operation was correctly
                  --  performed which means that the unit was RCI. The
                  --  missing body file is not an issue as long as the
                  --  unit is not assigned to a partition to build.

                  if ALI = No_ALI_Id then
                     Compile (Sfile,
                              Comp_Flags,
                              Fatal => False,
                              Silent => True);

                     List ((1 => Afile), List_Flags, Output);
                     Load_ALIs (Output);

                     --  If the ALI file is still missing, then we have a
                     --  real problem.

                     ALI := Get_ALI_Id (Afile);
                     if ALI = No_ALI_Id then
                        raise Compilation_Error;
                     end if;

                     --  Check that the unit was really assigned to a
                     --  partition we are not going to build.

                     Partition := Get_Partition_Id (ALIs.Table (ALI).Uname);
                     if not Units.Table (ALIs.Table (ALI).Last_Unit).RCI
                       or else Partition = No_Partition_Id
                       or else Partitions.Table (Partition).To_Build
                     then
                        raise Compilation_Error;
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
                           Sources.Append (Sfile);
                        end if;
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
      end;
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
