------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ L I S T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1995-2004 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Table;
with XE_Front;    use XE_Front;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Types;    use XE_Types;
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

   procedure Push (Sfile : File_Name_Type);
   function  Pop return File_Name_Type;

   ------------
   -- Parser --
   ------------

   package Parser is

      subtype Token_Type is Natural range 0 .. 18;

      No_Such_ALI      : constant Token_Type :=  0;  --  "Can't"
      T_Unit           : constant Token_Type :=  1;  --  "Unit"
      T_Name           : constant Token_Type :=  2;  --  "Name"
      T_Kind           : constant Token_Type :=  3;  --  "Kind"
      T_Flags          : constant Token_Type :=  4;  --  "Flags"
      T_Source         : constant Token_Type :=  5;  --  "Source"
      T_Has_RACW       : constant Token_Type :=  6;  --  "Has_RACW"
      T_Remote_Types   : constant Token_Type :=  7;  --  "Remote_Types"
      T_Shared_Passive : constant Token_Type :=  8;  --  "Shared_Passive
      T_RCI            : constant Token_Type :=  9;  --  "RCI"
      T_Predefined     : constant Token_Type := 10;  --  "Predefined"
      T_Internal       : constant Token_Type := 11;  --  "Internal"
      T_Is_Generic     : constant Token_Type := 12;  --  "Is_Generic"
      T_Preelaborated  : constant Token_Type := 13;  --  "Preelaborated"
      T_Package        : constant Token_Type := 14;  --  "package"
      T_Subprogram     : constant Token_Type := 15;  --  "subprogram"
      T_Spec           : constant Token_Type := 16;  --  "spec"
      T_Body           : constant Token_Type := 17;  --  "body"
      No_Such_Token    : constant Token_Type := 18;

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

   procedure Load_ALI (Sfile : File_Name_Type; My_ALI : out ALI_Id);
   --  Read the ALI file corresponding to Sfile (spawning gnat list)
   --  and return an ALI id.

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

      subtype Valid_Token_Type is Token_Type
        range Token_Type'First .. Token_Type'Last - 1;

      Image : constant array (Valid_Token_Type) of String_Access :=
        (No_Such_ALI      => new String'("Can't"),
         T_Unit           => new String'("Unit"),
         T_Name           => new String'("Name"),
         T_Kind           => new String'("Kind"),
         T_Flags          => new String'("Flags"),
         T_Source         => new String'("Source"),
         T_Has_RACW       => new String'("Has_RACW"),
         T_Remote_Types   => new String'("Remote_Types"),
         T_Shared_Passive => new String'("Shared_Passive"),
         T_RCI            => new String'("RCI"),
         T_Predefined     => new String'("Predefined"),
         T_Internal       => new String'("Internal"),
         T_Is_Generic     => new String'("Is_Generic"),
         T_Preelaborated  => new String'("Preelaborated"),
         T_Package        => new String'("package"),
         T_Subprogram     => new String'("subprogram"),
         T_Spec           => new String'("spec"),
         T_Body           => new String'("body"));

      --  These values are automatically generated for the set of
      --  tokens from Token_Type. Do not modify them.

      P  : constant array (0 ..  1) of Natural := (2, 4);
      T1 : constant array (0 ..  1) of Byte    := (14, 25);
      T2 : constant array (0 ..  1) of Byte    := (19, 34);
      G  : constant array (0 .. 36) of Byte    :=
        (0, 0, 0, 1, 0, 14, 0, 12, 14, 0, 4, 0, 0, 5, 8, 4, 0, 4,
         12, 0, 0, 0, 0, 5, 0, 0, 10, 0, 5, 0, 3, 0, 11, 6, 0, 15, 0);

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
            F1 := (F1 + Natural (T1 (K)) * J) mod 37;
            F2 := (F2 + Natural (T2 (K)) * J) mod 37;
         end loop;
         return (Natural (G (F1)) + Natural (G (F2))) mod 18;
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
      R : constant ALIs_Record := ALIs.Table (My_ALI);

   begin
      Write_Str  ("Afile     => ");
      Write_Name (R.Afile);
      Write_Eol;
      for J in R.First_Unit .. R.Last_Unit loop
         Write_Str  ("*  Unit   => ");
         Write_Name (Units.Table (J).Uname);
         Write_Eol;
         Write_Str  ("*  Sfile  => ");
         Write_Name (Units.Table (J).Sfile);
         Write_Eol;
      end loop;
      for J in R.First_Sdep .. R.Last_Sdep loop
         Write_Str  ("*  Source => ");
         Write_Name (Sdep.Table (J).Sfile);
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

   --------------
   -- Load_ALI --
   --------------

   procedure Load_ALI (Sfile : File_Name_Type; My_ALI : out ALI_Id) is
      use Parser;

      Output      : File_Name_Type;
      Skip_Line   : Boolean := False;
      Ofile       : File_Name_Type := No_Name;
      Afile       : File_Name_Type := No_Name;
      My_Unit     : Unit_Id        := No_Unit_Id;
      My_Sdep     : Sdep_Id        := No_Sdep_Id;
      Empty_Lines : Natural        := 0;

   begin
      My_ALI := No_ALI_Id;

      --  Execute and parse result of gnat list -v -a

      List (Strip_Directory (Sfile), (Verbose_Flag, Readonly_Flag), Output);

      if No (Output) then
         return;
      end if;

      Parser.Open (Get_Name_String (Output));
      while Empty_Lines /= 5 loop
         Parser.Get_Line;
         if Parser.Number_Of_Fields = 0 then
            Empty_Lines := Empty_Lines + 1;
         end if;
      end loop;

      while not Parser.End_Of_File loop
         Parser.Get_Line;
         case Parser.Token (1) is
            when No_Such_ALI =>
               exit;

            when T_Unit =>

               --  Allocate Unit and initialize Unit entry

               Units.Increment_Last;
               My_Unit                      := Units.Last;
               Units.Table (My_Unit)        := Default_Unit;
               Units.Table (My_Unit).My_ALI := My_ALI;

               --  Add it to ALI unit list

               if ALIs.Table (My_ALI).First_Unit = No_Unit_Id then
                  ALIs.Table (My_ALI).First_Unit := My_Unit;
               end if;
               ALIs.Table (My_ALI).Last_Unit := My_Unit;

            when T_Name =>
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Parser.Field (3));
               Units.Table (My_Unit).Uname := Name_Find;

               --  When Uname is unknown in ALI, update it and associate
               --  unit name with ALI id.

               if No (ALIs.Table (My_ALI).Uname) then
                  ALIs.Table (My_ALI).Uname := Units.Table (My_Unit).Uname;
                  Set_ALI_Id (ALIs.Table (My_ALI).Uname, My_ALI);
               end if;

            when T_Kind =>

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

                  if Units.Table (My_Unit).Unit_Kind = 's' then
                     ALIs.Table (My_ALI).Main_Program := Proc;
                  end if;

            when T_Source =>
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Parser.Field (3));
               Units.Table (My_Unit).Sfile := Name_Find;
               if ALIs.Table (My_ALI).Sfile = No_File_Name then
                  ALIs.Table (My_ALI).Sfile :=
                    Units.Table (My_Unit).Sfile;
               end if;
               Set_ALI_Id (Units.Table (My_Unit).Sfile, My_ALI);

            when No_Such_Token =>

               --  First line of the block we are looking for

               if No (Ofile) then
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Parser.Field (1));
                  Ofile := Name_Find;
                  if Name_Buffer (1 .. Name_Len) /= "<no_obj>" then
                     Get_Name_String (Strip_Suffix (Ofile));
                  else
                     Get_Name_String (Strip_Suffix (Sfile));
                  end if;
                  Get_Name_String_And_Append (ALI_Suffix_Id);
                  Afile := Name_Find;

                  --  This object file is already in the table

                  if Get_Name_Table_Info (Afile) /= 0 then
                     return;
                  end if;

                  --  Allocate ALI and initialize ALI entry

                  ALIs.Increment_Last;
                  My_ALI                        := ALIs.Last;
                  ALIs.Table (My_ALI)           := Default_ALI;
                  ALIs.Table (My_ALI).Ofile     := Ofile;
                  ALIs.Table (My_ALI).Afile     := Afile;
                  Set_ALI_Id (Afile, My_ALI);
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;
      Parser.Close;
      Remove_Temp_File (Output);

      if My_ALI = No_ALI_Id then
         return;
      end if;

      --  Execute and parse result of gnat list -d -a

      Skip_Line := True;
      List (Sfile, (Dependencies_Flag, Readonly_Flag), Output);
      Parser.Open (Get_Name_String (Output));
      while not Parser.End_Of_File loop
         Parser.Get_Line;

         --  We start a new block. Stop skipping lines as this new
         --  block might be the one we are looking for.

         if Parser.Number_Of_Fields = 0 then
            null;

         elsif Parser.Number_Of_Fields = 1 then
            Skip_Line := False;

         elsif Skip_Line then
            Skip_Line := False;

         else
            Sdep.Increment_Last;
            My_Sdep := Sdep.Last;
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Parser.Field (2));
            Sdep.Table (My_Sdep).Sfile := Name_Find;
            if ALIs.Table (My_ALI).Last_Sdep = No_Sdep_Id then
               ALIs.Table (My_ALI).First_Sdep := My_Sdep;
            end if;
            ALIs.Table (My_ALI).Last_Sdep := My_Sdep;
            Get_Name_String (Strip_Directory (Sdep.Table (My_Sdep).Sfile));
            if Name_Len > 8
              and then Name_Buffer (1 .. 8) = "s-taskin"
            then
               ALIs.Table (My_ALI).Tasking := 'U';
            end if;
         end if;
      end loop;
      Parser.Close;
      Remove_Temp_File (Output);
   end Load_ALI;

   -------------------------------
   -- Load_All_Registered_Units --
   -------------------------------

   procedure Load_All_Registered_Units is
      S : File_Name_Type;
      A : ALI_Id;
      P : Partition_Id;
      F : File_Name_Type renames Part_Main_Src_Name;

   begin
      Write_Line ("procedure Partition is");
      Write_Line ("begin");
      Write_Line ("   null;");
      Write_Line ("end Partition;");
      Set_Standard_Output;
      Build (F, No_Name, (Compile_Only_Flag, Keep_Going_Flag), False);

      Load_ALI (F, A);
      if A = No_ALI_Id then
         raise Compilation_Error;
      end if;

      Set_ALI_Id (F, A);
      if Debug_Mode then
         Dump_ALI (A);
      end if;
      for J in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         Push (Sdep.Table (J).Sfile);
      end loop;

      --  Load all the ali files

      loop
         S := Pop;
         exit when S = No_File_Name;
         Load_ALI (S, A);
         if A = No_ALI_Id then
            Compile (S, (1 => Semantic_Only_Flag), False);
            Load_ALI (S, A);
            if A = No_ALI_Id then
               raise Compilation_Error;
            end if;
            P := Get_Partition_Id (ALIs.Table (A).Uname);
            if not Units.Table (ALIs.Table (A).Last_Unit).RCI
              or else P = No_Partition_Id
              or else Partitions.Table (P).To_Build
            then
               raise Compilation_Error;
            end if;
         end if;
         Set_ALI_Id (S, A);
         if Debug_Mode then
            Dump_ALI (A);
         end if;
         for J in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
            if not Is_Predefined_File (Sdep.Table (J).Sfile) then
               Push (Sdep.Table (J).Sfile);
            end if;
         end loop;
      end loop;
   end Load_All_Registered_Units;

   ---------
   -- Pop --
   ---------

   function Pop return File_Name_Type is
      Sfile : File_Name_Type;

   begin
      while Sources.First <= Sources.Last loop
         Sfile := Sources.Table (Sources.Last);
         Sources.Decrement_Last;

         --  Check that we did not yet try to find the corresponding
         --  ali file. Note that we do not use Get_ALI_Id since some
         --  ali files may have been searched and not found. In this
         --  case, we set their info field to No_ALI_Id.

         if Get_Name_Table_Info (Sfile) = 0 then
            if Verbose_Mode then
               Message ("load", Strip_Directory (Sfile));
            end if;
            return Sfile;
         end if;
      end loop;
      return No_File_Name;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Sfile : File_Name_Type) is
      S : File_Name_Type := Sfile;

   begin
      --  Deal with special predefined units

      Get_Name_String (Strip_Directory (Strip_Suffix (Sfile)));
      if Name_Len = 8 then
         if Name_Buffer (1 .. 8) = "unchconv" then
            Get_Name_String (Sfile);
            Name_Buffer (Name_Len - 11 .. Name_Len - 4) := "a-unccon";
            S := Name_Find;

         elsif Name_Buffer (1 .. 8) = "unchdeal" then
            Get_Name_String (Sfile);
            Name_Buffer (Name_Len - 11 .. Name_Len - 4) := "a-uncdea";
            S := Name_Find;
         end if;
      end if;

      --  Check that we did not yet try to find the corresponding ali
      --  file. Note that we do not use Get_ALI_Id since some ali
      --  files may have been searched and not found. In this case, we
      --  set their info field to No_ALI_Id.

      if Get_Name_Table_Info (S) = 0 then
         Sources.Append (S);
      end if;
   end Push;

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
