------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Osint;       use Osint;
with Output;      use Output;
with Prj.Com;     use Prj.Com;
with Stringt;     use Stringt;
with Table;

package body Prj.Env is

   type Naming_Id is new Nat;
   No_Naming : constant Naming_Id := 0;

   Ada_Path_Buffer : String_Access := new String (1 .. 1_000);
   Ada_Path_Length : Natural := 0;

   package Namings is new Table.Table (
     Table_Component_Type => Naming_Data,
     Table_Index_Type     => Naming_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 100,
     Table_Name           => "Prj.Env.Namings");

   Default_Naming : constant Naming_Id := Namings.First;

   Gnat_Adc_Created : Boolean := False;
   Gnat_Adc_Saved   : Boolean := False;

   Gnat_Adc_File_Name : constant String := "gnat.adc" & ASCII.NUL;
   Saved_Gnat_Adc_File_Name : String (1 .. 10) := "gnat0.adc" & ASCII.NUL;
   Digit_Pos : constant := 5;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_Id) return String;
   --  ??? comment needed

   function Spec_Path_Name_Of (Unit : Unit_Id) return String;
   --  ??? comment needed

   procedure Add_To_Path (Path : String);

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path (Project : Project_Id) return String is
      Seen   : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);

      procedure Add (Project : Project_Id) is
      begin
         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);
         else
            declare
               Current : Project_Element := Project_Lists.Table (Seen);

            begin
               loop
                  if Current.Project = Project then
                     return;
                  end if;

                  exit when Current.Next = Empty_Project_List;
                  Current := Project_Lists.Table (Current.Next);
               end loop;

               Project_Lists.Increment_Last;
               Current.Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         declare
            Data : Project_Data := Projects.Table (Project);
            List : Project_List := Data.Imported_Projects;

            Current : String_List_Id := Data.Source_Dirs;
            Source_Dir : String_Element;

         begin
            while Current /= Nil_String loop
               if Ada_Path_Length > 0 then
                  Add_To_Path (Path => (1 => Path_Separator));
               end if;

               Source_Dir := String_Elements.Table (Current);
               String_To_Name_Buffer (Source_Dir.Value);
               Add_To_Path (Name_Buffer (1 .. Name_Len));
               Current := Source_Dir.Next;
            end loop;

            if Data.Modifies /= No_Project then
               Add (Data.Modifies);
            end if;

            while List /= Empty_Project_List loop
               Add (Project_Lists.Table (List).Project);
               List := Project_Lists.Table (List).Next;
            end loop;
         end;
      end Add;

   --  Start of processing for Ada_Include_Path

   begin
      if Projects.Table (Project).Include_Path = No_String then
         Ada_Path_Length := 0;
         Add (Project);
         Start_String;
         Store_String_Chars (Ada_Path_Buffer (1 .. Ada_Path_Length));
         Projects.Table (Project).Include_Path := End_String;
      end if;

      String_To_Name_Buffer (Projects.Table (Project).Include_Path);
      return Name_Buffer (1 .. Name_Len);
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path (Project : Project_Id) return String is
      Seen   : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);

      procedure Add (Project : Project_Id) is
      begin
         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);

         else
            declare
               Current : Project_Element := Project_Lists.Table (Seen);

            begin
               loop
                  if Current.Project = Project then
                     return;
                  end if;

                  exit when Current.Next = Empty_Project_List;
                  Current := Project_Lists.Table (Current.Next);
               end loop;

               Project_Lists.Increment_Last;
               Current.Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         declare
            Data : Project_Data := Projects.Table (Project);
            List : Project_List := Data.Imported_Projects;

         begin
            if Data.Object_Directory /= No_Name then
               if Ada_Path_Length > 0 then
                  Add_To_Path (Path => (1 => Path_Separator));
               end if;

               Add_To_Path (Get_Name_String (Data.Object_Directory));
            end if;

            if Data.Modifies /= No_Project then
               Add (Data.Modifies);
            end if;

            while List /= Empty_Project_List loop
               Add (Project_Lists.Table (List).Project);
               List := Project_Lists.Table (List).Next;
            end loop;
         end;
      end Add;

   --  Start of processing for Ada_Objects_Path

   begin
      if Projects.Table (Project).Objects_Path = No_String then
         Ada_Path_Length := 0;
         Add (Project);
         Start_String;
         Store_String_Chars (Ada_Path_Buffer (1 .. Ada_Path_Length));
         Projects.Table (Project).Objects_Path := End_String;
      end if;

      String_To_Name_Buffer (Projects.Table (Project).Objects_Path);
      return Name_Buffer (1 .. Name_Len);
   end Ada_Objects_Path;

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path (Path : String) is
   begin
      if Ada_Path_Length + Path'Length > Ada_Path_Buffer'Last then
         declare
            New_Ada_Path_Buffer : constant String_Access :=
                                    new String
                                      (1 .. Ada_Path_Buffer'Last +
                                                 Ada_Path_Buffer'Last);

         begin
            New_Ada_Path_Buffer (1 .. Ada_Path_Length) :=
              Ada_Path_Buffer (1 .. Ada_Path_Length);
            Ada_Path_Buffer := New_Ada_Path_Buffer;
         end;
      end if;

      Ada_Path_Buffer
        (Ada_Path_Length + 1 .. Ada_Path_Length + Path'Length) := Path;
      Ada_Path_Length := Ada_Path_Length + Path'Length;
   end Add_To_Path;

   -----------------------
   -- Body_Path_Name_Of --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_Id) return String is
      Data : Unit_Data := Units.Table (Unit);

   begin
      if Data.File_Names (Body_Part).Path = No_Name then
         declare
            Current_Source : String_List_Id :=
              Projects.Table (Data.File_Names (Body_Part).Project).Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            Data.File_Names (Body_Part).Path :=
              Data.File_Names (Body_Part).Name;

            while Current_Source /= Nil_String loop
               String_To_Name_Buffer
                 (String_Elements.Table (Current_Source).Value);
               Path :=
                 Locate_Regular_File
                 (Namet.Get_Name_String
                  (Data.File_Names (Body_Part).Name),
                  Name_Buffer (1 .. Name_Len));

               if Path /= null then
                  Name_Len := Path'Length;
                  Name_Buffer (1 .. Name_Len) := Path.all;
                  Data.File_Names (Body_Part).Path := Name_Enter;
                  exit;

               else
                  Current_Source :=
                    String_Elements.Table (Current_Source).Next;
               end if;
            end loop;

            Units.Table (Unit) := Data;
         end;
      end if;

      return Namet.Get_Name_String (Data.File_Names (Body_Part).Path);
   end Body_Path_Name_Of;

   ---------------------
   -- Create_Gnat_Adc --
   ---------------------

   procedure Create_Gnat_Adc (Project : Project_Id) is
      File         : File_Descriptor;
      Current_Unit : Unit_Id := Units.First;

      First_Project : Project_List := Empty_Project_List;

      Current_Project : Project_List;
      Current_Naming  : Naming_Id;

      procedure Check (Project : Project_Id);

      procedure Check_Gnat_Adc;

      procedure Put
        (Unit_Name : in Name_Id;
         File_Name : in Name_Id;
         Unit_Kind : in Spec_Or_Body);

      procedure Put
        (File : in File_Descriptor;
         S    : in String);

      procedure Put_Line
        (File : in File_Descriptor;
         S    : in String);

      --------------------
      -- Check_Gnat_Adc --
      --------------------

      procedure Check_Gnat_Adc is
      begin
         if not Gnat_Adc_Created then
            declare
               Saved_File : File_Descriptor;
               Buffer     : String (1 .. 1000);
               Last_In    : Natural;
               Last_Out   : Natural;
               Dummy      : Boolean;

            begin
               File := Open_Read (Gnat_Adc_File_Name'Address,
                                  GNAT.OS_Lib.Text);

               if File /= Invalid_FD then
                  loop
                     Saved_File := Create_New_File
                       (Saved_Gnat_Adc_File_Name'Address,
                        GNAT.OS_Lib.Text);
                     exit when Saved_File /= Invalid_FD;
                     Saved_Gnat_Adc_File_Name (Digit_Pos) :=
                       Character'Succ
                       (Saved_Gnat_Adc_File_Name (Digit_Pos));
                  end loop;

                  if Current_Verbosity = High then
                     Write_Str ("Saving existing gnat.adc in gnat");
                     Write_Char (Saved_Gnat_Adc_File_Name (Digit_Pos));
                     Write_Line (".adc");
                  end if;

                  loop
                     Last_In := Read (File, Buffer'Address, Buffer'Length);
                     Last_Out := Write (Saved_File, Buffer'Address, Last_In);

                     if Last_In /= Last_Out then
                        Osint.Fail ("Disk full");
                     end if;

                     exit when Last_In /= Buffer'Length;
                  end loop;

                  Close (File);
                  Close (Saved_File);
                  Delete_File (Gnat_Adc_File_Name'Address, Dummy);
                  Gnat_Adc_Saved := True;
               end if;

               File := Create_New_File
                 (Gnat_Adc_File_Name'Address,
                  GNAT.OS_Lib.Text);

               if Gnat_Adc_Saved then
                  declare
                     Saved_File : File_Descriptor;
                     Buffer     : String (1 .. 1000);
                     Last_In    : Natural;
                     Last_Out   : Natural;

                  begin
                     Saved_File :=
                       Open_Read (Saved_Gnat_Adc_File_Name'Address,
                                  GNAT.OS_Lib.Text);
                     loop
                        Last_In := Read
                          (Saved_File, Buffer'Address, Buffer'Length);
                        Last_Out := Write
                          (File, Buffer'Address, Last_In);

                        if Last_Out /= Last_In then
                           Osint.Fail ("Disk full");
                        end if;

                        exit when Last_In /= Buffer'Length;
                     end loop;

                     Close (Saved_File);
                  end;
               end if;

               Gnat_Adc_Created := True;
            end;
         end if;
      end Check_Gnat_Adc;

      -----------
      -- Check --
      -----------

      procedure Check (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);
      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Data.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         Current_Project := First_Project;
         while Current_Project /= Empty_Project_List
           and then Project_Lists.Table (Current_Project).Project /= Project
         loop
            Current_Project := Project_Lists.Table (Current_Project).Next;
         end loop;

         if Current_Project = Empty_Project_List then
            Project_Lists.Increment_Last;
            Project_Lists.Table (Project_Lists.Last) :=
              (Project => Project, Next => First_Project);
            First_Project := Project_Lists.Last;

            Current_Naming := Default_Naming;
            while Current_Naming <= Namings.Last and then
              not Same_Naming_Scheme
              (Left => Namings.Table (Current_Naming),
               Right => Data.Naming) loop
               Current_Naming := Current_Naming + 1;
            end loop;

            if Current_Naming > Namings.Last then
               Namings.Increment_Last;
               Namings.Table (Namings.Last) := Data.Naming;

               Check_Gnat_Adc;
               Put_Line
                 (File, "pragma Source_File_Name");
               Put_Line
                 (File, "  (Spec_File_Name  => ""*" &
                  Namet.Get_Name_String (Data.Naming.Specification_Append) &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Data.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                 Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                  """);");
               Put_Line
                 (File, "pragma Source_File_Name");
               Put_Line
                 (File, "  (Body_File_Name  => ""*" &
                  Namet.Get_Name_String (Data.Naming.Body_Append) &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Data.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                  Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                  """);");

               if Data.Naming.Body_Append /= Data.Naming.Separate_Append then
                  Put_Line
                    (File, "pragma Source_File_Name");
                  Put_Line
                    (File, "  (Subunit_File_Name  => ""*" &
                     Namet.Get_Name_String (Data.Naming.Separate_Append) &
                     """,");
                  Put_Line
                    (File, "   Casing          => " &
                     Image (Data.Naming.Casing) &
                     ",");
                  Put_Line
                    (File, "   Dot_Replacement => """ &
                     Namet.Get_Name_String (Data.Naming.Dot_Replacement) &
                     """);");
               end if;
            end if;

            if Data.Modifies /= No_Project then
               Check (Data.Modifies);
            end if;

            declare
               Current : Project_List := Data.Imported_Projects;

            begin
               while Current /= Empty_Project_List loop
                  Check (Project_Lists.Table (Current).Project);
                  Current := Project_Lists.Table (Current).Next;
               end loop;
            end;
         end if;
      end Check;

      ---------
      -- Put --
      ---------

      procedure Put
        (Unit_Name : in Name_Id;
         File_Name : in Name_Id;
         Unit_Kind : in Spec_Or_Body)
      is
      begin
         Check_Gnat_Adc;
         Put (File, "pragma Source_File_Name (");
         Put (File, Namet.Get_Name_String (Unit_Name));

         if Unit_Kind = Specification then
            Put (File, ", Spec_File_Name => """);
         else
            Put (File, ", Body_File_Name => """);
         end if;

         Put (File, Namet.Get_Name_String (File_Name));
         Put_Line (File, """);");
      end Put;

      procedure Put
        (File : in File_Descriptor;
         S    : in String)
      is
         Last : Natural;

      begin
         Last := Write (File, S (S'First)'Address, S'Length);

         if Last /= S'Length then
            Osint.Fail ("Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Str (S);
         end if;
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line
        (File : in File_Descriptor;
         S    : in String)
      is
         S0   : String (1 .. S'Length + 1);
         Last : Natural;

      begin
         S0 (1 .. S'Length) := S;
         S0 (S0'Last) := ASCII.LF;
         Last := Write (File, S0'Address, S0'Length);

         if Last /= S'Length + 1 then
            Osint.Fail ("Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Line (S);
         end if;
      end Put_Line;

   --  Start of processing for Create_Gnat_Adc

   begin
      if Current_Verbosity > Default then
         Write_Str ("Creating (if necessary) gnat.adc.");
         Write_Eol;
         Write_Str ("Checking Naming Schemes.");
         Write_Eol;
      end if;

      Namings.Set_Last (Default_Naming);

      Check (Project);

      while Current_Unit <= Units.Last loop
         declare
            Unit : constant Unit_Data :=
                     Units.Table (Current_Unit);

         begin
            if Unit.File_Names (Specification).Needs_Pragma then
               Put (Unit.Name,
                    Unit.File_Names (Specification).Name,
                    Specification);
            end if;

            if Unit.File_Names (Body_Part).Needs_Pragma then
               Put (Unit.Name,
                    Unit.File_Names (Body_Part).Name,
                    Body_Part);
            end if;

            Current_Unit := Current_Unit + 1;
         end;

      end loop;

      if Gnat_Adc_Created then
         Close (File);
      end if;

      if Current_Verbosity > Default then
         Write_Line ("End of creation of gnat.adc.");
      end if;

   end Create_Gnat_Adc;

   ------------------------------------
   -- File_Name_Of_Library_Unit_Body --
   ------------------------------------

   function File_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return String
   is
      Data : constant Project_Data := Projects.Table (Project);
      Original_Name      : String := Name;

      Extended_Spec_Name : String :=
        Name & Namet.Get_Name_String (Data.Naming.Specification_Append);
      Extended_Body_Name : String :=
        Name & Namet.Get_Name_String (Data.Naming.Body_Append);

      Unit    : Unit_Data;

      The_Original_Name : Name_Id;
      The_Spec_Name : Name_Id;
      The_Body_Name : Name_Id;

   begin
      Canonical_Case_File_Name (Original_Name);
      Name_Len := Original_Name'Length;
      Name_Buffer (1 .. Name_Len) := Original_Name;
      The_Original_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Spec_Name);
      Name_Len := Extended_Spec_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Spec_Name;
      The_Spec_Name := Name_Find;

      Canonical_Case_File_Name (Extended_Body_Name);
      Name_Len := Extended_Body_Name'Length;
      Name_Buffer (1 .. Name_Len) := Extended_Body_Name;
      The_Body_Name := Name_Find;

      if Current_Verbosity = High then
         Write_Str  ("Looking for file name of """);
         Write_Str  (Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Spec Name = """);
         Write_Str  (Extended_Spec_Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Body Name = """);
         Write_Str  (Extended_Body_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      for Current in reverse Units.First .. Units.Last loop
         Unit := Units.Table (Current);

         if Unit.File_Names (Body_Part).Project = Project then
            declare
               Current_Name : constant Name_Id :=
                                Unit.File_Names (Body_Part).Name;

            begin
               if Current_Name /= No_Name then
                  if Current_Verbosity = High then
                     Write_Str  ("   Comparing with """);
                     Write_Str  (Get_Name_String (Current_Name));
                     Write_Char ('"');
                     Write_Eol;
                  end if;

                  if Current_Name = The_Original_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Original_Name;

                  elsif Current_Name = The_Body_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Extended_Body_Name;

                  else
                     if Current_Verbosity = High then
                        Write_Line ("   not good");
                     end if;
                  end if;
               end if;
            end;
         end if;

         if Units.Table (Current).File_Names (Specification).Project =
                                                                 Project
         then
            declare
               Current_Name : constant Name_Id :=
                                Unit.File_Names (Specification).Name;

            begin
               if Current_Name /= No_Name then
                  if Current_Verbosity = High then
                     Write_Str  ("   Comparing with """);
                     Write_Str  (Get_Name_String (Current_Name));
                     Write_Char ('"');
                     Write_Eol;
                  end if;

                  if Current_Name = The_Original_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Original_Name;

                  elsif Current_Name = The_Spec_Name then
                     if Current_Verbosity = High then
                        Write_Line ("   OK");
                     end if;

                     return Extended_Spec_Name;

                  else
                     if Current_Verbosity = High then
                        Write_Line ("   not good");
                     end if;
                  end if;
               end if;
            end;
         end if;

      end loop;

      return "";
   end File_Name_Of_Library_Unit_Body;

   -------------------------
   -- For_All_Object_Dirs --
   -------------------------

   procedure For_All_Object_Dirs (Project : Project_Id) is
      Seen : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);
         List : Project_List := Data.Imported_Projects;

      begin
         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);

         else
            declare
               Current : Project_List := Seen;

            begin
               loop
                  if Project_Lists.Table (Current).Project = Project then
                     return;
                  end if;

                  exit when Project_Lists.Table (Current).Next =
                    Empty_Project_List;
                  Current := Project_Lists.Table (Current).Next;
               end loop;

               Project_Lists.Increment_Last;
               Project_Lists.Table (Current).Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         if Data.Object_Directory /= No_Name then
            Get_Name_String (Data.Object_Directory);
            Action (Name_Buffer (1 .. Name_Len));
         end if;

         if Data.Modifies /= No_Project then
            Add (Data.Modifies);
         end if;

         while List /= Empty_Project_List loop
            Add (Project_Lists.Table (List).Project);
            List := Project_Lists.Table (List).Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Object_Dirs

   begin
      Add (Project);
   end For_All_Object_Dirs;

   -------------------------
   -- For_All_Source_Dirs --
   -------------------------

   procedure For_All_Source_Dirs (Project : Project_Id) is
      Seen : Project_List := Empty_Project_List;

      procedure Add (Project : Project_Id);

      ---------
      -- Add --
      ---------

      procedure Add (Project : Project_Id) is
         Data : constant Project_Data := Projects.Table (Project);
         List : Project_List := Data.Imported_Projects;

      begin
         if Seen = Empty_Project_List then
            Project_Lists.Increment_Last;
            Seen := Project_Lists.Last;
            Project_Lists.Table (Seen) :=
              (Project => Project, Next => Empty_Project_List);
         else
            declare
               Current : Project_List := Seen;

            begin
               loop
                  if Project_Lists.Table (Current).Project = Project then
                     return;
                  end if;

                  exit when Project_Lists.Table (Current).Next =
                    Empty_Project_List;
                  Current := Project_Lists.Table (Current).Next;
               end loop;

               Project_Lists.Increment_Last;
               Project_Lists.Table (Current).Next := Project_Lists.Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => Project, Next => Empty_Project_List);
            end;
         end if;

         declare
            Current : String_List_Id := Data.Source_Dirs;
            The_String : String_Element;
         begin
            while Current /= Nil_String loop
               The_String := String_Elements.Table (Current);
               String_To_Name_Buffer (The_String.Value);
               Action (Name_Buffer (1 .. Name_Len));
               Current := The_String.Next;
            end loop;
         end;

         if Data.Modifies /= No_Project then
            Add (Data.Modifies);
         end if;

         while List /= Empty_Project_List loop
            Add (Project_Lists.Table (List).Project);
            List := Project_Lists.Table (List).Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Source_Dirs

   begin
      Add (Project);
   end For_All_Source_Dirs;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Source_File_Name : String;
      Project          : out Project_Id;
      Path             : out Name_Id)
   is
   begin
      if Current_Verbosity > Default then
         Write_Str ("Getting Reference_Of (""");
         Write_Str (Source_File_Name);
         Write_Str (""") ... ");
      end if;

      declare
         Original_Name : String := Source_File_Name;
         Unit          : Unit_Data;

      begin
         Canonical_Case_File_Name (Original_Name);

         for Id in Units.First .. Units.Last loop
            Unit := Units.Table (Id);

            if (Unit.File_Names (Specification).Name /= No_Name
                 and then
                   Namet.Get_Name_String
                     (Unit.File_Names (Specification).Name) = Original_Name)
              or else (Unit.File_Names (Specification).Path /= No_Name
                         and then
                           Namet.Get_Name_String
                           (Unit.File_Names (Specification).Path) =
                                                              Original_Name)
            then
               Project := Unit.File_Names (Specification).Project;
               Path := Unit.File_Names (Specification).Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Specification.");
                  Write_Eol;
               end if;

               return;

            elsif (Unit.File_Names (Body_Part).Name /= No_Name
                    and then
                      Namet.Get_Name_String
                        (Unit.File_Names (Body_Part).Name) = Original_Name)
              or else (Unit.File_Names (Body_Part).Path /= No_Name
                         and then Namet.Get_Name_String
                                    (Unit.File_Names (Body_Part).Path) =
                                                             Original_Name)
            then
               Project := Unit.File_Names (Body_Part).Project;
               Path := Unit.File_Names (Body_Part).Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Body.");
                  Write_Eol;
               end if;

               return;
            end if;

         end loop;
      end;

      Project := No_Project;
      Path    := No_Name;

      if Current_Verbosity > Default then
         Write_Str ("Cannot be found.");
         Write_Eol;
      end if;
   end Get_Reference;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Namings.Increment_Last;
      Namings.Table (Namings.Last) := Standard_Naming_Data;
   end Initialize;

   ------------------------------------
   -- Path_Name_Of_Library_Unit_Body --
   ------------------------------------

   function Path_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return String
   is
      Data : constant Project_Data := Projects.Table (Project);
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
        Name & Namet.Get_Name_String (Data.Naming.Specification_Append);
      Extended_Body_Name : String :=
        Name & Namet.Get_Name_String (Data.Naming.Body_Append);

      First   : Unit_Id := Units.First;
      Current : Unit_Id;
      Unit    : Unit_Data;

   begin
      Canonical_Case_File_Name (Original_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);

      if Current_Verbosity = High then
         Write_Str  ("Looking for path name of """);
         Write_Str  (Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Spec Name = """);
         Write_Str  (Extended_Spec_Name);
         Write_Char ('"');
         Write_Eol;
         Write_Str  ("   Extended Body Name = """);
         Write_Str  (Extended_Body_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      while First <= Units.Last
        and then Units.Table (First).File_Names (Body_Part).Project /= Project
      loop
         First := First + 1;
      end loop;

      Current := First;
      while Current <= Units.Last loop
         Unit := Units.Table (Current);

         if Unit.File_Names (Body_Part).Project = Project
           and then Unit.File_Names (Body_Part).Name /= No_Name
         then
            declare
               Current_Name : constant String :=
                 Namet.Get_Name_String (Unit.File_Names (Body_Part).Name);
            begin
               if Current_Verbosity = High then
                  Write_Str  ("   Comparing with """);
                  Write_Str  (Current_Name);
                  Write_Char ('"');
                  Write_Eol;
               end if;

               if Current_Name = Original_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Body_Path_Name_Of (Current);

               elsif Current_Name = Extended_Body_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Body_Path_Name_Of (Current);

               else
                  if Current_Verbosity = High then
                     Write_Line ("   not good");
                  end if;
               end if;
            end;

         elsif Unit.File_Names (Specification).Name /= No_Name then
            declare
               Current_Name : constant String :=
                                Namet.Get_Name_String
                                  (Unit.File_Names (Specification).Name);

            begin
               if Current_Verbosity = High then
                  Write_Str  ("   Comparing with """);
                  Write_Str  (Current_Name);
                  Write_Char ('"');
                  Write_Eol;
               end if;

               if Current_Name = Original_Name then
                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Spec_Path_Name_Of (Current);

               elsif Current_Name = Extended_Spec_Name then

                  if Current_Verbosity = High then
                     Write_Line ("   OK");
                  end if;

                  return Spec_Path_Name_Of (Current);

               else
                  if Current_Verbosity = High then
                     Write_Line ("   not good");
                  end if;
               end if;
            end;
         end if;

      end loop;

      return "";
   end Path_Name_Of_Library_Unit_Body;

   -------------------
   -- Print_Sources --
   -------------------

   procedure Print_Sources is
      Unit : Unit_Data;

   begin
      Write_Line ("List of Sources:");

      for Id in Units.First .. Units.Last loop
         Unit := Units.Table (Id);
         Write_Str  ("   ");
         Write_Line (Namet.Get_Name_String (Unit.Name));

         if Unit.File_Names (Specification).Name /= No_Name then
            if Unit.File_Names (Specification).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Projects.Table
                   (Unit.File_Names (Specification).Project).Path_Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      spec: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Specification).Name));
         end if;

         if Unit.File_Names (Body_Part).Name /= No_Name then
            if Unit.File_Names (Body_Part).Project = No_Project then
               Write_Line ("   No project");

            else
               Write_Str  ("   Project: ");
               Get_Name_String
                 (Projects.Table
                   (Unit.File_Names (Body_Part).Project).Path_Name);
               Write_Line (Name_Buffer (1 .. Name_Len));
            end if;

            Write_Str  ("      body: ");
            Write_Line
              (Namet.Get_Name_String
               (Unit.File_Names (Body_Part).Name));
         end if;

      end loop;

      Write_Line ("end of List of Sources.");
   end Print_Sources;

   ----------------------
   -- Restore_Gnat_Adc --
   ----------------------

   procedure Restore_Gnat_Adc is
      File       : File_Descriptor;
      Saved_File : File_Descriptor;
      Buffer     : String (1 .. 1000);
      Last_In    : Natural;
      Last_Out   : Natural;
      Dummy      : Boolean;

   begin
      if Gnat_Adc_Created then
         Delete_File (Gnat_Adc_File_Name'Address, Dummy);
         Gnat_Adc_Created := False;

         if Gnat_Adc_Saved then
            Saved_File := Open_Read (Saved_Gnat_Adc_File_Name'Address,
                                     GNAT.OS_Lib.Text);
            File := Create_New_File (Gnat_Adc_File_Name'Address,
                                     GNAT.OS_Lib.Text);
            loop
               Last_In := Read (Saved_File, Buffer'Address, Buffer'Length);
               Last_Out := Write (File, Buffer'Address, Last_In);

               if Last_Out /= Last_In then
                  Osint.Fail ("Disk full");
               end if;

               exit when Last_In /= Buffer'Length;
            end loop;

            Close (File);
            Close (Saved_File);
            Delete_File (Saved_Gnat_Adc_File_Name'Address, Dummy);
            Gnat_Adc_Saved := False;
         end if;
      end if;
   end Restore_Gnat_Adc;

   -----------------------
   -- Spec_Path_Name_Of --
   -----------------------

   function Spec_Path_Name_Of (Unit : Unit_Id) return String is
      Data : Unit_Data := Units.Table (Unit);

   begin
      if Data.File_Names (Specification).Path = No_Name then
         declare
            Current_Source : String_List_Id :=
              Projects.Table (Data.File_Names (Specification).Project).Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            Data.File_Names (Specification).Path :=
              Data.File_Names (Specification).Name;

            while Current_Source /= Nil_String loop
               String_To_Name_Buffer
                 (String_Elements.Table (Current_Source).Value);
               Path := Locate_Regular_File
                 (Namet.Get_Name_String
                  (Data.File_Names (Specification).Name),
                  Name_Buffer (1 .. Name_Len));

               if Path /= null then
                  Name_Len := Path'Length;
                  Name_Buffer (1 .. Name_Len) := Path.all;
                  Data.File_Names (Specification).Path := Name_Enter;
                  exit;
               else
                  Current_Source :=
                    String_Elements.Table (Current_Source).Next;
               end if;
            end loop;

            Units.Table (Unit) := Data;
         end;
      end if;

      return Namet.Get_Name_String (Data.File_Names (Specification).Path);
   end Spec_Path_Name_Of;

end Prj.Env;
