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

with Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Osint;       use Osint;
with Output;      use Output;
with Prj.Com;     use Prj.Com;

package body Prj.Env is

   use Ada;
   use Ada.Strings;

   type Naming_Record;
   type Naming_Ref is access Naming_Record;
   type Naming_Record is record
      Naming : Naming_Data;
      Next : Naming_Ref;
   end record;

   Default_Naming : constant Naming_Ref :=
                      new Naming_Record'(Naming => Standard_Naming_Data,
                                         Next => null);

   Current_Verbosity : Verbosity := Default;

   Gnat_Adc_Created : Boolean := False;
   Gnat_Adc_Saved   : Boolean := False;

   Gnat_Adc_File_Name : constant String := "gnat.adc" & ASCII.NUL;
   Saved_Gnat_Adc_File_Name : String (1 .. 10) := "gnat0.adc" & ASCII.NUL;
   Digit_Pos : constant := 5;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_List) return String;
   --  ??? comment needed

   function Spec_Path_Name_Of (Unit : Unit_List) return String;
   --  ??? comment needed

   ----------------------
   -- Ada_Include_Path --
   ----------------------

   function Ada_Include_Path (Ref : Reference) return String is
      Result : Unbounded.Unbounded_String;
      Seen   : Reference_List := null;

      procedure Add (Ref : in Reference);

      procedure Add (Ref : in Reference) is
         List : Reference_List := Ref.Imported_Projects;

      begin
         if Seen = null then
            Seen := new Reference_Data' (Ref, null);
         else
            declare
               Current : Reference_List := Seen;
            begin
               loop
                  if Current.Ref = Ref then
                     return;
                  end if;

                  exit when Current.Next = null;
                  Current := Current.Next;
               end loop;

               Current.Next := new Reference_Data' (Ref, null);
            end;
         end if;

         declare
            Current : String_List := Ref.Source_Dirs;

         begin
            while Current /= null loop
               if Unbounded.Length (Result) > 0 then
                  Unbounded.Append (Result, Path_Separator);
               end if;

               Unbounded.Append (Result, Current.Value.all);
               Current := Current.Next;
            end loop;
         end;

         if Ref.Modifies /= null then
            Add (Ref.Modifies);
         end if;

         while List /= null loop
            Add (List.Ref);
            List := List.Next;
         end loop;
      end Add;

   --  Start of processing for Ada_Include_Path

   begin
      if Ref.Include_Path = null then
         Add (Ref);
         Ref.Include_Path := new String'(Unbounded.To_String (Result));
      end if;

      return Ref.Include_Path.all;
   end Ada_Include_Path;

   ----------------------
   -- Ada_Objects_Path --
   ----------------------

   function Ada_Objects_Path (Ref : Reference)return String is
      Result : Unbounded.Unbounded_String;
      Seen   : Reference_List := null;

      procedure Add (Ref : in Reference);

      procedure Add (Ref : in Reference) is
         List : Reference_List := Ref.Imported_Projects;

      begin
         if Seen = null then
            Seen := new Reference_Data' (Ref, null);
         else
            declare
               Current : Reference_List := Seen;
            begin
               loop
                  if Current.Ref = Ref then
                     return;
                  end if;

                  exit when Current.Next = null;
                  Current := Current.Next;
               end loop;

               Current.Next := new Reference_Data' (Ref, null);
            end;
         end if;

         if Ref.Object_Directory /= null then
            if Unbounded.Length (Result) > 0 then
               Unbounded.Append (Result, Path_Separator);
            end if;

            Unbounded.Append (Result, Ref.Object_Directory.all);
         end if;

         if Ref.Modifies /= null then
            Add (Ref.Modifies);
         end if;

         while List /= null loop
            Add (List.Ref);
            List := List.Next;
         end loop;
      end Add;

   --  Start of processing for Ada_Objects_Path

   begin
      if Ref.Objects_Path = null then
         Add (Ref);
         Ref.Objects_Path := new String'(Unbounded.To_String (Result));
      end if;
      return Ref.Objects_Path.all;
   end Ada_Objects_Path;

   -----------------------
   -- Body_Path_Name_Of --
   -----------------------

   function Body_Path_Name_Of (Unit : Unit_List) return String is
   begin
      if Unit.File_Names (Body_Part).Path = null then
         declare
            Current_Source : String_List := Unit.Ref.Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            Unit.File_Names (Body_Part).Path :=
              Unit.File_Names (Body_Part).Name;

            while Current_Source /= null loop
               Path :=
                 Locate_Regular_File (Unit.File_Names (Body_Part).Name.all,
                                      Current_Source.Value.all);
               if Path /= null then
                  Unit.File_Names (Body_Part).Path := Path;
                  exit;
               else
                  Current_Source := Current_Source.Next;
               end if;
            end loop;
         end;
      end if;

      return Unit.File_Names (Body_Part).Path.all;
   end Body_Path_Name_Of;

   ---------------------
   -- Create_Gnat_Adc --
   ---------------------

   procedure Create_Gnat_Adc (Ref : in Reference) is
      File         : File_Descriptor;
      Current_Unit : Unit_List := First_Unit;

      First_Ref    : Reference_List;
      First_Naming : Naming_Ref := Default_Naming;

      Current_Ref    : Reference_List;
      Current_Naming : Naming_Ref;

      procedure Check_Ref (Refer : in Reference);

      procedure Check_Gnat_Adc;

      procedure Put
        (Unit_Name : in String_Access;
         File_Name : in String_Access;
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

      ---------------
      -- Check_Ref --
      ---------------

      procedure Check_Ref (Refer : in Reference) is
      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Refer.Name.all);
            Write_Str (""".");
            Write_Eol;
         end if;

         Current_Ref := First_Ref;
         while Current_Ref /= null
           and then Current_Ref.Ref /= Refer
         loop
            Current_Ref := Current_Ref.Next;
         end loop;

         if Current_Ref = null then
            First_Ref :=
              new Reference_Data' (Ref => Refer, Next => First_Ref);

            Current_Naming := First_Naming;
            while Current_Naming /= null and then
              not Same_Naming_Scheme
              (Left => Current_Naming.Naming,
               Right => Refer.Naming) loop
               Current_Naming := Current_Naming.Next;
            end loop;

            if Current_Naming = null then
               First_Naming :=
                 new Naming_Record'
                       (Naming => Refer.Naming,
                        Next   => First_Naming);

               Check_Gnat_Adc;
               Put_Line
                 (File, "pragma Source_File_Name");
               Put_Line
                 (File, "  (Spec_File_Name  => ""*" &
                  Refer.Naming.Specification_Append.all &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Refer.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                  Refer.Naming.Dot_Replacement.all &
                  """);");
               Put_Line
                 (File, "pragma Source_File_Name");
               Put_Line
                 (File, "  (Body_File_Name  => ""*" &
                  Refer.Naming.Body_Append.all &
                  """,");
               Put_Line
                 (File, "   Casing          => " &
                  Image (Refer.Naming.Casing) & ",");
               Put_Line
                 (File, "   Dot_Replacement => """ &
                  Refer.Naming.Dot_Replacement.all &
                  """);");

               if Refer.Naming.Body_Append.all /=
                    Refer.Naming.Separate_Append.all
               then
                  Put_Line
                    (File, "pragma Source_File_Name");
                  Put_Line
                    (File, "  (Subunit_File_Name  => ""*" &
                     Refer.Naming.Separate_Append.all &
                     """,");
                  Put_Line
                    (File, "   Casing          => " &
                     Image (Refer.Naming.Casing) &
                     ",");
                  Put_Line
                    (File, "   Dot_Replacement => """ &
                     Refer.Naming.Dot_Replacement.all &
                     """);");
               end if;
            end if;

            if Refer.Modifies /= null then
               Check_Ref (Refer.Modifies);
            end if;

            declare
               Current : Reference_List := Refer.Imported_Projects;

            begin
               while Current /= null loop
                  Check_Ref (Current.Ref);
                  Current := Current.Next;
               end loop;
            end;
         end if;
      end Check_Ref;

      ---------
      -- Put --
      ---------

      procedure Put
        (Unit_Name : in String_Access;
         File_Name : in String_Access;
         Unit_Kind : in Spec_Or_Body)
      is
      begin
         Check_Gnat_Adc;
         Put (File, "pragma Source_File_Name (");
         Put (File, Unit_Name.all);

         if Unit_Kind = Specification then
            Put (File, ", Spec_File_Name => """);
         else
            Put (File, ", Body_File_Name => """);
         end if;

         Put (File, File_Name.all);
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

      Check_Ref (Ref);

      while Current_Unit /= null loop
         if Current_Unit.File_Names (Specification).Needs_Pragma then
            Put (Current_Unit.Name,
                 Current_Unit.File_Names (Specification).Name,
                 Specification);
         end if;

         if Current_Unit.File_Names (Body_Part).Needs_Pragma then
            Put (Current_Unit.Name,
                 Current_Unit.File_Names (Body_Part).Name,
                 Body_Part);
         end if;

         Current_Unit := Current_Unit.Next;
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
     (Name : String;
      Ref  : Reference)
      return String
   is
      Original_Name      : String := Name;
      Extended_Spec_Name : String :=
                             Name & Ref.Naming.Specification_Append.all;
      Extended_Body_Name : String :=
                             Name & Ref.Naming.Body_Append.all;

      First   : Unit_List := First_Unit;
      Current : Unit_List;

   begin
      Canonical_Case_File_Name (Original_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);
      Canonical_Case_File_Name (Extended_Spec_Name);

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

      while First /= null
        and then First.Ref /= Ref
      loop
         First := First.Next;
      end loop;

      Current := First;
      while Current /= null and then Current.Ref = Ref loop
         if Current.File_Names (Body_Part).Name /= null then
            if Current_Verbosity = High then
               Write_Str  ("   Comparing with """);
               Write_Str  (Current.File_Names (Body_Part).Name.all);
               Write_Char ('"');
               Write_Eol;
            end if;

            if Current.File_Names (Body_Part).Name.all = Original_Name then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Original_Name;

            elsif Current.File_Names (Body_Part).Name.all =
                    Extended_Body_Name
            then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Extended_Body_Name;

            else
               if Current_Verbosity = High then
                  Write_Line ("   not good");
               end if;
            end if;

         elsif Current.File_Names (Specification).Name /= null then
            if Current_Verbosity = High then
               Write_Str  ("   Comparing with """);
               Write_Str  (Current.File_Names (Specification).Name.all);
               Write_Char ('"');
               Write_Eol;
            end if;

            if Current.File_Names (Specification).Name.all =
                 Original_Name
            then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Original_Name;

            elsif Current.File_Names (Specification).Name.all =
                    Extended_Spec_Name
            then
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

         Current := Current.Next;
      end loop;

      return "";
   end File_Name_Of_Library_Unit_Body;

   -------------------------
   -- For_All_Object_Dirs --
   -------------------------

   procedure For_All_Object_Dirs (Ref : in Reference) is
      Seen : Reference_List := null;

      procedure Add (Ref : Reference);

      ---------
      -- Add --
      ---------

      procedure Add (Ref : Reference) is
         List : Reference_List := Ref.Imported_Projects;

      begin
         if Seen = null then
            Seen := new Reference_Data' (Ref, null);
         else
            declare
               Current : Reference_List := Seen;

            begin
               loop
                  if Current.Ref = Ref then
                     return;
                  end if;

                  exit when Current.Next = null;
                  Current := Current.Next;
               end loop;

               Current.Next := new Reference_Data' (Ref, null);
            end;
         end if;

         if Ref.Object_Directory /= null then
            Action (Ref.Object_Directory.all);
         end if;

         if Ref.Modifies /= null then
            Add (Ref.Modifies);
         end if;

         while List /= null loop
            Add (List.Ref);
            List := List.Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Object_Dirs

   begin
      Add (Ref);
   end For_All_Object_Dirs;

   -------------------------
   -- For_All_Source_Dirs --
   -------------------------

   procedure For_All_Source_Dirs (Ref : in Reference) is
      Seen : Reference_List := null;

      procedure Add (Ref : Reference);

      ---------
      -- Add --
      ---------

      procedure Add (Ref : Reference) is
         List : Reference_List := Ref.Imported_Projects;

      begin
         if Seen = null then
            Seen := new Reference_Data' (Ref, null);
         else
            declare
               Current : Reference_List := Seen;

            begin
               loop
                  if Current.Ref = Ref then
                     return;
                  end if;

                  exit when Current.Next = null;
                  Current := Current.Next;
               end loop;

               Current.Next := new Reference_Data' (Ref, null);
            end;
         end if;

         declare
            Current : String_List := Ref.Source_Dirs;

         begin
            while Current /= null loop
               Action (Current.Value.all);
               Current := Current.Next;
            end loop;
         end;

         if Ref.Modifies /= null then
            Add (Ref.Modifies);
         end if;

         while List /= null loop
            Add (List.Ref);
            List := List.Next;
         end loop;
      end Add;

   --  Start of processing for For_All_Source_Dirs

   begin
      Add (Ref);
   end For_All_Source_Dirs;

   -------------------
   -- Get_Reference --
   -------------------

   procedure Get_Reference
     (Source_File_Name : String;
      Ref              : in out Reference;
      Path             : in out String_Access)
   is
   begin
      if Current_Verbosity > Default then
         Write_Str ("Getting Reference_Of (""");
         Write_Str (Source_File_Name);
         Write_Str (""") ... ");
      end if;

      declare
         Original_Name : String    := Source_File_Name;
         Unit          : Unit_List := First_Unit;

      begin
         Canonical_Case_File_Name (Original_Name);

         while Unit /= null loop
            if (Unit.File_Names (Specification).Name /= null
                  and then Unit.File_Names (Specification).Name.all =
                             Original_Name)
              or else (Unit.File_Names (Specification).Path /= null
                         and then Unit.File_Names (Specification).Path.all =
                                    Original_Name)
            then
               Ref := Unit.Ref;
               Path := Unit.File_Names (Specification).Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Specification.");
                  Write_Eol;
               end if;

               return;

            elsif (Unit.File_Names (Body_Part).Name /= null
                     and then Unit.File_Names (Body_Part).Name.all =
                                Original_Name)
              or else (Unit.File_Names (Body_Part).Path /= null
                         and then Unit.File_Names (Body_Part).Path.all =
                                    Original_Name)
            then
               Ref := Unit.Ref;
               Path := Unit.File_Names (Body_Part).Path;

               if Current_Verbosity > Default then
                  Write_Str ("Done: Body.");
                  Write_Eol;
               end if;

               return;
            end if;

            Unit := Unit.Next;
         end loop;
      end;

      Ref := null;

      if Current_Verbosity > Default then
         Write_Str ("Cannot be found.");
         Write_Eol;
      end if;
   end Get_Reference;

   ------------------------------------
   -- Path_Name_Of_Library_Unit_Body --
   ------------------------------------

   function Path_Name_Of_Library_Unit_Body
     (Name : String;
      Ref  : Reference)
      return String
   is
      Original_Name : String := Name;

      Extended_Spec_Name : String :=
                             Name & Ref.Naming.Specification_Append.all;
      Extended_Body_Name : String :=
                             Name & Ref.Naming.Body_Append.all;

      First   : Unit_List := First_Unit;
      Current : Unit_List;

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

      while First /= null
        and then First.Ref /= Ref
      loop
         First := First.Next;
      end loop;

      Current := First;
      while Current /= null and then Current.Ref = Ref loop
         if Current.File_Names (Body_Part).Name /= null then
            if Current_Verbosity = High then
               Write_Str  ("   Comparing with """);
               Write_Str  (Current.File_Names (Body_Part).Name.all);
               Write_Char ('"');
               Write_Eol;
            end if;

            if Current.File_Names (Body_Part).Name.all = Original_Name then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Body_Path_Name_Of (Current);

            elsif Current.File_Names (Body_Part).Name.all =
                    Extended_Body_Name
            then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Body_Path_Name_Of (Current);

            else
               if Current_Verbosity = High then
                  Write_Line ("   not good");
               end if;
            end if;

         elsif Current.File_Names (Specification).Name /= null then
            if Current_Verbosity = High then
               Write_Str  ("   Comparing with """);
               Write_Str  (Current.File_Names (Specification).Name.all);
               Write_Char ('"');
               Write_Eol;
            end if;

            if Current.File_Names (Specification).Name.all = Original_Name then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Spec_Path_Name_Of (Current);

            elsif Current.File_Names (Specification).Name.all =
                    Extended_Spec_Name
            then
               if Current_Verbosity = High then
                  Write_Line ("   OK");
               end if;

               return Spec_Path_Name_Of (Current);

            else
               if Current_Verbosity = High then
                  Write_Line ("   not good");
               end if;
            end if;
         end if;

         Current := Current.Next;
      end loop;

      return "";
   end Path_Name_Of_Library_Unit_Body;

   -------------------
   -- Print_Sources --
   -------------------

   procedure Print_Sources is
      Current : Unit_List;

   begin
      Write_Line ("List of Sources:");

      Current := First_Unit;
      while Current /= null loop
         Write_Str  ("   ");
         Write_Line (Current.Name.all);

         if Current.Ref = null then
            Write_Line ("   No project");
         else
            Write_Str  ("   Project: ");
            Write_Line (Current.Ref.Path_Name.all);
         end if;

         if Current.File_Names (Specification).Name /= null then
            Write_Str  ("      spec: ");
            Write_Line (Current.File_Names (Specification).Name.all);
         end if;

         if Current.File_Names (Body_Part).Name /= null then
            Write_Str  ("      body: ");
            Write_Line (Current.File_Names (Body_Part).Name.all);
         end if;

         Current := Current.Next;
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

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (To : Verbosity) is
   begin
      Current_Verbosity := To;
   end Set_Verbosity;

   -----------------------
   -- Spec_Path_Name_Of --
   -----------------------

   function Spec_Path_Name_Of (Unit : Unit_List) return String is
   begin
      if Unit.File_Names (Specification).Path = null then
         declare
            Current_Source : String_List := Unit.Ref.Sources;
            Path : GNAT.OS_Lib.String_Access;

         begin
            Unit.File_Names (Specification).Path :=
              Unit.File_Names (Specification).Name;

            while Current_Source /= null loop
               Path :=
                 Locate_Regular_File (Unit.File_Names (Body_Part).Name.all,
                                      Current_Source.Value.all);
               if Path /= null then
                  Unit.File_Names (Specification).Path := Path;
                  exit;
               else
                  Current_Source := Current_Source.Next;
               end if;
            end loop;
         end;
      end if;

      return Unit.File_Names (Specification).Path.all;
   end Spec_Path_Name_Of;

end Prj.Env;
