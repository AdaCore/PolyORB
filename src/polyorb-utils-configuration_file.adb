------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . U T I L S . C O N F I G U R A T I O N _ F I L E      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with GNAT.OS_Lib;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Utils.Configuration_File is

   use Ada.Text_IO;
   use PolyORB.Log;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.configuration_file");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   use Configuration_Table;

   Local_Configuration_Table : Table_Instance;

   type Section_Entry_Props is record
      Name : PolyORB.Utils.Strings.String_Ptr;
      Val  : PolyORB.Utils.Strings.String_Ptr;
   end record;

   package Section_Entries is new PolyORB.Utils.Chained_Lists
     (Section_Entry_Props);

   type Conf_Entry is record
      Section : PolyORB.Utils.Strings.String_Ptr;
      Entries : Section_Entries.List;
   end record;

   package Sections is new PolyORB.Utils.Chained_Lists (Conf_Entry);
   Sections_List : Sections.List;

   procedure Reset_Sections_List;
   --  Reset the chained list built from the configuration table to
   --  ease displaying and generating configuration files.

   procedure Make_Global_List (K : String; V : String_Ptr);

   procedure Reset_Table
     (Table : in out Configuration_Table.Table_Instance);
   --  Reset configuration table

   --------------------
   -- Make_Global_Key --
   ---------------------

   function Make_Global_Key (Section, Key : String) return String;

   function Make_Global_Key (Section, Key : String) return String is
   begin
      return "[" & Section & "]" & Key;
   end Make_Global_Key;

   -----------------
   -- Reset_Table --
   -----------------

   procedure Reset_Table
     (Table : in out Configuration_Table.Table_Instance)
   is
      It : Iterator := First (Table);
   begin
      --  Deallocate elements from the table

      while not Last (It) loop
         declare
            V : String_Ptr := Value (It);
         begin
            Free (V);
            Next (It);
         end;
      end loop;

      --  Reset the configuration table

      Finalize (Table);
      Initialize (Table);
   end Reset_Table;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Reset_Table (Local_Configuration_Table);
      Reset_Sections_List;
   end Reset;

   ------------------------------
   -- Load_Configuration_Table --
   ------------------------------

   procedure Load_Configuration_Table
     (Configuration_Filename : String;
      Is_Default             : Boolean;
      Table                  : in out Configuration_Table.Table_Instance)
   is
      Current_Section : String_Ptr := null;
      Current_Line    : Integer    := 0;

      procedure Set_Current_Section (S : String);
      --  Enter a new section named S

      procedure Set_Current_Section (S : String) is
      begin
         Free (Current_Section);
         Current_Section := +S;
      end Set_Current_Section;

      Conf_File : File_Type;

      Line : String (1 .. 1_024);
      Last : Integer;

      use PolyORB.Utils;

   begin
      --  Reset the table and the sections list

      Reset_Table (Table);
      Reset_Sections_List;

      if not GNAT.OS_Lib.Is_Regular_File (Configuration_Filename) then
         if not Is_Default then
            O (Configuration_Filename & " is not a regular file", Error);
         end if;
         return;
      end if;

      pragma Debug (C, O ("Loading configuration from "
                       & Configuration_Filename));

      Open (Conf_File, In_File, Configuration_Filename);

      while not End_Of_File (Conf_File) loop
         Get_Line (Conf_File, Line, Last);
         Current_Line := Current_Line + 1;

         if Last - Line'First >= 0 then
            case Line (Line'First) is
               when '#' =>
                  null;

               when '[' =>
                  declare
                     Bra : constant Integer := Line'First;
                     Ket : constant Integer :=
                        Find (Line (Line'First .. Last), Bra, ']');
                  begin
                     if False
                       or else Ket > Last
                       or else Ket = Bra + 1
                       or else Ket /= Last
                     then
                        O ("Syntax error on line" &
                           Integer'Image (Current_Line) &
                           ": " &
                           Line (Line'First .. Last));
                        raise Constraint_Error;
                     end if;

                     Set_Current_Section (Line (Bra + 1 .. Ket - 1));
                  end;

               when others =>
                  declare
                     Eq : constant Integer :=
                        Find (Line (Line'First .. Last), Line'First, '=');
                  begin
                     if Current_Section = null then
                        O ("Assignment out of any section on line" &
                           Current_Line'Img & ": "
                           & Line (Line'First .. Last), Error);
                        raise Constraint_Error;
                     end if;

                     if Eq not in Line'First + 1 .. Last then
                        O ("Syntax error on line" &
                           Current_Line'Img & ": "
                           & Line (Line'First .. Last), Error);
                        raise Constraint_Error;
                     end if;

                     declare
                        K : constant String :=
                              Make_Global_Key
                                (Section => Current_Section.all,
                                 Key     => Line (Line'First .. Eq - 1));
                        V : String_Ptr      :=
                              Configuration_Table.Lookup (Table, K, null);
                     begin
                        if V /= null then
                           Free (V);
                        end if;
                        Configuration_Table.Insert
                          (Table, K, +Line (Eq + 1 .. Last));
                     end;
                  end;
            end case;
         end if;
      end loop;

      Free (Current_Section);
      Close (Conf_File);
   end Load_Configuration_Table;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf (Configuration_Filename, Section, Key, Value : String) is
      pragma Unreferenced (Configuration_Filename);

      V : constant String_Ptr :=
        Lookup (Local_Configuration_Table,
                Make_Global_Key (Section, Key), null);
   begin
      if V /= null then
         --  Remove the entry from the table

         Delete (Local_Configuration_Table,
                 Make_Global_Key (Section, Key));
      end if;

      Insert (Local_Configuration_Table,
              Make_Global_Key (Section, Key), new String'(Value));
   end Set_Conf;

   -------------------------
   -- Reset_Sections_List --
   -------------------------

   procedure Reset_Sections_List is
      Iter : Sections.Iterator := Sections.First (Sections_List);
   begin
      while not Sections.Last (Iter) loop
         declare
            S : String_Ptr := Sections.Value (Iter).Section;
         begin
            Free (S);
         end;

         declare
            Iter2 : Section_Entries.Iterator :=
              Section_Entries.First (Sections.Value (Iter).Entries);
         begin
            while not Section_Entries.Last (Iter2) loop
               declare
                  N : String_Ptr := Section_Entries.Value (Iter2).Name;
                  V : String_Ptr := Section_Entries.Value (Iter2).Val;
               begin
                  Free (V);
                  Free (N);
                  Section_Entries.Next (Iter2);
               end;
            end loop;
         end;
         Section_Entries.Deallocate (Sections.Value (Iter).Entries);

         Sections.Next (Iter);
      end loop;

      Sections.Deallocate (Sections_List);
   end Reset_Sections_List;

   ----------------------
   -- Make_Global_List --
   ----------------------

   procedure Make_Global_List (K : String; V : String_Ptr) is

      Iter            : Sections.Iterator :=
        Sections.First (Sections_List);
      Section_Found   : Boolean           := False;
      Entry_Found     : Boolean           := False;
      Current_Section : String_Ptr        := null;

      procedure Set_Current_Section (S : String);
      --  Enter a new section named S

      procedure Set_Current_Section (S : String) is
      begin
         Free (Current_Section);
         Current_Section := +S;
      end Set_Current_Section;

   begin
      --  For each section we build a list of entries

      if K (K'First) = '[' then
         declare
            Bra : constant Integer := K'First;
            Ket : constant Integer := Find (K (K'First .. K'Last), Bra, ']');
         begin
            if False or else Ket > K'Last or else Ket = Bra + 1 then

               O ("Syntax error : " & K (K'First .. K'Last));
               raise Constraint_Error;
            end if;

            Set_Current_Section (K (Bra + 1 .. Ket - 1));

            --  Lookup the current section in the entries

            Section_Found := False;
            Entry_Found   := False;

            while not Sections.Last (Iter) loop
               if Sections.Value (Iter).Section.all =
                  Current_Section.all
               then
                  Section_Found := True;

                  --  Lookup if the entry is already present, if yes rewrite it

                  declare
                     Iter2 : Section_Entries.Iterator :=
                        Section_Entries.First (Sections.Value (Iter).Entries);
                  begin
                     while not Section_Entries.Last (Iter2) loop
                        if Section_Entries.Value (Iter2).Name.all =
                           K (Ket + 1 .. K'Last)
                        then
                           --  Rewrite

                           Section_Entries.Value (Iter2).Val :=
                             new String'(V.all);

                           Entry_Found := True;
                        end if;
                        Section_Entries.Next (Iter2);
                     end loop;

                     --  The section is found but not the entry

                     if not Entry_Found then
                        Section_Entries.Append
                          (Sections.Value (Iter).Entries,
                           Section_Entry_Props'
                          (Name => new String'(K (Ket + 1 .. K'Last)),
                           Val  => new String'(V.all)));
                     end if;
                  end;
               end if;
               Sections.Next (Iter);
            end loop;

            --  The section is not found, add a new section
            --  in the chained list

            if not Section_Found then
               declare
                  Entries_List : Section_Entries.List;
               begin
                  Section_Entries.Append
                    (Entries_List,
                     Section_Entry_Props'
                    (Name => new String'(K (Ket + 1 .. K'Last)),
                     Val  => new String'(V.all)));

                  Sections.Append
                    (Sections_List,
                     Conf_Entry'
                    (Section => new String'(Current_Section.all),
                     Entries => Entries_List));
               end;
            end if;
         end;
      end if;
   end Make_Global_List;

   -------------
   -- Display --
   -------------

   procedure Display is
      It : Iterator := First (Local_Configuration_Table);
   begin
      Reset_Sections_List;

      while not Last (It) loop
         Make_Global_List (K => Key (It), V => Value (It));
         Next (It);
      end loop;

      declare
         Iter : Sections.Iterator := Sections.First (Sections_List);
      begin
         while not Sections.Last (Iter) loop
            Put_Line ("[" & Sections.Value (Iter).Section.all & "]");
            declare
               Iter2 : Section_Entries.Iterator :=
                  Section_Entries.First (Sections.Value (Iter).Entries);
            begin
               while not Section_Entries.Last (Iter2) loop
                  Put_Line
                    (Section_Entries.Value (Iter2).Name.all &
                     "=" &
                     Section_Entries.Value (Iter2).Val.all);
                  Section_Entries.Next (Iter2);
               end loop;
            end;
            Sections.Next (Iter);
         end loop;
      end;
   end Display;

   ---------------------------------
   -- Generate_Configuration_File --
   ---------------------------------

   procedure Generate_Configuration_File (Configuration_Filename : String) is
      Fd : File_Type;
      It : Iterator := First (Local_Configuration_Table);
   begin
      Reset_Sections_List;
      Create (Fd, Out_File, Configuration_Filename, Configuration_Filename);

      while not Last (It) loop
         Make_Global_List (K => Key (It), V => Value (It));
         Next (It);
      end loop;

      declare
         Iter : Sections.Iterator := Sections.First (Sections_List);
      begin
         while not Sections.Last (Iter) loop
            Put_Line (Fd, "[" & Sections.Value (Iter).Section.all & "]");
            declare
               Iter2 : Section_Entries.Iterator :=
                  Section_Entries.First (Sections.Value (Iter).Entries);
            begin
               while not Section_Entries.Last (Iter2) loop
                  Put_Line
                    (Fd,
                     Section_Entries.Value (Iter2).Name.all &
                     "=" &
                     Section_Entries.Value (Iter2).Val.all);
                  Section_Entries.Next (Iter2);
               end loop;
            end;
            Sections.Next (Iter);
         end loop;
      end;
      Close (Fd);
   end Generate_Configuration_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Initialize (Local_Configuration_Table);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
     (Name      => +"utils.configuration_file",
      Conflicts => Empty,
      Depends   => Empty,
      Provides  => Empty,
      Implicit  => True,
      Init      => Initialize'Access,
      Shutdown  => null));
end PolyORB.Utils.Configuration_File;
