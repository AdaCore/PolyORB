------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          G E N _ C O D E S E T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Generate a code sets data packages from the OSF/OpenGroup code set
--  registry file from an Open Group code set registry file (version 1.2).

--  The latest code set registry file can be downloaded from the Open Group
--  FTP server at ftp://ftp.opengroup.org/pub/code_set_registry/

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with GNAT.Table;

procedure Gen_Codeset is

   package ATIO renames Ada.Text_IO;

   Output : Ada.Streams.Stream_IO.File_Type;

   procedure Put (S : String;  Width : Integer := 0);
   generic
      type T is range <>;
   procedure Integer_Put (I : T; Width : Integer := 0);
   procedure Put (C : Character);
   procedure Put_Line (S : String);
   procedure New_Line;
   --  Same as ATIO.*, but reimplemented on top of Stream_IO and
   --  always using UNIX-style line terminators.

   type Mode_Type is (Description, Compatibility);

   type Code_Set_Id is range 0 .. 2**32 - 1;

   type Character_Set_Id is range 0 .. 2**16 - 1;

   package Description_Table is
     new GNAT.Table (Character, Natural, 1, 1024, 1024);

   package Character_Sets_Table is
     new GNAT.Table (Character_Set_Id, Natural, 1, 1024, 1024);

   type Code_Set_Info is record
      Code_Set            : Code_Set_Id;
      Description_First   : Positive;
      Description_Last    : Natural;
      Character_Set_First : Positive;
      Character_Set_Last  : Natural;
   end record;

   package Code_Set_Table is
     new GNAT.Table (Code_Set_Info, Natural, 1, 1024, 1024);

   procedure Add_Description
     (Description :     String;
      First       : out Positive;
      Last        : out Natural);
   --  Copy description string into descriptions table, and return
   --  the first and last indices of copied string.

   procedure Process_Code_Set;
   --  Process one section of code set registry file.

   procedure Compact_Character_Sets_Table;
   --  Compress contents of Character_Sets_Table.

   procedure Generate_Description_Data_Module;

   procedure Generate_Compatibility_Data_Module;

   procedure Put (Buffer : out String; Value : Code_Set_Id);
   procedure Put (Buffer : out String; Value : Character_Set_Id);

   Pkg_Name : String renames Ada.Command_Line.Argument (2);

   Mode     : Mode_Type;

   Line  : String (1 .. 1024);
   First : constant Positive := Line'First;
   Last  : Natural := Line'First - 1;

   Short_Description : constant String := "Short Description";
   Registered_Value  : constant String := "Registered Value";
   Character_Set_Ids : constant String := "Character Set ID(s)";

   ---------------------
   -- Add_Description --
   ---------------------

   procedure Add_Description (Description : String;
                              First       :    out Positive;
                              Last        :    out Natural)
   is
   begin
      First := Description_Table.Last + 1;

      for J in Description'Range loop
         Description_Table.Append (Description (J));
      end loop;

      Last := Description_Table.Last;
   end Add_Description;

   ----------------------------------
   -- Compact_Character_Sets_Table --
   ----------------------------------

   procedure Compact_Character_Sets_Table is

      package Aux_Table is
        new GNAT.Table (Character_Set_Id, Natural, 1, 1024, 1024);

      function Find (First : Positive; Last : Natural) return Natural;
      --  Return first index of sequence of characters sets in Aux_Table
      --  what equal of sequence Character_Sets_Talbe (First .. Last).

      ----------
      -- Find --
      ----------

      function Find (First : Positive; Last : Natural) return Natural is
         Length : constant Natural := Last - First + 1;
         Found  : Boolean          := False;
      begin
         for J in Aux_Table.First .. Aux_Table.Last - Length + 1 loop
            if Aux_Table.Table (J) = Character_Sets_Table.Table (First) then
               Found := True;
               for K in First + 1 .. Last loop
                  if Aux_Table.Table (J + K - First)
                       /= Character_Sets_Table.Table (K)
                  then
                     Found := False;
                     exit;
                  end if;
               end loop;

               if Found then
                  return J;
               end if;
            end if;
         end loop;

         return 0;
      end Find;

   begin
      for J in Code_Set_Table.First .. Code_Set_Table.Last loop
         declare
            Info   : Code_Set_Info renames Code_Set_Table.Table (J);
            Length : constant Natural :=
                       Info.Character_Set_Last - Info.Character_Set_First + 1;
            Index  : constant Natural :=
                       Find
                         (Info.Character_Set_First, Info.Character_Set_Last);
            First  : constant Natural := Aux_Table.Last + 1;
         begin
            if Index = 0 then
               for J in Info.Character_Set_First .. Info.Character_Set_Last
               loop
                  Aux_Table.Append (Character_Sets_Table.Table (J));
               end loop;

               Info.Character_Set_First := First;
               Info.Character_Set_Last  := First + Length - 1;

            else
               Info.Character_Set_First := Index;
               Info.Character_Set_Last  := Index + Length - 1;
            end if;
         end;
      end loop;

      for J in Aux_Table.First .. Aux_Table.Last loop
         Character_Sets_Table.Set_Item (J, Aux_Table.Table (J));
      end loop;

      Character_Sets_Table.Set_Last (Aux_Table.Last);

      Aux_Table.Free;
   end Compact_Character_Sets_Table;

   ----------------------------------------
   -- Generate_Compatibility_Data_Module --
   ----------------------------------------

   procedure Generate_Compatibility_Data_Module is
      procedure Put is new Integer_Put (Integer);
   begin
      Put_Line ("--  AUTOMATICALLY GENERATED, DO NOT EDIT!");
      New_Line;

      --  Disable style checks (N), and set maximum line length to the largest
      --  allowed value (M32766).

      Put_Line ("pragma Style_Checks (""NM32766"");");
      Put_Line ("private package " & Pkg_Name & " is");
      New_Line;

      Put_Line
       ("   Info : constant array (Positive range <>)"
          & " of Code_Set_Info_Record :=");
      Put ("     (");

      for J in 1 .. Code_Set_Table.Last loop
         declare
            Info : Code_Set_Info renames Code_Set_Table.Table (J);
            Buf  : String (1 .. 13);

            procedure Put_Description;
            --  Output Ada comment with description of current entry

            procedure Put_Description is
            begin
               Put (" --  ");
               for J in Info.Description_First .. Info.Description_Last loop
                  Put (Description_Table.Table (J));
               end loop;
               New_Line;
            end Put_Description;

         begin
            Put (Buf, Info.Code_Set);

            Put ('(');
            Put (Buf (2 .. 13));
            Put (',');
            Put (Info.Character_Set_First, 3);
            Put (',');
            Put (Info.Character_Set_Last, 3);
            Put (')');

            if J /= Code_Set_Table.Last then
               Put (", ");
               Put_Description;
               Put ("      ");

            else
               Put (");");
               Put_Description;
            end if;
         end;
      end loop;
      New_Line;

      Put_Line ("   Character_Sets : constant Character_Set_Id_Array :=");
      Put ("     (");

      for J in 1 .. Character_Sets_Table.Last loop
         declare
            Buf : String (1 .. 9);
         begin
            Put (Buf, Character_Sets_Table.Table (J));
            Put (Buf (2 .. 9));
         end;

         if J /= Character_Sets_Table.Last then
            Put (",");
            if J mod 7 = 0 then
               New_Line;
               Put ("      ");
            else
               Put (' ');
            end if;
         end if;
      end loop;
      Put_Line (");");
      New_Line;

      Put ("end " & Pkg_Name & ";");
   end Generate_Compatibility_Data_Module;

   --------------------------------------
   -- Generate_Description_Data_Module --
   --------------------------------------

   procedure Generate_Description_Data_Module is
      procedure Put is new Integer_Put (Integer);
   begin
      Put_Line ("--  AUTOMATICALLY GENERATED, DO NOT EDIT!");
      Put_Line ("package " & Pkg_Name & " is");
      New_Line;

      Put_Line ("   type Info_Record is record");
      Put_Line ("      Code_Set : Code_Set_Id;");
      Put_Line ("      First    : Positive;");
      Put_Line ("      Last     : Natural;");
      Put_Line ("   end record;");
      New_Line;

      Put_Line ("   Info : constant array (Positive range <>) of Info_Record");
      Put ("     := (");

      for J in 1 .. Code_Set_Table.Last loop
         declare
            Info : Code_Set_Info renames Code_Set_Table.Table (J);
            Buf  : String (1 .. 13);
         begin
            Put (Buf, Info.Code_Set);

            Put ('(');
            Put (Buf (2 .. 13));
            Put (',');
            Put (Info.Description_First, 5);
            Put (',');
            Put (Info.Description_Last, 5);
            Put (')');
         end;

         if J /= Code_Set_Table.Last then
            Put (',');
            New_Line;
            Put ("         ");
         else
            Put_Line (");");
         end if;
      end loop;
      New_Line;

      Put_Line ("   Description : constant String");
      Put ("     := """);
      for J in 1 .. Description_Table.Last loop
         Put (Description_Table.Table (J));
         if J = 68 or else (J > 68 and then (J - 68) mod 64 = 0) then
            Put ('"');
            New_Line;
            Put ("          & """);
         end if;
      end loop;
      Put (""";");
      New_Line;

      Put ("end " & Pkg_Name & ";");
   end Generate_Description_Data_Module;

   -----------------
   -- Integer_Put --
   -----------------

   procedure Integer_Put (I : T; Width : Integer := 0) is
      Img   : constant String := T'Image (I);
      First : Integer := Img'First;
   begin
      if Img (First) = ' ' then
         First := First + 1;
      end if;
      Put (Img (First .. Img'Last), Width);
   end Integer_Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.LF);
   end New_Line;

   ----------------------
   -- Process_Code_Set --
   ----------------------

   procedure Process_Code_Set is
      Description_First   : Positive    := 1;
      Description_Last    : Natural     := 0;
      Code_Set            : Code_Set_Id := 0;
      Character_Set_First : constant Positive := Character_Sets_Table.Last + 1;
      Character_Set_Last  : Natural     := 0;
      Length              : Natural;
   begin
      for J in 1 .. 4 loop
         ATIO.Get_Line (Line, Last);
         Length := Last - Line'First + 1;

         if Length - Line'First + 1 > Short_Description'Length + 2
           and then Line (First .. First + Short_Description'Length - 1)
                      = Short_Description
         then
            Add_Description
             (Line (First + Short_Description'Length + 1 .. Last),
              Description_First,
              Description_Last);

         elsif Length > Registered_Value'Length + 2
           and then Line (First .. First + Registered_Value'Length - 1)
                      = Registered_Value
         then
            declare
               Image : constant String
                 := "16#" & Line (Registered_Value'Last + 4 .. Last) & '#';
            begin
               Code_Set := Code_Set_Id'Value (Image);
            end;

         elsif Length > Character_Set_Ids'Length + 2
           and then Line (First .. First + Character_Set_Ids'Length - 1)
                      = Character_Set_Ids
         then
            declare
               First : Positive := Character_Set_Ids'Last + 4;
            begin
               while First < Last loop
                  declare
                     Image : constant String
                       := "16#" & Line (First .. First + 3) & '#';
                  begin
                     Character_Sets_Table.Append
                      (Character_Set_Id'Value (Image));
                     Character_Set_Last := Character_Sets_Table.Last;
                  end;
                  First := First + 7;
               end loop;
            end;

         else
            null; -- XXX
         end if;
      end loop;

      Code_Set_Table.Append
       ((Code_Set,
         Description_First,
         Description_Last,
         Character_Set_First,
         Character_Set_Last));
   end Process_Code_Set;

   ---------
   -- Put --
   ---------

   procedure Put (S : String; Width : Integer := 0) is
      use Ada.Streams;
      Len : Integer := Width;

   begin
      if S'Length > Len then
         Len := S'Length;
      end if;
      declare
         SS : aliased String (1 .. Len) := (others => ' ');
         subtype SEA is Stream_Element_Array
                          (1 .. Stream_Element_Offset (Len));
         Bytes : SEA;
         for Bytes'Address use SS'Address;
         pragma Import (Ada, Bytes);
      begin
         SS (Len - S'Length + 1 .. Len) := S;
         Write (Output, Bytes);
      end;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Write (Output,
        Ada.Streams.Stream_Element_Array'(0 => Character'Pos (C)));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Buffer : out String; Value : Character_Set_Id) is
      package IO is new Ada.Text_IO.Integer_IO (Character_Set_Id);
      Aux : Character;
   begin
      IO.Put (Buffer, Value, 16);
      if Buffer (Buffer'First + 1) = ' ' then
         Buffer (Buffer'First + 1 .. Buffer'First + 3) := "16#";
         for J in Buffer'First + 4 .. Buffer'Last - 1 loop
            Aux := Buffer (J);
            Buffer (J) := '0';
            exit when Aux = '#';
         end loop;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Buffer : out String; Value : Code_Set_Id) is
      package IO is new Ada.Text_IO.Integer_IO (Code_Set_Id);
      Aux : Character;
   begin
      IO.Put (Buffer, Value, 16);
      if Buffer (Buffer'First + 1) = ' ' then
         Buffer (Buffer'First + 1 .. Buffer'First + 3) := "16#";
         for J in Buffer'First + 4 .. Buffer'Last - 1 loop
            Aux := Buffer (J);
            Buffer (J) := '0';
            exit when Aux = '#';
         end loop;
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

--  Start of processing for Gen_Codeset

begin
   if Ada.Command_Line.Argument_Count /= 3
     or else (Ada.Command_Line.Argument (1) /= "-d"
     and then Ada.Command_Line.Argument (1) /= "-c")
   then
      ATIO.Put_Line (ATIO.Standard_Error, "Usage:");
      ATIO.Put_Line (ATIO.Standard_Error,
        "gen_codesets <data_switch> <package_name> <output_file>");
      ATIO.Put_Line (ATIO.Standard_Error, "<data_switch>:");
      ATIO.Put_Line (ATIO.Standard_Error, "   -d  Code sets description");
      ATIO.Put_Line (ATIO.Standard_Error, "   -c  Code sets compatibility");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   if Ada.Command_Line.Argument (1) = "-d" then
      Mode := Description;
   else
      Mode := Compatibility;
   end if;

   while not ATIO.End_Of_File (ATIO.Standard_Input) loop
      ATIO.Get_Line (Line, Last);
      if Line (First .. Last) = "start" then
         Process_Code_Set;
      end if;
   end loop;

   Create (Output, Out_File, Ada.Command_Line.Argument (3));

   if Mode = Compatibility then
      Compact_Character_Sets_Table;
      Generate_Compatibility_Data_Module;
   else
      Generate_Description_Data_Module;
   end if;

end Gen_Codeset;
