------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          G E N _ C O D E S E T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This program generate packages PolyORB.GIOP_P.Code_Sets.Data and
--  PolyORB.GIOP_P.Code_Sets.Descriptions.Data from the OSF/OpenGroup
--  code set registry file.

--  Latest code set registry file may be downloaded from OpenGroup FTP server
--  ftp://ftp.opengroup.org/pub/code_set_registry/

--  This program oriented on version 1.2 of code set registry file format.

--  $Id$

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with GNAT.Table;

procedure Gen_Codeset is

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

   procedure Add_Description (Description : in     String;
                              First       :    out Positive;
                              Last        :    out Natural);
   --  Copy description string into descriptions table and return first and
   --  last indexes of copied string.

   procedure Process_Code_Set;
   --  Process one section of code set registry file.

   procedure Compact_Character_Sets_Table;
   --  Compress contents of Character_Sets_Table.

   procedure Put (Buffer :    out String;
                  Value  : in     Code_Set_Id);

   procedure Put (Buffer :    out String;
                  Value  : in     Character_Set_Id);

   File : File_Type;

   Line  : String (1 .. 1024);
   First : constant Positive := Line'First;
   Last  : Natural := Line'First - 1;

   Short_Description : constant String := "Short Description";
   Registered_Value  : constant String := "Registered Value";
   Character_Set_Ids : constant String := "Character Set ID(s)";

   ---------------------
   -- Add_Description --
   ---------------------

   procedure Add_Description (Description : in     String;
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

      function Find (First : in Positive; Last : in Natural) return Natural;
      --  Return first index of sequence of characters sets in Aux_Table
      --  what equal of sequence Character_Sets_Talbe (First .. Last).

      ----------
      -- Find --
      ----------

      function Find (First : in Positive; Last : in Natural) return Natural is
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
            Length : constant Natural
              := Info.Character_Set_Last - Info.Character_Set_First + 1;
            Index  : constant Natural
              := Find (Info.Character_Set_First, Info.Character_Set_Last);
            First  : constant Natural := Aux_Table.Last + 1;
         begin
            if Index = 0 then
               for J in Info.Character_Set_First
                          .. Info.Character_Set_Last
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

      Put_Line
       ("   Original size:"
          & Integer'Image
             (Character_Sets_Table.Last - Character_Sets_Table.First + 1)
          & "   Size after compression:"
          & Integer'Image (Aux_Table.Last - Aux_Table.First + 1));

      for J in Aux_Table.First .. Aux_Table.Last loop
         Character_Sets_Table.Set_Item (J, Aux_Table.Table (J));
      end loop;

      Character_Sets_Table.Set_Last (Aux_Table.Last);

      Aux_Table.Free;
   end Compact_Character_Sets_Table;

   ----------------------
   -- Process_Code_Set --
   ----------------------

   procedure Process_Code_Set is
      Description_First   : Positive    := 1;
      Description_Last    : Natural     := 0;
      Code_Set            : Code_Set_Id := 0;
      Character_Set_First : Positive    := Character_Sets_Table.Last + 1;
      Character_Set_Last  : Natural     := 0;
   begin
      for J in 1 .. 4 loop
         Get_Line (File, Line, Last);

         if Line'Length > Short_Description'Length + 2
           and then Line (First .. First + Short_Description'Length - 1)
                      = Short_Description
         then
            Add_Description
             (Line (First + Short_Description'Length + 1 .. Last),
              Description_First,
              Description_Last);

         elsif Line'Length > Registered_Value'Length + 2
           and then Line (First .. First + Registered_Value'Length - 1)
                      = Registered_Value
         then
            declare
               Image : constant String
                 := "16#" & Line (Registered_Value'Last + 4 .. Last) & '#';
            begin
               Code_Set := Code_Set_Id'Value (Image);
            end;

         elsif Line'Length > Character_Set_Ids'Length + 2
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

   procedure Put (Buffer :    out String;
                  Value  : in     Code_Set_Id)
   is
      package Code_Set_Id_IO is new Integer_IO (Code_Set_Id);
      use Code_Set_Id_IO;
      Aux : Character;
   begin
      Put (Buffer, Value, 16);
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

   procedure Put (Buffer :    out String;
                  Value  : in     Character_Set_Id)
   is
      package Character_Set_Id_IO is new Integer_IO (Character_Set_Id);
      use Character_Set_Id_IO;
      Aux : Character;
   begin
      Put (Buffer, Value, 16);
      if Buffer (Buffer'First + 1) = ' ' then
         Buffer (Buffer'First + 1 .. Buffer'First + 3) := "16#";
         for J in Buffer'First + 4 .. Buffer'Last - 1 loop
            Aux := Buffer (J);
            Buffer (J) := '0';
            exit when Aux = '#';
         end loop;
      end if;
   end Put;

begin
   Put_Line ("Loading code set registry database...");
   Open (File, In_File, "cs_registry1.2h");

   while not End_Of_File (File) loop
      Get_Line (File, Line, Last);
      if Line (First .. Last) = "start" then
         Process_Code_Set;
      end if;
   end loop;

   Close (File);

   Put_Line ("Compacting tables data...");

   Compact_Character_Sets_Table;

   Put_Line ("Generate data packages...");

   --  Genrate code set's descriptions data for po_catref utility.

   Create (File, Out_File, "polyorb-giop_p-code_sets-descriptions-data.ads");
   New_Line (File);

   Put_Line
    (File, "private package PolyORB.GIOP_P.Code_Sets.Descriptions.Data is");
   New_Line (File);

   Put_Line
    (File, "   Info : constant array (Positive range <>) of Info_Record");
   Put (File, "     := (");

   for J in 1 .. Code_Set_Table.Last loop
      declare
         Info : Code_Set_Info renames Code_Set_Table.Table (J);
         Buf  : String (1 .. 13);
      begin
         Put (Buf, Info.Code_Set);

         Put (File, '(');
         Put (File, Buf (2 .. 13));
         Put (File, ',');
         Put (File, Info.Description_First, 5);
         Put (File, ',');
         Put (File, Info.Description_Last, 5);
         Put (File, ')');
      end;

      if J /= Code_Set_Table.Last then
         Put (File, ',');
         New_Line (File);
         Put (File, "         ");
      else
         Put (File, ");");
         New_Line (File);
      end if;
   end loop;
   New_Line (File);

   Put_Line (File, "   Description : constant String");
   Put (File, "     := """);
   for J in 1 .. Description_Table.Last loop
      Put (File, Description_Table.Table (J));
      if J = 68 or else (J > 68 and then (J - 68) mod 64 = 0) then
         Put (File, '"');
         New_Line (File);
         Put (File, "          & """);
      end if;
   end loop;
   Put (File, """;");
   New_Line (File);
   New_Line (File);

   Put_Line (File, "end PolyORB.GIOP_P.Code_Sets.Descriptions.Data;");

   Close (File);

   --  Generate code set's characters set data.

   Create (File, Out_File, "polyorb-giop_p-code_sets-data.ads");
   New_Line (File);

   Put_Line
    (File, "private package PolyORB.GIOP_P.Code_Sets.Data is");
   New_Line (File);

   Put_Line
    (File,
     "   Info : constant array (Positive range <>) of Code_Set_Info_Record");
   Put (File, "     := (");

   for J in 1 .. Code_Set_Table.Last loop
      declare
         Info : Code_Set_Info renames Code_Set_Table.Table (J);
         Buf  : String (1 .. 13);
      begin
         Put (Buf, Info.Code_Set);

         Put (File, '(');
         Put (File, Buf (2 .. 13));
         Put (File, ',');
         Put (File, Info.Character_Set_First, 4);
         Put (File, ',');
         Put (File, Info.Character_Set_Last, 4);
         Put (File, ')');
      end;

      if J /= Code_Set_Table.Last then
         Put (File, ',');
         New_Line (File);
         Put (File, "         ");
      else
         Put (File, ");");
         New_Line (File);
      end if;
   end loop;
   New_Line (File);

   Put_Line
    (File,
     "   Character_Sets : constant Character_Set_Id_Array");
   Put (File, "     := (");

   for J in 1 .. Character_Sets_Table.Last loop
      declare
         Buf : String (1 .. 9);
      begin
         Put (Buf, Character_Sets_Table.Table (J));
         Put (File, Buf (2 .. 9));
      end;

      if J /= Character_Sets_Table.Last then
         Put (File, ",");
         if J mod 7 = 0 then
            New_Line (File);
            Put (File, "         ");
         else
            Put (File, ' ');
         end if;
      end if;
   end loop;
   Put (File, ");");
   New_Line (File);
   New_Line (File);

   Put_Line (File, "end PolyORB.GIOP_P.Code_Sets.Data;");

   Close (File);
end Gen_Codeset;
