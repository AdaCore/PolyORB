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

--  Generate a compact code set table.

--  $Id$

with Ada.Command_Line;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with GNAT.Table;

procedure Gen_Codeset is

   Pkg_Name : constant String := Ada.Command_Line.Argument (1);

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

   package Code_Set_Id_IO is new Integer_IO (Code_Set_Id);
   use Code_Set_Id_IO;

   procedure Add_Description (Description : in     String;
                              First       :    out Positive;
                              Last        :    out Natural);

   procedure Process_Code_Set;

   Line  : String (1 .. 1024);
   First : constant Positive := Line'First;
   Last  : Natural := Line'First - 1;

   Short_Description : constant String := "Short Description";
   Registered_Value  : constant String := "Registered Value";
   Character_Set_Ids : constant String := "Character Set ID(s)";

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

   procedure Process_Code_Set is
      Description_First   : Positive    := 1;
      Description_Last    : Natural     := 0;
      Code_Set            : Code_Set_Id := 0;
      Character_Set_First : constant Positive := Character_Sets_Table.Last + 1;
      Character_Set_Last  : Natural     := 0;
      Length              : Natural;
   begin
      for J in 1 .. 4 loop
         Get_Line (Line, Last);
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

begin
   while not End_Of_File (Standard_Input) loop
      Get_Line (Line, Last);
      if Line (First .. Last) = "start" then
         Process_Code_Set;
      end if;
   end loop;

   Put_Line
    ("private package " & Pkg_Name & " is");
   New_Line;

   Put_Line
    ("   Info : constant array (Positive range <>) of Info_Record");
   Put ("     := (");

   for J in 1 .. Code_Set_Table.Last loop
      declare
         Info : Code_Set_Info renames Code_Set_Table.Table (J);
         Buf  : String (1 .. 13);
      begin
         Put (Buf, Info.Code_Set, 16);
         if Buf (2) = ' ' then
            Buf (2 .. 4) := "16#";
            for J in 5 .. 12 loop
               if Buf (J) = '#' then
                  Buf (J) := '0';
                  exit;
               else
                  Buf (J) := '0';
               end if;
            end loop;
         end if;

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
         Put (");");
         New_Line;
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

   Put_Line ("end " & Pkg_Name & ";");
end Gen_Codeset;
