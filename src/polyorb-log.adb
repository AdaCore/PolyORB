------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;

with PolyORB.Dynamic_Dict;
pragma Elaborate_All (PolyORB.Dynamic_Dict);

package body PolyORB.Log is

   package Log_Level_Dict is new PolyORB.Dynamic_Dict (Log_Level);
   --  A hash table that stores the logging level associated
   --  with each facility.

   -------------------
   -- Get_Log_Level --
   -------------------

   function Get_Log_Level
     (Facility : in String)
     return Log_Level
     renames Log_Level_Dict.Lookup;

   -------------------
   -- Set_Log_Level --
   -------------------

   procedure Set_Log_Level
     (Facility : in String;
      Level    : Log_Level)
     renames Log_Level_Dict.Register;

   -------------------------------
   -- Generic body Facility_Log --
   -------------------------------

   package body Facility_Log is

      Initialized : Boolean := False;
      Facility_Level : Log_Level := Info;

      procedure Output
        (Message : in String;
         Level   : Log_Level := Debug)
      is
      begin
         if not Initialized then
            begin
               Facility_Level := Get_Log_Level (Facility);
            exception
               when Log_Level_Dict.Key_Not_Found =>
                  Facility_Level := Notice;
               when others =>
                  raise;
            end;
            Initialized := True;
         end if;

         if Level >= Facility_Level then
            Put_Line (Facility & ": " & Message);
         end if;
      end Output;

   end Facility_Log;

   --------------------------------
   -- Initialization subprograms --
   --------------------------------

   function Is_Space (C : Character) return Boolean;
   --  True if, and only if, C is a whitespace character
   --  (space or horizontal tab).

   procedure Parse_Line (L : String);
   --  Parse a configuration directive of the form
   --  'FACILITY=LEVEL'.

   function Is_Space (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_Space;

   procedure Parse_Line (L : String)
   is
      Name_First : Integer := L'First;
      Name_Last : Integer;
      Level_First : Integer;
      Level_Last : Integer;
   begin

      --  Skip initial whitespace

      while Name_First <= L'Last and then Is_Space (L (Name_First)) loop
         Name_First := Name_First + 1;
      end loop;

      --  Find end of facility name

      Name_Last := Name_First;
      while Name_Last < L'Last
        and then L (Name_Last + 1) /= '='
        and then not Is_Space (L (Name_Last + 1)) loop
         Name_Last := Name_Last + 1;
      end loop;

      --  Find start of level name

      Level_First := Name_Last + 1;
      while Level_First <= L'Last
        and then (Is_Space (L (Level_First))
                  or else L (Level_First) = '=') loop
         Level_First := Level_First + 1;
      end loop;

      --  Find end of level name

      Level_Last := Level_First;
      while Level_Last < L'Last and then not Is_Space (L (Level_Last + 1)) loop
         Level_Last := Level_Last + 1;
      end loop;


      declare
         Facility : constant String := L (Name_First .. Name_Last);
         Level    : constant String
           := Ada.Characters.Handling.To_Upper (L (Level_First .. Level_Last));
      begin
         for L in Log_Level loop
            --  Technical Note: Given a scalar type (such as an integer, an
            --  enumeration, etc), Type'Image (X) returns the value of X
            --  converted to a string.

            if Log_Level'Image (L) = Level then
               Put_Line ("Setting log level for " & Facility & " to " & Level);
               Set_Log_Level (Facility, L);
               exit;
            end if;
         end loop;

      end;
   end Parse_Line;

   procedure Initialize
   is
      --  XXX Should be merged with a global configration file!

      Filename : constant String := "polyorb_log.conf";

      File : File_Type;
      Line : String (1 .. 256);
      Last : Natural;

   begin
      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Last /= 0 then
            if Line (1) /= '#' then
               Parse_Line (Line (1 .. Last));
            end if;
         end if;
      end loop;

      Close (File);

   exception
      when others =>
      null;
   end Initialize;

begin
   Initialize;
end PolyORB.Log;
