--  $Id$

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;

with GNAT.HTable;

package body Droopi.Log is

   ------------------------------------------------
   -- A hash table that stores the logging level --
   -- associated with each facility.             --
   ------------------------------------------------

   type String_Ptr is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

   type Hash_Val is new Integer range 0 .. 32;

   function Hash (S : String_Ptr) return Hash_Val;
   function Equal (S1, S2 : String_Ptr) return Boolean;
   --  Simple {hash,equality} functions operating on a string access.
   --  Used in instanciation of GNAT.HTable.

   function Hash (S : String_Ptr) return Hash_Val
   is
      function Hash_String is new GNAT.HTable.Hash (Hash_Val);
   begin
      pragma Assert (S /= null);
      return Hash_String (S.all);
   end Hash;

   function Equal (S1, S2 : String_Ptr) return Boolean is
   begin
      pragma Assert (S1 /= null and then S2 /= null);
      return S1.all = S2.all;
   end Equal;

   type Log_Level_Info is record
      Key_Ptr : String_Ptr;
      Level   : Log_Level;
   end record;

   Null_Log_Level_Info : constant Log_Level_Info
     := (Key_Ptr => null, Level => Info);

   package HT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Val,
      Element    => Log_Level_Info,
      No_Element => Null_Log_Level_Info,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   -------------------
   -- Get_Log_Level --
   -------------------

   function Get_Log_Level
     (Facility : in String)
      return Log_Level
   is
      F   : String_Ptr := new String'(Facility);
      LLI : constant Log_Level_Info := HT.Get (F);
   begin
      Free (F);
      return LLI.Level;
   end Get_Log_Level;

   -------------------
   -- Set_Log_Level --
   -------------------

   procedure Set_Log_Level
     (Facility : in String;
      Level    : Log_Level)
   is
      F   : String_Ptr := new String'(Facility);
      LLI : Log_Level_Info := HT.Get (F);
   begin
      LLI.Level := Level;

      if LLI.Key_Ptr = null then
         LLI.Key_Ptr := F;
      else
         Free (F);
      end if;

      HT.Set (LLI.Key_Ptr, LLI);
   end Set_Log_Level;

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
            Facility_Level := Get_Log_Level (Facility);
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
               Set_Log_Level (Facility, L);
               exit;
            end if;
         end loop;

      end;
   end Parse_Line;

   procedure Initialize
   is
      --  XXX Should be merged with a global configration file!

      Filename : constant String := "droopi_log.conf";

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

end Droopi.Log;
