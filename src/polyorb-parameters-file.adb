------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P A R A M E T E R S . F I L E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with PolyORB.Dynamic_Dict;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Parameters.File is

   use Ada.Text_IO;

   use PolyORB.Utils.Strings;

   -------
   -- O --
   -------

   procedure O (S : String);
   pragma Inline (O);
   --  Output a diagnostic or error message

   --  Note: We are currently initializing structures on which
   --  PolyORB.Log.Facility_Log depends. Thus we cannot instantiate
   --  this package and use PolyORB.Log.Internals.Put_Line instead.

   Debug : constant Boolean := True;

   procedure O (S : String) is
   begin
      if Debug then
         PolyORB.Log.Internals.Put_Line (S);
      end if;
   end O;

   -------------------------------------------------------------
   -- Dictionary of configuration parameters loaded from file --
   -------------------------------------------------------------

   package Variables is new PolyORB.Dynamic_Dict (Value => String_Ptr);

   function Make_Global_Key (Section, Key : String) return String;
   --  Build Dynamic Dict key from (Section, Key) tuple

   ----------------------
   -- File data source --
   ----------------------

   type File_Source is new Parameters_Source with null record;
   function Get_Conf
     (Source       : access File_Source;
      Section, Key : String) return String;

   The_File_Source : aliased File_Source;

   ---------------------
   -- Fetch_From_File --
   ---------------------

   function Fetch_From_File (Key : String) return String;

   function Fetch_From_File (Key : String) return String is
      Filename : constant String := Key (Key'First + 5 .. Key'Last);
      File     : File_Type;
      Result   : String (1 .. 1024);
      Last     : Natural;
   begin
      Open (File, In_File, Filename);
      Get_Line (File, Result, Last);
      Close (File);
      return Result (1 .. Last);

   exception
      when Name_Error =>
         return "";
   end Fetch_From_File;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Source       : access File_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);

      V : constant String_Ptr := Variables.Lookup
                                   (Make_Global_Key (Section, Key), null);
   begin
      if V /= null then
         return V.all;
      else
         return "";
      end if;
   end Get_Conf;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File (Conf_File_Name : String) is
      Current_Section : String_Ptr := null;
      Current_Line : Integer := 0;

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
      pragma Debug (O ("Loading configuration from " & Conf_File_Name));

      begin
         Open (Conf_File, In_File, Conf_File_Name);
      exception
         when Name_Error =>
            --  No configuration file

            pragma Debug (O ("No " & Conf_File_Name & " configuration file."));
            return;
      end;

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
                     Ket : constant Integer
                       := Find (Line (Line'First .. Last), Bra, ']');
                  begin
                     if False
                       or else Ket > Last
                       or else Ket = Bra + 1
                       or else Ket /= Last
                     then
                        O ("Syntax error on line" &
                           Integer'Image (Current_Line) &
                           ": " & Line (Line'First .. Last));
                        raise Constraint_Error;
                     end if;

                     Set_Current_Section (Line (Bra + 1 .. Ket - 1));
                  end;

               when others =>
                  declare
                     Eq : constant Integer
                       := Find (Line (Line'First .. Last),
                                Line'First, '=');
                  begin
                     if Current_Section = null then
                        O ("Assignment out of any section on line" &
                           Integer'Image (Current_Line) &
                           ": " & Line (Line'First .. Last));
                        raise Constraint_Error;
                     end if;

                     if Eq not in Line'First + 1 .. Last - 1 then
                        O ("Syntax error on line" &
                           Integer'Image (Current_Line) &
                           ": " & Line (Line'First .. Last));
                        raise Constraint_Error;
                     end if;

                     Variables.Register (
                       Make_Global_Key (
                         Section => Current_Section.all,
                         Key     => Line (Line'First .. Eq - 1)),
                       +(Line (Eq + 1 .. Last)));
                  end;
            end case;
         end if;
      end loop;

      Close (Conf_File);
   end Load_Configuration_File;

   -----------------------------
   -- Configuration_File_Name --
   -----------------------------

   function Configuration_File_Name return String is
   begin
      return Get_Conf (Section => "conf", Key => "",
                       Default => PolyORB_Conf_Default_Filename);
      --  XXX special case for backwards compatibility: we want the
      --  associated environment variable to be POLYORB_CONF for now.
      --  Ultimately this should become Section => "configuration",
      --  Key => "file".
   end Configuration_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Load_Configuration_File (Configuration_File_Name);
      Register_Source (The_File_Source'Access);

      Fetch_From_File_Hook := Fetch_From_File'Access;
   end Initialize;

   ---------------------
   -- Make_Global_Key --
   ---------------------

   function Make_Global_Key (Section, Key : String) return String is
   begin
      return "[" & Section & "]" & Key;
   end Make_Global_Key;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.file",
       Conflicts => Empty,
       Depends   => +"log?"
         & "parameters.environment?"
         & "parameters.overrides?",
       Provides  => +"parameters",
       Implicit  => True,
       Init      => Initialize'Access));
end PolyORB.Parameters.File;
