------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O N F I G U R A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

--  PolyORB runtime configuration facility.

--  $Id$

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;          use Ada.Text_IO;

with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with System;

with PolyORB.Dynamic_Dict;
pragma Elaborate_All (PolyORB.Dynamic_Dict);
with PolyORB.Utils;
with PolyORB.Utils.Strings;

package body PolyORB.Configuration is

   use PolyORB.Utils.Strings;

   package Environment_Variables is
      new PolyORB.Dynamic_Dict (String_Ptr);

   procedure Load_Configuration_File;
   --  Load the configuration file.

   procedure Set_Variable
     (Section : String;
      Key     : String;
      Value   : String);
   --  Set a variable in a section from the configuration file.

   procedure Set_Environment_Variable
     (Key, Value : String);
   --  Record a value for a variable in the [environment]
   --  section from the configuration file.

   function Fetch (Key : String) return String;
   --  Get the string from a file (if Key starts with file: and the file
   --  exists, otherwise it is an empty string), or the string itself
   --  otherwise.

   function Get_Env (Key : String; Default : String := "")
     return String;
   --  Get the value of variable Key from the system
   --  environment variables, returning Default if not found.

   -----------
   -- Fetch --
   -----------

   function Fetch (Key : String) return String is
   begin
      if Key'Length > 4
        and then Key (Key'First .. Key'First + 4) = "file:"
      then
         declare
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
         end;
      else
         return Key;
      end if;
   end Fetch;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf (Key : String; Default : String := "")
     return String
   is
      From_Env : constant String := Get_Env (Key);
      Default_Value : aliased String := Default;
   begin
      if From_Env /= "" then
         return Fetch (From_Env);
      else
         return Fetch
           (Environment_Variables.Lookup
            (Key, String_Ptr'(Default_Value'Unchecked_Access)).all);
      end if;
   end Get_Conf;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (Key : String; Default : String := "") return String is

      function getenv (Key : System.Address) return chars_ptr;
      pragma Import (C, getenv, "getenv");

      C_Key   : aliased char_array := To_C (Key);
      C_Value : constant chars_ptr := getenv (C_Key'Address);
   begin
      if C_Value = Null_Ptr then
         return Default;
      else
         return Value (C_Value);
      end if;
   end Get_Env;

   procedure Set_Environment_Variable
     (Key, Value : String)
   is
      P : String_Ptr := Environment_Variables.Lookup (Key, null);
   begin
      if P /= null then
         Free (P);
      end if;
      P := +Value;
      Environment_Variables.Register (Key, P);
   end Set_Environment_Variable;

   procedure Set_Variable
     (Section : String;
      Key     : String;
      Value   : String)
   is
   begin
      Configuration_Sections.Lookup (To_Lower (Section)).all
        (To_Lower (Key), Value);
   end Set_Variable;

   procedure Load_Configuration_File
   is
      Current_Section : String_Ptr
        := +Environment_Configuration_Section;
      Current_Line : Integer := 0;

      procedure Set_Current_Section (S : String);
      --  Enter a new section named S.

      procedure Set_Current_Section (S : String) is
      begin
         Free (Current_Section);
         Current_Section := +S;
      end Set_Current_Section;

      Conf_File : File_Type;
      Conf_File_Name : constant String
        := Get_Env (Filename_Variable, Default_Filename);

      Line : String (1 .. 1_024);
      Last : Integer;

      use PolyORB.Utils;

   begin
      Open (Conf_File, In_File, Conf_File_Name);
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
                        raise Syntax_Error;
                     end if;

                     Set_Current_Section (Line (Bra + 1 .. Ket - 1));
                  end;

               when others =>
                  declare
                     Eq : constant Integer
                       := Find (Line (Line'First .. Last),
                                Line'First, '=');
                  begin
                     if Eq not in Line'First + 1 .. Last - 1 then
                        raise Syntax_Error;
                     end if;

                     Set_Variable
                       (Section => Current_Section.all,
                        Key     => Line (Line'First .. Eq - 1),
                        Value   => Line (Eq + 1 .. Last));
                  end;
            end case;
         end if;
      end loop;
   end Load_Configuration_File;

begin
   Configuration_Sections.Register
     (Environment_Configuration_Section,
      Set_Environment_Variable'Access);
   Load_Configuration_File;
end PolyORB.Configuration;
