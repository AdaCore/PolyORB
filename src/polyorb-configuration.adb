------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . C O N F I G U R A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  PolyORB runtime configuration facility.

--  $Id$

with Ada.Characters.Handling;
with Ada.Text_IO;

with Interfaces.C.Strings;
with System;

with PolyORB.Dynamic_Dict;
with PolyORB.Log;
with PolyORB.Utils;
with PolyORB.Utils.Strings;

package body PolyORB.Configuration is

   use Ada.Characters.Handling;
   use Ada.Text_IO;

   use Interfaces.C;
   use Interfaces.C.Strings;

   use PolyORB.Utils.Strings;

   -------
   -- O --
   -------

   procedure O (S : String);
   pragma Inline (O);
   --  Output a diagnostic or error message.

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

   --------------------------------------------
   -- The configuration variables dictionary --
   --------------------------------------------

   package Variables is
      new PolyORB.Dynamic_Dict (Value => String_Ptr);

   procedure Set_Variable
     (Section : String;
      Key     : String;
      Value   : String);
   --  Set a variable in a section from the configuration file.

   function Fetch (Key : String) return String;
   --  Get the string from a file (if Key starts with file: and the file
   --  exists, otherwise it is an empty string), or the string itself
   --  otherwise.

   function Get_Env (Key : String; Default : String := "")
     return String;
   --  Get the value of variable Key from the system
   --  environment variables, returning Default if not found.

   function Make_Global_Key (Section, Key : String) return String;
   --  Build Dynamic Dict key from (Section, Key) tuple.

   function Make_Env_Name (Section, Key : String) return String;
   --  Build environment variable from (Section, Key) tuple.

   function To_Boolean (V : String) return Boolean;
   --  Convert a String value to a Boolean value according
   --  to the rules indicated in the spec for boolean configuration
   --  variables.

   ---------------------
   -- Make_Global_Key --
   ---------------------

   function Make_Global_Key (Section, Key : String) return String is
   begin
      return "[" & Section & "]" & Key;
   end Make_Global_Key;

   -------------------
   -- Make_Env_Name --
   -------------------

   function Make_Env_Name
     (Section, Key : String)
     return String
   is
      Result : String := "POLYORB_"
        & To_Upper (Section & "_" & Key);
   begin
      for J in Result'Range loop
         case Result (J) is
            when
              '0' .. '9' |
              'A' .. 'Z' |
              'a' .. 'z' |
              '_'        =>
               null;
            when others =>
               Result (J) := '_';
         end case;
      end loop;

      return Result;
   end Make_Env_Name;

   -----------
   -- Fetch --
   -----------

   function Fetch (Key : String)  return String is
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

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean
     (V : String)
     return Boolean
   is
      VV : constant String := To_Lower (V);
   begin
      if VV'Length > 0 then
         case VV (VV'First) is
            when '0' | 'n' =>
               return False;

            when '1' | 'y' =>
               return True;

            when 'o' =>
               if VV = "off" then
                  return False;
               elsif VV = "on" then
                  return True;
               end if;

            when 'd' =>
               if VV = "disable" then
                  return False;
               end if;

            when 'e' =>
               if VV = "enable" then
                  return True;
               end if;

            when 'f' =>
               if VV = "false" then
                  return False;
               end if;

            when 't' =>
               if VV = "true" then
                  return True;
               end if;

            when others =>
               null;
         end case;
      end if;

      raise Constraint_Error;
   end To_Boolean;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Section, Key : String;
      Default      : String := "")
     return String
   is
      From_Env : constant String
        := Get_Env (Make_Env_Name (Section, Key));

      Default_Value : aliased String := Default;

   begin
      if From_Env /= "" then
         return Fetch (From_Env);
      else
         return Fetch
           (Variables.Lookup
            (Make_Global_Key (Section, Key),
             String_Ptr'(Default_Value'Unchecked_Access)).all);
      end if;
   end Get_Conf;

   function Get_Conf
     (Section, Key : String;
      Default      : Boolean := False)
     return Boolean
   is
      Default_Value : constant array (Boolean'Range) of
        String (1 .. 1) := (False => "0", True => "1");
   begin
      return To_Boolean (Get_Conf (Section, Key, Default_Value (Default)));
   end Get_Conf;

   function Get_Conf
     (Section, Key : String;
      Default      : Integer := 0)
     return Integer is
   begin
      return Integer'Value (Get_Conf (Section, Key, Integer'Image (Default)));
   end Get_Conf;

   -------------
   -- Get_Env --
   -------------

   function Get_Env
     (Key     : String;
      Default : String := "")
     return String
   is
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

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Section : String;
      Key     : String;
      Value   : String)
   is
      K : constant String := Make_Global_Key (Section, Key);
      P : String_Ptr := Variables.Lookup (K, null);
   begin
      pragma Debug (O (K & "=" & Value));
      if P /= null then
         Variables.Unregister (K);
         Free (P);
      end if;

      Variables.Register (K, +Value);
   end Set_Variable;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File
     (Conf_File_Name : String)
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

      Line : String (1 .. 1_024);
      Last : Integer;

      Success : Boolean := False;

      use PolyORB.Utils;

   begin
      pragma Debug (O ("Loading configuration from " & Conf_File_Name));

      begin
         Open (Conf_File, In_File, Conf_File_Name);
         Success := True;
      exception
         when Name_Error =>
            --  No configuration file.
            pragma Debug (O ("No " & Conf_File_Name & " configuration file."));
            null;
         when others =>
            raise;
      end;

      while Success and then not End_Of_File (Conf_File) loop
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
                        O ("Syntax error on line" &
                           Integer'Image (Current_Line) &
                           ": " & Line (Line'First .. Last));
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

   ---------------------------------
   -- PolyORB_Configuration_File  --
   ---------------------------------

   function PolyORB_Configuration_File
     return String is
   begin
      return Get_Env (PolyORB_Conf_Filename_Variable,
                      PolyORB_Conf_Default_Filename);

   end PolyORB_Configuration_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.Log.Get_Conf_Hook := Get_Conf'Access;
   end Initialize;

end PolyORB.Configuration;
