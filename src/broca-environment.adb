------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . E N V I R O N M E N T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with System;

package body Broca.Environment is

   function Fetch (Key : String) return String;
   --  Get the string from a file (if Key starts with file: and the file
   --  exists, otherwise it is an empty string), or the string itself
   --  otherwise.

   function Get_Env (Key : String; Default : String := "") return String;

   function Lookup (Key : String; Default : String := "") return String;

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

   ---------
   -- Get --
   ---------

   function Get_Conf (Key : String; Default : String := "") return String is
      From_Env : constant String := Get_Env (Key);
   begin
      if From_Env /= "" then
         return Fetch (From_Env);
      else
         return Fetch (Lookup (Key, Default));
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

   ------------
   -- Lookup --
   ------------

   function Lookup (Key : String; Default : String := "") return String is
      Conf_File      : File_Type;
      Conf_File_Name : constant String :=
        Get_Env (Filename_Variable, Default_Filename);
      Line           : String (1 .. 200);
      Last           : Natural;
      Aug_Key        : constant String := Key & '=';
      Aug_Key_Len    : constant Positive := Aug_Key'Length;
   begin
      Open (Conf_File, In_File, Conf_File_Name);
      while not End_Of_File (Conf_File) loop
         Get_Line (Conf_File, Line, Last);
         if Last >= Aug_Key_Len
           and then Line (1 .. Aug_Key_Len) = Aug_Key
         then
            return Line (Aug_Key_Len + 1 .. Last);
         end if;
      end loop;
      return Default;
   exception
      when Name_Error =>
         return Default;
   end Lookup;

end Broca.Environment;
