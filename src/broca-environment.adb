with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with System;

package body Broca.Environment is

   function Get_Env (Key : String; Default : String := "") return String;

   function Lookup (Key : String; Default : String := "") return String;

   ---------
   -- Get --
   ---------

   function Get_Conf (Key : String; Default : String := "") return String is
      From_Env : constant String := Get_Env (Key);
   begin
      if From_Env /= "" then
         return From_Env;
      else
         return Lookup (Key, Default);
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
