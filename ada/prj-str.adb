------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . S T R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Errout;    use Errout;
with Namet;     use Namet;
with Prj.Ext;   use Prj.Ext;
with Scans;     use Scans;
with Scn;       use Scn;
with Sinfo;     use Sinfo;
with Stringt;   use Stringt;

package body Prj.Str is

   procedure Add (To_Exp : in out String_Id; Str : in String_Id);
   --  Concatenate two strings and returns another string if both
   --  arguments are not null string.

   function External_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Parse an external value:
   --
   --   external_value ::=
   --     _external_ (string_expression [,string_expression])

   function Term
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Parse a string_term:
   --
   --   string_term ::=
   --     literal_string |
   --     <single_variable_>name |
   --     external_value

   function Variable_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Get the string value of the current identifier token.
   --  If no variable with this name, returns No_String.

   ---------
   -- Add --
   ---------

   procedure Add (To_Exp : in out String_Id; Str : in String_Id) is
   begin
      if To_Exp = Types.No_String or else String_Length (To_Exp) = 0 then
         To_Exp := Str;

      elsif Str /= No_String then
         Start_String (To_Exp);
         Store_String_Chars (Str);
         To_Exp := End_String;
      end if;
   end Add;

   --------------------
   -- External_Value --
   --------------------

   function External_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return     String_Id
   is
   begin
      --  The current token is External

      --  Get the left parenthesis

      Scan;

      Expect (Tok_Left_Paren, "(");

      --  Scan past the left parenthesis

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      --  Get the name of the external reference

      declare
         Name : constant String_Id := Value (Project, Pkg);

      begin
         case Token is
            when Tok_Right_Paren =>
               if Name /= No_String then
                  String_To_Name_Buffer (Name);
                  return Prj.Ext.Value_Of (Name_Find);
               end if;

            when Tok_Comma =>

               --  Scan past the comma

               Scan;

               --  Get the default

               declare
                  Default : constant String_Id := Value (Project, Pkg);

               begin
                  Expect (Tok_Right_Paren, ")");

                  if Name /= No_String then
                     String_To_Name_Buffer (Name);
                     return Prj.Ext.Value_Of (Name_Find, Default);
                  end if;

               end;

            when others =>
               Error_Msg_BC ("Expected ',' or ')'.");
         end case;
      end;

      return No_String;
   end External_Value;

   ----------
   -- Term --
   ----------

   function Term
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id
   is
   begin

      --  A string term can be a literal string, a single string variable,
      --  or an external value.

      case Token is
         when Tok_String_Literal =>
            return Strval (Token_Node);

         when Tok_Identifier =>
            return Variable_Value (Project, Pkg);

         when Tok_External =>
            return External_Value (Project, Pkg);

         when others =>
            Error_Msg_BC ("Expected literal string, " &
                         "variable identifier or external value.");
      end case;

      return No_String;
   end Term;

   -----------
   -- Value --
   -----------

   function Value
     (Project    : Project_Data;
      Pkg        : Package_Id)
      return       Name_Id
   is
      Result : String_Id := Value (Project, Pkg);

   begin
      if Result = No_String or else String_Length (Result) = 0 then
         return No_Name;

      else
         String_To_Name_Buffer (Result);
         return Name_Find;
      end if;

   end Value;

   function Value
     (Project    : Project_Data;
      Pkg        : Package_Id)
      return       String
   is
      Result : String_Id := Value (Project, Pkg);

   begin
      if Result = No_String then
         return "";

      else
         String_To_Name_Buffer (Result);
         return Name_Buffer (1 .. Name_Len);
      end if;

   end Value;

   function Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id
   is
      Result : String_Id := No_String;

   begin
      --  A string expression is a sequence of string terms connected
      --  by ampersands (&).

      loop
         Add (Result, Term (Project, Pkg));
         Scan;
         exit when Token /= Tok_Ampersand;
         Scan;
      end loop;

      return Result;
   end Value;

   --------------------
   -- Variable_Value --
   --------------------

   function Variable_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id
   is
      Current_Package : Package_Id := Pkg;
      Name            : constant Name_Id := Token_Name;

      function Value (Var : Variable_Id) return String_Id;

      function Value (Var : Variable_Id) return String_Id is
         Current      : Variable_Id := Var;
         The_Variable : Variable;

      begin
         while Current /= No_Variable loop
            The_Variable := Variable_Elements.Table (Current);

            if The_Variable.Name = Name then
               if The_Variable.Value.Kind = List then
                  Error_Msg_BC ("Variable cannot be list.");
                  return No_String;
               else
                  return The_Variable.Value.Value;
               end if;
            end if;

            Current := The_Variable.Next;
         end loop;

         return No_String;
      end Value;

   --   Start of processing for Variable_Value

   begin
      --  First check if the variable is defined in the current package
      --  or in one of its ancestors, if any.

      while Current_Package /= No_Package loop
         declare
            The_Package : constant Package_Element :=
                            Packages.Table (Current_Package);
            Result      : constant String_Id :=
                            Value (The_Package.Decl.Variables);

         begin
            if Result /= No_String then
               return Result;
            end if;

            Current_Package := The_Package.Parent;
         end;

      end loop;

      --  We have not found the variable in a package, so look outside
      --  of the packages.

      if Project.Name /= No_Name then
         declare
            Result : constant String_Id := Value (Project.Decl.Variables);

         begin
            if Result /= No_String then
               return Result;
            end if;
         end;
      end if;

      Error_Msg_BC ("Unknown variable");
      return No_String;
   end Variable_Value;

end Prj.Str;
