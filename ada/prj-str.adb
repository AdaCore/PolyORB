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
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
with Output;    use Output;
with Prj.Com;   use Prj.Com;
with Prj.Ext;   use Prj.Ext;
with Prj.Util;  use Prj.Util;
with Scans;     use Scans;
with Sinfo;     use Sinfo;
with Stringt;   use Stringt;

package body Prj.Str is

   procedure Add (To_Exp : in out String_Id; Str : in String_Id);
   --  Concatenate two strings and returns another string if both
   --  arguments are not null string.

   function Duplicate (The_Value : Variable_Value;
                       Location : Source_Ptr)
                      return Variable_Value;
   --  Duplicate the value of a variable
   --  Make a deep copy in case of a string list

   function External_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Parse an external value:
   --
   --   external_value ::=
   --     _external_ (string_expression [,string_expression])

   function Single_String_Term
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Parse a string_term:
   --
   --   string_term ::=
   --     literal_string |
   --     <single_variable_>name |
   --     external_value

   function Single_String_Variable_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id;
   --  Get the string value of the current identifier token.
   --  If no variable with this name, returns No_String.

   function Value_Of_Variable
     (Project     : Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
     return Variable_Value;
   --  Returns the value of a variable, using Duplicate,
   --  when Do_Not_Skip is True

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

   function Duplicate (The_Value : Variable_Value;
                       Location : Source_Ptr)
                      return Variable_Value is
   begin
      case The_Value.Kind is
         when Undefined | Single =>
            return The_Value;

         when List =>
            declare
               Result : Variable_Value (List);
               Origin : String_List_Id := The_Value.Values;
               Destin : String_List_Id;

            begin
               if Origin /= Nil_String then
                  String_Elements.Increment_Last;
                  Destin := String_Elements.Last;
                  Result.Values := Destin;
                  String_Elements.Table (Destin) :=
                    (Value => String_Elements.Table (Origin).Value,
                     Location => Location,
                     Next => Nil_String);

                  loop
                     Origin := String_Elements.Table (Origin).Next;
                     exit when Origin = Nil_String;
                     String_Elements.Increment_Last;
                     String_Elements.Table (Destin).Next :=
                       String_Elements.Last;
                     Destin := String_Elements.Last;
                     String_Elements.Table (Destin) :=
                       (Value => String_Elements.Table (Origin).Value,
                        Location => Location,
                        Next => Nil_String);
                  end loop;
               end if;
               return Result;
            end;
      end case;
   end Duplicate;

   --------------------
   -- External_Value --
   --------------------

   function External_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id
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
               Error_Msg_BC ("',' or ')' expected");
         end case;
      end;

      return No_String;
   end External_Value;

   ----------
   -- Term --
   ----------

   function Single_String_Term
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
            return Single_String_Variable_Value (Project, Pkg);

         when Tok_External =>
            return External_Value (Project, Pkg);

         when others =>
            Error_Msg_BC ("expected literal string, " &
                         "variable identifier or external value");
      end case;

      return No_String;
   end Single_String_Term;

   ----------------------------------
   -- Single_String_Variable_Value --
   ----------------------------------

   function Single_String_Variable_Value
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String_Id
   is
      The_Variable_Value : constant Variable_Value :=
                             Value_Of_Variable (Project => Project,
                                                Pkg => Pkg,
                                                Do_Not_Skip => False);
   begin
      case The_Variable_Value.Kind is
         when Undefined =>
            return No_String;

         when Single =>
            return The_Variable_Value.Value;

         when List =>
            Error_Msg_BC ("variable cannot be list");
            return No_String;
      end case;
   end Single_String_Variable_Value;

   -----------
   -- Value --
   -----------

   function Value
     (Project     : Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
      return        Variable_Value
   is
      Result : Variable_Value := Nil_Variable_Value;
      Last   : String_List_Id := Nil_String;

   begin
      loop
         case Token is

            when Tok_Left_Paren =>

               case Result.Kind is
                  when  Single =>
                     Error_Msg_BC
                       ("cannot append a string list to a string expression");
                     exit;

                  when Undefined =>
                     Result := (Kind => List,
                                Location => No_Location,
                                Values => Nil_String);

                  when List =>
                     null;

               end case;

               declare
                  Element    : String_Element;
                  Current    : String_List_Id;
                  The_String : String_Id;
                  Location   : Source_Ptr;
                  Values     : String_List_Id := Nil_String;

               begin
                  Scan;

                  if Token = Tok_Right_Paren then
                     Scan;

                  else
                     Location := Scan_Ptr;
                     The_String := Prj.Str.Value (Project, Pkg);

                     if Do_Not_Skip then
                        String_Elements.Increment_Last;
                        Current := String_Elements.Last;
                        Values := Current;
                        Element := (Value    => The_String,
                                    Location => Location,
                                    Next     => Nil_String);
                     end if;

                     loop
                        case Token is
                           when Tok_Right_Paren =>
                              Scan;
                              exit;

                           when Tok_Comma =>
                              Scan;

                              Location   := Scan_Ptr;
                              The_String := Prj.Str.Value (Project, Pkg);

                              if Do_Not_Skip then
                                 String_Elements.Increment_Last;
                                 Element.Next := String_Elements.Last;
                                 String_Elements.Table (Current) := Element;
                                 Current := String_Elements.Last;
                                 Element :=
                                   (Value    => The_String,
                                    Location => Location,
                                    Next     => Nil_String);
                              end if;

                           when others =>
                              Error_Msg_BC ("',' or ')' expected");
                        end case;
                     end loop;

                     if Do_Not_Skip then
                        String_Elements.Table (Current) := Element;
                     end if;

                  end if;

                  if Do_Not_Skip then
                     if Values /= Nil_String then

                        if Result.Values = Nil_String then
                           Result.Values := Values;
                        else
                           String_Elements.Table (Last).Next := Values;
                        end if;

                        Last := Current;
                     end if;
                  end if;
               end;

            when Tok_String_Literal | Tok_External =>
               declare
                  The_String : String_Id;
                  Location   : Source_Ptr := Scan_Ptr;

               begin
                  if Token = Tok_String_Literal then
                     The_String := Strval (Token_Node);
                  else
                     The_String := External_Value (Project, Pkg);
                  end if;

                  case Result.Kind is
                     when Undefined =>
                        Result := (Kind => Single,
                                   Location => No_Location,
                                   Value => The_String);

                     when Single =>
                        Add (Result.Value, The_String);

                     when List =>
                        if Do_Not_Skip then
                           String_Elements.Increment_Last;
                           String_Elements.Table (Last).Next :=
                             String_Elements.Last;
                           Last := String_Elements.Last;
                           String_Elements.Table (Last) :=
                             (Value    => The_String,
                              Location => Location,
                              Next     => Nil_String);
                        end if;
                  end case;

                  Scan;

               end;

            when Tok_Identifier =>
               declare
                  The_Variable_Value : constant Variable_Value :=
                                         Value_Of_Variable
                                           (Project, Pkg, Do_Not_Skip);

               begin
                  if The_Variable_Value.Kind /= Undefined then
                     case Result.Kind is
                        when Undefined =>
                           Result := The_Variable_Value;
                           if The_Variable_Value.Kind = List then
                              Last := The_Variable_Value.Values;
                           end if;

                        when List =>
                           if Do_Not_Skip then
                              if The_Variable_Value.Kind = Single then
                                 String_Elements.Increment_Last;

                                 if Result.Values = Nil_String then
                                    Result.Values := String_Elements.Last;
                                 else
                                    String_Elements.Table (Last).Next :=
                                      String_Elements.Last;
                                 end if;

                                 Last := String_Elements.Last;
                                 String_Elements.Table (Last) :=
                                   (Value => The_Variable_Value.Value,
                                    Location => The_Variable_Value.Location,
                                    Next => Nil_String);

                              else -- It is a string list
                                 if Result.Values = Nil_String then
                                    Last := The_Variable_Value.Values;
                                    Result.Values := The_Variable_Value.Values;
                                 else
                                    String_Elements.Table (Last).Next :=
                                      The_Variable_Value.Values;
                                 end if;
                              end if;
                           end if;

                        when Single =>
                           if The_Variable_Value.Kind = List then
                              Error_Msg_BC
                                ("concatenating a string list" &
                                 " to a string is forbiden");
                           else
                              Add (Result.Value, The_Variable_Value.Value);
                           end if;
                     end case;

                     if Do_Not_Skip
                       and then The_Variable_Value.Kind = List
                     then
                        while Last /= Nil_String and then
                          String_Elements.Table (Last).Next /= Nil_String loop
                           Last := String_Elements.Table (Last).Next;
                        end loop;
                     end if;
                  end if;
               end;

               Scan;

            when others =>
               Error_Msg_BC ("literal string, literal string list, " &
                             "variable identifier or external value expected");

         end case;

         exit when Token /= Tok_Ampersand;

         Scan;
      end loop;

      return Result;
   end Value;

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
     (Project : Project_Data;
      Pkg     : Package_Id)
      return    String
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
         Add (Result, Single_String_Term (Project, Pkg));
         Scan;
         exit when Token /= Tok_Ampersand;
         Scan;
      end loop;

      return Result;
   end Value;

   function Value_Of_Variable
     (Project     : Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
     return Variable_Value
   is
      Current_Package : Package_Id := Pkg;
      Name            : Name_Id := Token_Name;
      Location        : Source_Ptr := Scan_Ptr;

      function Value (Var : Variable_Id) return Variable_Value;

      function Value (Var : Variable_Id) return Variable_Value is
         Current      : Variable_Id := Var;
         The_Variable : Variable;

      begin
         while Current /= No_Variable loop
            The_Variable := Variable_Elements.Table (Current);

            if The_Variable.Name = Name then
               if not Do_Not_Skip or else The_Variable.Value.Kind = Single then
                  return The_Variable.Value;

               --  Duplicate string list

               else
                  declare
                     Result : Variable_Value (Kind => List);
                     Origin : String_List_Id := The_Variable.Value.Values;
                     Destin : String_List_Id;

                  begin
                     if Origin /= Nil_String then
                        String_Elements.Increment_Last;
                        Destin := String_Elements.Last;
                        Result.Values := Destin;
                        String_Elements.Table (Destin) :=
                          (Value => String_Elements.Table (Origin).Value,
                           Location => Location,
                           Next => Nil_String);

                        loop
                           Origin := String_Elements.Table (Origin).Next;
                           exit when Origin = Nil_String;
                           String_Elements.Increment_Last;
                           String_Elements.Table (Destin).Next :=
                             String_Elements.Last;
                           Destin := String_Elements.Last;
                           String_Elements.Table (Destin) :=
                             (Value => String_Elements.Table (Origin).Value,
                              Location => Location,
                              Next => Nil_String);
                        end loop;
                     end if;

                     return Result;
                  end;
               end if;
            end if;

            Current := The_Variable.Next;
         end loop;

         return Nil_Variable_Value;
      end Value;

   --   Start of processing for Single_String_Variable_Value

   begin
      --  First check if the variable is defined in the current package
      --  or in one of its ancestors, if any.

      while Current_Package /= No_Package loop
         declare
            The_Package : constant Package_Element :=
                            Packages.Table (Current_Package);
            Result      : constant Variable_Value :=
                            Value (The_Package.Decl.Variables);

         begin
            if Result /= Nil_Variable_Value then
               return Result;
            end if;

            Current_Package := The_Package.Parent;
         end;

      end loop;

      --  We have not found the variable in a package, so look outside
      --  of the packages.

      if Project.Name /= No_Name then
         declare
            Result : constant Variable_Value := Value (Project.Decl.Variables);

         begin
            if Result /= Nil_Variable_Value then
               return Result;
            end if;
         end;
      end if;

      --  We have not found the variable. It may be that the name
      --  is the name of an external project.

      declare
         The_Scan_State : Saved_Scan_State;

      begin
         Save_Scan_State (The_Scan_State);
         Scan;

         if Token /= Tok_Dot then
            Restore_Scan_State (The_Scan_State);

         else
            declare
               Current  : Project_List := Project.Imported_Projects;
               Element  : Project_Element;
               Imported : Project_Id := No_Project;

            begin
               if Project.Modifies /= No_Project then
                  if Projects.Table (Project.Modifies).Name = Name then
                     if Current_Verbosity = High then
                        Write_Str ("Found ");
                        Write_Line (Get_Name_String (Name));
                     end if;
                     Imported := Project.Modifies;
                  end if;
               end if;

               if Imported = No_Project then

                  while Current /= Empty_Project_List loop

                     Element := Project_Lists.Table (Current);

                     if Projects.Table (Element.Project).Name = Name then

                        if Current_Verbosity = High then
                           Write_Str ("Found ");
                           Write_Line (Get_Name_String (Name));
                        end if;

                        Imported := Element.Project;
                        exit;

                     else

                        Current := Element.Next;
                     end if;

                  end loop;

               end if;

               if Imported = No_Project then
                  Restore_Scan_State (The_Scan_State);
                  Error_Msg_BC
                    ("not a project imported by this project");
                  return Nil_Variable_Value;

               else
                  declare
                     The_Package  : Package_Id := No_Package;
                     Current_Decl : Declarations;
                     The_Value    : Prj.Variable_Value;

                  begin
                     loop
                        Scan;
                        Expect (Tok_Identifier, "identifier");

                        if Token /= Tok_Identifier then
                           return Nil_Variable_Value;

                        else
                           Name := Token_Name;

                           if The_Package = No_Package then
                              Current_Decl :=
                                Projects.Table (Imported).Decl;

                           else
                              Current_Decl :=
                                Packages.Table (The_Package).Decl;
                           end if;

                           The_Value :=
                             Value_Of (Name, Current_Decl.Variables);

                           case The_Value.Kind is
                              when Undefined =>
                                 null;

                              when Single =>
                                 return The_Value;

                              when List =>
                                 if not Do_Not_Skip then
                                    return The_Value;
                                 else
                                    return Duplicate (The_Value, Scan_Ptr);
                                 end if;
                           end case;

                           The_Package :=
                             Value_Of (Name, Current_Decl.Packages);

                           if The_Package = No_Package then
                              Error_Msg_BC
                                ("package or variable name expected");
                              return Nil_Variable_Value;
                           end if;

                           Scan;
                           Expect (Tok_Dot, ".");

                           if Token /= Tok_Dot then
                              return Nil_Variable_Value;
                           end if;
                        end if;
                     end loop;
                  end;
               end if;
            end;
         end if;
      end;

      Error_Msg_BC ("unknown variable");
      return Nil_Variable_Value;
   end Value_Of_Variable;

end Prj.Str;
