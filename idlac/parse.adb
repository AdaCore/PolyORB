--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Tokens; use Tokens;
with Types; use Types;
with Errors;

package body Parse is

   ---------------------
   --  Initialization --
   ---------------------

   procedure Initialize (Filename : in String;
                         Preprocess : in Boolean;
                         Keep_Temporary_Files : in Boolean) is
   begin
      Tokens.Initialize (Filename, Preprocess, Keep_Temporary_Files);
   end Initialize;


   --------------------------------------
   --  management of the token stream  --
   --------------------------------------

   --  This is a little buffer to put tokens if we have
   --  to look a bit further than the current_token.
   --  A second buffer is used to keep the location of each token,
   --  and a third one for their string value (usefull in case of
   --  an identifier ou a literal)

   --  buffer length
   Buffer_Length : constant Natural := 4;

   --  a type for indexes on the buffer
   type Buffer_Index is mod Buffer_Length;

   --  definition of a pointer on a string and the associated
   --  deallocation
   type String_Ptr is access String;
   procedure Free_String_Ptr is new Ada.Unchecked_Deallocation
     (Object => String,
      Name => String_Ptr);

   --  the buffers themself
   Token_Buffer : array (Buffer_Index) of Idl_Token
     := (others => T_Error);
   Location_Buffer : array (Buffer_Index) of Errors.Location
     := (others => (Filename => null, Line => 0, Col => 0));
   String_Buffer : array (Buffer_Index) of String_Ptr
     := (others => null);

   --  index of the current token in the buffer
   Current_Index : Buffer_Index := 0;

   --  index of the newest token in the buffer (could be different
   --  from the current token if we looked a bit further in the past)
   Newest_Index : Buffer_Index := 0;

   -----------------
   --  Get_Token  --
   -----------------
   function Get_Token return Idl_Token is
   begin
      return Token_Buffer (Current_Index);
   end Get_Token;

   ----------------------------
   --  Get_Token_From_Lexer  --
   ----------------------------
   procedure Get_Token_From_Lexer is
   begin
      Newest_Index := Newest_Index + 1;
      Token_Buffer (Newest_Index) := Tokens.Get_Next_Token;
      Location_Buffer (Newest_Index) := Tokens.Get_Lexer_Location;
      if String_Buffer (Newest_Index) /= null then
         Free_String_Ptr (String_Buffer (Newest_Index));
      end if;
      case Token_Buffer (Newest_Index) is
         when T_Lit_Decimal_Integer |
           T_Lit_Octal_Integer |
           T_Lit_Hexa_Integer |
           T_Lit_Simple_Char |
           T_Lit_Escape_Char |
           T_Lit_Octal_Char |
           T_Lit_Hexa_Char |
           T_Lit_Unicode_Char |
           T_Lit_Wide_Simple_Char |
           T_Lit_Wide_Escape_Char |
           T_Lit_Wide_Octal_Char |
           T_Lit_Wide_Hexa_Char |
           T_Lit_Wide_Unicode_Char |
           T_Lit_Simple_Floating_Point |
           T_Lit_Exponent_Floating_Point |
           T_Lit_Pure_Exponent_Floating_Point |
           T_Lit_String |
           T_Lit_Wide_String |
           T_Lit_Simple_Fixed_Point |
           T_Lit_Floating_Fixed_Point |
           T_Identifier |
           T_Pragma =>
            String_Buffer (Newest_Index) :=
             new String'(Tokens.Get_Lexer_String);
         when others =>
            null;
      end case;
   end Get_Token_From_Lexer;

   ------------------
   --  Next_Token  --
   ------------------
   procedure Next_Token is
   begin
      if Current_Index = Newest_Index then
         Get_Token_From_Lexer;
      end if;
      Current_Index := Current_Index + 1;
   end Next_Token;

   -----------------------
   --  View_Next_Token  --
   -----------------------
   function View_Next_Token return Idl_Token is
   begin
      if Current_Index = Newest_Index then
         Get_Token_From_Lexer;
      end if;
      return Token_Buffer (Current_Index + 1);
   end View_Next_Token;

   --------------------------
   --  Get_Token_Location  --
   --------------------------
   function Get_Token_Location return Errors.Location is
   begin
      return Location_Buffer (Current_Index);
   end Get_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------
   function Get_Previous_Token_Location return Errors.Location is
   begin
      return Location_Buffer (Current_Index - 1);
   end Get_Previous_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------
   function Get_Previous_Previous_Token_Location return Errors.Location is
   begin
      return Location_Buffer (Current_Index - 2);
   end Get_Previous_Previous_Token_Location;

   -------------------------------
   --  Get_Next_Token_Location  --
   -------------------------------
   function Get_Next_Token_Location return Errors.Location is
   begin
      return Location_Buffer (Current_Index + 1);
   end Get_Next_Token_Location;

   ------------------------
   --  Get_Token_String  --
   ------------------------
   function Get_Token_String return String is
   begin
      return String_Buffer (Current_Index).all;
   end Get_Token_String;

   ---------------------------------
   --  Get_Previous_Token_String  --
   ---------------------------------
   function Get_Previous_Token_String return String is
   begin
      return String_Buffer (Current_Index - 1).all;
   end Get_Previous_Token_String;

   -----------------------------
   --  Get_Next_Token_String  --
   -----------------------------
   function Get_Next_Token_String return String is
   begin
      return String_Buffer (Current_Index + 1).all;
   end Get_Next_Token_String;



   --------------------------
   --  Parsing of the idl  --
   --------------------------

   ---------------------------
   --  Parse_Specification  --
   ---------------------------
   function Parse_Specification return N_Repository_Acc is
      Result : N_Repository_Acc;
   begin
      Result := new N_Repository;
      Set_Location (Result.all, Get_Token_Location);
      --  The repository is the root scope.
      Push_Scope (Result);
      Next_Token;
      declare
         Definition : N_Root_Acc;
         Definition_Result : Boolean;
      begin
         while Get_Token /= T_Eof loop
            Parse_Definition (Definition, Definition_Result);
            if not Definition_Result then
               Go_To_Next_Definition;
            end if;
            if Definition /= null then
               Append_Node (Result.Contents,
                            Definition);
            end if;
         end loop;
      end;
      Pop_Scope;
      return Result;
   end Parse_Specification;

   ------------------------
   --  Parse_Definition  --
   ------------------------
   procedure Parse_Definition (Result : out N_Root_Acc;
                               Success : out Boolean) is
   begin
      case Get_Token is
         when T_Typedef
           | T_Struct
           | T_Union
           | T_Enum
           | T_Native =>
            Parse_Type_Dcl (Result, Success);
            if not Success then
               return;
            end if;
         when T_Const =>
            declare
               Res : N_Const_Acc;
            begin
               Parse_Const_Dcl (Res, Success);
               Result := N_Root_Acc (Res);
               if not Success then
                  return;
               end if;
            end;
         when T_Exception =>
            declare
               Res : N_Exception_Acc;
            begin
               Parse_Except_Dcl (Res, Success);
               Result := N_Root_Acc (Res);
               if not Success then
                  return;
               end if;
            end;
         when T_Abstract =>
            case View_Next_Token is
               when T_Interface =>
                  declare
                     Res : N_Named_Acc;
                  begin
                     Parse_Interface (Res, Success);
                     Result := N_Root_Acc (Res);
                     if not Success then
                        return;
                     end if;
                  end;
               when T_ValueType  =>
                  declare
                     Res : N_Named_Acc;
                  begin
                     Parse_Value (Res, Success);
                     Result := N_Root_Acc (Res);
                     if not Success then
                        return;
                     end if;
                  end;
               when others =>
                  declare
                     Loc : Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + 9;
                     Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                          "interface" &
                                          Ada.Characters.Latin_1.Quotation &
                                          " or " &
                                          Ada.Characters.Latin_1.Quotation &
                                          "valuetype" &
                                          Ada.Characters.Latin_1.Quotation &
                                          " expected after the " &
                                          "abstract keyword.",
                                          Errors.Error,
                                          Get_Token_Location);
                     Success := False;
                     Result := null;
                     --  consumes T_Abstract
                     Next_Token;
                     return;
                  end;
            end case;
         when T_Interface =>
            declare
               Res : N_Named_Acc;
            begin
               Parse_Interface (Res, Success);
               Result := N_Root_Acc (Res);
               if not Success then
                  return;
               end if;
            end;
         when T_Module =>
            declare
               Res : N_Module_Acc;
            begin
               Parse_Module (Res, Success);
               Result := N_Root_Acc (Res);
               if not Success then
                  return;
               end if;
            end;
         when T_ValueType
           | T_Custom =>
            declare
               Res : N_Named_Acc;
            begin
               Parse_Value (Res, Success);
               Result := N_Root_Acc (Res);
               if not Success then
                  return;
               end if;
            end;
         when others =>
            Errors.Parser_Error ("definition expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := null;
            Success := False;
            return;
      end case;
      if Get_Token /= T_Semi_Colon then
         Errors.Parser_Error ("';' expected at the end of a definition.",
                              Errors.Error,
                              Get_Token_Location);
         Success := False;
      else
         Next_Token;
      end if;
      return;
   end Parse_Definition;

   --------------------
   --  Parse_Module  --
   --------------------
   procedure Parse_Module (Result : out N_Module_Acc;
                           Success : out Boolean) is
   begin
      --  Is there an identifier ?
      Next_Token;
      case Get_Token is
         when  T_Identifier =>
            case View_Next_Token is
               when T_Left_Cbracket =>
                  --  Creation of the node
                  Result := new N_Module;
                  Types.Set_Location (Result.all,
                                      Get_Previous_Token_Location);
                  --  try to add the identifier to the scope
                  if not Types.Add_Identifier (Result,
                                               Get_Token_String) then
                     --  there is a name collision with the module name
                     declare
                        Loc : Errors.Location;
                     begin
                        Loc := Types.Get_Location
                          (Find_Identifier_Node (Get_Token_String).all);
                        Errors.Parser_Error
                          ("This module name is already defined in" &
                           " this scope : " &
                           Errors.Display_Location (Loc),
                           Errors.Error,
                           Get_Token_Location);
                     end;
                  end if;
                  --  consume the T_Left_Cbracket token
                  Next_Token;
                  --  parse the module body
                  Next_Token;
                  declare
                     Definition : N_Root_Acc;
                     Definition_Result : Boolean;
                  begin
                     Push_Scope (Result);
                     while Get_Token /= T_Right_Cbracket loop
                        --  try to parse a definition
                        Parse_Definition (Definition, Definition_Result);
                        if Definition_Result then
                           --  successfull
                           Append_Node (Result.Contents,
                                        Definition);
                        else
                           --  failed
                           Go_To_Next_Definition;
                        end if;
                     end loop;
                     Pop_Scope;
                     --  consume the T_Right_Cbracket token
                     Next_Token;
                  end;
                  --  end of the module body parsing
                  Success := True;
               when others =>
                  declare
                     Loc : Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + Get_Token_String'Length + 1;
                     Errors.Parser_Error ("'{' expected. ",
                                          Errors.Error,
                                          Loc);
                  end;
                  Result := null;
                  Success := False;
            end case;
         when others =>
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 7;
               Errors.Parser_Error ("Identifier expected. ",
                                    Errors.Error,
                                    Loc);
            end;
            Result := null;
            Success := False;
      end case;
      return;
   end Parse_Module;

   -----------------------
   --  Parse_Interface  --
   -----------------------
   procedure Parse_Interface (Result : out  N_Named_Acc;
                              Success : out Boolean) is
      Res : N_Interface_Acc;
      Fd_Res : N_Forward_Interface_Acc;
      Definition : Identifier_Definition_Acc;
   begin
      --  interface header.
      Res := new N_Interface;
      --  is the interface abstracted
      if Get_Token = T_Abstract then
         Res.Abst := True;
         --  the T_Interface token should "interface"
         --  (it is already checked)
         Next_Token;
      else
         Res.Abst := False;
      end if;
      Set_Location (Res.all, Get_Token_Location);
      Next_Token;
      --  Expect an identifier
      if Get_Token = T_Identifier then
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope !
         if Definition /= null
           and then Definition.Parent_Scope = Get_Current_Scope then
            --  is it a forward declaration
            if Get_Kind (Definition.Node.all) = K_Forward_Interface then
               --  Check if they are both of the same abstract kind
               if N_Forward_Interface_Acc (Definition.Node).Abst
                 /= Res.Abst then
                  declare
                     Loc : Errors.Location;
                  begin
                        Loc := Types.Get_Location
                          (N_Forward_Interface_Acc (Definition.Node).all);
                        Errors.Parser_Error
                          ("Forward declaration "
                           & Errors.Display_Location (Loc)
                           & " has not the same abstract type",
                           Errors.Error,
                           Get_Previous_Token_Location);
                  end;
               end if;
               Fd_Res := N_Forward_Interface_Acc (Get_Node (Definition));
               --  is this interface also a forward declaration
               --  FIXME >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
               if View_Next_Token /= T_Semi_Colon then
                  Fd_Res.Forward := Res;
                  Res.Forward := Fd_Res;
                  Redefine_Identifier (Definition, Res);
               end if;
            else
               declare
                  Loc : Errors.Location;
               begin
                  Loc := Types.Get_Location
                    (Find_Identifier_Node (Get_Token_String).all);
                  Errors.Parser_Error
                    ("This interface name is already declared in" &
                     " this scope : " &
                     Errors.Display_Location (Loc),
                     Errors.Error,
                     Get_Token_Location);
                  Success := False;
                  Result := null;
                  Fd_Res := null;
                  return;
               end;
            end if;
         else
            Fd_Res := null;
            Res.Forward := null;
            if not Add_Identifier (Res,
                                   Get_Token_String) then
               raise Errors.Internal_Error;
            end if;
            Definition := Find_Identifier_Definition (Get_Token_String);
         end if;
      else
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Errors.Parser_Error
              (" identifier expected after 'interface'",
               Errors.Error,
               Loc);
            Success := False;
            Result := null;
            return;
         end;
      end if;
      Next_Token;
      --  Hups, this was just a forward declaration.
      if Get_Token = T_Semi_Colon then
         --  is it another forward declaration
         if Fd_Res /= null then
            declare
               Loc : Errors.Location;
            begin
               Loc := Types.Get_Location (Fd_Res.all);
               Errors.Parser_Error
                 ("forward declaration already defined in" &
                  " this scope : " &
                  Errors.Display_Location (Loc),
                  Errors.Warning,
                  Get_Token_Location);
               Fd_Res := new N_Forward_Interface;
               Set_Location (Fd_Res.all, Get_Location (Res.all));
               Fd_Res.Forward := null;
               Fd_Res.Abst := Res.Abst;
               --  The first forward should be the right one
               --  not the last
               --  Redefine_Identifier (Definition, Fd_Res);
               Success := True;
               Result := N_Named_Acc (Fd_Res);
               return;
            end;
         else
            Fd_Res := new N_Forward_Interface;
            Set_Location (Fd_Res.all, Get_Location (Res.all));
            Fd_Res.Forward := null;
            Fd_Res.Abst := Res.Abst;
            Redefine_Identifier (Definition, Fd_Res);
            --  Free (Res); ???????????????????
            Result := N_Named_Acc (Fd_Res);
            Success := True;
            return;
         end if;
      else
         --  use the Interface4 rule
         Parse_Interface_Dcl_End (Res, Success);
         Result := N_Named_Acc (Res);
         return;
      end if;
      return;
   end Parse_Interface;


   ----------------------------
   --  Parse_Interface_Body  --
   ----------------------------
   procedure Parse_Interface_Body (List : in out Node_List) is
      Ite : Node_Iterator;
   begin
      Init (Ite, List);
--       loop
--          exit when Token = T_Right_Cbracket;
--          Parse_Export (List);
--       end loop;
   end Parse_Interface_Body;


   --------------------
   --  Parse_Export  --
   --------------------
   procedure Parse_Export (Result : out N_Root_Acc;
                           Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--       case Token is
--          when T_Readonly | T_Attribute =>
--             Parse_Attr_Dcl (List);
--          when T_Oneway | T_Void | T_Colon_Colon | T_Identifier |
--            T_Short | T_Long | T_Float | T_Double | T_Unsigned |
--            T_Char | T_Wchar | T_Boolean | T_Octet | T_Any | T_Object |
--            T_String | T_Wstring =>
--             Append_Node (List, N_Root_Acc (Parse_Op_Dcl));
--          when T_Right_Cbracket =>
--             return;
--          when T_Exception =>
--             Append_Node (List, N_Root_Acc (Parse_Except_Dcl));
--          when T_Union =>
--             Append_Node (List, N_Root_Acc (Parse_Union_Type));
--          when T_Struct =>
--             Append_Node (List, N_Root_Acc (Parse_Struct_Type));
--          when T_Enum =>
--             Append_Node (List, N_Root_Acc (Parse_Enum_Type));
--          when T_Typedef =>
--             Append_Node (List, Parse_Type_Dcl);
--          when others =>
--             Errors.Parser_Error ("declaration of an operation expected",
--                                  Errors.Error);
--             raise Parse_Error;
--       end case;
--       if Token /= T_Semi_Colon then
--          Errors.Parser_Error ("`;' expected here, found "
--                               & Idl_Token'Image (Token),
--                               Errors.Error);
--       else
--          Next_Token;
--       end if;
   end Parse_Export;

   --------------------------------
   --  Parse_Interface_Decl_End  --
   --------------------------------
   procedure Parse_Interface_Dcl_End (Result : in out N_Interface_Acc;
                                     Success : out Boolean) is
   begin
      --  interface header.
      if Get_Token = T_Colon then
         --  inheritance_spec
         loop
            Next_Token;
            declare
               Scoped_Success : Boolean;
               Name : N_Scoped_Name_Acc;
            begin
               --  FIXME : no test on scoped_success
               Parse_Scoped_Name (Name, Scoped_Success);
               Append_Node (Result.Parents, N_Root_Acc (Name));
            end;
            exit when Get_Token /= T_Comma;
         end loop;
      end if;

      if Get_Token = T_Left_Cbracket then
         Next_Token;
      else
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length + 1;
            Errors.Parser_Error
              ("'{' expected",
               Errors.Error,
               Loc);
            Success := False;
            return;
         end;
      end if;
      --  Create a scope for the interface.
      Push_Scope (Result);
      Parse_Interface_Body (Result.Contents);
      Pop_Scope;
      --  consume the right bracket at the end of the interface body
      --  verification of the presence of this bracket was done
      --  in Parse_Interface_Body
      Next_Token;
      Success := True;
      return;
   end Parse_Interface_Dcl_End;


   -------------------------
   --  Parse_Scoped_Name  --
   -------------------------
   procedure Parse_Scoped_Name (Result : out N_Scoped_Name_Acc;
                                Success : out Boolean) is
--       Res, Prev : N_Scoped_Name_Acc;
--       Scope : N_Scope_Acc;
--       Name : N_Named_Acc;
   begin
      Result := null;
      Success := False;
--       Prev := null;
--       Res := new N_Scoped_Name;
--       Set_Location (Res.all, Get_Location);
--       if Token = T_Colon_Colon then
--          Scope := Get_Root_Scope;
--       else
--          Name := Find_Identifier_Node;
--          Next_Token;
--          if Token /= T_Colon_Colon then
--             if Name = null then
--                raise Errors.Internal_Error;
--             end if;
--             Res.Value := Name;
--             return Res;
--          end if;
--       end if;
--       loop
--          if Name.all not in N_Scope'Class then
--             Errors.Parser_Error ("identifier is not a scope",
--                                   Errors.Error);
--             raise Errors.Internal_Error;
--          end if;
--          Next_Token;
--          Scope := N_Scope_Acc (Name);
--          Expect (T_Identifier);
--          Name := Find_Identifier_Node (Scope);
--          Next_Token;
--          exit when Token /= T_Colon_Colon;
--       end loop;
--       if Name = null then
--          raise Errors.Internal_Error;
--       end if;
--       Res.Value := Name;
--       return Res;
   end Parse_Scoped_Name;

   -------------------
   --  Parse_Value  --
   -------------------
   procedure Parse_Value (Result : out N_Named_Acc;
                          Success : out Boolean) is
   begin
      case Get_Token is
         when T_Custom =>
            Next_Token;
            declare
               Res : N_ValueType_Acc;
            begin
               Parse_Custom_Value (Res, Success);
               Result := N_Named_Acc (Res);
            end;
         when T_Abstract =>
            Next_Token;
            Parse_Abstract_Value (Result, Success);
         when T_ValueType =>
            Parse_Direct_Value (Result, Success);
         when others =>
            raise Errors.Internal_Error;
      end case;
      return;
   end Parse_Value;

   --------------------------
   --  Parse_Custom_Value  --
   --------------------------
   procedure Parse_Custom_Value (Result : out N_ValueType_Acc;
                                 Success : out Boolean) is
   begin
      if Get_Token /= T_ValueType then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " expected after custom keyword.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := null;
         Success := False;
      else
         Next_Token;
         if Get_Token /= T_Identifier then
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Errors.Parser_Error ("identifier expected.",
                                    Errors.Error,
                                    Loc);
            end;
            Result := null;
            Success := False;
         else
            Parse_End_Value_Dcl (Result, Success, True, False);
         end if;
      end if;
      return;
   end Parse_Custom_Value;

   ----------------------------
   --  Parse_Abstract_Value  --
   ----------------------------
   procedure Parse_Abstract_Value (Result : out N_Named_Acc;
                                   Success : out Boolean) is
   begin
      if Get_Token /= T_ValueType then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 9;
            Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 "expected after abstract keyword.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := null;
         Success := False;
      else
         Next_Token;
         if Get_Token /= T_Identifier then
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Errors.Parser_Error ("identifier expected.",
                                    Errors.Error,
                                    Loc);
            end;
            Result := null;
            Success := False;
         else
            case View_Next_Token is
               when T_Colon
                 | T_Supports
                 | T_Left_Cbracket =>
                  declare
                     Res : N_ValueType_Acc;
                  begin
                     Parse_End_Value_Dcl (Res, Success, False, true);
                     Result := N_Named_Acc (Res);
                  end;
               when T_Semi_Colon =>
                  declare
                     Res : N_Forward_ValueType_Acc;
                  begin
                     Parse_End_Value_Forward_Dcl (Res, Success, True);
                     Result := N_Named_Acc (Res);
                  end;
               when others =>
                  declare
                     Loc : Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + Get_Token_String'Length;
                     Errors.Parser_Error ("Bad value definition. " &
                                          "Inheritance specification, '{'" &
                                          " or ';' expected.",
                                          Errors.Error,
                                          Loc);
                  end;
                  Result := null;
                  Success := False;
            end case;
         end if;
      end if;
      return;
   end Parse_Abstract_Value;

   --------------------------
   --  Parse_Direct_Value  --
   --------------------------
   procedure Parse_Direct_Value (Result : out N_Named_Acc;
                                 Success : out Boolean) is
   begin
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Errors.Parser_Error ("identifier expected.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := null;
         Success := False;
      else
         case View_Next_Token is
            when T_Float
              | T_Double
              | T_Long
              | T_Short
              | T_Unsigned
              | T_Char
              | T_Wchar
              | T_Boolean
              | T_Octet
              | T_Any
              | T_Object
              | T_ValueBase
              | T_Sequence
              | T_String
              | T_Wstring
              | T_Fixed
              | T_Identifier
              | T_Colon_Colon
              | T_Struct
              | T_Union
              | T_Enum =>
               declare
                  Res : N_Boxed_ValueType_Acc;
               begin
                  Parse_End_Value_Box_Dcl (Res, Success);
                  Result := N_Named_Acc (Res);
               end;
            when T_Semi_Colon =>
               declare
                  Res : N_Forward_ValueType_Acc;
               begin
                  Parse_End_Value_Forward_Dcl (Res, Success, False);
                  Result := N_Named_Acc (Res);
               end;
            when T_Colon
              | T_Supports
              | T_Left_Cbracket =>
               declare
                  Res : N_ValueType_Acc;
               begin
                  Parse_End_Value_Dcl (Res, Success, False, False);
                  Result := N_Named_Acc (Res);
               end;
            when others =>
               declare
                  Loc : Errors.Location;
               begin
                  Loc := Get_Token_Location;
                  Loc.Col := Loc.Col + Get_Token_String'Length;
                  Errors.Parser_Error ("Bad value definition. " &
                                       "Type, inheritance specification, " &
                                       "'{' or ';' expected.",
                                       Errors.Error,
                                       Loc);
               end;
               Result := null;
               Success := False;
         end case;
      end if;
      return;
   end Parse_Direct_Value;

   ---------------------------
   --  Parse_End_Value_Dcl  --
   ---------------------------
   procedure Parse_End_Value_Dcl (Result : out N_ValueType_Acc;
                                  Success : out Boolean;
                                  Custom : in Boolean;
                                  Abst : in Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := new N_ValueType;
      Result.Abst := Abst;
      Result.Custom := Custom;
      if (Abst or Custom) then
         Set_Location (Result.all, Get_Previous_Previous_Token_Location);
      else
         Set_Location (Result.all, Get_Previous_Token_Location);
      end if;
      --  try to find a previous definition
      Definition := Find_Identifier_Definition (Get_Token_String);
      --  Is there a previous definition and in the same scope ?
      if Definition /= null
        and then Definition.Parent_Scope = Get_Current_Scope then
         --  There was probably a forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            declare
               Fd_Decl : N_Forward_ValueType_Acc;
            begin
               Fd_Decl := N_Forward_ValueType_Acc (Get_Node (Definition));
               Fd_Decl.Forward := Result;
               Result.Forward := Fd_Decl;
               Redefine_Identifier (Definition, Result);
            end;
         else
            Errors.Parser_Error
            ("The identifier used for this valuetype is already "
             & "defined in the same scope : " &
             Errors.Display_Location (Get_Location (Definition.Node.all)),
             Errors.Error,
             Get_Token_Location);
            Result.Forward := null;
         end if;
      else
         --  no previous definition
         Result.Forward := null;
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
      end if;
      Next_Token;
      if Get_Token = T_Colon
        or Get_Token = T_Supports then
         --  there is some inheritance specification here
         declare
            Inherit_Success : Boolean;
         begin
            Parse_Value_Inheritance_Spec (Result, Inherit_Success);
            if not Inherit_Success then
               Go_To_Next_Left_Cbracket;
            end if;
         end;
      end if;
      if Get_Token /= T_Left_Cbracket then
         --  only possible after some inheritance specification
         --  else, it was already tested before
         --  Then, the previous token is an identifier
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
                  Errors.Parser_Error ("Bad value definition. " &
                                       "'{' expected.",
                                       Errors.Error,
                                       Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      Push_Scope (Result);
      while Get_Token /= T_Right_Cbracket loop
         declare
            Element_Success : Boolean;
            Element : N_Root_Acc;
         begin
            if Abst then
               --  rule Value5
               Parse_Export (Element, Element_Success);
            else
               --  rule Value6
                  Parse_Value_Element (Element, Element_Success);
            end if;
            if not Element_Success then
               Go_To_Next_Value_Element;
            else
               Append_Node (Result.Contents, Element);
            end if;
         end;
      end loop;
      Pop_Scope;
      --  consumes the right Cbracket
      Next_Token;
      Success := True;
      return;
   end Parse_End_Value_Dcl;

   -----------------------------------
   --  Parse_End_Value_Forward_Dcl  --
   -----------------------------------
   procedure Parse_End_Value_Forward_Dcl (Result : out N_Forward_ValueType_Acc;
                                          Success : out Boolean;
                                          Abst : in Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := new N_Forward_ValueType;
      Result.Abst := Abst;
      if Abst then
         Set_Location (Result.all, Get_Previous_Previous_Token_Location);
      else
         Set_Location (Result.all, Get_Previous_Token_Location);
      end if;
      --  try to find a previous definition
      Definition := Find_Identifier_Definition (Get_Token_String);
      --  Is there a previous definition and in the same scope ?
      if Definition /= null
        and then Definition.Parent_Scope = Get_Current_Scope then
         --  There was probably a previous forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            --  nothing to do : this new forward declaration is useless
            Errors.Parser_Error
              ("This valuetype was already declared forward : " &
               Errors.Display_Location (Get_Location (Definition.Node.all)),
               Errors.Warning,
               Get_Token_Location);
         else
            Errors.Parser_Error
            ("The identifier used for this valuetype is already "
             & "defined in the same scope : " &
             Errors.Display_Location (Get_Location (Definition.Node.all)),
             Errors.Error,
             Get_Token_Location);
         end if;
      else
         --  no previous forward
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
      end if;
      --  consumes the identifier
      Next_Token;
      Success := True;
      return;
   end Parse_End_Value_Forward_Dcl;

   -------------------------------
   --  Parse_End_Value_Box_Dcl  --
   -------------------------------
   procedure Parse_End_Value_Box_Dcl (Result : out N_Boxed_ValueType_Acc;
                                      Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := new N_Boxed_ValueType;
      Set_Location (Result.all, Get_Previous_Token_Location);
      --  try to find a previous definition
      Definition := Find_Identifier_Definition (Get_Token_String);
      --  Is there a previous definition and in the same scope ?
      if Definition /= null
        and then Definition.Parent_Scope = Get_Current_Scope then
         --  There was probably a previous forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            Errors.Parser_Error
              ("This valuetype was forward declared : " &
               Errors.Display_Location (Get_Location (Definition.Node.all)) &
               ". It can not be a boxed one.",
               Errors.Error,
               Get_Previous_Token_Location);
         else
            Errors.Parser_Error
              ("The identifier used for this valuetype is already "
               & "defined in the same scope : " &
               Errors.Display_Location (Get_Location (Definition.Node.all)),
               Errors.Error,
               Get_Previous_Token_Location);
         end if;
      else
         --  no previous forward
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
      end if;
      --  consumes the identifier
      Next_Token;
      Parse_Type_Spec (Result.Boxed_Type, Success);
      return;
   end Parse_End_Value_Box_Dcl;

   -------------------------------
   --  Parse_End_Value_Box_Dcl  --
   -------------------------------
   procedure Parse_Value_Inheritance_Spec (Result : in out N_ValueType_Acc;
                                           Success : out Boolean) is
   begin
      if Get_Token = T_Colon then
         Next_Token;
         if Get_Token = T_Truncatable then
            if Result.Abst then
               Errors.Parser_Error
                 ("The truncatable modifier may not " &
                  "be used in an abstract value.",
                  Errors.Error,
                  Get_Token_Location);
            elsif Result.Custom then
               Errors.Parser_Error
                 ("The truncatable modifier may not " &
                  "be used in a custom value.",
                  Errors.Error,
                  Get_Token_Location);
            else
               Result.Truncatable := True;
            end if;
            Next_Token;
         end if;
         --  parse value inheritance
         declare
            Name : N_Scoped_Name_Acc;
            Name_Success : Boolean;
         begin
            Parse_Value_Name (Name, Name_Success);
            if Name_Success then
               case Get_Kind (Name.Value.all) is
                  when K_ValueType =>
                     if Result.Abst then
                        if not N_ValueType_Acc (Name.Value).Abst then
                           Errors.Parser_Error
                             ("An abstract value may not inherit from a " &
                              "stateful one.",
                              Errors.Error,
                              Get_Token_Location);
                        end if;
                     else
                        if N_ValueType_Acc (Name.Value).Abst then
                           Errors.Parser_Error
                             ("The truncatable modifier may not be used " &
                              "for an abstract value inheritance.",
                              Errors.Error,
                              Get_Token_Location);
                        end if;
                     end if;
                     Append_Node (Result.Parents, N_Root_Acc (Name));
                     Next_Token;
                  when K_Forward_ValueType =>
                     Errors.Parser_Error
                       ("A value may not inherit from a forward declared" &
                        " value whose definition has not yet been seen.",
                        Errors.Error,
                        Get_Token_Location);
                  when K_Boxed_ValueType =>
                     Errors.Parser_Error
                       ("A value may not inherit from a boxed value.",
                        Errors.Error,
                        Get_Token_Location);
                  when K_Interface
                    | K_Forward_Interface =>
                     Errors.Parser_Error
                       ("A value may not inherit from an interface. "&
                        "It can only support it.",
                        Errors.Error,
                        Get_Token_Location);
                  when others =>
                     declare
                        Loc : Errors.Location;
                     begin
                        Loc := Get_Previous_Token_Location;
                        Loc.Col := Loc.Col + 2;
                        Errors.Parser_Error
                          ("Value name expected.",
                           Errors.Error,
                           Loc);
                     end;
               end case;
            else
               Go_To_Next_Left_Cbracket;
               Success := False;
               return;
            end if;
         end;
         while Get_Token = T_Comma loop
            Next_Token;
            declare
               Name : N_Scoped_Name_Acc;
               Name_Success : Boolean;
            begin
               Parse_Value_Name (Name, Name_Success);
               if Name_Success then
                  case Get_Kind (Name.Value.all) is
                     when K_ValueType =>
                        if Is_In_List (Result.Parents, N_Root_Acc (Name)) then
                           --  already inherited
                           Errors.Parser_Error
                             ("Already inherited of this value.",
                              Errors.Error,
                              Get_Token_Location);
                        else
                           if not N_ValueType_Acc (Name.Value).Abst then
                              Errors.Parser_Error
                                ("A stateful value may only derive from a " &
                                 "single stateful value and this one must " &
                                 "be the first element in the inheritance.",
                                 Errors.Error,
                                 Get_Token_Location);
                           end if;
                           Append_Node (Result.Parents, N_Root_Acc (Name));
                        end if;
                        Next_Token;
                     when K_Forward_ValueType =>
                        Errors.Parser_Error
                          ("A value may not inherit from a forward declared" &
                           " value whose definition has not yet been seen.",
                           Errors.Error,
                           Get_Token_Location);
                     when K_Boxed_ValueType =>
                        Errors.Parser_Error
                          ("A value may not inherit from a boxed value.",
                           Errors.Error,
                           Get_Token_Location);
                     when K_Interface
                        | K_Forward_Interface =>
                        Errors.Parser_Error
                          ("A value may not inherit from an interface. "&
                           "It can only support it.",
                        Errors.Error,
                           Get_Token_Location);
                     when others =>
                        declare
                           Loc : Errors.Location;
                        begin
                           Loc := Get_Previous_Token_Location;
                           Loc.Col := Loc.Col + 2;
                           Errors.Parser_Error
                             ("Value name expected.",
                              Errors.Error,
                              Loc);
                        end;
                  end case;
               else
                  Go_To_Next_Left_Cbracket;
                  Success := False;
                  return;
               end if;
            end;
         end loop;
      end if;
      --  since we entered this method after reading T_colon or
      --  T_Supports, we should have T_Supports now
      case Get_Token is
         when T_Supports =>
            Next_Token;
            declare
               Non_Abstract_Interface : Boolean := False;
            begin
               --  parse interface inheritance
               declare
                  Name : N_Scoped_Name_Acc;
                  Name_Success : Boolean;
               begin
                  Parse_Value_Name (Name, Name_Success);
                  if Name_Success then
                     case Get_Kind (Name.Value.all) is
                        when K_Interface =>
                           if not N_Interface_Acc (Name.Value).Abst then
                              Non_Abstract_Interface := True;
                           end if;
                           Append_Node (Result.Supports, N_Root_Acc (Name));
                           Next_Token;
                        when K_Forward_Interface =>
                           Errors.Parser_Error
                             ("A value may not support a forward declared" &
                              " interface whose declaration has not yet " &
                              "been seen.",
                              Errors.Error,
                              Get_Token_Location);
                        when K_Boxed_ValueType
                          | K_ValueType
                          | K_Forward_ValueType =>
                           Errors.Parser_Error
                             ("A value may not support another value. " &
                              " However, it can inherit from it.",
                              Errors.Error,
                              Get_Token_Location);
                        when others =>
                           declare
                              Loc : Errors.Location;
                           begin
                              Loc := Get_Previous_Token_Location;
                              Loc.Col := Loc.Col + 9;
                              Errors.Parser_Error
                                ("Value name expected.",
                                 Errors.Error,
                                 Loc);
                           end;
                     end case;
                  else
                     Go_To_Next_Left_Cbracket;
                     Success := False;
                     return;
                  end if;
               end;
               while Get_Token = T_Comma loop
                  Next_Token;
                  declare
                     Name : N_Scoped_Name_Acc;
                     Name_Success : Boolean;
                  begin
                     Parse_Value_Name (Name, Name_Success);
                     if Name_Success then
                        case Get_Kind (Name.Value.all) is
                           when K_Interface =>
                              if not Result.Abst
                                and then not N_Interface_Acc
                                (Name.Value).Abst then
                                 if Non_Abstract_Interface then
                                    Errors.Parser_Error
                                      ("A stateful value may support only " &
                                       "one non abstract interface. This " &
                                       "is the second one.",
                                       Errors.Error,
                                       Get_Token_Location);
                                 else
                                    Non_Abstract_Interface := True;
                                 end if;
                              end if;
                              Append_Node (Result.Supports, N_Root_Acc (Name));
                              Next_Token;
                           when K_Forward_Interface =>
                              Errors.Parser_Error
                                ("A value may not support a forward declared" &
                                 " interface whose declaration has not yet " &
                                 "been seen.",
                                 Errors.Error,
                                 Get_Token_Location);
                           when K_Boxed_ValueType
                             | K_ValueType
                             | K_Forward_ValueType =>
                              Errors.Parser_Error
                                ("A value may not support another value. " &
                                 " However, it can inherit from it.",
                                 Errors.Error,
                                 Get_Token_Location);
                           when others =>
                              declare
                                 Loc : Errors.Location;
                              begin
                                 Loc := Get_Previous_Token_Location;
                                 Loc.Col := Loc.Col + 9;
                                 Errors.Parser_Error
                                   ("Value name expected.",
                                    Errors.Error,
                                    Loc);
                              end;
                        end case;
                     else
                        Go_To_Next_Left_Cbracket;
                        Success := False;
                        return;
                     end if;
                  end;
               end loop;
            end;
         when T_Left_Cbracket =>
            Success := True;
            return;
         when others =>
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
               Errors.Parser_Error
                 ("',', " &
                  Ada.Characters.Latin_1.Quotation &
                  "supports" &
                  Ada.Characters.Latin_1.Quotation &
                  " or '{' expected.",
                  Errors.Error,
                  Loc);
               Success := False;
               return;
            end;
      end case;
   end Parse_Value_Inheritance_Spec;

   ---------------------------
   --  Parse_Value_Element  --
   ---------------------------
   procedure Parse_Value_Element  (Result : out N_Root_Acc;
                                   Success : out Boolean) is
   begin
      case Get_Token is
         when T_Typedef
           | T_Struct
           | T_Union
           | T_Enum
           | T_Native
           | T_Const
           | T_Exception
           | T_Readonly
           | T_Attribute
           | T_Oneway
           | T_Void
           | T_Float
           | T_Double
           | T_Long
           | T_Short
           | T_Unsigned
           | T_Char
           | T_Wchar
           | T_Boolean
           | T_Octet
           | T_Any
           | T_Object
           | T_ValueBase
           | T_String
           | T_Wstring
           | T_Identifier
           | T_Colon_Colon =>
            Parse_Export (Result, Success);
         when T_Public
           | T_Private =>
            declare
               Res : N_State_Member_Acc;
            begin
               Parse_State_Member (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Factory =>
            declare
               Res : N_Initializer_Acc;
            begin
               Parse_Init_Dcl (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            Errors.Parser_Error ("value_element expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := null;
            Success := False;
            return;
      end case;
   end Parse_Value_Element;

   --------------------------
   --  Parse_State_Member  --
   --------------------------
   procedure Parse_State_Member (Result : out N_State_Member_Acc;
                                 Success : out Boolean) is
   begin
      Result := new N_State_Member;
      Set_Location (Result.all, Get_Token_Location);
      case Get_Token is
         when T_Public =>
            Result.Is_Public := True;
         when T_Private =>
            Result.Is_Public := False;
         when others =>
            raise Errors.Internal_Error;
      end case;
      Next_Token;
      Parse_Type_Spec (Result.State_Type, Success);
      if not Success then
         Go_To_End_Of_State_Member;
         return;
      end if;
      Parse_Declarators (Result.State_Declarators,
                         Success);
      if not Success then
         Go_To_End_Of_State_Member;
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Errors.Parser_Error ("missing ';' at the end of the state " &
                              "declaration.",
                              Errors.Error,
                              Get_Token_Location);
         Success := False;
      else
         Next_Token;
      end if;
      return;
   end Parse_State_Member;

   ----------------------
   --  Parse_Init_Dcl  --
   ----------------------
   procedure Parse_Init_Dcl (Result : out N_Initializer_Acc;
                             Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      if View_Next_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + 8;
            Errors.Parser_Error ("Identifier expected after keyword " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "factory" &
                                 Ada.Characters.Latin_1.Quotation &
                                 ".",
                                 Errors.Error,
                                 Loc);
         end;
         Success := False;
         return;
      end if;
      Result := new N_Initializer;
      Set_Location (Result.all, Get_Token_Location);
      --  consume T_Factory
      Next_Token;
      --  try to find a previous definition
      Definition := Find_Identifier_Definition (Get_Token_String);
      --  Is there a previous definition and in the same scope ?
      if Definition /= null
        and then Definition.Parent_Scope = Get_Current_Scope then
         Errors.Parser_Error
           ("The identifier used for this initializer is already "
            & "defined in the same scope : " &
            Errors.Display_Location (Get_Location (Definition.Node.all)),
            Errors.Error,
            Get_Token_Location);
      else
         --  no previous definition
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Parser_Error ("missing '(' in initializer declaration.",
                                 Errors.Error,
                                 Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token /= T_Right_Paren then
         --  there is some parameters
         declare
            Decls_Success : Boolean;
         begin
            Push_Scope (Result);
            Parse_Init_Param_Decls (Result.Param_Decls, Decls_Success);
            Pop_Scope;
            if not Decls_Success then
               Go_To_Next_Right_Paren;
            else
               if Get_Token /= T_Right_Paren then
                  Errors.Parser_Error ("missing ')' at the end of " &
                                       "initializer declaration.",
                                       Errors.Error,
                                       Get_Token_Location);
                  Success := False;
                  return;
               end if;
            end if;
         end;
      end if;
      --  consumes the T_Right_Parenthesis
      Next_Token;
      if Get_Token /= T_Semi_Colon then
         Errors.Parser_Error ("missing ';' at the end of initializer " &
                              "declaration.",
                              Errors.Error,
                              Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Init_Dcl;

   ------------------------
   --  Parse_Init_Decls  --
   ------------------------
   procedure Parse_Init_Param_Decls (Result : out Node_List;
                                     Success : out Boolean) is
   begin
      Result := Nil_List;
      declare
         Decl : N_Param_Acc;
         Decl_Success : Boolean;
      begin
         Parse_Init_Param_Decl (Decl, Decl_Success);
         if Decl_Success then
            Append_Node (Result, N_Root_Acc (Decl));
         else
            Success := False;
            return;
         end if;
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Decl : N_Param_Acc;
            Decl_Success : Boolean;
         begin
            Parse_Init_Param_Decl (Decl, Decl_Success);
            if Decl_Success then
               Append_Node (Result, N_Root_Acc (Decl));
            else
               Success := False;
               return;
            end if;
         end;
      end loop;
      Success := True;
      return;
   end Parse_Init_Param_Decls;

   -----------------------
   --  Parse_Init_Decl  --
   -----------------------
   procedure Parse_Init_Param_Decl (Result : out N_Param_Acc;
                                    Success : out Boolean) is
   begin
      case Get_Token is
         when T_In =>
            Next_Token;
         when T_Out
           | T_Inout =>
            Errors.Parser_Error ("an initializer parameter can only be " &
                                 "in mode " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "in" &
                                 Ada.Characters.Latin_1.Quotation &
                                 ".",
                                 Errors.Error,
                                 Get_Token_Location);
            Next_Token;
         when T_Float
           | T_Double
           | T_Long
           | T_Short
           | T_Unsigned
           | T_Char
           | T_Wchar
           | T_Boolean
           | T_Octet
           | T_Any
           | T_Object
           | T_ValueBase
           | T_String
           | T_Wstring
           | T_Identifier
           | T_Colon_Colon =>
            Errors.Parser_Error ("an initializer parameter should begin " &
                                 "with keyword " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "in" &
                                 Ada.Characters.Latin_1.Quotation &
                                 ".",
                                 Errors.Error,
                                 Get_Token_Location);
         when others =>
            Errors.Parser_Error ("bad initializer parameter declaration.",
                                 Errors.Error,
                                 Get_Token_Location);
            Success := False;
            Result := null;
            return;
      end case;
      Result := new N_Param;
      Set_Location (Result.all, Get_Previous_Token_Location);
      Result.Mode := Mode_In;
      Parse_Param_Type_Spec (Result.Param_Type, Success);
      if not Success then
         return;
      end if;
      Parse_Simple_Declarator (Result, Success);
      return;
   end Parse_Init_Param_Decl;

   -----------------------
   --  Parse_Const_Dcl  --
   -----------------------
   procedure Parse_Const_Dcl (Result : out N_Const_Acc;
                              Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--    function Parse_Const_Dcl return N_Const_Acc is
--       Res : N_Const_Acc;
--    begin
--       Res := new N_Const;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_Const);
--       Next_Token;
--       Res.C_Type := Parse_Const_Type;
--       Expect (T_Identifier);
--       Add_Identifier (Res);
--       Scan_Expect (T_Equal);
--       Next_Token;
--       Res.Expr := Parse_Const_Exp;
--       return Res;
--    end Parse_Const_Dcl;
   end Parse_Const_Dcl;

   ----------------------
   --  Parse_Type_Dcl  --
   ----------------------
   procedure Parse_Type_Dcl (Result : out N_Root_Acc;
                             Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--    function Parse_Type_Dcl return N_Root_Acc is
--    begin
--       case Token is
--          when T_Typedef =>
--             Next_Token;
--             return N_Root_Acc (Parse_Type_Declarator);
--          when T_Struct =>
--             return N_Root_Acc (Parse_Struct_Type);
--          when T_Union =>
--             return N_Root_Acc (Parse_Union_Type);
--          when T_Enum =>
--             return N_Root_Acc (Parse_Enum_Type);
--          when T_Native =>
--             declare
--                Res : N_Native_Acc;
--             begin
--                Res := new N_Native;
--                Set_Location (Res.all, Get_Location);
--                Expect (T_Native);
--                Next_Token;
--                Res.Decl := Parse_Declarator;
--                if Res.Decl.Array_Bounds /= Nil_List then
--                   Errors.Parser_Error ("simple declarator expected",
--                                        Errors.Error);
--                end if;
--                return N_Root_Acc (Res);
--             end;
--          when others =>
--             Errors.Parser_Error ("type declarator expected",
--                                  Errors.Error);
--             raise Parse_Error;
--       end case;
--    end Parse_Type_Dcl;
   end Parse_Type_Dcl;

   -----------------------
   --  Parse_Type_Spec  --
   -----------------------
   procedure Parse_Type_Spec (Result : out N_Root_Acc;
                              Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--       case Token is
--          when T_Float | T_Double | T_Long | T_Short | T_Unsigned |
--            T_Char | T_Wchar | T_Boolean | T_Octet | T_Any | T_Object |
--            T_Sequence | T_String | T_Wstring | T_Fixed |
--            T_Colon_Colon | T_Identifier =>
--             return Parse_Simple_Type_Spec;
--          when T_Enum | T_Struct | T_Union =>
--             return Parse_Constr_Type_Spec;
--          when others =>
--             Errors.Parser_Error ("type specifier expected",
--                                   Errors.Error);
--             raise Parse_Error;
--       end case;
   end  Parse_Type_Spec;

   -------------------------
   --  Parse_Declarators  --
   -------------------------
   procedure Parse_Declarators (Result : out N_Declarator_Acc;
                               Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--    procedure Parse_Declarators (List : in out Node_List) is
--       El : N_Declarator_Acc;
--    begin
--       loop
--          El := Parse_Declarator;
--          Append_Node (List, N_Root_Acc (El));
--          exit when Token /= T_Comma;
--          Next_Token;
--       end loop;
   end Parse_Declarators;

   -------------------------------
   --  Parse_Simple_Declarator  --
   -------------------------------
   procedure Parse_Simple_Declarator (Result : in out N_Param_Acc;
                                      Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      if Get_Token /= T_Identifier then
         Errors.Parser_Error ("Identifier expected in parameter " &
                              "declaration.",
                              Errors.Error,
                              Get_Token_Location);
         Success := False;
         return;
      else
         --  try to find a previous definition
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then Definition.Parent_Scope = Get_Current_Scope then
            Errors.Parser_Error
              ("This identifier is already used in this parameters " &
               "declaration : " &
               Errors.Display_Location (Get_Location (Definition.Node.all)),
               Errors.Error,
               Get_Token_Location);
         else
            --  no previous definition
            if not Add_Identifier (Result,
                                   Get_Token_String) then
               raise Errors.Internal_Error;
            end if;
         end if;
      end if;
      Success := True;
      return;
   end Parse_Simple_Declarator;

   ------------------------
   --  Parse_Except_Dcl  --
   ------------------------
   procedure Parse_Except_Dcl (Result : out N_Exception_Acc;
                               Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--    function Parse_Except_Dcl return N_Exception_Acc is
--       Res : N_Exception_Acc;
--    begin
--       Expect (T_Exception);
--       Res := new N_Exception;
--       Set_Location (Res.all, Get_Location);
--       Scan_Expect (T_Identifier);
--       Add_Identifier (Res);
--       Scan_Expect (T_Left_Cbracket);
--       Next_Token;
--       while Token /= T_Right_Cbracket loop
--          Parse_Member_List (Res.Members);
--       end loop;
--       Next_Token;
--       return Res;
   end Parse_Except_Dcl;

   -----------------------------
   --  Parse_Param_Type_Spec  --
   -----------------------------
   procedure Parse_Param_Type_Spec (Result : out N_Root_Acc;
                                    Success : out Boolean) is
   begin
      Result := null;
      Success := False;
--       case Token is
--          when T_String =>
--             return N_Root_Acc (Parse_String_Type);
--          when T_Wstring =>
--             return Parse_Wide_String_Type;
--          when T_Fixed =>
--             return Parse_Fixed_Pt_Type;
--          when T_Colon_Colon | T_Identifier =>
--             return N_Root_Acc (Parse_Scoped_Name);
--          when T_Float | T_Double | T_Long | T_Short | T_Unsigned | T_Char
--            | T_Wchar | T_Boolean | T_Octet | T_Any | T_Object =>
--             return Parse_Base_Type_Spec;
--          when others =>
--             Errors.Parser_Error ("param type specifier expected",
--                                   Errors.Error);
--             raise Errors.Internal_Error;
--       end case;
   end Parse_Param_Type_Spec;




--    --  FIXME: to add: rules 25, 26, 81, 82.

--    function Parse_Param_Type_Spec return N_Root_Acc;
--    function Parse_Const_Exp return N_Root_Acc;
--    function Parse_Sequence_Type return N_Sequence_Acc;
--    function Parse_Constr_Type_Spec return N_Root_Acc;
--    function Parse_Module return N_Module_Acc;



--    --  Rule 24:
--    --  <literal> ::= <integer_literal>
--    --            | <string_literal>
--    --            | <wide_string_literal>
--    --            | <character_literal>
--    --            | <wide_character_literal>
--    --            | <fixed_pt_literal>
--    --            | <floating_pt_literal>
--    --            | <boolean_literal>
--    function Parse_Literal return N_Root_Acc is
--    begin
--       case Token is
--          when T_Lit_Decimal_Integer =>
--             declare
--                Res : N_Lit_Integer_Acc;
--             begin
--                Res := new N_Lit_Integer;
--                Res.Lit := new String'(Tokens.Get_Token_String);
--                Next_Token;
--                return N_Root_Acc (Res);
--             end;
--          when T_Lit_Simple_Floating_Point =>
--             declare
--                Res : N_Lit_Floating_Point_Acc;
--             begin
--                Res := new N_Lit_Floating_Point;
--                Res.Lit := new String'(Tokens.Get_Token_String);
--                Next_Token;
--                return N_Root_Acc (Res);
--             end;
--          when others =>
--             raise Errors.Internal_Error;
--       end case;
--    end Parse_Literal;

--    --  Rule 74:
--    --  <op_type_spec> ::= <param_type_spec>
--    --                 |   "void"
--    function Parse_Op_Type_Spec return N_Root_Acc is
--       Res : N_Root_Acc;
--    begin
--       if Token = T_Void then
--          Res := N_Root_Acc'(new N_Void);
--          Set_Location (Res.all, Get_Location);
--          Next_Token;
--          return Res;
--       else
--          return N_Root_Acc (Parse_Param_Type_Spec);
--       end if;
--    end Parse_Op_Type_Spec;

--    --  Rule 76:
--  --  <param_dcl> ::= <param_attribute> <param_type_spec> <simple_declarator>
--    --
--    --  Rule 77:
--    --  <param_attribute> ::= "in"
--    --                    |   "out"
--    --                    |   "inout"
--    procedure Parse_Param_Dcl (List : in out Node_List) is
--       Res : N_Param_Acc;
--    begin
--       Res := new N_Param;
--       Set_Location (Res.all, Get_Location);
--       case Token is
--          when T_In =>
--             Res.Mode := Mode_In;
--             Next_Token;
--             if Token = T_Out then
--                Errors.Parser_Error ("`in out' must be `inout'",
--                                      Errors.Error);
--                Res.Mode := Mode_Inout;
--                Next_Token;
--             end if;
--          when T_Out =>
--             Res.Mode := Mode_Out;
--             Next_Token;
--          when T_Inout =>
--             Res.Mode := Mode_Inout;
--             Next_Token;
--          when others =>
--             Errors.Parser_Error ("mode `in', `out' or `inout' expected",
--                                   Errors.Error);
--             Errors.Parser_Error ("assume `in'",
--                                   Errors.Error);
--             Res.Mode := Mode_In;
--       end case;
--       Res.P_Type := Parse_Param_Type_Spec;
--       Expect (T_Identifier);
--       Add_Identifier (N_Param_Acc (Res));
--       Next_Token;
--       Append_Node (List, N_Root_Acc (Res));
--    end Parse_Param_Dcl;

--    --  Rule 75:
--    --  <parameter_dcls> ::= "(" <param_dcl> { "," <param_dcl> }* ")"
--    --                   |   "(" ")"
--    function Parse_Parameter_Dcls return Node_List is
--       Res : Node_List := Nil_List;
--    begin
--       Expect (T_Left_Paren);
--       Next_Token;
--       if Token = T_Right_Paren then
--          Next_Token;
--          return Nil_List;
--       else
--          loop
--             Parse_Param_Dcl (Res);
--             exit when Token = T_Right_Paren;
--             Expect (T_Comma);
--             Next_Token;
--          end loop;
--          Next_Token;
--          return Res;
--       end if;
--    end Parse_Parameter_Dcls;

--    --  Rule 72:
--    --  <op_dcl> ::= [ <op_attribute> ] <op_type_spec> <identifier>
--    --               <parameters_dcls> [ <raises_expr> ] [ <context_expr> ]
--    --
--    --  Rule 78:
--  --  <raises_expr> ::= "raises" "(" <scoped_name> { "," <scoped_name" }* ")"
--    --
--    --  Rule 79:
--    --  <context_expr> ::= "context" "(" <string_literal> { ","
--    --                                   <string_literal> }* ")"
--    function Parse_Op_Dcl return N_Operation_Acc is
--       Res : N_Operation_Acc;
--    begin
--       Res := new N_Operation;
--       Set_Location (Res.all, Get_Location);

--       --  Rule 73
--       --  <op_attribute> ::= "oneway"
--       if Token = T_Oneway then
--          Res.Is_Oneway := True;
--          Next_Token;
--       else
--          Res.Is_Oneway := False;
--       end if;

--       Res.Op_Type := Parse_Op_Type_Spec;
--       Expect (T_Identifier);
--       Add_Identifier (Res);
--       Next_Token;
--       Push_Scope (Res);
--       Res.Parameters := Parse_Parameter_Dcls;
--       Pop_Scope;
--       if Token = T_Raises then
--          Scan_Expect (T_Left_Paren);
--          Next_Token;
--          loop
--             Append_Node (Res.Raises, N_Root_Acc (Parse_Scoped_Name));
--             exit when Token = T_Right_Paren;
--             Expect (T_Comma);
--             Next_Token;
--          end loop;
--          Next_Token;
--       end if;
--       if Token = T_Context then
--          Scan_Expect (T_Left_Paren);
--          Next_Token;
--          loop
--             Expect (T_Lit_String);
--             Append_Node (Res.contexts, N_Root_Acc (Parse_Literal));
--             exit when Token = T_Right_Paren;
--             Expect (T_Comma);
--             Next_Token;
--          end loop;
--          Next_Token;
--       end if;
--       return Res;
--    end Parse_Op_Dcl;

--    --  Rule 23:
--    --  <primary_expr> ::= <scoped_name>
--    --                 |   <literal>
--    --                 |   "(" <const_expr> ")"
--    function Parse_Primary_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--    begin
--       case Token is
--          when T_Colon_Colon | T_Identifier =>
--             return N_Root_Acc (Parse_Scoped_Name);
--          when T_Left_Paren =>
--             Next_Token;
--             Res := Parse_Const_Exp;
--             Expect (T_Right_Paren);
--             Next_Token;
--             return Res;
--          when T_Lit_Decimal_Integer | T_Lit_Simple_Char
--            | T_Lit_Simple_Floating_Point | T_Lit_String
--            | T_Lit_Simple_Fixed_Point | T_True | T_False =>
--             return Parse_Literal;
--          when others =>
--             Errors.Parser_Error ("bad token for a primary expression: "
--                         & Image (Token),
--                                   Errors.Error);
--             raise Parse_Error;
--       end case;
--    end Parse_Primary_Expr;

--    --  Rule 21:
--    --  <unary_expr> ::= <unary_operator> <primary_expr>
--    --               |   <primary_expr>
--    --
--    --  Rule 22:
--    --  <unary_operator> ::= "+" | "-" | "~"
--    function Parse_Unary_Expr return N_Root_Acc is
--       R : N_Unary_Expr_Acc;
--    begin
--       case Token is
--          when T_Minus =>
--             R := new N_Neg_Expr;
--             Next_Token;
--             R.Operand := Parse_Primary_Expr;
--             return N_Root_Acc (R);
--          when T_Plus =>
--             R := new N_Id_Expr;
--             Next_Token;
--             R.Operand := Parse_Primary_Expr;
--             return N_Root_Acc (R);
--          when T_Tilde =>
--             R := new N_Not_Expr;
--             Next_Token;
--             R.Operand := Parse_Primary_Expr;
--             return N_Root_Acc (R);
--          when others =>
--             return Parse_Primary_Expr;
--       end case;
--    end Parse_Unary_Expr;

--    --  Rule 20:
--    --  <mult_expr> ::= <unary_expr>
--    --              |   <mult_expr> "*" <unary_expr>
--    --              |   <mult_expr> "/" <unary_expr>
--    --              |   <mult_expr> "%" <unary_expr>
--    function Parse_Mult_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_Binary_Expr_Acc;
--    begin
--       Res := Parse_Unary_Expr;
--       loop
--          if Token = T_Star then
--             R := new N_Mul_Expr;
--          elsif Token = T_Slash then
--             R := new N_Div_Expr;
--          elsif Token = T_Ampersand then
--             R := new N_Mod_Expr;
--          else
--             exit;
--          end if;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_Unary_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_Mult_Expr;

--    --  Rule 19:
--    --  <add_expr> ::= <mult_expr>
--    --             |   <add_expr> "+" <mult_expr>
--    --             |   <add_expr> "-" <mult_expr>
--    function Parse_Add_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_Binary_Expr_Acc;
--    begin
--       Res := Parse_Mult_Expr;
--       loop
--          if Token = T_Plus then
--             R := new N_Add_Expr;
--          elsif Token = T_Minus then
--             R := new N_Sub_Expr;
--          else
--             exit;
--          end if;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_Mult_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_Add_Expr;

--    --  Rule 18:
--    --  <shift_expr> ::= <add_expr>
--    --               |   <shift_expr> ">>" <add_expr>
--    --               |   <shift_expr> "<<" <add_expr>
--    function Parse_Shift_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_Binary_Expr_Acc;
--    begin
--       Res := Parse_Add_Expr;
--       loop
--          if Token = T_Greater_Greater then
--             R := new N_Shr_Expr;
--          elsif Token = T_Less_Less then
--             R := new N_Shl_Expr;
--          else
--             exit;
--          end if;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_Add_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_Shift_Expr;

--    --  Rule 17:
--    --  <and_expr> ::= <shift_expr>
--    --             |   <and_expr> "&" <shift_expr>
--    function Parse_And_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_And_Expr_Acc;
--    begin
--       Res := Parse_Shift_Expr;
--       while Token = T_Circumflex loop
--          R := new N_And_Expr;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_Shift_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_And_Expr;

--    --  Rule 16:
--    --  <xor_expr> ::= <and_expr>
--    --             |   <xor_expr> "^" <and_expr>
--    function Parse_Xor_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_Xor_Expr_Acc;
--    begin
--       Res := Parse_And_Expr;
--       while Token = T_Circumflex loop
--          R := new N_Xor_Expr;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_And_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_Xor_Expr;

--    --  Rule 15:
--    --  <or_expr> ::= <xor_expr>
--    --            |   <or_expr> "^" <xor_expr>
--    function Parse_Or_Expr return N_Root_Acc is
--       Res : N_Root_Acc;
--       R : N_Or_Expr_Acc;
--    begin
--       Res := Parse_Xor_Expr;
--       while Token = T_Bar loop
--          R := new N_Or_Expr;
--          R.Left := Res;
--          Next_Token;
--          R.Right := Parse_Xor_Expr;
--          Res := N_Root_Acc (R);
--       end loop;
--       return Res;
--    end Parse_Or_Expr;

--    --  Rule 14:
--    --  <const_expr> ::= <or_expr>
--    function Parse_Const_Exp return N_Root_Acc is
--    begin
--       return Parse_Or_Expr;
--    end Parse_Const_Exp;

--    --  Rule 66:
--    --  <string_type> ::= "string" "<" <positive_int_const> ">"
--    --                |   "string"
--    function Parse_String_Type return N_String_Acc is
--       Res : N_String_Acc;
--    begin
--       Res := new N_String;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_String);
--       Next_Token;
--       if Token = T_Less then
--          Next_Token;
--          Res.Bound := Parse_Const_Exp;
--          Expect (T_Greater);
--          Next_Token;
--       end if;
--       return Res;
--    end Parse_String_Type;

--    --  Rule 67:
--    --  <wide_string_type> ::= "wstring" "<" <positive_int_const> ">"
--    --                     |   "wstring"
--    function Parse_Wide_String_Type return N_Root_Acc is
--    begin
--       Errors.Parser_Error ("can't parse wide string type",
--                             Errors.Error);
--       raise Errors.Internal_Error;
--       return null;
--    end Parse_Wide_String_Type;

--    --  Rule 81:
--    --  <fixed_pt_type>  ::= "fixed" "<" <positive_int_const> ","
--    --                       <integer_literal> ">"
--    function Parse_Fixed_Pt_Type return N_Root_Acc is
--    begin
--       Errors.Parser_Error ("can't parse fixed pt type",
--                             Errors.Error);
--       raise Errors.Internal_Error;
--       return null;
--    end Parse_Fixed_Pt_Type;

--    --  Rule 31:
--    --  <base_type_spec> ::= <floating_pt_type>
--    --                   |   <integer_type>
--    --                   |   <char_type>
--    --                   |   <wide_char_type>
--    --                   |   <boolean_type>
--    --                   |   <octet_type>
--    --                   |   <any_type>
--    --                   |   <object_type>
--    --
--    --  Rule 38:
--    --  <floating_pt_type> ::= "float"
--    --                     |   "double"
--    --                     |   "long" "double"
--    --
--    --  Rule 39:
--    --  <integer_type> ::= <signed_int>
--    --                 |   <unsigned_int>
--    --
--    --  Rule 40:
--    --  <signed_int> ::= <signed_short_int>
--    --               |   <signed_long_int>
--    --               |   <signed_longlong_int>
--    --
--    --  Rule 41:
--    --  <signed_short_int> ::= "short"
--    --
--    --  Rule 42:
--    --  <signed_long_int> := "long"
--    --
--    --  Rule 43:
--    --  <signed_longlong_int> ::= "long" "long"
--    --
--    --  Rule 44:
--    --  <unsigned_int> ::= <unsigned_short_int>
--    --                 |   <unsigned_long_int>
--    --                 |   <unsigned_longlong_int>
--    --
--    --  Rule 45:
--    --  <unsigned_short_int> ::= "unsigned" "short"
--    --
--    --  Rule 46:
--    --  <unsigned_long_int> ::= "unsigned" "long"
--    --
--    --  Rule 47:
--    --  <unsigned_longlong_int> ::= "unsigned" "long" "long"
--    --
--    --  Rule 48:
--    --  <char_type> ::= "char"
--    --
--    --  Rule 49:
--    --  <wide_char_type> ::= "wchar"
--    --
--    --  Rule 50:
--    --  <boolean_type> ::= "boolean"
--    --
--    --  Rule 51:
--    --  <octet_type> ::= "octet"
--    --
--    --  Rule 52:
--    --  <any_type> ::= "any"
--    --
--    --  Rule 53:
--    --  <object_type> ::= "Object"
--    function Parse_Base_Type_Spec return N_Root_Acc is
--    begin
--       case Token is
--          when T_Float =>
--             Next_Token;
--             return N_Root_Acc'(new N_Float);
--          when T_Double =>
--             Next_Token;
--             return N_Root_Acc'(new N_Double);
--          when T_Short =>
--             Next_Token;
--             return N_Root_Acc'(new N_Short);
--          when T_Long =>
--             Next_Token;
--             case Token is
--                when T_Double =>
--                   Next_Token;
--                   return N_Root_Acc'(new N_Long_Double);
--                when T_Long =>
--                   Next_Token;
--                   return N_Root_Acc'(new N_Long_Long);
--                when others =>
--                   return N_Root_Acc'(new N_Long);
--             end case;
--          when T_Unsigned =>
--             Next_Token;
--             case Token is
--                when T_Short =>
--                   Next_Token;
--                   return N_Root_Acc'(new N_Unsigned_Short);
--                when T_Long =>
--                   Next_Token;
--                   if Token = T_Long then
--                      Next_Token;
--                      return N_Root_Acc'(new N_Unsigned_Long_Long);
--                   else
--                      return N_Root_Acc'(new N_Unsigned_Long);
--                   end if;
--                when others =>
--                   Errors.Parser_Error ("`unsigned' must be followed either "
--                                        & "by `short' or `long'",
--                                        Errors.Error);
--                   Errors.Parser_Error ("`unsigned long' assumed",
--                                        Errors.Error);
--                   return N_Root_Acc'(new N_Unsigned_Long);
--             end case;
--          when T_Char =>
--             Next_Token;
--             return N_Root_Acc'(new N_Char);
--          when T_Wchar =>
--             Next_Token;
--             return N_Root_Acc'(new N_Wchar);
--          when T_Boolean =>
--             Next_Token;
--             return N_Root_Acc'(new N_Boolean);
--          when T_Octet =>
--             Next_Token;
--             return N_Root_Acc'(new N_Octet);
--          when T_Any =>
--             Next_Token;
--             return N_Root_Acc'(new N_Any);
--          when T_Object =>
--             Next_Token;
--             return N_Root_Acc'(new N_Object);
--          when others =>
--             Errors.Parser_Error ("base type expected",
--                                  Errors.Error);
--             raise Errors.Internal_Error;
--       end case;
--    end Parse_Base_Type_Spec;

--    --  Rule 70:
--    --  <attr_dcl> ::= [ "readonly" ] "attribute" <param_type_spec>
--    --                 <simple_declarator> { "," <simple_declarator> }*
--    procedure Parse_Attr_Dcl (List : in out Node_List) is
--       El : N_Attribute_Acc;
--    begin
--       El := new N_Attribute;
--       Set_Location (El.all, Get_Location);
--       if Token = T_Readonly then
--          El.Is_Readonly := True;
--          Next_Token;
--       else
--          El.Is_Readonly := False;
--       end if;
--       Expect (T_Attribute);
--       Next_Token;
--       El.A_Type := Parse_Param_Type_Spec;
--       Expect (T_Identifier);
--       Add_Identifier (El);
--       Append_Node (List, N_Root_Acc (El));
--       Next_Token;
--    end Parse_Attr_Dcl;

--    --  Rule 32:
--    --  <template_type_spec> ::= <sequence_type>
--    --                       |   <string_type>
--    --                       |   <wide_string_type>
--    --                       |   <fixed_pt_type>
--    function Parse_Template_Type_Spec return N_Root_Acc is
--    begin
--       case Token is
--          when T_Sequence =>
--             return N_Root_Acc (Parse_Sequence_Type);
--          when T_String =>
--             return N_Root_Acc (Parse_String_Type);
--          when T_Wstring =>
--             return N_Root_Acc (Parse_Wide_String_Type);
--          when T_Fixed =>
--             return N_Root_Acc (Parse_Fixed_Pt_Type);
--          when others =>
--             raise Errors.Internal_Error;
--       end case;
--    end Parse_Template_Type_Spec;

--    --  Rule 30:
--    --  <simple_type_spec> ::= <base_type_spec>
--    --                     |   <template_type_spec>
--    --                     |   <scoped_name>
--    function Parse_Simple_Type_Spec return N_Root_Acc is
--    begin
--       case Token is
--          when T_Colon_Colon | T_Identifier =>
--             return N_Root_Acc (Parse_Scoped_Name);
--          when T_Sequence | T_String | T_Wstring | T_Fixed =>
--             return Parse_Template_Type_Spec;
--          when T_Float | T_Double | T_Long | T_Short | T_Unsigned |
--            T_Char | T_Wchar | T_Octet | T_Boolean | T_Any | T_Object =>
--             return Parse_Base_Type_Spec;
--          when others =>
--             Errors.Parser_Error ("simple type expected",
--                                   Errors.Error);
--             raise Parse_Error;
--       end case;
--    end Parse_Simple_Type_Spec;

--    --  Rule 65:
--    --  <sequence_type> ::= "sequence" "<" <simple_type_spec>
--    --                      "," <positive_int_const> ">"
--    --                  |   "sequence" "<" <simple_type_spec> ">"
--    function Parse_Sequence_Type return N_Sequence_Acc is
--       Res : N_Sequence_Acc;
--    begin
--       Res := new N_Sequence;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_Sequence);
--       Scan_Expect (T_Less);
--       Next_Token;
--       Res.S_Type := Parse_Simple_Type_Spec;
--       if Token = T_Comma then
--          Next_Token;
--          Res.Bound := Parse_Const_Exp;
--       else
--          Res.Bound := null;
--       end if;
--       Expect (T_Greater);
--       Next_Token;
--       return Res;
--    end Parse_Sequence_Type;

--    --  Rule 35:
--    --  <declarator> ::= <simple_declarator>
--    --               |   <complex_declarator>
--    --
--    --  Rule 36:
--    --  <simple_declarator> ::= <identifier>
--    --
--    --  Rule 37:
--    --  <complex_declarator> ::= <array_declarator>
--    --
--    --  Rule 68:
--    --  <array_declarator> ::= <identifier> <fixed_array_size>+
--    --
--    --  Rule 69:
--    --  <fixed_array_size> ::= "[" <positive_int_const> "]"
--    function Parse_Declarator return N_Declarator_Acc is
--       Res : N_Declarator_Acc;
--    begin
--       Res := new N_Declarator;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_Identifier);
--       Add_Identifier (Res);
--       Next_Token;
--       while Token = T_Left_Sbracket loop
--          Next_Token;
--          Append_Node (Res.Array_Bounds, Parse_Const_Exp);
--          Expect (T_Right_Sbracket);
--          Next_Token;
--       end loop;
--       return Res;
--    end Parse_Declarator;

--    --  Rule 55:
--    --  <member_list> ::= <member>+
--    --
--    --  Rule 56:
--    --  <member> ::= <type_spec> <declarators> ";"
--    procedure Parse_Member_List (List : in out Node_List) is
--       Res : N_Member_Acc;
--    begin
--       loop
--          Res := new N_Member;
--          Set_Location (Res.all, Get_Location);
--          Res.M_Type := Parse_Type_Spec;
--          Parse_Declarators (Res.Decl);
--          Expect (T_Semi_Colon);
--          Append_Node (List, N_Root_Acc (Res));
--          Next_Token;
--          exit when Token = T_Right_Cbracket;
--       end loop;
--    end Parse_Member_List;

--    --  Rule 60:
--    --  <case> ::= <case_label>+ <element_spec> ";"
--    --
--    --  Rule 61:
--    --  <case_label> ::= "case" <const_exp> ":"
--    --               |   "default ":"
--    function Parse_Case return N_Case_Acc is
--       Res : N_Case_Acc;
--    begin
--       Res := new N_Case;
--       Set_Location (Res.all, Get_Location);
--       Res.Labels := Nil_List;
--       loop
--          case Token is
--             when T_Case =>
--                Next_Token;
--                Append_Node (Res.Labels, Parse_Const_Exp);
--             when T_Default =>
--                Next_Token;
--                Append_Node (Res.Labels, null);
--             when others =>
--                exit;
--          end case;
--          Expect (T_Colon);
--          Next_Token;
--       end loop;
--       if Res.Labels = Nil_List then
--          Errors.Parser_Error ("`case' or `default' expected",
--                                Errors.Error);
--          raise Parse_Error;
--       end if;
--       Res.C_Type := Parse_Type_Spec;
--       Res.C_Decl := Parse_Declarator;
--       Expect (T_Semi_Colon);
--       Next_Token;
--       return Res;
--    end Parse_Case;

--    --  Rule 57:
--    --  <union_type> ::= "union" <identifier>
--    --                   "switch" "(" <switch_type_spec> ")"
--    --                   "{" <switch_body> "}"
--    --
--    --  Rule 58:
--    --  <switch_type_spec> ::= <integer_type>
--    --                     |   <char_type>
--    --                     |   <boolean_type>
--    --                     |   <enum_type>
--    --                     |   <scoped_name>
--    --
--    --  Rule 59:
--    --  <switch_body> ::= <case>+
--    function Parse_Union_Type return N_Union_Acc is
--       Res : N_Union_Acc;
--    begin
--       Res := new N_Union;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_Union);
--       Scan_Expect (T_Identifier);
--       Add_Identifier (Res);
--       Scan_Expect (T_Switch);
--       Scan_Expect (T_Left_Paren);
--       Next_Token;
--       case Token is
--          when T_Short | T_Unsigned | T_Boolean =>
--             Res.Switch_Type := Parse_Base_Type_Spec;
--          when T_Long =>
--             Next_Token;
--             if Token = T_Long then
--                Next_Token;
--                Res.Switch_Type := new N_Long_Long;
--             else
--                Res.Switch_Type := new N_Long;
--             end if;
--          when T_Enum =>
--             raise Errors.Internal_Error;
--          when T_Colon_Colon | T_Identifier =>
--             Res.Switch_Type := N_Root_Acc (Parse_Scoped_Name);
--          when others =>
--             Errors.Parser_Error ("switch type expected",
--                                  Errors.Error);
--             raise Parse_Error;
--       end case;
--       Expect (T_Right_Paren);
--       Scan_Expect (T_Left_Cbracket);
--       Push_Scope (Res);
--       Next_Token;
--       loop
--          Append_Node (Res.Cases, N_Root_Acc (Parse_Case));
--          exit when Token = T_Right_Cbracket;
--       end loop;
--       Pop_Scope;
--       Next_Token;
--       return Res;
--    end Parse_Union_Type;

--    --  Rule 54:
--    --  <struct_type> ::= "struct" <identifier> "{" <member_list> "}"
--    function Parse_Struct_Type return N_Struct_Acc is
--       Res : N_Struct_Acc;
--    begin
--       Res := new N_Struct;
--       Set_Location (Res.all, Get_Location);
--       Scan_Expect (T_Identifier);
--       Add_Identifier (Res);
--       Scan_Expect (T_Left_Cbracket);
--       Next_Token;
--       Push_Scope (Res);
--       Parse_Member_List (Res.Members);
--       Expect (T_Right_Cbracket);
--       Pop_Scope;
--       Next_Token;
--       return Res;
--    end Parse_Struct_Type;

--    --  Rule 63:
--    --  <enum_type> ::= "enum" <identifier> "{" <enumerator>
--    --                  { "," <enumerator> }* "}"
--    --  Rule 64:
--    --  <enumerator> ::= <identifier>
--    function Parse_Enum_Type return N_Enum_Acc is
--       Res : N_Enum_Acc;
--       El : N_Enumerator_Acc;
--    begin
--       Res := new N_Enum;
--       Set_Location (Res.all, Get_Location);
--       Expect (T_Enum);
--       Scan_Expect (T_Identifier);
--       Add_Identifier (Res);
--       Scan_Expect (T_Left_Cbracket);
--       loop
--          Scan_Expect (T_Identifier);
--          El := new N_Enumerator;
--          Set_Location (El.all, Get_Location);
--          Add_Identifier (El);
--          Append_Node (Res.Enumerators, N_Root_Acc (El));
--          Next_Token;
--          exit when Token /= T_Comma;
--       end loop;
--       Expect (T_Right_Cbracket);
--       Next_Token;
--       return Res;
--    end Parse_Enum_Type;

--    --  Rule 33:
--    --  <constr_type_spec> ::= <struct_type>
--    --                     |   <union_type>
--    --                     |   <enum_type>
--    function Parse_Constr_Type_Spec return N_Root_Acc is
--    begin
--       case Token is
--          when T_Struct =>
--             return N_Root_Acc (Parse_Struct_Type);
--          when T_Union =>
--             return N_Root_Acc (Parse_Union_Type);
--          when T_Enum =>
--             return N_Root_Acc (Parse_Enum_Type);
--          when others =>
--             Errors.Parser_Error ("constructed type expected",
--                                  Errors.Error);
--             raise Errors.Internal_Error;
--       end case;
--    end Parse_Constr_Type_Spec;

--    --  Rule 28:
--    --  <type_declarator> ::= <type_spec> <declarators>
--    function Parse_Type_Declarator return N_Type_Declarator_Acc is
--       Res : N_Type_Declarator_Acc;
--    begin
--       Res := new N_Type_Declarator;
--       Set_Location (Res.all, Get_Location);
--       Res.T_Type := Parse_Type_Spec;
--       Parse_Declarators (Res.Declarators);
--       return Res;
--    end Parse_Type_Declarator;



--    --  Rule 13:
--    --  <const_type> ::= <integer_type>
--    --               |   <char_type>
--    --               |   <wide_char_type>
--    --               |   <boolean_type>
--    --               |   <floating_pt_type>
--    --               |   <string_type>
--    --               |   <wide_string_type>
--    --               |   <fixed_pt_const_type>
--    --               |   <scoped_name>
--    function Parse_Const_Type return N_Root_Acc is
--    begin
--       case Token is
--          when T_Short | T_Long | T_Float | T_Double | T_Unsigned |
--            T_Char | T_Wchar | T_Boolean =>
--             return Parse_Base_Type_Spec;
--          when T_String =>
--             return N_Root_Acc (Parse_String_Type);
--          when T_Wstring =>
--             return Parse_Wide_String_Type;
--          when T_Fixed =>
--             raise Errors.Internal_Error;
--             --  return Parse_Fixed_Pt_Const_Type;
--          when T_Colon_Colon | T_Identifier =>
--             return N_Root_Acc (Parse_Scoped_Name);
--          when others =>
--             Errors.Parser_Error ("const type expected",
--                                  Errors.Error);
--             raise Parse_Error;
--       end case;
--    end Parse_Const_Type;






   ------------------------------
   --  To resume after errors  --
   ------------------------------

   -----------------------------
   --  Go_To_Next_Definition  --
   -----------------------------
   --  Tries to reach the beginning of the next definition.
   --  Called when the parser encounters an error during the
   --  parsing of a definition in order to try to continue the
   --  parsing after the bad definition.
   procedure Go_To_Next_Definition is
   begin
      while Get_Token /= T_Eof loop
         case Get_Token is
            when T_Module
              | T_Interface
              | T_Custom
              | T_Abstract
              | T_ValueType =>
               return;
            when others =>
               Next_Token;
         end case;
      end loop;
   end Go_To_Next_Definition;


   -----------------------------
   --  Go_To_Next_L_Cbracket  --
   -----------------------------
   procedure Go_To_Next_Left_Cbracket is
   begin
      while Get_Token /= T_Left_Cbracket loop
         Next_Token;
      end loop;
   end Go_To_Next_Left_Cbracket;

   -------------------------
   --  Go_To_Next_Export  --
   -------------------------
   procedure Go_To_Next_Export is
   begin
      null;
   end Go_To_Next_Export;

   --------------------------------
   --  Go_To_Next_Value_Element  --
   --------------------------------
   procedure Go_To_Next_Value_Element is
   begin
      null;
   end Go_To_Next_Value_Element;

   ---------------------------------
   --  Go_To_End_Of_State_Member  --
   ---------------------------------
   procedure Go_To_End_Of_State_Member is
   begin
      while Get_Token /= T_Semi_Colon loop
         Next_Token;
      end loop;
      Next_Token;
   end Go_To_End_Of_State_Member;

   ------------------------------------
   --  Go_To_Next_Right_Parenthesis  --
   ------------------------------------
   procedure Go_To_Next_Right_Paren is
   begin
      while Get_Token /= T_Right_Paren loop
         Next_Token;
      end loop;
   end Go_To_Next_Right_Paren;

   --
   --  INUTILE ?????
   --
   --
   --    --  Be sure TOKEN_KIND is the current token.
   --    --  If not, emit a message, and try to as wise as possible.
   --    procedure Expect (Token_Kind : Idl_Token) is
   --    begin
   --       if Token = Token_Kind then
   --          return;
   --       end if;

   --       if Token_Kind = T_Greater and then Token = T_Greater_Greater then
   --          Errors.Parser_Error
   --            ("`>>' is a shift operator, use `> >' for a double `>'",
   --             Errors.Error);
   --          Set_Replacement_Token (T_Greater);
   --       else
   --          Errors.Parser_Error ("unexpected token " & Image (Token)
   --                      & ", " & Image (Token_Kind) & " expected",
   --                                Errors.Error);
   --          raise Parse_Error;
   --       end if;
   --    end Expect;

   --    procedure Scan_Expect (Token_Kind : Idl_Token) is
   --    begin
   --       Next_Token;
   --       Expect (Token_Kind);
   --    end Scan_Expect;


end Parse;
