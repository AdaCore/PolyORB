with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with GNAT.Case_Util;
with Idl_Fe.Lexer; use Idl_Fe.Lexer;
with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Errors;
with Idl_Fe.Debug;
pragma Elaborate_All (Idl_Fe.Debug);

--  with Ada.Text_IO;

package body Idl_Fe.Parser is

   --------------
   --   Debug  --
   --------------

   Flag : constant Natural := Idl_Fe.Debug.Is_Active ("idl_fe.parser");
   procedure O is new Idl_Fe.Debug.Output (Flag);

   ---------------------
   --  Initialization --
   ---------------------

   procedure Initialize (Filename : in String;
                         Preprocess : in Boolean;
                         Keep_Temporary_Files : in Boolean) is
   begin
      Idl_Fe.Lexer.Initialize (Filename, Preprocess, Keep_Temporary_Files);
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
   Buffer_Length : constant Natural := 5;

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
   Location_Buffer : array (Buffer_Index) of Idl_Fe.Errors.Location
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
      pragma Debug (O ("Get_Token : token is " & Idl_Token'Image
                       (Token_Buffer (Current_Index))));
      return Token_Buffer (Current_Index);
   end Get_Token;

   ----------------------------
   --  Get_Token_From_Lexer  --
   ----------------------------
   procedure Get_Token_From_Lexer is
   begin
      Newest_Index := Newest_Index + 1;
      Token_Buffer (Newest_Index) := Idl_Fe.Lexer.Get_Next_Token;
      Location_Buffer (Newest_Index) := Idl_Fe.Lexer.Get_Lexer_Location;
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
             new String'(Idl_Fe.Lexer.Get_Lexer_String);
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

   ----------------------------
   --  View_Next_Next_Token  --
   ----------------------------
   function View_Next_Next_Token return Idl_Token is
   begin
      if Current_Index = Newest_Index then
         Get_Token_From_Lexer;
      end if;
      if Current_Index = Newest_Index - 1 then
         Get_Token_From_Lexer;
      end if;
      return Token_Buffer (Current_Index + 2);
   end View_Next_Next_Token;

   --------------------------
   --  Get_Token_Location  --
   --------------------------
   function Get_Token_Location return Idl_Fe.Errors.Location is
   begin
      return Location_Buffer (Current_Index);
   end Get_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------
   function Get_Previous_Token_Location return Idl_Fe.Errors.Location is
   begin
      return Location_Buffer (Current_Index - 1);
   end Get_Previous_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------
   function Get_Previous_Previous_Token_Location
     return Idl_Fe.Errors.Location is
   begin
      return Location_Buffer (Current_Index - 2);
   end Get_Previous_Previous_Token_Location;

   -------------------------------
   --  Get_Next_Token_Location  --
   -------------------------------
   function Get_Next_Token_Location return Idl_Fe.Errors.Location is
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


   ---------------------------------
   --  Management of expressions  --
   ---------------------------------

   --  the actual list of already used values
   Used_Values : Set_Ptr := null;

   ----------------------
   --  Add_Used_Value  --
   ----------------------
   function Add_Used_Value (C : N_Expr_Acc) return Boolean is
      Old_Used : Set_Ptr := null;
      Used : Set_Ptr := Used_Values;
   begin
      while Used /= null and then Used.Interval.Max < C.Value loop
         Old_Used := Used;
         Used := Used.Next;
      end loop;
      if Used = null then
         if Old_Used = null then
            Used_Values := new Set;
            Used_Values.Next := null;
            Used_Values.Interval := (Min => C.Value, Max => C.Value);
         else
            if Is_Prec (Used.Interval.Max, C.Value) then
               if Used.Next /= null
                 and then Is_Prec (C.Value, Used.Next.Interval.Min) then
                  --  merge the intervals
                  declare
                     Old_Used : Set_Ptr := Used.Next;
                  begin
                     Used.Interval.Max := Used.Next.Interval.Max;
                     Used.Next := Used.Next.Next;
                     Free (Old_Used);
                  end;
               else
                  --  only change the upper bound of the interval
                  Used.Interval.Max := C.Value;
               end if;
            else
               Old_Used.Next := new Set;
               Old_Used.Next.all.Next := null;
               Old_Used.Next.all.Interval := (Min => C.Value, Max => C.Value);
            end if;
         end if;
      else
         if Used.Interval.Min > C.Value then
            if Is_Prec (C.Value, Used.Interval.Min) then
               if Old_Used /= null
                 and then Is_Prec (Old_Used.Interval.Max, C.Value) then
                  --  merge the intervals
                  Old_Used.Interval.Max := Used.Interval.Max;
                  Old_Used.Next := Used.Next;
                  Free (Used);
               else
                  --  only change the lower bound of the interval
                  Used.Interval.Min := C.Value;
               end if;
            else
               Old_Used.Next := new Set;
               Old_Used.Next.all.Next := Used;
               Old_Used.Next.all.Interval := (Min => C.Value, Max => C.Value);
            end if;
         else
            return False;
         end if;
      end if;
      return True;
   end Add_Used_Value;

   --------------------------
   --  Release_All_Values  --
   --------------------------
   procedure Release_All_Used_Values is
      Old_Used_Values : Set_Ptr;
   begin
      while Used_Values /= null loop
         Old_Used_Values := Used_Values;
         Used_Values := Used_Values.Next;
         Free (Old_Used_Values);
      end loop;
   end Release_All_Used_Values;


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
               Res : N_Const_Dcl_Acc;
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
                     Loc : Idl_Fe.Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + 9;
                     Idl_Fe.Errors.Parser_Error
                       (Ada.Characters.Latin_1.Quotation &
                        "interface" &
                        Ada.Characters.Latin_1.Quotation &
                        " or " &
                        Ada.Characters.Latin_1.Quotation &
                        "valuetype" &
                        Ada.Characters.Latin_1.Quotation &
                        " expected after the abstract keyword.",
                        Idl_Fe.Errors.Error,
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
            Idl_Fe.Errors.Parser_Error ("definition expected.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            Result := null;
            Success := False;
            return;
      end case;
      if Get_Token /= T_Semi_Colon then
         Idl_Fe.Errors.Parser_Error
           ("';' expected at the end of a definition.",
            Idl_Fe.Errors.Error,
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
                        Loc : Idl_Fe.Errors.Location;
                     begin
                        Loc := Types.Get_Location
                          (Find_Identifier_Node (Get_Token_String).all);
                        Idl_Fe.Errors.Parser_Error
                          ("This module name is already defined in" &
                           " this scope : " &
                           Idl_Fe.Errors.Display_Location (Loc),
                           Idl_Fe.Errors.Error,
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
                     Loc : Idl_Fe.Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + Get_Token_String'Length + 1;
                     Idl_Fe.Errors.Parser_Error ("'{' expected. ",
                                          Idl_Fe.Errors.Error,
                                          Loc);
                  end;
                  Result := null;
                  Success := False;
            end case;
         when others =>
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 7;
               Idl_Fe.Errors.Parser_Error ("Identifier expected. ",
                                    Idl_Fe.Errors.Error,
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
      pragma Debug (O ("Parse_Interface : enter"));
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
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            --  is it a forward declaration
            if Get_Kind (Definition.Node.all) = K_Forward_Interface then
               --  Check if they are both of the same abstract kind
               if N_Forward_Interface_Acc (Definition.Node).Abst
                 /= Res.Abst then
                  declare
                     Loc : Idl_Fe.Errors.Location;
                  begin
                        Loc := Types.Get_Location
                          (N_Forward_Interface_Acc (Definition.Node).all);
                        Idl_Fe.Errors.Parser_Error
                          ("Forward declaration "
                           & Idl_Fe.Errors.Display_Location (Loc)
                           & " has not the same abstract type",
                           Idl_Fe.Errors.Error,
                           Get_Previous_Token_Location);
                  end;
               end if;
               Fd_Res := N_Forward_Interface_Acc (Get_Node (Definition));
               --  is this interface not a forward declaration
               --  FIXME >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
               if View_Next_Token /= T_Semi_Colon then
                  Fd_Res.Forward := Res;
                  Res.Forward := Fd_Res;
                  Redefine_Identifier (Definition, Res);
                  --  the forward declaration is now implemented
                  Add_Int_Val_Definition (N_Named_Acc (Fd_Res));
               end if;
            else
               declare
                  Loc : Idl_Fe.Errors.Location;
               begin
                  Loc := Types.Get_Location
                    (Find_Identifier_Node (Get_Token_String).all);
                  Idl_Fe.Errors.Parser_Error
                    ("This interface name is already declared in" &
                     " this scope : " &
                     Idl_Fe.Errors.Display_Location (Loc),
                     Idl_Fe.Errors.Error,
                     Get_Token_Location);
                  Success := False;
                  Result := null;
                  Fd_Res := null;
                  return;
               end;
            end if;
         else
            pragma Debug (O ("Parse_Interface : identifier not defined"));
            Fd_Res := null;
            Res.Forward := null;
            if not Add_Identifier (Res,
                                   Get_Token_String) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
            Definition := Find_Identifier_Definition (Get_Token_String);
         end if;
      else
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Idl_Fe.Errors.Parser_Error
              (" identifier expected after 'interface'",
               Idl_Fe.Errors.Error,
               Loc);
            Success := False;
            Result := null;
            return;
         end;
      end if;
      pragma Debug (O ("Parse_Interface : identifier parsed"));
      Next_Token;
      --  Hups, this was just a forward declaration.
      if Get_Token = T_Semi_Colon then
         --  is it another forward declaration
         if Fd_Res /= null then
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Types.Get_Location (Fd_Res.all);
               Idl_Fe.Errors.Parser_Error
                 ("forward declaration already defined in" &
                  " this scope : " &
                  Idl_Fe.Errors.Display_Location (Loc),
                  Idl_Fe.Errors.Warning,
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
            --  A forward declaration should be added
            Add_Int_Val_Forward (N_Named_Acc (Fd_Res));
            --  Free (Res); ???????????????????
            Result := N_Named_Acc (Fd_Res);
            Success := True;
            return;
         end if;
      else
         --  use the Interface4 rule
         Parse_Interface_Dcl_End (Res, Success);
         if not Success then
            Result := null;
         else
            Result := N_Named_Acc (Res);
         end if;
         return;
      end if;
      return;
   end Parse_Interface;

   --------------------
   --  Parse_Export  --
   --------------------
   procedure Parse_Export (Result : out N_Root_Acc;
                           Success : out Boolean) is
   begin
      case Get_Token is
--          when T_Readonly | T_Attribute =>
--             Parse_Attr_Dcl (List);
         when T_Oneway | T_Void | T_Colon_Colon | T_Identifier |
           T_Short | T_Long | T_Float | T_Double | T_Unsigned |
           T_Char | T_Wchar | T_Boolean | T_Octet | T_Any | T_Object |
           T_String | T_Wstring =>
            declare
               Result_Operation : N_Operation_Acc;
            begin
               Parse_Op_Dcl (Result_Operation, Success);
               Result := N_Root_Acc (Result_Operation);
            end;
            --          when T_Right_Cbracket =>
            --             return;
            --          when T_Exception =>
            --             Append_Node (List, N_Root_Acc (Parse_Except_Dcl));
         when T_Union =>
            declare
               Result_Union : N_Union_Acc;
            begin
               Parse_Union_Type (Result_Union, Success);
               Result := N_Root_Acc (Result_Union);
            end;
         when T_Struct =>
            declare
               Result_Struct : N_Struct_Acc;
            begin
               Parse_Struct_Type (Result_Struct, Success);
               Result := N_Root_Acc (Result_Struct);
            end;
         when T_Enum =>
            declare
               Result_Enum : N_Enum_Acc;
            begin
               Parse_Enum_Type (Result_Enum, Success);
               Result := N_Root_Acc (Result_Enum);
            end;
         when T_Typedef =>
            Parse_Type_Dcl (Result, Success);
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("declaration of an operation expected",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
            return;
      end case;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length + 1;
            Idl_Fe.Errors.Parser_Error ("`;' expected",
                                        Idl_Fe.Errors.Error,
                                        Loc);
         end;
      end if;
      Next_Token;
   end Parse_Export;

   --------------------------------
   --  Parse_Interface_Decl_End  --
   --------------------------------
   procedure Parse_Interface_Dcl_End (Result : in out N_Interface_Acc;
                                     Success : out Boolean) is
      Body_Success : Boolean;
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
               if not Scoped_Success then
                  Go_To_Next_Left_Cbracket;
                  exit;
               else
                  Append_Node (Result.Parents, N_Root_Acc (Name));
               end if;
            end;
            exit when Get_Token /= T_Comma;
         end loop;
      end if;

      if Get_Token = T_Left_Cbracket then
         Next_Token;
      else
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length + 1;
            Idl_Fe.Errors.Parser_Error
              ("'{' expected",
               Idl_Fe.Errors.Error,
               Loc);
            Success := False;
            return;
         end;
      end if;
      --  Create a scope for the interface.
      Push_Scope (Result);
      Parse_Interface_Body (Result.Contents, Body_Success);
      Pop_Scope;
      if not Body_Success then
         Result := null;
         Success := False;
         return;
      else
         --  consume the right bracket at the end of the interface body
         --  verification of the presence of this bracket was done
         --  in Parse_Interface_Body
         Next_Token;
      end if;
      Success := True;
      return;
   end Parse_Interface_Dcl_End;



   ----------------------------
   --  Parse_Interface_Body  --
   ----------------------------
   procedure Parse_Interface_Body (List : in out Node_List;
                                   Success : out Boolean) is
      Export_Success : Boolean;
      Result : N_Root_Acc;
   begin
      Success := True;
      loop
         exit when Get_Token = T_Right_Cbracket;
         Parse_Export (Result, Export_Success);
         if not Export_Success then
            pragma Debug (O ("Parse_Interface_Body : Export_Success = false"));
            Go_To_Next_Export;
         else
            pragma Debug (O ("Parse_Interface_Body : Export_Success = True"));
            Append_Node (List, Result);
         end if;
      end loop;
   end Parse_Interface_Body;


   -------------------------
   --  Parse_Scoped_Name  --
   -------------------------
   procedure Parse_Scoped_Name (Result : out N_Scoped_Name_Acc;
                                Success : out Boolean) is
      Res, Prev : N_Scoped_Name_Acc;
      Scope : N_Scope_Acc;
      Name : N_Named_Acc;
   begin
      pragma Debug (O ("Parse_Scoped_Name : enter"));
      Result := null;
      Success := False;
      Prev := null;
      Res := new N_Scoped_Name;
      Set_Location (Res.all, Get_Token_Location);
      if Get_Token = T_Colon_Colon then
         Scope := Get_Root_Scope;
      else
         --  token should be an identifier
         if Get_Token /= T_Identifier then
            Idl_Fe.Errors.Parser_Error
              (" identifier expected in the scoped name",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
            return;
         end if;
         Name := Find_Identifier_Node (Get_Token_String);
         if Name = null then
            pragma Debug (O ("Parse_Scoped_Name : name is null"));
            Idl_Fe.Errors.Parser_Error
              ("Bad identifier in scoped name",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
            return;
         end if;
         Next_Token;
         --  we should perhaps import this identifier :
         --  first we should look at the current scope.
         --  If it is a Struct, Union, Operation or Exception
         --  we should import it in the parent scope of the
         --  current scope if necessary;
         --  else we should import it in the current scope.
         --  If it is a module or repository, the
         --  add function won't do anything
         if Get_Current_Scope.all not in N_Forward'Class and
           Get_Current_Scope.all not in N_Imports'Class then
            if Get_Previous_Scope /= Get_Definition (Name).Parent_Scope then
               Add_Definition_To_Imported (Get_Definition (Name),
                                           Get_Previous_Scope);
            end if;
         else
            if Get_Current_Scope /= Get_Definition (Name).Parent_Scope then
               Add_Definition_To_Imported (Get_Definition (Name),
                                           Get_Current_Scope);
            end if;
         end if;
         if Get_Token /= T_Colon_Colon then
            Res.Value := Name;
            Success := True;
            Result := Res;
            pragma Debug (O ("Parse_Scoped_Name : end if simple identifier"));
            return;
         end if;
         --  is the identifier a scope?
         if Name.all not in N_Scope'Class then
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
               Idl_Fe.Errors.Parser_Error
                 ("Bad identifier in scoped name",
                  Idl_Fe.Errors.Error,
                  Loc);
               Success := False;
               Result := null;
               return;
            end;
         else
            Scope := N_Scope_Acc (Name);
         end if;
      end if;
      --  now we will loop in the scopes to get the right definition
      loop
         Next_Token;
         if Get_Token /= T_Identifier then
            Idl_Fe.Errors.Parser_Error
              (" identifier expected in the scoped name",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
            return;
         end if;
         --  find the indentifier in the scope
         Name := Find_Identifier_In_Storage (Scope, Get_Token_String).Node;
         if Name = null then
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
               Idl_Fe.Errors.Parser_Error
                 ("Bad identifier in scoped name",
                  Idl_Fe.Errors.Error,
                  Loc);
               Success := False;
               Result := null;
               return;
            end;
         end if;
         Next_Token;
         if Get_Token = T_Colon_Colon then
            if Name.all not in N_Scope'Class then
               declare
                  Loc : Idl_Fe.Errors.Location;
               begin
                  Loc := Get_Previous_Token_Location;
                  Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
                  Idl_Fe.Errors.Parser_Error
                    ("Bad identifier in scoped name",
                     Idl_Fe.Errors.Error,
                     Loc);
                  Success := False;
                  Result := null;
                  return;
               end;
            else
               Scope := N_Scope_Acc (Name);
            end if;
         end if;
         exit when Get_Token /= T_Colon_Colon;
      end loop;
      if Name = null then
         raise Idl_Fe.Errors.Internal_Error;
      end if;
      Res.Value := Name;
      Success := True;
      Result := Res;
      pragma Debug (O ("Parse_Scoped_Name : return"));
      return;
   end Parse_Scoped_Name;

   -------------------
   --  Parse_Value  --
   -------------------
   procedure Parse_Value (Result : out N_Named_Acc;
                          Success : out Boolean) is
   begin
      pragma Debug (O ("Initialize_Local_Object : enter"));
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
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      return;
   end Parse_Value;

   --------------------------
   --  Parse_Custom_Value  --
   --------------------------
   procedure Parse_Custom_Value (Result : out N_ValueType_Acc;
                                 Success : out Boolean) is
   begin
      pragma Debug (O ("Parse_Custom_Value : enter"));
      if Get_Token /= T_ValueType then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Idl_Fe.Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " expected after custom keyword.",
                                 Idl_Fe.Errors.Error,
                                 Loc);
         end;
         Result := null;
         Success := False;
      else
         Next_Token;
         if Get_Token /= T_Identifier then
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Idl_Fe.Errors.Parser_Error ("identifier expected.",
                                    Idl_Fe.Errors.Error,
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
      pragma Debug (O ("Parse_Abstract_Value : enter"));
      if Get_Token /= T_ValueType then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 9;
            Idl_Fe.Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 "expected after abstract keyword.",
                                 Idl_Fe.Errors.Error,
                                 Loc);
         end;
         Result := null;
         Success := False;
      else
         Next_Token;
         pragma Debug (O ("Parse_Abstract_Value : check for identifier"));
         if Get_Token /= T_Identifier then
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Idl_Fe.Errors.Parser_Error ("identifier expected.",
                                    Idl_Fe.Errors.Error,
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
                     Loc : Idl_Fe.Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + Get_Token_String'Length;
                     Idl_Fe.Errors.Parser_Error ("Bad value definition. " &
                                          "Inheritance specification, '{'" &
                                          " or ';' expected.",
                                          Idl_Fe.Errors.Error,
                                          Loc);
                  end;
                  Result := null;
                  Success := False;
            end case;
         end if;
      end if;
      pragma Debug (O ("Parse_Abstract_Value : end"));
      return;
   end Parse_Abstract_Value;

   --------------------------
   --  Parse_Direct_Value  --
   --------------------------
   procedure Parse_Direct_Value (Result : out N_Named_Acc;
                                 Success : out Boolean) is
   begin
      pragma Debug (O ("Parse_Direct_Value : enter"));
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Idl_Fe.Errors.Parser_Error ("identifier expected.",
                                 Idl_Fe.Errors.Error,
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
                  Loc : Idl_Fe.Errors.Location;
               begin
                  Loc := Get_Token_Location;
                  Loc.Col := Loc.Col + Get_Token_String'Length;
                  Idl_Fe.Errors.Parser_Error ("Bad value definition. " &
                                       "Type, inheritance specification, " &
                                       "'{' or ';' expected.",
                                       Idl_Fe.Errors.Error,
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
      pragma Debug (O ("Parse_End_Value_Dcl : enter"));
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
        and then (Definition.Parent_Scope = Get_Current_Scope or
        Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id) then
         --  There was probably a forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            declare
               Fd_Decl : N_Forward_ValueType_Acc;
            begin
               Fd_Decl := N_Forward_ValueType_Acc (Get_Node (Definition));
               Add_Int_Val_Definition (N_Named_Acc (Fd_Decl));
               Fd_Decl.Forward := Result;
               Result.Forward := Fd_Decl;
               Redefine_Identifier (Definition, Result);
            end;
         else
            Idl_Fe.Errors.Parser_Error
            ("The identifier used for this valuetype is already "
             & "defined in the same scope : " &
             Idl_Fe.Errors.Display_Location
             (Get_Location (Definition.Node.all)),
             Idl_Fe.Errors.Error,
             Get_Token_Location);
            Result.Forward := null;
         end if;
      else
         --  no previous definition
         Result.Forward := null;
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Idl_Fe.Errors.Internal_Error;
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
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
                  Idl_Fe.Errors.Parser_Error ("Bad value definition. " &
                                       "'{' expected.",
                                       Idl_Fe.Errors.Error,
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
      pragma Debug (O ("Parse_End_Value_Dcl : end"));
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
        and then (Definition.Parent_Scope = Get_Current_Scope or
        Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id) then
         --  There was probably a previous forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            --  nothing to do : this new forward declaration is useless
            Idl_Fe.Errors.Parser_Error
              ("This valuetype was already declared forward : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Warning,
               Get_Token_Location);
         else
            Idl_Fe.Errors.Parser_Error
            ("The identifier used for this valuetype is already "
             & "defined in the same scope : " &
             Idl_Fe.Errors.Display_Location
             (Get_Location (Definition.Node.all)),
             Idl_Fe.Errors.Error,
             Get_Token_Location);
         end if;
      else
         --  no previous forward
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Idl_Fe.Errors.Internal_Error;
         end if;
      end if;
      --  consumes the identifier
      Next_Token;
      Add_Int_Val_Forward (N_Named_Acc (Result));
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
        and then (Definition.Parent_Scope = Get_Current_Scope or
        Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id) then
         --  There was probably a previous forward declaration
         if Get_Kind (Definition.Node.all) = K_Forward_ValueType then
            Idl_Fe.Errors.Parser_Error
              ("This valuetype was forward declared : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)) &
               ". It can not be a boxed one.",
               Idl_Fe.Errors.Error,
               Get_Previous_Token_Location);
         else
            Idl_Fe.Errors.Parser_Error
              ("The identifier used for this valuetype is already "
               & "defined in the same scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Previous_Token_Location);
         end if;
         Next_Token;
         Parse_Type_Spec (Result.Boxed_Type, Success);
      else
         --  no previous forward
         declare
            Name : String_Ptr;
         begin
            --  the purpose here is to avoid the use of the value
            --  name in the definition of its type. For example, the
            --  following declaration is illegal :
            --      valuetype FooSeq sequence <FooSeq>
            Name := new String'(Get_Token_String);
            Next_Token;
            Parse_Type_Spec (Result.Boxed_Type, Success);
            if not Add_Identifier (Result,
                                   Name.all) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
            Free_String_Ptr (Name);
         end;
      end if;
      return;
   end Parse_End_Value_Box_Dcl;

   ------------------------------------
   --  Parse_Value_Inheritance_Spec  --
   ------------------------------------
   procedure Parse_Value_Inheritance_Spec (Result : in out N_ValueType_Acc;
                                           Success : out Boolean) is
   begin
      pragma Debug (O ("Parse_Value_Inheritance_Spec : enter"));
      if Get_Token = T_Colon then
         Next_Token;
         if Get_Token = T_Truncatable then
            if Result.Abst then
               Idl_Fe.Errors.Parser_Error
                 ("The truncatable modifier may not " &
                  "be used in an abstract value.",
                  Idl_Fe.Errors.Error,
                  Get_Token_Location);
            elsif Result.Custom then
               Idl_Fe.Errors.Parser_Error
                 ("The truncatable modifier may not " &
                  "be used in a custom value.",
                  Idl_Fe.Errors.Error,
                  Get_Token_Location);
            else
               Result.Truncatable := True;
            end if;
            Next_Token;
         end if;
         pragma Debug (O ("Parse_Value_Inheritance_Spec : truncable treated"));
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
                           Idl_Fe.Errors.Parser_Error
                             ("An abstract value may not inherit from a " &
                              "stateful one.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
                        end if;
                     else
                        if N_ValueType_Acc (Name.Value).Abst then
                           Idl_Fe.Errors.Parser_Error
                             ("The truncatable modifier may not be used " &
                              "for an abstract value inheritance.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
                        end if;
                     end if;
                     Append_Node (Result.Parents, N_Root_Acc (Name));
                  when K_Forward_ValueType =>
                     Idl_Fe.Errors.Parser_Error
                       ("A value may not inherit from a forward declared" &
                        " value whose definition has not yet been seen.",
                        Idl_Fe.Errors.Error,
                        Get_Token_Location);
                  when K_Boxed_ValueType =>
                     Idl_Fe.Errors.Parser_Error
                       ("A value may not inherit from a boxed value.",
                        Idl_Fe.Errors.Error,
                        Get_Token_Location);
                  when K_Interface
                    | K_Forward_Interface =>
                     Idl_Fe.Errors.Parser_Error
                       ("A value may not inherit from an interface. "&
                        "It can only support it.",
                        Idl_Fe.Errors.Error,
                        Get_Token_Location);
                  when others =>
                     declare
                        Loc : Idl_Fe.Errors.Location;
                     begin
                        Loc := Get_Previous_Token_Location;
                        Loc.Col := Loc.Col + 2;
                        Idl_Fe.Errors.Parser_Error
                          ("Value name expected.",
                           Idl_Fe.Errors.Error,
                           Loc);
                     end;
               end case;
            else
               Go_To_Next_Left_Cbracket;
               Success := False;
               return;
            end if;
         end;
         pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                          "first parent parsed"));
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
                        pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                                         "parent is a valuetype"));
                        if Is_In_List (Result.Parents, N_Root_Acc (Name)) then
                           --  already inherited
                           Idl_Fe.Errors.Parser_Error
                             ("Already inherited of this value.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
                        else
                           if not N_ValueType_Acc (Name.Value).Abst then
                              Idl_Fe.Errors.Parser_Error
                                ("A stateful value may only derive from a " &
                                 "single stateful value and this one must " &
                                 "be the first element in the inheritance.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
                           end if;
                           Append_Node (Result.Parents, N_Root_Acc (Name));
                        end if;
                     when K_Forward_ValueType =>
                        Idl_Fe.Errors.Parser_Error
                          ("A value may not inherit from a forward declared" &
                           " value whose definition has not yet been seen.",
                           Idl_Fe.Errors.Error,
                           Get_Token_Location);
                     when K_Boxed_ValueType =>
                        Idl_Fe.Errors.Parser_Error
                          ("A value may not inherit from a boxed value.",
                           Idl_Fe.Errors.Error,
                           Get_Token_Location);
                     when K_Interface
                        | K_Forward_Interface =>
                        Idl_Fe.Errors.Parser_Error
                          ("A value may not inherit from an interface. "&
                           "It can only support it.",
                        Idl_Fe.Errors.Error,
                           Get_Token_Location);
                     when others =>
                        declare
                           Loc : Idl_Fe.Errors.Location;
                        begin
                           Loc := Get_Previous_Token_Location;
                           Loc.Col := Loc.Col + 2;
                           Idl_Fe.Errors.Parser_Error
                             ("Value name expected.",
                              Idl_Fe.Errors.Error,
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
      pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                       "begining parsing of supports declarations"));
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
                        when K_Forward_Interface =>
                           Idl_Fe.Errors.Parser_Error
                             ("A value may not support a forward declared" &
                              " interface whose declaration has not yet " &
                              "been seen.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
                        when K_Boxed_ValueType
                          | K_ValueType
                          | K_Forward_ValueType =>
                           Idl_Fe.Errors.Parser_Error
                             ("A value may not support another value. " &
                              " However, it can inherit from it.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
                        when others =>
                           declare
                              Loc : Idl_Fe.Errors.Location;
                           begin
                              Loc := Get_Previous_Token_Location;
                              Loc.Col := Loc.Col + 9;
                              Idl_Fe.Errors.Parser_Error
                                ("Value name expected.",
                                 Idl_Fe.Errors.Error,
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
                                    Idl_Fe.Errors.Parser_Error
                                      ("A stateful value may support only " &
                                       "one non abstract interface. This " &
                                       "is the second one.",
                                       Idl_Fe.Errors.Error,
                                       Get_Token_Location);
                                 else
                                    Non_Abstract_Interface := True;
                                 end if;
                              end if;
                              Append_Node (Result.Supports, N_Root_Acc (Name));
                           when K_Forward_Interface =>
                              Idl_Fe.Errors.Parser_Error
                                ("A value may not support a forward declared" &
                                 " interface whose declaration has not yet " &
                                 "been seen.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
                           when K_Boxed_ValueType
                             | K_ValueType
                             | K_Forward_ValueType =>
                              Idl_Fe.Errors.Parser_Error
                                ("A value may not support another value. " &
                                 " However, it can inherit from it.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
                           when others =>
                              declare
                                 Loc : Idl_Fe.Errors.Location;
                              begin
                                 Loc := Get_Previous_Token_Location;
                                 Loc.Col := Loc.Col + 9;
                                 Idl_Fe.Errors.Parser_Error
                                   ("Value name expected.",
                                    Idl_Fe.Errors.Error,
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
         when others =>
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
               Idl_Fe.Errors.Parser_Error
                 ("',', " &
                  Ada.Characters.Latin_1.Quotation &
                  "supports" &
                  Ada.Characters.Latin_1.Quotation &
                  " or '{' expected.",
                  Idl_Fe.Errors.Error,
                  Loc);
               Success := False;
            end;
      end case;
      pragma Debug (O ("Parse_Value_Inheritance_Spec : enter"));
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
            Idl_Fe.Errors.Parser_Error ("value_element expected.",
                                 Idl_Fe.Errors.Error,
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
            raise Idl_Fe.Errors.Internal_Error;
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
         Idl_Fe.Errors.Parser_Error ("missing ';' at the end of the state " &
                              "declaration.",
                              Idl_Fe.Errors.Error,
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
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + 8;
            Idl_Fe.Errors.Parser_Error ("Identifier expected after keyword " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "factory" &
                                 Ada.Characters.Latin_1.Quotation &
                                 ".",
                                 Idl_Fe.Errors.Error,
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
        and then (Definition.Parent_Scope = Get_Current_Scope or
        Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id) then
         Idl_Fe.Errors.Parser_Error
           ("The identifier used for this initializer is already "
            & "defined in the same scope : " &
            Idl_Fe.Errors.Display_Location
            (Get_Location (Definition.Node.all)),
            Idl_Fe.Errors.Error,
            Get_Token_Location);
      else
         --  no previous definition
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Idl_Fe.Errors.Internal_Error;
         end if;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("missing '(' in initializer declaration.",
               Idl_Fe.Errors.Error,
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
                  Idl_Fe.Errors.Parser_Error ("missing ')' at the end of " &
                                       "initializer declaration.",
                                       Idl_Fe.Errors.Error,
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
         Idl_Fe.Errors.Parser_Error ("missing ';' at the end of initializer " &
                              "declaration.",
                              Idl_Fe.Errors.Error,
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
            Idl_Fe.Errors.Parser_Error
              ("an initializer parameter can only be " &
               "in mode " &
               Ada.Characters.Latin_1.Quotation &
               "in" &
               Ada.Characters.Latin_1.Quotation &
               ".",
               Idl_Fe.Errors.Error,
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
            Idl_Fe.Errors.Parser_Error
              ("an initializer parameter should begin " &
               "with keyword " &
               Ada.Characters.Latin_1.Quotation &
               "in" &
               Ada.Characters.Latin_1.Quotation &
               ".",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("bad initializer parameter declaration.",
               Idl_Fe.Errors.Error,
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
      Parse_Simple_Declarator (Result.Declarator, Success);
      return;
   end Parse_Init_Param_Decl;

   -----------------------
   --  Parse_Const_Dcl  --
   -----------------------
   procedure Parse_Const_Dcl (Result : out N_Const_Dcl_Acc;
                              Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Next_Token;
      Result := new N_Const_Dcl;
      Set_Location (Result.all, Get_Previous_Token_Location);
      Parse_Const_Type (Result.Constant_Type, Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Identifier then
         Idl_Fe.Errors.Parser_Error ("Identifier expected.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         Success := False;
         return;
      else
         --  try to find a previous definition
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         else
            --  no previous definition
            if not Add_Identifier (Result,
                                   Get_Token_String) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
         end if;
      end if;
      Next_Token;
      if Get_Token /= T_Equal then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error ("'=' expected in const declaration.",
                                 Idl_Fe.Errors.Error,
                                 Loc);
         end;
         case Get_Token is
            when T_Minus
              | T_Plus
              | T_Tilde
              | T_Colon_Colon
              | T_Identifier
              | T_Lit_Decimal_Integer
              | T_Lit_Octal_Integer
              | T_Lit_Hexa_Integer
              | T_Lit_Simple_Char
              | T_Lit_Escape_Char
              | T_Lit_Octal_Char
              | T_Lit_Hexa_Char
              | T_Lit_Unicode_Char
              | T_Lit_Wide_Simple_Char
              | T_Lit_Wide_Escape_Char
              | T_Lit_Wide_Octal_Char
              | T_Lit_Wide_Hexa_Char
              | T_Lit_Wide_Unicode_Char
              | T_Lit_Simple_Floating_Point
              | T_Lit_Exponent_Floating_Point
              | T_Lit_Pure_Exponent_Floating_Point
              | T_Lit_String
              | T_Lit_Wide_String
              | T_Lit_Simple_Fixed_Point
              | T_Lit_Floating_Fixed_Point
              | T_Left_Paren =>
               null;
            when others =>
               Success := False;
               return;
         end case;
      else
         Next_Token;
      end if;
      Parse_Const_Exp (Result.Expression,
                       Result.Constant_Type,
                       Success);
      return;
   end Parse_Const_Dcl;

   ------------------------
   --  Parse_Const_Type  --
   ------------------------
   procedure Parse_Const_Type (Result : out N_Root_Acc;
                               Success : out Boolean) is
   begin
      case Get_Token is
         when T_Long =>
            if View_Next_Token = T_Double then
               Parse_Floating_Pt_Type (Result, Success);
            else
               Parse_Integer_Type (Result, Success);
            end if;
         when T_Short
           | T_Unsigned =>
            Parse_Integer_Type (Result, Success);
         when T_Char =>
            declare
               Res : N_Type_Declarator_Acc;
            begin
               Parse_Type_Declarator (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Struct =>
            declare
               Res : N_Struct_Acc;
            begin
               Parse_Struct_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Union =>
            declare
               Res : N_Union_Acc;
            begin
               Parse_Union_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Float
           | T_Double =>
            Parse_Floating_Pt_Type (Result, Success);
         when T_String =>
            declare
               Res : N_String_Acc;
            begin
               Parse_String_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Wstring =>
            declare
               Res : N_Wide_String_Acc;
            begin
               Parse_Wide_String_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Fixed =>
            declare
               Res : N_Fixed_Acc;
            begin
               Parse_Fixed_Pt_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Colon_Colon
           | T_Identifier =>
            declare
               Res : N_Scoped_Name_Acc;
            begin
               Parse_Scoped_Name (Res, Success);
               --  The <scoped_name> in the <const_type> production
               --  must be a previously defined integer, char, wide_char,
               --  boolean, floating_pt, string, wide_string, octet or
               --  enum type.
               case Get_Kind (Res.Value.all) is
                  when K_Short
                    | K_Long
                    | K_Long_Long
                    | K_Unsigned_Short
                    | K_Unsigned_Long
                    | K_Unsigned_Long_Long
                    | K_Char
                    | K_Wide_Char
                    | K_Boolean
                    | K_Float
                    | K_Double
                    | K_Long_Double
                    | K_String
                    | K_Wide_String
                    | K_Octet
                    | K_Enum =>
                     null;
                  when others =>
                     Idl_Fe.Errors.Parser_Error
                       ("Invalid type in constant. The " &
                        "scoped name should refer to " &
                        "an integer, char, wide_cahr, " &
                        "boolean, floating_pt, string, " &
                        "wide_string, octet or enum type.",
                        Idl_Fe.Errors.Error,
                        Get_Token_Location);
               end case;
               Result := N_Root_Acc (Res);
            end;
         when T_Octet =>
            declare
               Res : N_Octet_Acc;
            begin
               Parse_Octet_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            Idl_Fe.Errors.Parser_Error ("constant type expected.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            Success := False;
            Result := null;
      end case;
      return;
   end Parse_Const_Type;

   -----------------------
   --  Parse_Const_Exp  --
   -----------------------
   procedure Parse_Const_Exp (Result : out N_Expr_Acc;
                              Constant_Type : in N_Root_Acc;
                              Success : out Boolean) is
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Or_Expr (Result, Success);
      --  check compatibility between the constant expression
      --  and its supposed type
      case Get_Kind (Constant_Type.all) is
         when K_Short =>
            case Result.Value.Const_Type.Kind is
               when C_Short =>
                  null;
               when C_UShort =>
                  if Short_Value_Ptr (Result.Value).Value > Idl_Short'Last then
                     Idl_Fe.Errors.Parser_Error
                       ("constraint error : this value is " &
                        "not in range of type short but " &
                        "unsigned short.",
                        Idl_Fe.Errors.Error,
                        Loc);
                  end if;
               when C_Long =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type short but long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_LongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type short but " &
                     "long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_ULong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type short but " &
                     "unsigned long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_ULongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type short but " &
                     "unsigned long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Long =>
            case Result.Value.Const_Type.Kind is
               when C_Short
                 | C_UShort
                 | C_Long =>
                  null;
               when C_ULong =>
                  if Long_Value_Ptr (Result.Value).Value > Idl_Long'Last then
                     Idl_Fe.Errors.Parser_Error
                       ("constraint error : this value is " &
                        "not in range of type long but " &
                        "unsigned long.",
                        Idl_Fe.Errors.Error,
                        Loc);
                  end if;
               when C_LongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type long but " &
                     "long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_ULongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type long but " &
                     "unsigned long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Long_Long =>
            case Result.Value.Const_Type.Kind is
               when C_Short
                 | C_UShort
                 | C_Long
                 | C_ULong
                 | C_LongLong =>
                  null;
               when C_ULongLong =>
                  if LongLong_Value_Ptr (Result.Value).Value >
                    Idl_LongLong'Last then
                     Idl_Fe.Errors.Parser_Error
                       ("constraint error : this value is " &
                        "not in range of type long long " &
                        "but unsigned long long.",
                        Idl_Fe.Errors.Error,
                        Loc);
                  end if;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Unsigned_Short =>
            case Result.Value.Const_Type.Kind is
               when C_Short =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned short " &
                     "but short.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_Long =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned short " &
                     "but long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_LongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned short " &
                     "but long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_UShort =>
                  null;
               when C_ULong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned short " &
                     "but unsigned long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_ULongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned short " &
                     "but unsigned long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Unsigned_Long =>
            case Result.Value.Const_Type.Kind is
               when C_Short =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "but short.",
                     Idl_Fe.Errors.Error,
                     Loc);

               when C_Long =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "but long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_LongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "but long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_UShort
                 | C_ULong =>
                  null;
               when C_ULongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "but unsigned long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Unsigned_Long_Long =>
            case Result.Value.Const_Type.Kind is
               when C_Short =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "long but short.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_Long =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "long but long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_LongLong =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type unsigned long " &
                     "long but long long.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_UShort
                 | C_ULong
                 | C_ULongLong =>
                  null;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an integer.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Char =>
            case Result.Value.Const_Type.Kind is
               when C_Char =>
                  null;
               when C_WChar =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not of type char but wide_char.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a char.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Wide_Char =>
            case Result.Value.Const_Type.Kind is
               when C_WChar =>
                  null;
               when C_Char =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not of type wide_char but char.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a char.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Boolean =>
            case Result.Value.Const_Type.Kind is
               when C_Boolean =>
                  null;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a boolean.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Float =>
            case Result.Value.Const_Type.Kind is
               when C_Float =>
                  null;
               when C_Double =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type float " &
                     "but double.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when C_LongDouble =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type float " &
                     "but long double.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a floating point number.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Double =>
            case Result.Value.Const_Type.Kind is
               when C_Float
                 | C_Double =>
                  null;
               when C_LongDouble =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not in range of type double " &
                     "but long double.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a floating point number.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Long_Double =>
            case Result.Value.Const_Type.Kind is
               when C_Float
                 | C_Double
                 | C_LongDouble =>
                  null;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a floating point number.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Fixed =>
            case Result.Value.Const_Type.Kind is
               when C_Fixed =>
                  null;  --  FIXME
--                   declare
--                      Dg_Node : Idl_Short
--                        renames Short_Value_Ptr (N_Fixed_Acc (Constant_Type)
--                                                  .Scale.Value).Value;
--                      Dg_Exp : Idl_Fixed_Digits_Nb;
--                        renames Result.Value.Const_Type.Digits_Nb;
--                      Sc_Node : Idl_Short
--                        renames Short_Value_Ptr (N_Fixed_Acc (Constant_Type)
--                                                  .Digits_Nb.Value).Value;
--                      Sc_Exp : Idl_Fixed_Scale;
--                        renames Result.Value.Const_Type.Scale;
--                   begin
--                      if Sc_Node > Sc_Exp
--                        and then Dg_Node > Dg_Exp + (Sc_Node - Sc_Exp) then
--                         null;
--                      else
--                      Idl_Fe.Errors.Parser_Error
--                        ("constraint error : this value " &
--                                              "is more precise than the " &
--                                              "declared type",
--                                              Idl_Fe.Errors.Error,
--                                              Loc);
--                      end if;
--                   end;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a fixed point number.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_String =>
            case Result.Value.Const_Type.Kind is
               when C_String =>
                  null;
               when C_WString =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not of type string but wide_string.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a string.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Wide_String =>
            case Result.Value.Const_Type.Kind is
               when C_WString =>
                  null;
               when C_String =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not of type wide_string but string.",
                     Idl_Fe.Errors.Error,
                     Loc);
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not a string.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Octet =>
            case Result.Value.Const_Type.Kind is
               when C_Octet =>
                  null;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an octet.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when K_Enum =>
            case Result.Value.Const_Type.Kind is
               when C_Enum =>
                  null;
               when others =>
                  Idl_Fe.Errors.Parser_Error
                    ("constraint error : this value is " &
                     "not an enumeration value.",
                     Idl_Fe.Errors.Error,
                     Loc);
            end case;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Parse_Const_Exp;

   --------------------
   --  Parse_Or_Exp  --
   --------------------
   procedure Parse_Or_Expr (Result : out N_Expr_Acc;
                            Success : out Boolean) is
      Xor_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Xor_Expr (Xor_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Bar then
         declare
            Res : N_Or_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Or_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Xor_Exp);
            Parse_Or_Expr (Res.Right, Success);
            if Success then
               Eval_Or_Expr (Res.Left.Value,
                             Res.Right.Value,
                             Res.Value,
                             Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := Xor_Exp;
      end if;
      return;
   end Parse_Or_Expr;

   ---------------------
   --  Parse_Xor_Exp  --
   ---------------------
   procedure Parse_Xor_Expr (Result : out N_Expr_Acc;
                             Success : out Boolean) is
      And_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_And_Expr (And_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Circumflex then
         declare
            Res : N_Xor_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Xor_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (And_Exp);
            Parse_Xor_Expr (Res.Right, Success);
            if Success then
               Eval_Xor_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := And_Exp;
      end if;
      return;
   end Parse_Xor_Expr;

   ---------------------
   --  Parse_And_Exp  --
   ---------------------
   procedure Parse_And_Expr (Result : out N_Expr_Acc;
                             Success : out Boolean) is
      Shift_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Shift_Expr (Shift_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Ampersand then
         declare
            Res : N_And_Expr_Acc;
         begin
            Next_Token;
            Res := new N_And_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Shift_Exp);
            Parse_And_Expr (Res.Right, Success);
            if Success then
               Eval_And_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := Shift_Exp;
      end if;
      return;
   end Parse_And_Expr;

   -----------------------
   --  Parse_Shift_Exp  --
   -----------------------
   procedure Parse_Shift_Expr (Result : out N_Expr_Acc;
                               Success : out Boolean) is
      Add_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Add_Expr (Add_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Greater_Greater then
         declare
            Res : N_Shr_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Shr_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Add_Exp);
            Parse_Shift_Expr (Res.Right, Success);
            if Success then
               Eval_Shr_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      elsif Get_Token = T_Less_Less then
         declare
            Res : N_Shl_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Shl_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Add_Exp);
            Parse_Shift_Expr (Res.Right, Success);
            if Success then
               Eval_Shl_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := Add_Exp;
      end if;
      return;
   end Parse_Shift_Expr;

   ---------------------
   --  Parse_Add_Exp  --
   ---------------------
   procedure Parse_Add_Expr (Result : out N_Expr_Acc;
                             Success : out Boolean) is
      Mult_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Mult_Expr (Mult_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Plus then
         declare
            Res : N_Add_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Add_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Mult_Exp);
            Parse_Add_Expr (Res.Right, Success);
            if Success then
               Eval_Add_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      elsif Get_Token = T_Minus then
         declare
            Res : N_Sub_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Sub_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Mult_Exp);
            Parse_Add_Expr (Res.Right, Success);
            if Success then
               Eval_Sub_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := Mult_Exp;
      end if;
      return;
   end Parse_Add_Expr;

   ----------------------
   --  Parse_Mult_Exp  --
   ----------------------
   procedure Parse_Mult_Expr (Result : out N_Expr_Acc;
                              Success : out Boolean) is
      Unary_Exp : N_Expr_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Unary_Expr (Unary_Exp, Success);
      if not Success then
         return;
      end if;
      if Get_Token = T_Star then
         declare
            Res : N_Mul_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Mul_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Unary_Exp);
            Parse_Mult_Expr (Res.Right, Success);
            if Success then
               Eval_Mul_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      elsif Get_Token = T_Slash then
         declare
            Res : N_Div_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Div_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Unary_Exp);
            Parse_Mult_Expr (Res.Right, Success);
            if Success then
               Eval_Div_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      elsif Get_Token = T_Percent then
         declare
            Res : N_Mod_Expr_Acc;
         begin
            Next_Token;
            Res := new N_Mod_Expr;
            Set_Location (Result.all, Loc);
            Res.Left := N_Expr_Acc (Unary_Exp);
            Parse_Mult_Expr (Res.Right, Success);
            if Success then
               Eval_Mod_Expr (Res.Left.Value,
                              Res.Right.Value,
                              Res.Value,
                              Loc);
            end if;
            Result := N_Expr_Acc (Res);
         end;
      else
         Result := Unary_Exp;
      end if;
      return;
   end Parse_Mult_Expr;

   -----------------------
   --  Parse_Unary_Exp  --
   -----------------------
   procedure Parse_Unary_Expr (Result : out N_Expr_Acc;
                               Success : out Boolean) is
   begin
      case Get_Next_Token is
         when T_Minus =>
            declare
               Res : N_Neg_Expr_Acc;
            begin
               Next_Token;
               Res := new N_Neg_Expr;
               Set_Location (Res.all, Get_Previous_Token_Location);
               Parse_Primary_Expr (Res.Operand, Success);
               if Success then
                  Eval_Neg_Expr (Res.Operand.Value,
                                 Res.Value,
                                 Get_Previous_Token_Location);
               end if;
               Result := N_Expr_Acc (Res);
            end;
         when T_Plus =>
            declare
               Res : N_Id_Expr_Acc;
            begin
               Next_Token;
               Res := new N_Id_Expr;
               Set_Location (Res.all, Get_Previous_Token_Location);
               Parse_Primary_Expr (Res.Operand, Success);
               if Success then
                  Res.Value := Res.Operand.Value;
               end if;
               Result := N_Expr_Acc (Res);
            end;
         when T_Tilde =>
            declare
               Res : N_Not_Expr_Acc;
            begin
               Next_Token;
               Res := new N_Not_Expr;
               Set_Location (Res.all, Get_Previous_Token_Location);
               Parse_Primary_Expr (Res.Operand, Success);
               if Success then
                  Eval_Not_Expr (Res.Operand.Value,
                                 Res.Value,
                                 Get_Previous_Token_Location);
               end if;
               Result := N_Expr_Acc (Res);
            end;
         when others =>
            Parse_Primary_Expr (Result, Success);
      end case;
      return;
   end Parse_Unary_Expr;

   -------------------------
   --  Parse_Primary_Exp  --
   -------------------------
   procedure Parse_Primary_Expr (Result : out N_Expr_Acc;
                                 Success : out Boolean) is
   begin
      Result := null;
      Success := False;
   end Parse_Primary_Expr;

   --------------------------------
   --  Parse_Positive_Int_Const  --
   --------------------------------
   procedure Parse_Positive_Int_Const (Result : out N_Expr_Acc;
                                       Success : out Boolean) is
   begin
      Result := null;
      Success := False;
   end Parse_Positive_Int_Const;

   ----------------------
   --  Parse_Type_Dcl  --
   ----------------------
   procedure Parse_Type_Dcl (Result : out N_Root_Acc;
                             Success : out Boolean) is
   begin
      Result := null;
      Success := False;
      case Get_Token is
         when T_Typedef =>
            Next_Token;
            declare
               Res : N_Type_Declarator_Acc;
            begin
               Parse_Type_Declarator (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Struct =>
            declare
               Res : N_Struct_Acc;
            begin
               Parse_Struct_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Union =>
            declare
               Res : N_Union_Acc;
            begin
               Parse_Union_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Enum =>
            declare
               Res : N_Enum_Acc;
            begin
               Parse_Enum_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Native =>
            declare
               Res : N_Native_Acc;
            begin
               Res := new N_Native;
               Set_Location (Res.all, Get_Token_Location);
               Next_Token;
               Parse_Simple_Declarator (Res.Declarator, Success);
               if not Success then
                  Result := null;
                  return;
               end if;
               Result :=  N_Root_Acc (Res);
            end;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      return;
   end Parse_Type_Dcl;

   -----------------------------
   --  Parse_Type_Declarator  --
   -----------------------------
   procedure Parse_Type_Declarator (Result : out N_Type_Declarator_Acc;
                                    Success : out Boolean) is
   begin
      Result := new N_Type_Declarator;
      Set_Location (Result.all, Get_Token_Location);
      Parse_Type_Spec (Result.T_Type, Success);
      if not Success then
         pragma Debug (O ("Parse_Type_declarator : type_spec return false"));
         return;
      end if;
      Parse_Declarators (Result.Declarators, Success);
      return;
   end Parse_Type_Declarator;


   -----------------------
   --  Parse_Type_Spec  --
   -----------------------
   procedure Parse_Type_Spec (Result : out N_Root_Acc;
                              Success : out Boolean) is
   begin
      case Get_Token is
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
           | T_Colon_Colon
           | T_Identifier =>
            Parse_Simple_Type_Spec (Result, Success);
         when T_Enum
           | T_Struct
           | T_Union =>
            Parse_Constr_Type_Spec (Result, Success);
         when others =>
            Idl_Fe.Errors.Parser_Error ("type specification expected.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            Success := False;
            Result := null;
      end case;
      return;
   end  Parse_Type_Spec;


   ------------------------------
   --  Parse_Simple_Type_Spec  --
   ------------------------------
   procedure Parse_Simple_Type_Spec (Result : out N_Root_Acc;
                                     Success : out Boolean) is
   begin
      case Get_Token is
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
           | T_ValueBase =>
            Parse_Base_Type_Spec (Result, Success);
         when T_Sequence
           | T_String
           | T_Wstring
           | T_Fixed =>
            Parse_Template_Type_Spec (Result, Success);
         when T_Colon_Colon
           | T_Identifier =>
            declare
               Res : N_Scoped_Name_Acc;
            begin
               Parse_Scoped_Name (Res, Success);
               Result := N_Root_Acc (Res);
               --  FIXME
               --  >>>>>>>>>>>>>>>>> what are the accepted types?
            end;
         when others =>
            Idl_Fe.Errors.Parser_Error ("simple type specification expected.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            Result := null;
            Success := False;
      end case;
      return;
   end Parse_Simple_Type_Spec;

   ----------------------------
   --  Parse_Base_Type_Spec  --
   ----------------------------
   procedure Parse_Base_Type_Spec (Result : out N_Root_Acc;
                                   Success : out Boolean) is
   begin
      case Get_Token is
         when T_Float
           | T_Double =>
            Parse_Floating_Pt_Type (Result, Success);
         when T_Long =>
            if View_Next_Token = T_Double then
               Parse_Floating_Pt_Type (Result, Success);
            else
               Parse_Integer_Type (Result, Success);
            end if;
         when T_Short
           | T_Unsigned =>
            Parse_Integer_Type (Result, Success);
         when T_Char =>
            declare
               Res : N_Char_Acc;
            begin
               Parse_Char_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Wchar =>
            declare
               Res : N_Wide_Char_Acc;
            begin
               Parse_Wide_Char_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Boolean =>
            declare
               Res : N_Boolean_Acc;
            begin
               Parse_Boolean_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Octet =>
            declare
               Res : N_Octet_Acc;
            begin
               Parse_Octet_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Any =>
            declare
               Res : N_Any_Acc;
            begin
               Parse_Any_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Object =>
            declare
               Res : N_Object_Acc;
            begin
               Parse_Object_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_ValueBase =>
            declare
               Res : N_ValueBase_Acc;
            begin
               Parse_Value_Base_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      return;
   end Parse_Base_Type_Spec;

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
--                   Idl_Fe.Errors.Parser_Error
--                     ("`unsigned' must be followed either "
--                                        & "by `short' or `long'",
--                                        Idl_Fe.Errors.Error);
--                   Idl_Fe.Errors.Parser_Error ("`unsigned long' assumed",
--                                        Idl_Fe.Errors.Error);
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
--             Idl_Fe.Errors.Parser_Error ("base type expected",
--                                  Idl_Fe.Errors.Error);
--             raise Idl_Fe.Errors.Internal_Error;
--       end case;

   --------------------------------
   --  Parse_Template_Type_Spec  --
   --------------------------------
   procedure Parse_Template_Type_Spec (Result : out N_Root_Acc;
                                       Success : out Boolean) is
   begin
      case Get_Token is
         when T_Sequence =>
            declare
               Res : N_Sequence_Acc;
            begin
               Parse_Sequence_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_String =>
            declare
               Res : N_String_Acc;
            begin
               Parse_String_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Wstring =>
            declare
               Res : N_Wide_String_Acc;
            begin
               Parse_Wide_String_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Fixed =>
            declare
               Res : N_Fixed_Acc;
            begin
               Parse_Fixed_Pt_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Parse_Template_Type_Spec;

   ------------------------------
   --  Parse_Constr_Type_Spec  --
   ------------------------------
   procedure Parse_Constr_Type_Spec (Result : out N_Root_Acc;
                                     Success : out Boolean) is
   begin
      case Get_Token is
         when T_Struct =>
            declare
               Res : N_Struct_Acc;
            begin
               Parse_Struct_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Union =>
            declare
               Res : N_Union_Acc;
            begin
               Parse_Union_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Enum =>
            declare
               Res : N_Enum_Acc;
            begin
               Parse_Enum_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Parse_Constr_Type_Spec;

   -------------------------
   --  Parse_Declarators  --
   -------------------------
   procedure Parse_Declarators (Result : out Node_List;
                                Success : out Boolean) is
   begin
      Result := Nil_List;
      declare
         Res : N_Declarator_Acc;
      begin
         Parse_Declarator (Res, Success);
         if not Success then
            return;
         else
            Append_Node (Result, N_Root_Acc (Res));
         end if;
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Res : N_Declarator_Acc;
         begin
            Parse_Declarator (Res, Success);
            if not Success then
               return;
            else
               Append_Node (Result, N_Root_Acc (Res));
            end if;
         end;
      end loop;
      return;
   end Parse_Declarators;

   ------------------------
   --  Parse_Declarator  --
   ------------------------
   procedure Parse_Declarator (Result : out N_Declarator_Acc;
                               Success : out Boolean) is
   begin
      if Get_Token /= T_Identifier then
         Idl_Fe.Errors.Parser_Error ("Identifier expected.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         Success := False;
         Result := null;
         return;
      else
         if View_Next_Token = T_Left_Sbracket then
            Parse_Complex_Declarator (Result, Success);
         else
            Parse_Simple_Declarator (Result, Success);
         end if;
      end if;
      --  >>>>>>>>>>>>>>>>>>>>>>> Vince added it
      --    Next_Token;
      return;
   end Parse_Declarator;

   -------------------------------
   --  Parse_Simple_Declarator  --
   -------------------------------
   procedure Parse_Simple_Declarator (Result : out N_Declarator_Acc;
                                      Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      if Get_Token /= T_Identifier then
         Idl_Fe.Errors.Parser_Error ("Identifier expected.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         Success := False;
         return;
      else
         --  try to find a previous definition
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there in a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         else
            Result := new N_Declarator;
            Set_Location (Result.all, Get_Token_Location);
            --  no previous definition
            if not Add_Identifier (Result,
                                   Get_Token_String) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
            Result.Array_Bounds := Nil_List;
         end if;
      end if;
      Success := True;
      Next_Token;
      return;
   end Parse_Simple_Declarator;

   --------------------------------
   --  Parse_Complex_Declarator  --
   --------------------------------
   procedure Parse_Complex_Declarator (Result : out N_Declarator_Acc;
                                       Success : out Boolean)
     renames Parse_Array_Declarator;

   ------------------------------
   --  Parse_Floating_Pt_Type  --
   ------------------------------
   procedure Parse_Floating_Pt_Type (Result : in out N_Root_Acc;
                                     Success : out Boolean) is
   begin
      case Get_Token is
         when T_Float =>
            Next_Token;
            Result := new N_Float;
            Set_Location (Result.all, Get_Token_Location);
         when T_Double =>
            Next_Token;
            Result := new N_Double;
            Set_Location (Result.all, Get_Token_Location);
         when T_Long =>
            Next_Token;
            Next_Token;
            Result := new N_Long_Double;
            Set_Location (Result.all, Get_Token_Location);
         when others =>
               raise Idl_Fe.Errors.Internal_Error;
      end case;
      Success := True;
      return;
   end Parse_Floating_Pt_Type;

   --------------------------
   --  Parse_Integer_Type  --
   --------------------------
   procedure Parse_Integer_Type (Result : in out N_Root_Acc;
                                 Success : out Boolean) is
   begin
      case Get_Token is
         when T_Long
           | T_Short =>
            Parse_Signed_Int (Result, Success);
         when T_Unsigned =>
            Parse_Unsigned_Int (Result, Success);
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Parse_Integer_Type;

   ------------------------
   --  Parse_Signed_Int  --
   ------------------------
   procedure Parse_Signed_Int (Result : in out N_Root_Acc;
                               Success : out Boolean) is
   begin
      case Get_Token is
         when T_Long =>
            if View_Next_Token = T_Long then
               Parse_Signed_Longlong_Int (Result, Success);
            else
               Parse_Signed_Long_Int (Result, Success);
            end if;
         when T_Short =>
            Parse_Signed_Short_Int (Result, Success);
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Parse_Signed_Int;

   ------------------------------
   --  Parse_Signed_Short_Int  --
   ------------------------------
   procedure Parse_Signed_Short_Int (Result : in out N_Root_Acc;
                                     Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Short;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Signed_Short_Int;

   -----------------------------
   --  Parse_Signed_Long_Int  --
   -----------------------------
   procedure Parse_Signed_Long_Int (Result : in out N_Root_Acc;
                                    Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Long;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Signed_Long_Int;

   ---------------------------------
   --  Parse_Signed_Longlong_Int  --
   ---------------------------------
   procedure Parse_Signed_Longlong_Int (Result : in out N_Root_Acc;
                                        Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := new N_Long_Long;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Signed_Longlong_Int;

   --------------------------
   --  Parse_Unsigned_Int  --
   --------------------------
   procedure Parse_Unsigned_Int (Result : in out N_Root_Acc;
                                 Success : out Boolean) is
   begin
      case View_Next_Token is
         when T_Long =>
            if View_Next_Next_Token = T_Long then
               Parse_Unsigned_Longlong_Int (Result, Success);
            else
               Parse_Unsigned_Long_Int (Result, Success);
            end if;
         when T_Short =>
            Parse_Unsigned_Short_Int (Result, Success);
         when others =>
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 9;
               Idl_Fe.Errors.Parser_Error (Ada.Characters.Latin_1.Quotation &
                                    "short" &
                                    Ada.Characters.Latin_1.Quotation &
                                    " or " &
                                    Ada.Characters.Latin_1.Quotation &
                                    "long" &
                                    Ada.Characters.Latin_1.Quotation &
                                    " expected after unsigned.",
                                    Idl_Fe.Errors.Error,
                                    Loc);
               Success := False;
               Result := null;
               return;
            end;
      end case;
   end Parse_Unsigned_Int;

   --------------------------------
   --  Parse_Unsigned_Short_Int  --
   --------------------------------
   procedure Parse_Unsigned_Short_Int (Result : in out N_Root_Acc;
                                       Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := new N_Unsigned_Short;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Short_Int;

   -------------------------------
   --  Parse_Unsigned_Long_Int  --
   -------------------------------
   procedure Parse_Unsigned_Long_Int (Result : in out N_Root_Acc;
                                      Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := new N_Unsigned_Long;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Long_Int;

   -----------------------------------
   --  Parse_Unsigned_Longlong_Int  --
   -----------------------------------
   procedure Parse_Unsigned_Longlong_Int (Result : in out N_Root_Acc;
                                          Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Next_Token;
      Result := new N_Unsigned_Long_Long;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Longlong_Int;

   -----------------------
   --  Parse_Char_Type  --
   -----------------------
   procedure Parse_Char_Type (Result : in out N_Char_Acc;
                              Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Char;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Char_Type;

   ----------------------------
   --  Parse_Wide_Char_Type  --
   ----------------------------
   procedure Parse_Wide_Char_Type (Result : in out N_Wide_Char_Acc;
                                   Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Wide_Char;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Wide_Char_Type;

   --------------------------
   --  Parse_Boolean_Type  --
   --------------------------
   procedure Parse_Boolean_Type (Result : in out N_Boolean_Acc;
                                 Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Boolean;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Boolean_Type;

   ------------------------
   --  Parse_Octet_Type  --
   ------------------------
   procedure Parse_Octet_Type (Result : in out N_Octet_Acc;
                               Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Octet;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Octet_Type;

   ----------------------
   --  Parse_Any_Type  --
   ----------------------
   procedure Parse_Any_Type (Result : in out N_Any_Acc;
                             Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Any;
      Set_Location (Result.all, Get_Token_Location);
      Success := True;
   end Parse_Any_Type;

   -------------------------
   --  Parse_Object_Type  --
   -------------------------
   procedure Parse_Object_Type (Result : in out N_Object_Acc;
                                Success : out Boolean) is
   begin
      Result := null;
      Success := False;
   end Parse_Object_Type;

   -------------------------
   --  Parse_Struct_Type  --
   -------------------------
   procedure Parse_Struct_Type (Result : out N_Struct_Acc;
                                Success : out Boolean) is
      Name : String_Ptr;
   begin
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Idl_Fe.Errors.Parser_Error
              ("identifier expected in struct declaration.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
         end;
      end if;
      declare
         Definition : Identifier_Definition_Acc;
      begin
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         end if;
         Result := new N_Struct;
         Set_Location (Result.all, Get_Token_Location);
         --  here we keep the name of the struct without adding it
         --  to the scope in order to avoid recursive constructed
         --  type as in :
         --      struct foo {
         --          foo chain;
         --  }
         Name := new String'(Get_Token_String);
      end;
      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("'{' expected in struct definition.",
               Idl_Fe.Errors.Error,
               Loc);
            Success := False;
            if not Add_Identifier (Result,
                                   Name.all) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
            Free_String_Ptr (Name);
            return;
         end;
      end if;
      Next_Token;
      Push_Scope (Result);
      Parse_Member_List (Result.Members, Success);
      Pop_Scope;
      if not Success then
         return;
      end if;
      if not Add_Identifier (Result, Name.all) then
         --  the error was raised before <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         Success := False;
         null;
      end if;
      Free_String_Ptr (Name);
      if Get_Token /= T_Right_Cbracket then
         Idl_Fe.Errors.Parser_Error
           ("'}' expected at the end of struct definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Struct_Type;


   -------------------------
   --  Parse_Member_List  --
   -------------------------
   procedure Parse_Member_List (Result : out Node_List;
                                Success : out Boolean) is
   begin
      Result := Nil_List;
      if Get_Token = T_Right_Cbracket then
         Idl_Fe.Errors.Parser_Error
           ("member expected : a struct may not be empty.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
      end if;
      loop
         declare
            Member : N_Member_Acc;
            Member_Success : Boolean;
         begin
            Parse_Member (Member, Member_Success);
            if not Member_Success then
               Go_To_Next_Member;
            else
               Append_Node (Result, N_Root_Acc (Member));
            end if;
         end;
         exit when Get_Token = T_Right_Cbracket;
      end loop;
         Success := True;
         return;
   end Parse_Member_List;

   --------------------
   --  Parse_Member  --
   --------------------
   procedure Parse_Member (Result : out N_Member_Acc;
                           Success : out Boolean) is
      Type_Spec : N_Root_Acc;
      Loc : Idl_Fe.Errors.Location;
   begin
      Loc := Get_Token_Location;
      Parse_Type_Spec (Type_Spec, Success);
      if not Success then
         return;
      end if;
      Result := new N_Member;
      Set_Location (Result.all, Loc);
      Result.M_Type := Type_Spec;
      Parse_Declarators (Result.Decl, Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Idl_Fe.Errors.Parser_Error
           ("';' expected at the end of member declaration.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      --  to eat the semi-colon
      Next_Token;
      return;
   end Parse_Member;

   ------------------------
   --  Parse_Union_Type  --
   ------------------------
   procedure Parse_Union_Type (Result : out N_Union_Acc;
                               Success : out Boolean) is
      Name : String_Ptr;
   begin
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 6;
            Idl_Fe.Errors.Parser_Error
              ("identifier expected in union definition.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            return;
         end;
      end if;
      declare
         Definition : Identifier_Definition_Acc;
      begin
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         end if;
         Result := new N_Union;
         Set_Location (Result.all, Get_Token_Location);
         --  here we keep the name of the union without adding it
         --  to the scope in order to avoid recursive constructed
         --  type as in :
         --      union foo {
         --          foo chain;
         --  }
         Name := new String'(Get_Token_String);
      end;
      Next_Token;
      if Get_Token /= T_Switch then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("switch expected in union definition.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 2;
            Idl_Fe.Errors.Parser_Error
              ("'(' expected after " &
               Ada.Characters.Latin_1.Quotation &
               "switch" &
               Ada.Characters.Latin_1.Quotation &
               ".",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      Push_Scope (Result);
      Parse_Switch_Type_Spec (Result.Switch_Type, Success);
      if not Success then
         Pop_Scope;
         return;
      end if;
      if Get_Token /= T_Right_Paren then
         Idl_Fe.Errors.Parser_Error
           ("')' expected at the end of switch " &
            "specification.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         Pop_Scope;
         return;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 2;
            Idl_Fe.Errors.Parser_Error
              ("'{' expected at the beginning of union.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            Pop_Scope;
            return;
         end;
      end if;
      Next_Token;
      Parse_Switch_Body (Result.Cases,
                         Result.Switch_Type,
                         Success);
      Pop_Scope;
      if not Success then
         return;
      end if;
      if not Add_Identifier (Result, Name.all) then
         --  the error was raised before
         Success := False;
         null;
      end if;
      Free_String_Ptr (Name);
      if Get_Token /= T_Right_Cbracket then
            Idl_Fe.Errors.Parser_Error
              ("'}' expected at the end of union.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Result := null;
            Success := False;
            return;
      end if;
      Next_Token;
      return;
   end Parse_Union_Type;

   ------------------------------
   --  Parse_Switch_Type_Spec  --
   ------------------------------
   procedure Parse_Switch_Type_Spec (Result : out N_Root_Acc;
                                     Success : out Boolean) is
   begin
      case Get_Token is
         when T_Long
           | T_Short
           | T_Unsigned =>
            Parse_Integer_Type (Result, Success);
         when T_Char =>
            declare
               Res : N_Char_Acc;
            begin
               Parse_Char_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Boolean =>
            declare
               Res : N_Boolean_Acc;
            begin
               Parse_Boolean_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Enum =>
            declare
               Res : N_Enum_Acc;
            begin
               Parse_Enum_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Colon_Colon
           | T_Identifier =>
            declare
               Res : N_Scoped_Name_Acc;
            begin
               Parse_Scoped_Name (Res, Success);
               --  The <scoped_name> in the <switch_type_spec> production
               --  must be a previously defined integer, char, boolean
               --  or enum type.
               case Get_Kind (Res.Value.all) is
                  when K_Short
                    | K_Long
                    | K_Long_Long
                    | K_Unsigned_Short
                    | K_Unsigned_Long
                    | K_Unsigned_Long_Long
                    | K_Char
                    | K_Wide_Char
                    | K_Boolean
                    | K_Enum =>
                     null;
                  when others =>
                     Idl_Fe.Errors.Parser_Error
                       ("Invalid type in switch. The " &
                        "scoped name should refer to " &
                        "an integer, char, boolean or " &
                        " enum type.",
                        Idl_Fe.Errors.Error,
                        Get_Token_Location);
               end case;
               Result := N_Root_Acc (Res);
            end;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("switch type expected.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
      end case;
      return;
   end Parse_Switch_Type_Spec;

   -------------------------
   --  Parse_Switch_Body  --
   -------------------------
   procedure Parse_Switch_Body (Result : out Node_List;
                                Switch_Type : in N_Root_Acc;
                                Success : out Boolean) is
      Default_Clause : Boolean := False;
   begin
      Result := Nil_List;
      if Get_Token = T_Right_Cbracket then
         Idl_Fe.Errors.Parser_Error
           ("case clause expected : " &
            "a union may not be empty.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
      end if;
      loop
         declare
            Case_Clause : N_Case_Acc;
            Case_Success : Boolean;
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Token_Location;
            Parse_Case (Case_Clause,
                        Switch_Type,
                        Case_Success);
            if not Case_Success then
               Go_To_End_Of_Case;
            else
               Append_Node (Result, N_Root_Acc (Case_Clause));
               if Default_Clause then
                  if Is_In_List (Case_Clause.Labels, null) then
                     Idl_Fe.Errors.Parser_Error
                       ("default clause already appeared.",
                        Idl_Fe.Errors.Error,
                        Loc);
                  else
                     Idl_Fe.Errors.Parser_Error
                       ("useless clause : default " &
                        "already appeared.",
                        Idl_Fe.Errors.Warning,
                        Loc);
                  end if;
               else
                  if Is_In_List (Case_Clause.Labels, null) then
                     Default_Clause := True;
                  end if;
               end if;
            end if;
         end;
         exit when Get_Token = T_Right_Cbracket;
      end loop;
      Release_All_Used_Values;
      Success := True;
      return;
   end Parse_Switch_Body;

   ------------------
   --  Parse_Case  --
   ------------------
   procedure Parse_Case (Result : out N_Case_Acc;
                         Switch_Type : in N_Root_Acc;
                         Success : out Boolean) is
   begin
      case Get_Token is
         when T_Case
           | T_Default =>
            null;
         when others =>
            Idl_Fe.Errors.Parser_Error ("invalid case label : " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "case" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " or " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "default" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " expected.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            Result := null;
            Success := False;
            return;
      end case;
      Result := new N_Case;
      Set_Location (Result.all, Get_Token_Location);
      Result.Labels := Nil_List;
      while Get_Token = T_Case or Get_Token = T_Default loop
         declare
            Case_Label : N_Expr_Acc;
            Case_Success : Boolean;
         begin
            Parse_Case_Label (Case_Label, Switch_Type, Case_Success);
            if not Case_Success then
               Go_To_End_Of_Case_Label;
            else
               Append_Node (Result.Labels, N_Root_Acc (Case_Label));
            end if;
         end;
      end loop;
      Parse_Element_Spec (Result.Case_Type,
                          Result.Case_Decl,
                          Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Idl_Fe.Errors.Parser_Error
           ("';' expected at the end of case clause.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
      else
         Next_Token;
      end if;
      return;
   end Parse_Case;

   ------------------------
   --  Parse_Case_Label  --
   ------------------------
   procedure Parse_Case_Label (Result : out N_Expr_Acc;
                               Switch_Type : in N_Root_Acc;
                               Success : out Boolean) is
   begin
      case Get_Token is
         when T_Case =>
            declare
               Loc : Idl_Fe.Errors.Location;
            begin
               Next_Token;
               Loc := Get_Token_Location;
               Parse_Const_Exp (Result, Switch_Type, Success);
               if not Success then
                  return;
               end if;
               --  Verifying that a clause does not appear twice
               if not Add_Used_Value (Result) then
                  Idl_Fe.Errors.Parser_Error
                    ("This value was already taken into " &
                     "account in this switch statement.",
                     Idl_Fe.Errors.Warning,
                     Loc);
               end if;
            end;
         when T_Default =>
            Next_Token;
            Result := null;
            Success := True;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      if Get_Token /= T_Colon_Colon then
         Idl_Fe.Errors.Parser_Error
           ("':' expected at the end of case label.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
      else
         Next_Token;
      end if;
      return;
   end Parse_Case_Label;

   --------------------------
   --  Parse_Element_Spec  --
   --------------------------
   procedure Parse_Element_Spec (Element_Type : out N_Root_Acc;
                                 Element_Decl : out N_Declarator_Acc;
                                 Success : out Boolean) is
   begin
      Parse_Type_Spec (Element_Type, Success);
      if not Success then
         return;
      end if;
      Parse_Declarator (Element_Decl, Success);
      return;
   end Parse_Element_Spec;

   -----------------------
   --  Parse_Enum_Type  --
   -----------------------
   procedure Parse_Enum_Type (Result : out N_Enum_Acc;
                              Success : out Boolean) is
   begin
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 5;
            Idl_Fe.Errors.Parser_Error
              ("identifier expected in enumeration " &
               "definition.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            return;
         end;
      end if;
      Result := new N_Enum;
      Set_Location (Result.all, Get_Token_Location);
      Result.Enumerators := Nil_List;
      declare
         Definition : Identifier_Definition_Acc;
      begin
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            return;
         else
            if not Add_Identifier (Result,
                                   Get_Token_String) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
         end if;
      end;
      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("'{' expected in enumeration definition.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      if Get_Token = T_Right_Cbracket then
         Idl_Fe.Errors.Parser_Error
           ("identifier expected : " &
            "an enumeration may not be empty.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
      end if;
      declare
         Enum : N_Enumerator_Acc;
      begin
         Parse_Enumerator (Enum, Success);
         if not Success then
            return;
         end if;
         Append_Node (Result.Enumerators, N_Root_Acc (Enum));
      end;
      declare
         Count : Idl_Enum := 1;
      begin
         while Get_Token = T_Comma loop
            Next_Token;
            declare
               Enum : N_Enumerator_Acc;
            begin
               Parse_Enumerator (Enum, Success);
               if not Success then
                  return;
               end if;
               Count := Count + 1;
               if Count = 0 then
                  Idl_Fe.Errors.Parser_Error
                    ("two much possible values in this " &
                     "enumeration : maximum is 2^32.",
                     Idl_Fe.Errors.Error,
                     Get_Token_Location);
               end if;
               Append_Node (Result.Enumerators, N_Root_Acc (Enum));
            end;
         end loop;
      end;
      if Get_Token /= T_Right_Cbracket then
         Idl_Fe.Errors.Parser_Error
           ("'}' expected at the end of enumeration " &
            "definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Enum_Type;

   ------------------------
   --  Parse_Enumerator  --
   ------------------------
   procedure Parse_Enumerator (Result : out N_Enumerator_Acc;
                               Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      if Get_Token /= T_Identifier then
         Idl_Fe.Errors.Parser_Error ("Identifier expected.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         Success := False;
         return;
      else
         Result := new N_Enumerator;
         Set_Location (Result.all, Get_Token_Location);
         --  try to find a previous definition
         Definition := Find_Identifier_Definition (Get_Token_String);
         --  Is there a previous definition and in the same scope ?
         if Definition /= null
           and then (Definition.Parent_Scope = Get_Current_Scope or
           Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
         then
            Idl_Fe.Errors.Parser_Error
              ("This identifier is already used in this scope : " &
               Idl_Fe.Errors.Display_Location
               (Get_Location (Definition.Node.all)),
               Idl_Fe.Errors.Error,
               Get_Token_Location);
         else
            --  no previous definition
            if not Add_Identifier (Result,
                                   Get_Token_String) then
               raise Idl_Fe.Errors.Internal_Error;
            end if;
         end if;
      end if;
      Success := True;
      --  eat the identifier
      Next_Token;
      return;
   end Parse_Enumerator;

   ---------------------------
   --  Parse_Sequence_Type  --
   ---------------------------
   procedure Parse_Sequence_Type (Result : out N_Sequence_Acc;
                                  Success : out Boolean) is
   begin
      Next_Token;
      if Get_Token /= T_Less then
         Idl_Fe.Errors.Parser_Error
           ("'<' expected in sequence definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Result := new N_Sequence;
      Set_Location (Result.all, Get_Previous_Token_Location);
      Next_Token;
      Parse_Simple_Type_Spec (Result.Sequence_Type, Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Comma and Get_Token /= T_Greater then
         Idl_Fe.Errors.Parser_Error
           ("',' or '>' expected in sequence definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      if Get_Token = T_Comma then
         Next_Token;
         Parse_Positive_Int_Const (Result.Bound, Success);
         if not Success then
            return;
         end if;
      else
         Result.Bound := null;
      end if;
      case Get_Token is
         when T_Greater =>
            Next_Token;
         when T_Greater_Greater =>
            if Get_Kind (Get_Current_Scope.all) = K_Sequence then
               Idl_Fe.Errors.Parser_Error
                 ("'>' expected. You probably forgot " &
                  "the space between both letters.",
                  Idl_Fe.Errors.Error,
                  Get_Token_Location);
            else
               Idl_Fe.Errors.Parser_Error
                 ("'>' expected at the end of "
                  & "sequence definition.",
                  Idl_Fe.Errors.Error,
                  Get_Token_Location);
               Success := False;
               return;
            end if;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("'>' expected at the end of "
               & "sequence definition.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
      end case;
      return;
   end Parse_Sequence_Type;

   -------------------------
   --  Parse_String_Type  --
   -------------------------
   procedure Parse_String_Type (Result : out N_String_Acc;
                                Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_String;
      Set_Location (Result.all, Get_Previous_Token_Location);
      if Get_Token = T_Less then
         Parse_Positive_Int_Const (Result.Bound, Success);
         if not Success then
            return;
         end if;
         if Get_Token /= T_Greater then
            Idl_Fe.Errors.Parser_Error
              ("'>' expected in string definition.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
         end if;
      else
         Result.Bound := null;
      end if;
      Success := True;
      return;
   end Parse_String_Type;

   ------------------------------
   --  Parse_Wide_String_Type  --
   ------------------------------
   procedure Parse_Wide_String_Type (Result : out N_Wide_String_Acc;
                                     Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Wide_String;
      Set_Location (Result.all, Get_Previous_Token_Location);
      if Get_Token = T_Less then
         Parse_Positive_Int_Const (Result.Bound, Success);
         if not Success then
            return;
         end if;
         if Get_Token /= T_Greater then
            Idl_Fe.Errors.Parser_Error
              ("'>' expected in wide string definition.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
         end if;
      else
         Result.Bound := null;
      end if;
      Success := True;
      return;
   end Parse_Wide_String_Type;

   ------------------------------
   --  Parse_Array_Declarator  --
   ------------------------------
   procedure Parse_Array_Declarator (Result : out N_Declarator_Acc;
                                     Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := new N_Declarator;
      Set_Location (Result.all, Get_Token_Location);
      --  try to find a previous definition
      Definition := Find_Identifier_Definition (Get_Token_String);
      --  Is there a previous definition and in the same scope ?
      if Definition /= null
        and then (Definition.Parent_Scope = Get_Current_Scope or
        Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
      then
         Idl_Fe.Errors.Parser_Error
           ("This identifier is already used in this scope : " &
            Idl_Fe.Errors.Display_Location
            (Get_Location (Definition.Node.all)),
            Idl_Fe.Errors.Error,
            Get_Token_Location);
      else
         --  no previous definition
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Idl_Fe.Errors.Internal_Error;
         end if;
      end if;
      Result.Array_Bounds := Nil_List;
      while Get_Next_Token = T_Left_Sbracket loop
         declare
            Expr : N_Expr_Acc;
         begin
            Parse_Fixed_Array_Size (Expr, Success);
            if not Success then
               return;
            end if;
            Append_Node (Result.Array_Bounds, N_Root_Acc (Expr));
         end;
      end loop;
      return;
   end Parse_Array_Declarator;

   ------------------------------
   --  Parse_Fixed_Array_Size  --
   ------------------------------
   procedure Parse_Fixed_Array_Size (Result : out N_Expr_Acc;
                                     Success : out Boolean) is
   begin
      Next_Token;
      Parse_Positive_Int_Const (Result, Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Right_Sbracket then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("']' expected in array definition.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Fixed_Array_Size;

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

   --------------------
   --  parse_op_dcl  --
   --------------------
   procedure Parse_Op_Dcl (Result : out N_Operation_Acc;
                           Success : out Boolean) is
   begin
      Result := new N_Operation;
      Set_Location (Result.all, Get_Token_Location);
      if Get_Token = T_Oneway then
         Result.Is_Oneway := True;
         Next_Token;
      else
         Result.Is_Oneway := False;
      end if;
      Parse_Op_Type_Spec (Result.Operation_Type, Success);
      if not Success then
         return;
      end if;
      if Get_Token /= T_Identifier then
         Idl_Fe.Errors.Parser_Error
           ("Identifier expected in operation declaration.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         declare
            Definition : Identifier_Definition_Acc;
         begin
            --  try to find a previous definition
            Definition := Find_Identifier_Definition (Get_Token_String);
            --  Is there a previous definition and in the same scope ?
            if Definition /= null
              and then (Definition.Parent_Scope = Get_Current_Scope or
             Check_Imported_Identifier_Index (Get_Token_String) /= Nil_Uniq_Id)
            then
               Idl_Fe.Errors.Parser_Error
                 ("This identifier is already used in this scope : " &
                  Idl_Fe.Errors.Display_Location
                  (Get_Location (Definition.Node.all)),
                  Idl_Fe.Errors.Error,
                  Get_Token_Location);
               pragma Debug (O ("Parse_op_dcl : bad identifier"));
               Result := null;
               Success := False;
               return;
            else
               --  no previous definition
               if not Add_Identifier (Result,
                                      Get_Token_String) then
                  raise Idl_Fe.Errors.Internal_Error;
               end if;
            end if;
         end;
      end if;
      Next_Token;
      Push_Scope (Result);
      Parse_Parameter_Dcls (Result.Parameters, Success);
      Pop_Scope;
      if not Success then
         return;
      end if;
      if Get_Token = T_Raises then
         Parse_Raises_Expr (Result.Raises, Success);
         if not Success then
            return;
         end if;
      else
         Result.Raises := Nil_List;
      end if;
      if Get_Token = T_Context then
         Parse_Context_Expr (Result.Contexts, Success);
         if not Success then
            return;
         end if;
      else
         Result.Contexts := Nil_List;
      end if;
      return;
   end Parse_Op_Dcl;

   --------------------------
   --  Parse_Op_Type_Spec  --
   --------------------------
   procedure Parse_Op_Type_Spec (Result : out N_Root_Acc;
                                 Success : out Boolean) is
   begin
      case Get_Token is
         when T_Void =>
            Result := new N_Void;
            Set_Location (Result.all, Get_Token_Location);
            Next_Token;
            Success := True;
            return;
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
           | T_Colon_Colon
           | T_Identifier =>
            Parse_Param_Type_Spec (Result, Success);
            return;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("void" &
               " or type specification expected.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := null;
            return;
      end case;
   end Parse_Op_Type_Spec;

   ----------------------------
   --  Parse_Parameter_Dcls  --
   ----------------------------
   procedure Parse_Parameter_Dcls (Result : out  Node_List;
                                   Success : out Boolean) is
   begin
      Result := Nil_List;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Idl_Fe.Errors.Parser_Error
              ("'(' expected in operation definition.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token /= T_Right_Paren then
         declare
            Param : N_Param_Acc;
         begin
            Parse_Param_Dcl (Param, Success);
            if not Success then
               return;
            end if;
            Append_Node (Result, N_Root_Acc (Param));
         end;
      end if;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Param : N_Param_Acc;
         begin
            Parse_Param_Dcl (Param, Success);
            if not Success then
               return;
            end if;
            Append_Node (Result, N_Root_Acc (Param));
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Idl_Fe.Errors.Parser_Error
           ("')' expected at the end of the " &
            "parameters definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Parameter_Dcls;

   -----------------------
   --  Parse_Param_Dcl  --
   -----------------------
   procedure Parse_Param_Dcl (Result : out N_Param_Acc;
                              Success : out boolean) is
      Attr_Success : Boolean;
   begin
      Result := new N_Param;
      Set_Location (Result.all, Get_Token_Location);
      Parse_Param_Attribute (Result.Mode, Attr_Success);
      if not Attr_Success then
         case Get_Token is
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
              | T_Colon_Colon
              | T_Identifier =>
               null;
            when others =>
               Success := False;
               return;
         end case;
      end if;
      Parse_Param_Type_Spec (Result.Param_Type, Success);
      if not Success then
         return;
      end if;
      Parse_Simple_Declarator (Result.Declarator, Success);
      return;
   end Parse_Param_Dcl;

   -----------------------------
   --  Parse_Param_Attribute  --
   -----------------------------
   procedure Parse_Param_Attribute (Result : out Param_Mode;
                                    Success : out Boolean) is
   begin
      case Get_Token is
         when T_In =>
            Result := Mode_In;
         when T_Out =>
            Result := Mode_Out;
         when T_Inout =>
            Result := Mode_Inout;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("mode expected (in, out or inout).",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Result := Mode_In;
            Success := False;
            return;
      end case;
      Next_Token;
      Success := True;
      return;
   end Parse_Param_Attribute;

   -------------------------
   --  Parse_Raises_Expr  --
   -------------------------
   procedure Parse_Raises_Expr (Result : out Node_List;
                                Success : out Boolean) is
   begin
      Result := Nil_List;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Idl_Fe.Errors.Parser_Error
              ("'(' expected in raises statement.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token = T_Right_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 1;
            Idl_Fe.Errors.Parser_Error
              ("scoped_name expected : a raise statement " &
               "may not be empty.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Next_Token;
         Success := True;
         return;
      end if;
      declare
         Name : N_Scoped_Name_Acc;
      begin
         Parse_Scoped_Name (Name, Success);
         if not Success then
            return;
         end if;
         Append_Node (Result, N_Root_Acc (Name));
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Name : N_Scoped_Name_Acc;
         begin
            Parse_Scoped_Name (Name, Success);
            if not Success then
               return;
            end if;
            Append_Node (Result, N_Root_Acc (Name));
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Idl_Fe.Errors.Parser_Error
           ("')' expected at the end of the " &
            "raises statement.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      return;
   end Parse_Raises_Expr;

   --------------------------
   --  Parse_Context_Expr  --
   --------------------------
   procedure Parse_Context_Expr (Result : out Node_List;
                                 Success : out Boolean) is
   begin
      Result := Nil_List;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Idl_Fe.Errors.Parser_Error
              ("'(' expected in context statement.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token = T_Right_Paren then
         declare
            Loc : Idl_Fe.Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 1;
            Idl_Fe.Errors.Parser_Error
              ("string literal expected : a context " &
               "statement may not be empty.",
               Idl_Fe.Errors.Error,
               Loc);
         end;
         Next_Token;
         Success := True;
         return;
      end if;
      declare
         Name : N_Lit_String_Acc;
      begin
         Parse_String_Literal (Name, Success);
         if not Success then
            return;
         end if;
         Check_Context_String (Name.Value.all);
         Append_Node (Result, N_Root_Acc (Name));
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Name : N_Lit_String_Acc;
         begin
            Parse_String_Literal (Name, Success);
            if not Success then
               return;
            end if;
            Check_Context_String (Name.Value.all);
            Append_Node (Result, N_Root_Acc (Name));
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Idl_Fe.Errors.Parser_Error
           ("')' expected at the end of the " &
            "context statement.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      return;
   end Parse_Context_Expr;

   -----------------------------
   --  Parse_Param_Type_Spec  --
   -----------------------------
   procedure Parse_Param_Type_Spec (Result : out N_Root_Acc;
                                    Success : out Boolean) is
   begin
      case Get_Token is
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
           | T_ValueBase =>
            Parse_Base_Type_Spec (Result, Success);
         when T_String =>
            declare
               Res : N_String_Acc;
            begin
               Parse_String_Type (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when T_Colon_Colon | T_Identifier =>
            declare
               Res : N_Scoped_Name_Acc;
            begin
               Parse_Scoped_Name (Res, Success);
               Result := N_Root_Acc (Res);
            end;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("param type specifier expected.",
               Idl_Fe.Errors.Error,
               Get_Token_Location);
            Success := False;
      end case;
      return;
   end Parse_Param_Type_Spec;

   ---------------------------
   --  Parse_Fixed_Pt_Type  --
   ---------------------------
   procedure Parse_Fixed_Pt_Type (Result : out N_Fixed_Acc;
                                  Success : out Boolean) is
   begin
      Next_Token;
      Result := new N_Fixed;
      Set_Location (Result.all, Get_Previous_Token_Location);
      if Get_Token /= T_Less then
         Idl_Fe.Errors.Parser_Error
           ("'<' expected in fixed point type definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      Parse_Positive_Int_Const (Result.Digits_Nb, Success);
      --  FIXME
--       if Result.Digits_Nb.Value.all < 0
--         or Result.Digits_Nb.Value.all > 31 then
--          Idl_Fe.Errors.Parser_Error
--            ("invalid number of digits in fixed point " &
--                               "type definition : it should be in range " &
--                               "0 .. 31.",
--                               Idl_Fe.Errors.Error,
--                               Get_Token_Location);
--       end if;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Comma then
         Idl_Fe.Errors.Parser_Error
           ("',' expected in fixed point type definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      Parse_Positive_Int_Const (Result.Scale, Success);
      --  FIXME
--       if Result.Scale.Value.all < 0 then
--          Idl_Fe.Errors.Parser_Error
--             ("invalid scale factor in fixed point " &
--                               "type definition : it may not be negative.",
--                               Idl_Fe.Errors.Error,
--                               Get_Token_Location);
--       elsif Result.Digits_Nb.Value.all >= Result.Scale.Value.all then
--          Idl_Fe.Errors.Parser_Error
--             ("invalid scale factor in fixed point " &
--                               "type definition : it should not exceed" &
--                               "the number of digits.",
--                               Idl_Fe.Errors.Error,
--                               Get_Token_Location);
--       end if;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Greater then
         Idl_Fe.Errors.Parser_Error
           ("'>' expected in fixed point type definition.",
            Idl_Fe.Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Fixed_Pt_Type;

   -----------------------------
   --  Parse_Value_Base_Type  --
   -----------------------------
   procedure Parse_Value_Base_Type (Result : in out N_ValueBase_Acc;
                                    Success : out Boolean) is
   begin
      Result := null;
      Success := False;
   end Parse_Value_Base_Type;



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
--                Res.Lit := new String'(Idl_Fe.Lexer.Get_Token_String);
--                Next_Token;
--                return N_Root_Acc (Res);
--             end;
--          when T_Lit_Simple_Floating_Point =>
--             declare
--                Res : N_Lit_Floating_Point_Acc;
--             begin
--                Res := new N_Lit_Floating_Point;
--                Res.Lit := new String'(Idl_Fe.Lexer.Get_Token_String);
--                Next_Token;
--                return N_Root_Acc (Res);
--             end;
--          when others =>
--             raise Idl_Fe.Errors.Internal_Error;
--       end case;
--    end Parse_Literal;


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
--             Idl_Fe.Errors.Parser_Error
--                ("bad token for a primary expression: "
--                         & Image (Token),
--                                   Idl_Fe.Errors.Error);
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



   ---------------------------
   --  Parsing of literals  --
   ---------------------------

   ----------------------------
   --  Parse_String_Literal  --
   ----------------------------
   procedure Parse_String_Literal (Result : out N_Lit_String_Acc;
                                   Success : out Boolean) is
   begin
      Result := null;
      Success := False;
   end Parse_String_Literal;

   ----------------------------
   --  Check_Context_String  --
   ----------------------------
   procedure Check_Context_String (S : in String) is
      use GNAT.Case_Util;
      use Ada.Characters.Latin_1;
   begin
      if To_Lower (S (S'First)) not in LC_A .. LC_Z then
         Idl_Fe.Errors.Parser_Error ("invalid string for context " &
                              "declaration : the first character " &
                              "must be an alphabetic one.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         return;
      end if;
      for I in S'First + 1 .. S'Last - 1 loop
         if To_Lower (S (I)) not in LC_A .. LC_Z
           and S (I) not in '0' .. '9'
           and S (I) /= '.'
           and S (I) /= '_' then
            Idl_Fe.Errors.Parser_Error ("invalid string for context " &
                                 "declaration : it may only content " &
                                 "alphabetic, digit, period, underscore " &
                                 "characters plus an asterisk at the end.",
                                 Idl_Fe.Errors.Error,
                                 Get_Token_Location);
            return;
         end if;
      end loop;
      if To_Lower (S (S'Last)) not in LC_A .. LC_Z
        and S (S'Last) not in '0' .. '9'
        and S (S'Last) /= '.'
        and S (S'Last) /= '_'
        and S (S'Last) /= '*' then
         Idl_Fe.Errors.Parser_Error ("invalid string for context " &
                              "declaration : the last character may only " &
                              "be an alphabetic, digit, period, " &
                              "underscore or asterisk character.",
                              Idl_Fe.Errors.Error,
                              Get_Token_Location);
         return;
      end if;
   end Check_Context_String;


   ---------------------------------
   --  evaluation of expressions  --
   ---------------------------------
   --------------------
   --  Eval_Or_Expr  --
   --------------------
   procedure Eval_Or_Expr (Left : in Value_Ptr;
                           Right : in Value_Ptr;
                           Result : out Value_Ptr;
                           Loc : in Idl_Fe.Errors.Location) is
   begin
      if Left = null or Right = null then
         Result := null;
         return;
      end if;
      case Left.Const_Type.Kind is
         when C_Short
           | C_UShort
           | C_Long
           | C_ULong
           | C_LongLong
           | C_ULongLong =>
            null;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("invalid type in the left term of this " &
               "expression. Or expression is only " &
               "applicable to integer expressions.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            return;
      end case;
      case Right.Const_Type.Kind is
         when C_Short
           | C_UShort
           | C_Long
           | C_ULong
           | C_LongLong
           | C_ULongLong =>
            null;
         when others =>
            Idl_Fe.Errors.Parser_Error
              ("invalid type in the right term of this " &
               "expression. Or expression is only " &
               "applicable to integer expressions.",
               Idl_Fe.Errors.Error,
               Loc);
            Result := null;
            return;
      end case;
      case Right.Const_Type.Kind is
         when C_Short =>
            case Left.Const_Type.Kind is
               when C_Short =>
                  Result := new Short_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Short);
                  Short_Value_Ptr (Result).Value :=
                    Short_Value_Ptr (Right).Value or
                    Short_Value_Ptr (Left).Value;
               when C_UShort =>
                  if UShort_Value_Ptr (Left).Value <=
                    Idl_UShort (Idl_Short'Last) then
                     Result := new Short_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Short);
                     Short_Value_Ptr (Result).Value :=
                       Short_Value_Ptr (Right).Value or
                       Idl_Short (UShort_Value_Ptr (Left).Value);
                  else
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Idl_Long (Short_Value_Ptr (Right).Value) or
                       Idl_Long (UShort_Value_Ptr (Left).Value);
                  end if;
               when C_Long =>
                  Result := new Long_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Long);
                  Long_Value_Ptr (Result).Value :=
                    Idl_Long (Short_Value_Ptr (Right).Value) or
                    Long_Value_Ptr (Left).Value;
               when C_ULong =>
                  if ULong_Value_Ptr (Left).Value <=
                    Idl_ULong (Idl_Long'Last) then
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Idl_Long (Short_Value_Ptr (Right).Value) or
                       Idl_Long (ULong_Value_Ptr (Left).Value);
                  else
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (Short_Value_Ptr (Right).Value) or
                       Idl_LongLong (ULong_Value_Ptr (Left).Value);
                  end if;
               when C_LongLong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    Idl_LongLong (Short_Value_Ptr (Right).Value) or
                    LongLong_Value_Ptr (Left).Value;
               when C_ULongLong =>
                  if ULongLong_Value_Ptr (Left).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (Short_Value_Ptr (Right).Value) or
                       Idl_LongLong (ULongLong_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is negative and the other one is " &
                        "unsigned long long.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when C_UShort =>
            case Left.Const_Type.Kind is
               when C_UShort =>
                  Result := new UShort_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_UShort);
                  UShort_Value_Ptr (Result).Value :=
                    UShort_Value_Ptr (Right).Value or
                    UShort_Value_Ptr (Left).Value;
               when C_Short =>
                  if UShort_Value_Ptr (Right).Value <=
                    Idl_UShort (Idl_Short'Last) then
                     Result := new Short_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Short);
                     Short_Value_Ptr (Result).Value :=
                       Idl_Short (UShort_Value_Ptr (Right).Value) or
                       Short_Value_Ptr (Left).Value;
                  else
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Idl_Long (UShort_Value_Ptr (Right).Value) or
                       Idl_Long (Short_Value_Ptr (Left).Value);
                  end if;
               when C_Long =>
                  Result := new Long_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Long);
                  Long_Value_Ptr (Result).Value :=
                    Idl_Long (UShort_Value_Ptr (Right).Value) or
                    Long_Value_Ptr (Left).Value;
               when C_ULong =>
                  Result := new ULong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULong);
                  ULong_Value_Ptr (Result).Value :=
                    Idl_ULong (UShort_Value_Ptr (Right).Value) or
                    ULong_Value_Ptr (Left).Value;
               when C_LongLong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    Idl_LongLong (UShort_Value_Ptr (Right).Value) or
                    LongLong_Value_Ptr (Left).Value;
               when C_ULongLong =>
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value :=
                    Idl_ULongLong (UShort_Value_Ptr (Right).Value) or
                    ULongLong_Value_Ptr (Left).Value;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when C_Long =>
            case Left.Const_Type.Kind is
               when C_Short =>
                  Result := new Long_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Long);
                  Long_Value_Ptr (Result).Value :=
                    Long_Value_Ptr (Right).Value or
                    Idl_Long (Short_Value_Ptr (Left).Value);
               when C_UShort =>
                  Result := new Long_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Long);
                  Long_Value_Ptr (Result).Value :=
                    Long_Value_Ptr (Right).Value or
                    Idl_Long (UShort_Value_Ptr (Left).Value);
               when C_Long =>
                  Result := new Long_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_Long);
                  Long_Value_Ptr (Result).Value :=
                    Long_Value_Ptr (Right).Value or
                    Long_Value_Ptr (Left).Value;
               when C_ULong =>
                  if ULong_Value_Ptr (Left).Value <=
                    Idl_ULong (Idl_Long'Last) then
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Long_Value_Ptr (Right).Value or
                       Idl_Long (ULong_Value_Ptr (Left).Value);
                  else
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (Long_Value_Ptr (Right).Value) or
                       Idl_LongLong (ULong_Value_Ptr (Left).Value);
                  end if;
               when C_LongLong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    Idl_LongLong (Long_Value_Ptr (Right).Value) or
                    LongLong_Value_Ptr (Left).Value;
               when C_ULongLong =>
                  if ULongLong_Value_Ptr (Left).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (Long_Value_Ptr (Right).Value) or
                       Idl_LongLong (ULongLong_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is negative and the other one is " &
                        "unsigned long long.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when C_ULong =>
            case Left.Const_Type.Kind is
               when C_UShort =>
                  Result := new ULong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULong);
                  ULong_Value_Ptr (Result).Value :=
                    ULong_Value_Ptr (Right).Value or
                    Idl_ULong (UShort_Value_Ptr (Left).Value);
               when C_Short =>
                  if ULong_Value_Ptr (Right).Value <=
                    Idl_ULong (Idl_Long'Last) then
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Idl_Long (ULong_Value_Ptr (Right).Value) or
                       Idl_Long (Short_Value_Ptr (Left).Value);
                  else
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (ULong_Value_Ptr (Right).Value) or
                       Idl_LongLong (Short_Value_Ptr (Left).Value);
                  end if;
               when C_ULong =>
                  Result := new ULong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULong);
                  ULong_Value_Ptr (Result).Value :=
                    ULong_Value_Ptr (Right).Value or
                    ULong_Value_Ptr (Left).Value;
               when C_Long =>
                  if ULong_Value_Ptr (Right).Value <=
                    Idl_ULong (Idl_Long'Last) then
                     Result := new Long_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_Long);
                     Long_Value_Ptr (Result).Value :=
                       Idl_Long (ULong_Value_Ptr (Right).Value) or
                       Long_Value_Ptr (Left).Value;
                  else
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (ULong_Value_Ptr (Right).Value) or
                       Idl_LongLong (Long_Value_Ptr (Left).Value);
                  end if;
               when C_ULongLong =>
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value :=
                    Idl_ULongLong (ULong_Value_Ptr (Right).Value) or
                    ULongLong_Value_Ptr (Left).Value;
               when C_LongLong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    Idl_LongLong (ULong_Value_Ptr (Right).Value) or
                    LongLong_Value_Ptr (Left).Value;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when C_LongLong =>
            case Left.Const_Type.Kind is
               when C_Short =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    LongLong_Value_Ptr (Right).Value or
                    Idl_LongLong (Short_Value_Ptr (Left).Value);
               when C_UShort =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    LongLong_Value_Ptr (Right).Value or
                    Idl_LongLong (UShort_Value_Ptr (Left).Value);
               when C_Long =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    LongLong_Value_Ptr (Right).Value or
                    Idl_LongLong (Long_Value_Ptr (Left).Value);
               when C_ULong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    LongLong_Value_Ptr (Right).Value or
                    Idl_LongLong (ULong_Value_Ptr (Left).Value);
               when C_LongLong =>
                  Result := new LongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                  LongLong_Value_Ptr (Result).Value :=
                    LongLong_Value_Ptr (Right).Value or
                    LongLong_Value_Ptr (Left).Value;
               when C_ULongLong =>
                  if ULongLong_Value_Ptr (Left).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       LongLong_Value_Ptr (Right).Value or
                       Idl_LongLong (ULongLong_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is negative and the other one is " &
                        "unsigned long long.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when C_ULongLong =>
            case Left.Const_Type.Kind is
               when C_UShort =>
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value :=
                    ULongLong_Value_Ptr (Right).Value or
                    Idl_ULongLong (UShort_Value_Ptr (Left).Value);
               when C_Short =>
                  if ULongLong_Value_Ptr (Right).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (ULongLong_Value_Ptr (Right).Value) or
                       Idl_LongLong (Short_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is unsigned long long and the other " &
                        "one is negative.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when C_ULong =>
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value :=
                    ULongLong_Value_Ptr (Right).Value or
                    Idl_ULongLong (ULong_Value_Ptr (Left).Value);
               when C_Long =>
                  if ULongLong_Value_Ptr (Right).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (ULongLong_Value_Ptr (Right).Value) or
                       Idl_LongLong (Long_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is unsigned long long and the other " &
                        "one is negative.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when C_ULongLong =>
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value :=
                    ULongLong_Value_Ptr (Right).Value or
                    ULongLong_Value_Ptr (Left).Value;
               when C_LongLong =>
                  if ULongLong_Value_Ptr (Right).Value <=
                    Idl_ULongLong (Idl_LongLong'Last) then
                     Result := new LongLong_Value;
                     Result.Const_Type := new Const_Type'(Kind => C_LongLong);
                     LongLong_Value_Ptr (Result).Value :=
                       Idl_LongLong (ULongLong_Value_Ptr (Right).Value) or
                       Idl_LongLong (LongLong_Value_Ptr (Left).Value);
                  else
                     Result := null;
                     Idl_Fe.Errors.Parser_Error
                       ("incompatible type between the two terms : the " &
                        "first one is unsigned long long and the other " &
                        "one is negative.",
                        Idl_Fe.Errors.Error,
                        Loc);
                     return;
                  end if;
               when others =>
                  raise Idl_Fe.Errors.Internal_Error;
            end case;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      --  if positive, then use unsigned types
      case Result.Const_Type.Kind is
         when C_Short =>
            if Short_Value_Ptr (Result).Value > 0 then
               declare
                  Value : Idl_Short;
               begin
                  Value := Short_Value_Ptr (Result).Value;
                  Free (Result.Const_Type);
                  Free (Short_Value_Ptr (Result));
                  Result := new UShort_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_UShort);
                  UShort_Value_Ptr (Result).Value := Idl_UShort (Value);
               end;
            end if;
         when C_Long =>
            if Long_Value_Ptr (Result).Value > 0 then
               declare
                  Value : Idl_Long;
               begin
                  Value := Long_Value_Ptr (Result).Value;
                  Free (Result.Const_Type);
                  Free (Long_Value_Ptr (Result));
                  Result := new ULong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULong);
                  ULong_Value_Ptr (Result).Value := Idl_ULong (Value);
               end;
            end if;
         when C_LongLong =>
            if LongLong_Value_Ptr (Result).Value > 0 then
               declare
                  Value : Idl_LongLong;
               begin
                  Value := LongLong_Value_Ptr (Result).Value;
                  Free (Result.Const_Type);
                  Free (LongLong_Value_Ptr (Result));
                  Result := new ULongLong_Value;
                  Result.Const_Type := new Const_Type'(Kind => C_ULongLong);
                  ULongLong_Value_Ptr (Result).Value := Idl_ULongLong (Value);
               end;
            end if;
         when C_ULong
           | C_UShort
           | C_ULongLong =>
            null;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
   end Eval_Or_Expr;

   ---------------------
   --  Eval_Xor_Expr  --
   ---------------------
   procedure Eval_Xor_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Xor_Expr;

   ---------------------
   --  Eval_And_Expr  --
   ---------------------
   procedure Eval_And_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_And_Expr;

   ---------------------
   --  Eval_Shr_Expr  --
   ---------------------
   procedure Eval_Shr_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Shr_Expr;

   ---------------------
   --  Eval_Shl_Expr  --
   ---------------------
   procedure Eval_Shl_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Shl_Expr;

   ---------------------
   --  Eval_Add_Expr  --
   ---------------------
   procedure Eval_Add_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Add_Expr;

   ---------------------
   --  Eval_Sub_Expr  --
   ---------------------
   procedure Eval_Sub_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Sub_Expr;

   ---------------------
   --  Eval_Mul_Expr  --
   ---------------------
   procedure Eval_Mul_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Mul_Expr;

   ---------------------
   --  Eval_Div_Expr  --
   ---------------------
   procedure Eval_Div_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Div_Expr;

   ---------------------
   --  Eval_Mod_Expr  --
   ---------------------
   procedure Eval_Mod_Expr (Left : in Value_Ptr;
                            Right : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Mod_Expr;

   ---------------------
   --  Eval_Neg_Expr  --
   ---------------------
   procedure Eval_Neg_Expr (Operand : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Neg_Expr;

   ---------------------
   --  Eval_Not_Expr  --
   ---------------------
   procedure Eval_Not_Expr (Operand : in Value_Ptr;
                            Result : out Value_Ptr;
                            Loc : in Idl_Fe.Errors.Location) is
   begin
      Result := null;
      return;
   end Eval_Not_Expr;

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_Short) return Idl_Short is
   begin
      return X;
   end "or";

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_Long) return Idl_Long is
   begin
      return X;
   end "or";

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_LongLong) return Idl_LongLong is
   begin
      return X;
   end "or";

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_UShort) return Idl_UShort is
   begin
      return X;
   end "or";

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_ULong) return Idl_ULong is
   begin
      return X;
   end "or";

   ----------
   --  or  --
   ----------
   function "or" (X, Y : Idl_ULongLong) return Idl_ULongLong is
   begin
      return X;
   end "or";

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
      Num : Natural := 0;
   begin
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Module
                 | T_Interface
                 | T_Union
                 | T_Struct
                 | T_Enum
                 | T_Typedef
                 | T_Custom
                 | T_Abstract
                 | T_ValueType
                 | T_Right_Cbracket =>
                  return;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop;
      if Get_Current_Scope /= Get_Root_Scope then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Definition;


   -----------------------------
   --  Go_To_Next_L_Cbracket  --
   -----------------------------
   procedure Go_To_Next_Left_Cbracket is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Left_Cbracket loop
         Next_Token;
      end loop;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Left_Cbracket;

   -----------------------------
   --  Go_To_Next_R_Cbracket  --
   -----------------------------
   procedure Go_To_Next_Right_Cbracket is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Right_Cbracket;

   -------------------------
   --  Go_To_Next_Export  --
   -------------------------
   procedure Go_To_Next_Export is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon
        and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      if Get_Token /= T_Right_Cbracket then
         Next_Token;
      end if;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Export;

   --------------------------------
   --  Go_To_Next_Value_Element  --
   --------------------------------
   procedure Go_To_Next_Value_Element is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon
        and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      if Get_Token /= T_Right_Cbracket then
         Next_Token;
      end if;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Value_Element;

   ---------------------------------
   --  Go_To_End_Of_State_Member  --
   ---------------------------------
   procedure Go_To_End_Of_State_Member is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon loop
         Next_Token;
      end loop;
      Next_Token;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_End_Of_State_Member;

   ------------------------------------
   --  Go_To_Next_Right_Parenthesis  --
   ------------------------------------
   procedure Go_To_Next_Right_Paren is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Right_Paren loop
         Next_Token;
      end loop;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Right_Paren;

   -------------------------
   --  Go_To_Next_Member  --
   -------------------------
   procedure Go_To_Next_Member is
   begin
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon
        and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      if Get_Token /= T_Right_Cbracket then
         Next_Token;
      end if;
      if Get_Current_Scope /= Get_Root_Scope and Get_Token = T_Eof then
         raise Errors.Internal_Error;
      end if;
   end Go_To_Next_Member;

   -------------------------
   --  Go_To_End_Of_Case  --
   -------------------------
   procedure Go_To_End_Of_Case is
   begin
      null;
   end Go_To_End_Of_Case;

   -------------------------------
   --  Go_To_End_Of_Case_Label  --
   -------------------------------
   procedure Go_To_End_Of_Case_Label is
   begin
      null;
   end Go_To_End_Of_Case_Label;



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
   --          Idl_Fe.Errors.Parser_Error
   --            ("`>>' is a shift operator, use `> >' for a double `>'",
   --             Idl_Fe.Errors.Error);
   --          Set_Replacement_Token (T_Greater);
   --       else
   --          Idl_Fe.Errors.Parser_Error ("unexpected token " & Image (Token)
   --                      & ", " & Image (Token_Kind) & " expected",
   --                                Idl_Fe.Errors.Error);
   --          raise Parse_Error;
   --       end if;
   --    end Expect;

   --    procedure Scan_Expect (Token_Kind : Idl_Token) is
   --    begin
   --       Next_Token;
   --       Expect (Token_Kind);
   --    end Scan_Expect;


end Idl_Fe.Parser;
