------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        I D L _ F E . P A R S E R                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;

with Utils; use Utils;
with Idl_Fe.Lexer; use Idl_Fe.Lexer;
with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree, Idl_Fe.Tree.Synthetic;
with Errors;
with Idl_Fe.Debug;
pragma Elaborate_All (Idl_Fe.Debug);

with Interfaces;

package body Idl_Fe.Parser is

   use Idl_Fe.Lexer.Lexer_State;
   --  Only the parser may access the current state
   --  of the lexer.

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Idl_Fe.Debug.Is_Active ("idl_fe.parser");
   procedure O is new Idl_Fe.Debug.Output (Flag);

   Flag2 : constant Natural
     := Idl_Fe.Debug.Is_Active ("idl_fe.parser_method_trace");
   procedure O2 is new Idl_Fe.Debug.Output (Flag2);

   ---------------------
   --  Initialization --
   ---------------------

   procedure Initialize
     (Filename : in String;
      Preprocess : in Boolean;
      Keep_Temporary_Files : in Boolean) is
   begin
      Idl_Fe.Lexer.Initialize
        (Filename, Preprocess, Keep_Temporary_Files);
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
   Buffer_Length : constant Natural := 6;

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
     := (others => (Dirname => null, Filename => null, Line => 0, Col => 0));
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

   --------------------------
   -- Get_Token_From_Lexer --
   --------------------------

   procedure Get_Token_From_Lexer is
   begin
      pragma Debug (O ("Get_Token_From_Lexer: enter"));
      Newest_Index := Newest_Index + 1;
      Token_Buffer (Newest_Index) := Idl_Fe.Lexer.Get_Next_Token;
      pragma Debug (O ("Get_Token_From_Lexer : location file is " &
                       Get_Lexer_Location.Filename.all));
      Location_Buffer (Newest_Index) := Get_Lexer_Location;
      if String_Buffer (Newest_Index) /= null then
         Free_String_Ptr (String_Buffer (Newest_Index));
      end if;
      case Token_Buffer (Newest_Index) is
         when T_Lit_Decimal_Integer |
           T_Lit_Octal_Integer |
           T_Lit_Hexa_Integer |
           T_Lit_Char |
           T_Lit_Wide_Char |
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
             new String'(Get_Lexer_String);
         when others =>
            String_Buffer (Newest_Index) := null;
      end case;
      pragma Debug (O ("Get_Token_From_Lexer: end"));
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


   ---------------------------
   --  View_Previous_Token  --
   ---------------------------

   function View_Previous_Token return Idl_Token is
   begin
      return Token_Buffer (Current_Index - 1);
   end View_Previous_Token;

   ------------------------------------
   --  View_Previous_Previous_Token  --
   ------------------------------------

   function View_Previous_Previous_Token return Idl_Token is
   begin
      return Token_Buffer (Current_Index - 2);
   end View_Previous_Previous_Token;

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

   function Get_Token_Location return Errors.Location is
   begin
      pragma Debug (O ("Get_Token_Location : enter & end"));
      return Location_Buffer (Current_Index);
   end Get_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------

   function Get_Previous_Token_Location return Errors.Location is
   begin
      pragma Debug (O ("Get_Previous_Token_Location : enter," &
                       " Current_Index - 1 = " &
                       Buffer_Index'Image (Current_Index - 1)));
      return Location_Buffer (Current_Index - 1);
   end Get_Previous_Token_Location;

   -----------------------------------
   --  Get_Previous_Token_Location  --
   -----------------------------------

   function Get_Previous_Previous_Token_Location
     return Errors.Location is
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

   ------------------------------------------
   --  Get_Previous_Previous_Token_String  --
   ------------------------------------------

   function Get_Previous_Previous_Token_String return String is
   begin
      return String_Buffer (Current_Index - 2).all;
   end Get_Previous_Previous_Token_String;

   -----------------------------
   --  Get_Next_Token_String  --
   -----------------------------

   function Get_Next_Token_String return String is
   begin
      return String_Buffer (Current_Index + 1).all;
   end Get_Next_Token_String;


   -------------------------------
   --  Divide_T_Greater_Greater --
   -------------------------------

   procedure Divide_T_Greater_Greater is
      Loc : Errors.Location := Get_Token_Location;
   begin
      if Get_Token /= T_Greater_Greater then
         return;
      end if;
      Token_Buffer (Current_Index) := T_Greater;
      if Newest_Index /= Current_Index then
         declare
            I : Buffer_Index := Newest_Index;
         begin
            if String_Buffer (Newest_Index + 1) /= null then
               Free_String_Ptr (String_Buffer (Newest_Index + 1));
            end if;
            while I /= Current_Index loop
               Token_Buffer (I + 1) := Token_Buffer (I);
               Location_Buffer (I + 1) := Location_Buffer (I);
               String_Buffer (I + 1) := String_Buffer (I);
               I := I - 1;
               pragma Debug (O ("Divide T_Greater_Greater : Token I+1 is " &
                                Idl_Token'Image (Token_Buffer (I + 1))));
               pragma Debug (O ("I = " & Buffer_Index'Image (I)));
            end loop;
         end;
      end if;
      Newest_Index := Newest_Index + 1;
      Loc.Col := Loc.Col + 1;
      Token_Buffer (Current_Index + 1) := T_Greater;
      Location_Buffer (Current_Index + 1) := Loc;
      String_Buffer (Current_Index + 1) := null;
   end Divide_T_Greater_Greater;


   ---------------------------------
   --  Management of expressions  --
   ---------------------------------

--    --  the actual list of already used values
--    Used_Values : Set_Ptr := null;

--    ----------------------
--    --  Add_Used_Value  --
--    ----------------------
--    function Add_Used_Value (C : Node_Id) return Boolean is
--       Old_Used : Set_Ptr := null;
--       Used : Set_Ptr := Used_Values;
--    begin
--       while Used /= null and then Used.Interval.Max < Value (C) loop
--          Old_Used := Used;
--          Used := Used.Next;
--       end loop;
--       if Used = null then
--          if Old_Used = null then
--             Used_Values := new Set;
--             Used_Values.Next := null;
--             Used_Values.Interval := (Min => Value (C), Max => Value (C));
--          else
--             if Used.Interval.Max = Value (C) - 1 then
--                if Used.Next /= null
--                  and then Value (C) = Used.Next.Interval.Min - 1 then
--                   --  merge the intervals
--                   declare
--                      Old_Used : Set_Ptr := Used.Next;
--                   begin
--                      Used.Interval.Max := Used.Next.Interval.Max;
--                      Used.Next := Used.Next.Next;
--                      Free (Old_Used);
--                   end;
--                else
--                   --  only change the upper bound of the interval
--                   Used.Interval.Max := Value (C);
--                end if;
--             else
--                Old_Used.Next := new Set;
--                Old_Used.Next.all.Next := null;
--                Old_Used.Next.all.Interval :=
--                   (Min => Value (C), Max => Value (C));
--             end if;
--          end if;
--       else
--          if Used.Interval.Min > Value (C) then
--             if Value (C) = Used.Interval.Min - 1 then
--                if Old_Used /= null
--                  and then Old_Used.Interval.Max = Value (C) - 1 then
--                   --  merge the intervals
--                   Old_Used.Interval.Max := Used.Interval.Max;
--                   Old_Used.Next := Used.Next;
--                   Free (Used);
--                else
--                   --  only change the lower bound of the interval
--                   Used.Interval.Min := Value (C);
--                end if;
--             else
--                Old_Used.Next := new Set;
--                Old_Used.Next.all.Next := Used;
--                Old_Used.Next.all.Interval
--                  := (Min => Value (C), Max => Value (C));
--             end if;
--          else
--             return False;
--          end if;
--       end if;
--       return True;
--    end Add_Used_Value;

--    --------------------------
--    --  Release_All_Values  --
--    --------------------------

--    procedure Release_All_Used_Values is
--       Old_Used_Values : Set_Ptr;
--    begin
--       pragma Debug (O ("Release_All_Used_Values: enter"));
--       while Used_Values /= null loop
--          Old_Used_Values := Used_Values;
--          Used_Values := Used_Values.Next;
--          Free (Old_Used_Values);
--       end loop;
--    end Release_All_Used_Values;

   --------------------------
   --  Parsing of the idl  --
   --------------------------

   ---------------------------
   --  Parse_Specification  --
   ---------------------------

   function Parse_Specification return Node_Id is
      Result : Node_Id;
   begin
      pragma Debug (O2 ("Parse_Specification: enter"));
      --  first call next_token in order to initialize the location
      Next_Token;
      Result := Make_Repository (Get_Token_Location);
      --  The repository is the root scope.
      Push_Scope (Result);
      declare
         Definition : Node_Id;
         Definition_Result : Boolean;
         Def_Nb : Natural := 0;
      begin
         while Get_Token /= T_Eof loop
            if Get_Token = T_Right_Cbracket then
               Errors.Error
                 ("Invalid '}', nothing to be closed.",
                  Errors.Error,
                  Get_Token_Location);
               Next_Token;
               if Get_Token = T_Semi_Colon then
                  Next_Token;
               end if;
            end if;
            Parse_Definition (Definition, Definition_Result);
            if not Definition_Result then
               --  we can be here for two reasons :
               --    either the definition parsing crashed and we'd like to go
               --  to the next one
               --    either the definition was right but it was an already
               --  existing module that was reopened. In this case,
               --  go_to_next_definition won't have any effect since we are
               --  on a definition
               Go_To_Next_Definition;
            elsif Definition /= No_Node then
               Def_Nb := Def_Nb + 1;
               Append_Node_To_Contents (Result, Definition);
            end if;
         end loop;
         if Def_Nb = 0 then
            Errors.Error
              ("Definition expected : a specification may not be empty.",
               Errors.Error,
               Get_Token_Location);
         end if;
      end;
      Pop_Scope;
      pragma Debug (O2 ("Parse_Specification: end"));
      return Result;
   end Parse_Specification;

   ------------------------
   --  Parse_Definition  --
   ------------------------
   procedure Parse_Definition
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Definition: enter"));
      case Get_Token is
         when T_Typedef
           | T_Struct
           | T_Union
           | T_Enum
           | T_Native =>
            Parse_Type_Dcl (Result, Success);
            if not Success then
               pragma Debug (O2 ("Parse_Definition: end"));
               return;
            end if;
         when T_Const =>
            Parse_Const_Dcl (Result, Success);
            if not Success then
               pragma Debug (O2 ("Parse_Definition: end"));
               return;
            end if;

         when T_Exception =>
            Parse_Except_Dcl (Result, Success);
            if not Success then
               pragma Debug (O2 ("Parse_Definition: end"));
               return;
            end if;

         when T_Abstract =>
            case View_Next_Token is
               when T_Interface =>
                  Parse_Interface (Result, Success);
                  if not Success then
                     pragma Debug (O2 ("Parse_Definition: end"));
                     return;
                  end if;

               when T_ValueType  =>
                  Parse_Value (Result, Success);
                  if not Success then
                     pragma Debug (O2 ("Parse_Definition: end"));
                     return;
                  end if;

               when others =>
                  declare
                     Loc : Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + 9;
                     Errors.Error
                       (Ada.Characters.Latin_1.Quotation &
                        "interface" &
                        Ada.Characters.Latin_1.Quotation &
                        " or " &
                        Ada.Characters.Latin_1.Quotation &
                        "valuetype" &
                        Ada.Characters.Latin_1.Quotation &
                        " expected after the abstract keyword.",
                        Errors.Error,
                        Get_Token_Location);
                     Success := False;
                     Result := No_Node;
                     --  consumes T_Abstract
                     Next_Token;
                     pragma Debug (O2 ("Parse_Definition: end"));
                     return;
                  end;
            end case;

         when T_Interface =>
            Parse_Interface (Result, Success);
            if not Success then
               pragma Debug (O2 ("Parse_Definition: end"));
               return;
            end if;

         when T_Module =>
            declare
               Reopen : Boolean;
            begin
               Parse_Module (Result, Success, Reopen);
               if not Success then
                  pragma Debug (O2 ("Parse_Definition: end"));
                  return;
               end if;
               --  if the module was reopened then we don't want its node to
               --  be added again to the definition list of the current scope.
               --  Thus, we put success to false, indicating that no node was
               --  generated
               if Reopen then
                  Success := False;
               end if;
            end;

         when
           T_ValueType |
           T_Custom    =>
            Parse_Value (Result, Success);
            if not Success then
               pragma Debug (O2 ("Parse_Definition: end"));
               return;
            end if;

         when T_Pragma =>
            Parse_Pragma (Result, Success);
            if not Success then
               --  here the pragma is ignored and no node created
               --  so we parse the next definition (if it exists)
               pragma Debug (O ("Parse_Definition : parse definition " &
                                "after pragma, current token is " &
                                Idl_Token'Image (Get_Token)));
               Parse_Definition (Result, Success);
            end  if;
            pragma Debug (O2 ("Parse_Definition: end"));
            return;

         when T_Eof
           | T_Right_Cbracket =>
            Result := No_Node;
            Success := False;
            pragma Debug (O2 ("Parse_Definition: end"));
            return;

         when others =>
            Errors.Error ("definition expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := No_Node;
            Success := False;
            pragma Debug (O2 ("Parse_Definition: end"));
            return;
      end case;
      if Get_Token /= T_Semi_Colon then
         Errors.Error
           ("';' expected at the end of a definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
      else
         Next_Token;
      end if;
      pragma Debug (O2 ("Parse_Definition: end"));
      return;
   end Parse_Definition;

   --------------------
   --  Parse_Module  --
   --------------------
   procedure Parse_Module
      (Result : out Node_Id;
       Success : out Boolean;
       Reopen  : out Boolean)
   is
   begin
      pragma Debug (O2 ("Parse_Module: enter"));
      Reopen := False;
      --  Is there an identifier ?
      Next_Token;
      case Get_Token is
         when  T_Identifier =>
            case View_Next_Token is
               when T_Left_Cbracket =>
                  declare
                     --  true if we have to create a module
                     Build_Module : Boolean := True;
                  begin
                     --  See if the identifier is not already used
                     if not Is_Redefinable
                       (Get_Token_String, Get_Lexer_Location)
                     then

                        --  There is a name collision with the module name

                        declare
                           Def : Identifier_Definition_Acc;
                        begin
                           Def := Find_Identifier_Definition
                             (Get_Token_String, Get_Lexer_Location);

                           if Kind (Def.Node) = K_Module
                             and then Def.Parent_Scope = Get_Current_Scope
                           then

                              --  If the previous definition was a module in
                              --  the same scope, then reopen it...

                              pragma Debug (O ("Parse_Module: reopening a " &
                                               "module"));
                              Reopen := True;
                              Result := Def.Node;
                              Build_Module := False;
                           else

                              --  ... else raise an error

                              declare
                                 Loc : Errors.Location;
                              begin
                                 Loc := Types.Get_Location
                                   (Find_Identifier_Node
                                    (Get_Token_String, Get_Lexer_Location));
                                 Errors.Error
                                   ("This module name is already defined in" &
                                    " this scope : " &
                                    Errors.Location_To_String (Loc),
                                    Errors.Error,
                                    Get_Token_Location);
                              end;
                           end if;
                        end;
                     end if;

                     if Build_Module then
                        declare
                           Ok : Boolean;
                        begin
                           --  Creation of the node
                           Result := Make_Module (Get_Previous_Token_Location);
                           --  here, the addentifier is really added only if
                           --  we're not in the case where the module name
                           --  was already defined
                           Ok := Add_Identifier
                             (Result,
                              Get_Token_String);
                           Set_Default_Repository_Id (Result);
                           Set_Initial_Current_Prefix (Result);
                        end;
                     end if;
                  end;
                  --  consume the T_Left_Cbracket token
                  Next_Token;
                  --  parse the module body
                  Next_Token;
                  declare
                     Definition : Node_Id;
                     Definition_Result : Boolean;
                  begin
                     pragma Debug (O ("Parse_Module: parse body"));
                     Push_Scope (Result);
                     pragma Debug (O ("Parse_Module: after push_scope, " &
                                      "current scope is : " &
                                      Name (Get_Current_Scope)));
                     if Get_Token = T_Right_Cbracket then
                        Errors.Error
                          ("definition expected : a module may not be empty.",
                           Errors.Error,
                           Get_Token_Location);
                     end if;
                     while Get_Token /= T_Right_Cbracket and
                       Get_Token /= T_Eof loop
                        --  try to parse a definition
                        Parse_Definition (Definition, Definition_Result);
                        if Definition_Result then
                           Append_Node_To_Contents (Result, Definition);
                        else
                           --  failed
                           Go_To_Next_Definition;
                        end if;
                     end loop;
                     Pop_Scope;
                     pragma Debug (O ("Parse_Module: after pop_scope, " &
                                      "current scope is : " &
                                      Name (Get_Current_Scope)));
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
                     Errors.Error ("'{' expected. ",
                                          Errors.Error,
                                          Loc);
                  end;
                  Result := No_Node;
                  Success := False;
            end case;
         when others =>
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 7;
               Errors.Error
                 ("Identifier expected in module.",
                  Errors.Error,
                  Loc);
            end;
            Result := No_Node;
            Success := False;
      end case;
      return;
      pragma Debug (O2 ("Parse_Module: end"));
   end Parse_Module;

   -----------------------
   --  Parse_Interface  --
   -----------------------

   procedure Parse_Interface
     (Result : out  Node_Id;
      Success : out Boolean) is
      Res : Node_Id;
      Fd_Res : Node_Id;
      Definition : Identifier_Definition_Acc;
   begin
      pragma Debug (O2 ("Parse_Interface: enter"));
      --  interface header.
      Res := Make_Interface (Get_Token_Location);
      --  is the interface abstracted
      if Get_Token = T_Abstract then
         Set_Abst (Res, True);
         --  the T_Interface token should "interface"
         --  (it is already checked)
         Next_Token;
      else
         Set_Abst (Res, False);
      end if;
      Set_Location (Res, Get_Token_Location);
      Set_Initial_Current_Prefix (Res);
      Next_Token;
      --  Expect an identifier
      if Get_Token = T_Identifier then
         Definition := Find_Identifier_Definition
           (Get_Token_String, Get_Lexer_Location);
         --  Is there a previous definition and in the same scope !
         if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
            --  is it a forward declaration
            if Definition.Parent_Scope = Get_Current_Scope and
              Kind (Definition.Node) = K_Forward_Interface then
               --  Check if they are both of the same abstract kind
               if Abst (Definition.Node) /= Abst (Res) then
                  declare
                     Loc : Errors.Location;
                  begin
                        Loc := Types.Get_Location
                          (Definition.Node);
                        Errors.Error
                          ("Forward declaration "
                           & Errors.Location_To_String (Loc)
                           & " has not the same abstract type",
                           Errors.Error,
                           Get_Previous_Token_Location);
                  end;
               end if;
               Fd_Res := Get_Node (Definition);
               if View_Next_Token /= T_Semi_Colon then
                  Set_Forward (Fd_Res, Res);
                  Set_Forward (Res, Fd_Res);
                  Redefine_Identifier (Definition, Res);
                  --  The forward declaration is now implemented.
                  Add_Int_Val_Definition (Fd_Res);
                  Set_Repository_Id (Res, Repository_Id (Fd_Res));
               end if;
            else
               declare
                  Loc : Errors.Location;
               begin
                  Loc := Types.Get_Location
                    (Find_Identifier_Node
                     (Get_Token_String, Get_Lexer_Location));
                  Errors.Error
                    ("This interface name is already declared in" &
                     " this scope : " &
                     Errors.Location_To_String (Loc),
                     Errors.Error,
                     Get_Token_Location);
                  Success := False;
                  Result := No_Node;
                  Fd_Res := No_Node;
                  return;
               end;
            end if;
         else
            pragma Debug (O ("Parse_Interface : identifier not defined"));
            Fd_Res := No_Node;
            Set_Forward (Res, No_Node);
            if not Add_Identifier (Res, Get_Token_String) then
               raise Errors.Internal_Error;
            end if;
            Set_Default_Repository_Id (Res);
            Definition := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         end if;
      else
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Errors.Error
              (" identifier expected after 'interface'",
               Errors.Error,
               Loc);
            Success := False;
            Result := No_Node;
            return;
         end;
      end if;
      pragma Debug (O ("Parse_Interface : identifier parsed"));
      Next_Token;
      --  Hups, this was just a forward declaration.
      if Get_Token = T_Semi_Colon then
         --  is it another forward declaration
         if Fd_Res /= No_Node then
            declare
               Loc : Errors.Location;
            begin
               Loc := Types.Get_Location (Fd_Res);
               Errors.Error
                 ("interface already forward declared in" &
                  " this scope : " &
                  Errors.Location_To_String (Loc),
                  Errors.Warning,
                  Get_Token_Location);
               --  This is only a warning: the OMG IDL grammar
               --  allows multiple forward declarations of an
               --  interface.

               Fd_Res := Make_Forward_Interface (Get_Location (Res));
               Set_Forward (Fd_Res, No_Node);
               Set_Abst (Fd_Res, Abst (Res));
               Set_Repository_Id (Fd_Res, Repository_Id (Res));

               --  FIXME : we must deallocate this node : Free (Res);
               Success := True;
               Result := Fd_Res;
               return;
            end;
         else
            Fd_Res := Make_Forward_Interface (Get_Location (Res));
            Set_Forward (Fd_Res, No_Node);
            Set_Abst (Fd_Res, Abst (Res));
            Redefine_Identifier (Definition, Fd_Res);
            Set_Repository_Id (Fd_Res, Repository_Id (Res));

            --  Add a forward declaration
            Add_Int_Val_Forward (Fd_Res);
            --  FIXME : we must deallocate this node : Free (Res);
            Result := Fd_Res;
            Success := True;
            return;
         end if;
      else
         --  use the Interface4 rule
         Parse_Interface_Dcl_End (Res, Success);
         if not Success then
            Result := No_Node;
         else
            Result := Res;
         end if;
         return;
      end if;
      return;
      pragma Debug (O2 ("Parse_Interface: end"));
   end Parse_Interface;

   --------------------
   --  Parse_Export  --
   --------------------
   procedure Parse_Export (Result : out Node_Id;
                           Success : out Boolean) is
   begin
      case Get_Token is
         when T_Readonly | T_Attribute =>
            declare
               Result_Attr : Node_Id;
            begin
               Parse_Attr_Dcl (Result_Attr, Success);
               Result := Result_Attr;
            end;
         when T_Oneway
           | T_Void
           | T_Colon_Colon
           | T_Identifier
           | T_Short
           | T_Long
           | T_Float
           | T_Double
           | T_Unsigned
           | T_Char
           | T_Wchar
           | T_Boolean
           | T_Octet
           | T_Any
           | T_Object
           | T_String
           | T_Wstring
           | T_ValueBase =>
            declare
               Result_Operation : Node_Id;
            begin
               Parse_Op_Dcl (Result_Operation, Success);
               Result := Result_Operation;
            end;
         when T_Exception =>
            declare
               Result_Except : Node_Id;
            begin
               Parse_Except_Dcl (Result_Except, Success);
               Result := Result_Except;
            end;
         when T_Const =>
            declare
               Result_Const : Node_Id;
            begin
               Parse_Const_Dcl (Result_Const, Success);
               Result := Result_Const;
            end;
         when T_Union
           | T_Struct
           | T_Enum
           | T_Native
           | T_Typedef =>
            Parse_Type_Dcl (Result, Success);
         when T_Pragma =>
            Parse_Pragma (Result, Success);
            if not Success then
               --  here the pragma is ignored and no node created
               --  so we parse the next export (if it exists)
               Parse_Export (Result, Success);
            end if;
            pragma Debug (O2 ("Parse_Export: end"));
            return;
         when T_Right_Cbracket =>
            --  here we just parsed a pragma but it was the last export of the
            --  interface or value. Thus, we return without creating a node but
            --  without an error message
            Success := False;
            Result := No_Node;
            return;
         when others =>
            Errors.Error
              ("declaration of a type, a constant, an exception, " &
               "an attribute or an operation expected",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := No_Node;
            return;
      end case;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Errors.Error ("';' expected",
                                     Errors.Error,
                                     Get_Token_Location);
         Go_To_End_Of_Export;
      else
         Next_Token;
      end if;
   end Parse_Export;

   -------------------------------
   --  Parse_Interface_Dcl_End  --
   -------------------------------
   procedure Parse_Interface_Dcl_End
     (Result : in out Node_Id;
      Success : out Boolean) is
      Body_Success : Boolean;
   begin
      pragma Debug (O2 ("Parse_Interface_Dcl_End: enter"));
      --  interface header.
      if Get_Token = T_Colon then
         --  inheritance_spec
         loop
            Next_Token;
            declare
               Scoped_Success : Boolean;
               Name : Node_Id;
            begin
               Parse_Interface_Name (Name, Scoped_Success);
               if not Scoped_Success then
                  Go_To_Next_Left_Cbracket;
                  exit;
               end if;
               if Name /= No_Node and then
                 Kind (Value (Name)) = K_Interface then
                  --  verify it was not already inherited
                  pragma Debug (O ("Parse_Interface_Dcl_End : verify " &
                                   "duplicated inheritance"));
                  if Is_In_Pointed_List (Parents (Result), Name) then
                     pragma Debug (O ("Parse_Interface_Dcl_End : duplicated " &
                                      "inheritance"));
                     Errors.Error ("An interface may not " &
                                                 "directly inherit more " &
                                                 "than once from another one.",
                                                 Errors.Error,
                                                 Get_Token_Location);
                  else
                     pragma Debug (O ("Parse_Interface_Dcl_End : non " &
                                      "duplicated inheritance"));
                     --  verify the abstraction of the inherited interface
                     if Abst (Result) and not Abst (Value (Name)) then
                        Errors.Error
                          ("An abstract interface may not inherit from " &
                           "a statefull one.",
                           Errors.Error,
                           Get_Token_Location);
                     end if;
                     --  verify that the imported interface does not
                     --  define an attribute or an operation already
                     --  defined in a previouly imported one.
                     if Interface_Is_Importable (Name, Result) then
                        Append_Node_To_Parents (Result, Name);
                     else
                        --  one of the attribute or operation of the
                        --  new interface to be imported was already
                        --  defined in the previously imported ones
                        Errors.Error
                          ("The attribute or operation definitions "&
                           " in this interface clashes with the definitions " &
                           "of the previouly imported ones.",
                           Errors.Error,
                           Get_Token_Location);
                     end if;
                  end if;
               end if;
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
            Errors.Error
              ("'{' expected",
               Errors.Error,
               Loc);
            Success := False;
            return;
         end;
      end if;
      --  Create a scope for the interface.
      Push_Scope (Result);
      declare
         List : Node_List;
      begin
         List := Contents (Result);
         Parse_Interface_Body (List, Body_Success);
         Set_Contents (Result, List);
      end;
      Pop_Scope;
      if not Body_Success then
         Result := No_Node;
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
      pragma Debug (O2 ("Parse_Interface_Dcl_End: end"));
   end Parse_Interface_Dcl_End;



   ----------------------------
   --  Parse_Interface_Body  --
   ----------------------------

   procedure Parse_Interface_Body
     (List : in out Node_List;
      Success : out Boolean)
   is
      Export_Success : Boolean;
      Result : Node_Id;
   begin
      Success := True;
      loop
         exit when Get_Token = T_Right_Cbracket or else Get_Token = T_Eof;
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

   ----------------------------
   --  Parse_Interface_Name  --
   ----------------------------

   procedure Parse_Interface_Name (Result : out Node_Id;
                                   Success : out Boolean) is
   begin
      Parse_Scoped_Name (Result, Success);
      --  the scoped name should denote an interface
      if Success and then
        Result /= No_Node then
         if Kind (Value (Result)) /= K_Interface then
            if Kind (Value (Result)) = K_Forward_Interface then
               Errors.Error
                 ("the inherited scoped name should denote a statefull " &
                  "interface, not a forwarded one.",
                  Errors.Error,
                  Get_Previous_Token_Location);
            else
               Errors.Error
                 ("the inherited scoped name should denote an interface",
                  Errors.Error,
                  Get_Previous_Token_Location);
            end if;
         end if;
      end if;
   end Parse_Interface_Name;

   -------------------------
   --  Parse_Scoped_Name  --
   -------------------------

   procedure Parse_Scoped_Name (Result : out Node_Id;
                                Success : out Boolean) is
      Res, Prev : Node_Id;
      Scope : Node_Id;
      A_Name : Node_Id := No_Node;
   begin
      pragma Debug (O2 ("Parse_Scoped_Name: enter"));

      Result := No_Node;
      Success := False;
      Prev := No_Node;
      --  creation of a scoped_name node
      Res := Make_Scoped_Name (Get_Token_Location);
      --  if it begins with :: then the scope of reference is
      --  the root scope
      if Get_Token = T_Colon_Colon then
         Scope := Get_Root_Scope;
         pragma Debug (O ("Parse_Scoped_Name: root scope is defined at " &
                          Errors.Location_To_String
                          (Get_Location (Scope))));
      else
         --  token should be an identifier
         if Get_Token /= T_Identifier then
            Errors.Error
              (" identifier or '::' expected at the " &
               "beginning of a scoped name",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := No_Node;
            pragma Debug (O2 ("Parse_Scoped_Name: end"));
            return;
         end if;

         --  gets the name of the scope of reference for this scoped_name
         --  if the current scope is one of the following :
         --    struct, union, operation, exception
         --  then we have to look at the parent scope level
         --  (see COBA v2.3 3.15.3 : Special Scoping Rules for Type Names)
         --  if it is a union, you have to take care of a potential
         --  enum type definition inside the switch statement. In this
         --  precise case, one of the label of the enum can be used inside
         --  the union.

         case Kind (Get_Current_Scope) is
            when K_Struct
              | K_Exception
              | K_Operation =>
               declare
                  The_Scope : Node_Id := Get_Current_Scope;
               begin
                  Pop_Scope;
                  A_Name := Find_Identifier_Node
                    (Get_Token_String, Get_Lexer_Location);
                  Push_Scope (The_Scope);
               end;
            when K_Union =>
               declare
                  The_Scope : Node_Id := Get_Current_Scope;
               begin
                  pragma Debug (O ("Parse_Scoped_Name : dealing with "
                                    & "a union scope"));
                  --  first try to find the name in the potential
                  --  enum declaration of the switch statement
                  --  for this purpose, we look in the current_scope
                  --  and check if the result is in the switch or not
                  --  if not, we just skip because of 3.15.3 (see above)
                  A_Name := Find_Identifier_Node
                    (Get_Token_String, Get_Lexer_Location);
                  if A_Name /= No_Node and then
                    Switch_Type (Get_Current_Scope) /= No_Node and then
                    Kind (Switch_Type (Get_Current_Scope)) = K_Enum and then
                    Is_In_List (Enumerators (Switch_Type (Get_Current_Scope)),
                                A_Name)
                  then
                     pragma Debug (O ("Parse_Scoped_Name : found something "
                                       & "in the current scope. That was "
                                       & "interesting"));
                     null;
                  else
                     pragma Debug (O ("Parse_Scoped_Name : found something "
                                       & "in the current scope but "
                                       & "not interesting"));
                     A_Name := No_Node;
                  end if;
                  if A_Name = No_Node then
                     pragma Debug (O ("Parse_Scoped_Name : looking in "
                                       & "the parent scope"));
                     --  else look at the parent scope
                     Pop_Scope;
                     A_Name := Find_Identifier_Node
                       (Get_Token_String, Get_Lexer_Location);
                     --  this reopens the union scope.
                     Push_Scope (The_Scope);
                  end if;
               end;
            when others =>
               A_Name := Find_Identifier_Node
                 (Get_Token_String, Get_Lexer_Location);
         end case;
         --  If it does not correspond to a previously defined scope
         if A_Name = No_Node then
            pragma Debug (O ("Parse_Scoped_Name : name is null"));
            Errors.Error
              ("Bad identifier in scoped name : " &
               "identifier `" & Get_Token_String & "' does not exist",
               Errors.Error,
               Get_Token_Location);
            Go_To_End_Of_Scoped_Name;
            Success := True;
            pragma Debug (O2 ("Parse_Scoped_Name: end"));
            return;
         end if;

         --  If we are not in its definition scope,
         --  we should perhaps import this identifier:
         --
         --  first we should look at the current scope.
         --  If it is a Struct, Union, Operation or Exception
         --  we should import the identifier into the parent scope
         --  of the current scope if necessary;
         --  else we should import it into the current scope.
         --
         --  If it is a module, an interface, a valuetype
         --  or the repository, the add function won't do
         --  anything.
         --  XXX Thomas 2000-08-25: I don't understand the above
         --      3 lines /at all/.

         declare
            CSK : constant Node_Kind := Kind (Get_Current_Scope);
         begin
            if CSK = K_Repository
              or else CSK = K_Module
              or else CSK = K_Interface
              or else CSK = K_ValueType
            then
               pragma Debug
                 (O ("Parse_Scope_Name: Current_Scope is a Gen_Scope."));
               if Get_Current_Scope
                   /= Definition (A_Name).Parent_Scope
                 or else Name (Get_Current_Scope)
                   /= Get_Token_String
               then
                  pragma Debug (O ("Parse_Scoped_Name: importing """
                                   & Get_Token_String
                                   & """ into Current_Scope"));
                  Add_Definition_To_Imported
                    (Definition (A_Name),
                     Get_Current_Scope);
               end if;
            else
               pragma Debug
                 (O ("Parse_Scoped_Name: Current_Scope is not a Gen_Scope."));
               if Get_Previous_Scope
                   /= Definition (A_Name).Parent_Scope
                 or else Name (Get_Previous_Scope)
                 /= Get_Token_String
               then
                  pragma Debug (O ("Parse_Scoped_Name: importing """
                                   & Get_Token_String
                                   & """ into Previous_Scope"));
                  Add_Definition_To_Imported
                    (Definition (A_Name),
                     Get_Previous_Scope);
               end if;
            end if;
         end;
         --  here we deal with the case of an identifier
         --  with '::' after it : it must denote a scope
         if View_Next_Token = T_Colon_Colon then
            --  Is the identifier a scope?
            if not Is_Scope (A_Name) then
               Errors.Error
                 ("Bad identifier in scoped name : " &
                  "identifier `" & Name (A_Name) &
                  "' does not denote a scope",
                  Errors.Error,
                  Get_Token_Location);
               Go_To_End_Of_Scoped_Name;
               Success := True;
               pragma Debug (O2 ("Parse_Scoped_Name: end"));
               return;
            else
               Scope := A_Name;
            end if;
            --  to eat the identifier representing the current scope
            Next_Token;
         end if;
      end if;
      pragma Debug (O ("Parse_Scoped_Name : beginning of loop"));
      --  Loop through the scopes to get the right definition
      declare
         Def : Identifier_Definition_Acc;
      begin
         while Get_Token = T_Colon_Colon loop
            --  consumes the '::'
            Next_Token;
            --  we should have an identifier here
            if Get_Token /= T_Identifier then
               Errors.Error
                 (" identifier expected in the scoped name",
                  Errors.Error,
                  Get_Token_Location);
               Success := False;
               Result := No_Node;
               pragma Debug (O2 ("Parse_Scoped_Name: end"));
               return;
            end if;
            --  Find the identifier in the reference scope
            Def := Find_Identifier_In_Storage
              (Scope, Get_Token_String);
            --  if it does not exist
            if Def = null then
               Errors.Error
                 ("Bad identifier `" & Get_Token_String &
                  "' in scoped name : this identifier does not exist " &
                  "in the given scope",
                  Errors.Error,
                  Get_Token_Location);
               Go_To_End_Of_Scoped_Name;
               Success := True;
               pragma Debug (O2 ("Parse_Scoped_Name: end"));
               return;
            end if;
            A_Name := Def.Node;
            --  if it is not the end of the scoped name, the
            --  current identifier should denote a node
            if View_Next_Token = T_Colon_Colon then
               if not Is_Scope (A_Name) then
                  Errors.Error
                    ("Bad identifier `" & Name (A_Name) &
                     "' in scoped name : this identifier does not denote " &
                     "a scope",
                     Errors.Error,
                     Get_Token_Location);
                  Go_To_End_Of_Scoped_Name;
                  Success := True;
                  pragma Debug (O2 ("Parse_Scoped_Name: end"));
                  return;
               else
                  Scope := A_Name;
               end if;
               --  consumes the identifier
               Next_Token;
            end if;
         end loop;
         pragma Debug (O ("Parse_Scoped_Name : end of loop"));
         Set_Value (Res, A_Name);

         --  Here we try to avoid recursivity in structs and unions
         if (Kind (Get_Current_Scope) = K_Struct
             or Kind (Get_Current_Scope) = K_Union)
           and Get_Current_Scope = A_Name then
            --  recursivity is allowed through sequences or Pragma
            if View_Previous_Previous_Token /= T_Sequence and
              View_Previous_Previous_Token /= T_Pragma then
               Errors.Error
                 ("Recursive definitions not allowed",
                  Errors.Error,
                  Get_Token_Location);
               Success := False;
               Result := No_Node;
               pragma Debug (O2 ("Parse_Scoped_Name: end"));
               return;
            end if;
         end if;
      end;
      --  consumes the last identifier
      Next_Token;
      Success := True;
      Result := Res;
      pragma Debug (O ("Parse_Scoped_Name : " &
                       "end if simple identifier"));
      pragma Debug (O2 ("Parse_Scoped_Name: end"));
      return;
   end Parse_Scoped_Name;

   -------------------
   --  Parse_Value  --
   -------------------
   procedure Parse_Value (Result : out Node_Id;
                          Success : out Boolean) is
   begin
      pragma Debug (O2 ("Initialize_Local_Object: enter"));
      case Get_Token is
         when T_Custom =>
            Next_Token;
            declare
               Res : Node_Id;
            begin
               Parse_Custom_Value (Res, Success);
               Result := Res;
            end;
         when T_Abstract =>
            Next_Token;
            Parse_Abstract_Value (Result, Success);
         when T_ValueType =>
            Parse_Direct_Value (Result, Success);
         when others =>
            raise Errors.Internal_Error;
      end case;
      pragma Debug (O2 ("Initialize_Local_Object: end"));
      return;
   end Parse_Value;

   --------------------------
   --  Parse_Custom_Value  --
   --------------------------
   procedure Parse_Custom_Value (Result : out Node_Id;
                                 Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Custom_Value: enter"));
      if Get_Token /= T_ValueType then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Errors.Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " expected after custom keyword.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := No_Node;
         Success := False;
      else
         Next_Token;
         if Get_Token /= T_Identifier then
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Errors.Error ("identifier expected.",
                                    Errors.Error,
                                    Loc);
            end;
            Result := No_Node;
            Success := False;
         else
            Parse_End_Value_Dcl (Result, Success, True, False);
         end if;
      end if;
      pragma Debug (O2 ("Parse_Custom_Value: enter"));
      return;
   end Parse_Custom_Value;

   ----------------------------
   --  Parse_Abstract_Value  --
   ----------------------------
   procedure Parse_Abstract_Value (Result : out Node_Id;
                                   Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Abstract_Value: enter"));
      if Get_Token /= T_ValueType then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 9;
            Errors.Error (Ada.Characters.Latin_1.Quotation &
                                 "valuetype" &
                                 Ada.Characters.Latin_1.Quotation &
                                 "expected after abstract keyword.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := No_Node;
         Success := False;
      else
         Next_Token;
         pragma Debug (O ("Parse_Abstract_Value : check for identifier"));
         if Get_Token /= T_Identifier then
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 10;
               Errors.Error ("identifier expected.",
                                    Errors.Error,
                                    Loc);
            end;
            Result := No_Node;
            Success := False;
         else
            case View_Next_Token is
               when T_Colon
                 | T_Supports
                 | T_Left_Cbracket =>
                  declare
                     Res : Node_Id;
                  begin
                     Parse_End_Value_Dcl (Res, Success, False, True);
                     Result := Res;
                  end;
               when T_Semi_Colon =>
                  declare
                     Res : Node_Id;
                  begin
                     Parse_End_Value_Forward_Dcl (Res, Success, True);
                     Result := Res;
                  end;
               when others =>
                  declare
                     Loc : Errors.Location;
                  begin
                     Loc := Get_Token_Location;
                     Loc.Col := Loc.Col + Get_Token_String'Length;
                     Errors.Error ("Bad value definition. " &
                                          "Inheritance specification, '{'" &
                                          " or ';' expected.",
                                          Errors.Error,
                                          Loc);
                  end;
                  Result := No_Node;
                  Success := False;
            end case;
         end if;
      end if;
      pragma Debug (O2 ("Parse_Abstract_Value: end"));
      return;
   end Parse_Abstract_Value;

   --------------------------
   --  Parse_Direct_Value  --
   --------------------------
   procedure Parse_Direct_Value (Result : out Node_Id;
                                 Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Direct_Value: enter"));
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 10;
            Errors.Error ("identifier expected.",
                                 Errors.Error,
                                 Loc);
         end;
         Result := No_Node;
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
                  Res : Node_Id;
               begin
                  Parse_End_Value_Box_Dcl (Res, Success);
                  Result := Res;
               end;
            when T_Semi_Colon =>
               declare
                  Res : Node_Id;
               begin
                  Parse_End_Value_Forward_Dcl (Res, Success, False);
                  Result := Res;
               end;
            when T_Colon
              | T_Supports
              | T_Left_Cbracket =>
               declare
                  Res : Node_Id;
               begin
                  Parse_End_Value_Dcl (Res, Success, False, False);
                  Result := Res;
               end;
            when others =>
               declare
                  Loc : Errors.Location;
               begin
                  Loc := Get_Token_Location;
                  Loc.Col := Loc.Col + Get_Token_String'Length;
                  Errors.Error ("Bad value definition. " &
                                       "Type, inheritance specification, " &
                                       "'{' or ';' expected.",
                                       Errors.Error,
                                       Loc);
               end;
               Result := No_Node;
               Success := False;
         end case;
      end if;
      pragma Debug (O2 ("Parse_Direct_Value: end"));
      return;
   end Parse_Direct_Value;

   ---------------------------
   --  Parse_End_Value_Dcl  --
   ---------------------------
   procedure Parse_End_Value_Dcl (Result : out Node_Id;
                                  Success : out Boolean;
                                  Custom : in Boolean;
                                  Abst : in Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      pragma Debug (O2 ("Parse_End_Value_Dcl: enter"));
      Result := Make_ValueType (Get_Previous_Token_Location);
      Set_Abst (Result, Abst);
      Set_Custom (Result, Custom);
      if (Abst or else Custom) then
         Set_Location (Result, Get_Previous_Previous_Token_Location);
      else
         Set_Location (Result, Get_Previous_Token_Location);
      end if;
      Set_Initial_Current_Prefix (Result);

      --  Try to find a previous definition

      Definition := Find_Identifier_Definition
        (Get_Token_String, Get_Lexer_Location);

      --  Is there a previous definition and in the same scope?

      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then

         --  Is it a forward declaration?

         if  Definition.Parent_Scope = Get_Current_Scope
           and then Kind (Definition.Node) = K_Forward_ValueType
         then
            declare
               Fd_Decl : Node_Id;
            begin
               Fd_Decl := Get_Node (Definition);
               Add_Int_Val_Definition (Fd_Decl);
               Set_Forward (Fd_Decl, Result);
               Set_Forward (Result, Fd_Decl);
               Set_Repository_Id (Result, Repository_Id (Fd_Decl));
               Redefine_Identifier (Definition, Result);
            end;

         else
            Errors.Error
            ("The identifier used for this valuetype is already "
             & "defined in the same scope : "
             & Errors.Location_To_String
                 (Get_Location (Definition.Node)),
                  Errors.Error,
                  Get_Token_Location);
            Set_Forward (Result, No_Node);
         end if;
      else

         --  No previous definition

         Set_Forward (Result, No_Node);
         if not Add_Identifier (Result, Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
         Set_Default_Repository_Id (Result);
      end if;

      Next_Token;
      if Get_Token = T_Colon
        or else Get_Token = T_Supports then

         --  An inheritance specification is present

         declare
            Inherit_Success : Boolean;
         begin
            Parse_Value_Inheritance_Spec (Result, Inherit_Success);
            if not Inherit_Success then
               Go_To_Next_Left_Cbracket;
               if Get_Token = T_Eof then
                  Success := False;
                  return;
               end if;
            end if;
         end;
      end if;

      if Get_Token /= T_Left_Cbracket then

         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;

            --  At this point, this is only possible after an
            --  inheritance specification.
            --  The previous token is therefore an identifier.

            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("Bad value definition: '{' expected.",
               Errors.Error, Loc);
         end;
         Success := False;
         return;
      end if;

      Next_Token;
      Push_Scope (Result);
      while Get_Token /= T_Right_Cbracket loop
         declare
            Element_Success : Boolean;
            Element : Node_Id;
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
               if Get_Token = T_Eof then
                  Success := False;
                  return;
               end if;
            else
               Append_Node_To_Contents (Result, Element);
            end if;
         end;
      end loop;
      Pop_Scope;

      --  Consume the right Cbracket

      Next_Token;
      Success := True;
      pragma Debug (O2 ("Parse_End_Value_Dcl: end"));
      return;
   end Parse_End_Value_Dcl;

   -----------------------------------
   --  Parse_End_Value_Forward_Dcl  --
   -----------------------------------
   procedure Parse_End_Value_Forward_Dcl (Result : out Node_Id;
                                          Success : out Boolean;
                                          Abst : in Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := Make_Forward_ValueType (Get_Previous_Token_Location);
      Set_Abst (Result, Abst);
      if Abst then
         Set_Location (Result, Get_Previous_Previous_Token_Location);
      else
         Set_Location (Result, Get_Previous_Token_Location);
      end if;
      --  try to find a previous definition
      Definition := Find_Identifier_Definition
        (Get_Token_String, Get_Lexer_Location);
      --  Is there a previous definition and in the same scope ?
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         --  is it a forward
         if Definition.Parent_Scope = Get_Current_Scope and
           Kind (Definition.Node) = K_Forward_ValueType then
            --  nothing to do : this new forward declaration is useless
            Errors.Error
              ("This valuetype was already declared forward : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Warning,
               Get_Token_Location);
         else
            Errors.Error
              ("The identifier used for this valuetype is already "
               & "defined in the same scope : " &
             Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end if;
      else
         --  no previous forward
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
         Set_Default_Repository_Id (Result);
         Add_Int_Val_Forward (Result);
      end if;
      --  consumes the identifier
      Next_Token;
      Success := True;
      return;
   end Parse_End_Value_Forward_Dcl;

   -------------------------------
   --  Parse_End_Value_Box_Dcl  --
   -------------------------------

   procedure Parse_End_Value_Box_Dcl (Result : out Node_Id;
                                      Success : out Boolean) is
      Definition : Identifier_Definition_Acc;
   begin
      Result := Make_Boxed_ValueType (Get_Previous_Token_Location);
      --  try to find a previous definition
      Definition := Find_Identifier_Definition
        (Get_Token_String, Get_Lexer_Location);
      --  Is there a previous definition and in the same scope ?
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         --  is it a forward
         if Definition.Parent_Scope = Get_Current_Scope and
           Kind (Definition.Node) = K_Forward_ValueType then
            --  nothing to do : this new forward declaration is useless
            Errors.Error
              ("This valuetype was forward declared : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)) &
               ". It can not be a boxed one.",
               Errors.Error,
               Get_Previous_Token_Location);
            --  To avoid a second error, due to the non declaration
            --  of the forward value
            Add_Int_Val_Definition (Definition.Node);
         else
            Errors.Error
              ("The identifier used for this valuetype is already "
               & "defined in the same scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end if;
         Next_Token;
         declare
            Node : Node_Id;
         begin
            Node := Boxed_Type (Result);
            Parse_Type_Spec (Node,
                             Success);
            Set_Boxed_Type (Result, Node);
         end;
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
            declare
               Node : Node_Id;
            begin
               Node := Boxed_Type (Result);
               Parse_Type_Spec (Node,
                                Success);
               Set_Boxed_Type (Result, Node);
            end;
            if not Add_Identifier (Result,
                                   Name.all) then
               raise Errors.Internal_Error;
            end if;
            Set_Default_Repository_Id (Result);

            Free_String_Ptr (Name);
         end;
      end if;
      return;
   end Parse_End_Value_Box_Dcl;

   ------------------------------------
   --  Parse_Value_Inheritance_Spec  --
   ------------------------------------
   procedure Parse_Value_Inheritance_Spec (Result : in out Node_Id;
                                           Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Value_Inheritance_Spec: enter"));
      Success := True;
      if Get_Token = T_Colon then
         Next_Token;
         if Get_Token = T_Truncatable then
            if Abst (Result) then
               Errors.Error
                 ("The truncatable modifier may not " &
                  "be used in an abstract value.",
                  Errors.Error,
                  Get_Token_Location);
            elsif Custom (Result) then
               Errors.Error
                 ("The truncatable modifier may not " &
                  "be used in a custom value.",
                  Errors.Error,
                  Get_Token_Location);
            else
               Set_Truncatable (Result, True);
            end if;
            Next_Token;
         end if;
         pragma Debug (O ("Parse_Value_Inheritance_Spec : truncable treated"));
         --  parse value inheritance
         declare
            Name : Node_Id;
            Name_Success : Boolean;
         begin
            Parse_Value_Name (Name, Name_Success);
            if Name_Success and then
              Name /= No_Node then
               case Kind (Value (Name)) is
                  when K_ValueType =>
                     if Abst (Result) then
                        if not Abst (Value (Name)) then
                           Errors.Error
                             ("An abstract value may not inherit from a " &
                              "stateful one.",
                              Errors.Error,
                              Get_Token_Location);
                        end if;
                     else
                        if Abst (Value (Name)) and then
                          Truncatable (Result) then
                           Errors.Error
                             ("The truncatable modifier may not be used " &
                              "for an abstract value inheritance.",
                              Errors.Error,
                              Get_Token_Location);
                        end if;
                     end if;
                     Append_Node_To_Parents (Result, Name);

                  when K_Forward_ValueType =>
                     Errors.Error
                       ("A value may not inherit from a forward declared" &
                        " value whose definition has not yet been seen.",
                        Errors.Error,
                        Get_Token_Location);
                  when K_Boxed_ValueType =>
                     Errors.Error
                       ("A value may not inherit from a boxed value.",
                        Errors.Error,
                        Get_Token_Location);
                  when K_Interface
                    | K_Forward_Interface =>
                     Errors.Error
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
                        Errors.Error
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
         pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                          "first parent parsed"));
         while Get_Token = T_Comma loop
            Next_Token;
            declare
               Name : Node_Id;
               Name_Success : Boolean;
            begin
               Parse_Value_Name (Name, Name_Success);
               if Name_Success and then
                 Name /= No_Node then
                  case Kind (Value (Name)) is
                     when K_ValueType =>
                        pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                                         "parent is a valuetype"));
                        if Is_In_Pointed_List (Parents (Result), Name) then
                           --  already inherited
                           Errors.Error
                             ("A value may not directly inherit more than " &
                              "once from another one.",
                              Errors.Error,
                              Get_Token_Location);
                        else
                           if not Abst (Value (Name)) then
                              Errors.Error
                                ("A stateful value may only derive from a " &
                                 "single stateful value and this one must " &
                                 "be the first element in the inheritance.",
                                 Errors.Error,
                                 Get_Token_Location);
                           end if;
                           Append_Node_To_Parents (Result, Name);
                        end if;
                     when K_Forward_ValueType =>
                        Errors.Error
                          ("A value may not inherit from a forward declared" &
                           " value whose definition has not yet been seen.",
                           Errors.Error,
                           Get_Token_Location);
                     when K_Boxed_ValueType =>
                        Errors.Error
                          ("A value may not inherit from a boxed value.",
                           Errors.Error,
                           Get_Token_Location);
                     when K_Interface
                        | K_Forward_Interface =>
                        Errors.Error
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
                           Errors.Error
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
      pragma Debug (O ("Parse_Value_Inheritance_Spec : " &
                       "beginning parsing of supports declarations"));
      --  Since we entered this method after reading T_colon
      --  or T_Supports, we should have T_Supports now.
      case Get_Token is
         when T_Supports =>
            Next_Token;
            declare
               Non_Abstract_Interface : Boolean := False;
            begin
               --  parse interface inheritance
               declare
                  Name : Node_Id;
                  Name_Success : Boolean;
               begin
                  Parse_Interface_Name (Name, Name_Success);
                  if Name_Success then
                     case Kind (Value (Name)) is
                        when K_Interface =>
                           if not Abst (Value (Name)) then
                              Non_Abstract_Interface := True;
                           end if;
                           Append_Node_To_Supports (Result, Name);
                        when K_Forward_Interface =>
                           Errors.Error
                             ("A value may not support a forward declared" &
                              " interface whose declaration has not yet " &
                              "been seen.",
                              Errors.Error,
                              Get_Token_Location);
                        when K_Boxed_ValueType
                          | K_ValueType
                          | K_Forward_ValueType =>
                           Errors.Error
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
                              Errors.Error
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
                     Name : Node_Id;
                     Name_Success : Boolean;
                  begin
                     Parse_Interface_Name (Name, Name_Success);
                     if Name_Success then
                        case Kind (Value (Name)) is
                           when K_Interface =>
                              if Is_In_Pointed_List (Supports (Result),
                                                     Name) then
                                 --  already inherited
                                 Errors.Error
                                   ("A value may not directly support " &
                                    "a given interface more than once.",
                                    Errors.Error,
                                    Get_Token_Location);
                              else
                                 if not Abst (Result)
                                   and then not Abst (Value (Name)) then
                                    if Non_Abstract_Interface then
                                       Errors.Error
                                         ("A stateful value may support " &
                                          "only " &
                                          "one non abstract interface. This " &
                                          "is the second one.",
                                          Errors.Error,
                                          Get_Token_Location);
                                    else
                                       Non_Abstract_Interface := True;
                                    end if;
                                 end if;
                                 Append_Node_To_Supports (Result, Name);
                              end if;
                           when K_Forward_Interface =>
                              Errors.Error
                                ("A value may not support a forward declared" &
                                 " interface whose declaration has not yet " &
                                 "been seen.",
                                 Errors.Error,
                                 Get_Token_Location);
                           when K_Boxed_ValueType
                             | K_ValueType
                             | K_Forward_ValueType =>
                              Errors.Error
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
                                 Errors.Error
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
         when others =>
            declare
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
               Errors.Error
                 ("',', " &
                  Ada.Characters.Latin_1.Quotation &
                  "supports" &
                  Ada.Characters.Latin_1.Quotation &
                  " or '{' expected.",
                  Errors.Error,
                  Loc);
               Success := False;
            end;
      end case;
      pragma Debug (O2 ("Parse_Value_Inheritance_Spec: end"));
   end Parse_Value_Inheritance_Spec;

   ------------------------
   --  Parse_Value_Name  --
   ------------------------

   procedure Parse_Value_Name (Result : out Node_Id;
                               Success : out Boolean) is
   begin
      Parse_Scoped_Name (Result, Success);
      --  the checking of the type is done inside the method
      --  parse_value_inheritance_spec, which is the only user of
      --  this procedure
   end Parse_Value_Name;

   ---------------------------
   --  Parse_Value_Element  --
   ---------------------------
   procedure Parse_Value_Element  (Result : out Node_Id;
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
            Parse_State_Member (Result, Success);
         when T_Factory =>
            Parse_Init_Dcl (Result, Success);
         when T_Pragma =>
            Parse_Pragma (Result, Success);
            if not Success then
               --  here the pragma is ignored and no node created
               --  so we parse the next export (if it exists)
               Parse_Value_Element (Result, Success);
            end if;
            return;
         when T_Right_Cbracket =>
            --  here we just parsed a pragma but it was the last element of the
            --  value. Thus, we return without creating a node but
            --  without an error message
            Success := False;
            Result := No_Node;
            return;
         when others =>
            Errors.Error ("value_element expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := No_Node;
            Success := False;
            return;
      end case;
   end Parse_Value_Element;

   --------------------------
   --  Parse_State_Member  --
   --------------------------
   procedure Parse_State_Member (Result : out Node_Id;
                                 Success : out Boolean) is
   begin
      Result := Make_State_Member (Get_Token_Location);
      case Get_Token is
         when T_Public =>
            Set_Is_Public (Result, True);
         when T_Private =>
            Set_Is_Public (Result, False);
         when others =>
            raise Errors.Internal_Error;
      end case;
      Next_Token;
      declare
         Node : Node_Id;
      begin
         Node := State_Type (Result);
         Parse_Type_Spec (Node,
                          Success);
         Set_State_Type (Result, Node);
      end;
      if not Success then
         Go_To_End_Of_State_Member;
         return;
      end if;
      declare
         Node : Node_List;
      begin
         Node := State_Declarators (Result);
         Parse_Declarators (Node,
                            Result,
                            Success);
         Set_State_Declarators (Result, Node);
      end;
      if not Success then
         Go_To_End_Of_State_Member;
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Errors.Error ("missing ';' at the end of the state " &
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

   procedure Parse_Init_Dcl (Result : out Node_Id;
                             Success : out Boolean) is
   begin
      if View_Next_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;
            Loc.Col := Loc.Col + 8;
            Errors.Error ("Identifier expected after keyword " &
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
      Result := Make_Initializer (Get_Token_Location);
      --  consume T_Factory
      Next_Token;
      --  Is there a previous definition
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         declare
            Definition : Identifier_Definition_Acc :=
              Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("The identifier used for this initializer is already "
               & "defined in the same scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end;
      else
         --  no previous definition
         if not Add_Identifier (Result,
                                Get_Token_String) then
            raise Errors.Internal_Error;
         end if;
         Set_Default_Repository_Id (Result);

      end if;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("missing '(' in initializer declaration.",
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
            declare
               Node : Node_List;
            begin
               Node := Param_Decls (Result);
               Parse_Init_Param_Decls (Node,
                                       Decls_Success);
               Set_Param_Decls (Result, Node);
            end;
            Pop_Scope;
            if not Decls_Success then
               Go_To_Next_Right_Paren;
               if Get_Token = T_Eof then
                  Success := False;
                  return;
               end if;
            else
               if Get_Token /= T_Right_Paren then
                  Errors.Error ("missing ')' at the end of " &
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
         Errors.Error ("missing ';' at the end of initializer " &
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
         Decl : Node_Id;
         Decl_Success : Boolean;
      begin
         Parse_Init_Param_Decl (Decl, Decl_Success);
         if Decl_Success then
            Append_Node (Result, Decl);
         else
            Success := False;
            return;
         end if;
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Decl : Node_Id;
            Decl_Success : Boolean;
         begin
            Parse_Init_Param_Decl (Decl, Decl_Success);
            if Decl_Success then
               Append_Node (Result, Decl);
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
   procedure Parse_Init_Param_Decl (Result : out Node_Id;
                                    Success : out Boolean) is
   begin
      case Get_Token is
         when T_In =>
            Next_Token;
         when T_Out
           | T_Inout =>
            Errors.Error
              ("an initializer parameter can only be " &
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
            Errors.Error
              ("an initializer parameter should begin " &
               "with keyword " &
               Ada.Characters.Latin_1.Quotation &
               "in" &
               Ada.Characters.Latin_1.Quotation &
               ".",
               Errors.Error,
               Get_Token_Location);
         when others =>
            Errors.Error
              ("bad initializer parameter declaration.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := No_Node;
            return;
      end case;
      Result := Make_Param (Get_Previous_Token_Location);
      Set_Mode (Result, Mode_In);
      declare
         Type_Node : Node_Id;
      begin
         Type_Node := Param_Type (Result);
         Parse_Param_Type_Spec
           (Type_Node, Success);
         Set_Param_Type (Result, Type_Node);
         if not Success then
            return;
         end if;
         declare
            Node : Node_Id;
         begin
            Node := Declarator (Result);
            Parse_Simple_Declarator (Node,
                                     Result,
                                     Success);
            Set_Declarator (Result, Node);
         end;
      end;
      return;
   end Parse_Init_Param_Decl;

   -----------------------
   --  Parse_Const_Dcl  --
   -----------------------

   procedure Parse_Const_Dcl (Result : out Node_Id;
                              Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Const_Dcl: enter"));
      Next_Token;
      Result := Make_Const_Dcl (Get_Previous_Token_Location);
      declare
         Node : Node_Id;
      begin
         Parse_Const_Type (Node,
                           Success);
         Set_Constant_Type (Result, Node);
      end;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Identifier then
         Errors.Error
           ("Identifier expected in constant declaration.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         --  Is there a previous definition
         if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
            declare
               Definition : Identifier_Definition_Acc
                 := Find_Identifier_Definition
                 (Get_Token_String, Get_Lexer_Location);
            begin
               Errors.Error
                 ("This identifier is already defined in this scope : " &
                  Errors.Location_To_String
                  (Get_Location (Definition.Node)),
                  Errors.Error,
                  Get_Token_Location);
            end;
         else
            --  no previous definition
            if not Add_Identifier (Result, Get_Token_String) then
               raise Errors.Internal_Error;
            end if;
            Set_Default_Repository_Id (Result);

         end if;
      end if;
      Next_Token;
      if Get_Token /= T_Equal then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error ("'=' expected in const declaration.",
                                 Errors.Error,
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
              | T_Lit_Char
              | T_Lit_Wide_Char
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
      declare
         Node : Node_Id;
      begin
         Node := Expression (Result);
         Parse_Const_Exp (Node,
                          Constant_Type (Result),
                          Success);
         Set_Expression (Result, Node);
      end;
      return;
      pragma Debug (O2 ("Parse_Const_Dcl: end"));
   end Parse_Const_Dcl;

   ------------------------
   --  Parse_Const_Type  --
   ------------------------
   procedure Parse_Const_Type (Result : out Node_Id;
                               Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Const_Type: enter"));
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
               Parse_Char_Type (Result, Success);
         when T_Wchar =>
            Parse_Wide_Char_Type (Result, Success);
         when T_Boolean =>
            Parse_Boolean_Type (Result, Success);
         when T_Float
           | T_Double =>
            Parse_Floating_Pt_Type (Result, Success);
         when T_String =>
               Parse_String_Type (Result, Success);
         when T_Wstring =>
            Parse_Wide_String_Type (Result, Success);
         when T_Fixed =>
            Parse_Fixed_Pt_Type (Result, Success);
         when T_Colon_Colon
           | T_Identifier =>
            Parse_Scoped_Name (Result, Success);
            --  The <scoped_name> in the <const_type> production
            --  must be a previously defined integer, char, wide_char,
            --  boolean, floating_pt, string, wide_string, octet
            --  or enum type.
            if not Success then
               Result := No_Node;
               pragma Debug (O2 ("Parse_Const_Type: end"));
               return;
            end if;
            if Result /= No_Node then
               declare
                  Invalid_Type : Boolean := False;
               begin
                  if S_Type (Result) /= No_Node then
                     pragma Debug
                       (O ("Parse_Const_Type : scoped name " &
                           "found. Its type is " &
                           Img (Kind (S_Type (Result)))));
                     case Kind (S_Type (Result)) is
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
                           Invalid_Type := True;
                     end case;
                  else
                     Invalid_Type := True;
                  end if;
                  if Invalid_Type then
                     Errors.Error
                       ("Invalid type in constant. The " &
                        "scoped name should refer to " &
                        "an integer, char, wide_char, " &
                        "boolean, floating_pt, string, " &
                        "wide_string, octet or enum type.",
                        Errors.Error,
                        Get_Token_Location);
                     Success := False;
                  end if;
               end;
            end if;
         when T_Octet =>
            Parse_Octet_Type (Result, Success);
         when others =>
            Errors.Error ("constant type expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Success := False;
            Result := No_Node;
      end case;
      pragma Debug (O2 ("Parse_Const_Type: end"));
      return;
   end Parse_Const_Type;

   -----------------------
   --  Parse_Const_Exp  --
   -----------------------
   procedure Parse_Const_Exp (Result : out Node_Id;
                              Constant_Type : in Node_Id;
                              Success : out Boolean) is
      Loc : Errors.Location;
      C_Type : Constant_Value_Ptr;
   begin
      pragma Debug (O2 ("Parse_Const_Exp: enter"));
      Loc := Get_Token_Location;
      if Constant_Type /= No_Node then
         case Kind (Constant_Type) is
            when K_Short =>
               C_Type := new Constant_Value (Kind => C_Short);
            when K_Unsigned_Short =>
               C_Type := new Constant_Value (Kind => C_UShort);
            when K_Long =>
               C_Type := new Constant_Value (Kind => C_Long);
            when K_Unsigned_Long =>
               C_Type := new Constant_Value (Kind => C_ULong);
            when K_Long_Long =>
               C_Type := new Constant_Value (Kind => C_LongLong);
            when K_Unsigned_Long_Long =>
               C_Type := new Constant_Value (Kind => C_ULongLong);
            when K_Char =>
               C_Type := new Constant_Value (Kind => C_Char);
            when K_Wide_Char =>
               C_Type := new Constant_Value (Kind => C_WChar);
            when K_Boolean =>
               C_Type := new Constant_Value (Kind => C_Boolean);
            when K_Float =>
               C_Type := new Constant_Value (Kind => C_Float);
            when K_Double =>
               C_Type := new Constant_Value (Kind => C_Double);
            when K_Long_Double =>
               C_Type := new Constant_Value (Kind => C_LongDouble);
            when K_Fixed =>
               --  FIXME : verify the values of digits_nb and scale
               C_Type := new Constant_Value (Kind => C_Fixed);
               C_Type.Digits_Nb := Expr_Value
                 (Digits_Nb (Constant_Type)).Integer_Value;
               C_Type.Scale := Expr_Value
                 (Scale (Constant_Type)).Integer_Value;
            when K_String =>
               C_Type := new Constant_Value (Kind => C_String);
               if Bound (Constant_Type) = No_Node then
                  C_Type.String_Length := -1;
               else
                  C_Type.String_Length :=
                    Expr_Value (Bound (Constant_Type)).Integer_Value;
               end if;
            when K_Wide_String =>
               C_Type := new Constant_Value (Kind => C_WString);
               if Bound (Constant_Type) = No_Node then
                  C_Type.WString_Length := -1;
               else
                  C_Type.WString_Length :=
                    Expr_Value (Bound (Constant_Type)).Integer_Value;
               end if;
            when K_Octet =>
               C_Type := new Constant_Value (Kind => C_Octet);
            when K_Enum =>
               C_Type := new Constant_Value (Kind => C_Enum);
               C_Type.Enum_Name := Constant_Type;
            when K_Scoped_Name =>
               case Kind (S_Type (Constant_Type)) is
                  when K_Short =>
                     C_Type := new Constant_Value (Kind => C_Short);
                  when K_Unsigned_Short =>
                     C_Type := new Constant_Value (Kind => C_UShort);
                  when K_Long =>
                     C_Type := new Constant_Value (Kind => C_Long);
                  when K_Unsigned_Long =>
                     C_Type := new Constant_Value (Kind => C_ULong);
                  when K_Long_Long =>
                     C_Type := new Constant_Value (Kind => C_LongLong);
                  when K_Unsigned_Long_Long =>
                     C_Type := new Constant_Value (Kind => C_ULongLong);
                  when K_Char =>
                     C_Type := new Constant_Value (Kind => C_Char);
                  when K_Wide_Char =>
                     C_Type := new Constant_Value (Kind => C_WChar);
                  when K_Boolean =>
                     C_Type := new Constant_Value (Kind => C_Boolean);
                  when K_Float =>
                     C_Type := new Constant_Value (Kind => C_Float);
                  when K_Double =>
                     C_Type := new Constant_Value (Kind => C_Double);
                  when K_Long_Double =>
                     C_Type := new Constant_Value (Kind => C_LongDouble);
                  when K_Fixed =>
                     --  FIXME : verify the values of digits_nb and scale
                     C_Type := new Constant_Value (Kind => C_Fixed);
                     C_Type.Digits_Nb :=
                       Expr_Value (Digits_Nb (S_Type (Constant_Type)))
                       .Integer_Value;
                     C_Type.Scale :=
                       Expr_Value (Scale (S_Type (Constant_Type)))
                       .Integer_Value;
                  when K_String =>
                     C_Type := new Constant_Value (Kind => C_String);
                     if Bound (S_Type (Constant_Type)) = No_Node then
                        C_Type.String_Length := -1;
                     else
                        C_Type.String_Length :=
                          Expr_Value (Bound (S_Type (Constant_Type)))
                          .Integer_Value;
                     end if;
                  when K_Wide_String =>
                     C_Type := new Constant_Value (Kind => C_WString);
                     if Bound (S_Type (Constant_Type)) = No_Node then
                        C_Type.WString_Length := -1;
                     else
                        C_Type.WString_Length :=
                          Expr_Value (Bound (S_Type (Constant_Type)))
                          .Integer_Value;
                     end if;
                  when K_Octet =>
                     C_Type := new Constant_Value (Kind => C_Octet);
                  when K_Enum =>
                     C_Type := new Constant_Value (Kind => C_Enum);
                     C_Type.Enum_Name := S_Type (Constant_Type);
                  when others =>
                     raise Errors.Internal_Error;
               end case;
            when others =>
               raise Errors.Internal_Error;
         end case;
      else
         C_Type := new Constant_Value (Kind => C_No_Kind);
      end if;
      Parse_Or_Expr (Result, Success, C_Type);
      if Result /= No_Node then
         Check_Value_Range (Result, True);
      end if;
      Free (C_Type);
      pragma Debug (O2 ("Parse_Const_Exp: end"));
   end Parse_Const_Exp;

   ---------------------
   --  Parse_Or_Expr  --
   ---------------------
   procedure Parse_Or_Expr (Result : out Node_Id;
                            Success : out Boolean;
                            Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Or_Expr: enter"));
      Parse_Xor_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      while Get_Token = T_Bar loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
         begin
            pragma Debug (O ("Parse_Or_Expr : '|' detected"));
            Loc := Get_Token_Location;
            Next_Token;
            pragma Debug (O ("Parse_Or_Expr : making the or node"));
            Res := Make_Or_Expr (Loc);
            pragma Debug (O ("Parse_Or_Expr : setting the first term"));
            Set_Left (Res, Result);
            pragma Debug (O ("Parse_Or_Expr : parsing of the second term"));
            Parse_Xor_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet
              or else Expr_Type.Kind = C_Short
              or else Expr_Type.Kind = C_Long
              or else Expr_Type.Kind = C_LongLong
              or else Expr_Type.Kind = C_UShort
              or else Expr_Type.Kind = C_ULong
              or else Expr_Type.Kind = C_ULongLong
              or else Expr_Type.Kind = C_General_Integer
            then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind
                 and then Expr_Value (Right (Res)).Kind /= C_No_Kind
               then
                  if Expr_Value (Right (Res)).Kind /= C_General_Integer
                    and then Expr_Value (Left (Res)).Kind /= C_General_Integer
                  then
                     Set_Expr_Value (Res, Duplicate (Expr_Type));
                  else
                     Set_Expr_Value
                       (Res, new Constant_Value (Kind => C_General_Integer));
                  end if;
                  Expr_Value (Res).Integer_Value :=
                    Expr_Value (Left (Res)).Integer_Value
                    or Expr_Value (Right (Res)).Integer_Value;
                  Check_Value_Range (Res, False);
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               Errors.Error ("The | operation is not defined " &
                                    "on this type.",
                                    Errors.Error,
                                    Loc);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop;
      pragma Debug (O2 ("Parse_Or_Expr: end"));
      return;
   end Parse_Or_Expr;

   ---------------------
   --  Parse_Xor_Exp  --
   ---------------------
   procedure Parse_Xor_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Xor_Expr: enter"));
      Parse_And_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      while Get_Token = T_Circumflex loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;
            Next_Token;
            Res := Make_Xor_Expr (Loc);
            Set_Left (Res, Result);
            Parse_And_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet
              or else Expr_Type.Kind = C_Short
              or else Expr_Type.Kind = C_Long
              or else Expr_Type.Kind = C_LongLong
              or else Expr_Type.Kind = C_UShort
              or else Expr_Type.Kind = C_ULong
              or else Expr_Type.Kind = C_ULongLong
              or else Expr_Type.Kind = C_General_Integer
            then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind
                 and then Expr_Value (Right (Res)).Kind /= C_No_Kind
               then
                  if Expr_Value (Right (Res)).Kind /= C_General_Integer
                    and then Expr_Value (Left (Res)).Kind /= C_General_Integer
                  then
                     Set_Expr_Value (Res, Duplicate (Expr_Type));
                  else
                     Set_Expr_Value
                       (Res, new Constant_Value (Kind => C_General_Integer));
                  end if;
                  Expr_Value (Res).Integer_Value :=
                    Expr_Value (Left (Res)).Integer_Value
                    xor Expr_Value (Right (Res)).Integer_Value;
                  Check_Value_Range (Res, False);
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               Errors.Error ("The ^ operation is not defined " &
                                    "on this type.",
                                    Errors.Error,
                                    Loc);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop;
      pragma Debug (O2 ("Parse_Xor_Expr: end"));
      return;
   end Parse_Xor_Expr;

   ---------------------
   --  Parse_And_Exp  --
   ---------------------
   procedure Parse_And_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_And_Expr: enter"));
      Parse_Shift_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      while Get_Token = T_Ampersand loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
         begin
            Loc := Get_Token_Location;
            Next_Token;
            Res := Make_And_Expr (Loc);
            Set_Left (Res, Result);
            Parse_Shift_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet
              or else Expr_Type.Kind = C_Short
              or else Expr_Type.Kind = C_Long
              or else Expr_Type.Kind = C_LongLong
              or else Expr_Type.Kind = C_UShort
              or else Expr_Type.Kind = C_ULong
              or else Expr_Type.Kind = C_ULongLong
              or else Expr_Type.Kind = C_General_Integer
            then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind
                 and then Expr_Value (Right (Res)).Kind /= C_No_Kind
               then
                  if Expr_Value (Right (Res)).Kind /= C_General_Integer
                    and then Expr_Value (Left (Res)).Kind /= C_General_Integer
                  then
                     Set_Expr_Value (Res, Duplicate (Expr_Type));
                  else
                     Set_Expr_Value
                       (Res, new Constant_Value (Kind => C_General_Integer));
                  end if;
                  Expr_Value (Res).Integer_Value :=
                    Expr_Value (Left (Res)).Integer_Value
                    and Expr_Value (Right (Res)).Integer_Value;
                  Check_Value_Range (Res, False);
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               Errors.Error ("The & operation is not defined " &
                                    "on this type.",
                                    Errors.Error,
                                    Loc);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop;
      pragma Debug (O2 ("Parse_And_Expr: end"));
      return;
   end Parse_And_Expr;

   -----------------------
   --  Parse_Shift_Exp  --
   -----------------------
   procedure Parse_Shift_Expr (Result : out Node_Id;
                               Success : out Boolean;
                               Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Shift_Expr: enter"));
      Parse_Add_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      While_Loop :
      while Get_Token = T_Greater_Greater or
        Get_Token = T_Less_Less loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
            Shl : Boolean;
         begin
            --  if we have a t_greater_greater and no expression
            --  after it, it is likely to be a double sequence
            --  end, so we exit from the loop
            if Get_Token = T_Greater_Greater then
               case View_Next_Token is
                  when T_Left_Paren
                    | T_Plus
                    | T_Minus
                    | T_Tilde
                    | T_Lit_Decimal_Integer
                    | T_Lit_Octal_Integer
                    | T_Lit_Hexa_Integer =>
                     null;
                  when T_Identifier =>
                     --  FIXME : not always exit
                     exit While_Loop;
                  when T_Greater
                    | T_Greater_Greater
                    | T_Comma =>
                     exit While_Loop;
                  when others =>
                     null;
               end case;
            end if;
            Loc := Get_Token_Location;
            Next_Token;
            if View_Previous_Token = T_Greater_Greater then
               Shl := False;
               Res := Make_Shr_Expr (Loc);
            else
               Shl := True;
               Res := Make_Shl_Expr (Loc);
            end if;
            Set_Left (Res, Result);
            Parse_Add_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet or
              Expr_Type.Kind = C_Short or
              Expr_Type.Kind = C_Long or
              Expr_Type.Kind = C_LongLong or
              Expr_Type.Kind = C_UShort or
              Expr_Type.Kind = C_ULong or
              Expr_Type.Kind = C_ULongLong or
              Expr_Type.Kind = C_General_Integer then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind and
                 Expr_Value (Right (Res)).Kind /= C_No_Kind then
                  if Expr_Value (Right (Res)).Kind /= C_General_Integer and
                    Expr_Value (Left (Res)).Kind /= C_General_Integer then
                     Set_Expr_Value (Res, Duplicate (Expr_Type));
                  else
                     Set_Expr_Value
                       (Res, new Constant_Value (Kind => C_General_Integer));
                  end if;
                  --  check the value of the right operand
                  if Expr_Value (Right (Res)).Integer_Value < 0 or
                    Expr_Value (Right (Res)).Integer_Value > 63
                  then
                     if Expr_Value (Right (Res)).Integer_Value < 0 then
                        Errors.Error ("The right operand must be " &
                                             "positive. The shift operation " &
                                             "will be ignored.",
                                             Errors.Error,
                                             Loc);
                        Expr_Value (Res).Integer_Value :=
                          Expr_Value (Left (Res)).Integer_Value;
                     else
                        Errors.Error ("The right operand must be " &
                                             "less than 64. The result will " &
                                             "be put to 0.",
                                             Errors.Error,
                                             Loc);
                        Expr_Value (Res).Integer_Value := 0;
                     end if;
                  else
                     if Shl then
                        Expr_Value (Res).Integer_Value := Shift_Left
                          (Expr_Value (Left (Res)).Integer_Value,
                           Natural (Expr_Value (Right (Res)).Integer_Value));
                     else
                        Expr_Value (Res).Integer_Value := Shift_Right
                          (Expr_Value (Left (Res)).Integer_Value,
                           Natural (Expr_Value (Right (Res)).Integer_Value));
                     end if;
                  end if;
                  Check_Value_Range (Res, False);
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               Errors.Error ("The << and >> operations are not " &
                                    "defined on this type.",
                                    Errors.Error,
                                    Loc);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop While_Loop;
      pragma Debug (O2 ("Parse_Shift_Expr: end"));
      return;
   end Parse_Shift_Expr;

   ---------------------
   --  Parse_Add_Exp  --
   ---------------------
   procedure Parse_Add_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Add_Expr: enter"));
      Parse_Mult_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      while Get_Token = T_Plus or
        Get_Token = T_Minus loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
            Plus : Boolean;
         begin
            Loc := Get_Token_Location;
            Next_Token;
            if View_Previous_Token = T_Plus then
               Plus := True;
               Res := Make_Add_Expr (Loc);
            else
               Plus := False;
               Res := Make_Sub_Expr (Loc);
            end if;
            Set_Left (Res, Result);
            Parse_Mult_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet or
              Expr_Type.Kind = C_Short or
              Expr_Type.Kind = C_Long or
              Expr_Type.Kind = C_LongLong or
              Expr_Type.Kind = C_UShort or
              Expr_Type.Kind = C_ULong or
              Expr_Type.Kind = C_ULongLong or
              Expr_Type.Kind = C_General_Integer or
              Expr_Type.Kind = C_Float or
              Expr_Type.Kind = C_Double or
              Expr_Type.Kind = C_LongDouble or
              Expr_Type.Kind = C_General_Float or
              Expr_Type.Kind = C_Fixed or
              Expr_Type.Kind = C_General_Fixed then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind and
                 Expr_Value (Right (Res)).Kind /= C_No_Kind then
                  if Expr_Type.Kind = C_Octet or
                    Expr_Type.Kind = C_Short or
                    Expr_Type.Kind = C_Long or
                    Expr_Type.Kind = C_LongLong or
                    Expr_Type.Kind = C_UShort or
                    Expr_Type.Kind = C_ULong or
                    Expr_Type.Kind = C_ULongLong or
                    Expr_Type.Kind = C_General_Integer then
                     if Expr_Value (Right (Res)).Kind /= C_General_Integer and
                       Expr_Value (Left (Res)).Kind /= C_General_Integer then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res,
                           new Constant_Value (Kind => C_General_Integer));
                     end if;
                     if Plus then
                        Expr_Value (Res).Integer_Value :=
                          Expr_Value (Left (Res)).Integer_Value +
                          Expr_Value (Right (Res)).Integer_Value;
                     else
                        Expr_Value (Res).Integer_Value :=
                          Expr_Value (Left (Res)).Integer_Value -
                          Expr_Value (Right (Res)).Integer_Value;
                     end if;
                  elsif Expr_Type.Kind = C_Float or
                    Expr_Type.Kind = C_Double or
                    Expr_Type.Kind = C_LongDouble or
                    Expr_Type.Kind = C_General_Float then
                     if Expr_Value (Right (Res)).Kind /= C_General_Float and
                       Expr_Value (Left (Res)).Kind /= C_General_Float then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res, new Constant_Value (Kind => C_General_Float));
                     end if;
                     if Plus then
                        Expr_Value (Res).Float_Value :=
                          Expr_Value (Left (Res)).Float_Value +
                          Expr_Value (Right (Res)).Float_Value;
                     else
                        Expr_Value (Res).Float_Value :=
                          Expr_Value (Left (Res)).Float_Value -
                          Expr_Value (Right (Res)).Float_Value;
                     end if;
                  else
                     if Expr_Value (Right (Res)).Kind /= C_General_Fixed and
                       Expr_Value (Left (Res)).Kind /= C_General_Fixed then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res, new Constant_Value (Kind => C_General_Fixed));
                        Expr_Value (Res).Digits_Nb := Expr_Type.Digits_Nb;
                        Expr_Value (Res).Scale := Expr_Type.Scale;
                     end if;
                     declare
                        Res_Expr : Constant_Value_Ptr := Expr_Value (Res);
                     begin
                        if Plus then
                           Fixed_Add (Res_Expr,
                                      Expr_Value (Left (Res)),
                                      Expr_Value (Right (Res)));
                        else
                           Fixed_Sub (Res_Expr,
                                      Expr_Value (Left (Res)),
                                      Expr_Value (Right (Res)));
                        end if;
                        Set_Expr_Value (Res, Res_Expr);
                     end;
                  end if;
                  Check_Value_Range (Res, False);
                  if Expr_Value (Res).Kind = C_Fixed and
                    Expr_Type.Kind = C_Fixed then
                     --  checks precision of the fixed value after
                     --  the possible simplifications in
                     --  check_value_range
                     if Expr_Value (Res).Digits_Nb - Expr_Value (Res).Scale >
                       Expr_Type.Digits_Nb - Expr_Type.Scale or
                       Expr_Value (Res).Scale > Expr_Type.Scale then
                        Errors.Error
                          ("The specified type for this fixed point " &
                           "constant is not enough precise for its value. " &
                           "A more precise type will be used.",
                           Errors.Error,
                           Get_Token_Location);
                        declare
                           Value : Constant_Value_Ptr := Expr_Value (Res);
                        begin
                           Set_Expr_Value
                             (Res,
                              new Constant_Value (Kind => C_General_Fixed));
                           Expr_Value (Res).Fixed_Value := Value.Fixed_Value;
                           Expr_Value (Res).Digits_Nb := Value.Digits_Nb;
                           Expr_Value (Res).Scale := Value.Scale;
                           Free (Value);
                        end;
                     end if;
                  end if;
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               Errors.Error ("The + and - operations are not defined " &
                                    "on this type.",
                                    Errors.Error,
                                    Loc);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop;
      pragma Debug (O2 ("Parse_Add_Expr: end"));
      return;
   end Parse_Add_Expr;

   ----------------------
   --  Parse_Mult_Exp  --
   ----------------------
   procedure Parse_Mult_Expr (Result : out Node_Id;
                              Success : out Boolean;
                              Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Mult_Expr: enter"));
      Parse_Unary_Expr (Result, Success, Expr_Type);
      if not Success then
         return;
      end if;
      while Get_Token = T_Star or
        Get_Token = T_Slash or
        Get_Token = T_Percent loop
         declare
            Res : Node_Id;
            Res_Right : Node_Id;
            Loc : Errors.Location;
            type Operator_Type is (Mul, Div, Modulo);
            Op : Operator_Type;
         begin
            Loc := Get_Token_Location;
            Next_Token;
            if View_Previous_Token = T_Star then
               Op := Mul;
               Res := Make_Mul_Expr (Loc);
            elsif View_Previous_Token = T_Slash then
               Op := Div;
               Res := Make_Div_Expr (Loc);
            else
               Op := Modulo;
               Res := Make_Mod_Expr (Loc);
            end if;
            Set_Left (Res, Result);
            Parse_Unary_Expr (Res_Right, Success, Expr_Type);
            if not Success or else Res_Right = No_Node then
               Set_Right (Res, No_Node);
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            Set_Right (Res, Res_Right);
            if Left (Res) = No_Node then
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
               Result := Res;
               return;
            end if;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet or
              Expr_Type.Kind = C_Short or
              Expr_Type.Kind = C_Long or
              Expr_Type.Kind = C_LongLong or
              Expr_Type.Kind = C_UShort or
              Expr_Type.Kind = C_ULong or
              Expr_Type.Kind = C_ULongLong or
              Expr_Type.Kind = C_General_Integer or
              ((Expr_Type.Kind = C_Float or
                Expr_Type.Kind = C_Double or
                Expr_Type.Kind = C_LongDouble or
                Expr_Type.Kind = C_General_Float or
                Expr_Type.Kind = C_Fixed or
                Expr_Type.Kind = C_General_Fixed) and
               Op /= Modulo) then
               --  test if both sons have a type
               if Expr_Value (Left (Res)).Kind /= C_No_Kind and
                 Expr_Value (Right (Res)).Kind /= C_No_Kind then
                  if Expr_Type.Kind = C_Octet or
                    Expr_Type.Kind = C_Short or
                    Expr_Type.Kind = C_Long or
                    Expr_Type.Kind = C_LongLong or
                    Expr_Type.Kind = C_UShort or
                    Expr_Type.Kind = C_ULong or
                    Expr_Type.Kind = C_ULongLong or
                    Expr_Type.Kind = C_General_Integer then
                     if Expr_Value (Right (Res)).Kind /= C_General_Integer and
                       Expr_Value (Left (Res)).Kind /= C_General_Integer then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res,
                           new Constant_Value (Kind => C_General_Integer));
                     end if;
                     if Op = Mul then
                        Expr_Value (Res).Integer_Value :=
                          Expr_Value (Left (Res)).Integer_Value *
                          Expr_Value (Right (Res)).Integer_Value;
                     elsif Op = Div then
                        if Expr_Value (Right (Res)).Integer_Value = 0 then
                           Errors.Error
                             ("The second operand of the division is 0. " &
                              "The operation will be ignored.",
                              Errors.Error,
                              Loc);
                           Expr_Value (Res).Integer_Value :=
                             Expr_Value (Left (Res)).Integer_Value;
                        else
                           Expr_Value (Res).Integer_Value :=
                             Expr_Value (Left (Res)).Integer_Value /
                             Expr_Value (Right (Res)).Integer_Value;
                        end if;
                     else
                        if Expr_Value (Right (Res)).Integer_Value = 0 then
                           Errors.Error
                             ("The second operand of the modulo is 0. " &
                              "The modulo operation will be ignored.",
                              Errors.Error,
                              Loc);
                           Expr_Value (Res).Integer_Value :=
                             Expr_Value (Left (Res)).Integer_Value;
                        else
                           Expr_Value (Res).Integer_Value :=
                             Expr_Value (Left (Res)).Integer_Value mod
                             Expr_Value (Right (Res)).Integer_Value;
                        end if;
                     end if;
                  elsif Expr_Type.Kind = C_Float or
                    Expr_Type.Kind = C_Double or
                    Expr_Type.Kind = C_LongDouble or
                    Expr_Type.Kind = C_General_Float then
                     if Expr_Value (Right (Res)).Kind /= C_General_Float and
                       Expr_Value (Left (Res)).Kind /= C_General_Float then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res, new Constant_Value (Kind => C_General_Float));
                     end if;
                     if Op = Mul then
                        Expr_Value (Res).Float_Value :=
                          Expr_Value (Left (Res)).Float_Value *
                          Expr_Value (Right (Res)).Float_Value;
                     else
                        if Expr_Value (Right (Res)).Float_Value = 0.0 then
                           Errors.Error
                             ("The second operand of the division is 0. " &
                              "The operation will be ignored.",
                              Errors.Error,
                              Loc);
                           Expr_Value (Res).Float_Value :=
                             Expr_Value (Left (Res)).Float_Value;
                        else
                           Expr_Value (Res).Float_Value :=
                             Expr_Value (Left (Res)).Float_Value /
                             Expr_Value (Right (Res)).Float_Value;
                        end if;
                     end if;
                  else
                     if Expr_Value (Right (Res)).Kind /= C_General_Fixed and
                       Expr_Value (Left (Res)).Kind /= C_General_Fixed then
                        Set_Expr_Value (Res, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Res, new Constant_Value (Kind => C_General_Fixed));
                        Expr_Value (Res).Digits_Nb := Expr_Type.Digits_Nb;
                        Expr_Value (Res).Scale := Expr_Type.Scale;
                     end if;
                     declare
                        Res_Expr : Constant_Value_Ptr := Expr_Value (Res);
                     begin
                        if Op = Mul then
                           Fixed_Mul (Res_Expr,
                                      Expr_Value (Left (Res)),
                                      Expr_Value (Right (Res)));
                        else
                           if Expr_Value (Right (Res)).Fixed_Value = 0 then
                              Errors.Error
                                ("The second operand of the division is 0. " &
                                 "The operation will be ignored.",
                                 Errors.Error,
                                 Loc);
                              Expr_Value (Res).Fixed_Value :=
                                Expr_Value (Left (Res)).Fixed_Value;
                           else
                              Fixed_Div (Res_Expr,
                                         Expr_Value (Left (Res)),
                                         Expr_Value (Right (Res)));
                           end if;
                        end if;
                     end;
                  end if;
                  Check_Value_Range (Res, False);
                  if Expr_Value (Res).Kind = C_Fixed and
                    Expr_Type.Kind = C_Fixed then
                     --  checks precision of the fixed value after
                     --  the possible simplifications in
                     --  check_value_range
                     if Expr_Value (Res).Digits_Nb - Expr_Value (Res).Scale >
                       Expr_Type.Digits_Nb - Expr_Type.Scale or
                       Expr_Value (Res).Scale > Expr_Type.Scale then
                        Errors.Error
                          ("The specified type for this fixed point " &
                           "constant is not enough precise for its value. " &
                           "A more precise type will be used.",
                           Errors.Error,
                           Get_Token_Location);
                        declare
                           Value : Constant_Value_Ptr := Expr_Value (Res);
                        begin
                           Set_Expr_Value
                             (Res,
                              new Constant_Value (Kind => C_General_Fixed));
                           Expr_Value (Res).Fixed_Value := Value.Fixed_Value;
                           Expr_Value (Res).Digits_Nb := Value.Digits_Nb;
                           Expr_Value (Res).Scale := Value.Scale;
                           Free (Value);
                        end;
                     end if;
                  end if;
               else
                  Set_Expr_Value
                    (Res, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               if Op = Modulo then
                  Errors.Error ("The % operation is not defined " &
                                       "on this type.",
                                       Errors.Error,
                                       Loc);
               else
                  Errors.Error ("The * and / operations are not " &
                                       "defined on this type.",
                                       Errors.Error,
                                       Loc);
               end if;
               Set_Expr_Value
                 (Res, new Constant_Value (Kind => C_No_Kind));
            end if;
            Result := Res;
         end;
      end loop;
      pragma Debug (O2 ("Parse_Mult_Expr: end"));
      return;
   end Parse_Mult_Expr;

   -----------------------
   --  Parse_Unary_Exp  --
   -----------------------

   procedure Parse_Unary_Expr (Result : out Node_Id;
                               Success : out Boolean;
                               Expr_Type : in Constant_Value_Ptr) is
      type Operator_Type is (Plus, Minus, Tilde);
      Op : Operator_Type;
      Loc : Errors.Location;
   begin
      pragma Debug (O2 ("Parse_Unary_Expr: enter"));
      case Get_Token is
         when T_Plus
           | T_Minus
           | T_Tilde =>
            Loc := Get_Token_Location;
            if Get_Token = T_Plus then
               Op := Plus;
               Result := Make_Id_Expr (Get_Token_Location);
            elsif Get_Token = T_Minus then
               Op := Minus;
               Result := Make_Neg_Expr (Get_Token_Location);
            else
               Op := Tilde;
               Result := Make_Not_Expr (Get_Token_Location);
            end if;
            Next_Token;
            declare
               Operand : Node_Id;
            begin
               Parse_Primary_Expr (Operand, Success, Expr_Type);
               if not Success or else Operand = No_Node then
                  Set_Operand (Result, No_Node);
                  Set_Expr_Value
                    (Result, new Constant_Value (Kind => C_No_Kind));
                  pragma Debug (O2 ("Parse_Unary_Expr: end"));
                  return;
               end if;
               Set_Operand (Result, Operand);
            end;
            --  test if the types are ok for the or operation
            if Expr_Type.Kind = C_Octet
              or else Expr_Type.Kind = C_Short
              or else Expr_Type.Kind = C_Long
              or else Expr_Type.Kind = C_LongLong
              or else Expr_Type.Kind = C_UShort
              or else Expr_Type.Kind = C_ULong
              or else Expr_Type.Kind = C_ULongLong
              or else Expr_Type.Kind = C_General_Integer
              or else ((Expr_Type.Kind = C_Float
                or else Expr_Type.Kind = C_Double
                or else Expr_Type.Kind = C_LongDouble
                or else Expr_Type.Kind = C_General_Float
                or else Expr_Type.Kind = C_Fixed
                or else Expr_Type.Kind = C_General_Fixed)
               and then Op /= Tilde)
            then
               --  Test whether the operand has a type
               if Expr_Value (Operand (Result)).Kind /= C_No_Kind then
                  if Expr_Type.Kind = C_Octet or
                    Expr_Type.Kind = C_Short or
                    Expr_Type.Kind = C_Long or
                    Expr_Type.Kind = C_LongLong or
                    Expr_Type.Kind = C_UShort or
                    Expr_Type.Kind = C_ULong or
                    Expr_Type.Kind = C_ULongLong or
                    Expr_Type.Kind = C_General_Integer then
                     if Expr_Value (Operand (Result)).Kind
                       /= C_General_Integer then
                        Set_Expr_Value (Result, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Result,
                           new Constant_Value (Kind => C_General_Integer));
                     end if;
                     if Op = Plus then
                        Expr_Value (Result).Integer_Value :=
                          Expr_Value (Operand (Result)).Integer_Value;
                     elsif Op = Minus then
                        Expr_Value (Result).Integer_Value :=
                          -Expr_Value (Operand (Result)).Integer_Value;
                     else
                        Expr_Value (Result).Integer_Value :=
                          not Expr_Value (Operand (Result)).Integer_Value;
                     end if;
                  elsif Expr_Type.Kind = C_Float or
                    Expr_Type.Kind = C_Double or
                    Expr_Type.Kind = C_LongDouble or
                    Expr_Type.Kind = C_General_Float then
                     if Expr_Value (Operand (Result)).Kind
                       /= C_General_Float then
                        Set_Expr_Value (Result, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Result,
                           new Constant_Value (Kind => C_General_Float));
                     end if;
                     if Op = Plus then
                        Expr_Value (Result).Float_Value :=
                          Expr_Value (Operand (Result)).Float_Value;
                     else
                        Expr_Value (Result).Float_Value :=
                          -Expr_Value (Operand (Result)).Float_Value;
                     end if;
                  else
                     if Expr_Value (Operand (Result)).Kind
                       /= C_General_Fixed then
                        Set_Expr_Value (Result, Duplicate (Expr_Type));
                     else
                        Set_Expr_Value
                          (Result,
                           new Constant_Value (Kind => C_General_Fixed));
                        Expr_Value (Result).Digits_Nb := Expr_Type.Digits_Nb;
                        Expr_Value (Result).Scale := Expr_Type.Scale;
                     end if;
                     declare
                        Res_Expr : Constant_Value_Ptr := Expr_Value (Result);
                     begin
                        if Op = Plus then
                           Fixed_Id (Res_Expr,
                                     Expr_Value (Operand (Result)));
                        else
                           Fixed_Neg (Res_Expr,
                                      Expr_Value (Operand (Result)));
                        end if;
                        Set_Expr_Value (Result, Res_Expr);
                     end;
                  end if;
               else
                  Set_Expr_Value
                    (Result, new Constant_Value (Kind => C_No_Kind));
               end if;
            else
               if Op = Tilde then
                  Errors.Error ("The ~ operation is not defined " &
                                       "on this type.",
                                       Errors.Error,
                                       Loc);
               else
                  Errors.Error ("The unary + and - operations are " &
                                       "not defined on this type.",
                                       Errors.Error,
                                       Loc);
               end if;
               Set_Expr_Value
                 (Result, new Constant_Value (Kind => C_No_Kind));
            end if;
            Check_Value_Range (Result, False);
         when others =>
            Parse_Primary_Expr (Result, Success, Expr_Type);
      end case;
      pragma Debug (O2 ("Parse_Unary_Expr: end"));
      return;
   end Parse_Unary_Expr;

   -------------------------
   --  Parse_Primary_Exp  --
   -------------------------

   procedure Parse_Primary_Expr (Result : out Node_Id;
                                 Success : out Boolean;
                                 Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Primary_Expr: enter"));
      case Get_Token is
         when  T_Colon_Colon
           | T_Identifier =>
            declare
               Local_Res : Node_Id;
            begin
               Parse_Scoped_Name (Local_Res, Success);
               if Success then
                  --  this scoped name must denote a previously
                  --  defined constant or enum value
                  if Local_Res /= No_Node then
                     --  If it is a constant, check its type and
                     --  duplicate its value
                     if Kind (Value (Local_Res)) = K_Const_Dcl then
                        Check_Expr_Value
                          (Expr_Value (Expression (Value (Local_Res))),
                           Expr_Type);
                        Result := Expression (Value (Local_Res));
                     elsif Kind (Value (Local_Res)) = K_Enumerator then
                        --  If it is an enum value, check the specified type
                        Result := Make_Lit_Enum (Get_Token_Location);
                        if Expr_Type.Kind = C_Enum then
                           --  checks that the value is of the right type
                           pragma Debug (O ("Parse_Primary_Expr : Kind " &
                                            "(Expr_Type.Enum_Name) is " &
                                            Node_Kind'Image
                                            (Kind (Expr_Type.Enum_Name))));
                           if not Is_In_List
                             (Enumerators (Expr_Type.Enum_Name),
                              Value (Local_Res)) then
                              Errors.Error
                                ("The specified type for this constant " &
                                 "does not match with its value.",
                                 Errors.Error,
                                 Get_Token_Location);
                              Set_Expr_Value
                                (Result,
                                 new Constant_Value (Kind => C_No_Kind));
                           else
                              Set_Expr_Value
                                (Result, new Constant_Value (Kind => C_Enum));
                              Expr_Value (Result).Enum_Name :=
                                Expr_Type.Enum_Name;
                              Expr_Value (Result).Enum_Value :=
                                Value (Local_Res);
                           end if;
                        else
                           Set_Expr_Value
                             (Result, new Constant_Value (Kind => C_No_Kind));
                           Errors.Error
                             ("The specified type for this constant " &
                              "does not match with its value.",
                              Errors.Error,
                              Get_Token_Location);
                        end if;
                     else
                        --  If no constant and no enum value, error
                        Errors.Error
                          ("This scoped name must denote a constant value",
                           Errors.Error,
                           Get_Token_Location);
                        Result := No_Node;
                     end if;
                  else
                     Result := No_Node;
                  end if;
               else
                  Result := No_Node;
               end if;
            end;
         when T_Lit_Decimal_Integer
           | T_Lit_Octal_Integer
           | T_Lit_Hexa_Integer
           | T_Lit_String
           | T_Lit_Wide_String
           | T_Lit_Char
           | T_Lit_Wide_Char
           | T_Lit_Simple_Floating_Point
           | T_Lit_Exponent_Floating_Point
           | T_Lit_Pure_Exponent_Floating_Point
           | T_Lit_Simple_Fixed_Point
           | T_Lit_Floating_Fixed_Point
           | T_True
           | T_False =>
            Parse_Literal (Result,
                           Success,
                           Expr_Type);
         when T_Left_Paren =>
            Next_Token;
            Parse_Or_Expr (Result, Success, Expr_Type);
            if not Success then
               pragma Debug (O2 ("Parse_Primary_Expr: end"));
               return;
            end if;
            if Get_Token /= T_Right_Paren then
               Errors.Error ("')' expected at the end  of ." &
                                           "a constant expression.",
                                           Errors.Error,
                                           Get_Token_Location);
               Success := False;
               pragma Debug (O2 ("Parse_Primary_Expr: end"));
               return;
            end if;
            Next_Token;
         when others =>
            Errors.Error ("primary expression expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := No_Node;
            Success := False;
            pragma Debug (O2 ("Parse_Primary_Expr: end"));
            return;
      end case;
      pragma Debug (O2 ("Parse_Primary_Expr: end"));
      return;
   end Parse_Primary_Expr;

   ---------------------
   --  Parse_Literal  --
   ---------------------
   procedure Parse_Literal (Result : out Node_Id;
                            Success : out Boolean;
                            Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Literal: enter"));
      case Get_Token is
         when T_Lit_Decimal_Integer
              | T_Lit_Octal_Integer
              | T_Lit_Hexa_Integer =>
            pragma Debug (O ("Parse_Literal : literal is an integer"));
            declare
               Res : Node_Id;
            begin
               Parse_Integer_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_String =>
            pragma Debug (O ("Parse_Literal : literal is a string"));
            declare
               Res : Node_Id;
            begin
               Parse_String_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_Wide_String =>
            pragma Debug (O ("Parse_Literal : literal is a wide string"));
            declare
               Res : Node_Id;
            begin
               Parse_Wide_String_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_Char =>
            declare
               Res : Node_Id;
            begin
               Parse_Char_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_Wide_Char =>
            declare
               Res : Node_Id;
            begin
               Parse_Wide_Char_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_Simple_Floating_Point
           | T_Lit_Exponent_Floating_Point
           | T_Lit_Pure_Exponent_Floating_Point =>
            declare
               Res : Node_Id;
            begin
               Parse_Floating_Pt_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_Lit_Simple_Fixed_Point
           | T_Lit_Floating_Fixed_Point =>
            declare
               Res : Node_Id;
            begin
               Parse_Fixed_Pt_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when T_True
           | T_False =>
            declare
               Res : Node_Id;
            begin
               Parse_Boolean_Literal (Res, Success, Expr_Type);
               Result := Res;
            end;
         when others =>
            raise Errors.Internal_Error;
      end case;
      pragma Debug (O2 ("Parse_Literal: end"));
   end Parse_Literal;

   -----------------------------
   --  Parse_Boolean_Literal  --
   -----------------------------
   procedure Parse_Boolean_Literal (Result : out Node_Id;
                                    Success : out Boolean;
                                    Expr_Type : in Constant_Value_Ptr) is
   begin
      Result := Make_Lit_Boolean (Get_Token_Location);
      if Expr_Type.Kind = C_Boolean then
         if Get_Token = T_True then
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_Boolean));
            Expr_Value (Result).Boolean_Value := True;
         else
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_Boolean));
            Expr_Value (Result).Boolean_Value := False;
         end if;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Boolean_Literal;

   --------------------------------
   --  Parse_Positive_Int_Const  --
   --------------------------------
   procedure Parse_Positive_Int_Const (Result : out Node_Id;
                                       Success : out Boolean) is
      C_Type : Constant_Value_Ptr
        := new Constant_Value (Kind => C_General_Integer);
   begin
      --  here we can not call parse_const_exp directly since we
      --  don't have a node specifying the type of the constant
      --  So, we call parse_or_exp and check the result
      Parse_Or_Expr (Result, Success, C_Type);
      if Expr_Value (Result).Integer_Value < Idl_ULongLong_Min then
         Errors.Error
           ("The specified type for this integer constant " &
            "does not allow a negative value",
            Errors.Error,
            Get_Token_Location);
      end if;
      if Expr_Value (Result).Integer_Value > Idl_ULongLong_Max then
         Errors.Error
           ("The specified type for this integer constant " &
            "does not allow this value",
            Errors.Error,
            Get_Token_Location);
      end if;
      Free (C_Type);
   end Parse_Positive_Int_Const;

   --------------------
   -- Parse_Type_Dcl --
   --------------------

   procedure Parse_Type_Dcl
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Type_Dcl: enter"));
      Result := No_Node;
      Success := False;

      case Get_Token is

         when T_Typedef =>
            Next_Token;
            declare
               Res : Node_Id;
            begin
               Parse_Type_Declarator (Res, Success);
               Result := Res;
            end;

         when T_Struct =>
            declare
               Res : Node_Id;
            begin
               Parse_Struct_Type (Res, Success);
               Result := Res;
            end;

         when T_Union =>
            declare
               Res : Node_Id;
            begin
               Parse_Union_Type (Res, Success);
               Result := Res;
            end;

         when T_Enum =>
            declare
               Res : Node_Id;
            begin
               Parse_Enum_Type (Res, Success);
               Result := Res;
            end;

         when T_Native =>
            declare
               Res : Node_Id;
            begin
               Res := Make_Native (Get_Token_Location);
               Next_Token;
               declare
                  Node : Node_Id;
               begin
                  Node := Declarator (Res);
                  Parse_Simple_Declarator
                    (Node, Res, Success);
                  Set_Declarator (Res, Node);
               end;

               if not Success then
                  Result := No_Node;
                  return;
               end if;
               Result :=  Res;
            end;
         when others =>
            raise Errors.Internal_Error;
      end case;

      pragma Debug (O2 ("Parse_Type_Dcl: end"));
      return;
   end Parse_Type_Dcl;

   ---------------------------
   -- Parse_Type_Declarator --
   ---------------------------

   procedure Parse_Type_Declarator
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Type_declarator: enter"));

      Result := Make_Type_Declarator (Get_Token_Location);

      declare
         Node : Node_Id;
      begin
         Node := T_Type (Result);
         Parse_Type_Spec (Node, Success);
         Set_T_Type (Result, Node);
      end;

      if not Success then
         pragma Debug (O ("Parse_Type_Declarator: type_spec return false"));
         pragma Debug (O2 ("Parse_Type_declarator: end"));
         return;
      end if;

      declare
         Node : Node_List;
      begin
         Node := Declarators (Result);
         Parse_Declarators
           (Node, Result, Success);
         Set_Declarators (Result, Node);
      end;

      pragma Debug (O2 ("Parse_Type_declarator: end"));
      return;
   end Parse_Type_Declarator;

   ---------------------
   -- Parse_Type_Spec --
   ---------------------

   procedure Parse_Type_Spec
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Type_Spec: enter"));

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
            Errors.Error ("type specification expected.",
                          Errors.Error,
                          Get_Token_Location);
            Success := False;
            Result := No_Node;
      end case;

      pragma Debug (O2 ("Parse_Type_Spec: end"));
      return;
   end Parse_Type_Spec;

   ----------------------------
   -- Parse_Simple_Type_Spec --
   ----------------------------

   procedure Parse_Simple_Type_Spec
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Simple_Type_Spec: enter"));
      pragma Debug (O ("Parse_Simple_Type_Spec: token is " &
                       Idl_Token'Image (Get_Token)));

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

            Parse_Scoped_Name (Result, Success);

            if not Success then
               Result := No_Node;
            end if;

            if Result /= No_Node then
               declare
                  Not_A_Type : Boolean := False;
               begin

                  --  Check that the scoped name denotes a type

                  pragma Debug (O ("Parse_Simple_Type_Spec: " &
                                   "kind of result is " &
                                   Img (Kind (Result))));
                  if S_Type (Result) /= No_Node then

                     pragma Debug (O ("Parse_Simple_Type_Spec: " &
                                      "scoped name without an S_Type"));
                     case Kind (S_Type (Result)) is
                        when K_Float
                          | K_Double
                          | K_Long_Double
                          | K_Long
                          | K_Long_Long
                          | K_Short
                          | K_Unsigned_Long
                          | K_Unsigned_Long_Long
                          | K_Unsigned_Short
                          | K_Char
                          | K_Wide_Char
                          | K_Boolean
                          | K_Octet
                          | K_Any
                          | K_Object
                          | K_ValueBase
                          | K_Sequence
                          | K_String
                          | K_Wide_String
                          | K_Fixed
                          | K_Enum
                          | K_Struct
                          | K_Union
                          | K_Interface
                          | K_Forward_Interface
                          | K_ValueType
                          | K_Boxed_ValueType
                          | K_Forward_ValueType =>
                           null;
                        when others =>
                           Not_A_Type := True;
                     end case;

                  else
                     Not_A_Type := True;
                  end if;

                  if Not_A_Type then
                     Errors.Error
                       ("This scoped name does not denote an "
                        & " acceptable type for a Simple_Type_Spec.",
                        Errors.Error,
                        Get_Token_Location);
                  end if;

               end;
            end if;
         when T_Enum
           | T_Struct
           | T_Union =>
            Errors.Error ("simple type specification " &
                          "expected. No constructed " &
                          "type allowed here.",
                          Errors.Error,
                          Get_Token_Location);
            Parse_Constr_Type_Spec (Result, Success);
         when others =>
            Errors.Error ("simple type specification expected.",
                          Errors.Error,
                          Get_Token_Location);
            Result := No_Node;
            Success := False;
      end case;
      pragma Debug (O2 ("Parse_Simple_Type_Spec: end"));
      return;
   end Parse_Simple_Type_Spec;

   --------------------------
   -- Parse_Base_Type_Spec --
   --------------------------

   procedure Parse_Base_Type_Spec
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Base_Type_Spec: enter"));

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
               Res : Node_Id;
            begin
               Parse_Char_Type (Res, Success);
               Result := Res;
            end;

         when T_Wchar =>

            declare
               Res : Node_Id;
            begin
               Parse_Wide_Char_Type (Res, Success);
               Result := Res;
            end;

         when T_Boolean =>

            declare
               Res : Node_Id;
            begin
               Parse_Boolean_Type (Res, Success);
               Result := Res;
            end;

         when T_Octet =>

            declare
               Res : Node_Id;
            begin
               Parse_Octet_Type (Res, Success);
               Result := Res;
            end;

         when T_Any =>

            declare
               Res : Node_Id;
            begin
               Parse_Any_Type (Res, Success);
               Result := Res;
            end;

         when T_Object =>

            declare
               Res : Node_Id;
            begin
               Parse_Object_Type (Res, Success);
               Result := Res;
            end;

         when T_ValueBase =>

            declare
               Res : Node_Id;
            begin
               Parse_Value_Base_Type (Res, Success);
               Result := Res;
            end;

         when others =>
            raise Errors.Internal_Error;

      end case;
      pragma Debug (O2 ("Parse_Base_Type_Spec: end"));
      return;
   end Parse_Base_Type_Spec;

   ------------------------------
   -- Parse_Template_Type_Spec --
   ------------------------------

   procedure Parse_Template_Type_Spec
     (Result : out Node_Id;
      Success : out Boolean) is
   begin

      case Get_Token is
         when T_Sequence =>

            declare
               Res : Node_Id;
            begin
               Parse_Sequence_Type (Res, Success);
               Result := Res;
            end;

         when T_String =>

            declare
               Res : Node_Id;
            begin
               Parse_String_Type (Res, Success);
               Result := Res;
            end;

         when T_Wstring =>

            declare
               Res : Node_Id;
            begin
               Parse_Wide_String_Type (Res, Success);
               Result := Res;
            end;

         when T_Fixed =>

            declare
               Res : Node_Id;
            begin
               Parse_Fixed_Pt_Type (Res, Success);
               Result := Res;
            end;

         when others =>
            raise Errors.Internal_Error;
      end case;

   end Parse_Template_Type_Spec;

   ----------------------------
   -- Parse_Constr_Type_Spec --
   ----------------------------

   procedure Parse_Constr_Type_Spec
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      case Get_Token is

         when T_Struct =>

            declare
               Res : Node_Id;
            begin
               Parse_Struct_Type (Res, Success);
               Result := Res;
            end;

         when T_Union =>

            declare
               Res : Node_Id;
            begin
               Parse_Union_Type (Res, Success);
               Result := Res;
            end;

         when T_Enum =>

            declare
               Res : Node_Id;
            begin
               Parse_Enum_Type (Res, Success);
               Result := Res;
            end;

         when others =>
            raise Errors.Internal_Error;
      end case;
   end Parse_Constr_Type_Spec;

   -----------------------
   -- Parse_Declarators --
   -----------------------

   procedure Parse_Declarators
     (Result : out Node_List;
      Parent : in Node_Id;
      Success : out Boolean) is
   begin
      Result := Nil_List;
      declare
         Res : Node_Id;
      begin
         Parse_Declarator (Res, Parent, Success);
         if not Success then
            pragma Debug (O ("Parse_Declarators : first success = false"));
            return;
         else
            Append_Node (Result, Res);
         end if;
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Res : Node_Id;
         begin
            Parse_Declarator (Res, Parent, Success);
            if not Success then
               return;
            else
               Append_Node (Result, Res);
            end if;
         end;
      end loop;
      return;
   end Parse_Declarators;

   ------------------------
   --  Parse_Declarator  --
   ------------------------
   procedure Parse_Declarator (Result : out Node_Id;
                               Parent : in Node_Id;
                               Success : out Boolean) is
   begin
      pragma Debug (O2 ("parse_declarator: enter"));
      if Get_Token /= T_Identifier then
         Errors.Error
           ("Identifier expected in declarator.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         Result := No_Node;
         return;
      else
         if View_Next_Token = T_Left_Sbracket then
            pragma Debug (O ("Parse_Declarator : Array"));
            Parse_Complex_Declarator (Result, Parent, Success);
         else
            pragma Debug (O ("Parse_Declarator : Simple"));
            Parse_Simple_Declarator (Result, Parent, Success);
         end if;
      end if;
      pragma Debug (O2 ("parse_declarator: end"));
      return;
   end Parse_Declarator;

   -------------------------------
   --  Parse_Simple_Declarator  --
   -------------------------------
   procedure Parse_Simple_Declarator (Result : out Node_Id;
                                      Parent : in Node_Id;
                                      Success : out Boolean) is
   begin
      if Get_Token /= T_Identifier then
         Errors.Error
           ("Identifier expected in simple declarator.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         pragma Debug (O ("Parse_Simple_Declarator : the scope is " &
                          Img (Kind (Get_Current_Scope))));
         --  Is there a previous definition
         if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
            declare
               Definition : constant Identifier_Definition_Acc
                 := Find_Identifier_Definition
                 (Get_Token_String, Get_Lexer_Location);
            begin
               Errors.Error
                 ("This identifier is already defined in this scope : " &
                  Errors.Location_To_String
                  (Get_Location (Definition.Node)),
                  Errors.Error,
                  Get_Token_Location);
            end;
         end if;
         Result := Make_Declarator (Get_Token_Location);
         --  no previous definition
         if Add_Identifier (Result,
                            Get_Token_String) then
            Set_Default_Repository_Id (Result);
         end if;
         Set_Array_Bounds (Result, Nil_List);
         Set_Parent (Result, Parent);
      end if;
      Success := True;
      Next_Token;
      return;
   end Parse_Simple_Declarator;

   --------------------------------
   --  Parse_Complex_Declarator  --
   --------------------------------
   procedure Parse_Complex_Declarator (Result : out Node_Id;
                                       Parent : in Node_Id;
                                       Success : out Boolean)
     renames Parse_Array_Declarator;

   ------------------------------
   --  Parse_Floating_Pt_Type  --
   ------------------------------
   procedure Parse_Floating_Pt_Type (Result : in out Node_Id;
                                     Success : out Boolean) is
   begin
      case Get_Token is
         when T_Float =>
            Next_Token;
            Result := Make_Float (Get_Token_Location);
         when T_Double =>
            Next_Token;
            Result := Make_Double (Get_Token_Location);
         when T_Long =>
            Next_Token;
            Next_Token;
            Result := Make_Long_Double (Get_Token_Location);
         when others =>
               raise Errors.Internal_Error;
      end case;
      Success := True;
      return;
   end Parse_Floating_Pt_Type;

   --------------------------
   --  Parse_Integer_Type  --
   --------------------------
   procedure Parse_Integer_Type (Result : in out Node_Id;
                                 Success : out Boolean) is
   begin
      case Get_Token is
         when T_Long
           | T_Short =>
            Parse_Signed_Int (Result, Success);
         when T_Unsigned =>
            Parse_Unsigned_Int (Result, Success);
         when others =>
            raise Errors.Internal_Error;
      end case;
   end Parse_Integer_Type;

   ------------------------
   --  Parse_Signed_Int  --
   ------------------------
   procedure Parse_Signed_Int (Result : in out Node_Id;
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
            raise Errors.Internal_Error;
      end case;
   end Parse_Signed_Int;

   ------------------------------
   --  Parse_Signed_Short_Int  --
   ------------------------------
   procedure Parse_Signed_Short_Int (Result : in out Node_Id;
                                     Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Short (Get_Token_Location);
      Success := True;
   end Parse_Signed_Short_Int;

   -----------------------------
   --  Parse_Signed_Long_Int  --
   -----------------------------
   procedure Parse_Signed_Long_Int (Result : in out Node_Id;
                                    Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Long (Get_Token_Location);
      Success := True;
   end Parse_Signed_Long_Int;

   ---------------------------------
   --  Parse_Signed_Longlong_Int  --
   ---------------------------------
   procedure Parse_Signed_Longlong_Int (Result : in out Node_Id;
                                        Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := Make_Long_Long (Get_Token_Location);
      Success := True;
   end Parse_Signed_Longlong_Int;

   --------------------------
   --  Parse_Unsigned_Int  --
   --------------------------
   procedure Parse_Unsigned_Int (Result : in out Node_Id;
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
               Loc : Errors.Location;
            begin
               Loc := Get_Previous_Token_Location;
               Loc.Col := Loc.Col + 9;
               Errors.Error (Ada.Characters.Latin_1.Quotation &
                                    "short" &
                                    Ada.Characters.Latin_1.Quotation &
                                    " or " &
                                    Ada.Characters.Latin_1.Quotation &
                                    "long" &
                                    Ada.Characters.Latin_1.Quotation &
                                    " expected after unsigned.",
                                    Errors.Error,
                                    Loc);
               Success := False;
               Result := No_Node;
               return;
            end;
      end case;
   end Parse_Unsigned_Int;

   --------------------------------
   --  Parse_Unsigned_Short_Int  --
   --------------------------------
   procedure Parse_Unsigned_Short_Int (Result : in out Node_Id;
                                       Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := Make_Unsigned_Short (Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Short_Int;

   -------------------------------
   --  Parse_Unsigned_Long_Int  --
   -------------------------------
   procedure Parse_Unsigned_Long_Int (Result : in out Node_Id;
                                      Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Result := Make_Unsigned_Long (Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Long_Int;

   -----------------------------------
   --  Parse_Unsigned_Longlong_Int  --
   -----------------------------------
   procedure Parse_Unsigned_Longlong_Int (Result : in out Node_Id;
                                          Success : out Boolean) is
   begin
      Next_Token;
      Next_Token;
      Next_Token;
      Result := Make_Unsigned_Long_Long (Get_Token_Location);
      Success := True;
   end Parse_Unsigned_Longlong_Int;

   -----------------------
   --  Parse_Char_Type  --
   -----------------------
   procedure Parse_Char_Type (Result : in out Node_Id;
                              Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Char (Get_Token_Location);
      Success := True;
   end Parse_Char_Type;

   ----------------------------
   --  Parse_Wide_Char_Type  --
   ----------------------------
   procedure Parse_Wide_Char_Type (Result : in out Node_Id;
                                   Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Wide_Char (Get_Token_Location);
      Success := True;
   end Parse_Wide_Char_Type;

   --------------------------
   --  Parse_Boolean_Type  --
   --------------------------
   procedure Parse_Boolean_Type (Result : in out Node_Id;
                                 Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Boolean (Get_Token_Location);
      Success := True;
   end Parse_Boolean_Type;

   ------------------------
   --  Parse_Octet_Type  --
   ------------------------
   procedure Parse_Octet_Type (Result : in out Node_Id;
                               Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Octet (Get_Token_Location);
      Success := True;
   end Parse_Octet_Type;

   ----------------------
   --  Parse_Any_Type  --
   ----------------------
   procedure Parse_Any_Type (Result : in out Node_Id;
                             Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Any (Get_Token_Location);
      Success := True;
   end Parse_Any_Type;

   -------------------------
   --  Parse_Object_Type  --
   -------------------------
   procedure Parse_Object_Type (Result : in out Node_Id;
                                Success : out Boolean) is
   begin
      Result := Make_Object (Get_Token_Location);
      Success := True;
      Next_Token;
      return;
   end Parse_Object_Type;

   -------------------------
   --  Parse_Struct_Type  --
   -------------------------
   procedure Parse_Struct_Type (Result : out Node_Id;
                                Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Struct_Type: enter"));
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Errors.Error
              ("identifier expected in struct declaration.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;
      --  Is there a previous definition
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         declare
            Definition : constant Identifier_Definition_Acc
              := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("This identifier is already defined in this scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end;
      end if;
      Result := Make_Struct (Get_Token_Location);
      Set_Is_Exception_Members (Result, False);

      if Add_Identifier (Result, Get_Token_String) then
         Set_Default_Repository_Id (Result);
         Set_Initial_Current_Prefix (Result);
      end if;

      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("'{' expected in struct definition.",
               Errors.Error,
               Loc);
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      Push_Scope (Result);
      declare
         Node : Node_List;
      begin
         Node := Members (Result);
         Parse_Member_List (Node,
                            Success);
         Set_Members (Result, Node);
      end;
      Pop_Scope;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Right_Cbracket then
         Errors.Error
           ("'}' expected at the end of struct definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      pragma Debug (O2 ("Parse_Struct_Type: end"));
      return;
   end Parse_Struct_Type;


   -------------------------
   --  Parse_Member_List  --
   -------------------------
   procedure Parse_Member_List (Result : out Node_List;
                                Success : out Boolean) is
      Empty : Boolean := True;
   begin
      pragma Debug (O2 ("Parse_Member_List: enter"));
      Result := Nil_List;
      loop
         declare
            Member : Node_Id;
            Member_Success : Boolean;
         begin
            Parse_Member (Member, Member_Success);
            if not Member_Success then
               Go_To_Next_Member;
            else
               if Kind (Member) /= K_Pragma then
                  Empty := False;
               end if;
               Append_Node (Result, Member);
            end if;
         end;
         exit when Get_Token = T_Right_Cbracket or else Get_Token = T_Eof;
      end loop;
      if Empty then
         Errors.Error
           ("member expected : a struct may not be empty.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Success := True;
      pragma Debug (O2 ("Parse_Member_List: end"));
      return;
   end Parse_Member_List;

   --------------------
   --  Parse_Member  --
   --------------------
   procedure Parse_Member (Result : out Node_Id;
                           Success : out Boolean) is
      Type_Spec : Node_Id;
      Loc : Errors.Location;
   begin
      pragma Debug (O2 ("Parse_Member: enter"));
      if Get_Token = T_Pragma then
         Parse_Pragma (Result, Success);
         if not Success then
            --  here the pragma is ignored and no node created
            --  so we parse the next member (if it exists)
            Parse_Member (Result, Success);
         end if;
         return;
      end if;
      if Get_Token = T_Right_Cbracket then
         --  here, two situation possible :
         --  either we just parsed a pragma but it was the last member of the
         --  struct or the struct is empty.
         --  In both case, we return without creating a node
         Success := False;
         Result := No_Node;
         return;
      end if;
      Loc := Get_Token_Location;
      Parse_Type_Spec (Type_Spec, Success);
      if not Success then
         return;
      end if;
      Result := Make_Member (Loc);
      Set_M_Type (Result, Type_Spec);
      declare
         Node : Node_List;
      begin
         Node := Decl (Result);
         Parse_Declarators (Node,
                            Result,
                            Success);
         Set_Decl (Result, Node);
      end;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Errors.Error
           ("';' expected at the end of member declaration.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      --  to eat the semi-colon
      Next_Token;
      pragma Debug (O2 ("Parse_Member: end"));
      return;
   end Parse_Member;

   ------------------------
   --  Parse_Union_Type  --
   ------------------------
   procedure Parse_Union_Type (Result : out Node_Id;
                               Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Union_Type: enter"));
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 6;
            Errors.Error
              ("identifier expected in union definition.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;
      --  Is there a previous definition
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         declare
            Definition : constant Identifier_Definition_Acc
              := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("This identifier is already defined in this scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end;
      end if;
      Result := Make_Union (Get_Token_Location);
      if not Add_Identifier (Result, Get_Token_String) then
         --  the error was raised before
         Success := False;
      else
         Set_Default_Repository_Id (Result);
         Set_Initial_Current_Prefix (Result);
      end if;

      Next_Token;
      if Get_Token /= T_Switch then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("switch expected in union definition.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Paren then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 2;
            Errors.Error
              ("'(' expected after " &
               Ada.Characters.Latin_1.Quotation &
               "switch" &
               Ada.Characters.Latin_1.Quotation &
               ".",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;
      Next_Token;
      Push_Scope (Result);
      declare
         Node : Node_Id;
      begin
         Parse_Switch_Type_Spec (Node,
                                 Success);
         Set_Switch_Type (Result, Node);
      end;
      if not Success then
         Pop_Scope;
         return;
      end if;
      if Get_Token /= T_Right_Paren then
         Errors.Error
           ("')' expected at the end of switch " &
            "specification.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         Pop_Scope;
         return;
      end if;
      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 2;
            Errors.Error
              ("'{' expected at the beginning of union.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            Pop_Scope;
            return;
         end;
      end if;
      Next_Token;
      declare
         Node : Node_List;
         Default_Index : Long_Integer;
      begin
         Node := Cases (Result);
         Parse_Switch_Body (Node,
                            Switch_Type (Result),
                            Default_Index,
                            Success);
         Set_Cases (Result, Node);
         Set_Default_Index (Result, Default_Index);
      end;
      Pop_Scope;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Right_Cbracket then
            Errors.Error
              ("'}' expected at the end of union.",
               Errors.Error,
               Get_Token_Location);
            Result := No_Node;
            Success := False;
            return;
      end if;
      Next_Token;
      return;
      pragma Debug (O2 ("Parse_Union_Type: end"));
   end Parse_Union_Type;

   ------------------------------
   --  Parse_Switch_Type_Spec  --
   ------------------------------
   procedure Parse_Switch_Type_Spec (Result : out Node_Id;
                                     Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Switch_Type_Spec: enter"));
      case Get_Token is
         when T_Long
           | T_Short
           | T_Unsigned =>
            Parse_Integer_Type (Result, Success);
         when T_Char =>
            Parse_Char_Type (Result, Success);
         when T_Boolean =>
            Parse_Boolean_Type (Result, Success);
         when T_Enum =>
            Parse_Enum_Type (Result, Success);
         when T_Colon_Colon
           | T_Identifier =>
            Parse_Scoped_Name (Result, Success);
            --  The <scoped_name> in the <switch_type_spec> production
            --  must be a previously defined integer, char, boolean
            --  or enum type.
            if not Success then
               Result := No_Node;
            end if;
            if Result /= No_Node then
               declare
                  Invalid_Type : Boolean := False;
               begin
                  if S_Type (Result) /= No_Node then
                     case Kind (S_Type (Result)) is
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
                           Invalid_Type := True;
                     end case;
                  else
                     Invalid_Type := True;
                  end if;
                  if Invalid_Type then
                     Errors.Error
                       ("Invalid type in switch. The " &
                        "scoped name should refer to " &
                        "an integer, char, boolean or " &
                        " enum type.",
                        Errors.Error,
                        Get_Token_Location);
                  end if;
               end;
            end if;
         when others =>
            Errors.Error
              ("switch type expected.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := No_Node;
      end case;
      pragma Debug (O2 ("Parse_Switch_Type_Spec: end"));
      return;
   end Parse_Switch_Type_Spec;

   -------------------------
   --  Parse_Switch_Body  --
   -------------------------
   procedure Parse_Switch_Body (Result : out Node_List;
                                Switch_Type : in Node_Id;
                                Default_Index : out Long_Integer;
                                Success : out Boolean) is
      Empty : Boolean := True;
      I : Long_Integer := -1;
   begin
      pragma Debug (O2 ("Parse_Switch_Body: enter"));
      Result := Nil_List;
      Default_Index := -1;
      loop
         declare
            Case_Clause : Node_Id;
            Case_Success : Boolean;
            Loc : Errors.Location;
         begin
            pragma Debug (O ("Parse_Switch_Body : new case clause"));
            Loc := Get_Token_Location;
            Parse_Case (Case_Clause,
                        Switch_Type,
                        Case_Success);
            if not Case_Success then
               Go_To_End_Of_Case;
            else
               I := I + 1;
               Append_Node (Result, Case_Clause);
               if Kind (Case_Clause) /= K_Pragma then
                  Empty := False;
                  if Default_Index /= -1 then
                     if Is_In_List (Labels (Case_Clause), No_Node) then
                        Errors.Error
                          ("default clause already appeared.",
                           Errors.Error,
                           Loc);
                     end if;
                  else
                     if Is_In_List (Labels (Case_Clause), No_Node) then
                        Default_Index := I;
                     end if;
                  end if;
               end if;
            end if;
         end;
         exit when Get_Token = T_Right_Cbracket or else Get_Token = T_Eof;
      end loop;
      if Empty then
         Errors.Error
           ("case clause expected : " &
            "a union may not be empty.",
            Errors.Error,
            Get_Token_Location);
      end if;
--      Release_All_Used_Values;
      Success := True;
      return;
      pragma Debug (O2 ("Parse_Switch_Body: end"));
   end Parse_Switch_Body;

   ------------------
   --  Parse_Case  --
   ------------------
   procedure Parse_Case (Result : out Node_Id;
                         Switch_Type : in Node_Id;
                         Success : out Boolean) is
      Default_Label : Boolean := False;
      Loc : Errors.Location;
   begin
      pragma Debug (O2 ("Parse_Case: enter"));
      Loc := Get_Token_Location;
      case Get_Token is
         when T_Case
           | T_Default =>
            null;
         when T_Pragma =>
            Parse_Pragma (Result, Success);
            if not Success then
               --  here the pragma is ignored and no node created
               --  so we parse the next case (if it exists)
               Parse_Case (Result, Switch_Type, Success);
            end  if;
            pragma Debug (O2 ("Parse_Case: end"));
            return;
         when T_Right_Cbracket =>
            --  here we just parsed a pragma but it was the last case of the
            --  union. Thus, we return without creating a node but
            --  without an error message
            Result := No_Node;
            Success := False;
            pragma Debug (O2 ("Parse_Case: end"));
            return;
         when others =>
            Errors.Error ("invalid case label : " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "case" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " or " &
                                 Ada.Characters.Latin_1.Quotation &
                                 "default" &
                                 Ada.Characters.Latin_1.Quotation &
                                 " expected.",
                                 Errors.Error,
                                 Get_Token_Location);
            Result := No_Node;
            Success := False;
            return;
      end case;
      pragma Debug (O ("Parse_case : first token ok"));
      Result := Make_Case (Get_Token_Location);
      Set_Labels (Result, Nil_List);
      while Get_Token = T_Case or else Get_Token = T_Default loop
         declare
            Case_Label : Node_Id;
            Case_Success : Boolean;
         begin
            Parse_Case_Label (Case_Label, Switch_Type, Case_Success);
            if not Case_Success then
               Go_To_End_Of_Case_Label;
            else
               Append_Node_To_Labels (Result, Case_Label);
               if Case_Label = No_Node then
                  Default_Label := True;
               end if;
            end if;
         end;
      end loop;
      if Default_Label and then Get_Length (Labels (Result)) > 1 then
         Errors.Error ("Some labels are use less since you " &
                                     "one of them is the default clause",
                                     Errors.Warning,
                                     Loc);
      end if;
      pragma Debug (O ("Parse_case : all label parsed"));
      declare
         Node1 : Node_Id;
         Node2 : Node_Id;
      begin
         Node1 := Case_Type (Result);
         Node2 := Case_Decl (Result);
         Parse_Element_Spec (Node1,
                             Node2,
                             Result,
                             Success);
         Set_Case_Type (Result, Node1);
         Set_Case_Decl (Result, Node2);
      end;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Semi_Colon then
         Errors.Error
           ("';' expected at the end of case clause.",
            Errors.Error,
            Get_Token_Location);
      else
         Next_Token;
      end if;
      pragma Debug (O2 ("Parse_Case: end"));
      return;
   end Parse_Case;

   ------------------------
   --  Parse_Case_Label  --
   ------------------------
   procedure Parse_Case_Label (Result : out Node_Id;
                               Switch_Type : in Node_Id;
                               Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_case_label: enter"));
      case Get_Token is
         when T_Case =>
            declare
               Loc : Errors.Location;
            begin
               Next_Token;
               Loc := Get_Token_Location;
               Parse_Const_Exp (Result, Switch_Type, Success);
               if not Success then
                  return;
               end if;
               --  Verifying that a clause does not appear twice
--                if not Add_Used_Value (Result) then
--                   Errors.Error
--                     ("This value was already taken into " &
--                      "account in this switch statement.",
--                      Errors.Warning,
--                      Loc);
--                end if;
            end;
         when T_Default =>
            Next_Token;
            Result := No_Node;
            Success := True;
         when others =>
            raise Errors.Internal_Error;
      end case;
      if Get_Token /= T_Colon then
         Errors.Error
           ("':' expected at the end of case label.",
            Errors.Error,
            Get_Token_Location);
      else
         Next_Token;
      end if;
      return;
      pragma Debug (O2 ("Parse_case_label: end"));
   end Parse_Case_Label;

   --------------------------
   --  Parse_Element_Spec  --
   --------------------------
   procedure Parse_Element_Spec (Element_Type : out Node_Id;
                                 Element_Decl : out Node_Id;
                                 Parent : in Node_Id;
                                 Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Element_Spec: enter"));
      Parse_Type_Spec (Element_Type, Success);
      if not Success then
         return;
      end if;
      Parse_Declarator (Element_Decl, Parent, Success);
      pragma Debug (O2 ("Parse_Element_Spec: end"));
      return;
   end Parse_Element_Spec;

   ---------------------
   -- Parse_Enum_Type --
   ---------------------

   procedure Parse_Enum_Type
     (Result : out Node_Id;
      Success : out Boolean) is
   begin
      Next_Token;
      if Get_Token /= T_Identifier then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 5;
            Errors.Error
              ("Identifier expected in enumeration " &
               "definition.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;

      Result := Make_Enum (Get_Token_Location);
      Set_Enumerators (Result, Nil_List);

      --  Is there a previous definition?

      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         declare
            Definition : constant Identifier_Definition_Acc
              := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("This identifier is already defined in this scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end;
         return;
      end if;

      if not Add_Identifier (Result, Get_Token_String) then
         raise Errors.Internal_Error;
      end if;
      Set_Default_Repository_Id (Result);

      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("'{' expected in enumeration definition.",
               Errors.Error,
               Loc);
            Result := No_Node;
            Success := False;
            return;
         end;
      end if;

      Next_Token;
      if Get_Token = T_Right_Cbracket then
         Errors.Error
           ("identifier expected : " &
            "an enumeration may not be empty.",
            Errors.Error,
            Get_Token_Location);
         Next_Token;
         return;
      end if;

      declare
         Count : Idl_Integer := 1;
      begin
         loop
            declare
               Enum : Node_Id;
            begin
               if Get_Token = T_Pragma then
                  Parse_Pragma (Enum, Success);
                  if Success then
                     Append_Node_To_Enumerators (Result, Enum);
                  end if;
               else
                  Parse_Enumerator (Enum, Success);

                  if not Success then
                     Go_To_End_Of_Enumeration;
                     return;
                  end if;
                  Count := Count + 1;
                  if Count = Idl_Enum_Max then
                     Errors.Error
                       ("two much possible values in this " &
                        "enumeration : maximum is 2^32.",
                        Errors.Error,
                        Get_Token_Location);
                  end if;
                  Append_Node_To_Enumerators (Result, Enum);

                  if Get_Token = T_Comma then
                     Next_Token;
                  elsif Get_Token /= T_Pragma then
                     exit;
                  end if;
               end if;
            end;
         end loop;
      end;

      if Get_Token /= T_Right_Cbracket then
         Errors.Error
           ("'}' expected at the end of enumeration " &
            "definition.",
            Errors.Error,
            Get_Token_Location);
         Go_To_Next_Right_Cbracket;
         if Get_Token = T_Right_Cbracket then
            Next_Token;
         end if;
         if Get_Token = T_Semi_Colon then
            Next_Token;
         end if;
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Enum_Type;

   ------------------------
   --  Parse_Enumerator  --
   ------------------------
   procedure Parse_Enumerator (Result : out Node_Id;
                               Success : out Boolean) is
   begin
      if Get_Token = T_Pragma then
         Parse_Pragma (Result, Success);
         if not Success then
            --  here the pragma is ignored and no node created
            --  so we parse the next enumerator (if it exists)
            Parse_Enumerator (Result, Success);
         end if;
         return;
      end if;
      if Get_Token = T_Right_Cbracket then
         --  here we just parsed a pragma but it was the last enumerator of the
         --  enum. Thus, we return without creating a node but
         --  without an error message
         Success := False;
         Result := No_Node;
         return;
      end if;

      if Get_Token /= T_Identifier then
         Errors.Error
           ("Identifier expected in enumerator.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         Result := Make_Enumerator (Get_Token_Location);
         --  Is there a previous definition
         if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
            declare
               Definition : constant Identifier_Definition_Acc
                 := Find_Identifier_Definition
                 (Get_Token_String, Get_Lexer_Location);
            begin
               Errors.Error
                 ("This identifier is already defined in this scope : " &
                  Errors.Location_To_String
                  (Get_Location (Definition.Node)),
                  Errors.Error,
                  Get_Token_Location);
            end;
         else
            --  no previous definition
            if not Add_Identifier (Result, Get_Token_String) then
               raise Errors.Internal_Error;
            end if;
            Set_Default_Repository_Id (Result);

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
   procedure Parse_Sequence_Type (Result : out Node_Id;
                                  Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Sequence_Type: enter"));
      Next_Token;
      if Get_Token /= T_Less then
         Errors.Error
           ("'<' expected in sequence definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Result := Make_Sequence (Get_Previous_Token_Location);
      pragma Debug (O ("Parse_Sequence_Type : previous location :" &
                       " filename = " &
                       Get_Previous_Token_Location.Filename.all));
      Next_Token;
      declare
         Node : Node_Id;
      begin
         Node := Sequence_Type (Result);
         Parse_Simple_Type_Spec (Node, Success);
         Set_Sequence_Type (Result, Node);
      end;
      if not Success then
         return;
      end if;
      pragma Debug (O ("Parse_Sequence_Type : Token is" &
                       Idl_Token'Image (Get_Token)));
      --  should divide the greater_greater token!
      if Get_Token = T_Greater_Greater then
         Errors.Error
           ("'>>' could be considered as a constant operation." &
            "You should better insert a space between the two '>'.",
            Errors.Warning,
            Get_Token_Location);
         Divide_T_Greater_Greater;
      end if;
      if Get_Token /= T_Comma and Get_Token /= T_Greater then
         Errors.Error
           ("',' or '>' expected in sequence definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      if Get_Token = T_Comma then
         Next_Token;
         declare
            Node : Node_Id;
         begin
            Node := Bound (Result);
            Parse_Positive_Int_Const (Node, Success);
            Set_Bound (Result, Node);
         end;
         if not Success then
            return;
         end if;
      else
         Set_Bound (Result, No_Node);
      end if;

      --  should divide the greater_greater token!
      if Get_Token = T_Greater_Greater then
         Errors.Error
           ("'>>' could be considered as a constant operation." &
            "You should better insert a space between the two '>'.",
            Errors.Warning,
            Get_Token_Location);
         Divide_T_Greater_Greater;
      end if;

      case Get_Token is
         when T_Greater =>
            Next_Token;
         when others =>
            Errors.Error
              ("'>' expected at the end of "
               & "sequence definition.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
      end case;
      pragma Debug (O2 ("Parse_Sequence_Type: end"));
      return;
   end Parse_Sequence_Type;

   -------------------------
   --  Parse_String_Type  --
   -------------------------
   procedure Parse_String_Type (Result : out Node_Id;
                                Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_String (Get_Previous_Token_Location);
      if Get_Token = T_Less then
         declare
            Node : Node_Id;
         begin
            Next_Token;
            Node := Bound (Result);
            Parse_Positive_Int_Const (Node, Success);
            Set_Bound (Result, Node);
         end;
         if not Success then
            return;
         end if;
         if Get_Token /= T_Greater then
            Errors.Error
              ("'>' expected in string definition.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
         end if;
         Next_Token;
      else
         Set_Bound (Result, No_Node);
      end if;
      Success := True;
      return;
   end Parse_String_Type;

   ------------------------------
   --  Parse_Wide_String_Type  --
   ------------------------------
   procedure Parse_Wide_String_Type (Result : out Node_Id;
                                     Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Wide_String (Get_Previous_Token_Location);
      if Get_Token = T_Less then
         declare
            Node : Node_Id;
         begin
            Next_Token;
            Node := Bound (Result);
            Parse_Positive_Int_Const (Node, Success);
            Set_Bound (Result, Node);
         end;
         if not Success then
            return;
         end if;
         if Get_Token /= T_Greater then
            Errors.Error
              ("'>' expected in wide string definition.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
         end if;
         Next_Token;
      else
         Set_Bound (Result, No_Node);
      end if;
      Success := True;
      return;
   end Parse_Wide_String_Type;

   ------------------------------
   --  Parse_Array_Declarator  --
   ------------------------------
   procedure Parse_Array_Declarator (Result : out Node_Id;
                                     Parent : in Node_Id;
                                     Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Array_Declarator: enter"));
      Result := Make_Declarator (Get_Token_Location);
      Set_Parent (Result, Parent);
      --  Is there a previous definition
      if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
         declare
            Definition : constant Identifier_Definition_Acc
              := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("This identifier is already defined in this scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
         end;
      end if;
      --  if any previous definition, just ignore the identifier
      --  but keep parsing parsing (no syntax error)
      if Add_Identifier (Result,
                         Get_Token_String) then
         Set_Default_Repository_Id (Result);
         Set_Array_Bounds (Result, Nil_List);
      end if;

      --  consumes the identifier
      Next_Token;
      while Get_Token = T_Left_Sbracket loop
         declare
            Expr : Node_Id;
         begin
            Parse_Fixed_Array_Size (Expr, Success);
            if not Success then
               pragma Debug (O ("Parse_Array_Declarator : " &
                                "Parse_Fixed_Array_Size returned false"));
               pragma Debug (O2 ("Parse_Array_Declarator: end"));
               return;
            end if;
            Append_Node_To_Array_Bounds (Result, Expr);
         end;
      end loop;
      pragma Debug (O2 ("Parse_Array_Declarator: end"));
      return;
   end Parse_Array_Declarator;

   ------------------------------
   --  Parse_Fixed_Array_Size  --
   ------------------------------
   procedure Parse_Fixed_Array_Size (Result : out Node_Id;
                                     Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Fixed_Array_Size: enter"));
      Next_Token;
      Parse_Positive_Int_Const (Result, Success);
      if not Success then
         pragma Debug (O ("Parse_fixed_array_size : "&
                          "Parse_positive_int_const returned false"));
         return;
      end if;
      if Get_Token /= T_Right_Sbracket then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("']' expected in array definition.",
               Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      pragma Debug (O2 ("Parse_Fixed_Array_Size: end"));
      return;
   end Parse_Fixed_Array_Size;

   ---------------------
   --  Parse_Attr_Dcl --
   ---------------------
   procedure Parse_Attr_Dcl (Result : out Node_Id;
                             Success : out Boolean) is
      El : Node_Id;
   begin
      El := Make_Attribute (Get_Token_Location);
      if Get_Token = T_Readonly then
         Set_Is_Readonly (El, True);
         Next_Token;
      else
         Set_Is_Readonly (El, False);
      end if;
      if Get_Token /= T_Attribute then
         Errors.Error
           ("'attribute' expected",
            Errors.Error,
            Get_Token_Location);
         Result := No_Node;
         Success := False;
         return;
      end if;
      Next_Token;
      pragma Debug (O ("Parse_Attr_dcl :" &
                       Idl_Token'Image (Get_Token)));
      declare
         Node : Node_Id;
      begin
         Node := A_Type (El);
         Parse_Param_Type_Spec (Node, Success);
         Set_A_Type (El, Node);
      end;
      if not Success then
         Result := No_Node;
         return;
      end if;
      if Get_Token /= T_Identifier then
         Errors.Error
           ("identifier expected",
            Errors.Error,
            Get_Token_Location);
         Result := No_Node;
         Success := False;
         return;
      end if;
      declare
         Res : Node_Id;
      begin
         Parse_Declarator (Res, El, Success);
         if not Success then
            Result := No_Node;
            Success := False;
            return;
         else
            Append_Node_To_Declarators (El, Res);
         end if;
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Res : Node_Id;
         begin
            Parse_Declarator (Res, El, Success);
            if not Success then
               Result := No_Node;
               Success := False;
               return;
            else
               Append_Node_To_Declarators (El, Res);
            end if;
         end;
      end loop;
      Result := El;
   end Parse_Attr_Dcl;

   ------------------------
   --  Parse_Except_Dcl  --
   ------------------------
   procedure Parse_Except_Dcl (Result : out Node_Id;
                               Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Except_Dcl: enter"));
      pragma Debug (O ("Parse_Except_Dcl : first token " &
                       Idl_Token'Image (Get_Token)));
      if Get_Token /= T_Exception then
         Errors.Error
           ("'exception' expected",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         Result := No_Node;
         return;
      end if;
      Result := Make_Exception (Get_Token_Location);
      --  memory leak
      Next_Token;
      if Get_Token /= T_Identifier then
         Errors.Error
           ("identifier expected",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      pragma Debug (O ("Parse_Except_Dcl : token before add : " &
                       Idl_Token'Image (Get_Token)));
      if not Add_Identifier (Result, Get_Token_String) then
         declare
            Definition : constant Identifier_Definition_Acc
              := Find_Identifier_Definition
              (Get_Token_String, Get_Lexer_Location);
         begin
            Errors.Error
              ("This identifier is already defined in this scope : " &
               Errors.Location_To_String
               (Get_Location (Definition.Node)),
               Errors.Error,
               Get_Token_Location);
            Success := False;
            return;
         end;
      end if;
      Set_Default_Repository_Id (Result);
      Set_Initial_Current_Prefix (Result);

      pragma Debug (O ("Parse_Except_Dcl : token after add : " &
                       Idl_Token'Image (Get_Token)));
      Next_Token;
      if Get_Token /= T_Left_Cbracket then
         Errors.Error
           ("'{' expected",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         Next_Token;
      end if;
      Push_Scope (Result);
      while Get_Token /= T_Right_Cbracket loop
         declare
            Mem : Node_Id;
            Mem_Success : Boolean;
         begin
            Parse_Member (Mem, Mem_Success);
            if not Mem_Success then
               Go_To_Next_Member;
               if Get_Token = T_Eof then
                  Success := False;
                  return;
               end if;
            else
               Append_Node_To_Members (Result, Mem);
            end if;
         end;
      end loop;
      Pop_Scope;
      --  to eat the right bracket
      Next_Token;
      Success := True;
      return;
      pragma Debug (O2 ("Parse_Except_Dcl: end"));
   end Parse_Except_Dcl;

   --------------------
   --  parse_op_dcl  --
   --------------------
   procedure Parse_Op_Dcl (Result : out Node_Id;
                           Success : out Boolean) is
   begin
      Result := Make_Operation (Get_Token_Location);
      Set_Initial_Current_Prefix (Result);
      if Get_Token = T_Oneway then
         Set_Is_Oneway (Result, True);
         Next_Token;
      else
         Set_Is_Oneway (Result, False);
      end if;
      declare
         Node : Node_Id;
      begin
         Node := Operation_Type (Result);
         Parse_Op_Type_Spec (Node, Success);
         Set_Operation_Type (Result, Node);
      end;
      if not Success then
         return;
      end if;
      if Get_Token /= T_Identifier then
         Errors.Error
           ("Identifier expected in operation declaration.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      else
         --  Is there a previous definition
         if not Is_Redefinable (Get_Token_String, Get_Lexer_Location) then
            declare
               Definition : constant Identifier_Definition_Acc
                 := Find_Identifier_Definition
                 (Get_Token_String, Get_Lexer_Location);
            begin
               Errors.Error
                 ("This identifier is already defined in this scope: " &
                  Errors.Location_To_String
                  (Get_Location (Definition.Node)),
                  Errors.Error,
                  Get_Token_Location);
               pragma Debug (O ("Parse_Op_Dcl: bad identifier"));
               Result := No_Node;
               Success := False;
               return;
            end;
         else
            --  no previous definition
            if not Add_Identifier (Result,
                                   Get_Token_String) then
                  raise Errors.Internal_Error;
            end if;
            Set_Default_Repository_Id (Result);

         end if;
      end if;
      Next_Token;
      Push_Scope (Result);
      declare
         Node : Node_List;
      begin
         Node := Parameters (Result);
         Parse_Parameter_Dcls (Node, Success);
         Set_Parameters (Result, Node);
      end;
      Pop_Scope;
      if not Success then
         return;
      end if;
      if Get_Token = T_Raises then
         declare
            Node : Node_List;
         begin
            Node := Raises (Result);
            Parse_Raises_Expr (Node, Success);
            Set_Raises (Result, Node);
         end;
         if not Success then
            return;
         end if;
      else
         Set_Raises (Result, Nil_List);
      end if;
      if Get_Token = T_Context then
         declare
            Node : Node_List;
         begin
            Node := Contexts (Result);
            Parse_Context_Expr (Node, Success);
            Set_Contexts (Result, Node);
         end;
      else
         Set_Contexts (Result, Nil_List);
      end if;
      return;
   end Parse_Op_Dcl;

   --------------------------
   --  Parse_Op_Type_Spec  --
   --------------------------
   procedure Parse_Op_Type_Spec (Result : out Node_Id;
                                 Success : out Boolean) is
   begin
      case Get_Token is
         when T_Void =>
            Result := Make_Void (Get_Token_Location);
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
           | T_Wstring
           | T_Colon_Colon
           | T_Identifier =>
            Parse_Param_Type_Spec (Result, Success);
            return;
         when others =>
            Errors.Error
              ("void or type specification expected.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
            Result := No_Node;
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
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + Get_Previous_Token_String'Length;
            Errors.Error
              ("'(' expected in operation definition.",
               Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token /= T_Right_Paren then
         declare
            Param : Node_Id;
         begin
            Parse_Param_Dcl (Param, Success);
            if not Success then
               return;
            end if;
            Append_Node (Result, Param);
         end;
      end if;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Param : Node_Id;
         begin
            Parse_Param_Dcl (Param, Success);
            if not Success then
               Go_To_Next_Right_Paren;
            else
               Append_Node (Result, Param);
            end if;
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Errors.Error
           ("')' expected at the end of the " &
            "parameters definition.",
            Errors.Error,
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
   procedure Parse_Param_Dcl (Result : out Node_Id;
                              Success : out Boolean) is
      Attr_Success : Boolean;
   begin
      pragma Debug (O2 ("Parse_Param_Dcl: enter"));
      Result := Make_Param (Get_Token_Location);
      declare
         Node : Param_Mode;
      begin
         Node := Mode (Result);
         Parse_Param_Attribute (Node, Attr_Success);
         Set_Mode (Result, Node);
      end;
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
      declare
         Node : Node_Id;
      begin
         Node := Param_Type (Result);
         Parse_Param_Type_Spec (Node, Success);
         Set_Param_Type (Result, Node);
      end;
      if not Success then
         return;
      end if;
      declare
         Node : Node_Id;
      begin
         Node := Declarator (Result);
         Parse_Simple_Declarator (Node, Result, Success);
         Set_Declarator (Result, Node);
      end;
      pragma Debug (O2 ("Parse_Param_Dcl: end"));
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
            Errors.Error
              ("mode expected (in, out or inout).",
               Errors.Error,
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
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Errors.Error
              ("'(' expected in raises statement.",
               Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token = T_Right_Paren then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 1;
            Errors.Error
              ("scoped_name expected : a raise statement " &
               "may not be empty.",
               Errors.Error,
               Loc);
         end;
         Next_Token;
         Success := True;
         return;
      end if;
      declare
         Name : Node_Id;
      begin
         Parse_Scoped_Name (Name, Success);
         if not Success then
            return;
         end if;
         if Name /= No_Node then
            if Kind (Value (Name)) /= K_Exception then
               Errors.Error
                 ("This scoped name is supposed " &
                  "to denote an exception.",
                  Errors.Error,
                  Get_Token_Location);
            end if;
         end if;
         Append_Node (Result, Name);
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Name : Node_Id;
         begin
            Parse_Scoped_Name (Name, Success);
            if not Success then
               Go_To_Next_Semi_Colon;
               return;
            end if;
            if Name /= No_Node then
               if Kind (Value (Name)) /= K_Exception then
                  Errors.Error
                    ("This scoped name is supposed " &
                     "to denote an exception.",
                     Errors.Error,
                     Get_Token_Location);
               elsif Is_In_Pointed_List (Result, Name) then
                  Errors.Error
                    ("An operation may not raise twice " &
                     "a given exception.",
                     Errors.Error,
                     Get_Token_Location);
               else
                  Append_Node (Result, Name);
               end if;
            end if;
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Errors.Error
           ("')' expected at the end of the " &
            "raises statement.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
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
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 7;
            Errors.Error
              ("'(' expected in context statement.",
               Errors.Error,
               Loc);
         end;
         Success := False;
         return;
      end if;
      Next_Token;
      if Get_Token = T_Right_Paren then
         declare
            Loc : Errors.Location;
         begin
            Loc := Get_Previous_Token_Location;
            Loc.Col := Loc.Col + 1;
            Errors.Error
              ("string literal expected : a context " &
               "statement may not be empty.",
               Errors.Error,
               Loc);
         end;
         Next_Token;
         Success := True;
         return;
      end if;
      declare
         Name : Node_Id;
         String_Type : Constant_Value_Ptr :=
          new Constant_Value (Kind => C_String);
      begin
         String_Type.String_Length := -1;
         Parse_String_Literal (Name,
                               Success,
                               String_Type);
         Free (String_Type);
         if not Success then
            return;
         end if;
         Check_Context_String (Expr_Value (Name).String_Value.all);
         Append_Node (Result, Name);
      end;
      while Get_Token = T_Comma loop
         Next_Token;
         declare
            Name : Node_Id;
            String_Type : Constant_Value_Ptr :=
             new Constant_Value (Kind => C_String);
         begin
            String_Type.String_Length := -1;
            Parse_String_Literal (Name,
                                  Success,
                                  String_Type);
            Free (String_Type);
            if not Success then
               Go_To_Next_Semi_Colon;
               return;
            end if;
            Check_Context_String (Expr_Value (Name).String_Value.all);
            Append_Node (Result, Name);
         end;
      end loop;
      if Get_Token /= T_Right_Paren then
         Errors.Error
           ("')' expected at the end of the " &
            "context statement.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      return;
   end Parse_Context_Expr;

   ---------------------------
   -- Parse_Param_Type_Spec --
   ---------------------------

   procedure Parse_Param_Type_Spec
     (Result : out Node_Id;
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
               Res : Node_Id;
            begin
               Parse_String_Type (Res, Success);
               Result := Res;
            end;

         when T_Wstring =>

            declare
               Res : Node_Id;
            begin
               Parse_Wide_String_Type (Res, Success);
               Result := Res;
            end;

         when T_Colon_Colon | T_Identifier =>

            Parse_Scoped_Name (Result, Success);

            if Result /= No_Node then
               declare
                  Not_A_Type : Boolean := False;
               begin

                  --  Check that the scoped name denotes a type

                  pragma Debug (O ("Parse_Simple_Type_Spec : " &
                                   "kind of result is " &
                                   Img (Kind (Result))));
                  if S_Type (Result) /= No_Node then
                     pragma Debug (O ("Parse_Simple_Type_Spec : " &
                                      "scoped name without an S_Type"));

                     case Kind (S_Type (Result)) is
                        when K_Float
                          | K_Double
                          | K_Long_Double
                          | K_Long
                          | K_Long_Long
                          | K_Short
                          | K_Unsigned_Long
                          | K_Unsigned_Long_Long
                          | K_Unsigned_Short
                          | K_Char
                          | K_Wide_Char
                          | K_Boolean
                          | K_Octet
                          | K_Fixed
                          | K_Any
                          | K_Object
                          | K_ValueBase
                          | K_String
                          | K_Wide_String
                          | K_Enum
                          | K_Struct
                          | K_Union
                          | K_Sequence
                          | K_Interface
                          | K_Forward_Interface
                          | K_ValueType
                          | K_Boxed_ValueType
                          | K_Forward_ValueType
                          | K_Native =>
                           null;
                        when others =>
                           Not_A_Type := True;
                     end case;
                  else
                     pragma Debug (O ("Parse_Simple_Type_Spec : " &
                                      "scoped name with an S_Type"));
                     Not_A_Type := True;
                  end if;
                  if Not_A_Type then
                     pragma Debug (O ("Parse_Simple_Type_Spec : " &
                                      "not_a_type error"));
                     Errors.Error
                       ("A Scoped_Named with a S_Type of "
                        & Img (Kind (S_Type (Result)))
                        & " is not acceptable as a Param_Type.",
                        Errors.Error,
                        Get_Token_Location);
                  end if;
               end;
            end if;
         when others =>
            Errors.Error
              ("param type specifier expected.",
               Errors.Error,
               Get_Token_Location);
            Success := False;
      end case;
      return;
   end Parse_Param_Type_Spec;

   ---------------------------
   --  Parse_Fixed_Pt_Type  --
   ---------------------------
   procedure Parse_Fixed_Pt_Type (Result : out Node_Id;
                                  Success : out Boolean) is
   begin
      Next_Token;
      Result := Make_Fixed (Get_Previous_Token_Location);
      if Get_Token /= T_Less then
         Errors.Error
           ("'<' expected in fixed point type definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      declare
         Node : Node_Id;
      begin
         Node := Digits_Nb (Result);
         Parse_Positive_Int_Const (Node, Success);
         Set_Digits_Nb (Result, Node);
      end;
      if not Success then
         Result := No_Node;
         Go_To_Next_Greater;
         return;
      end if;
      if Expr_Value (Digits_Nb (Result)).Integer_Value < 0 or
        Expr_Value (Digits_Nb (Result)).Integer_Value > 31 then
         Errors.Error
           ("invalid number of digits in fixed point " &
            "type definition : it should be in range " &
            "0 .. 31.",
            Errors.Error,
            Get_Token_Location);
      end if;
      if Get_Token /= T_Comma then
         Errors.Error
           ("',' expected in fixed point type definition.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         return;
      end if;
      Next_Token;
      declare
         Node : Node_Id;
      begin
         Node := Scale (Result);
         Parse_Positive_Int_Const (Node, Success);
         Set_Scale (Result, Node);
      end;
      if not Success then
         Go_To_Next_Greater;
         Result := No_Node;
         return;
      end if;
      if Expr_Value (Digits_Nb (Result)).Integer_Value <
        Expr_Value (Scale (Result)).Integer_Value then
         Errors.Error
           ("invalid scale in fixed point " &
            "type definition : it should be less " &
            "than or equal to the number of digits.",
            Errors.Error,
            Get_Token_Location);
      end if;
      if Get_Token /= T_Greater then
         Errors.Error
           ("'>' expected in fixed point type definition.",
            Errors.Error,
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
   procedure Parse_Value_Base_Type (Result : in out Node_Id;
                                    Success : out Boolean) is
   begin
      Result := No_Node;
      Success := False;
   end Parse_Value_Base_Type;


   ------------------------------
   --  Inheritance management  --
   ------------------------------

   -------------------------------
   --  Interface_Is_Importable  --
   -------------------------------
   function Interface_Is_Importable (Int : in Node_Id;
                                     Scope : in Node_Id)
                                     return Boolean is
      It, It2 : Node_Iterator;
      Node, Node2 : Node_Id;
      Result : Boolean := True;
      List : Node_List := Nil_List;
      Result_List : Node_List := Nil_List;

      procedure Call_Find_Identifier_In_Inheritance (Node : Node_Id);

      procedure Call_Find_Identifier_In_Inheritance (Node : Node_Id) is
         It3 : Node_Iterator;
         First_Node : Node_Id;
      begin
         --  find the given node in previous inheritance
         Find_Identifier_In_Inheritance (Name (Node), Scope, List);
         --  remove duplicated nodes
         Result_List := Simplify_Node_List (List);
         Free (List);
         --  if the list has more than one element,
         --  one of the two definition comes from an inherited
         --  interface that is not Scope, so the definitions clash.
         --  If there is only one element, we must test its scope.
         --  If there are none, there is no problem.
         case Get_Length (Result_List) is
            when 0 =>
               pragma Debug (O ("Interface_Is_Importable : list is nil_list"));
               null;
            when 1 =>
               Init (It3, Result_List);
               Get_Next_Node (It3, First_Node);
               pragma Debug (O ("Interface_Is_Importable : list length is 1"));
               pragma Debug (O ("Interface_Is_Importable : parent scope is " &
                                Name (Definition (First_Node).Parent_Scope)));
               pragma Debug (O ("Interface_Is_Importable : scope is " &
                                Name (Value (Int))));
               if Definition (First_Node).Parent_Scope /= Value (Int) then
                  Result := False;
               end if;
            when others =>
               pragma Debug (O ("Interface_Is_Importable : list length > 1"));
               Result := False;
         end case;
      end Call_Find_Identifier_In_Inheritance;

   begin
      pragma Debug (O2 ("Interface_Is_Importable: enter"));
      pragma Assert (Int /= No_Node);
      pragma Assert (Kind (Int) = K_Scoped_Name);
      pragma Assert (Kind (Value (Int)) = K_Interface);
      pragma Assert (Kind (Scope) = K_Interface);
      --  loop over each definition in the interface
      Init (It, Contents (Value (Int)));
      while (not Is_End (It)) and Result loop
         pragma Debug (O ("Interface_Is_Importable : beginning of loop"));
         Get_Next_Node (It, Node);
         --  if the current definition is an operation
         if Kind (Node) = K_Operation then
            Call_Find_Identifier_In_Inheritance (Node);
         end if;
         --  if it is an attribute, loop over its declarators
         if Kind (Node) = K_Attribute then
            Init (It2, Declarators (Node));
            while (not Is_End (It2)) and Result loop
               Get_Next_Node (It2, Node2);
               Call_Find_Identifier_In_Inheritance (Node2);
            end loop;
         end if;
      end loop;
      pragma Debug (O2 ("Interface_Is_Importable: end"));
      return Result;
   end Interface_Is_Importable;


   --------------------------
   --  Parsing of pragmas  --
   --------------------------

   --------------------
   --  Parse_Pragma  --
   --------------------

   procedure Parse_Pragma (Result : out Node_Id;
                           Success : out Boolean) is
   begin
      pragma Debug (O2 ("Parse_Pragma: enter"));
      Result := No_Node;
      Success := False;
      Next_Token;
      if Get_Token /= T_Identifier then
         Errors.Error
           ("pragma identifier expected",
            Errors.Error,
            Get_Token_Location);
         Go_To_End_Of_Pragma;
         return;
      end if;
      declare
         Pragma_Id : constant String
           := Get_Token_String;
      begin
         if Pragma_Id = "ID" then

            -----------------------------------------
            -- #pragma ID <scoped_name> <string>   --
            --                                     --
            -- Explicitly give a RepositoryID to a --
            -- named entity.                       --
            -----------------------------------------

            declare
               Name_Node : Node_Id;
               String_Lit_Node : Node_Id;
               Res_Success : Boolean;
               String_Constant_Type : Constant_Value_Ptr :=
                 new Constant_Value (Kind => C_String);
            begin
               String_Constant_Type.String_Length := -1;
               Next_Token;
               Parse_Scoped_Name (Name_Node, Res_Success);
               if not Res_Success then
                  Go_To_End_Of_Pragma;
                  return;
               end if;
               Parse_String_Literal (String_Lit_Node,
                                     Res_Success,
                                     String_Constant_Type);
               Free (String_Constant_Type);
               if not Res_Success then
                  Errors.Error
                    ("Repository ID expected.",
                     Errors.Error,
                     Get_Token_Location);
                  Go_To_End_Of_Pragma;
                  return;
               end if;

               if Name_Node /= No_Node then
                  if Is_Explicit_Repository_Id (Value (Name_Node)) then
                     Errors.Error
                       ("Entity already has an explicit repository ID.",
                        Errors.Error,
                        Get_Token_Location);
                     Go_To_End_Of_Pragma;
                     return;
                  end if;
                  Set_Is_Explicit_Repository_Id (Value (Name_Node), True);
                  Set_Repository_Id (Value (Name_Node), String_Lit_Node);
               end if;

               --  pragma ID does not generate any node:
               --  return with Success = False.

            end;

         elsif Pragma_Id = "prefix" then

            ---------------------------------------
            -- #pragma prefix <string>           --
            --                                   --
            -- Set the current Repository Id for --
            -- the current scope.                --
            ---------------------------------------

            declare
               String_Lit_Node : Node_Id;
               Res_Success : Boolean;
               Val : constant Constant_Value_Ptr
                 := new Constant_Value (Kind => C_String);
            begin
               Val.String_Length := -1;
               Next_Token;

               Parse_String_Literal
                 (String_Lit_Node, Res_Success, Val);
               if not Res_Success then
                  Errors.Error
                    ("Repository ID prefix expected.",
                     Errors.Error,
                     Get_Token_Location);
                  Go_To_End_Of_Pragma;
                  return;
               end if;

               Set_Current_Prefix
                 (Get_Current_Scope, String_Lit_Node);

               --  pragma prefix does not generate any node:
               --  return with Success = False.

            end;

         elsif Pragma_Id = "version" then

            ----------------------------------------------
            -- #pragma version <scoped_name> <string>   --
            --                                          --
            -- Set the current version of the           --
            -- Repository Id for a given name           --
            ----------------------------------------------

            declare
               Name_Node : Node_Id;
               Rep_Id : Node_Id;
               Res_Success : Boolean;
               Version : Version_Type;
            begin
               Next_Token;
               Parse_Scoped_Name (Name_Node, Res_Success);
               if not Res_Success then
                  Go_To_End_Of_Pragma;
                  return;
               end if;
               Parse_Version (Version, Res_Success);
               if not (Res_Success) then
                  Go_To_End_Of_Pragma;
                  return;
               end if;

               if Name_Node /= No_Node then
                  if Is_Explicit_Version_Id (Value (Name_Node)) or
                    Is_Explicit_Repository_Id (Value (Name_Node))
                  then
                     Errors.Error
                       ("Entity already has an explicit version ID.",
                        Errors.Error,
                        Get_Token_Location);
                     Go_To_End_Of_Pragma;
                     return;
                  end if;
                  Set_Is_Explicit_Version_Id (Value (Name_Node), True);
                  Rep_Id := Repository_Id (Value (Name_Node));
                  --  replace the former version, (should be 1.0)
                  declare
                     use Ada.Strings.Unbounded;
                     New_Rep : Unbounded_String :=
                       To_Unbounded_String (String_Value (Rep_Id));
                     Smajor : String :=
                       Interfaces.Unsigned_16'Image (Version.Major);
                     Sminor : String :=
                       Interfaces.Unsigned_16'Image (Version.Minor);
                  begin
                     Replace_Slice
                       (New_Rep,
                        Index (To_Unbounded_String
                               (String_Value (Rep_Id)), ":1.0") + 1,
                        String_Value (Rep_Id)'Length,
                        Smajor ((Smajor'First + 1) .. Smajor'Last) &
                        "." &
                        Sminor ((Sminor'First + 1) .. Sminor'Last));
                     Set_String_Value (Rep_Id, To_String (New_Rep));
                  end;
               end if;

               --  pragma version does not generate any node:
               --  return with Success = False.

            end;

         else
            Errors.Error
            ("Unknown pragma: " & Pragma_Id & ", will be ignored.",
             Errors.Warning,
             Get_Token_Location);
            Go_To_End_Of_Pragma;
            return;
         end if;

         if Get_Token /= T_End_Pragma then
            Errors.Error
              ("unexpected end of pragma line : the end will be ignored.",
               Errors.Error,
               Get_Token_Location);
            Go_To_End_Of_Pragma;
            return;
         end if;
         --  consumes the end_of_pragma token
         Next_Token;
         return;
      end;

      pragma Debug (O2 ("Parse_Pragma: leave"));
   end Parse_Pragma;


   ---------------------------
   --  Parsing of literals  --
   ---------------------------

   --------------------------
   --  Hexa_Char_To_Digit  --
   --------------------------
   function Hexa_Char_To_Digit
     (C : in Character)
   return Integer
   is
      use Ada.Characters.Latin_1;

      Result : Integer;
   begin
      Result := Character'Pos (C);
      if Result >= Character'Pos ('0') and
        Result <= Character'Pos ('9') then
         Result := Result - Character'Pos ('0');
      elsif Result >= Character'Pos (LC_A) and
        Result <= Character'Pos (LC_F) then
         Result := Result + 10 - Character'Pos ('a');
      else
         Result := Result + 10 - Character'Pos ('A');
      end if;
      return Result;
   end Hexa_Char_To_Digit;

   ------------------------
   --  Get_Char_Literal  --
   ------------------------
   procedure Get_Char_Literal (S : in String;
                               Result : out Idl_Character;
                               Offset : out Integer) is
      use Ada.Characters.Latin_1;
   begin
      if S (S'First) = '\' then
         Offset := 2;
         case S (S'First + 1) is
            when LC_N =>
               Result := LF;
            when LC_T =>
               Result := HT;
            when LC_V =>
               Result := VT;
            when LC_B =>
               Result := BS;
            when LC_R =>
               Result := CR;
            when LC_F =>
               Result := FF;
            when LC_A =>
               Result := BEL;
            when '\' =>
               Result := '\';
            when '?' =>
               Result := '?';
            when ''' =>
               Result := ''';
            when Quotation =>
               Result := Quotation;
            when '0' .. '7' =>
               declare
                  Pos : Integer;
               begin
                  Pos := Character'Pos (S (S'First + 1)) -
                    Character'Pos ('0');
                  for G in 2 .. 3 loop
                     if G < S'Length then
                        case S (S'First + G) is
                           when '0' .. '7' =>
                              Pos := 8 * Pos +
                                Character'Pos (S (S'First + G)) -
                                Character'Pos ('0');
                              Offset := G + 1;
                           when others => exit;
                        end case;
                     end if;
                  end loop;
                  Result := Character'Val (Pos);
               end;
            when LC_X =>
               declare
                  Pos : Integer;
               begin
                  Pos := Hexa_Char_To_Digit (S (S'First + 2));
                  Offset := 3;
                  pragma Debug (O ("Get_Char_Literal : Pos = " &
                                   Integer'Image (Pos)));
                  if S'Length > 3 then
                     case S (S'First + 3) is
                        when '0' .. '9' | 'A' .. 'F' | LC_A .. LC_F =>
                           Pos := (16 * Pos) +
                             Hexa_Char_To_Digit (S (S'First + 3));
                           Offset := 4;
                        when others => null;
                     end case;
                  end if;
                  pragma Debug (O ("Get_Char_Literal : Pos = " &
                                   Integer'Image (Pos)));
                  Result := Character'Val (Pos);
               end;
            when others =>
               raise Errors.Internal_Error;
         end case;
      else
         Result := S (S'First);
         Offset := 1;
      end if;
   end Get_Char_Literal;

   -----------------------------
   --  Get_Wide_Char_Literal  --
   -----------------------------
   procedure Get_Wide_Char_Literal (S : in String;
                                    Result : out Idl_Wide_Character;
                                    Offset : out Integer) is
      use Ada.Characters.Latin_1;
   begin
      if S (S'First) = '\' then
         Offset := 2;
         case S (S'First + 1) is
            when LC_N =>
               Result := Wide_Character'Val (Character'Pos (ASCII.Lf));
            when LC_T =>
               Result := Wide_Character'Val (Character'Pos (ASCII.Ht));
            when LC_V =>
               Result := Wide_Character'Val (Character'Pos (ASCII.Vt));
            when LC_B =>
               Result := Wide_Character'Val (Character'Pos (ASCII.Bs));
            when LC_R =>
               Result := Wide_Character'Val (Character'Pos (CR));
            when LC_F =>
               Result := Wide_Character'Val (Character'Pos (FF));
            when LC_A =>
               Result := Wide_Character'Val (Character'Pos (BEL));
            when '\' =>
               Result := '\';
            when '?' =>
               Result := '?';
            when ''' =>
               Result := ''';
            when Quotation =>
               Result := Wide_Character'Val (Character'Pos (Quotation));
            when '0' .. '7' =>
               declare
                  Pos : Integer;
               begin
                  Pos := Character'Pos (S (S'First + 1)) -
                    Character'Pos ('0');
                  for G in 2 .. 3 loop
                     if G < S'Length then
                        case S (S'First + G) is
                           when '0' .. '7' =>
                              Pos := 8 * Pos +
                                Character'Pos (S (S'First + G)) -
                                Character'Pos ('0');
                              Offset := G + 1;
                           when others => exit;
                        end case;
                     end if;
                  end loop;
                  Result := Wide_Character'Val (Pos);
               end;
            when LC_X =>
               declare
                  Pos : Integer;
               begin
                  Pos := Hexa_Char_To_Digit (S (S'First + 2));
                  Offset := 3;
                  pragma Debug (O ("Get_Char_Literal : Pos = " &
                                   Integer'Image (Pos)));
                  if S'Length > 3 then
                     case S (S'First + 3) is
                        when '0' .. '9' | 'A' .. 'F' | LC_A .. LC_F =>
                           Pos := (16 * Pos) +
                             Hexa_Char_To_Digit (S (S'First + 3));
                           Offset := 4;
                        when others => null;
                     end case;
                  end if;
                  pragma Debug (O ("Get_Char_Literal : Pos = " &
                                   Integer'Image (Pos)));
                  Result := Wide_Character'Val (Pos);
               end;
            when LC_U =>
               declare
                  Pos : Integer;
               begin
                  Pos := Hexa_Char_To_Digit (S (S'First + 2));
                  Offset := 3;
                  for G in 3 .. 5 loop
                     if G < S'Length then
                        case S (S'First + G) is
                           when '0' .. '9' | 'A' .. 'F' | LC_A .. LC_F =>
                              Pos := 16 * Pos +
                                Hexa_Char_To_Digit (S (S'First + G));
                              Offset := G + 1;
                           when others => exit;
                        end case;
                     end if;
                  end loop;
                  Result := Wide_Character'Val (Pos);
               end;
            when others =>
               raise Errors.Internal_Error;
         end case;
      else
         Result :=  Wide_Character'Val
           (Character'Pos (S (S'First)));
         Offset := 1;
      end if;
   end Get_Wide_Char_Literal;

   ---------------------------
   --  Get_Integer_Literal  --
   ---------------------------
   function Get_Integer_Literal return Idl_Integer is
      S : String := Get_Token_String;
      Result : Idl_Integer := 0;
      I : Natural := 0;
   begin
      pragma Debug (O2 ("Get_Integer_Literal: enter"));
      case Get_Token is
         when T_Lit_Decimal_Integer =>
            while I < S'Length loop
               Result := Result * 10 +
                 (Character'Pos (S (S'First + I)) - Character'Pos ('0'));
               I := I + 1;
            end loop;
         when T_Lit_Octal_Integer =>
            I := 1;
            while I < S'Length loop
               Result := Result * 8 +
                 (Character'Pos (S (S'First + I)) - Character'Pos ('0'));
               I := I + 1;
            end loop;
         when T_Lit_Hexa_Integer =>
            I := 2;
            while I < S'Length loop
               Result := Result * 16 +
                 Idl_Integer (Hexa_Char_To_Digit (S (S'First + I)));
               I := I + 1;
            end loop;
         when others =>
            return Result;
      end case;
      pragma Debug (O2 ("Get_Integer_Literal: end"));
         return Result;
   end Get_Integer_Literal;

   ---------------------
   --  Parse_Version  --
   ---------------------
   procedure Parse_Version (Result : out Version_Type;
                            Success : out Boolean) is
   begin
      if Get_Token /= T_Lit_Simple_Floating_Point then
         Errors.Error
           ("Invalid version number.",
            Errors.Error,
            Get_Token_Location);
         Success := False;
         Result.Minor := 0;
         Result.Major := 1;
         return;
      end if;
      declare
         S : String := Get_Token_String;
         Minor : Interfaces.Unsigned_16 := 0;
         Major : Interfaces.Unsigned_16 := 0;
         I : Natural := 0;
         use Interfaces;
      begin
         while S (S'First + I) /= '.' loop
            Major := Major * 10 +
              (Character'Pos (S (S'First + I)) - Character'Pos ('0'));
            I := I + 1;
         end loop;
         I := I + 1;
         while I < S'Length loop
            Minor := Minor * 10 +
              (Character'Pos (S (S'First + I)) - Character'Pos ('0'));
            I := I + 1;
         end loop;
         Result.Minor := Minor;
         Result.Major := Major;
         Success := True;
         Next_Token;
      end;
   end Parse_Version;

   -----------------------------
   --  Parse_Integer_Literal  --
   -----------------------------
   procedure Parse_Integer_Literal (Result : out Node_Id;
                                    Success : out Boolean;
                                    Expr_Type : in Constant_Value_Ptr) is
   begin
      pragma Debug (O2 ("Parse_Integer_Literal: enter"));
      Result := Make_Lit_Integer (Get_Token_Location);
      case Expr_Type.Kind is
         when C_Octet
           | C_Short
           | C_Long
           | C_LongLong
           | C_UShort
           | C_ULong
           | C_ULongLong
           | C_General_Integer =>
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => Expr_Type.Kind));
            Expr_Value (Result).Integer_Value := Get_Integer_Literal;
            Check_Value_Range (Result, False);
         when others =>
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_No_Kind));
            Errors.Error
              ("The specified type for this constant " &
               "does not match with its value.",
               Errors.Error,
               Get_Token_Location);
      end case;
      Next_Token;
      Success := True;
      pragma Debug (O2 ("Parse_Integer_Literal: end"));
      return;
   end Parse_Integer_Literal;

   ----------------------------
   --  Parse_String_Literal  --
   ----------------------------

   procedure Parse_String_Literal
     (Result : out Node_Id;
      Success : out Boolean;
      Expr_Type : in Constant_Value_Ptr) is

      function Get_String_Literal return Idl_String;

      function Get_String_Literal return Idl_String is
         S : String := Get_Token_String;
         Result : String (1 .. S'Length);
         L : Natural := 0;
         I : Natural := 0;
         Offset : Integer;
         C : Character;
         use Ada.Characters.Latin_1;
      begin
         while I < S'Length loop
            if S (S'First + I) = Quotation then
               I := I + 2;
            else
               L := L + 1;
               Get_Char_Literal
                 (S (S'First + I .. S'Last), C, Offset);
               Result (L) := C;
               I := I + Offset;
            end if;
         end loop;
         if Expr_Type.String_Length >= 0 then
            if L > Integer (Expr_Type.String_Length) then
               Errors.Error
                 ("This value does not match with the specified type : " &
                  "the string is too long.",
                  Errors.Error,
                  Get_Token_Location);
            end if;
         end if;
         return new String'(Result (1 .. L));
      end Get_String_Literal;

   begin
      pragma Debug (O2 ("Parse_String_Literal: enter"));
      if Get_Token /= T_Lit_String then
         Errors.Error
           ("String literal expected here.",
            Errors.Error,
            Get_Token_Location);
         Result := No_Node;
         Success := False;
         pragma Debug (O2 ("Parse_String_Literal: end"));
         return;
      end if;
      Result := Make_Lit_String (Get_Token_Location);
      if Expr_Type.Kind = C_String then
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_String));
         Expr_Value (Result).String_Value := Get_String_Literal;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      pragma Debug (O2 ("Parse_String_Literal: end"));
      return;
   end Parse_String_Literal;

   ---------------------------------
   --  Parse_Wide_String_Literal  --
   ---------------------------------
   procedure Parse_Wide_String_Literal (Result : out Node_Id;
                                        Success : out Boolean;
                                        Expr_Type : in Constant_Value_Ptr) is

      function Get_WString_Literal return Idl_Wide_String;

      function Get_WString_Literal return Idl_Wide_String is
         S : String := Get_Token_String;
         Result : Wide_String (1 .. S'Length);
         L : Natural := 0;
         I : Natural := 0;
         Offset : Integer;
         C : Wide_Character;
         use Ada.Characters.Latin_1;
      begin
         while I < S'Length loop
            if S (S'First + I) = Quotation then
               I := I + 2;
            else
               L := L + 1;
               Get_Wide_Char_Literal
                 (S (S'First + I .. S'Last), C, Offset);
               Result (L) := C;
               I := I + Offset;
            end if;
         end loop;
         if Expr_Type.WString_Length >= 0 then
            if L > Integer (Expr_Type.WString_Length) then
               Errors.Error
                 ("This value does not match with the specified type : " &
                  "the string is too long.",
                  Errors.Error,
                  Get_Token_Location);
            end if;
         end if;
         return new Wide_String'(Result (1 .. L));
      end Get_WString_Literal;

   begin
      if Get_Token /= T_Lit_Wide_String then
         Errors.Error
           ("Wide string literal expected here.",
            Errors.Error,
            Get_Token_Location);
         Result := No_Node;
         Success := False;
         return;
      end if;
      Result := Make_Lit_Wide_String (Get_Token_Location);
      if Expr_Type.Kind = C_WString then
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_WString));
         Expr_Value (Result).WString_Value := Get_WString_Literal;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Wide_String_Literal;

   --------------------------
   --  Parse_Char_Literal  --
   --------------------------
   procedure Parse_Char_Literal (Result : out Node_Id;
                                 Success : out Boolean;
                                 Expr_Type : in Constant_Value_Ptr) is
   begin
      Result := Make_Lit_Character (Get_Token_Location);
      if Expr_Type.Kind = C_Char then
         declare
            Useless : Integer;
            C : Character;
         begin
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_Char));
            Get_Char_Literal (Get_Token_String,
                              C,
                              Useless);
            Expr_Value (Result).Char_Value := C;
         end;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Char_Literal;

   -------------------------------
   --  Parse_Wide_Char_Literal  --
   -------------------------------
   procedure Parse_Wide_Char_Literal (Result : out Node_Id;
                                      Success : out Boolean;
                                      Expr_Type : in Constant_Value_Ptr) is
   begin
      Result := Make_Lit_Wide_Character (Get_Token_Location);
      if Expr_Type.Kind = C_WChar then
         declare
            Useless : Integer;
            C : Wide_Character;
         begin
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_WChar));
            Get_Wide_Char_Literal (Get_Token_String,
                                   C,
                                   Useless);
            Expr_Value (Result).WChar_Value := C;
         end;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Wide_Char_Literal;

   ----------------------------
   --  Get_Floating_Literal  --
   ----------------------------
   function Get_Float_Literal return Idl_Float is
      S : String := Get_Token_String;
      Result : Idl_Float := 0.0;
      I : Natural := 0;
   begin
      while S (S'First + I) /= '.' and
        S (S'First + I) /= 'e' and
        S (S'First + I) /= 'E' loop
         Result := Result * 10.0 +
           Idl_Float (Character'Pos (S (S'First + I)) -
                      Character'Pos ('0'));
         I := I + 1;
      end loop;
      if Get_Token = T_Lit_Simple_Floating_Point or
        Get_Token = T_Lit_Exponent_Floating_Point then
         I := I + 1;
         declare
            Offset : Idl_Float := 0.1;
         begin
            while I < S'Length and then
              (S (S'First + I) /= 'e' and
               S (S'First + I) /= 'E') loop
               Result := Result + Offset *
                    Idl_Float (Character'Pos (S (S'First + I)) -
                               Character'Pos ('0'));
               I := I + 1;
               Offset := Offset / 10.0;
            end loop;
         end;
      end if;
      if Get_Token = T_Lit_Exponent_Floating_Point or
        Get_Token = T_Lit_Pure_Exponent_Floating_Point then
         declare
            Exponent : Integer := 0;
         begin
            I := I + 1;
            while I < S'Length loop
               Exponent := Exponent * 10 +
                 (Character'Pos (S (S'First + I)) - Character'Pos ('0'));
               I := I + 1;
            end loop;
            Result := Result * (10.0 ** Exponent);
         end;
      end if;
      return Result;
   end Get_Float_Literal;

   ---------------------------------
   --  Parse_Floating_Pt_Literal  --
   ---------------------------------
   procedure Parse_Floating_Pt_Literal (Result : out Node_Id;
                                        Success : out Boolean;
                                        Expr_Type : in Constant_Value_Ptr) is
   begin
      Result := Make_Lit_Floating_Point (Get_Token_Location);
      case Expr_Type.Kind is
         when C_Float
           | C_Double
           | C_LongDouble
           | C_General_Float =>
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => Expr_Type.Kind));
            Expr_Value (Result).Float_Value := Get_Float_Literal;
            Check_Value_Range (Result, False);
         when others =>
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_No_Kind));
            Errors.Error
              ("The specified type for this constant " &
               "does not match with its value.",
               Errors.Error,
               Get_Token_Location);
      end case;
      Next_Token;
      Success := True;
      return;
   end Parse_Floating_Pt_Literal;

   ------------------------------
   --  Parse_Fixed_Pt_Literal  --
   ------------------------------
   procedure Parse_Fixed_Pt_Literal (Result : out Node_Id;
                                     Success : out Boolean;
                                     Expr_Type : in Constant_Value_Ptr) is

      procedure Get_Fixed_Literal;

      procedure Get_Fixed_Literal is
         S : String := Get_Token_String;
         Res : Idl_Integer := 0;
         I : Natural := 0;
         L1, L2 : Natural := 0;
         Last_Zeros_Nb : Natural := 0;
      begin
         --  first remove leading zeros
         while S (S'First + I) = '0' loop
            I := I + 1;
         end loop;
         --  parse the integer part
         while S (S'First + I) /= '.' and
           S (S'First + I) /= 'd' and
           S (S'First + I) /= 'D' loop
            Res := Res * 10 +
              Idl_Integer (Character'Pos (S (S'First + I)) -
                           Character'Pos ('0'));
            if S (S'First + I) = '0' then
               Last_Zeros_Nb := Last_Zeros_Nb + 1;
            else
               Last_Zeros_Nb := 0;
            end if;
            I := I + 1;
            L1 := L1 + 1;
         end loop;
         --  parse fractionnal part
         if Get_Token = T_Lit_Floating_Fixed_Point then
            I := I + 1;
            while S (S'First + I) /= 'd' and
              S (S'First + I) /= 'D' loop
               Res := Res * 10 +
                 Idl_Integer (Character'Pos (S (S'First + I)) -
                              Character'Pos ('0'));
               if S (S'First + I) = '0' then
                  Last_Zeros_Nb := Last_Zeros_Nb + 1;
               else
                  Last_Zeros_Nb := 0;
               end if;
               I := I + 1;
               L2 := L2 + 1;
            end loop;
         end if;
         Res := Res / 10 ** Last_Zeros_Nb;
         --  check type precision
         if (L1 /= 0 and
             Idl_Integer (L1) > Expr_Type.Digits_Nb - Expr_Type.Scale) or
           (Idl_Integer (L2 - Last_Zeros_Nb) > Expr_Type.Scale) then
            Errors.Error
              ("The specified type for this constant " &
               "is not enough precise for this value. " &
               "A more precise type will be used.",
               Errors.Error,
               Get_Token_Location);
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_General_Fixed));
         else
            Set_Expr_Value (Result,
                            new Constant_Value (Kind => C_Fixed));
         end if;
         --  stores results
         Expr_Value (Result).Fixed_Value := Res;
         Expr_Value (Result).Digits_Nb :=
           Idl_Integer (L1 + L2 - Last_Zeros_Nb);
         Expr_Value (Result).Scale :=
           Idl_Integer (L2 - Last_Zeros_Nb);
      end Get_Fixed_Literal;

   begin
      Result := Make_Lit_Fixed_Point (Get_Token_Location);
      if Expr_Type.Kind = C_Fixed then
         Get_Fixed_Literal;
      else
         Set_Expr_Value (Result,
                         new Constant_Value (Kind => C_No_Kind));
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      Next_Token;
      Success := True;
      return;
   end Parse_Fixed_Pt_Literal;

   -------------------------
   --  Check_Value_Range  --
   -------------------------
   procedure Check_Value_Range (Node : in out Node_Id;
                                Full : in Boolean) is
      N : Constant_Value_Ptr renames Expr_Value (Node);

      procedure Integer_Precision_Exceeded;
      procedure Float_Precision_Exceeded;
      procedure Fixed_Precision_Exceeded;

      procedure Integer_Precision_Exceeded is
         Old_Value : Constant_Value_Ptr := Expr_Value (Node);
      begin
         pragma Debug (O2 ("Integer_Precision_Exceeded: enter"));
         Errors.Error
           ("The specified type for this integer constant " &
            "does not allow this value",
            Errors.Error,
            Get_Token_Location);
         Set_Expr_Value
           (Node,
            new Constant_Value (Kind => C_General_Integer));
         Expr_Value (Node).Integer_Value := Old_Value.Integer_Value;
         Free (Old_Value);
         pragma Debug (O2 ("Integer_Precision_Exceeded: end"));
      end Integer_Precision_Exceeded;

      procedure Float_Precision_Exceeded is
         Old_Value : Constant_Value_Ptr := Expr_Value (Node);
      begin
         pragma Debug (O2 ("Float_Precision_Exceeded: enter"));
         Errors.Error
           ("The specified type for this floating point constant " &
            "does not allow this value",
            Errors.Error,
            Get_Token_Location);
         Set_Expr_Value
           (Node,
            new Constant_Value (Kind => C_General_Float));
         Expr_Value (Node).Float_Value := Old_Value.Float_Value;
         Free (Old_Value);
         pragma Debug (O2 ("Float_Precision_Exceeded: end"));
      end Float_Precision_Exceeded;

      procedure Fixed_Precision_Exceeded is
         Old_Value : Constant_Value_Ptr := Expr_Value (Node);
      begin
         pragma Debug (O2 ("Fixed_Precision_Exceeded: enter"));
         Errors.Error
           ("invalid number of digits in fixed point " &
            "type definition : it should be in range " &
            "0 .. 31.",
            Errors.Error,
            Get_Token_Location);
         Set_Expr_Value
           (Node,
            new Constant_Value (Kind => C_General_Fixed));
         Expr_Value (Node).Fixed_Value := Old_Value.Fixed_Value;
         Expr_Value (Node).Digits_Nb := Old_Value.Digits_Nb;
         Expr_Value (Node).Scale := Old_Value.Scale;
         Free (Old_Value);
         pragma Debug (O2 ("Float_Precision_Exceeded: end"));
      end Fixed_Precision_Exceeded;

   begin
      pragma Debug (O2 ("Check_Value_Range: enter"));
      pragma Debug (O ("Check_Value_Range : Kind (Node) is " &
                       Node_Kind'Image (Kind (Node)) &
                       ", Full = " & Boolean'Image (Full)));
      pragma Debug (O ("Check_Value_Range : N.kind is " &
                       Const_Kind'Image (N.Kind)));
      pragma Assert (Kind (Node) = K_Add_Expr
                     or else Kind (Node) = K_And_Expr
                     or else Kind (Node) = K_Binary_Expr
                     or else Kind (Node) = K_Div_Expr
                     or else Kind (Node) = K_Expr
                     or else Kind (Node) = K_Id_Expr
                     or else Kind (Node) = K_Lit_Boolean
                     or else Kind (Node) = K_Lit_Character
                     or else Kind (Node) = K_Lit_Enum
                     or else Kind (Node) = K_Lit_Fixed_Point
                     or else Kind (Node) = K_Lit_Floating_Point
                     or else Kind (Node) = K_Lit_Integer
                     or else Kind (Node) = K_Lit_String
                     or else Kind (Node) = K_Lit_Wide_Character
                     or else Kind (Node) = K_Lit_Wide_String
                     or else Kind (Node) = K_Literal
                     or else Kind (Node) = K_Mod_Expr
                     or else Kind (Node) = K_Mul_Expr
                     or else Kind (Node) = K_Neg_Expr
                     or else Kind (Node) = K_Not_Expr
                     or else Kind (Node) = K_Or_Expr
                     or else Kind (Node) = K_Shl_Expr
                     or else Kind (Node) = K_Shr_Expr
                     or else Kind (Node) = K_Sub_Expr
                     or else Kind (Node) = K_Unary_Expr
                     or else Kind (Node) = K_Xor_Expr);
      case N.Kind is
         when C_Octet =>
            if N.Integer_Value < Idl_Octet_Min
              or else N.Integer_Value > Idl_Octet_Max then
               Integer_Precision_Exceeded;
            end if;
         when C_Short =>
            if Full then
               if N.Integer_Value < Idl_Short_Min
                 or else N.Integer_Value > Idl_Short_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_Short_Min
                 or else N.Integer_Value > Idl_UShort_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_Long =>
            if Full then
               if N.Integer_Value < Idl_Long_Min
                 or else N.Integer_Value > Idl_Long_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_Long_Min
                 or else N.Integer_Value > Idl_ULong_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_LongLong =>
            if Full then
               if N.Integer_Value < Idl_LongLong_Min
                 or else N.Integer_Value > Idl_LongLong_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_LongLong_Min
                 or else N.Integer_Value > Idl_ULongLong_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_UShort =>
            if Full then
               if N.Integer_Value < Idl_UShort_Min
                 or else N.Integer_Value > Idl_UShort_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_Short_Min
                 or else N.Integer_Value > Idl_UShort_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_ULong =>
            if Full then
               if N.Integer_Value < Idl_ULong_Min
                 or else N.Integer_Value > Idl_ULong_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_Long_Min
                 or else N.Integer_Value > Idl_ULong_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_ULongLong =>
            if Full then
               if N.Integer_Value < Idl_ULongLong_Min
                 or else N.Integer_Value > Idl_ULongLong_Max then
                  Integer_Precision_Exceeded;
               end if;
            else
               if N.Integer_Value < Idl_LongLong_Min
                 or else N.Integer_Value > Idl_ULongLong_Max then
                  Integer_Precision_Exceeded;
               end if;
            end if;
         when C_Float =>
            if N.Float_Value < Idl_Float_Min
              or else N.Float_Value > Idl_Float_Max then
               Float_Precision_Exceeded;
            end if;
         when C_Double =>
            if N.Float_Value < Idl_Double_Min
              or else N.Float_Value > Idl_Double_Max then
               Float_Precision_Exceeded;
            end if;
         when C_LongDouble =>
            if N.Float_Value < Idl_Long_Double_Min
              or else N.Float_Value > Idl_Long_Double_Max then
               Float_Precision_Exceeded;
            end if;
         when C_Fixed
           | C_General_Fixed =>
            --  simplification of a fixed point literal
            if N.Fixed_Value /= 0 then
               --  first remove the trailing zeros
               while N.Fixed_Value mod 10 = 0 loop
                  N.Fixed_Value := N.Fixed_Value / 10;
                  N.Digits_Nb := N.Digits_Nb - 1;
                  N.Scale := N.Scale + 1;
               end loop;
               --  then remove the leading zeros
               while N.Fixed_Value / 10 ** Natural (N.Digits_Nb - 1) = 0 loop
                  N.Digits_Nb := N.Digits_Nb - 1;
               end loop;
            else
               N.Digits_Nb := 0;
               N.Scale := 0;
            end if;
            --  Checks the number of digits
            if N.Digits_Nb > 31 then
               Fixed_Precision_Exceeded;
            end if;
         when others =>
            null;
      end case;
      pragma Debug (O2 ("Check_Value_Range: end"));
   end Check_Value_Range;

   ------------------------
   --  Check_Expr_Value  --
   ------------------------

   procedure Check_Expr_Value
     (Value : in Constant_Value_Ptr;
      Value_Type : in Constant_Value_Ptr) is
      Types_Ok : Boolean := True;
   begin
      pragma Debug (O2 ("Check_Expr_Value: enter"));
      case Value.Kind is
         when C_General_Integer =>
            pragma Debug (O ("Check_Expr_Value : "
                             & "dealing with a General_Integer"));
            case Value_Type.Kind is
               when C_Octet
                 | C_Short
                 | C_Long
                 | C_LongLong
                 | C_UShort
                 | C_ULong
                 | C_ULongLong =>
                  null;
               when others =>
                  Types_Ok := False;
            end case;
         when C_General_Float =>
            pragma Debug (O ("Check_Expr_Value : "
                             & "dealing with a General_Float"));
            case Value_Type.Kind is
               when C_Float
                 | C_Double
                 | C_LongDouble =>
                  null;
               when others =>
                  Types_Ok := False;
            end case;
         when C_General_Fixed =>
            pragma Debug (O ("Check_Expr_Value : "
                             & "dealing with a General_Fixed"));
            if Value_Type.Kind /= C_Fixed then
               Types_Ok := False;
            end if;
         when others =>
            pragma Debug (O ("Check_Expr_Value : "
                             & "dealing with something else"));
            case Value_Type.Kind is
               when C_General_Integer =>
                  case Value.Kind is
                     when C_Octet
                       | C_Short
                       | C_Long
                       | C_LongLong
                       | C_UShort
                       | C_ULong
                       | C_ULongLong =>
                        null;
                     when others =>
                        Types_Ok := False;
                  end case;
               when C_General_Float =>
                  case Value.Kind is
                     when C_Float
                       | C_Double
                       | C_LongDouble =>
                        null;
                     when others =>
                        Types_Ok := False;
                  end case;
               when C_General_Fixed =>
                  if Value.Kind /= C_Fixed then
                     Types_Ok := False;
                  end if;
               when others =>
                  if Value.Kind /= Value_Type.Kind then
                     Types_Ok := False;
                  end if;
            end case;
      end case;
      if Types_Ok then
         case Value.Kind is
            when C_Fixed =>
               if Value.Digits_Nb - Value.Scale >
                 Value_Type.Digits_Nb - Value_Type.Scale or
                 Value.Scale > Value_Type.Scale then
                  Errors.Error
                    ("The specified type for this fixed point " &
                     "constant is not enough precise for its value. " &
                     "A more precise type will be used.",
                     Errors.Error,
                     Get_Token_Location);
               end if;
            when C_Enum =>
               if Value.Enum_Name /= Value_Type.Enum_Name then
                  Errors.Error
                    ("The specified type for this enum constant " &
                     "does not match with its value.",
                     Errors.Error,
                     Get_Token_Location);
               end if;
            when others =>
               null;
         end case;
      else
         Errors.Error
           ("The specified type for this constant " &
            "does not match with its value.",
            Errors.Error,
            Get_Token_Location);
      end if;
      null;
      pragma Debug (O2 ("Check_Expr_Value: end"));
   end Check_Expr_Value;

   ----------------------------
   --  Check_Context_String  --
   ----------------------------
   procedure Check_Context_String (S : in String) is
      use GNAT.Case_Util;
      use Ada.Characters.Latin_1;
   begin
      if S'Length = 0 then
         return;
      end if;
      if To_Lower (S (S'First)) not in LC_A .. LC_Z then
         Errors.Error ("invalid string for context " &
                              "declaration : the first character " &
                              "must be an alphabetic one.",
                              Errors.Error,
                              Get_Token_Location);
         return;
      end if;
      for I in S'First + 1 .. S'Last - 1 loop
         if To_Lower (S (I)) not in LC_A .. LC_Z
           and S (I) not in '0' .. '9'
           and S (I) /= '.'
           and S (I) /= '_' then
            Errors.Error ("invalid string for context " &
                                 "declaration : it may only content " &
                                 "alphabetic, digit, period, underscore " &
                                 "characters plus an asterisk at the end.",
                                 Errors.Error,
                                 Get_Token_Location);
            return;
         end if;
      end loop;
      if To_Lower (S (S'Last)) not in LC_A .. LC_Z
        and S (S'Last) not in '0' .. '9'
        and S (S'Last) /= '.'
        and S (S'Last) /= '_'
        and S (S'Last) /= '*' then
         Errors.Error ("invalid string for context " &
                              "declaration : the last character may only " &
                              "be an alphabetic, digit, period, " &
                              "underscore or asterisk character.",
                              Errors.Error,
                              Get_Token_Location);
         return;
      end if;
   end Check_Context_String;


   -------------------------------
   -- Evaluation of expressions --
   -------------------------------

   --------
   -- Or --
   --------

   function "or" (X, Y : Idl_Integer) return Idl_Integer is
      I : Idl_Integer := 0;
      Res : Idl_Integer := 0;
      Exp : Idl_Integer := 1;
      XX : Idl_Integer := abs X;
      YY : Idl_Integer := abs Y;
   begin
      while XX > 0 or else YY > 0 loop
         if (XX mod 2 = 1) or else (YY mod 2 = 1) then
            Res := Res + Exp;
         end if;
         I := I + 1;
         Exp := Exp * 2;
         XX := XX / 2;
         YY := YY / 2;
      end loop;
      if X < 0 or else Y < 0 then
         return -Res;
      else
         return Res;
      end if;
   end "or";

   ---------
   -- Xor --
   ---------

   function "xor" (X, Y : Idl_Integer) return Idl_Integer is
      I : Idl_Integer := 0;
      Res : Idl_Integer := 0;
      Exp : Idl_Integer := 1;
      XX : Idl_Integer := abs X;
      YY : Idl_Integer := abs Y;
   begin
      while XX > 0 or else YY > 0 loop
         if XX mod 2 + YY mod 2 = 1 then
            Res := Res + Exp;
         end if;
         I := I + 1;
         Exp := Exp * 2;
         XX := XX / 2;
         YY := YY / 2;
      end loop;
      if (X < 0 and then Y < 0)
        or else (X > 0 and then Y > 0)
      then
         return Res;
      else
         return -Res;
      end if;
   end "xor";

   ---------
   -- And --
   ---------

   function "and" (X, Y : Idl_Integer) return Idl_Integer is
      I : Idl_Integer := 0;
      Res : Idl_Integer := 0;
      Exp : Idl_Integer := 1;
      XX : Idl_Integer := abs X;
      YY : Idl_Integer := abs Y;
   begin
      while XX > 0 or else YY > 0 loop
         if (XX mod 2 = 1) and then (YY mod 2 = 1) then
            Res := Res + Exp;
         end if;
         I := I + 1;
         Exp := Exp * 2;
         XX := XX / 2;
         YY := YY / 2;
      end loop;
      if X < 0 and then Y < 0 then
         return -Res;
      else
         return Res;
      end if;
   end "and";

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left (X : Idl_Integer; Y : Natural) return Idl_Integer is
   begin
      return X * 2 ** Y;
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right (X : Idl_Integer; Y : Natural) return Idl_Integer is
   begin
      return X / 2 ** Y;
   end Shift_Right;

   ---------
   -- Max --
   ---------

   function Max (X, Y : Idl_Integer) return Idl_Integer is
   begin
      if X > Y then
         return X;
      else
         return Y;
      end if;
   end Max;

   ---------------
   -- Fixed_Add --
   ---------------

   procedure Fixed_Add (Res : in out Constant_Value_Ptr;
                        Left, Right : in Constant_Value_Ptr) is
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Left.Kind = C_Fixed);
      pragma Assert (Right.Kind = C_Fixed);
      Res.Digits_Nb :=
        Max (Left.Digits_Nb - Left.Scale,
             Right.Digits_Nb - Right.Scale) +
        Max (Left.Scale, Right.Scale) + 1;
      Res.Scale := Max (Left.Scale, Right.Scale);
      Res.Fixed_Value :=
        Left.Fixed_Value * 10 ** Natural (Res.Scale - Left.Scale) +
        Right.Fixed_Value * 10 ** Natural (Res.Scale - Right.Scale);
   end Fixed_Add;

   ---------------
   -- Fixed_Sub --
   ---------------

   procedure Fixed_Sub
     (Res : in out Constant_Value_Ptr;
      Left, Right : in Constant_Value_Ptr) is
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Left.Kind = C_Fixed);
      pragma Assert (Right.Kind = C_Fixed);

      Res.Digits_Nb :=
        Max (Left.Digits_Nb - Left.Scale,
             Right.Digits_Nb - Right.Scale) +
        Max (Left.Scale, Right.Scale) + 1;
      Res.Scale := Max (Left.Scale, Right.Scale);
      Res.Fixed_Value :=
        Left.Fixed_Value * 10 ** Natural (Res.Scale - Left.Scale)
        - Right.Fixed_Value * 10 ** Natural (Res.Scale - Right.Scale);
   end Fixed_Sub;

   ---------------
   -- Fixed_Mul --
   ---------------

   procedure Fixed_Mul (Res : in out Constant_Value_Ptr;
                        Left, Right : in Constant_Value_Ptr) is
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Left.Kind = C_Fixed);
      pragma Assert (Right.Kind = C_Fixed);

      Res.Digits_Nb := Left.Digits_Nb + Right.Digits_Nb;
      Res.Scale := Left.Scale + Right.Scale;
      Res.Fixed_Value := Left.Fixed_Value * Right.Fixed_Value;
   end Fixed_Mul;

   ---------------
   -- Fixed_Div --
   ---------------

   procedure Fixed_Div
     (Res : in out Constant_Value_Ptr;
      Left, Right : in Constant_Value_Ptr)
   is
      Dn, S, Fv : Idl_Integer := 0;
      Remainder : Idl_Integer;
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Left.Kind = C_Fixed);
      pragma Assert (Right.Kind = C_Fixed);

      Fv := Left.Fixed_Value / Right.Fixed_Value;
      Remainder := Left.Fixed_Value mod Right.Fixed_Value;
      declare
         Ffvv : Idl_Integer := Fv;
      begin
         while Ffvv /= 0 loop
            Dn := Dn + 1;
            Ffvv := Ffvv / 10;
         end loop;
      end;
      S := Left.Scale - Right.Scale;
      while Remainder /= 0 and then Dn < 31 loop
         Fv := Fv * 10 + (Remainder * 10) / Right.Fixed_Value;
         Dn := Dn + 1;
         S := S + 1;
         Remainder := (Remainder * 10) mod Right.Fixed_Value;
      end loop;
      Res.Fixed_Value := Fv;
      Res.Digits_Nb := Dn;
      Res.Scale := S;
   end Fixed_Div;

   --------------
   -- Fixed_Id --
   --------------

   procedure Fixed_Id
     (Res : in out Constant_Value_Ptr;
      Operand : in Constant_Value_Ptr) is
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Operand.Kind = C_Fixed);

      Res.Digits_Nb := Operand.Digits_Nb;
      Res.Scale := Operand.Scale;
      Res.Fixed_Value := Operand.Fixed_Value;
   end Fixed_Id;

   ---------------
   -- Fixed_Neg --
   ---------------

   procedure Fixed_Neg
     (Res : in out Constant_Value_Ptr;
      Operand : in Constant_Value_Ptr) is
   begin
      pragma Assert (Res.Kind = C_Fixed);
      pragma Assert (Operand.Kind = C_Fixed);
      Res.Digits_Nb := Operand.Digits_Nb;
      Res.Scale := Operand.Scale;
      Res.Fixed_Value := -Operand.Fixed_Value;
   end Fixed_Neg;

   ---------
   -- not --
   ---------

   function "not" (X : Idl_Integer) return Idl_Integer
   is
      I : Idl_Integer := 0;
      Res : Idl_Integer := 0;
      Exp : Idl_Integer := 1;
      XX : Idl_Integer := abs X;
   begin
      while XX > 0 loop
         if XX mod 2 = 0 then
            Res := Res + Exp;
         end if;
         I := I + 1;
         Exp := Exp * 2;
         XX := XX / 2;
      end loop;
      if X < 0 then
         return Res;
      else
         return -Res;
      end if;
   end "not";

   --------------------
   -- Error recovery --
   --------------------

   ---------------------------
   -- Go_To_Next_Definition --
   ---------------------------

   --  Try to reach the beginning of the next definition.
   --  Called when the parser encounters an error during the
   --  parsing of a definition in order to try to continue the
   --  parsing after the bad definition.

   procedure Go_To_Next_Definition is
      Num : Natural := 0;
   begin
      pragma Debug (O2 ("Go_To_Next_Definition: enter"));
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Module
                 | T_Interface
                 | T_Exception
                 | T_Union
                 | T_Struct
                 | T_Enum
                 | T_Typedef
                 | T_Custom
                 | T_Abstract
                 | T_ValueType
                 | T_Const
                 | T_Right_Cbracket =>
                  pragma Debug (O2 ("Go_To_Next_Definition: end"));
                  return;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket and Num > 0 then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop;
      pragma Debug (O2 ("Go_To_Next_Definition: end"));
   end Go_To_Next_Definition;

   -------------------------
   -- Go_To_End_Of_Export --
   -------------------------

   --  Try to reach the end of en export.
   --  Called when the parser encounters an error during the
   --  parsing of an export in order to try to continue the
   --  parsing after the bad export.

   procedure Go_To_End_Of_Export is
      Num : Natural := 0;
   begin
      pragma Debug (O2 ("Go_To_Next_Definition: enter"));
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Readonly
                 | T_Attribute
                 | T_Oneway
                 | T_Void
                 | T_String
                 | T_Wstring
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
                 | T_Colon_Colon
                 | T_Identifier
                 | T_Exception
                 | T_Union
                 | T_Struct
                 | T_Enum
                 | T_Typedef
                 | T_Custom
                 | T_Abstract
                 | T_Const
                 | T_Right_Cbracket =>
                  return;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket and Num > 0 then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop;
      pragma Debug (O2 ("Go_To_Next_Definition: end"));
   end Go_To_End_Of_Export;

   ------------------------------
   -- Go_To_Next_Left_Cbracket --
   ------------------------------

   procedure Go_To_Next_Left_Cbracket is
   begin
      pragma Debug (O2 ("Go_To_Next_Left_CBracket: enter"));
      while Get_Token /= T_Eof and Get_Token /= T_Left_Cbracket loop
         Next_Token;
      end loop;
      pragma Debug (O2 ("Go_To_Next_Left_CBracket: end"));
   end Go_To_Next_Left_Cbracket;

   -------------------------------
   -- Go_To_Next_Right_Cbracket --
   -------------------------------

   procedure Go_To_Next_Right_Cbracket is
   begin
      pragma Debug (O2 ("Go_To_Next_Right_CBracket: enter"));
      while Get_Token /= T_Eof and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      pragma Debug (O2 ("Go_To_Next_Right_CBracket: end"));
   end Go_To_Next_Right_Cbracket;

   -----------------------
   -- Go_To_Next_Export --
   -----------------------

   procedure Go_To_Next_Export is
      Num : Natural := 0;
   begin
      pragma Debug (O2 ("Go_To_Next_Export: enter"));
      While_Loop :
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Eof
                 | T_Semi_Colon
                 | T_Right_Cbracket =>
                  exit While_Loop;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket and Num > 0 then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop While_Loop;
      if Get_Token = T_Semi_Colon then
         Next_Token;
      end if;
      pragma Debug (O2 ("Go_To_Next_Export: end"));
   end Go_To_Next_Export;

   ------------------------------
   -- Go_To_Next_Value_Element --
   ------------------------------

   procedure Go_To_Next_Value_Element is
      Num : Natural := 0;
   begin
      pragma Debug (O2 ("Go_To_Next_Value_Element: enter"));
      While_Loop :
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Eof
                 | T_Semi_Colon
                 | T_Right_Cbracket =>
                  exit While_Loop;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket and Num > 0 then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop While_Loop;
      if Get_Token = T_Semi_Colon then
         Next_Token;
      end if;
      pragma Debug (O2 ("Go_To_Next_Value_Element: end"));
   end Go_To_Next_Value_Element;

   -------------------------------
   -- Go_To_End_Of_State_Member --
   -------------------------------

   procedure Go_To_End_Of_State_Member is
   begin
      pragma Debug (O2 ("Go_To_End_Of_State_Member: enter"));
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon loop
         Next_Token;
      end loop;
      if Get_Token /= T_Eof then
         Next_Token;
      else
         null;
      end if;
      pragma Debug (O2 ("Go_To_End_Of_State_Member: end"));
   end Go_To_End_Of_State_Member;

   ----------------------------
   -- Go_To_Next_Right_Paren --
   ----------------------------

   procedure Go_To_Next_Right_Paren is
   begin
      pragma Debug (O2 ("Go_To_Next_Right_Paren: enter"));
      while Get_Token /= T_Eof and Get_Token /= T_Right_Paren loop
         Next_Token;
      end loop;
      pragma Debug (O2 ("Go_To_Next_Right_Paren: end"));
   end Go_To_Next_Right_Paren;

   -----------------------
   -- Go_To_Next_Member --
   -----------------------

   procedure Go_To_Next_Member is
   begin
      pragma Debug (O2 ("Go_To_Next_Right_Paren: enter"));
      while Get_Token /= T_Eof and Get_Token /= T_Semi_Colon
        and Get_Token /= T_Right_Cbracket loop
         Next_Token;
      end loop;
      if Get_Token /= T_Eof and Get_Token /= T_Right_Cbracket then
         Next_Token;
      else
         null;
      end if;
      pragma Debug (O2 ("Go_To_Next_Right_Paren: end"));
   end Go_To_Next_Member;

   -----------------------
   -- Go_To_End_Of_Case --
   -----------------------

   procedure Go_To_End_Of_Case is
      Num : Natural := 0;
   begin

      --  Go to the next clause (T_Case or T_Default) or to
      --  the next right cbracket (if this was the last clause).

      While_Loop :
      while Get_Token /= T_Eof loop
         if Num = 0 then
            case Get_Token is
               when T_Eof
                 | T_Case
                 | T_Right_Cbracket =>
                  exit While_Loop;
               when others =>
                  null;
            end case;
         end if;
         if Get_Token = T_Left_Cbracket then
            Num := Num + 1;
         end if;
         if Get_Token = T_Right_Cbracket and Num > 0 then
            Num := Num - 1;
         end if;
         Next_Token;
      end loop While_Loop;
   end Go_To_End_Of_Case;

   -----------------------------
   -- Go_To_End_Of_Case_Label --
   -----------------------------

   procedure Go_To_End_Of_Case_Label is
   begin

      --  Go to the next colon and consume it

      while Get_Token /= T_Colon loop
         Next_Token;
      end loop;
      Next_Token;
   end Go_To_End_Of_Case_Label;

   ------------------------------
   -- Go_To_End_Of_Scoped_Name --
   ------------------------------

   procedure Go_To_End_Of_Scoped_Name is
   begin
      --  Skip the current token (an identifier)

      Next_Token;

      --  While there are '::' tokens, skip them,
      --  and skip the following identifier.

      while Get_Token = T_Colon_Colon loop
         Next_Token;
         if Get_Token = T_Identifier then
            Next_Token;
         end if;
      end loop;
   end Go_To_End_Of_Scoped_Name;

   -------------------------
   -- Go_To_End_Of_Pragma --
   -------------------------

   procedure Go_To_End_Of_Pragma is
   begin
      while Get_Token /= T_End_Pragma loop
         Next_Token;
      end loop;
      Next_Token;
   end Go_To_End_Of_Pragma;

   ------------------------------
   -- Go_To_End_Of_Enumeration --
   ------------------------------

   procedure Go_To_End_Of_Enumeration is
   begin
      Go_To_Next_Right_Cbracket;
      if Get_Token /= T_Eof then
         Next_Token;
         if Get_Token = T_Semi_Colon then
            Next_Token;
         end if;
      end if;
   end Go_To_End_Of_Enumeration;

   ---------------------------
   -- Go_To_Next_Semi_Colon --
   ---------------------------

   procedure Go_To_Next_Semi_Colon is
   begin
      while Get_Token /= T_Semi_Colon loop
         Next_Token;
      end loop;
      Next_Token;
   end Go_To_Next_Semi_Colon;

   ------------------------
   -- Go_To_Next_Greater --
   ------------------------

   procedure Go_To_Next_Greater is
   begin
      while Get_Token /= T_Greater loop
         Next_Token;
      end loop;
      Next_Token;
   end Go_To_Next_Greater;

end Idl_Fe.Parser;
