------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Representations                              --
--                                                                          --
--                               B O D Y                                    --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;
with Ada.Characters.Handling;      use Ada.Characters.Handling;

with Droopi.Types;
with Droopi.Any;
with Droopi.Log;

with Sequences.Unbounded;

package body Droopi.Representations.SOAP is

   use Droopi.Types;
   use Droopi.Any;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.soap");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   -----------------------------------------------------
   ----------------------------------------------------

   function Get (Str : Stream_Char_Access)
       return Character
   is
      use Ada.Strings.Unbounded;
   begin
      Str.Current_Pos := Str.Current_Pos + 1;
      return Element (Str.Chars, Str.Current_Pos - 1);
   end Get;

   function Peek (Str : Stream_Char_Access)
       return Character
   is
      use Ada.Strings.Unbounded;
   begin
      return Element (Str.Chars, Str.Current_Pos);
   end Peek;

   function End_Of_Input (Str : Stream_Char_Access)
       return Boolean
   is
      use Ada.Strings.Unbounded;
   begin
      if Str.Current_Pos > Length (Str.Chars) then
         return True;
      else
         return False;
      end if;
   end End_Of_Input;



   function Erase_Space (S : String)
    return String
   is
      Str : String := S (2 .. S'Last);
   begin
      return Str;
   end Erase_Space;


   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg  : Types.Short;
      XML_Comp : out XML_Component_Access)
   is
      Comp : aliased XML_Component;
      Str : String :=  Types.Short'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg : Types.Long;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Long'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg : Types.Long_Long;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Long_Long'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg :  Types.Unsigned_Short;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Unsigned_Short'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg  :  Types.Unsigned_Long;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Unsigned_Long'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg  : Types.Unsigned_Long_Long;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Unsigned_Long_Long'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;


   procedure To_XML_Component
     (Name : Types.Identifier;
      Arg  : Types.Float;
      XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Float'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0.0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;


   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Double;
       XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Double'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0.0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Boolean;
       XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
   begin
      Comp.Tag := XML_String (Name);
      Comp.Value :=  To_Droopi_String
         (Types.Boolean'Image (Arg));
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Char;
       XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : XML_String := XML_Null_String;
   begin
      Comp.Tag := XML_String (Name);
      Append (Str, Arg);
      Comp.Value := Str;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Octet;
       XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
      Str : String := Types.Octet'Image (Arg);
   begin
      Comp.Tag := XML_String (Name);
      if Arg > 0 then
         Comp.Value := To_Droopi_String (Erase_Space (Str));
      else
         Comp.Value := To_Droopi_String (Str);
      end if;
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;



   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.String;
       XML_Comp : out XML_Component_Access)
   is
      Comp : XML_Component;
   begin

      Comp.Tag := XML_String (Name);
      Comp.Value :=  XML_String (Arg);
      XML_Comp := new XML_Component'(Comp);
   end To_XML_Component;

   function To_XML_String
      (XML_Comp : XML_Component_Access)
      return XML_String
   is
      use Attributes_Seq;
      S : XML_String;
      Attr : Attributes_Record_Access;
   begin
      Append (S, "<" & XML_Comp.Tag);
      for I in 1 .. Length (XML_Comp.Attributes) loop
         Attr := Attributes_Seq.Element_Of (XML_Comp.Attributes, I);
         Append (S, " " & Attr.Tag_Id & "=" & Attr.Value);
      end loop;
      Append (S, ">");


      if XML_Comp.Childs.Nbr_Of_Items > 0 then
         declare
            Elt : Child_List_Access := XML_Comp.Childs.Head;
            Str : XML_String;
         begin
            loop
               exit when Elt = XML_Comp.Childs.Tail;
               Str := To_XML_String (Elt.Item);
               Append (S, Str);
               Elt := Elt.Next;
            end loop;
         end;
      end if;
      Append (S, "</" & XML_Comp.Tag & ">");

      return S;
   end To_XML_String;



   procedure Set_Parent
        (Child  : in out XML_Component_Access;
         Pt     : XML_Component_Access)
   is
   begin
      Child.Parent := Pt;
   end Set_Parent;


   procedure Add_Comp_List
      (Container_Elt : in out Container_Element_Access;
       XML_Elt  : XML_Component_Access)
   is
      List_Elt   : Child_List_Access := new Child_List_Record;
   begin
      List_Elt.Item := XML_Elt;
      if Container_Elt.Head = null then
         Container_Elt.Head :=  List_Elt;
      else
         Container_Elt.Tail.Next := List_Elt;
      end if;

      Container_Elt.Tail := List_Elt;
      Container_Elt.Nbr_Of_Items := Container_Elt.Nbr_Of_Items + 1;
   end Add_Comp_List;


   procedure Add_Child
      (Comp : in out XML_Component_Access;
       Child   : XML_Component_Access)
   is
      Ce       : Container_Element_Access;
      --  Element  : Child_List_Access;
   begin
      if Comp.Childs = null then
         Ce := new Container_Element;
         Comp.Childs := Ce;
      end if;

      Add_Comp_List (Comp.Childs, Child);
   end Add_Child;


   procedure Tree_Course
      (Comp : in XML_Component_Access;
       Container : in out Container_Element_Access)
   is
      Course_Elt : Child_List_Access
                    := new Child_List_Record;
   begin
      Course_Elt.Item := Comp;
      if Container.Head = null then
         Container.Head := Course_Elt;
      else
         Container.Tail.Next := Course_Elt;
      end if;
      Container.Tail := Course_Elt;

      if Comp.Childs /= null then
         if Comp.Childs.Nbr_Of_Items > 0 then
            Course_Elt := Comp.Childs.Head;
            loop
               exit when Course_Elt = null;
               Tree_Course (Course_Elt.Item, Container);
               Course_Elt := Course_Elt.Next;
            end loop;
         end if;
      end if;
   end Tree_Course;

   procedure Add_Attributes
       (Comp :  in out XML_Component_Access;
        Id   :  XML_String;
        Val  :  XML_String)
   is
      use Attributes_Seq;
      Attr : Attributes_Record_Access :=
         new Attributes_Record'(Tag_Id => Id, Value => Val);
   begin
      Attributes_Seq.Append (Comp.Attributes, Attr);
   end Add_Attributes;


   function Get_Attributes
       (Comp : XML_Component_Access)
       return Attributes_Seq.Sequence
   is
   begin
      return Comp.Attributes;
   end Get_Attributes;

   procedure Initialize
     (Comp : XML_Component_Access;
      Tag  : XML_String;
      Value : XML_String := XML_Null_String;
      Comp_Type : Xsd_Types := Xsd_Simple;
      Is_Method : Boolean := False)
   is
   begin
      Comp.Tag := Tag;
      Comp.Value := Value;
      Comp.Component_Type := Comp_Type;
      Comp.Is_Method := Is_Method;
   end Initialize;


   ----------------------------------
   --  Parse XML Incoming Strings
   ------------------------------------

   function Next_Token (Str : Stream_Char_Access)
     return XML_String
   is
      type Type_Of_Symbol is (None, Identifier, Text);
      State       : Type_Of_Symbol := None;

      function End_Of_Identifier (ch : Character) return Boolean;

      function End_Of_Identifier (ch : Character) return Boolean is
      begin
         return ch = '>' or
                 ch = '<' or
                 not (Is_Alphanumeric (ch) or ch = '_' or ch = ':');
      end End_Of_Identifier;
      Line : XML_String := XML_Null_String;

   begin
      while not End_Of_Input (Str) loop
         declare
            ch  : Character := Get (Str);
         begin
            case State is
               when None =>
                  if ch = Character'Val (34)  then
                     State := Text;
                  elsif Is_Alphanumeric (ch) then
                        Line := Line & ch;
                        if End_Of_Identifier (Peek (Str)) then
                           exit;
                        else
                           State := Identifier;
                        end if;

                  elsif ch = '<' then
                     Line := Line & ch;
                     ch := Peek (Str);
                     if ch = '/' or ch = '?' or ch = '!' then
                        Line := Line & Get (Str);
                     end if;
                     exit;
                  elsif ch = '/' then
                     Line := Line & ch;
                     if Peek (Str) = '>' then
                        Line := Line & Get (Str);
                        exit;
                     end if;
                  elsif ch = '>' or ch = '=' then
                     Line := Line & ch;
                     exit;
                  end if;

               when Identifier =>
                  Line := Line & ch;

                  ch := Peek (Str);
                  exit when
                      ch = '>' or ch = '<' or
                      not (Is_Alphanumeric (ch) or ch = '_' or ch = ':');

               when Text =>
                  if ch /= Character'Val (34)  then
                     Line := Line & ch;
                  else
                     exit;
                  end if;
            end case;
         end;
      end loop;
      return Line;
   end Next_Token;

   procedure Parse_Tag
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access)
   is
      Token : XML_String;
      Param : XML_String;
      Value : XML_String;
   begin
      Token := Next_Token (Str);
      XML_Comp.Tag := Token;

      while not End_Of_Input (Str) loop
         Param := Next_Token (Str);
         if Param = "/>" then
            XML_Comp.Empty := True;
            exit;
         elsif Param = ">" then
            exit;
         end if;

         Token := Next_Token (Str);
         if Token = "=" then
            Value := Next_Token (Str);
            Add_Attributes (XML_Comp, Param, Value);
         end if;
      end loop;

   end Parse_Tag;


   procedure Parse_Component
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access)
   is
      Token : XML_String;
   begin
      Parse_Tag (XML_Comp, Str);
      if not XML_Comp.Empty then

         Token := Next_Token (Str);
         while not End_Of_Input (Str) loop
            exit when Token = "</";

            if Token = "<" then
               declare
                  Child   : XML_Component_Access := new XML_Component;
               begin
                  Add_Child (XML_Comp, Child);
                  Set_Parent (Child, XML_Comp);
                  Parse_Component (Child, Str);
               end;
            else
               XML_Comp.Value := XML_Comp.Value & " " & Token;
            end if;

            Token := Next_Token (Str);
         end loop;

         Token := Next_Token (Str);
         if Token /= XML_Comp.Tag then
            raise Unexpected_Token;
         end if;
         Token := Next_Token (Str);
         if Token /= ">" then
            raise Unexpected_Token;
         end if;
      end if;

   end Parse_Component;


   procedure XML_Parse
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access)
   is
      Token : XML_String;
   begin
      Token := Next_Token (Str);

      if Token /= To_Droopi_String ("<") then
         raise Unexpected_Token;
      end if;
      Parse_Component (XML_Comp, Str);
   end XML_Parse;



end Droopi.Representations.SOAP;



