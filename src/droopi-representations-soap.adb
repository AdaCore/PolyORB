------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Representations                              --
--                                                                          --
--                               B O D Y                                    --
--                                                                          --
------------------------------------------------------------------------------



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


   procedure Add_Child
        (Comp : in out XML_Component_Access;
         Child   : XML_Component_Access)
   is
      Ce       : Container_Element_Access;
      Element  : Child_List_Access;
   begin
      if Comp.Childs = null then
         Ce := new Container_Element;
         Comp.Childs := Ce;
      end if;

      Element := new Child_List_Record;

      Element.Item := Child;

      if Comp.Childs.Head = null then
         Comp.Childs.Head :=  Element;
      else
         Comp.Childs.Tail.Next := Element;

      end if;

      Comp.Childs.Tail := Element;
      Comp.Childs.Nbr_Of_Items := Comp.Childs.Nbr_Of_Items + 1;

   end Add_Child;



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

   procedure Initialize_XML_Comp
     (Comp : XML_Component_Access;
      Tag  : XML_String;
      Comp_Type : Xsd_Types)
   is
   begin
      Comp.Tag := Tag;
      Comp.Component_Type := Comp_Type;
   end Initialize_XML_Comp;




end Droopi.Representations.SOAP;



