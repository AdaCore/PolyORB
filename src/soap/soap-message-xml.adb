------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with Ada.Strings.Unbounded;
--  with Ada.Strings.Fixed;
with Ada.Exceptions;
--  with Ada.Calendar;

with Input_Sources.Strings;
with Unicode.CES.Basic_8bit;
with DOM.Core.Nodes;
with Sax.Readers;

with PolyORB.Any;
with PolyORB.Types;

with SOAP.Message.Reader;
with SOAP.Message.Response.Error;
with SOAP.Types;

package body SOAP.Message.XML is

   use Ada;
   use Ada.Strings.Unbounded;
   use DOM.Core.Nodes;
   use SOAP.Message.Reader;
   use PolyORB.Any;
   use PolyORB.Types;

   NL         : constant String := ASCII.CR & ASCII.LF;

   XML_Header : constant String := "<?xml version=""1.0""?>";

   URL_Enc    : constant String := "http://schemas.xmlsoap.org/soap/encoding/";
   URL_Env    : constant String := "http://schemas.xmlsoap.org/soap/envelope/";
   URL_xsd    : constant String := "http://www.w3.org/1999/XMLSchema";
   URL_xsi    : constant String := "http://www.w3.org/1999/XMLSchema-instance";

   Start_Env  : constant String := "<SOAP-ENV:Envelope";
   End_Env    : constant String := "</SOAP-ENV:Envelope>";

   Header     : constant String
     := Start_Env & ' '
     & "SOAP-ENV:encodingStyle=""" & URL_Enc & """ "
     & "xmlns:SOAP-ENC=""" & URL_Enc & """ "
     & "xmlns:SOAP-ENV=""" & URL_Env & """ "
     & "xmlns:xsd=""" & URL_xsd & """ "
     & "xmlns:xsi=""" & URL_xsi & """>";

   Start_Body      : constant String := "<SOAP-ENV:Body>";
   End_Body        : constant String := "</SOAP-ENV:Body>";

   Start_Fault_Env : constant String := "<SOAP-ENV:Fault>";
   pragma Warnings (Off);
   pragma Unreferenced (Start_Fault_Env);
   pragma Warnings (On);

   type Array_State is
     (Void, A_Undefined, A_Int, A_Short, A_UInt, A_UShort,
      A_Float, A_Double, A_String,
      A_Boolean, A_Time_Instant, A_Base64);

   type State is record
      Wrapper_Name : Unbounded_String;
      Parameters   : SOAP.Parameters.List;
      A_State      : Array_State := Void;
   end record;

   procedure Parse_Envelope (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Document (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Body     (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Wrapper  (N : in DOM.Core.Node; S : in out State);

   function Parse_Int       (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;
   function Parse_Short     (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;
   function Parse_UInt      (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;
   function Parse_UShort    (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;

   function Parse_Float     (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;
   function Parse_Double    (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;

   function Parse_String    (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;

   function Parse_Boolean   (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;

   function Parse_Null      (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue;

--    function Parse_Base64    (N : in DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

--    function Parse_Time_Instant
--      (N : in DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

   function Parse_Param
     (N        : in DOM.Core.Node;
      S        : in State)
     return PolyORB.Any.NamedValue;

--    function Parse_Array
--      (N : in DOM.Core.Node;
--       S : in State)
--      return PolyORB.Any.NamedValue;

   function Parse_Record
     (N : in DOM.Core.Node;
      S : in State)
     return PolyORB.Any.NamedValue;

   procedure Error (Node : in DOM.Core.Node; Message : in String);
   pragma No_Return (Error);
   --  Raises SOAP_Error with the Message as exception message.

   -----------
   -- Image --
   -----------

   function Image (O : in Object'Class) return String is
   begin
      return To_String (XML.Image (O));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in Object'Class) return Unbounded_String is
      Message_Body : Unbounded_String;
   begin
      --  Header

      Append (Message_Body, XML_Header & NL);
      Append (Message_Body, Header & NL);

      --  Body

      Append (Message_Body, Start_Body & NL);

      --  Wrapper

      Append (Message_Body, Message.XML_Image (O));

      --  End of Body and Envelope

      Append (Message_Body, End_Body & NL);
      Append (Message_Body, End_Env & NL);

      return Message_Body;
   end Image;

   ------------------
   -- Load_Payload --
   ------------------

   function Load_Payload (XML : in String) return Message.Payload.Object is
      use Input_Sources.Strings;

      Str     : aliased String := XML;

      Source  : String_Input;
      Reader  : Tree_Reader;
      S       : State;
      Doc     : DOM.Core.Document;

   begin
      Open (Str'Unchecked_Access,
            Unicode.CES.Basic_8bit.Basic_8bit_Encoding,
            Source);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source);
      Close (Source);

      Doc := Get_Tree (Reader);

      SOAP.Parameters.Create (S.Parameters);
      Parse_Document (Doc, S);

      Free (Doc);

      return Message.Payload.Build (To_String (S.Wrapper_Name), S.Parameters);
   end Load_Payload;

   -------------------
   -- Load_Response --
   -------------------

   function Load_Response
     (XML : in String)
     return Message.Response.Object'Class
   is
      use Input_Sources.Strings;

      Str     : aliased String := XML;

      Source  : String_Input;
      Reader  : Tree_Reader;
      S       : State;
      Doc     : DOM.Core.Document;

   begin
      Open (Str'Unchecked_Access,
            Unicode.CES.Basic_8bit.Basic_8bit_Encoding,
            Source);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source);
      Close (Source);

      Doc := Get_Tree (Reader);

      SOAP.Parameters.Create (S.Parameters);
      Parse_Document (Doc, S);

      Free (Doc);

      if SOAP.Parameters.Exist (S.Parameters, "faultcode") then
         return Message.Response.Error.Build
           (Faultcode   =>
              Message.Response.Error.Faultcode
               (String'(SOAP.Parameters.Get (S.Parameters, "faultcode"))),
            Faultstring => SOAP.Parameters.Get (S.Parameters, "faultstring"));
      else
         return Message.Response.Object'(Null_Unbounded_String,
                                         S.Wrapper_Name,
                                         S.Parameters);
      end if;

   exception
      when E : others =>
         return Message.Response.Error.Build
           (Faultcode   => Message.Response.Error.Client,
            Faultstring => Exceptions.Exception_Message (E));
   end Load_Response;

   -----------------
   -- Parse_Array --
   -----------------

--    function Parse_Array
--      (N : in DOM.Core.Node;
--       S : in State)
--      return PolyORB.Any.NamedValue
--    is
--       use type DOM.Core.Node;
--       use SOAP.Types;

--       function A_State (A_Type : in String) return Array_State;
--       --  Returns the Array_State given the SOAP-ENC:arrayType value.

--       function A_State (A_Type : in String) return Array_State is
--          N : constant Positive := Strings.Fixed.Index (A_Type, "[");
--          T : constant String   := A_Type (A_Type'First .. N - 1);
--       begin
--          if T = Types.XML_Int then
--             return A_Int;

--          elsif T = Types.XML_Float then
--             return A_Float;

--          elsif T = Types.XML_String then
--             return A_String;

--          elsif T = Types.XML_Boolean then
--             return A_Boolean;

--          elsif T = Types.XML_Time_Instant then
--             return A_Time_Instant;

--         elsif T = Types.XML_Base64 then
--             return A_Base64;

--          elsif T = Types.XML_Undefined then
--             return A_Undefined;
--          end if;

--          Error (Parse_Array.N,
--            "Wrong type or not supported type in an array");
--       end A_State;

--       Name   : constant String := Local_Name (N);
--       OS     : Types.Object_Set (1 .. Max_Object_Size);
--       K      : Natural := 0;

--       Field  : DOM.Core.Node;

--       Atts   : constant DOM.Core.Named_Node_Map := Attributes (N);

--       A_Type : constant Array_State
--         := A_State (Node_Value (Get_Named_Item
--              (Atts, "SOAP-ENC:arrayType")));

--    begin
--       Field := First_Child (N);

--       while Field /= null loop
--          K := K + 1;
--          OS (K) := +Parse_Param (Field,
--                                  (S.Wrapper_Name, S.Parameters, A_Type));

--          Field := Next_Sibling (Field);
--       end loop;

--       return Types.A (OS (1 .. K), Name);
--    end Parse_Array;

--    ------------------
--    -- Parse_Base64 --
--    ------------------

--    function Parse_Base64
--      (N : in DOM.Core.Node) return PolyORB.Any.NamedValue is
--       Name  : constant String := Local_Name (N);
--       Value : DOM.Core.Node;
--    begin
--       Normalize (N);
--       Value := First_Child (N);
--       return Types.B64 (Node_Value (Value), Name);
--    end Parse_Base64;

   ----------------
   -- Parse_Body --
   ----------------

   procedure Parse_Body (N : in DOM.Core.Node; S : in out State) is
   begin
      Parse_Wrapper (First_Child (N), S);
   end Parse_Body;

   -------------------
   -- Parse_Boolean --
   -------------------

   function Parse_Boolean
     (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue
   is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any (Node_Value (Value) = "1"),
         Arg_Modes => ARG_IN);
   end Parse_Boolean;

   function Parse_Null
     (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue
   is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
   begin
      return NamedValue'
        (Name => Name,
         Argument => Get_Empty_Any (TC_Void),
         Arg_Modes => ARG_IN);
   end Parse_Null;

   --------------------
   -- Parse_Document --
   --------------------

   procedure Parse_Document (N : in DOM.Core.Node; S : in out State) is
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
   begin
      if Length (NL) = 1 then
         Parse_Envelope (First_Child (N), S);
      else
         Error (N, "Document must have a single node, found "
                & Natural'Image (Length (NL)));
      end if;
   end Parse_Document;

   --------------------
   -- Parse_Envelope --
   --------------------

   procedure Parse_Envelope (N : in DOM.Core.Node; S : in out State) is
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
   begin
      if Length (NL) = 1 then
         Parse_Body (First_Child (N), S);
      else
         Error (N, "Envelope must have a single node, found "
                & Natural'Image (Length (NL)));
      end if;
   end Parse_Envelope;

   -----------------
   -- Parse_Float --
   -----------------

   function Parse_Float (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue
   is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any
         (PolyORB.Types.Float'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_Float;

   function Parse_Double (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue
   is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any
         (PolyORB.Types.Double'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_Double;

   ---------------
   -- Parse_Int --
   ---------------

   function Parse_Int (N : in DOM.Core.Node) return PolyORB.Any.NamedValue is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any (Long'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_Int;

   function Parse_Short (N : in DOM.Core.Node) return PolyORB.Any.NamedValue is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any (Short'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_Short;

   function Parse_UInt (N : in DOM.Core.Node) return PolyORB.Any.NamedValue is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument => To_Any (Unsigned_Long'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_UInt;

   function Parse_UShort (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return NamedValue'
        (Name => Name,
         Argument =>
           To_Any (Unsigned_Short'Value (Node_Value (Value))),
         Arg_Modes => ARG_IN);
   end Parse_UShort;

   -----------------
   -- Parse_Param --
   -----------------

   function Parse_Param
     (N        : in DOM.Core.Node;
      S        : in State)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;

      --  Name : constant PolyORB.Types.Identifier
      --    := To_PolyORB_String (Local_Name (N));
      Atts : constant DOM.Core.Named_Node_Map := Attributes (N);
   begin
      if To_String (S.Wrapper_Name) = "Fault" then
         return Parse_String (N);

      else
         if Length (Atts) = 0 and then S.A_State = Void then
            --  No attributes, this is a SOAP record since we are not parsing
            --  arrays entries.

            return Parse_Record (N, S);

         else
            case S.A_State is
               when A_Int =>
                  return Parse_Int (N);
               when A_Short =>
                  return Parse_Short (N);

               when A_UInt =>
                  return Parse_UInt (N);
               when A_UShort =>
                  return Parse_UShort (N);

               when A_Float =>
                  return Parse_Float (N);
               when A_Double =>
                  return Parse_Double (N);

               when A_String =>
                  return Parse_String (N);

               when A_Boolean =>
                  return Parse_Boolean (N);

               when A_Time_Instant =>
                  --  XXX return Parse_Time_Instant (N);
                  raise PolyORB.Not_Implemented;

               when A_Base64 =>
                  --  XXX return Parse_Base64 (N);
                  raise PolyORB.Not_Implemented;

               when Void | A_Undefined =>

                  declare
                     XSI_Type : constant DOM.Core.Node
                       := Get_Named_Item (Atts, "xsi:type");
                  begin
                     if XSI_Type = null then
                        declare
                           N : constant DOM.Core.Node
                             := Get_Named_Item (Atts, "xsi:null");
                        begin
                           if N = null then
                              Error (N, "Wrong or not supported type");
                           else
                              return Parse_Null (N);
                           end if;
                        end;

                     else

                        declare
                           xsd : constant String := Node_Value (XSI_Type);
                        begin
                           if xsd = Types.XML_Int then
                              return Parse_Int (N);
                           elsif xsd = Types.XML_Short then
                              return Parse_Short (N);

                           elsif xsd = Types.XML_UInt then
                              return Parse_UInt (N);
                           elsif xsd = Types.XML_UShort then
                              return Parse_UShort (N);

                           elsif xsd = Types.XML_Float then
                              return Parse_Float (N);
                           elsif xsd = Types.XML_Double then
                              return Parse_Double (N);

                           elsif xsd = Types.XML_String then
                              return Parse_String (N);

                           elsif xsd = Types.XML_Boolean then
                              return Parse_Boolean (N);

--                            elsif xsd = Types.XML_Time_Instant then
--                               return Parse_Time_Instant (N);

--                            elsif xsd = Types.XML_Base64 then
--                               return Parse_Base64 (N);

--                            elsif xsd = Types.XML_Array then
--                               return Parse_Array (N, S);

                           else
                              Error (N, "Wrong or not supported type");
                           end if;
                        end;
                     end if;
                  end;
            end case;
         end if;
      end if;
   end Parse_Param;

   ------------------
   -- Parse_Record --
   ------------------

   function Parse_Record
     (N : in DOM.Core.Node;
      S : in State)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;
      use SOAP.Types;
      use PolyORB.Any.TypeCode;

      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      TC_Record : TypeCode.Object;
      Any_Record : Any := Get_Empty_Any_Aggregate (TC_Record);

      Field : DOM.Core.Node;
   begin
      TC_Record := TC_Struct;
      Add_Parameter (TC_Record, To_Any
                     (PolyORB.Types.String (Name)));
      Add_Parameter (TC_Record, To_Any
                     (PolyORB.Types.String ("IDL:" & Name & ":1.0")));

      Field := First_Child (N);

      while Field /= null loop
         declare
            Field_Value : constant NamedValue
              := Parse_Param (Field, S);
         begin
            Add_Parameter (TC_Record, To_Any
                           (Get_Type (Field_Value.Argument)));
            Add_Parameter (TC_Record, To_Any
                           (PolyORB.Types.String (Field_Value.Name)));

            Add_Aggregate_Element (Any_Record, Field_Value.Argument);
         end;
         Field := Next_Sibling (Field);
      end loop;
      Set_Type (Any_Record, TC_Record);

      return NamedValue'
        (Name => Name,
         Argument => Any_Record,
         Arg_Modes => ARG_IN);
   end Parse_Record;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (N : in DOM.Core.Node)
     return PolyORB.Any.NamedValue
   is
      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));
      Value : DOM.Core.Node;
   begin
      Normalize (N);
      Value := First_Child (N);
      return NamedValue'(Name => Name,
                         Argument => To_Any
                         (To_PolyORB_String (Node_Value (Value))),
                         Arg_Modes => ARG_IN);
   end Parse_String;

   ------------------------
   -- Parse_Time_Instant --
   ------------------------

--    function Parse_Time_Instant
--      (N : in DOM.Core.Node)
--      return PolyORB.Any.NamedValue
--    is
--       use Ada.Calendar;

--       Name  : constant PolyORB.Types.Identifier
--         := To_PolyORB_String (Local_Name (N));
--       Value : constant DOM.Core.Node := First_Child (N);
--       TI    : constant String := Node_Value (Value);
--    begin
--       return Types.T
--         (Time_Of (Year    => Year_Number'Value (TI (1 .. 4)),
--                   Month   => Month_Number'Value (TI (6 .. 7)),
--                   Day     => Day_Number'Value (TI (9 .. 10)),
--                   Seconds => Duration (Natural'Value (TI (12 .. 13)) * 3600
--                                        + Natural'Value (TI (15 .. 16)) * 60
--                                        + Natural'Value (TI (18 .. 19)))),
--          Name,
--          Types.TZ'Value (TI (20 .. 22)));
--    end Parse_Time_Instant;

   -------------------
   -- Parse_Wrapper --
   -------------------

   procedure Parse_Wrapper (N : in DOM.Core.Node; S : in out State) is
      use type SOAP.Parameters.List;

      NL   : constant DOM.Core.Node_List := Child_Nodes (N);
      Name : constant String := Local_Name (N);
   begin
      S.Wrapper_Name := To_Unbounded_String (Name);

      for K in 0 .. Length (NL) - 1 loop
         S.Parameters := S.Parameters & Parse_Param (Item (NL, K), S);
      end loop;
   end Parse_Wrapper;

   -----------
   -- Error --
   -----------

   procedure Error (Node : in DOM.Core.Node; Message : in String) is
      Name : constant String := Local_Name (Node);
   begin
      Exceptions.Raise_Exception (SOAP_Error'Identity, Name & " - " & Message);
   end Error;

end SOAP.Message.XML;
