-----------------------------------------------------------------------------
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
with Ada.Exceptions;

with DOM.Core.Nodes;
with Sax.Readers;

with PolyORB.Any;
with PolyORB.Any.ObjRef;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.SOAP;
with PolyORB.References;
with PolyORB.Types;

with SOAP.Message.Reader;
with SOAP.Message.Response.Error;
with SOAP.Types;

package body SOAP.Message.XML is

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
      A_UByte, A_Float, A_Double, A_String,
      A_Boolean, A_Time_Instant, A_Base64);

   type Message_Kind is (Payload, Response);

   type State is record
      Wrapper_Name  : Unbounded_String;
      Parameters    : SOAP.Parameters.List;
      Kind          : Message_Kind;
      A_State       : Array_State := Void;
   end record;

   procedure Parse_Envelope (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Document (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Body     (N : in DOM.Core.Node; S : in out State);

   procedure Parse_Wrapper  (N : in DOM.Core.Node; S : in out State);

   --------------------------------------
   -- Elementary data parsing routines --
   --------------------------------------

   procedure Parse_Int
     (N  : in DOM.Core.Node;
      NV : in out NamedValue);

   procedure Parse_Short
     (N : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UInt
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UShort
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UByte
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Float
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);
   procedure Parse_Double
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_String
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_ObjRef
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Boolean
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Null
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Enum
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

--    function Parse_Base64    (N : in DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

--    function Parse_Time_Instant
--      (N : in DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

   --------------------------------------------------------
   -- Aggregates and data containers parsing subprograms --
   --------------------------------------------------------

   function Parse_Param
     (N        : in DOM.Core.Node;
      S        : in State;
      Expected_Type : in PolyORB.Any.TypeCode.Object)
     return PolyORB.Any.NamedValue;

--    function Parse_Array
--      (N : in DOM.Core.Node;
--       S : in State)
--      return PolyORB.Any.NamedValue;

   function Parse_Record
     (N : in DOM.Core.Node;
      S : in State;
      Expected_Type : in PolyORB.Any.TypeCode.Object)
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

   function Load_Payload
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in     PolyORB.Any.NVList.Ref)
     return Message.Payload.Object
   is
      Reader  : Tree_Reader;
      S       : State;
      Doc     : DOM.Core.Document;
   begin
      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source.all);

      Doc := Get_Tree (Reader);

      S.Parameters := SOAP.Parameters.List'(Args with null record);
      S.Kind := Payload;
      Parse_Document (Doc, S);

      Free (Doc);

      return Message.Payload.Build (To_String (S.Wrapper_Name), S.Parameters);
   end Load_Payload;

   -------------------
   -- Load_Response --
   -------------------

   function Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in     PolyORB.Any.NVList.Ref)
     return Message.Response.Object'Class
   is
      Reader  : Tree_Reader;
      S       : State;
      Doc     : DOM.Core.Document;
   begin
      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source.all);

      Doc := Get_Tree (Reader);

      SOAP.Parameters.Create (S.Parameters);
      S.Parameters := SOAP.Parameters.List'(Args with null record);
      S.Kind := Response;
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
            Faultstring => Ada.Exceptions.Exception_Message (E));
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

   procedure Parse_Boolean
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value (NV.Argument, Node_Value (Value) = "1");
   end Parse_Boolean;

   procedure Parse_Null
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
   begin
      null;
   end Parse_Null;

   procedure Parse_Enum
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      use PolyORB.Any.TypeCode;
      use type DOM.Core.Node;

   begin
      declare
         TC : constant PolyORB.Any.TypeCode.Object
           := Get_Precise_Type (NV.Argument);
         A : PolyORB.Any.Any
           := Get_Empty_Any_Aggregate (Get_Type (NV.Argument));

         Atts : constant DOM.Core.Named_Node_Map := Attributes (N);
         Value : constant DOM.Core.Node := First_Child (N);

         Enumerator_Id_Node : constant DOM.Core.Node
           := Get_Named_Item (Atts, "id");
         Enumerator_Literal : constant String := Node_Value (Value);
      begin
         if Enumerator_Id_Node /= null then
            Add_Aggregate_Element
              (A, To_Any (Unsigned_Long'Value
                          (Node_Value (Enumerator_Id_Node)) - 1));
         else
            for I in 1 .. Member_Count (TC) loop
               declare
                  Enumerator : constant PolyORB.Types.String
                    := From_Any (Get_Parameter (TC, I + 1));
               begin
                  if Enumerator_Literal = To_Standard_String
                    (Enumerator)
                  then
                     Add_Aggregate_Element (A, To_Any (I - 1));
                     exit;
                  end if;
               end;
            end loop;
         end if;
         Copy_Any_Value (NV.Argument, A);
      end;
   end Parse_Enum;

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

   procedure Parse_Float
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Float'Value
         (Node_Value (Value)));
   end Parse_Float;

   procedure Parse_Double
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Double'Value
         (Node_Value (Value)));
   end Parse_Double;

   ---------------
   -- Parse_Int --
   ---------------

   procedure Parse_Int
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Long'Value
         (Node_Value (Value)));
   end Parse_Int;

   procedure Parse_Short
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Short'Value
         (Node_Value (Value)));
   end Parse_Short;

   procedure Parse_UInt
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Unsigned_Long'Value
         (Node_Value (Value)));
   end Parse_UInt;

   procedure Parse_UShort
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Unsigned_Short'Value
         (Node_Value (Value)));
   end Parse_UShort;

   procedure Parse_UByte
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (NV.Argument,
         PolyORB.Types.Octet'Value
         (Node_Value (Value)));
   end Parse_UByte;

   ------------------
   -- Parse_String --
   ------------------

   procedure Parse_String
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : DOM.Core.Node;
   begin
      Normalize (N);
      Value := First_Child (N);
      Set_Any_Value
        (NV.Argument, To_PolyORB_String (Node_Value (Value)));
   end Parse_String;

   procedure Parse_ObjRef
     (N  : in     DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      P : PolyORB.Binding_Data.Profile_Access;
      R : PolyORB.References.Ref;
      use PolyORB.Any;
   begin
      Normalize (N);
      P := PolyORB.Binding_Data.SOAP.Create_Profile
        (To_PolyORB_String (Node_Value (First_Child (N))));
      PolyORB.References.Create_Reference
        (Profiles => (1 => P),
         Type_Id  => To_Standard_String
         (TypeCode.Id (Get_Type (NV.Argument))),
         R        => R);
      PolyORB.Any.ObjRef.Set_Any_Value (NV.Argument, R);
   end Parse_ObjRef;

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

   -----------------
   -- Parse_Param --
   -----------------

   function Parse_Param
     (N        : in DOM.Core.Node;
      S        : in State;
      Expected_Type : in PolyORB.Any.TypeCode.Object)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;

      Atts : constant DOM.Core.Named_Node_Map := Attributes (N);
      NV : PolyORB.Any.NamedValue;
   begin
      if To_String (S.Wrapper_Name) = "Fault" then
         NV.Name := To_PolyORB_String (Local_Name (N));
         NV.Argument := Get_Empty_Any (TC_String);
         Parse_String (N, NV);
         return NV;
      end if;

      if Length (Atts) = 0 and then S.A_State = Void then
         --  No attributes, this is a SOAP record since we are not parsing
         --  arrays entries.

         return Parse_Record (N, S, Expected_Type);
      end if;

      case S.A_State is
            --  XXX PARSING ARRAYS: not implemened.
--                when A_Int =>
--                   return Parse_Int (N);
--                when A_Short =>
--                   return Parse_Short (N);

--                when A_UInt =>
--                   return Parse_UInt (N);
--                when A_UShort =>
--                   return Parse_UShort (N);
--                when A_UByte =>
--                   return Parse_UByte (N);

--                when A_Float =>
--                   return Parse_Float (N);
--                when A_Double =>
--                   return Parse_Double (N);

--                when A_String =>
--                   return Parse_String (N);

--                when A_AnyURI =>
--                   return Parse_AnyURI (N);

--                when A_Boolean =>
--                   return Parse_Boolean (N);

--                when A_Time_Instant =>
--                   --  XXX return Parse_Time_Instant (N);
--                   raise PolyORB.Not_Implemented;

--                when A_Base64 =>
--                   --  XXX return Parse_Base64 (N);
--                   raise PolyORB.Not_Implemented;

         when Void | A_Undefined =>

            declare
               XSI_Type : constant DOM.Core.Node
                 := Get_Named_Item (Atts, "xsi:type");
            begin
               if XSI_Type = null then

                  if Get_Named_Item (Atts, "xsi:null") /= null then
                     NV.Name := To_PolyORB_String (Local_Name (N));
                     NV.Argument := Get_Empty_Any (TC_Void);
                  elsif PolyORB.Any.TypeCode.Kind
                    (Expected_Type) = Tk_Enum
                  then
                     NV.Name := To_PolyORB_String (Local_Name (N));
                     NV.Argument := Get_Empty_Any (Expected_Type);
                     Parse_Enum (N, NV);
                  else
                     Error (N, "Wrong or not supported type");
                     --  Error has raised an exception.
                  end if;
                  return NV;

               else

                  declare
                     xsd : constant String := Node_Value (XSI_Type);
                  begin

                     NV.Name := To_PolyORB_String (Local_Name (N));
                     NV.Argument := Get_Empty_Any (Expected_Type);

                     if xsd = Types.XML_Int then
                        Parse_Int (N, NV);
                     elsif xsd = Types.XML_Short then
                        Parse_Short (N, NV);

                     elsif xsd = Types.XML_UInt then
                        Parse_UInt (N, NV);
                     elsif xsd = Types.XML_UShort then
                        Parse_UShort (N, NV);
                     elsif xsd = Types.XML_UByte then
                        Parse_UByte (N, NV);

                     elsif xsd = Types.XML_Float then
                        Parse_Float (N, NV);
                     elsif xsd = Types.XML_Double then
                        Parse_Double (N, NV);

                     elsif xsd = Types.XML_String then
                        Parse_String (N, NV);

                     elsif xsd = Types.XML_Boolean then
                        Parse_Boolean (N, NV);
                     elsif TypeCode.Kind (Expected_Type) = Tk_Objref
                       and then xsd
                       = To_Standard_String (TypeCode.Id (Expected_Type))
                     then
                        Parse_ObjRef (N, NV);
                     else
                        Error (N, "Wrong or not supported type");
                     end if;
                  end;
               end if;
            end;
         when others =>
            raise PolyORB.Not_Implemented;
      end case;

      return NV;
   end Parse_Param;

   ------------------
   -- Parse_Record --
   ------------------

   function Parse_Record
     (N : in DOM.Core.Node;
      S : in State;
      Expected_Type : in PolyORB.Any.TypeCode.Object)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;
      use SOAP.Types;
      use PolyORB.Any.TypeCode;

      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));

      Any_Record : Any
        := Get_Empty_Any_Aggregate (Expected_Type);

      Field : DOM.Core.Node;
      I : Unsigned_Long := 1;
   begin
      Field := First_Child (N);

      while Field /= null loop
         declare
            Field_TC : constant PolyORB.Any.TypeCode.Object
              := Member_Type (Expected_Type, I);
            Field_Value : constant NamedValue
              := Parse_Param (Field, S, Field_TC);
         begin
            Add_Aggregate_Element (Any_Record, Field_Value.Argument);
         end;
         I := I + 1;
         Field := Next_Sibling (Field);
      end loop;

      return NamedValue'
        (Name => Name,
         Argument => Any_Record,
         Arg_Modes => ARG_IN);
   end Parse_Record;

   -------------------
   -- Parse_Wrapper --
   -------------------

   procedure Parse_Wrapper (N : in DOM.Core.Node; S : in out State) is
      use type SOAP.Parameters.List;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;

      NL   : constant DOM.Core.Node_List := Child_Nodes (N);
      Name : constant String := Local_Name (N);

      Args_Index : Integer := 0;
      Args_List : constant NV_Sequence_Access
        := List_Of (PolyORB.Any.NVList.Ref (S.Parameters));
      NV : PolyORB.Any.NamedValue;
   begin
      S.Wrapper_Name := To_Unbounded_String (Name);

      for K in 0 .. Length (NL) - 1 loop
         Args_Index := Args_Index + 1;
         loop
            --  Ignore any element in S.Args that is not of the
            --  proper mode (i.e. OUT elements when parsing a
            --  request, IN elements when parsing a response;
            --  INOUT elements are never skipped.)

            NV := Element_Of (Args_List.all, Args_Index);
            exit when NV.Arg_Modes = ARG_INOUT
              or else (S.Kind = Payload xor NV.Arg_Modes = ARG_OUT);
            Args_Index := Args_Index + 1;
         end loop;

         Copy_Any_Value
           (NV.Argument,
            Parse_Param
            (Item (NL, K), S, Get_Type (NV.Argument)).Argument);
      end loop;
   end Parse_Wrapper;

   -----------
   -- Error --
   -----------

   procedure Error (Node : in DOM.Core.Node; Message : in String) is
      Name : constant String := Local_Name (Node);
   begin
      Ada.Exceptions.Raise_Exception
        (SOAP_Error'Identity, Name & " - " & Message);
   end Error;

end SOAP.Message.XML;
