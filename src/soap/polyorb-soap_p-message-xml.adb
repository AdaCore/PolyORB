------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S O A P _ P . M E S S A G E . X M L            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with DOM.Core.Nodes;
with Sax.Readers;

with PolyORB.Any;
with PolyORB.Any.ObjRef;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Log;
with PolyORB.References;
with PolyORB.Types;

with PolyORB.SOAP_P.Message.Reader;
with PolyORB.SOAP_P.Message.Response.Error;
with PolyORB.SOAP_P.Types;

package body PolyORB.SOAP_P.Message.XML is

   use DOM.Core.Nodes;
   use SOAP_P.Message.Reader;

   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("soap.message.xml");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
   pragma Warnings (Off);
   pragma Unreferenced
     (A_Int, A_Short, A_UInt, A_UShort,
      A_UByte, A_Float, A_Double, A_String,
      A_Boolean, A_Time_Instant, A_Base64);
   --  Parsing of arrays is currently disabled.
   pragma Warnings (On);

   type Message_Kind is (Payload, Response);

   type State is record
      Wrapper_Name  : Unbounded_String;
      Parameters    : SOAP_P.Parameters.List;
      Kind          : Message_Kind;
      A_State       : Array_State := Void;
   end record;

   procedure Parse_Envelope (N : DOM.Core.Node; S : in out State);

   procedure Parse_Document (N : DOM.Core.Node; S : in out State);

   procedure Parse_Body     (N : DOM.Core.Node; S : in out State);

   procedure Parse_Wrapper  (N : DOM.Core.Node; S : in out State);

   --------------------------------------
   -- Elementary data parsing routines --
   --------------------------------------

   procedure Parse_Int
     (N  : DOM.Core.Node;
      NV : in out NamedValue);

   procedure Parse_Short
     (N : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UInt
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UShort
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_UByte
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Float
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);
   procedure Parse_Double
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_String
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Char
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_ObjRef
     (N       : DOM.Core.Node;
      NV      : in out PolyORB.Any.NamedValue;
      Type_Id : String);

   procedure Parse_Boolean
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

   procedure Parse_Enum
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue);

--    function Parse_Base64    (N : DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

--    function Parse_Time_Instant
--      (N : DOM.Core.Node)
--      return PolyORB.Any.NamedValue;

   --------------------------------------------------------
   -- Aggregates and data containers parsing subprograms --
   --------------------------------------------------------

   function Parse_Param
     (N        : DOM.Core.Node;
      S        : State;
      Expected_Type : PolyORB.Any.TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue;
   --  Parse any parameter. This is notionally the Unmashall_To_Any
   --  representation operation.

--    function Parse_Array
--      (N : DOM.Core.Node;
--       S : State)
--      return PolyORB.Any.NamedValue;

   function Parse_Record
     (N : DOM.Core.Node;
      S : State;
      Expected_Type : PolyORB.Any.TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue;

   function Parse_Sequence
     (N : DOM.Core.Node;
      S : State;
      Expected_Type : PolyORB.Any.TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue;

   procedure Error (Node : DOM.Core.Node; Message : String);
   pragma No_Return (Error);
   --  Raises SOAP_Error with the Message as exception message.

   -----------
   -- Image --
   -----------

   function Image (Obj : Object'Class) return String is
   begin
      return To_String (XML.Image (Obj));
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Obj : Object'Class)
     return Unbounded_String
   is
      Message_Body : Unbounded_String;
   begin
      --  Header

      Append (Message_Body, XML_Header & NL);
      Append (Message_Body, Header & NL);

      --  Body

      Append (Message_Body, Start_Body & NL);

      --  Wrapper

      Append (Message_Body, Message.XML_Image (Obj));

      --  End of Body and Envelope

      Append (Message_Body, End_Body & NL);
      Append (Message_Body, End_Env & NL);

      return Message_Body;
   end Image;

   ------------------
   -- Load_Payload --
   ------------------

   procedure Load_Payload
     (Source  : access Input_Sources.Input_Source'Class;
      Args    : in out PolyORB.Any.NVList.Ref;
      R_Payload :    out Message.Payload.Object_Access)
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

      S.Parameters := SOAP_P.Parameters.List'(Args with null record);
      S.Kind := Payload;
      Parse_Document (Doc, S);
      Args := PolyORB.Any.NVList.Ref (S.Parameters);
      --  May have been modified by Parse_Document (if it was
      --  initially empty).

      Free (Doc);

      R_Payload := new Message.Payload.Object'
        (Message.Payload.Build
         (To_String (S.Wrapper_Name), S.Parameters));
   end Load_Payload;

   -------------------
   -- Load_Response --
   -------------------

   function Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : PolyORB.Any.NVList.Ref)
     return Message.Response.Object_Access
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

      SOAP_P.Parameters.Create (S.Parameters);
      S.Parameters := SOAP_P.Parameters.List'(Args with null record);
      S.Kind := Response;
      Parse_Document (Doc, S);

      Free (Doc);

      if SOAP_P.Parameters.Exist (S.Parameters, "faultcode") then
         return new Message.Response.Error.Object'
           (Message.Response.Error.Build
            (Faultcode   =>
               Message.Response.Error.Faultcode
             (String'(SOAP_P.Parameters.Get (S.Parameters, "faultcode"))),
             Faultstring => SOAP_P.Parameters.Get
             (S.Parameters, "faultstring")));
      else
         return new Message.Response.Object'
            (Null_Unbounded_String,
             S.Wrapper_Name,
             S.Parameters);
      end if;

   exception
      when E : others =>
         return new Message.Response.Error.Object'
           (Message.Response.Error.Build
            (Faultcode   => Message.Response.Error.Client,
             Faultstring => Ada.Exceptions.Exception_Message (E)));
   end Load_Response;

   procedure Load_Response
     (Source : access Input_Sources.Input_Source'Class;
      Args   : in out PolyORB.Any.NVList.Ref)
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

      SOAP_P.Parameters.Create (S.Parameters);
      S.Parameters := SOAP_P.Parameters.List'(Args with null record);
      S.Kind := Response;
      Parse_Document (Doc, S);
      Args := PolyORB.Any.NVList.Ref (S.Parameters);
      --  May have been modified by Parse_Document (if it was
      --  initially empty).

      Free (Doc);
   end Load_Response;

   -----------------
   -- Parse_Array --
   -----------------

--    function Parse_Array
--      (N : DOM.Core.Node;
--       S : State)
--      return PolyORB.Any.NamedValue
--    is
--       use type DOM.Core.Node;
--       use SOAP.Types;

--       function A_State (A_Type : String) return Array_State;
--       --  Returns the Array_State given the SOAP-ENC:arrayType value.

--       function A_State (A_Type : String) return Array_State is
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
--      (N : DOM.Core.Node) return PolyORB.Any.NamedValue is
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

   procedure Parse_Body (N : DOM.Core.Node; S : in out State) is
   begin
      Parse_Wrapper (First_Child (N), S);
   end Parse_Body;

   -------------------
   -- Parse_Boolean --
   -------------------

   procedure Parse_Boolean
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (Node_Value (Value) = "1", Get_Container (NV.Argument).all);
   end Parse_Boolean;

   procedure Parse_Enum
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      use PolyORB.Any.TypeCode;
      use type DOM.Core.Node;

   begin
      declare
         TC : constant PolyORB.Any.TypeCode.Local_Ref
           := Get_Unwound_Type (NV.Argument);
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
                    := From_Any (Get_Parameter (TC, I + 1).all);
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
         Move_Any_Value (NV.Argument, A);
      end;
   end Parse_Enum;

   --------------------
   -- Parse_Sequence --
   --------------------

   function Parse_Sequence
     (N : DOM.Core.Node;
      S : State;
      Expected_Type : PolyORB.Any.TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue
   is
      use PolyORB.Any.TypeCode;
      use type DOM.Core.Node;

      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));

      A : PolyORB.Any.Any := Get_Empty_Any_Aggregate
        (Expected_Type);

      Unwound_Expected_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                                Unwind_Typedefs (Expected_Type);

      Content_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                       TypeCode.Content_Type (Unwound_Expected_Type);

      Values : constant DOM.Core.Node_List := Child_Nodes (N);
      Length : constant Unsigned_Long :=
                 Unsigned_Long (DOM.Core.Nodes.Length (Values));
      Bound  : constant Unsigned_Long :=
                 PolyORB.Any.TypeCode.Length (Unwound_Expected_Type);
      Child : DOM.Core.Node := First_Child (N);
   begin
      if Bound > 0 and then Length > Bound then
         raise Constraint_Error;
      end if;

      Add_Aggregate_Element (A, To_Any (Length));
      for I in 1 .. Length loop
         Add_Aggregate_Element
           (A, Parse_Param (Child, S, Content_Type).Argument);
         Child := Next_Sibling (Child);
      end loop;

      return (Name => Name, Argument => A, Arg_Modes => ARG_IN);
   end Parse_Sequence;

   --------------------
   -- Parse_Document --
   --------------------

   procedure Parse_Document (N : DOM.Core.Node; S : in out State) is
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

   procedure Parse_Envelope (N : DOM.Core.Node; S : in out State) is
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
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Float'Value (Node_Value (Value)),
         Get_Container (NV.Argument).all);
   end Parse_Float;

   ------------------
   -- Parse_Double --
   ------------------

   procedure Parse_Double
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Double'Value (Node_Value (Value)),
         Get_Container (NV.Argument).all);
   end Parse_Double;

   ---------------
   -- Parse_Int --
   ---------------

   procedure Parse_Int
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Long'Value (Node_Value (Value)),
        Get_Container (NV.Argument).all);
   end Parse_Int;

   -----------------
   -- Parse_Short --
   -----------------

   procedure Parse_Short
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Short'Value (Node_Value (Value)),
         Get_Container (NV.Argument).all);
   end Parse_Short;

   ----------------
   -- Parse_UInt --
   ----------------

   procedure Parse_UInt
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
           (PolyORB.Types.Unsigned_Long'Value (Node_Value (Value)),
            Get_Container (NV.Argument).all);
   end Parse_UInt;

   ------------------
   -- Parse_UShort --
   ------------------

   procedure Parse_UShort
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Unsigned_Short'Value (Node_Value (Value)),
         Get_Container (NV.Argument).all);
   end Parse_UShort;

   -----------------
   -- Parse_UByte --
   -----------------

   procedure Parse_UByte
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Set_Any_Value
        (PolyORB.Types.Octet'Value (Node_Value (Value)),
         Get_Container (NV.Argument).all);
   end Parse_UByte;

   ------------------
   -- Parse_String --
   ------------------

   procedure Parse_String
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      use type DOM.Core.Node;

      Value : DOM.Core.Node;
      Bound : constant PolyORB.Types.Unsigned_Long :=
                TypeCode.Length (Get_Unwound_Type (NV.Argument));
   begin
      Normalize (N);
      Value := First_Child (N);
      if Value /= null then
         declare
            S : constant String := Node_Value (Value);
         begin
            if Bound > 0 and then S'Length > Bound then
               raise Constraint_Error;
            end if;
            Set_Any_Value
              (To_PolyORB_String (Node_Value (Value)),
               Get_Container (NV.Argument).all);
         end;
      else
         Set_Any_Value
           (To_PolyORB_String (""), Get_Container (NV.Argument).all);
      end if;
   end Parse_String;

   ----------------
   -- Parse_Char --
   ----------------

   procedure Parse_Char
     (N  : DOM.Core.Node;
      NV : in out PolyORB.Any.NamedValue)
   is
      use type DOM.Core.Node;

      Value : DOM.Core.Node;

   begin
      Normalize (N);
      Value := First_Child (N);
      if Value /= null then
         declare
            Str : constant String := Node_Value (Value);
         begin
            if Str'Length = 1 then
               Set_Any_Value
                 (PolyORB.Types.Char (Str (Str'First)),
                  Get_Container (NV.Argument).all);
               return;
            end if;
         end;
      end if;
      raise Constraint_Error;
   end Parse_Char;

   ------------------
   -- Parse_ObjRef --
   ------------------

   procedure Parse_ObjRef
     (N       : DOM.Core.Node;
      NV      : in out PolyORB.Any.NamedValue;
      Type_Id : String)
   is
      P : PolyORB.Binding_Data.Profile_Access;
      R : PolyORB.References.Ref;
   begin
      pragma Debug (O ("Parse_ObjRef: Type_Id = " & Type_Id));
      Normalize (N);
      P := PolyORB.Binding_Data.SOAP.Create_Profile
        (To_PolyORB_String (Node_Value (First_Child (N))));
      PolyORB.References.Create_Reference
        (Profiles => (1 => P),
         Type_Id  => Type_Id,
         R        => R);
      PolyORB.Any.ObjRef.Set_Any_Value (R, Get_Container (NV.Argument).all);
   end Parse_ObjRef;

   ------------------------
   -- Parse_Time_Instant --
   ------------------------

--    function Parse_Time_Instant
--      (N : DOM.Core.Node)
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
     (N        : DOM.Core.Node;
      S        : State;
      Expected_Type : PolyORB.Any.TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;

      Atts : constant DOM.Core.Named_Node_Map := Attributes (N);
      NV : PolyORB.Any.NamedValue;

      procedure Get_Empty_Any_With_Default
        (Expected_Type,
           Default_Expected_Type : PolyORB.Any.TypeCode.Local_Ref;
         NV : in out PolyORB.Any.NamedValue);

      procedure Get_Empty_Any_With_Default
        (Expected_Type,
           Default_Expected_Type : PolyORB.Any.TypeCode.Local_Ref;
         NV : in out PolyORB.Any.NamedValue)
      is
         TCK : constant TCKind := TypeCode.Kind (Expected_Type);
      begin
         if TCK /= Tk_Void then
            NV.Argument := Get_Empty_Any (Expected_Type);
         else
            NV.Argument := Get_Empty_Any (Default_Expected_Type);
            NV.Arg_Modes := ARG_IN;
         end if;
      end Get_Empty_Any_With_Default;

   begin
      if To_String (S.Wrapper_Name) = "Fault" then
         NV.Name := To_PolyORB_String (Local_Name (N));
         NV.Argument := Get_Empty_Any (TC_String);
         Parse_String (N, NV);
         return NV;
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
               Expected_TCKind : constant TCKind
                 := PolyORB.Any.TypeCode.Kind
                 (Unwind_Typedefs (Expected_Type));
            begin
               if XSI_Type = null then

                  if Get_Named_Item (Atts, "xsi:null") /= null then
                     NV.Name := To_PolyORB_String (Local_Name (N));
                     NV.Argument := Get_Empty_Any (TC_Void);
                  else
                     case Expected_TCKind is
                        when Tk_Enum =>
                           NV.Name := To_PolyORB_String (Local_Name (N));
                           NV.Argument := Get_Empty_Any (Expected_Type);
                           Parse_Enum (N, NV);

                        when Tk_Sequence =>
                           return Parse_Sequence (N, S, Expected_Type);

                        when Tk_Struct =>
                           return Parse_Record (N, S, Expected_Type);

                        when others =>
                           Error (N, "Wrong or not supported type, expected "
                                  & TCKind'Image (Expected_TCKind));
                           --  Raises an exception.
                     end case;
                  end if;

                  return NV;

               else

                  declare
                     xsd : constant String := Node_Value (XSI_Type);
                  begin

                     NV.Name := To_PolyORB_String (Local_Name (N));

                     if xsd = Types.XML_Int then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Long, NV);
                        Parse_Int (N, NV);
                     elsif xsd = Types.XML_Short then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Short, NV);
                        Parse_Short (N, NV);

                     elsif xsd = Types.XML_UInt then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Unsigned_Long, NV);

                        Parse_UInt (N, NV);
                     elsif xsd = Types.XML_UShort then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Unsigned_Short, NV);
                        Parse_UShort (N, NV);
                     elsif xsd = Types.XML_UByte then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Octet, NV);
                        Parse_UByte (N, NV);

                     elsif xsd = Types.XML_Float then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Float, NV);
                        Parse_Float (N, NV);
                     elsif xsd = Types.XML_Double then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Double, NV);
                        Parse_Double (N, NV);

                     elsif xsd = Types.XML_String then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_String, NV);

                        if Expected_TCKind = Tk_Char then
                           Parse_Char (N, NV);
                        else
                           Parse_String (N, NV);
                        end if;
                     elsif xsd = Types.XML_Boolean then
                        Get_Empty_Any_With_Default
                          (Expected_Type,
                           PolyORB.Any.TC_Boolean, NV);
                        Parse_Boolean (N, NV);

                     elsif Expected_TCKind = Tk_Objref then
                        NV.Argument := Get_Empty_Any (Expected_Type);
                        Parse_ObjRef (N, NV, Type_Id => xsd);
                     else
                        Error (N, "Wrong or not supported type");
                     end if;
                  end;
               end if;
            end;
         when others =>
            raise Program_Error;
      end case;

      return NV;
   end Parse_Param;

   ------------------
   -- Parse_Record --
   ------------------

   function Parse_Record
     (N : DOM.Core.Node;
      S : State;
      Expected_Type : TypeCode.Local_Ref)
     return PolyORB.Any.NamedValue
   is
      use type DOM.Core.Node;
      use SOAP_P.Types;
      use PolyORB.Any.TypeCode;

      Unwound_Expected_Type : constant TypeCode.Local_Ref
        := Unwind_Typedefs (Expected_Type);

      Name  : constant PolyORB.Types.Identifier
        := To_PolyORB_String (Local_Name (N));

      Any_Record : PolyORB.Any.Any := Get_Empty_Any_Aggregate (Expected_Type);

      Field : DOM.Core.Node;
      I : Unsigned_Long := 0;
   begin
      pragma Debug (O ("Parse_Record: enter"));
      Field := First_Child (N);

      while Field /= null loop
         pragma Debug (O ("Parsing field" & Unsigned_Long'Image (I)));
         declare
            Field_TC : constant PolyORB.Any.TypeCode.Local_Ref
              := Member_Type (Unwound_Expected_Type, I);
            Field_Value : constant NamedValue
              := Parse_Param (Field, S, Field_TC);
         begin
            Add_Aggregate_Element (Any_Record, Field_Value.Argument);
         end;
         I := I + 1;
         Field := Next_Sibling (Field);
      end loop;

      pragma Debug (O ("Parse_Record: leaver"));
      return NamedValue'
        (Name => Name,
         Argument => Any_Record,
         Arg_Modes => ARG_IN);
   end Parse_Record;

   -------------------
   -- Parse_Wrapper --
   -------------------

   procedure Parse_Wrapper (N : DOM.Core.Node; S : in out State) is
      use type SOAP_P.Parameters.List;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      NL   : constant DOM.Core.Node_List := Child_Nodes (N);
      Name : constant String := Local_Name (N);

      It : Iterator;

      Constructing_Args_List : Boolean;
      --  True iff the args list is initially empty and we have
      --  to determine the types of the arguments using only the
      --  XML attributes in the message.

      NV : Element_Access;
      No_TypeCode : PolyORB.Any.TypeCode.Local_Ref;
   begin
      S.Wrapper_Name := To_Unbounded_String (Name);

      Constructing_Args_List := SOAP_P.Parameters.Is_Nil (S.Parameters);

      if Constructing_Args_List then
         SOAP_P.Parameters.Create (S.Parameters);
      end if;

      It := First (List_Of (PolyORB.Any.NVList.Ref (S.Parameters)).all);

      for J in 0 .. Length (NL) - 1 loop
         if not Constructing_Args_List then
            loop

               --  Ignore any element in S.Args that is not of the
               --  proper mode (i.e. OUT elements when parsing a
               --  request, IN elements when parsing a response;
               --  INOUT elements are never skipped.)

               exit when Last (It);

               NV := Value (It);
               exit when NV.Arg_Modes = ARG_INOUT
                 or else (S.Kind = Payload xor NV.Arg_Modes = ARG_OUT);
               Next (It);
            end loop;

            exit when Last (It);

            Move_Any_Value
              (NV.Argument,
               Parse_Param
               (Item (NL, J), S, Get_Type (NV.Argument)).Argument);
         else
            SOAP_P.Parameters.Add_Item
              (S.Parameters, Parse_Param
               (Item (NL, J), S, No_TypeCode));
         end if;
      end loop;
   end Parse_Wrapper;

   -----------
   -- Error --
   -----------

   procedure Error (Node : DOM.Core.Node; Message : String) is
      Name : constant String := Local_Name (Node);
   begin
      Ada.Exceptions.Raise_Exception
        (SOAP_Error'Identity, Name & " - " & Message);
   end Error;

end PolyORB.SOAP_P.Message.XML;
