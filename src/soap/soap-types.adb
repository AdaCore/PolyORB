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

with Ada.Long_Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with PolyORB.Any.ObjRef;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Types;
with PolyORB.Utils;

with SOAP.Utils;

package body SOAP.Types is

   use Ada;
   use PolyORB.Any;
   use PolyORB.Types;

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("soap.types");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function xsi_type (Name : in String) return String;
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

--    ---------
--    -- "+" --
--    ---------

--    function "+" (O : in Object'Class) return Object_Controlled is
--    begin
--       return (Finalization.Controlled with new Object'Class'(O));
--    end "+";

--    -------
--    -- A --
--    -------

--    function A
--      (V    : in Object_Set;
--       Name : in String)
--      return SOAP_Array is
--    begin
--       return (To_Unbounded_String (Name),
--               (Finalization.Controlled with new Object_Set'(V)));
--    end A;

--    ------------
--    -- Adjust --
--    ------------

--    procedure Adjust (O : in out Object_Controlled) is
--    begin
--       if O.O /= null then
--          O.O := new Object'Class'(O.O.all);
--       end if;
--    end Adjust;

--    procedure Adjust (O : in out Object_Set_Controlled) is
--    begin
--       if O.O /= null then
--          O.O := new Object_Set'(O.O.all);
--       end if;
--    end Adjust;

--    -------
--    -- B --
--    -------

--    function B
--      (V    : in Boolean;
--       Name : in String  := "item")
--      return XSD_Boolean is
--    begin
--       return (To_Unbounded_String (Name), V);
--    end B;

--    ---------
--    -- B64 --
--    ---------

--    function B64
--      (V      : in String;
--       Name   : in String  := "item")
--      return SOAP_Base64 is
--    begin
--       return (To_Unbounded_String (Name), To_Unbounded_String (V));
--    end B64;

--    -------
--    -- F --
--    -------

--    function F
--      (V    : in Long_Float;
--       Name : in String := "item")
--      return XSD_Float is
--    begin
--       return (To_Unbounded_String (Name), V);
--    end F;

--    --------------
--    -- Finalize --
--    --------------

--    procedure Finalize (O : in out Object_Controlled) is
--       procedure Free is
--          new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
--    begin
--       if O.O /= null then
--          Free (O.O);
--       end if;
--    end Finalize;

--    procedure Finalize (O : in out Object_Set_Controlled) is
--       procedure Free is
--          new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);
--    begin
--       if O.O /= null then
--          Free (O.O);
--       end if;
--    end Finalize;

   function TCK (A : Any) return TCKind;
   function UTCK (A : Any) return TCKind;
   --  Return the typecode kind of A. UTCK returns the
   --  kind after unwinding all levels of typedef.

   function TCK (A : Any) return TCKind is
   begin
      return TypeCode.Kind (Get_Type (A));
   end TCK;

   function UTCK (A : Any) return TCKind is
   begin
      return TypeCode.Kind (Get_Unwound_Type (A));
   end UTCK;

   ---------
   -- Get --
   ---------

   function Get (O : in NamedValue) return Integer is
      Kind : constant TCKind := UTCK (O.Argument);
   begin
      case Kind is
         when Tk_Short =>
            return Integer (Short'(From_Any (O.Argument)));
         when Tk_Long =>
            return Integer (Long'(From_Any (O.Argument)));

         when Tk_Ushort =>
            return Integer (Unsigned_Short'(From_Any (O.Argument)));
         when Tk_Ulong =>
            return Integer (Unsigned_Long'(From_Any (O.Argument)));
         when Tk_Octet =>
            return Integer (Octet'(From_Any (O.Argument)));

         when others =>
            Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Integer expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (O : in NamedValue) return Long_Float is
      Kind : constant TCKind := UTCK (O.Argument);
   begin
      case Kind is
         when Tk_Float =>
            return Long_Float (PolyORB.Types.Float'(From_Any (O.Argument)));
         when Tk_Double =>
            return Long_Float (PolyORB.Types.Double'(From_Any (O.Argument)));
         when others =>
            Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Float expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (O : in NamedValue) return String is
      Kind : constant TCKind := UTCK (O.Argument);
   begin
      case Kind is
         when Tk_String =>
            return To_Standard_String (From_Any (O.Argument));
         when Tk_Char =>
            return (1 => PolyORB.Types.Char'(From_Any (O.Argument)));
         when others =>
            Exceptions.Raise_Exception
              (Data_Error'Identity,
               "String/character expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (O : in NamedValue) return Boolean is
      Kind : constant TCKind := UTCK (O.Argument);
   begin
      case Kind is
         when Tk_Boolean =>
            return From_Any (O.Argument);
         when others =>
            Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Boolean expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

--    function Get (O : in NamedValue) return SOAP_Record is
--    begin
--       if O'Tag = Types.SOAP_Record'Tag then
--          return SOAP_Record (O);

--       else
--          Exceptions.Raise_Exception
--            (Data_Error'Identity,
--             "SOAP Struct expected, found "
--             & TCKind'Image (TCK (O.Argument)));
--       end if;
--    end Get;

--    function Get (O : in NamedValue) return SOAP_Array is
--    begin
--       if O'Tag = Types.SOAP_Array'Tag then
--          return SOAP_Array (O);

--       else
--          Exceptions.Raise_Exception
--            (Data_Error'Identity,
--             "SOAP Array expected, found "
--              & TCKind'Image (TCK (O.Argument)));
--       end if;
--    end Get;

--    -------
--    -- I --
--    -------

--    function I
--      (V    : in Integer;
--       Name : in String := "item")
--      return XSD_Integer is
--    begin
--       return (To_Unbounded_String (Name), V);
--    end I;

--    -----------
--    -- Image --
--    -----------

   function Image (O : NamedValue) return String is
      TC : constant TypeCode.Object
        := Get_Unwound_Type (O.Argument);
      Kind : constant TCKind := TypeCode.Kind (TC);
   begin
      pragma Debug
        (SOAP.Types.O ("Image: enter, Kind is "
                       & TCKind'Image (Kind)));
      case Kind is
         when
           Tk_Long   |
           Tk_Short  |
           Tk_Ulong  |
           Tk_Ushort |
           Tk_Octet  =>
            return PolyORB.Utils.Trimmed_Image (Get (O));

         when Tk_Float | Tk_Double =>

            declare
               use Ada;

               Result : String (1 .. Long_Float'Width);
            begin
               Long_Float_Text_IO.Put (Result, Get (O), Exp => 0);
               return Strings.Fixed.Trim (Result, Strings.Both);
            end;

         when Tk_String | Tk_Char =>
            return Get (O);

         when Tk_Boolean =>
            if Get (O) then
               return "1";
            else
               return "0";
            end if;

         when Tk_Enum =>
            declare
               use PolyORB.Any;

               Pos : constant PolyORB.Types.Unsigned_Long
                 := From_Any
                 (Get_Aggregate_Element
                  (O.Argument, TC_Unsigned_Long, 0));
               Enumerator : constant PolyORB.Types.String
                 := From_Any (TypeCode.Get_Parameter (TC, Pos + 2));
            begin
               return To_Standard_String (Enumerator);
            end;

         when Tk_Void =>
            return "";

         when others =>
            --  XXX ???
            pragma Debug
              (SOAP.Types.O ("Image: Unsupported typecode kind:"
                             & TCKind'Image (Kind)));
            raise Data_Error;
      end case;
   end Image;

--    function Image (O : in XSD_Time_Instant) return String is

--       function Image (Timezone : in TZ) return String;
--       --  Returns Image for the TZ

--       function Image (Timezone : in TZ) return String is

--          subtype Str2 is String (1 .. 2);

--          function I2D (N : Natural) return Str2;

--          function I2D (N : Natural) return Str2 is
--             V : constant String := Natural'Image (N);
--          begin
--             if N > 9 then
--                return V (V'First + 1 .. V'Last);
--             else
--                return '0' & V (V'First + 1 .. V'Last);
--             end if;
--          end I2D;

--       begin
--          if Timezone >= 0 then
--             return '+' & I2D (Timezone) & ":00";
--          else
--             return '-' & I2D (abs Timezone) & ":00";
--          end if;
--       end Image;

--    begin
--       return GNAT.Calendar.Time_IO.Image (O.T, "%Y-%m-%dT%H:%M:%S")
--         & Image (O.Timezone);
--    end Image;

--    function Image (O : in SOAP_Base64) return String is
--    begin
--       return To_String (O.V);
--    end Image;

--    function Image (O : in SOAP_Array) return String is
--       Result : Unbounded_String;
--    begin
--       Append (Result, '(');

--       for K in O.Items.O'Range loop
--          Append (Result, Integer'Image (K));
--          Append (Result, " => ");
--          Append (Result, Image (O.Items.O (K).O.all));

--          if K /= O.Items.O'Last then
--             Append (Result, ", ");
--          end if;
--       end loop;

--       Append (Result, ')');

--       return To_String (Result);
--    end Image;

--    function Image (O : in SOAP_Record) return String is
--       Result : Unbounded_String;
--    begin
--       Append (Result, '(');

--       for K in O.Items.O'Range loop
--          Append (Result, Name (O));
--          Append (Result, " => ");
--          Append (Result, Image (O.Items.O (K).O.all));

--          if K /= O.Items.O'Last then
--             Append (Result, ", ");
--          end if;
--       end loop;

--       Append (Result, ')');

--       return To_String (Result);
--    end Image;

--    -------
--    -- N --
--    -------

--    function N (Name : in String  := "item") return XSD_Null is
--    begin
--       return (Name => To_Unbounded_String (Name));
--    end N;

   ----------
   -- Name --
   ----------

   function Name (O : in NamedValue) return String is
   begin
      return To_Standard_String (O.Name);
   end Name;

--    -------
--    -- R --
--    -------

--    function R
--      (V    : in Object_Set;
--       Name : in String)
--      return SOAP_Record is
--    begin
--       return (To_Unbounded_String (Name),
--               (Finalization.Controlled with new Object_Set'(V)));
--    end R;

--    -------
--    -- S --
--    -------

--    function S
--      (V      : in String;
--       Name   : in String  := "item";
--       Encode : in Boolean := True)
--      return XSD_String is
--    begin
--       if Encode then
--          return (To_Unbounded_String (Name),
--                  To_Unbounded_String (Utils.Encode (V)));
--       else
--          return (To_Unbounded_String (Name),
--                  To_Unbounded_String (V));
--       end if;
--    end S;

--    -------
--    -- T --
--    -------

--    function T
--      (V        : in Calendar.Time;
--       Name     : in String        := "item";
--       Timezone : in TZ            := GMT)
--      return XSD_Time_Instant is
--    begin
--       return (To_Unbounded_String (Name), V, Timezone);
--    end T;

--    -------
--    -- V --
--    -------

--    function V (O : in XSD_Integer) return Integer is
--    begin
--       return O.V;
--    end V;

--    function V (O : in XSD_Float) return Long_Float is
--    begin
--       return O.V;
--    end V;

--    function V (O : in XSD_String) return String is
--    begin
--       return To_String (O.V);
--    end V;

--    function V (O : in XSD_Boolean) return Boolean is
--    begin
--       return O.V;
--    end V;

--    function V (O : in XSD_Time_Instant) return Calendar.Time is
--    begin
--       return O.T;
--    end V;

--    function V (O : in SOAP_Base64) return String is
--    begin
--       return To_String (O.V);
--    end V;

--    function V (O : in SOAP_Array) return Object_Set is
--    begin
--       return O.Items.O.all;
--    end V;

--    function V (O : in SOAP_Record; Name : in String) return NamedValue is
--    begin
--       for K in O.Items.O'Range loop
--          if Types.Name (O.Items.O (K).O.all) = Name then
--             return O.Items.O (K).O.all;
--          end if;
--       end loop;

--       Exceptions.Raise_Exception
--         (Types.Data_Error'Identity,
--          "(V) Struct object " & Name & " not found");
--    end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Record_Image (O : in NamedValue) return String;
   function XML_Enum_Image (O : in NamedValue) return String;
   function XML_ObjRef_Image (O : in NamedValue) return String;
   function XML_Sequence_Image (O : in NamedValue) return String;

   function XML_Image (O : in NamedValue) return String is
      Kind : constant TCKind := TypeCode.Kind
        (Get_Unwound_Type (O.Argument));
   begin
      pragma Debug
        (SOAP.Types.O ("XML_Image: arg """ & To_Standard_String (O.Name)
            & """ is a " & TCKind'Image (Kind)));

      case Kind is

         when Tk_Struct =>
            return XML_Record_Image (O);

         when Tk_Enum =>
            return XML_Enum_Image (O);

         when Tk_Objref =>
            return XML_ObjRef_Image (O);

         when Tk_Sequence =>
            return XML_Sequence_Image (O);

         when Tk_Void =>
            return "<" & To_Standard_String (O.Name)
              & " xsi:null=""1""/>";

         when others =>
            pragma Debug (SOAP.Types.O ("Defaulting."));
            return "<" & To_Standard_String (O.Name)
              & xsi_type (XML_Type (O)) & '>'
              & Image (O)
              & "</" & To_Standard_String (O.Name) & '>';
      end case;
   end XML_Image;

--    function XML_Image (O : in XSD_Null) return String is
--       OC : constant NamedValue := NamedValue (O);
--    begin
--       return "<" & Name (OC) & " xsi_null=""1""/>";
--    end XML_Image;

--    New_Line : constant String := ASCII.CR & ASCII.LF;

--    function XML_Image (O : in SOAP_Array) return String is

--       function Array_Type return String;
--       --  Returns the right SOAP array type.

--       function Array_Type return String is

--          T         : Ada.Tags.Tag;
--          Same_Type : Boolean := True;
--       begin
--          T := O.Items.O (O.Items.O'First).O'Tag;

--          for K in O.Items.O'First + 1 .. O.Items.O'Last loop
--             if T /= O.Items.O (K).O'Tag then
--                Same_Type := False;
--                exit;
--             end if;
--          end loop;

--          if Same_Type then
--             return XML_Type (O.Items.O (O.Items.O'First).O.all);

--          else
--             return XML_Undefined;
--          end if;
--       end Array_Type;

--       Result : Unbounded_String;
--    begin
--       --  Open array element

--       Append (Result, '<');
--       Append (Result, O.Name);
--       Append (Result, " SOAP-ENC:arrayType=""");
--       Append (Result, Array_Type);
--       Append (Result, '[');
--       Append (Result, AWS.Utils.Image (O.Items.O'Length));
--       Append (Result, "]"" ");
--       Append (Result, xsi_type (XML_Array));
--       Append (Result, '>');
--       Append (Result, New_Line);

--       --  Add all elements

--       for K in O.Items.O'Range loop
--          Append (Result, XML_Image (O.Items.O (K).O.all));
--          Append (Result, New_Line);
--       end loop;

--       --  End array element

--       Append (Result, Utils.Tag (To_String (O.Name), Start => False));

--       return To_String (Result);
--    end XML_Image;

   function XML_Enum_Image (O : in NamedValue) return String is
      Tag_Name : constant Standard.String
        := To_Standard_String (O.Name);
      Pos : constant PolyORB.Types.Unsigned_Long
        := 1 + From_Any
        (Get_Aggregate_Element (O.Argument, TC_Unsigned_Long, 0));
   begin
      return "<" & Tag_Name
        & " id="""
        & PolyORB.Utils.Trimmed_Image (Integer (Pos)) & """>"
        & Image (O)
        & "</" & Tag_Name & ">";
   end XML_Enum_Image;

   function XML_ObjRef_Image (O : in NamedValue) return String is
      Tag_Name : constant Standard.String
        := To_Standard_String (O.Name);

      Ref : constant PolyORB.References.Ref
        := PolyORB.Any.ObjRef.From_Any (O.Argument);

      SOAP_Profile : PolyORB.Binding_Data.Profile_Access;

      Result : PolyORB.Types.String;

      use PolyORB.Any;
      use type PolyORB.Binding_Data.Profile_Access;
      use PolyORB.Binding_Data.SOAP;

   begin
      Result := To_PolyORB_String ("<" & Tag_Name
        & " xsi:type="""
        & PolyORB.References.Type_Id_Of (Ref)
                                   & """>");
      PolyORB.References.Binding.Get_Tagged_Profile
        (Ref, PolyORB.Binding_Data.Tag_SOAP, SOAP_Profile);
      --  If the real reference (Ref) does not contain a SOAP
      --  profile, then Get_Tagged_Profile tries to create
      --  a proxy profile instead. Only if it is not possible
      --  to create such a proxy profile do we get a null pointer
      --  in SOAP_Profile.

      if SOAP_Profile /= null then
         declare
            URI : constant String
              := To_URI (SOAP_Profile_Type (SOAP_Profile.all));
         begin
            pragma Debug
              (SOAP.Types.O ("Exporting object with URI: " & URI));
            Append (Result, URI);
         end;
      else
         Append (Result, "#IOR:");

         --  XXX Is there a possibility to include a stringified IOR
         --  here anyway?
      end if;
      Append (Result, "</" & Tag_Name & ">");
      return To_Standard_String (Result);
   end XML_ObjRef_Image;

   function XML_Sequence_Image (O : in NamedValue) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Element_Type : constant PolyORB.Any.TypeCode.Object
        := TypeCode.Content_Type (Get_Unwound_Type (O.Argument));
      New_Line : constant String := ASCII.CR & ASCII.LF;
   begin
      Append (Result, SOAP.Utils.Tag
              (To_Standard_String (O.Name), Start => True));
      Append (Result, New_Line);

      declare
         Nb : constant PolyORB.Types.Unsigned_Long
           := PolyORB.Any.Get_Aggregate_Count (O.Argument);
      begin
         --  Note: element 0 in a Tk_Sequence aggregate holds the
         --  length of the sequence, so we can assume that Nb > 0.
         pragma Assert (Nb > 0);

         for I in 1 .. Nb - 1 loop
            Append
              (Result, XML_Image
               (PolyORB.Any.NamedValue'
                (Name     => To_PolyORB_String ("e"),
                 Argument =>
                   PolyORB.Any.Get_Aggregate_Element
                 (O.Argument, Element_Type, I),
                 Arg_Modes => ARG_IN)));

            Append (Result, New_Line);
         end loop;
      end;

      Append (Result, SOAP.Utils.Tag
              (To_Standard_String (O.Name), Start => False));

      return To_String (Result);
   end XML_Sequence_Image;

   function XML_Record_Image (O : in NamedValue) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Data_Type : constant PolyORB.Any.TypeCode.Object
        := Get_Unwound_Type (O.Argument);
      New_Line : constant String := ASCII.CR & ASCII.LF;
   begin
      pragma Debug (SOAP.Types.O ("XML_Record_Image: enter"));
      Append (Result, SOAP.Utils.Tag
              (To_Standard_String (O.Name), Start => True));
      Append (Result, New_Line);

      declare
         Nb : constant PolyORB.Types.Unsigned_Long
           := PolyORB.Any.Get_Aggregate_Count (O.Argument);
      begin
         for I in 0 .. Nb - 1 loop
            Append
              (Result, XML_Image
               (PolyORB.Any.NamedValue'
                (Name     =>
                   PolyORB.Any.TypeCode.Member_Name (Data_Type, I),
                 Argument =>
                   PolyORB.Any.Get_Aggregate_Element
                 (O.Argument, PolyORB.Any.TypeCode.Member_Type
                  (Data_Type, I), I),
                 Arg_Modes =>
                   ARG_IN)));

            Append (Result, New_Line);
         end loop;
      end;

      Append (Result, SOAP.Utils.Tag
              (To_Standard_String (O.Name), Start => False));

      pragma Debug (SOAP.Types.O ("XML_Record_Image: leave"));
      return To_String (Result);
   end XML_Record_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (O : in NamedValue) return String
   is
      K : constant TCKind := TypeCode.Kind
        (Get_Unwound_Type (O.Argument));
   begin
      case K is
         when Tk_Long =>
            return XML_Int;
         when Tk_Short =>
            return XML_Short;
         when Tk_Ulong =>
            return XML_UInt;
         when Tk_Ushort =>
            return XML_UShort;
         when Tk_Octet =>
            return XML_UByte;

         when Tk_Float =>
            return XML_Float;
         when Tk_Double =>
            return XML_Double;
         when Tk_String | Tk_Char =>
            return XML_String;
         when Tk_Boolean =>
            return XML_Boolean;
         when Tk_Array =>
            return XML_Array;

         when others =>
            return "";
            --  XXX ???
      end case;
   end XML_Type;

--    function XML_Type  (O : in XSD_Time_Instant) return String is
--    begin
--       return XML_Time_Instant;
--    end XML_Type;

--    function XML_Type (O : in XSD_Null) return String is
--    begin
--       return XML_Null;
--    end XML_Type;

--    function XML_Type (O : in SOAP_Base64) return String is
--    begin
--       return XML_Base64;
--    end XML_Type;

--    function XML_Type (O : in SOAP_Array) return String is
--    begin
--       return XML_Array;
--    end XML_Type;

--    function XML_Type  (O : in SOAP_Record) return String is
--    begin
--       return "";
--    end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : in String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end SOAP.Types;
