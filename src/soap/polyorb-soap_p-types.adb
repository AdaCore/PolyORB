------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . S O A P _ P . T Y P E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Long_Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with PolyORB.Any.ObjRef;
with PolyORB.Errors;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.SOAP_P.Types is

   use Ada;
   use PolyORB.Types;

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("soap.types");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   function xsi_type (Name : String) return String;
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

--    ---------
--    -- "+" --
--    ---------

--    function "+" (O : Object'Class) return Object_Controlled is
--    begin
--       return (Finalization.Controlled with new Object'Class'(O));
--    end "+";

--    -------
--    -- A --
--    -------

--    function A
--      (V    : Object_Set;
--       Name : String)
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
--       if NV.O /= null then
--          NV.O := new Object'Class'(NV.NV.all);
--       end if;
--    end Adjust;

--    procedure Adjust (O : in out Object_Set_Controlled) is
--    begin
--       if NV.O /= null then
--          NV.O := new Object_Set'(NV.NV.all);
--       end if;
--    end Adjust;

--    -------
--    -- B --
--    -------

--    function B
--      (V    : Boolean;
--       Name : String  := "item")
--      return XSD_Boolean is
--    begin
--       return (To_Unbounded_String (Name), V);
--    end B;

--    ---------
--    -- B64 --
--    ---------

--    function B64
--      (V      : String;
--       Name   : String  := "item")
--      return SOAP_Base64 is
--    begin
--       return (To_Unbounded_String (Name), To_Unbounded_String (V));
--    end B64;

--    -------
--    -- F --
--    -------

--    function F
--      (V    : Long_Float;
--       Name : String := "item")
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
--       if NV.O /= null then
--          Free (NV.O);
--       end if;
--    end Finalize;

--    procedure Finalize (O : in out Object_Set_Controlled) is
--       procedure Free is
--          new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);
--    begin
--       if NV.O /= null then
--          Free (NV.O);
--       end if;
--    end Finalize;

   function TCK (A : PolyORB.Any.Any) return TCKind;
   pragma Warnings (Off);
   pragma Unreferenced (TCK);
   pragma Warnings (On);

   function UTCK (A : PolyORB.Any.Any) return TCKind;
   --  Return the typecode kind of A. UTCK returns the
   --  kind after unwinding all levels of typedef.

   function TCK (A : PolyORB.Any.Any) return TCKind is
   begin
      return TypeCode.Kind (Get_Type (A));
   end TCK;

   function UTCK (A : PolyORB.Any.Any) return TCKind is
   begin
      return TypeCode.Kind (Get_Unwound_Type (A));
   end UTCK;

   ---------
   -- Get --
   ---------

   function Get (NV : NamedValue) return Integer is
      Kind : constant TCKind := UTCK (NV.Argument);
   begin
      case Kind is
         when Tk_Short =>
            return Integer (Short'(From_Any (NV.Argument)));
         when Tk_Long =>
            return Integer (Long'(From_Any (NV.Argument)));

         when Tk_Ushort =>
            return Integer (Unsigned_Short'(From_Any (NV.Argument)));
         when Tk_Ulong =>
            return Integer (Unsigned_Long'(From_Any (NV.Argument)));
         when Tk_Octet =>
            return Integer (Octet'(From_Any (NV.Argument)));

         when others =>
            Ada.Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Integer expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (NV : NamedValue) return Long_Float is
      Kind : constant TCKind := UTCK (NV.Argument);
   begin
      case Kind is
         when Tk_Float =>
            return Long_Float (PolyORB.Types.Float'(From_Any (NV.Argument)));
         when Tk_Double =>
            return Long_Float (PolyORB.Types.Double'(From_Any (NV.Argument)));
         when others =>
            Ada.Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Float expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (NV : NamedValue) return String is
      Kind : constant TCKind := UTCK (NV.Argument);
   begin
      case Kind is
         when Tk_String =>
            return To_Standard_String (From_Any (NV.Argument));
         when Tk_Char =>
            return (1 => PolyORB.Types.Char'(From_Any (NV.Argument)));
         when others =>
            Ada.Exceptions.Raise_Exception
              (Data_Error'Identity,
               "String/character expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

   function Get (NV : NamedValue) return Boolean is
      Kind : constant TCKind := UTCK (NV.Argument);
   begin
      case Kind is
         when Tk_Boolean =>
            return From_Any (NV.Argument);
         when others =>
            Ada.Exceptions.Raise_Exception
              (Data_Error'Identity,
               "Boolean expected, found " & TCKind'Image (Kind));
      end case;
   end Get;

--    function Get (NV : NamedValue) return SOAP_Record is
--    begin
--       if O'Tag = Types.SOAP_Record'Tag then
--          return SOAP_Record (O);

--       else
--          Exceptions.Raise_Exception
--            (Data_Error'Identity,
--             "SOAP Struct expected, found "
--             & TCKind'Image (TCK (NV.Argument)));
--       end if;
--    end Get;

--    function Get (NV : NamedValue) return SOAP_Array is
--    begin
--       if O'Tag = Types.SOAP_Array'Tag then
--          return SOAP_Array (O);

--       else
--          Exceptions.Raise_Exception
--            (Data_Error'Identity,
--             "SOAP Array expected, found "
--              & TCKind'Image (TCK (NV.Argument)));
--       end if;
--    end Get;

--    -------
--    -- I --
--    -------

--    function I
--      (V    : Integer;
--       Name : String := "item")
--      return XSD_Integer is
--    begin
--       return (To_Unbounded_String (Name), V);
--    end I;

--    -----------
--    -- Image --
--    -----------

   function Value_Image (NV : NamedValue) return String is
      TC : constant TypeCode.Object_Ptr := Get_Unwound_Type (NV.Argument);
      Kind : constant TCKind := TypeCode.Kind (TC);
   begin
      pragma Debug
        (C, O ("Image: enter, Kind is "
                       & TCKind'Image (Kind)));
      case Kind is
         when
           Tk_Long   |
           Tk_Short  |
           Tk_Ulong  |
           Tk_Ushort |
           Tk_Octet  =>
            return PolyORB.Types.Trimmed_Image
              (Long_Long (Integer'(Get (NV))));

         when Tk_Float | Tk_Double =>

            declare
               Result : String (1 .. Long_Float'Width);
            begin
               Long_Float_Text_IO.Put (Result, Get (NV), Exp => 0);
               return Strings.Fixed.Trim (Result, Strings.Both);
            end;

         when Tk_String | Tk_Char =>
            return Get (NV);

         when Tk_Boolean =>
            if Get (NV) then
               return "1";
            else
               return "0";
            end if;

         when Tk_Enum =>
            return To_Standard_String
                     (TypeCode.Enumerator_Name (TC,
                        Get_Aggregate_Element (NV.Argument, 0)));

         when Tk_Void =>
            return "";

         when others =>
            --  XXX ???
            return "Image: Unsupported TCKind:" & TCKind'Image (Kind);
      end case;
   end Value_Image;

--    function Image (O : XSD_Time_Instant) return String is

--       function Image (Timezone : TZ) return String;
--       --  Returns Image for the TZ

--       function Image (Timezone : TZ) return String is

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
--       return GNAT.Calendar.Time_IO.Image (NV.T, "%Y-%m-%dT%H:%M:%S")
--         & Image (NV.Timezone);
--    end Image;

--    function Image (O : SOAP_Base64) return String is
--    begin
--       return To_String (NV.V);
--    end Image;

--    function Image (O : SOAP_Array) return String is
--       Result : Unbounded_String;
--    begin
--       Append (Result, '(');

--       for K in NV.Items.O'Range loop
--          Append (Result, Integer'Image (K));
--          Append (Result, " => ");
--          Append (Result, Image (NV.Items.O (K).NV.all));

--          if K /= NV.Items.O'Last then
--             Append (Result, ", ");
--          end if;
--       end loop;

--       Append (Result, ')');

--       return To_String (Result);
--    end Image;

--    function Image (O : SOAP_Record) return String is
--       Result : Unbounded_String;
--    begin
--       Append (Result, '(');

--       for K in NV.Items.O'Range loop
--          Append (Result, Name (O));
--          Append (Result, " => ");
--          Append (Result, Image (NV.Items.O (K).NV.all));

--          if K /= NV.Items.O'Last then
--             Append (Result, ", ");
--          end if;
--       end loop;

--       Append (Result, ')');

--       return To_String (Result);
--    end Image;

--    -------
--    -- N --
--    -------

--    function N (Name : String  := "item") return XSD_Null is
--    begin
--       return (Name => To_Unbounded_String (Name));
--    end N;

   ----------
   -- Name --
   ----------

   function Name (NV : NamedValue) return String is
   begin
      return To_Standard_String (NV.Name);
   end Name;

--    -------
--    -- R --
--    -------

--    function R
--      (V    : Object_Set;
--       Name : String)
--      return SOAP_Record is
--    begin
--       return (To_Unbounded_String (Name),
--               (Finalization.Controlled with new Object_Set'(V)));
--    end R;

--    -------
--    -- S --
--    -------

--    function S
--      (V      : String;
--       Name   : String  := "item";
--       Encode : Boolean := True)
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
--      (V        : Calendar.Time;
--       Name     : String        := "item";
--       Timezone : TZ            := GMT)
--      return XSD_Time_Instant is
--    begin
--       return (To_Unbounded_String (Name), V, Timezone);
--    end T;

--    -------
--    -- V --
--    -------

--    function V (O : XSD_Integer) return Integer is
--    begin
--       return NV.V;
--    end V;

--    function V (O : XSD_Float) return Long_Float is
--    begin
--       return NV.V;
--    end V;

--    function V (O : XSD_String) return String is
--    begin
--       return To_String (NV.V);
--    end V;

--    function V (O : XSD_Boolean) return Boolean is
--    begin
--       return NV.V;
--    end V;

--    function V (O : XSD_Time_Instant) return Calendar.Time is
--    begin
--       return NV.T;
--    end V;

--    function V (O : SOAP_Base64) return String is
--    begin
--       return To_String (NV.V);
--    end V;

--    function V (O : SOAP_Array) return Object_Set is
--    begin
--       return NV.Items.NV.all;
--    end V;

--    function V (O : SOAP_Record; Name : String) return NamedValue is
--    begin
--       for K in NV.Items.O'Range loop
--          if Types.Name (NV.Items.O (K).NV.all) = Name then
--             return NV.Items.O (K).NV.all;
--          end if;
--       end loop;

--       Exceptions.Raise_Exception
--         (Types.Data_Error'Identity,
--          "(V) Struct object " & Name & " not found");
--    end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Record_Image (NV : NamedValue) return String;
   function XML_Enum_Image (NV : NamedValue) return String;
   function XML_ObjRef_Image (NV : NamedValue) return String;
   function XML_Sequence_Image (NV : NamedValue) return String;

   function XML_Image (NV : NamedValue) return String is
      Kind : constant TCKind := TypeCode.Kind (Get_Unwound_Type (NV.Argument));
   begin
      pragma Debug
        (C, O ("XML_Image: arg """
                         & To_Standard_String (XML_Image.NV.Name)
                         & """ is a " & TCKind'Image (Kind)));

      case Kind is

         when Tk_Struct =>
            return XML_Record_Image (NV);

         when Tk_Enum =>
            return XML_Enum_Image (NV);

         when Tk_Objref =>
            return XML_ObjRef_Image (NV);

         when Tk_Sequence =>
            return XML_Sequence_Image (NV);

         when Tk_Void =>
            return "<" & To_Standard_String (NV.Name)
              & " xsi:null=""1""/>";

         when others =>
            pragma Debug (C, O ("Defaulting."));
            return "<" & To_Standard_String (NV.Name)
              & xsi_type (XML_Type (NV)) & '>'
              & Value_Image (NV)
              & "</" & To_Standard_String (NV.Name) & '>';
      end case;
   end XML_Image;

--    function XML_Image (O : XSD_Null) return String is
--       OC : constant NamedValue := NamedValue (O);
--    begin
--       return "<" & Name (OC) & " xsi_null=""1""/>";
--    end XML_Image;

--    New_Line : constant String := ASCII.CR & ASCII.LF;

--    function XML_Image (O : SOAP_Array) return String is

--       function Array_Type return String;
--       --  Returns the right SOAP array type.

--       function Array_Type return String is

--          T         : Ada.Tags.Tag;
--          Same_Type : Boolean := True;
--       begin
--          T := NV.Items.O (NV.Items.O'First).O'Tag;

--          for K in NV.Items.O'First + 1 .. NV.Items.O'Last loop
--             if T /= NV.Items.O (K).O'Tag then
--                Same_Type := False;
--                exit;
--             end if;
--          end loop;

--          if Same_Type then
--             return XML_Type (NV.Items.O (NV.Items.O'First).NV.all);

--          else
--             return XML_Undefined;
--          end if;
--       end Array_Type;

--       Result : Unbounded_String;
--    begin
--       --  Open array element

--       Append (Result, '<');
--       Append (Result, NV.Name);
--       Append (Result, " SOAP-ENC:arrayType=""");
--       Append (Result, Array_Type);
--       Append (Result, '[');
--       Append (Result, AWS.Utils.Image (NV.Items.O'Length));
--       Append (Result, "]"" ");
--       Append (Result, xsi_type (XML_Array));
--       Append (Result, '>');
--       Append (Result, New_Line);

--       --  Add all elements

--       for K in NV.Items.O'Range loop
--          Append (Result, XML_Image (NV.Items.O (K).NV.all));
--          Append (Result, New_Line);
--       end loop;

--       --  End array element

--       Append (Result, Utils.Tag (To_String (NV.Name), Start => False));

--       return To_String (Result);
--    end XML_Image;

   function XML_Enum_Image (NV : NamedValue) return String is
      Tag_Name : constant Standard.String
        := To_Standard_String (NV.Name);
      Pos : constant PolyORB.Types.Unsigned_Long
        := 1 + From_Any
        (Get_Aggregate_Element (NV.Argument, TC_Unsigned_Long, 0));
   begin
      return "<" & Tag_Name
        & " id="""
        & PolyORB.Types.Trimmed_Image (Unsigned_Long_Long (Pos)) & """>"
        & Value_Image (NV)
        & "</" & Tag_Name & ">";
   end XML_Enum_Image;

   function XML_ObjRef_Image (NV : NamedValue) return String is
      Tag_Name : constant Standard.String
        := To_Standard_String (NV.Name);

      Ref : constant PolyORB.References.Ref
        := PolyORB.Any.ObjRef.From_Any (NV.Argument);

      SOAP_Profile : PolyORB.Binding_Data.Profile_Access;

      Result : PolyORB.Types.String;

      use type PolyORB.Binding_Data.Profile_Access;
      use PolyORB.Binding_Data.SOAP;

      use PolyORB.Errors;

      Error : Error_Container;

   begin
      Result := To_PolyORB_String ("<" & Tag_Name
        & " xsi:type="""
        & PolyORB.References.Type_Id_Of (Ref)
                                   & """>");
      PolyORB.References.Binding.Get_Tagged_Profile
        (Ref,
         PolyORB.Binding_Data.Tag_SOAP,
         SOAP_Profile,
         Error);
      --  If the real reference (Ref) does not contain a SOAP
      --  profile, then Get_Tagged_Profile tries to create
      --  a proxy profile instead. Only if it is not possible
      --  to create such a proxy profile do we get a null pointer
      --  in SOAP_Profile.

      if Found (Error) then
         raise Program_Error;
      end if;

      if SOAP_Profile /= null then
         declare
            URI : constant String
              := To_URI (SOAP_Profile_Type (SOAP_Profile.all));
         begin
            pragma Debug
              (C, O ("Exporting object with URI: " & URI));
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

   function XML_Sequence_Image (NV : NamedValue) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Element_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                                PolyORB.Any.TypeCode.To_Ref
                                  (TypeCode.Content_Type
                                   (Get_Unwound_Type (NV.Argument)));
      New_Line : constant String := ASCII.CR & ASCII.LF;
   begin
      Append (Result, Tag (To_Standard_String (NV.Name), Start => True));
      Append (Result, New_Line);

      declare
         Nb : constant PolyORB.Types.Unsigned_Long
           := PolyORB.Any.Get_Aggregate_Count (NV.Argument);
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
                     (NV.Argument, Element_Type, I),
                 Arg_Modes => ARG_IN)));

            Append (Result, New_Line);
         end loop;
      end;

      Append (Result, Tag (To_Standard_String (NV.Name), Start => False));

      return To_String (Result);
   end XML_Sequence_Image;

   function XML_Record_Image (NV : NamedValue) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Data_Type : constant PolyORB.Any.TypeCode.Object_Ptr :=
                    Get_Unwound_Type (NV.Argument);
      New_Line : constant String := ASCII.CR & ASCII.LF;
   begin
      pragma Debug (C, O ("XML_Record_Image: enter"));
      Append (Result, Tag (To_Standard_String (NV.Name), Start => True));
      Append (Result, New_Line);

      declare
         Nb : constant PolyORB.Types.Unsigned_Long
           := PolyORB.Any.Get_Aggregate_Count (NV.Argument);
      begin
         for I in 0 .. Nb - 1 loop
            Append
              (Result, XML_Image
               (PolyORB.Any.NamedValue'
                (Name     => PolyORB.Any.TypeCode.Member_Name (Data_Type, I),
                 Argument =>
                   PolyORB.Any.Get_Aggregate_Element
                     (NV.Argument,
                      PolyORB.Any.TypeCode.To_Ref
                        (PolyORB.Any.TypeCode.Member_Type (Data_Type, I)),
                      I),
                 Arg_Modes =>
                   ARG_IN)));

            Append (Result, New_Line);
         end loop;
      end;

      Append (Result, Tag (To_Standard_String (NV.Name), Start => False));

      pragma Debug (C, O ("XML_Record_Image: leave"));
      return To_String (Result);
   end XML_Record_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (NV : NamedValue) return String
   is
      K : constant TCKind := TypeCode.Kind (Get_Unwound_Type (NV.Argument));
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

--    function XML_Type  (O : XSD_Time_Instant) return String is
--    begin
--       return XML_Time_Instant;
--    end XML_Type;

--    function XML_Type (O : XSD_Null) return String is
--    begin
--       return XML_Null;
--    end XML_Type;

--    function XML_Type (O : SOAP_Base64) return String is
--    begin
--       return XML_Base64;
--    end XML_Type;

--    function XML_Type (O : SOAP_Array) return String is
--    begin
--       return XML_Array;
--    end XML_Type;

--    function XML_Type  (O : SOAP_Record) return String is
--    begin
--       return "";
--    end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end PolyORB.SOAP_P.Types;
