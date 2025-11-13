------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           S O A P . T Y P E S                            --
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
with Ada.Tags;
with PolyORB.Utils.Unchecked_Deallocation;
with Ada.Streams;

with AWS.Utils;
with AWS.Translator;

with SOAP.Utils;

with PolyORB.Types;
with PolyORB.Log;

package body SOAP.Types is

   use Ada;

   use PolyORB.Log;
   package L is
      new PolyORB.Log.Facility_Log ("aws.soap");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   --  the polyorb logging facility

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Object_Set,


      Name   => Object_Set_Access);

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Natural,


      Name   => Counter_Access);

   function xsi_type (Name : String) return String;
   pragma Inline (xsi_type);
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

   function Spaces (N : Natural) return String;
   pragma Inline (Spaces);
   --  Returns N * 3 spaces.

   ---------------------
   -- From_NamedValue --
   ---------------------

   function From_NamedValue
     (NV : PolyORB.Any.NamedValue)
     return Object'Class
   is
      use PolyORB.Any;
      use PolyORB.Types;
      use PolyORB.Any.TypeCode;

      O : constant Object_Access := From_Any (NV.Argument);
   begin
      pragma Debug (L.Output ("From_NamedValue: processing nv_arg named "
                              & To_String (NV.Name)));
      O.Name := To_Unbounded_String
        (PolyORB.Types.To_String (NV.Name));
      return O.all;
   end From_NamedValue;

   -------------------
   -- To_NamedValue --
   -------------------

   function To_NamedValue (O : Object'Class) return PolyORB.Any.NamedValue
   is
      use PolyORB.Any;

      NV : PolyORB.Any.NamedValue;
   begin
      NV.Name := PolyORB.Types.To_PolyORB_String (To_String (O.Name));
      NV.Arg_Modes := ARG_IN;
      NV.Argument := To_Any (O);

      return NV;
   end To_NamedValue;

   ---------
   -- "+" --
   ---------

   function "+" (O : Object'Class) return Object_Safe_Pointer is
   begin
      return (Finalization.Controlled with new Object'Class'(O));
   end "+";

   -------
   -- - --
   -------

   function "-" (O : Object_Safe_Pointer) return Object'Class is
   begin
      return O.O.all;
   end "-";

   -------
   -- A --
   -------

   function A
     (V    : Object_Set;
      Name : String)
      return SOAP_Array is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name),
                     new Natural'(1), new Object_Set'(V));
   end A;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Object_Safe_Pointer) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   procedure Adjust (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all + 1;
   end Adjust;

   -------
   -- B --
   -------

   function B
     (V    : Boolean;
      Name : String  := "item")
      return XSD_Boolean is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end B;

   ---------
   -- B64 --
   ---------

   function B64
     (V      : String;
      Name   : String  := "item")
      return SOAP_Base64 is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name), To_Unbounded_String (V));
   end B64;

   -------
   -- F --
   -------

   function F
     (V    : Long_Float;
      Name : String := "item")
      return XSD_Float is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end F;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Object_Safe_Pointer) is
      procedure Free is
         new PolyORB.Utils.Unchecked_Deallocation.Free

        (Object => Object'Class,

         Name   => Object_Access);
   begin
      if O.O /= null then
         Free (O.O);
      end if;
   end Finalize;

   procedure Finalize (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all - 1;

      if O.Ref_Counter.all = 0 then
         Free (O.O);
         Free (O.Ref_Counter);
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (O : Object'Class) return Integer is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Integer'Tag then
         return V (XSD_Integer (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Integer expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return Long_Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Float'Tag then
         return V (XSD_Float (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Float expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_String'Tag then
         return V (XSD_String (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "String expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return Boolean is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Boolean'Tag then
         return V (XSD_Boolean (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Boolean expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Base64 is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Base64'Tag then
         return SOAP_Base64 (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Base64 expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Record is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Record'Tag then
         return SOAP_Record (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Struct expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Array is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Array'Tag then
         return SOAP_Array (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Array expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   -------
   -- I --
   -------

   function I
     (V    : Integer;
      Name : String := "item")
     return XSD_Integer is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : Object) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end Image;

   function Image (O : XSD_Integer) return String is
      V : constant String := Integer'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   function Image (O : XSD_Float) return String is
      Result : String (1 .. Long_Float'Width);
   begin
      Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   function Image (O : XSD_String) return String is
   begin
      return To_String (O.V);
   end Image;

   function Image (O : XSD_Boolean) return String is
   begin
      if O.V then
         return "1";
      else
         return "0";
      end if;
   end Image;

--     function Image (O : XSD_Time_Instant) return String is

--        function Image (Timezone : TZ) return String;
--        --  Returns Image for the TZ

--        function Image (Timezone : TZ) return String is

--           subtype Str2 is String (1 .. 2);

--           function I2D (N : Natural) return Str2;

--           function I2D (N : Natural) return Str2 is
--              V : constant String := Natural'Image (N);
--           begin
--              if N > 9 then
--                 return V (V'First + 1 .. V'Last);
--              else
--                 return '0' & V (V'First + 1 .. V'Last);
--              end if;
--           end I2D;

--        begin
--           if Timezone >= 0 then
--              return '+' & I2D (Timezone) & ":00";
--           else
--              return '-' & I2D (abs Timezone) & ":00";
--           end if;
--        end Image;

--     begin
--        return GNAT.Calendar.Time_IO.Image (O.T, "%Y-%m-%dT%H:%M:%S")
--          & Image (O.Timezone);
--     end Image;

   function Image (O : SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end Image;

   function Image (O : SOAP_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Integer'Image (K));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   function Image (O : SOAP_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Name (O));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (O : in out Composite) is
   begin
      O.Ref_Counter := new Natural'(1);
   end Initialize;

   -------
   -- N --
   -------

   function N (Name : String  := "item") return XSD_Null is
   begin
      return (Finalization.Controlled with Name => To_Unbounded_String (Name));
   end N;

   ----------
   -- Name --
   ----------

   function Name (O : Object'Class) return String is
   begin
      return To_String (O.Name);
   end Name;

   -------
   -- R --
   -------

   function R
     (V    : Object_Set;
      Name : String)
      return SOAP_Record is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name),
                     new Natural'(1), new Object_Set'(V));
   end R;

   -------
   -- S --
   -------

   function S
     (V      : String;
      Name   : String  := "item";
      Encode : Boolean := True)
      return XSD_String is
   begin
      if Encode then
         return (Finalization.Controlled
                   with To_Unbounded_String (Name),
                        To_Unbounded_String (Utils.Encode (V)));
      else
         return (Finalization.Controlled
                   with To_Unbounded_String (Name),
                        To_Unbounded_String (V));
      end if;
   end S;

   ----------
   -- Size --
   ----------

   function Size (O : SOAP_Array) return Natural is
   begin
      return O.O'Length;
   end Size;

   ------------
   -- Spaces --
   ------------

   function Spaces (N : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return (3 * N) * ' ';
   end Spaces;

   -------
   -- T --
   -------

--     function T
--       (V        : Calendar.Time;
--        Name     : String        := "item";
--        Timezone : TZ            := GMT)
--        return XSD_Time_Instant is
--     begin
--        return (Finalization.Controlled
--                  with To_Unbounded_String (Name), V, Timezone);
--     end T;

   -------
   -- V --
   -------

   function V (O : XSD_Integer) return Integer is
   begin
      return O.V;
   end V;

   function V (O : XSD_Float) return Long_Float is
   begin
      return O.V;
   end V;

   function V (O : XSD_String) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : XSD_Boolean) return Boolean is
   begin
      return O.V;
   end V;

--     function V (O : XSD_Time_Instant) return Calendar.Time is
--     begin
--        return O.T;
--     end V;

   function V (O : SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : SOAP_Array) return Object_Set is
   begin
      return O.O.all;
   end V;

   function V (O : SOAP_Array; N : Positive) return Object'Class is
   begin
      return O.O (N).O.all;
   end V;

   function V (O : SOAP_Record; Name : String) return Object'Class is
   begin
      for K in O.O'Range loop
         if Types.Name (O.O (K).O.all) = Name then
            return O.O (K).O.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Types.Data_Error'Identity,
         "(V) Struct object " & Name & " not found");
   end V;

   function V (O : SOAP_Record) return Object_Set is
   begin
      return O.O.all;
   end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : Object) return String is
      Indent : constant Natural := 0; --  XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      return Spaces (Indent)
        & "<" & Name (OC) & xsi_type (XML_Type (OC)) & '>'
        & Image (OC)
        & "</" & Name (OC) & '>';
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : XSD_Integer) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : XSD_Float) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : XSD_String) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : XSD_Boolean) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

--     function XML_Image (O : XSD_Time_Instant) return String is
--     begin
--        return XML_Image (Object (O));
--     end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : XSD_Null) return String is
      Indent : constant Natural := 0; --  XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      return Spaces (Indent) & "<" & Name (OC) & " xsi_null=""1""/>";
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : SOAP_Base64) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   New_Line : constant String := ASCII.CR & ASCII.LF;

   function XML_Image (O : SOAP_Array) return String is

      Indent : constant Natural := 0; --  XML_Indent.Value;

      function Array_Type return String;
      --  Returns the right SOAP array type.

      ----------------
      -- Array_Type --
      ----------------

      function Array_Type return String is
         use type Ada.Tags.Tag;

         T         : Ada.Tags.Tag;
      begin
         if O.O'Length = 0 then
            --  This is a zero length array, type is undefined.
            return XML_Undefined;
         end if;

         T := O.O (O.O'First).O'Tag;

         if T = SOAP_Record'Tag then
            --  This is a record, no need to parse further.
            return XML_Undefined;
         end if;

         for K in O.O'First + 1 .. O.O'Last loop

            --  Not same type if type different or is a composite type.

            if T /= O.O (K).O'Tag
              or else O.O (K).O.all in SOAP.Types.Composite'Class
            then
               return XML_Undefined;
            end if;

         end loop;

         --  We have the same type.
         return XML_Type (O.O (O.O'First).O.all);
      end Array_Type;

      Result : Unbounded_String;
   begin
      --  Open array element

      Append (Result, Spaces (Indent));
      Append (Result, '<');
      Append (Result, O.Name);
      Append (Result, " SOAP-ENC:arrayType=""");
      Append (Result, Array_Type);
      Append (Result, '[');
      Append (Result, AWS.Utils.Image (O.O'Length));
      Append (Result, "]""");
      Append (Result, xsi_type (XML_Array));
      Append (Result, '>');
      Append (Result, New_Line);

      --  Add all elements

      --  XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      --  XML_Indent.Set_Value (Indent);

      --  End array element

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (To_String (O.Name), Start => False));

      return To_String (Result);
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : SOAP_Record) return String is
      Indent : constant Natural := 0; --  XML_Indent.Value;
      Result : Unbounded_String;
   begin
      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (Name (O), Start => True));
      Append (Result, New_Line);

      --  XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      --  XML_Indent.Set_Value (Indent);

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (Name (O), Start => False));

      return To_String (Result);
   end XML_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (O : Object) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end XML_Type;

   function XML_Type (O : XSD_Integer) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Int;
   end XML_Type;

   function XML_Type (O : XSD_Float) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Float;
   end XML_Type;

   function XML_Type (O : XSD_String) return String is
      pragma Warnings (Off, O);
   begin
      return XML_String;
   end XML_Type;

   function XML_Type (O : XSD_Boolean) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Boolean;
   end XML_Type;

--     function XML_Type  (O : XSD_Time_Instant) return String is
--        pragma Warnings (Off, O);
--     begin
--        return XML_Time_Instant;
--     end XML_Type;

   function XML_Type (O : XSD_Null) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Null;
   end XML_Type;

   function XML_Type (O : SOAP_Base64) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Base64;
   end XML_Type;

   function XML_Type (O : SOAP_Array) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Array;
   end XML_Type;

   function XML_Type  (O : SOAP_Record) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

   ------------
   -- To_Any --
   ------------

   function To_Any (Obj : Object'Class) return PolyORB.Any.Any
   is
      use PolyORB.Types;

      --  This is a general dispatch function. This is mandatory,
      --  since complex types such as SOAP_Array need to refer to a
      --  general To_Any function that handles Object'Class elements

   begin
      if Obj in XSD_Boolean then
         return PolyORB.Any.To_Any (XSD_Boolean (Obj).V);
      elsif Obj in XSD_Integer then
         return PolyORB.Any.To_Any
           (PolyORB.Types.Long (XSD_Integer (Obj).V));

         --  As integers are 32 bit signed integers in GNAT.

      elsif Obj in XSD_Float then
         return PolyORB.Any.To_Any
           (PolyORB.Types.Double (XSD_Float (Obj).V));

         --  As long_floats are 64 bit floats in GNAT.

      elsif Obj in XSD_String then
         return PolyORB.Any.To_Any
           (To_PolyORB_String (To_String (XSD_String (Obj).V)));
      elsif Obj in XSD_Null then
         return PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Null);
--      elsif Obj in XSD_Time_Instant then
         --  not coded yet
--         return PolyORB.Any.Get_Empty_Any (PolyORB.Any.TC_Null);
      elsif Obj in SOAP_Base64 then
         declare
            use Ada.Streams;

            Sq_Type : constant PolyORB.Any.TypeCode.Local_Ref
              := PolyORB.Any.TypeCode.TC_Sequence;
            Byte_Stream : constant Ada.Streams.Stream_Element_Array
              := AWS.Translator.Base64_Decode (V (SOAP_Base64 (Obj)));
         begin
            PolyORB.Any.TypeCode.Add_Parameter
              (Sq_Type,
               PolyORB.Any.To_Any
               (PolyORB.Types.Unsigned_Long
                (Byte_Stream'Last - Byte_Stream'First + 1)));
            PolyORB.Any.TypeCode.Add_Parameter
              (Sq_Type, PolyORB.Any.To_Any
               (PolyORB.Any.TypeCode.TC_Octet));
            declare
               Sq : PolyORB.Any.Any :=
                 PolyORB.Any.Get_Empty_Any_Aggregate
                 (Sq_Type);
            begin
               for K in Byte_Stream'Range loop
                  PolyORB.Any.Add_Aggregate_Element
                    (Sq, PolyORB.Any.To_Any
                     (PolyORB.Types.Octet (Byte_Stream (K))));
               end loop;
               return Sq;
            end;
         end;
      elsif Obj in SOAP_Array then
         declare
            use PolyORB.Any;
            Ar_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                        PolyORB.Any.TypeCode.TC_Array;
         begin

            pragma Debug (C, O ("To_Any: SOAP_Array: nb of elements= "
                             & Natural'Image (Size (SOAP_Array (Obj)))));

            PolyORB.Any.TypeCode.Add_Parameter
              (Ar_Type,
               PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long
                                   (Size (SOAP_Array (Obj)))));
            PolyORB.Any.TypeCode.Add_Parameter
              (Ar_Type,
               To_Any
               (TypeCode.To_Ref (Get_Unwound_Type
                (To_Any (-(SOAP_Array (Obj).O (SOAP_Array (Obj).O'First)))))));

            --  We first build the typecode.

            declare
               Ar : PolyORB.Any.Any :=
                 PolyORB.Any.Get_Empty_Any_Aggregate
                 (Ar_Type);
            begin
               for K in SOAP_Array (Obj).O'Range loop
                  PolyORB.Any.Add_Aggregate_Element
                    (Ar, To_Any (-(SOAP_Array (Obj).O (K))));
               end loop;
               return Ar;
            end;
         end;
      elsif Obj in SOAP_Record then
         declare
            use PolyORB.Any;

            St_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                        PolyORB.Any.TypeCode.TC_Struct;
         begin
            PolyORB.Any.TypeCode.Add_Parameter
              (St_Type,
               PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String
                                   (To_String (SOAP_Record (Obj).Name))));
            PolyORB.Any.TypeCode.Add_Parameter
              (St_Type,
               PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String
                                   ("repository_id")));
            for K in SOAP_Record (Obj).O'Range loop
               PolyORB.Any.TypeCode.Add_Parameter
                 (St_Type, To_Any (TypeCode.To_Ref (Get_Unwound_Type
                                   (To_Any (-(SOAP_Record (Obj).O (K)))))));
               --  thus we get the type

               declare
                  The_Element : constant Object'Class :=
                                  -(SOAP_Record (Obj).O (K));
               begin
                  PolyORB.Any.TypeCode.Add_Parameter
                    (St_Type, PolyORB.Any.To_Any
                     (PolyORB.Types.To_PolyORB_String
                      (To_String (The_Element.Name))));

                  --  Then we add the parameter name
               end;
            end loop;
            --  we first build the typecode

            declare
               St : PolyORB.Any.Any :=
                 PolyORB.Any.Get_Empty_Any_Aggregate
                 (St_Type);
            begin
               for K in SOAP_Record (Obj).O'Range loop
                  PolyORB.Any.Add_Aggregate_Element
                    (St, To_Any (-(SOAP_Record (Obj).O (K))));
               end loop;

               --  Finally we store the values
               return St;
            end;
         end;
      else
         raise Data_Error;
      end if;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : PolyORB.Any.Any) return Object_Access
   is
      use PolyORB.Any;

      Obj : Object_Access;

      Kind_Of_Any : constant PolyORB.Any.TCKind
        := PolyORB.Any.TypeCode.Kind
        (PolyORB.Any.Get_Unwound_Type (Item));
   begin
      pragma Debug (C, O ("From_Any: handling an Any of type "
                       & PolyORB.Any.Image (Get_Unwound_Type (Item))));

      if Kind_Of_Any = Tk_Null then
--           declare
--              Obj : XSD_Null;
--           begin
         Obj := new XSD_Null;
         Obj.Name := To_Unbounded_String ("item");
         return Obj;
--         end;

      elsif Kind_Of_Any = Tk_String then
         declare
            Obj : XSD_String;
            Value : constant PolyORB.Types.String
              := PolyORB.Any.From_Any (Item);
         begin
            Obj.V := To_Unbounded_String (PolyORB.Types.To_String (Value));
            Obj.Name := To_Unbounded_String ("item");
            return new XSD_String'(Obj);
         end;

      elsif Kind_Of_Any = Tk_Long then
         declare
            Obj : XSD_Integer;
            Value : constant PolyORB.Types.Long
              := PolyORB.Any.From_Any (Item);
         begin
            Obj.V := Integer (Value);
            Obj.Name := To_Unbounded_String ("item");
            return new XSD_Integer'(Obj);
         end;

      elsif Kind_Of_Any = Tk_Double then
         declare
            Obj : XSD_Float;
            Value : constant PolyORB.Types.Double
              := PolyORB.Any.From_Any (Item);
         begin
            Obj.V := Long_Float (Value);
            Obj.Name := To_Unbounded_String ("item");
            return new XSD_Float'(Obj);
         end;

      elsif Kind_Of_Any = Tk_Boolean then
         declare
            Obj : XSD_Boolean;
         begin
            Obj.V := PolyORB.Any.From_Any (Item);
            Obj.Name := To_Unbounded_String ("item");
            return new XSD_Boolean'(Obj);
         end;

      elsif Kind_Of_Any = Tk_Sequence then
         declare
            use AWS.Translator;
            use Ada.Streams;
            use PolyORB.Types;

            Number_Of_Elements : constant Unsigned_Long :=
              PolyORB.Any.Get_Aggregate_Count (Item);
            Byte_Stream : Stream_Element_Array
              (0 .. Stream_Element_Offset (Number_Of_Elements) - 1);

         begin
            if PolyORB.Any.TypeCode.Kind
              (PolyORB.Any.TypeCode.Content_Type
               (PolyORB.Any.Get_Type (Item)))
              /= Tk_Octet
            then
               raise Data_Error;
            else
               pragma Debug (C, O ("From_Any: Tk_Sequence (base 64): "
                                & "attempting to retrieve"
                                & Unsigned_Long'Image (Number_Of_Elements)
                                & " octets"));
               for Index in 0 .. Number_Of_Elements - 1 loop
                  declare
                     Element : constant PolyORB.Types.Octet
                       := PolyORB.Any.From_Any
                       (PolyORB.Any.Get_Aggregate_Element
                        (Item, PolyORB.Any.TypeCode.TC_Octet,
                         Index));
                  begin
                     Byte_Stream (Stream_Element_Offset (Index))
                       := Stream_Element (Element);
                  end;
               end loop;
               return new SOAP_Base64'
                 (B64 (Base64_Encode (Byte_Stream), "item"));
            end if;
         end;

      elsif Kind_Of_Any = Tk_Array then
         declare
            use PolyORB.Types;

            Number_Of_Elements : constant Unsigned_Long
              := Unsigned_Long (PolyORB.Any.TypeCode.Length
                                (PolyORB.Any.Get_Type (Item)));

            pragma Debug (C, O ("From_Any: Tk_Array: nb of elements= "
                             & Unsigned_Long'Image (Number_Of_Elements)));

            OS : Object_Set (1 .. Integer (Number_Of_Elements));
            PolyORB_Type_Of_Elements : constant PolyORB.Any.TypeCode.Local_Ref
              := PolyORB.Any.TypeCode.Content_Type
              (PolyORB.Any.Get_Type (Item));
         begin
            for Index in 1 .. Number_Of_Elements loop
               declare
                  New_Object : constant Object_Access :=
                    From_Any
                    (PolyORB.Any.Get_Aggregate_Element
                     (Item, PolyORB_Type_Of_Elements,
                      PolyORB.Types.Unsigned_Long
                      (Index - 1)));
               begin
                  New_Object.Name := To_Unbounded_String ("item");
                  pragma Debug (C, O ("From_Any: Tk_Array: index="
                                   & Positive'Image (Positive (Index))));
                  OS (Positive (Index)) := +(New_Object.all);
               end;
            end loop;
            return new SOAP_Array'(A (OS, "item"));
         end;

      elsif Kind_Of_Any = Tk_Struct then
         declare
            use PolyORB.Types;

            Number_Of_Elements : constant Unsigned_Long
              := Unsigned_Long (PolyORB.Any.TypeCode.Member_Count
                                (PolyORB.Any.Get_Type (Item)));
            OS : Object_Set (1 .. Integer (Number_Of_Elements));

         begin
            for Index in 1 .. Number_Of_Elements loop
               declare
                  Element : constant PolyORB.Any.Any :=
                    (PolyORB.Any.Get_Aggregate_Element
                     (Item, PolyORB.Any.TypeCode.Member_Type
                      (PolyORB.Any.Get_Type (Item), Index - 1),
                      PolyORB.Types.Unsigned_Long (Index - 1)));
                  New_Object : constant Object_Access := From_Any (Element);

               begin
                  New_Object.Name := To_Unbounded_String
                    (PolyORB.Types.To_String
                     (PolyORB.Any.TypeCode.Member_Name
                      (PolyORB.Any.Get_Type (Item), Index - 1)));
                  OS (Positive (Index)) := +(New_Object.all);
               end;
            end loop;
            return new SOAP_Record'(R (OS, "item"));
         end;
      else
         pragma Debug (C, O ("From_Any: no handler found for TCKind "
                          & Image (Get_Unwound_Type (Item)), Critical));
         raise Data_Error;
      end if;
   end From_Any;

end SOAP.Types;
