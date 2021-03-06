------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           S O A P . T Y P E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;
with Ada.Finalization;

with PolyORB.Any;

package SOAP.Types is

   Data_Error : exception;
   --  Raised when a variable has not the expected type.

   type Object is abstract tagged private;

   type Object_Access is access all Object'Class;

   type Object_Safe_Pointer is tagged private;

   type Object_Set is array (Positive range <>) of Object_Safe_Pointer;

   function Image (O : Object) return String;
   --  Returns O value image.

   function XML_Image (O : Object) return String;
   --  Returns O value encoded for use by the Payload object or Response
   --  object.

   function To_NamedValue (O : Object'Class) return PolyORB.Any.NamedValue;
   --  general function that converts a SOAP object into a
   --  NamedValue. It calls To_Any to convert data

   function From_NamedValue (NV : PolyORB.Any.NamedValue)
                            return Object'Class;
   --  general function that converts a PolyORB NamedValue into a SOAP
   --  object. It calls To_Any to convert data

   function To_Any (Obj : Object'Class) return PolyORB.Any.Any;
   --  general function that converts a SOAP object into a PolyORB Any

   function From_Any (Item : PolyORB.Any.Any) return Object_Access;
   --  general function that converts a PolyORB Any into a SOAP
   --  object, the name of which is set to "item"

   function XML_Type (O : Object) return String;
   --  Returns the XML type for the object.

   function Name (O : Object'Class) return String;
   --  Returns name for object O.

   function "+" (O : Object'Class) return Object_Safe_Pointer;
   --  Allocate an object into the heap and return a safe pointer to it.

   function "-" (O : Object_Safe_Pointer) return Object'Class;
   --  Returns the object associated with the safe pointer.

   type Scalar is abstract new Object with private;
   --  Scalar types are using a by-copy semantic.

   type Composite is abstract new Object with private;
   --  Composite types are using a by-reference semantic for efficiency
   --  reason. Not that these types are not thread safe.

   -------------
   -- Integer --
   -------------

   XML_Int : constant String := "xsd:int";

   type XSD_Integer is new Scalar with private;

   function Image     (O : XSD_Integer) return String;
   function XML_Image (O : XSD_Integer) return String;
   function XML_Type  (O : XSD_Integer) return String;

   function I (V : Integer; Name : String := "item") return XSD_Integer;
   function V (O : XSD_Integer) return Integer;

   -----------
   -- Float --
   -----------

   XML_Float : constant String := "xsd:float";

   type XSD_Float is new Scalar with private;

   function Image     (O : XSD_Float) return String;
   function XML_Image (O : XSD_Float) return String;
   function XML_Type  (O : XSD_Float) return String;

   function F (V : Long_Float; Name : String := "item") return XSD_Float;
   function V (O : XSD_Float) return Long_Float;

   ------------
   -- String --
   ------------

   XML_String : constant String := "xsd:string";

   type XSD_String is new Scalar with private;

   function Image     (O : XSD_String) return String;
   function XML_Image (O : XSD_String) return String;
   function XML_Type  (O : XSD_String) return String;

   function S
     (V      : String;
      Name   : String  := "item";
      Encode : Boolean := True)
      return XSD_String;

   function V (O : XSD_String) return String;

   -------------
   -- Boolean --
   -------------

   XML_Boolean : constant String := "xsd:boolean";

   type XSD_Boolean is new Scalar with private;

   function Image     (O : XSD_Boolean) return String;
   function XML_Image (O : XSD_Boolean) return String;
   function XML_Type  (O : XSD_Boolean) return String;

   function B (V : Boolean; Name : String  := "item") return XSD_Boolean;
   function V (O : XSD_Boolean) return Boolean;

   -----------------
   -- TimeInstant --
   -----------------

--   XML_Time_Instant : constant String := "xsd:timeInstant";
--
--   type XSD_Time_Instant is new Scalar with private;
--
--   function Image     (O : XSD_Time_Instant) return String;
--   function XML_Image (O : XSD_Time_Instant) return String;
--   function XML_Type  (O : XSD_Time_Instant) return String;
--
--   subtype TZ is Integer range -11 .. +11;
--   GMT : constant TZ := 0;
--
--   function T
--     (V        : Ada.Calendar.Time;
--     Name     : String        := "item";
--      Timezone : TZ            := GMT)
--     return XSD_Time_Instant;

--   function V (O : XSD_Time_Instant) return Ada.Calendar.Time;
   --  Returns a GMT date and time.

   --  TimeInstant has been disabled, since PolyORB does not have any
   --  time data type for now.

   ----------
   -- Null --
   ----------

   XML_Null : constant String := "1";

   type XSD_Null is new Scalar with private;

   function XML_Image (O : XSD_Null) return String;
   function XML_Type  (O : XSD_Null) return String;

   function N (Name : String  := "item") return XSD_Null;

   ------------
   -- Base64 --
   ------------

   XML_Base64 : constant String := "SOAP-ENC:base64";

   type SOAP_Base64 is new Scalar with private;

   function Image     (O : SOAP_Base64) return String;
   function XML_Image (O : SOAP_Base64) return String;
   function XML_Type  (O : SOAP_Base64) return String;

   function B64
     (V    : String;
      Name : String := "item")
      return SOAP_Base64;

   function V (O : SOAP_Base64) return String;

   -----------
   -- Array --
   -----------

   XML_Array     : constant String := "SOAP-ENC:Array";
   XML_Undefined : constant String := "xsd:ur-type";

   type SOAP_Array is new Composite with private;

   function Image     (O : SOAP_Array) return String;
   function XML_Image (O : SOAP_Array) return String;
   function XML_Type  (O : SOAP_Array) return String;

   function A
     (V    : Object_Set;
      Name : String)
      return SOAP_Array;

   function Size (O : SOAP_Array) return Natural;
   --  Returns the number of item into the array

   function V (O : SOAP_Array; N : Positive) return Object'Class;
   --  Returns SOAP_Array item at position N

   function V (O : SOAP_Array) return Object_Set;

   ------------
   -- Record --
   ------------

   type SOAP_Record is new Composite with private;

   function Image     (O : SOAP_Record) return String;
   function XML_Image (O : SOAP_Record) return String;
   function XML_Type  (O : SOAP_Record) return String;

   function R
     (V    : Object_Set;
      Name : String)
      return SOAP_Record;

   function V (O : SOAP_Record; Name : String) return Object'Class;
   --  Returns SOAP_Record field named Name

   function V (O : SOAP_Record) return Object_Set;

   ---------
   -- Get --
   ---------

   function Get (O : Object'Class) return Integer;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   function Get (O : Object'Class) return Long_Float;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Float.

   function Get (O : Object'Class) return String;
   --  Returns O value as a String. Raises Data_Error if O is not a SOAP
   --  String.

   function Get (O : Object'Class) return Boolean;
   --  Returns O value as a Boolean. Raises Data_Error if O is not a SOAP
   --  Boolean.

   function Get (O : Object'Class) return SOAP_Base64;
   --  Returns O value as a SOAP Base64. Raises Data_Error if O is not a SOAP
   --  Base64 object.

   function Get (O : Object'Class) return SOAP_Record;
   --  Returns O value as a SOAP Struct. Raises Data_Error if O is not a SOAP
   --  Struct.

   function Get (O : Object'Class) return SOAP_Array;
   --  Returns O value as a SOAP Array. Raises Data_Error if O is not a SOAP
   --  Array.

private

   use Ada.Strings.Unbounded;

   type Object_Safe_Pointer is new Ada.Finalization.Controlled with record
      O : Object_Access;
   end record;

   procedure Adjust     (O : in out Object_Safe_Pointer);
   pragma Inline (Adjust);

   procedure Finalize   (O : in out Object_Safe_Pointer);
   pragma Inline (Finalize);

   type Object is abstract new Ada.Finalization.Controlled with record
      Name : Unbounded_String;
   end record;

   type Scalar is abstract new Object with null record;

   type Counter_Access is access Natural;

   type Object_Set_Access is access Object_Set;

   type Composite is abstract new Object with record
      Ref_Counter : Counter_Access;
      O           : Object_Set_Access;
   end record;

   procedure Initialize (O : in out Composite);
   pragma Inline (Initialize);

   procedure Adjust     (O : in out Composite);
   pragma Inline (Adjust);

   procedure Finalize   (O : in out Composite);
   pragma Inline (Finalize);

   type XSD_Integer is new Scalar with record
      V : Integer;
   end record;

   type XSD_Float is new Scalar with record
      V : Long_Float;
   end record;

   type XSD_String is new Scalar with record
      V : Unbounded_String;
   end record;

   type XSD_Boolean is new Scalar with record
      V : Boolean;
   end record;

--   type XSD_Time_Instant is new Scalar with record
--      T        : Ada.Calendar.Time;
--      Timezone : TZ;
--   end record;

   type XSD_Null is new Scalar with null record;

   type SOAP_Base64 is new Scalar with record
      V : Unbounded_String;
   end record;

   type SOAP_Array is new Composite with null record;

   type SOAP_Record is new Composite with null record;

end SOAP.Types;
