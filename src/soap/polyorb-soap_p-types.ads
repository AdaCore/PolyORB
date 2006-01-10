------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . S O A P _ P . T Y P E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

--  This is a partial implementation of the Representation function
--  for the SOAP protocol personality of PolyORB.

with PolyORB.Any; use PolyORB.Any;

package PolyORB.SOAP_P.Types is

   Data_Error : exception;
   --  Raised when a variable has not the expected type.

   function Image (NV : NamedValue) return String;
   --  Returns O value image.

   function XML_Image (NV : NamedValue) return String;
   --  Returns O value encoded for use by the Payload object
   --  or Response object. This is notionally the Marshall_From_Any
   --  representation operation.

   function XML_Type (NV : NamedValue) return String;
   --  Returns the XML type for the object.

   function Name (NV : NamedValue) return String;
   --  Returns name for object O.

   function Get (NV : NamedValue) return Integer;
   --  Returns O value as an Integer.
   --  Raises Data_Error if O is not a SOAP Integer.

   function Get (NV : NamedValue) return Long_Float;
   --  Returns O value as an Integer.
   --  Raises Data_Error if O is not a SOAP Float.

   function Get (NV : NamedValue) return String;
   --  Returns O value as a String.
   --  Raises Data_Error if O is not a SOAP String.

   function Get (NV : NamedValue) return Boolean;
   --  Returns O value as a Boolean.
   --  Raises Data_Error if O is not a SOAP Boolean.

--    function "+" (NV : NamedValue) return NamedValue_Controlled;
--    --  Allocate an object into the heap and return an access to it.

--    function Image (NV : NamedValue) return String;
--    function XML_Image (NV : NamedValue) return String;
--    function XML_Type (NV : NamedValue) return String;

   -------------
   -- Integer --
   -------------

   XML_Long : constant String := "xsd:long";
   --  -9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807

   XML_ULong : constant String := "xsd:unsignedLong";
   --  0 .. 18_446_744_073_709_551_615

   XML_Int : constant String := "xsd:int";
   --  -2_147_483_648 .. 2_147_483_647

   XML_UInt : constant String := "xsd:unsignedInt";
   --  0 .. 4_294_967_295

   XML_Short : constant String := "xsd:short";
   --  -32_768 .. 32_767

   XML_UShort : constant String := "xsd:unsignedShort";
   --  0 .. 65535

   XML_UByte : constant String := "xsd:unsignedByte";
   --  0 .. 255

--    type XSD_Integer is new Scalar with private;

--    function Image     (O : XSD_Integer) return String;
--    function XML_Image (O : XSD_Integer) return String;
--    function XML_Type  (O : XSD_Integer) return String;

--    function I (V : Integer; Name : String := "item")
--      return XSD_Integer;
--    function V (O : XSD_Integer) return Integer;

--    -----------
--    -- Float --
--    -----------

   XML_Float : constant String := "xsd:float";
   XML_Double : constant String := "xsd:double";

--    type XSD_Float is new Scalar with private;

--    function Image     (O : XSD_Float) return String;
--    function XML_Image (O : XSD_Float) return String;
--    function XML_Type  (O : XSD_Float) return String;

--    function F (V : Long_Float; Name : String := "item")
--      return XSD_Float;
--    function V (O : XSD_Float) return Long_Float;

   ------------
   -- String --
   ------------

   XML_String : constant String := "xsd:string";

--    type XSD_String is new Scalar with private;

--    function Image     (O : XSD_String) return String;
--    function XML_Image (O : XSD_String) return String;
--    function XML_Type  (O : XSD_String) return String;

--    function S
--      (V      : String;
--       Name   : String  := "item";
--       Encode : Boolean := True)
--      return XSD_String;

--    function V (O : XSD_String) return String;

   -------------
   -- Boolean --
   -------------

   XML_Boolean : constant String := "xsd:boolean";

--    type XSD_Boolean is new Scalar with private;

--    function Image     (O : XSD_Boolean) return String;
--    function XML_Image (O : XSD_Boolean) return String;
--    function XML_Type  (O : XSD_Boolean) return String;

--    function B (V : Boolean; Name : String  := "item")
--      return XSD_Boolean;
--    function V (O : XSD_Boolean) return Boolean;

--    -----------------
--    -- TimeInstant --
--    -----------------

--    XML_Time_Instant : constant String := "xsd:timeInstant";

--    type XSD_Time_Instant is new Scalar with private;

--    function Image     (O : XSD_Time_Instant) return String;
--    function XML_Image (O : XSD_Time_Instant) return String;
--    function XML_Type  (O : XSD_Time_Instant) return String;

--    subtype TZ is Integer range -11 .. +11;
--    GMT : constant TZ := 0;

--    function T
--      (V        : Ada.Calendar.Time;
--       Name     : String        := "item";
--       Timezone : TZ            := GMT)
--      return XSD_Time_Instant;

--    function V (O : XSD_Time_Instant) return Ada.Calendar.Time;
--    --  Returns a GMT date and time.

   ----------
   -- Null --
   ----------

   XML_Null : constant String := "1";

--    type XSD_Null is new Scalar with private;

--    function XML_Image (O : XSD_Null) return String;
--    function XML_Type  (O : XSD_Null) return String;

--    function N (Name : String  := "item") return XSD_Null;

--    ------------
--    -- Base64 --
--    ------------

--    XML_Base64 : constant String := "SOAP-ENC:base64";

--    type SOAP_Base64 is new Scalar with private;

--    function Image     (O : SOAP_Base64) return String;
--    function XML_Image (O : SOAP_Base64) return String;
--    function XML_Type  (O : SOAP_Base64) return String;

--    function B64
--      (V    : String;
--       Name : String := "item")
--      return SOAP_Base64;

--    function V (O : SOAP_Base64) return String;

--    -----------
--    -- Array --
--    -----------

   XML_Array     : constant String := "SOAP-ENC:Array";
   XML_Undefined : constant String := "xsd:ur-type";

--    type SOAP_Array is new Composite with private;

--    function Image     (O : SOAP_Array) return String;
--    function XML_Image (O : SOAP_Array) return String;
--    function XML_Type  (O : SOAP_Array) return String;

--    function A
--      (V    : NamedValue_Set;
--       Name : String)
--      return SOAP_Array;

--    function V (O : SOAP_Array) return NamedValue_Set;

   XML_AnyURI : constant String := "xsd:anyURI";

--  private

--    use Ada.Strings.Unbounded;

--    procedure Adjust     (O : in out NamedValue_Controlled);
--    procedure Finalize   (O : in out NamedValue_Controlled);

--    type NamedValue is tagged record
--       Name : Unbounded_String;
--    end record;

--    type Scalar is abstract new NamedValue with null record;

--    type Composite is abstract new NamedValue with null record;

--    type XSD_Integer is new Scalar with record
--       V : Integer;
--    end record;

--    type XSD_Float is new Scalar with record
--       V : Long_Float;
--    end record;

--    type XSD_String is new Scalar with record
--       V : Unbounded_String;
--    end record;

--    type XSD_Boolean is new Scalar with record
--       V : Boolean;
--    end record;

--    type XSD_Time_Instant is new Scalar with record
--       T        : Ada.Calendar.Time;
--       Timezone : TZ;
--    end record;

--    type XSD_Null is new Scalar with null record;

--    type SOAP_Base64 is new Scalar with record
--       V : Unbounded_String;
--    end record;

--    type NamedValue_Set_Access is access NamedValue_Set;

--    type NamedValue_Set_Controlled is
--      new Ada.Finalization.Controlled with record
--       O : NamedValue_Set_Access;
--    end record;

--    procedure Adjust   (O : in out NamedValue_Set_Controlled);
--    procedure Finalize (O : in out NamedValue_Set_Controlled);

--    type SOAP_Array is new Composite with record
--       Items : NamedValue_Set_Controlled;
--    end record;

--    type SOAP_Record is new Composite with record
--       Items : NamedValue_Set_Controlled;
--    end record;

end PolyORB.SOAP_P.Types;
