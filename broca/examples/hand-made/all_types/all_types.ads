with CORBA;
with CORBA.Object;
with Ada.Exceptions;
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA);
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package all_types is

   type Ref is new CORBA.Object.Ref with null record;

   function Unchecked_To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   function To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref;

   function echoBoolean
     (Self : in Ref;
      arg : in CORBA.Boolean)
      return CORBA.Boolean;

   function echoShort
     (Self : in Ref;
      arg : in CORBA.Short)
      return CORBA.Short;

   function echoLong
     (Self : in Ref;
      arg : in CORBA.Long)
      return CORBA.Long;

   function echoUShort
     (Self : in Ref;
      arg : in CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short;

   function echoULong
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;

   function echoFloat
     (Self : in Ref;
      arg : in CORBA.Float)
      return CORBA.Float;

   function echoDouble
     (Self : in Ref;
      arg : in CORBA.Double)
      return CORBA.Double;

   function echoChar
     (Self : in Ref;
      arg : in CORBA.Char)
      return CORBA.Char;

   function echoOctet
     (Self : in Ref;
      arg : in CORBA.Octet)
      return CORBA.Octet;

   function echoString
     (Self : in Ref;
      arg : in CORBA.String)
      return CORBA.String;

   function echoRef
     (Self : in Ref;
      arg : in Ref)
      return Ref;

   type Color is
     (Red,
      Green,
      Blue);

   function echoColor
     (Self : in Ref;
      arg : in Color)
      return Color;

   my_exception : exception;

   type my_exception_Members is new CORBA.IDL_Exception_Members with
   record
      info : CORBA.Long;
   end record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out my_exception_Members);

   --  procedure testException
   --    (Self : in Ref;
   --     arg : in CORBA.Long);

   type myUnion (Switch : CORBA.Long := CORBA.Long'First) is
   record
      case Switch is
      when 1 =>
         Counter : CORBA.Long;
      when 2 =>
         Flag : CORBA.Boolean;
      when 3 =>
         Hue : Color;
      when others =>
         Unknown : CORBA.Long;
      end case;
   end record;

   function echoUnion
     (Self : in Ref;
      arg : in myUnion)
      return myUnion;

   type simple_array is array (0 .. 4) of CORBA.Long;

   type simple_array_Ptr is access simple_array;

   function echoArray
     (Self : in Ref;
      arg : in simple_array)
      return simple_array;

   type matrix is array (0 .. 2, 0 .. 2) of CORBA.Long;

   type matrix_Ptr is access matrix;

   function echoMatrix
     (Self : in Ref;
      arg : in matrix)
      return matrix;

   type a_Array is array (0 .. 9) of CORBA.Long;

   type a_Array_Ptr is access a_Array;

   type simple_struct is record
      a : a_Array;
      s : CORBA.String;
   end record;

   function echoStruct
     (Self : in Ref;
      arg : in simple_struct)
      return simple_struct;

   package IDL_Sequence_Short is
      new CORBA.Sequences.Unbounded (CORBA.Short);

   type U_sequence is new IDL_Sequence_Short.Sequence;

   function echoUsequence
     (Self : in Ref;
      arg : in U_sequence)
      return U_sequence;

   function Get_Counter
     (Self : in Ref)
     return CORBA.Long;

   function Get_myColor
     (Self : in Ref)
     return Color;

   procedure Set_myColor
     (Self : in Ref;
      To   : in Color);

   Repository_Id : constant CORBA.String
      := CORBA.To_CORBA_String ("IDL:all_types:1.0");

   function Is_A
     (Self   : Ref;
      Logical_Type_Id : CORBA.String)
      return CORBA.Boolean;

end all_types;
