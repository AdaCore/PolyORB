with CORBA.ORB;

with Ada.Text_IO;

with all_types.Helper;

package body all_types.Impl is

   pragma Warnings (Off);

   function echoBoolean
     (Self : access LocalObject;
      arg : in CORBA.Boolean)
     return CORBA.Boolean
   is
   begin
      return arg;
   end echoBoolean;

   function echoShort
     (Self : access LocalObject;
      arg : in CORBA.Short)
     return CORBA.Short
   is
   begin
      return arg;
   end echoShort;

   function echoLong
     (Self : access LocalObject;
      arg : in CORBA.Long)
     return CORBA.Long
   is
   begin
      return arg;
   end echoLong;

   function echoUShort
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Short)
     return CORBA.Unsigned_Short
   is
   begin
      return arg;
   end echoUShort;

   function echoULong
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
   begin
      return arg;
   end echoULong;

   function echoULLong
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Long_Long)
     return CORBA.Unsigned_Long_Long
   is
   begin
      return arg;
   end echoULLong;

   function echoFloat
     (Self : access LocalObject;
      arg : in CORBA.Float)
     return CORBA.Float
   is
   begin
      return arg;
   end echoFloat;

   function echoDouble
     (Self : access LocalObject;
      arg : in CORBA.Double)
     return CORBA.Double
   is
   begin
      return arg;
   end echoDouble;

   function echoChar
     (Self : access LocalObject;
      arg : in CORBA.Char)
     return CORBA.Char
   is
   begin
      return arg;
   end echoChar;

   function echoWChar
     (Self : access LocalObject;
      arg : in CORBA.Wchar)
     return CORBA.Wchar
   is
   begin
      return arg;
   end echoWChar;

   function echoOctet
     (Self : access LocalObject;
      arg : in CORBA.Octet)
     return CORBA.Octet
   is
   begin
      return arg;
   end echoOctet;

   function echoString
     (Self : access LocalObject;
      arg : in CORBA.String)
     return CORBA.String
   is
   begin
      Ada.Text_IO.Put_Line
        ("Thus spake I unto myself: « "
         & CORBA.To_Standard_String (arg)
         & " »");
      return arg;
   end echoString;

   function echoWString
     (Self : access LocalObject;
      arg : in CORBA.Wide_String)
     return CORBA.Wide_String
   is
   begin
      return arg;
   end echoWString;

   function echoRef
     (Self : access LocalObject;
      arg : in all_types.Local_Ref'Class)
     return all_types.Local_Ref'Class
   is
   begin
      return arg;
   end echoRef;

   function echoObject
     (Self : access LocalObject;
      arg : in CORBA.Object.Ref)
     return CORBA.Object.Ref
   is
   begin
      return arg;
   end echoObject;

   function echoOtherAllTypes
     (Self : access LocalObject;
      arg : in all_types.otherAllTypes)
     return all_types.otherAllTypes
   is
   begin
      return arg;
   end echoOtherAllTypes;

   function echoOtherObject
     (Self : access LocalObject;
      arg : in all_types.otherObject)
     return all_types.otherObject
   is
   begin
      return arg;
   end echoOtherObject;

   function echoColor
     (Self : access LocalObject;
      arg : in all_types.Color)
     return all_types.Color
   is
   begin
      return arg;
   end echoColor;

   function echoRainbow
     (Self : access LocalObject;
      arg : in all_types.Rainbow)
     return all_types.Rainbow
   is
   begin
      return arg;
   end echoRainbow;

   procedure testException
     (Self : access LocalObject;
      arg : in CORBA.Long)
   is
   begin
      all_types.Helper.Raise_my_exception
        (my_exception_Members'(Info => arg));
   end testException;

   procedure testUnknownException
     (Self : access LocalObject;
      arg : in CORBA.Long)
   is
   begin
      raise Constraint_Error;
   end testUnknownException;

   procedure testSystemException
     (Self : access LocalObject;
      arg : in CORBA.Long)
   is
   begin
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end testSystemException;

   function echoUnion
     (Self : access LocalObject;
      arg : in all_types.myUnion)
     return all_types.myUnion
   is
   begin
      return arg;
   end echoUnion;

   function echoUnionEnumSwitch
     (Self : access LocalObject;
      arg : in all_types.myUnionEnumSwitch)
     return all_types.myUnionEnumSwitch
   is
   begin
      return arg;
   end echoUnionEnumSwitch;

   function echoArray
     (Self : access LocalObject;
      arg : in all_types.simple_array)
     return all_types.simple_array
   is
   begin
      return arg;
   end echoArray;

   function echoMatrix
     (Self : access LocalObject;
      arg : in all_types.matrix)
     return all_types.matrix
   is
   begin
      return arg;
   end echoMatrix;

   function echoBigMatrix
     (Self : access LocalObject;
      arg : in all_types.bigmatrix)
     return all_types.bigmatrix
   is
   begin
      return arg;
   end echoBigMatrix;

   function echoNestedArray
     (Self : access LocalObject;
      arg : in all_types.nested_array)
     return all_types.nested_array
   is
   begin
      return arg;
   end echoNestedArray;

   function echoSixteenKb
     (Self : access LocalObject;
      arg : in all_types.sixteenKb)
     return all_types.sixteenKb
   is
   begin
      return arg;
   end echoSixteenKb;

   function echoStruct
     (Self : access LocalObject;
      arg : in all_types.simple_struct)
     return all_types.simple_struct
   is
   begin
      return arg;
   end echoStruct;

   function echoArrayStruct
     (Self : access LocalObject;
      arg : in all_types.array_struct)
     return all_types.array_struct
   is
   begin
      return arg;
   end echoArrayStruct;

   function echoNestedStruct
     (Self : access LocalObject;
      arg : in all_types.nested_struct)
     return all_types.nested_struct
   is
   begin
      return arg;
   end echoNestedStruct;

   function echoUsequence
     (Self : access LocalObject;
      arg : in all_types.U_sequence)
     return all_types.U_sequence
   is
   begin
      return arg;
   end echoUsequence;

   function echoBsequence
     (Self : access LocalObject;
      arg : in all_types.B_sequence)
     return all_types.B_sequence
   is
   begin
      return arg;
   end echoBsequence;

   function echoMoney
     (Self : access LocalObject;
      arg : in all_types.Money)
     return all_types.Money
   is
   begin
      return arg;
   end echoMoney;

   function Get_Counter
     (Self : access LocalObject)
     return CORBA.Long
   is
      use CORBA;
   begin
      Self.Attr_Counter := Self.Attr_Counter + 1;
      return Self.Attr_Counter;
   end Get_Counter;

   function Get_myColor
     (Self : access LocalObject)
     return all_types.Color
   is
   begin
      return Self.Attr_My_Color;
   end Get_myColor;

   procedure Set_myColor
     (Self : access LocalObject;
      To : in all_types.Color)
   is
   begin
      Self.Attr_My_Color := To;
   end Set_myColor;

   function Is_A
     (Self : access LocalObject;
      Logical_Type_Id : in Standard.String)
     return Standard.Boolean
   is
   begin
      return ((CORBA.Is_Equivalent
        (Logical_Type_Id,
         all_types.Repository_Id)
         or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Object:1.0"))
         or else False);
   end Is_A;

   procedure StopServer (Self : access LocalObject) is
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

end all_types.Impl;
