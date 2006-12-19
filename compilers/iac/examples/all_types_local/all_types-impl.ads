with CORBA.Local;
with CORBA;
with CORBA.Object;

package all_types.Impl is

   type LocalObject is
     new CORBA.Local.Object with private;

   type Object_Ptr is
     access all LocalObject'Class;

   function echoBoolean
     (Self : access LocalObject;
      arg : in CORBA.Boolean)
     return CORBA.Boolean;

   function echoShort
     (Self : access LocalObject;
      arg : in CORBA.Short)
     return CORBA.Short;

   function echoLong
     (Self : access LocalObject;
      arg : in CORBA.Long)
     return CORBA.Long;

   function echoUShort
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Short)
     return CORBA.Unsigned_Short;

   function echoULong
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   function echoULLong
     (Self : access LocalObject;
      arg : in CORBA.Unsigned_Long_Long)
     return CORBA.Unsigned_Long_Long;

   function echoFloat
     (Self : access LocalObject;
      arg : in CORBA.Float)
     return CORBA.Float;

   function echoDouble
     (Self : access LocalObject;
      arg : in CORBA.Double)
     return CORBA.Double;

   function echoChar
     (Self : access LocalObject;
      arg : in CORBA.Char)
     return CORBA.Char;

   function echoWChar
     (Self : access LocalObject;
      arg : in CORBA.Wchar)
     return CORBA.Wchar;

   function echoOctet
     (Self : access LocalObject;
      arg : in CORBA.Octet)
     return CORBA.Octet;

   function echoString
     (Self : access LocalObject;
      arg : in CORBA.String)
     return CORBA.String;

   function echoWString
     (Self : access LocalObject;
      arg : in CORBA.Wide_String)
     return CORBA.Wide_String;

   function echoRef
     (Self : access LocalObject;
      arg : in all_types.Local_Ref'Class)
     return all_types.Local_Ref'Class;

   function echoObject
     (Self : access LocalObject;
      arg : in CORBA.Object.Ref)
     return CORBA.Object.Ref;

   function echoOtherAllTypes
     (Self : access LocalObject;
      arg : in all_types.otherAllTypes)
     return all_types.otherAllTypes;

   function echoOtherObject
     (Self : access LocalObject;
      arg : in all_types.otherObject)
     return all_types.otherObject;

   function echoColor
     (Self : access LocalObject;
      arg : in all_types.Color)
     return all_types.Color;

   function echoRainbow
     (Self : access LocalObject;
      arg : in all_types.Rainbow)
     return all_types.Rainbow;

   procedure testException
     (Self : access LocalObject;
      arg : in CORBA.Long);

   procedure testUnknownException
     (Self : access LocalObject;
      arg : in CORBA.Long);

   procedure testSystemException
     (Self : access LocalObject;
      arg : in CORBA.Long);

   function echoUnion
     (Self : access LocalObject;
      arg : in all_types.myUnion)
     return all_types.myUnion;

   function echoUnionEnumSwitch
     (Self : access LocalObject;
      arg : in all_types.myUnionEnumSwitch)
     return all_types.myUnionEnumSwitch;

   function echoArray
     (Self : access LocalObject;
      arg : in all_types.simple_array)
     return all_types.simple_array;

   function echoMatrix
     (Self : access LocalObject;
      arg : in all_types.matrix)
     return all_types.matrix;

   function echoBigMatrix
     (Self : access LocalObject;
      arg : in all_types.bigmatrix)
     return all_types.bigmatrix;

   function echoNestedArray
     (Self : access LocalObject;
      arg : in all_types.nested_array)
     return all_types.nested_array;

   function echoSixteenKb
     (Self : access LocalObject;
      arg : in all_types.sixteenKb)
     return all_types.sixteenKb;

   function echoStruct
     (Self : access LocalObject;
      arg : in all_types.simple_struct)
     return all_types.simple_struct;

   function echoArrayStruct
     (Self : access LocalObject;
      arg : in all_types.array_struct)
     return all_types.array_struct;

   function echoNestedStruct
     (Self : access LocalObject;
      arg : in all_types.nested_struct)
     return all_types.nested_struct;

   function echoUsequence
     (Self : access LocalObject;
      arg : in all_types.U_sequence)
     return all_types.U_sequence;

   function echoBsequence
     (Self : access LocalObject;
      arg : in all_types.B_sequence)
     return all_types.B_sequence;

   function echoMoney
     (Self : access LocalObject;
      arg : in all_types.Money)
     return all_types.Money;

   function Get_Counter
     (Self : access LocalObject)
     return CORBA.Long;

   function Get_myColor
     (Self : access LocalObject)
     return all_types.Color;

   procedure Set_myColor
     (Self : access LocalObject;
      To : in all_types.Color);

   procedure StopServer (Self : access LocalObject);

   function Is_A
     (Self : access LocalObject;
      Logical_Type_Id : in Standard.String)
     return Standard.Boolean;

private
   type LocalObject is new CORBA.Local.Object with
      record
         Attr_My_Color : Color := Blue;
         Attr_Counter  : CORBA.Long := 0;
      end record;

end all_types.Impl;
