------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Representations                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------





with Droopi.Types; use Droopi.Types;
with Droopi.Any;
with Sequences.Unbounded;


package Droopi.Representations.SOAP is

   use Droopi.Any;

   subtype Xml_String is Types.String;

   type Xsd_Simple_Types is  (Xsd_Short, Xsd_Int, Xsd_Ushort,
       Xsd_Uint, Xsd_Float, Xsd_Double, Xsd_Boolean, Xsd_Char,
       Xsd_Byte, Xsd_String, Xsd_Longlong,
       Xsd_Ulonglong, Xsd_Undefined);

   Droopi_Types_To_Xml_Types :
    constant array (TCKind'Range) of Xsd_Simple_Types
     := (Tk_Short => Xsd_Short,
         Tk_Long  => Xsd_Int,
         Tk_Ushort => Xsd_Ushort,
         Tk_Ulong => Xsd_Byte,
         Tk_Float => Xsd_Float,
         Tk_Double => Xsd_Double,
         Tk_Boolean => Xsd_Boolean,
         Tk_Char => Xsd_Char,
         Tk_Octet => Xsd_Byte,
         Tk_String =>  Xsd_String,
         Tk_Longlong => Xsd_Longlong,
         Tk_Ulonglong => Xsd_Ulonglong,
         others => Xsd_Undefined);

   Droopi_Types_To_Xml_Strings :
    constant array (TCKind'Range) of Xml_String
     := (Tk_Short => To_Droopi_String ("short"),
         Tk_Long  => To_Droopi_String ("int"),
         Tk_Ushort => To_Droopi_String ("unsigned short"),
         Tk_Ulong => To_Droopi_String ("unsigned int"),
         Tk_Float => To_Droopi_String ("float"),
         Tk_Double => To_Droopi_String ("double"),
         Tk_Boolean => To_Droopi_String ("boolean"),
         Tk_Octet => To_Droopi_String ("byte"),
         Tk_String =>  To_Droopi_String ("string"),
         Tk_Longlong => To_Droopi_String ("long"),
         Tk_Ulonglong => To_Droopi_String ("unsigned long"),
         others => To_Droopi_String ("undefined"));

   SOAP_Error : exception;

   --  type XML_Struct;
   --  type XML_Struct_Access is access all XML_Struct;

   type XML_Struct_Element is
    record
     Name    : Types.Identifier;
     Value   : Any.Any;
   end record;

   package Struct_Seq is new Sequences.Unbounded (XML_Struct_Element);

   type XML_Struct is record
      Name       : Types.Identifier;
      Components : Struct_Seq.Sequence;
   end record;


   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Short)
         return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Long)
         return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Float)
     return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Long_Long)
         return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Unsigned_Short)
         return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg : Types.Unsigned_Long)
         return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Double)
        return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Char)
        return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg :  Types.Boolean)
       return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg :  Types.Octet)
       return Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.String)
       return Xml_String;

   function Any_To_Xml_String
      (Data_Struct  :  XML_Struct_Element)
      return Xml_String;

   function Struct_To_Xml_String
       (Stc           : Xml_Struct;
        Namespace_Uri : Xml_String)
       return Xml_String;

   generic
      type Element is private;
      Xml_Schema_Type  : Xml_String;
      Element_Name     : Types.Identifier;
      type Element_Array is array (Integer range <>) of Element;
      with function Element_To_Xml_String
         (Name : Types.Identifier;
          Arg     : Element) return Xml_String;
   function  Array_To_Xml_String
     (Array_Name : Types.Identifier;
      Arg : Element_Array)
    return Xml_String;


private

   Array_Prefix : constant Xml_String := To_Droopi_String
               ("SOAP-ENC:ArrayType=");

   Struct_Car : constant Types.Char := 'm';
   Namespace_Str : constant Xml_String := To_Droopi_String
               ("xmlns:");

end Droopi.Representations.SOAP;



