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
--  with Droopi.Any; use Droopi.Any;

package Droopi.Representations.SOAP is

   subtype Xml_String is Types.String;

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

end Droopi.Representations.SOAP;



