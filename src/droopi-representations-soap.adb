------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        SOAP Representations                              --
--                                                                          --
--                               B O D Y                                    --
--                                                                          --
------------------------------------------------------------------------------





--  with Ada.Text_IO;  use Ada.Text_IO;
--  with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Droopi.Types;
with Droopi.Any;

package body Droopi.Representations.SOAP is

   use Droopi.Types;
   use Droopi.Any;

   function Erase_Space (S : String)
    return String;

   function Erase_Space (S : String)
    return String
   is
      Str : String := S (2 .. S'Last);
   begin
      return Str;
   end Erase_Space;


   function To_Xml_String
     (Name : Types.Identifier;
      Arg  : Types.Short)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String :=  Types.Short'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Long)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Long'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Long_Long)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Long_Long'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Unsigned_Short)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Unsigned_Short'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Unsigned_Long)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Unsigned_Long'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Unsigned_Long_Long)
         return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Unsigned_Long_Long'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;


   function To_Xml_String
     (Name : Types.Identifier;
      Arg : Types.Float)
        return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Float'Image (Arg);
   begin
      if Arg > 0.0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;


   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Double)
        return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Double'Image (Arg);
   begin
      if Arg > 0.0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Octet)
        return Xml_String
   is
      S : Xml_String;
      S_Val : Xml_String;
      Str : String := Types.Octet'Image (Arg);
   begin
      if Arg > 0 then
         S_Val := To_Droopi_String (Erase_Space (Str));
      else
         S_Val := To_Droopi_String (Str);
      end if;
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, S_Val);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Char)
       return Xml_String
   is
      S : Xml_String;
   begin
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, Arg);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.Boolean)
       return Xml_String
   is
      S : Xml_String;
      Str : String := Types.Boolean'Image (Arg);
   begin
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, To_Droopi_String (Str));
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;

   function To_Xml_String
      (Name : Types.Identifier;
       Arg  : Types.String)
       return Xml_String
   is
      S : Xml_String;
   begin
      Append (S, "<");
      Append (S, Types.String (Name));
      Append (S, ">");
      Append (S, Arg);
      Append (S, "</");
      Append (S, Types.String (Name));
      Append (S, ">");
      return S;
   end To_Xml_String;


   function Array_To_Xml_String
     (Array_Name : Types.Identifier;
      Arg : Element_Array)
    return Xml_String
   is
      S     : Xml_String;
      Str   : Xml_String;
      Len   : Natural := Arg'Length;
      S_Len : String := Erase_Space (Natural'Image (Len));
   begin

      --  Building the XML Array Tag
      Append (S, "<");
      Append (S, Types.String (Array_Name));
      Append (S, " ");
      Append (S, Array_Prefix);
      Append (S, """");
      Append (S, "xsd:");
      Append (S, Types.String (Xml_Schema_Type));
      Append (S, "[");
      Append (S, To_Droopi_String (S_Len));
      Append (S, "]");
      Append (S, """>");

      --  Building the XML Array Components
      for I in Arg'Range loop
         Str :=  Element_To_Xml_String (Element_Name, Arg (I));
         Append (S, Str);
      end loop;

      --  Building the XML Closing Tag
      Append (S, "</");
      Append (S, To_Standard_String (Array_Name));
      Append (S, ">");
      return S;

   end  Array_To_Xml_String;

   function Any_To_Xml_String
      (Data_Struct  :  XML_Struct_Element)
     return Xml_String
   is
      S : Xml_String;
      Tc :  Xsd_Simple_Types := Droopi_Types_To_Xml_Types
           (Any.TypeCode.Kind (Get_Precise_Type (Data_Struct.Value)));
   begin

      --   Tc := Droopi_Types_To_Xml_Types (Any.TypeCode.Kind
      --          (Get_Precise_Type (Cp.Value)));
      if Tc  /=  Xsd_Undefined then
         raise SOAP_Error;
      end if;


      case Tc is
        when Xsd_Short =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Short'(From_Any (Data_Struct.Value)));
        when Xsd_Int =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Short'(From_Any (Data_Struct.Value)));
        when Xsd_Ushort =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Unsigned_Short'(From_Any (Data_Struct.Value)));
        when Xsd_Uint =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Unsigned_Long'(From_Any (Data_Struct.Value)));
        when Xsd_Float  =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Float'(From_Any (Data_Struct.Value)));
        when Xsd_Double  =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Double'(From_Any (Data_Struct.Value)));
        when Xsd_Boolean =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Boolean'(From_Any (Data_Struct.Value)));
        when Xsd_Char =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Char'(From_Any (Data_Struct.Value)));
        when Xsd_Byte =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Octet'(From_Any (Data_Struct.Value)));
        when Xsd_String =>
           S := To_Xml_String (Data_Struct.Name,
                Types.String'(From_Any (Data_Struct.Value)));
        when Xsd_Longlong  =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Long_Long'(From_Any (Data_Struct.Value)));
        when Xsd_ULonglong  =>
           S := To_Xml_String (Data_Struct.Name,
                Types.Unsigned_Long_Long'(From_Any (Data_Struct.Value)));
        when Xsd_Undefined =>
             raise SOAP_Error;

      end case;

      return S;

   end Any_To_Xml_String;



   function Struct_To_Xml_String
      (Stc            : Xml_Struct;
       NameSpace_Uri : Xml_String)
        return Xml_String
   is
      S : Xml_String;
      Cp : Xml_Struct_Element;
      use Struct_Seq;
   begin
      Append (S, "<");
      Append (S, Struct_Car);
      Append (S, ":");
      Append (S, Types.String (Stc.Name));
      Append (S, " ");
      Append (S, Struct_Car);
      Append (S, "=""");
      Append (S, NameSpace_Uri);
      Append (S, """>");

      for I in 1 .. Length (Stc.Components) loop
          Cp := Element_Of (Stc.Components, I);
          Append (S, Any_To_Xml_String (Cp));
      end loop;

      return S;
    end Struct_To_Xml_String;


end Droopi.Representations.SOAP;



