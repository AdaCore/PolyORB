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


   Droopi_Types_To_Xml_Types :
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


end Droopi.Representations.SOAP;



