
with Droopi.Any;
with Droopi.Types;


package body Droopi.Representations.SOAP.Any  is

   use Droopi.Any;

   procedure Any_To_XML_Components
      (Name     :  Types.Identifier;
       Param    :  Droopi.Any.Any;
       XML_Comp :  out XML_Component_Access)
   is
      Tc :  TCKind := TypeCode.Kind (Get_Precise_Type (Param));
   begin
      case Tc is
         when Tk_Short =>
            To_XML_Component (Name,
               Types.Short'(From_Any (Param)), XML_Comp);
         when Tk_Long =>
            To_XML_Component (Name,
              Types.Long'(From_Any (Param)), XML_Comp);
         when Tk_Ushort =>
            To_XML_Component (Name,
             Types.Unsigned_Short'(From_Any (Param)), XML_Comp);
         when Tk_Ulong =>
            To_XML_Component (Name,
             Types.Unsigned_Long'(From_Any (Param)), XML_Comp);
         when Tk_Float  =>
            To_XML_Component (Name,
                Types.Float'(From_Any (Param)), XML_Comp);
         when Tk_Double  =>
            To_XML_Component (Name,
                Types.Double'(From_Any (Param)), XML_Comp);
         when Tk_Boolean =>
            To_XML_Component (Name,
                Types.Boolean'(From_Any (Param)), XML_Comp);
         when Tk_Char =>
            To_XML_Component (Name,
                Types.Char'(From_Any (Param)), XML_Comp);
         when Tk_Octet =>
            To_XML_Component (Name,
                Types.Octet'(From_Any (Param)), XML_Comp);
         when Tk_String =>
            To_XML_Component (Name,
                Types.String'(From_Any (Param)), XML_Comp);
         when Tk_Longlong  =>
            To_XML_Component (Name,
                Types.Long'(From_Any (Param)), XML_Comp);
         when Tk_Ulonglong  =>
            To_XML_Component (Name,
                Types.Unsigned_Long'(From_Any (Param)), XML_Comp);
         when Tk_Struct =>
            Struct_To_XML_Components (Name, Param, XML_Comp);

         when Tk_Array =>
            Array_To_XML_Components (Param, XML_Comp);

         when others =>
            raise SOAP_Error;
      end case;

   end Any_To_XML_Components;


   procedure Array_To_XML_Components
     (Param    : Droopi.Any.Any;
      XML_Comp : out XML_Component_Access)
   is

      Nb : constant Droopi.Types.Unsigned_Long :=
             Droopi.Any.Get_Aggregate_Count (Param);
      Data_Type : Droopi.Any.TypeCode.Object
        := Droopi.Any.Get_Precise_Type (Param);
      Value : Droopi.Any.Any;
      Content_True_Type : Droopi.Any.TypeCode.Object :=
            Droopi.Any.TypeCode.Content_Type (Data_Type);
      Comp : XML_Component;
      Array_Content_Type : Xsd_Kind;
      Type_Tag : XML_String;
      Member_Tag : XML_String := XML_Null_String;

   begin
      while Droopi.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
      loop
         Content_True_Type :=
             Droopi.Any.TypeCode.Content_Type (Content_True_Type);
      end loop;

      Array_Content_Type :=  Droopi_Types_To_XML_Types
           (Droopi.Any.TypeCode.Kind
           (Content_True_Type));

      if Array_Content_Type  = Xsd_Undefined then
         raise SOAP_Error;
      else if Array_Content_Type = Xsd_Struct or
               Array_Content_Type = Xsd_Array then
            Type_Tag := "xsd:" & Array_Type_Tag;
      else
            Type_Tag :=  XML_String (Droopi_Types_To_Xsd_Strings
                 (Array_Content_Type));
            Member_Tag :=  Array_Member_Tag & Type_Tag;
      end if;
      end if;

      Comp.Tag := Array_Tag;
      Comp.Component_Type := Xsd_Array;

      declare
         Element_Comp : XML_Component_Access;

         Nb_Str  : XML_String := To_Droopi_String
                 (Types.Unsigned_Long'Image (Nb));

      begin
         XML_Comp := new XML_Component'(Comp);
         Add_Attributes
           (XML_Comp, Array_Type_Tag, Type_Tag & "[" & Nb_Str & "]");

         --  Building the members
         for I in 0 .. Nb - 1 loop
            Value := Droopi.Any.Get_Aggregate_Element (Param,
                     Content_True_Type, I);
            Any_To_XML_Components (Types.Identifier (Member_Tag),
                  Value, Element_Comp);
            Add_Child (XML_Comp, Element_Comp);
            Set_Parent (Element_Comp, XML_Comp);
         end loop;
      end;
   end Array_To_XML_Components;


   procedure Struct_To_XML_Components
     (Name     : Types.Identifier;
      Param    : Droopi.Any.Any;
      XML_Comp : out XML_Component_Access)
   is
      Data_Type : Droopi.Any.TypeCode.Object
        := Droopi.Any.Get_Precise_Type (Param);
      Nb : Droopi.Types.Unsigned_Long :=
           Droopi.Any.TypeCode.Member_Count (Data_Type);
      Member : Droopi.Any.Any;
      Member_Name : Types.Identifier;
      Comp : XML_Component;
   begin

      Comp.Tag := XML_String (Name);
      Comp.Component_Type := Xsd_Struct;

      declare
         Element_Comp : XML_Component_Access;
      begin
         XML_Comp := new XML_Component'(Comp);

         if Nb /= 0 then
            for I in 1 .. Nb loop
               Member_Name := Droopi.Any.TypeCode.Member_Name (Data_Type, I);
               Member := Droopi.Any.Get_Aggregate_Element
                    (Param, Droopi.Any.TypeCode.Member_Type
                     (Data_Type, I), I);
               Any_To_XML_Components (Member_Name, Member, Element_Comp);
               Add_Child (XML_Comp, Element_Comp);
               Set_Parent (Element_Comp, XML_Comp);
            end loop;
         end if;
      end;

   end Struct_To_XML_Components;



end  Droopi.Representations.SOAP.Any;
