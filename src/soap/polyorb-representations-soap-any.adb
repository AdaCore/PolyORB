


with PolyORB.Any;
with PolyORB.Types;

package body PolyORB.Representations.SOAP.Any  is

   use PolyORB.Any;

   procedure Any_To_XML_Components
      (Name     :  Types.Identifier;
       Param    :  PolyORB.Any.Any;
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
     (Param    : PolyORB.Any.Any;
      XML_Comp : out XML_Component_Access)
   is

      Nb : constant PolyORB.Types.Unsigned_Long :=
             PolyORB.Any.Get_Aggregate_Count (Param);
      Data_Type : PolyORB.Any.TypeCode.Object
        := PolyORB.Any.Get_Precise_Type (Param);
      Value : PolyORB.Any.Any;
      Content_True_Type : PolyORB.Any.TypeCode.Object :=
            PolyORB.Any.TypeCode.Content_Type (Data_Type);
      Comp : XML_Component;
      Array_Content_Type : Xsd_Kind;
      Type_Tag : XML_String;
      Member_Tag : XML_String := XML_Null_String;

   begin
      while PolyORB.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
      loop
         Content_True_Type :=
             PolyORB.Any.TypeCode.Content_Type (Content_True_Type);
      end loop;

      Array_Content_Type :=  PolyORB_Types_To_XML_Types
           (PolyORB.Any.TypeCode.Kind
           (Content_True_Type));

      if Array_Content_Type  = Xsd_Undefined then
         raise SOAP_Error;
      else if Array_Content_Type = Xsd_Struct or
               Array_Content_Type = Xsd_Array then
            Type_Tag := "xsd:" & Array_Type_Tag;
      else
            Type_Tag :=  XML_String (PolyORB_Types_To_Xsd_Strings
                 (Array_Content_Type));
            Member_Tag :=  Array_Member_Tag & Type_Tag;
      end if;
      end if;

      Comp.Tag := Array_Tag;
      Comp.Component_Type := Xsd_Array;

      declare
         Element_Comp : XML_Component_Access;

         Nb_Str  : XML_String := To_PolyORB_String
                 (Types.Unsigned_Long'Image (Nb));

      begin
         XML_Comp := new XML_Component'(Comp);
         Add_Attributes
           (XML_Comp, Array_Type_Tag, Type_Tag & "[" & Nb_Str & "]");

         --  Building the members
         for I in 0 .. Nb - 1 loop
            Value := PolyORB.Any.Get_Aggregate_Element (Param,
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
      Param    : PolyORB.Any.Any;
      XML_Comp : out XML_Component_Access)
   is
      Data_Type : PolyORB.Any.TypeCode.Object
        := PolyORB.Any.Get_Precise_Type (Param);
      Nb : PolyORB.Types.Unsigned_Long :=
           PolyORB.Any.TypeCode.Member_Count (Data_Type);
      Member : PolyORB.Any.Any;
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
               Member_Name := PolyORB.Any.TypeCode.Member_Name (Data_Type, I);
               Member := PolyORB.Any.Get_Aggregate_Element
                    (Param, PolyORB.Any.TypeCode.Member_Type
                     (Data_Type, I), I);
               Any_To_XML_Components (Member_Name, Member, Element_Comp);
               Add_Child (XML_Comp, Element_Comp);
               Set_Parent (Element_Comp, XML_Comp);
            end loop;
         end if;
      end;

   end Struct_To_XML_Components;


   procedure XML_Component_To_Any
     (XML_Comp : XML_Component_Access;
      Result    : in out PolyORB.Any.Any)
   is
      Tc       : constant TypeCode.Object
        := Get_Precise_Type (Result);
      Is_Empty : constant Boolean
        := PolyORB.Any.Is_Empty (Result);
   begin
      case TypeCode.Kind (Tc) is
         when Tk_Short =>
            declare
               S : Types.Short := Types.Short'Value (To_Standard_String
                    (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Long =>
            declare
               S : Types.Long := Types.Long'Value
                     (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Ushort =>
            declare
               S : Types.Unsigned_Short := Types.Unsigned_Short'Value
                     (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Ulong =>
            declare
               S : Types.Unsigned_Long := Types.Unsigned_Long'Value
                     (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Float =>
            declare
               S : Types.Float := Types.Float'Value
                     (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Double =>
            declare
               S : Types.Double := Types.Double'Value
                     (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Boolean =>
            declare
               S : Types.Boolean := Types.Boolean'Value
                      (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Char =>
            declare
               S : Types.Char := Element (Content_Value (XML_Comp), 1);
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Octet =>
            declare
               S : Types.Octet := Types.Octet'Value
                      (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Struct =>
            declare
               Nb : Unsigned_Long :=
                 TypeCode.Member_Count (Tc);
               Arg : PolyORB.Any.Any;
               Current_Comp : XML_Component_Access;
            begin
               PolyORB.Any.Set_Any_Aggregate_Value (Result);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     if Is_Empty then
                        Arg := Get_Empty_Any (TypeCode.Member_Type (Tc, I));
                     else
                        Arg := Get_Aggregate_Element
                          (Result,
                           TypeCode.Member_Type (Tc, I),
                           I);
                     end if;

                     Current_Comp := Nieme_Child (XML_Comp, Positive (Nb + 1));
                     if Current_Comp /= null then
                        XML_Component_To_Any (Current_Comp, Arg);
                     else
                        raise SOAP_Error;
                     end if;

                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;

         when Tk_Array =>
            declare
               Nb : Unsigned_Long := TypeCode.Length (Tc);
               Content_True_Type : PolyORB.Any.TypeCode.Object :=
                    TypeCode.Content_Type (Tc);
               Arg : PolyORB.Any.Any;
               Current_Comp : XML_Component_Access;
            begin
               while PolyORB.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
               loop
                  Nb := Nb * TypeCode.Length (Content_True_Type);
                  Content_True_Type :=
                    TypeCode.Content_Type (Content_True_Type);
               end loop;

               Set_Any_Aggregate_Value (Result);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     if Is_Empty then
                        Arg := Get_Empty_Any (Content_True_Type);
                     else
                        Arg := Get_Aggregate_Element
                          (Result, Content_True_Type, I);
                     end if;


                     Current_Comp := Nieme_Child (XML_Comp, Positive (Nb + 1));
                     if Current_Comp /= null then
                        XML_Component_To_Any (Current_Comp, Arg);
                     else
                        raise SOAP_Error;
                     end if;

                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;

         when Tk_Longlong =>
            declare
               Ll : Types.Long_Long := Types.Long_Long'Value
                      (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, Ll);
            end;
         when Tk_Ulonglong =>
            declare
               Ull : Types.Unsigned_Long_Long := Types.Unsigned_Long_Long'Value
                      (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, Ull);
            end;
         when Tk_Longdouble =>
            declare
               Ld : Types.Long_Double :=  Types.Long_Double'Value
                      (To_Standard_String (Content_Value (XML_Comp)));
            begin
               Set_Any_Value (Result, Ld);
            end;

         when others =>
            null;
      end case;



   end XML_Component_To_Any;




end  PolyORB.Representations.SOAP.Any;
