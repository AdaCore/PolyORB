------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Unchecked_Conversion;
with Ada.Streams;

with PolyORB.Any;
with PolyORB.Any.ObjRef;
with PolyORB.Buffers;
with PolyORB.Log;
with PolyORB.Opaque;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;
with PolyORB.Utils.Buffers;

package body PolyORB.Representations.CDR is

   use PolyORB.Any;
   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Opaque;
   use PolyORB.Types;
   use PolyORB.Utils.Buffers;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.cdr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------------------
   -- Types conversion functions --
   --------------------------------

   function To_Long_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Long_Long, PolyORB.Types.Long_Long);

   function To_Unsigned_Long_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Long_Long, PolyORB.Types.Unsigned_Long_Long);

   function To_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Long, PolyORB.Types.Long);

   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
     (PolyORB.Types.Long, PolyORB.Types.Unsigned_Long);

   function To_Short is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Short, PolyORB.Types.Short);

   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Short, PolyORB.Types.Unsigned_Short);

   subtype Double_Buf is Stream_Element_Array (1 .. 8);
   --  FIXME LONG DOUBLE
   subtype Long_Double_Buf is  Stream_Element_Array (1 .. 12);
   pragma Warnings (Off);
   pragma Unreferenced (Long_Double_Buf);
   pragma Warnings (On);

   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Float, PolyORB.Types.Unsigned_Long);

   function To_Float is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Long, PolyORB.Types.Float);

   function To_Double_Buf is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Double, Double_Buf);

   function To_Double is
      new Ada.Unchecked_Conversion
        (Double_Buf, PolyORB.Types.Double);

   --   function To_Long_Double_Buf is
   --      new Ada.Unchecked_Conversion
   --       (PolyORB.Types.Long_Double, Long_Double_Buf);
   --   function To_Long_Double is
   --      new Ada.Unchecked_Conversion
   --       (Long_Double_Buf, PolyORB.Types.Long_Double);

   ------------------
   -- TypeCode Ids --
   ------------------

   --  Numerical value associated to TypeCodes, as defined in CDR

   TC_Null_Id               : constant PolyORB.Types.Unsigned_Long := 0;
   TC_Void_Id               : constant PolyORB.Types.Unsigned_Long := 1;
   TC_Short_Id              : constant PolyORB.Types.Unsigned_Long := 2;
   TC_Long_Id               : constant PolyORB.Types.Unsigned_Long := 3;
   TC_Unsigned_Short_Id     : constant PolyORB.Types.Unsigned_Long := 4;
   TC_Unsigned_Long_Id      : constant PolyORB.Types.Unsigned_Long := 5;
   TC_Float_Id              : constant PolyORB.Types.Unsigned_Long := 6;
   TC_Double_Id             : constant PolyORB.Types.Unsigned_Long := 7;
   TC_Boolean_Id            : constant PolyORB.Types.Unsigned_Long := 8;
   TC_Char_Id               : constant PolyORB.Types.Unsigned_Long := 9;
   TC_Octet_Id              : constant PolyORB.Types.Unsigned_Long := 10;
   TC_Any_Id                : constant PolyORB.Types.Unsigned_Long := 11;
   TC_TypeCode_Id           : constant PolyORB.Types.Unsigned_Long := 12;
   TC_Principal_Id          : constant PolyORB.Types.Unsigned_Long := 13;
   TC_Object_Id             : constant PolyORB.Types.Unsigned_Long := 14;
   TC_Struct_Id             : constant PolyORB.Types.Unsigned_Long := 15;
   TC_Union_Id              : constant PolyORB.Types.Unsigned_Long := 16;
   TC_Enum_Id               : constant PolyORB.Types.Unsigned_Long := 17;
   TC_String_Id             : constant PolyORB.Types.Unsigned_Long := 18;
   TC_Sequence_Id           : constant PolyORB.Types.Unsigned_Long := 19;
   TC_Array_Id              : constant PolyORB.Types.Unsigned_Long := 20;
   TC_Alias_Id              : constant PolyORB.Types.Unsigned_Long := 21;
   TC_Except_Id             : constant PolyORB.Types.Unsigned_Long := 22;
   TC_Long_Long_Id          : constant PolyORB.Types.Unsigned_Long := 23;
   TC_Unsigned_Long_Long_Id : constant PolyORB.Types.Unsigned_Long := 24;
   TC_Long_Double_Id        : constant PolyORB.Types.Unsigned_Long := 25;
   TC_Wchar_Id              : constant PolyORB.Types.Unsigned_Long := 26;
   TC_Wide_String_Id        : constant PolyORB.Types.Unsigned_Long := 27;
   TC_Fixed_Id              : constant PolyORB.Types.Unsigned_Long := 28;
   TC_Value_Id              : constant PolyORB.Types.Unsigned_Long := 29;
   TC_Valuebox_Id           : constant PolyORB.Types.Unsigned_Long := 30;
   TC_Native_Id             : constant PolyORB.Types.Unsigned_Long := 31;
   TC_Abstract_Interface_Id : constant PolyORB.Types.Unsigned_Long := 32;

   -----------------
   -- Encapsulate --
   -----------------

   function Encapsulate
     (Buffer : access Buffer_Type)
     return Encapsulation is
   begin
      return Encapsulation'(To_Stream_Element_Array (Buffer));
   end Encapsulate;

   -------------------------
   -- Start_Encapsulation --
   -------------------------

   procedure Start_Encapsulation
     (Buffer : access Buffer_Type) is
   begin
      Set_Initial_Position (Buffer, 0);
      Marshall
        (Buffer,
         PolyORB.Types.Boolean
         (Endianness (Buffer.all) = Little_Endian));
      --  An encapsulation starts with a Boolean value
      --  which is True if the remainder of the buffer is
      --  Little_Endian, and False otherwise.
   end Start_Encapsulation;

   -----------------
   -- Decapsulate --
   -----------------

   procedure Decapsulate
     (Octets : access Encapsulation;
      Buffer : access Buffer_Type)
   is
      Endianness : Endianness_Type;
   begin
      if PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet (Octets (Octets'First))) then
         Endianness := Little_Endian;
      else
         Endianness := Big_Endian;
      end if;

      Initialize_Buffer
        (Buffer               => Buffer,
         Size                 => Octets'Length - 1,
         Data                 => Octets (Octets'First + 1)'Address,
         Endianness           => Endianness,
         Initial_CDR_Position => 1);
   end Decapsulate;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Any) is
   begin
      Marshall_From_Any (Buffer, Data.all);
   end Marshall_From_Any;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.Any)
   is
      Data_Type : constant PolyORB.Any.TypeCode.Object
        := PolyORB.Any.Get_Unwound_Type (Data);
   begin
      pragma Debug (O ("Marshall_From_Any : enter"));
      --  pragma Debug
      --  (0 (Debug_Any(PolyORB.Any.TypeCode.Kind (Data_Type)'Pos)))

      case PolyORB.Any.TypeCode.Kind (Data_Type) is

         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Marshall_From_Any : dealing with void or null"));
            null;

         when Tk_Short =>
            pragma Debug (O ("Marshall_From_Any : dealing with a short"));
            Marshall (Buffer, PolyORB.Types.Short'(From_Any (Data)));

         when Tk_Long =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long"));
            Marshall (Buffer, PolyORB.Types.Long'(From_Any (Data)));

         when Tk_Ushort =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ushort"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Short'(From_Any (Data)));

         when Tk_Ulong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ulong"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(From_Any (Data)));

         when Tk_Float =>
            pragma Debug (O ("Marshall_From_Any : dealing with a float"));
            Marshall (Buffer, PolyORB.Types.Float'(From_Any (Data)));

         when Tk_Double =>
            pragma Debug (O ("Marshall_From_Any : dealing with a double"));
            Marshall (Buffer, PolyORB.Types.Double'(From_Any (Data)));

         when Tk_Boolean =>
            pragma Debug (O ("Marshall_From_Any : dealing with a boolean"));
            Marshall (Buffer, PolyORB.Types.Boolean'(From_Any (Data)));

         when Tk_Char =>
            pragma Debug (O ("Marshall_From_Any : dealing with a char"));
            Marshall (Buffer, PolyORB.Types.Char'(From_Any (Data)));

         when Tk_Octet =>
            pragma Debug (O ("Marshall_From_Any : dealing with an octet"));
            Marshall (Buffer, PolyORB.Types.Octet'(From_Any (Data)));

         when Tk_Any =>
            pragma Debug (O ("Marshall_From_Any : dealing with an any"));
            Marshall (Buffer, PolyORB.Any.Any'(From_Any (Data)));

         when Tk_TypeCode =>
            pragma Debug (O ("Marshall_From_Any : dealing with a typecode"));
            Marshall (Buffer, PolyORB.Any.TypeCode.Object'(From_Any (Data)));

         when Tk_Principal =>
            --  FIXME : to be done
            pragma Debug (O ("Marshall_From_Any : dealing with a principal"));
            raise PolyORB.Not_Implemented;

         when Tk_Objref =>
            pragma Debug (O ("Marshall_From_Any : dealing with an objRef"));
            Marshall (Buffer, PolyORB.Any.ObjRef.From_Any (Data));

         when Tk_Struct | Tk_Except =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long
                 := PolyORB.Any.Get_Aggregate_Count (Data);
               Value : PolyORB.Any.Any;
            begin
               pragma Debug
                 (O ("Marshall_From_Any: dealing with a struct or exception"));

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Value := PolyORB.Any.Get_Aggregate_Element
                       (Data,
                        PolyORB.Any.TypeCode.Member_Type
                        (Data_Type, J), J);
                     Marshall_From_Any (Buffer, Value);
                  end loop;
               end if;
            end;

         when Tk_Union =>
            declare
               Nb : PolyORB.Types.Unsigned_Long;
               Value, Label_Value : PolyORB.Any.Any;
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with an union"));
               Label_Value := Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.Discriminator_Type (Data_Type),
                  PolyORB.Types.Unsigned_Long (0));
               pragma Debug (O ("Marshall_From_Any : got the label"));
               Marshall_From_Any (Buffer, Label_Value);
               pragma Debug (O ("Marshall_From_Any : label marshalled"));
               Nb := PolyORB.Any.Get_Aggregate_Count (Data);
               pragma Debug (O ("Marshall_From_Any : aggregate count = "
                                & PolyORB.Types.Unsigned_Long'Image (Nb)));
               if Nb > 1 then
                  for J in 1 .. Nb - 1 loop
                     pragma Debug (O ("Marshall_From_Any : inside loop, J = "
                                      & Unsigned_Long'Image (J)));
                     Value := PolyORB.Any.Get_Aggregate_Element
                       (Data,
                        PolyORB.Any.TypeCode.Member_Type_With_Label
                        (Data_Type, Label_Value, J - 1),
                        J);
                     pragma Debug (O ("Marshall_From_Any : about "
                                      & "to marshall from any"));
                     Marshall_From_Any (Buffer, Value);
                  end loop;
               end if;
            end;

         when Tk_Enum =>
            pragma Debug (O ("Marshall_From_Any : dealing with an enum"));
            Marshall_From_Any
              (Buffer,
               PolyORB.Any.Get_Aggregate_Element
               (Data,
                PolyORB.Any.TypeCode.TC_Unsigned_Long,
                PolyORB.Types.Unsigned_Long (0)));

         when Tk_String =>
            pragma Debug (O ("Marshall_From_Any : dealing with a string"));
            Marshall (Buffer, PolyORB.Types.String'(From_Any (Data)));

         when Tk_Sequence =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.Get_Aggregate_Count (Data) - 1;
               Value : PolyORB.Any.Any;
            begin
               pragma Debug (O
                  ("Marshall_From_Any : dealing with a sequence"));
               Value := PolyORB.Any.Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.TC_Unsigned_Long,
                  PolyORB.Types.Unsigned_Long (0));
               pragma Assert (Nb = From_Any (Value));
               Marshall_From_Any (Buffer, Value);

               for J in 1 .. Nb loop
                  Value := PolyORB.Any.Get_Aggregate_Element
                    (Data,
                     PolyORB.Any.TypeCode.Content_Type (Data_Type),
                     J);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;

         when Tk_Array =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.Get_Aggregate_Count (Data);
               Value : PolyORB.Any.Any;
               Content_True_Type : PolyORB.Any.TypeCode.Object :=
                 PolyORB.Any.TypeCode.Content_Type (Data_Type);
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with an array"));

               while PolyORB.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
               loop
                  Content_True_Type :=
                    PolyORB.Any.TypeCode.Content_Type (Content_True_Type);
               end loop;

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Value := PolyORB.Any.Get_Aggregate_Element
                       (Data,
                        Content_True_Type,
                        J);
                     pragma Debug (O ("Marshall_From_Any : value kind is "
                                      & PolyORB.Any.TCKind'Image
                                      (PolyORB.Any.TypeCode.Kind
                                       (PolyORB.Any.Get_Type (Value)))));
                     Marshall_From_Any (Buffer, Value);
                  end loop;
               end if;
            end;

         when Tk_Alias =>
            --  we should never reach this point
            pragma Debug (O ("Marshall_From_Any : dealing with an alias"));
            pragma Assert (False);
            raise Program_Error;

         when Tk_Longlong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long long"));
            Marshall (Buffer, PolyORB.Types.Long_Long'(From_Any (Data)));

         when Tk_Ulonglong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a ULongLong"));
            Marshall
              (Buffer,
               PolyORB.Types.Unsigned_Long_Long'(From_Any (Data)));

         when Tk_Longdouble =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a long double"));
            Marshall (Buffer, PolyORB.Types.Long_Double'(From_Any (Data)));

         when Tk_Widechar =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Wchar"));
            Marshall (Buffer, PolyORB.Types.Wchar'(From_Any (Data)));

         when Tk_Wstring =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a wide string"));
            Marshall (Buffer, PolyORB.Types.Wide_String'(From_Any (Data)));

         when Tk_Fixed =>
            --  declare
            --   Digit,Scale: PolyORB.Any.Any;
            --  begin
            --   pragma Debug (O ("Marshall_From_Any : dealing with a fixed"));
            --   Digit:=Get_Aggregate_Element
            --           (Data,
            --            PolyORB.Any.TypeCode.TC_Unsigned_Long,
            --            PolyORB.Any.TypeCode.Fixed_Digits(Data_Type),
            --            PolyORB.Types.Unsigned_Long(0));
            --   Marshall_From_Any(Buffer,Digit);
            --   Scale:=Get_Aggregate_Element
            --           (Data,
            --            PolyORB.Any.TypeCode.Fixed_Scale(Data_Type),
            --            PolyORB.Types.Unsigned_Long(1));
            --   Marshall_From_Any(Buffer,Scale);
            --   end;
            raise PolyORB.Not_Implemented;

         when Tk_Value =>
            declare
               --  Aggregate_Nb, Member_Nb : PolyORB.Types.Unsigned_Long;
               --  Value_Modifier, Value_Type,
               --      Value_Visibility : PolyORB.Any.Any;
               --  Already_Marshalled : False_Seq := Empty_Seq;
            begin
               --  pragma Debug
               --    (O ("Marshall_From_Any : dealing with a value"));
               --  Marshall (Buffer, Default_Value_Tag);

               --  Aggregate_Nb := PolyORB.Any.Get_Aggregate_Count (Data);
               --  Member_Nb := (Aggregate_Nb - 3) / 3;
               --  I := 5;
               --  J := 0;
               --  while (J < Member_Nb) loop
               --     Member_Value := PolyORB.Any.Get_Aggregate_Element
               --       (Data,
               --      PolyORB.Any.TypeCode.Member_Type (Data_Type, I + 3 * J),
               --        J);
               --     declare
               --        Member_Type : constant PolyORB.Any.TypeCode.Object
               --          := PolyORB.Any.Get_Unwound_Type (Member_Value);
               --     begin
               --        case PolyORB.Any.TypeCode.Kind (Member_Type) is
               --           when Tk_Value =>
               --              Marshall_From_Any
               --                (Buffer, Value, Already_Marshalled, 0);
               --           when others =>
               --              Marshall_From_Any (Buffer, Value);
               --        end case;
               --     end;
               --  end loop;
               raise PolyORB.Not_Implemented;
            end;


         when Tk_Valuebox =>
            pragma Debug (O ("Marshall_From_Any : dealing with a valuebox"));
            Marshall_From_Any (Buffer, PolyORB.Any.Get_Aggregate_Element
                 (Data, PolyORB.Any.TypeCode.Member_Type (Data_Type,
                 PolyORB.Types.Unsigned_Long (0)),
                 PolyORB.Types.Unsigned_Long (0)));

         when Tk_Native =>
            pragma Debug (O ("Marshall_From_Any : dealing with a native"));
            --  FIXME : to be done
            raise PolyORB.Not_Implemented;

         when Tk_Abstract_Interface =>
            pragma Debug (O
                 ("Marshall_From_Any : dealing with an abstract interface"));
            --  FIXME : to be done
            raise PolyORB.Not_Implemented;

      end case;
      pragma Debug (O ("Marshall_From_Any : end"));
   end Marshall_From_Any;

   --  procedure Marshall_From_Any
   --    (Buffer          : access Buffer_Type;
   --     Data            : in     PolyORB.Any.Any;
   --     Marshalled_List : in out False_List;
   --     Depth           : in     PolyORB.Types.Long)
   --  is
   --     Success : Boolean;
   --  begin
   --     Success := Marshall_Indirection (Buffer, Data, Already_Marshalled);
   --     if not Success then
   --        declare
   --           Aggregate_Nb, Member_Nb : PolyORB.Types.Unsigned_Long;
   --           Value : PolyORB.Any.Any;
   --        begin
   --           pragma Debug
   --             (O ("Marshall_From_Any : dealing with a value"));
   --           Marshall (Buffer, Default_Value_Tag);

   --           Aggregate_Nb := PolyORB.Any.Get_Aggregate_Count(Data);
   --           Member_Nb := (Aggregate_Nb - 3) / 3;
   --           I := 5;
   --           J := 0;
   --           while (J < Member_Nb) loop
   --              Member_Value := PolyORB.Any.Get_Aggregate_Element
   --                (Data,
   --                 PolyORB.Any.TypeCode.Member_Type (Data_Type, I + 3 * J),
   --                 J);
   --              declare
   --                 Member_Type : constant PolyORB.Any.TypeCode.Object
   --                    := PolyORB.Any.Get_Unwound_Type (Member_Value);
   --              begin
   --                 case PolyORB.Any.TypeCode.Kind (Member_Type) is
   --                    when Tk_Value =>
   --                       Marshall_From_Any
   --                         (Buffer, To_Real (Value),
   --                          Marshalled_List, Depth + 1);
   --                    when others =>
   --                       Marshall_From_Any (Buffer, Member_Value);
   --                 end case;
   --              end;
   --           end loop;
   --        end;
   --     end if;
   --  end Marshall_From_Any;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out PolyORB.Any.Any)
   is
      Tc : constant PolyORB.Any.TypeCode.Object
        := Get_Unwound_Type (Result);
   begin
      pragma Debug (O ("Unmarshall_To_Any : enter"));
      pragma Debug
        (O ("Unmarshall_To_Any : Any_Type is " &
            PolyORB.Any.TCKind'Image (TypeCode.Kind (Tc))));

      case Any.TypeCode.Kind (Tc) is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            declare
               S : constant Short := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a short"));
               pragma Debug (O ("Unmarshall_To_Any : its value is "
                                & PolyORB.Types.Short'Image (S)));
               Set_Any_Value (Result, S);
            end;

         when Tk_Long =>
            declare
               L : constant Long := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a long"));
               Set_Any_Value (Result, L);
            end;

         when Tk_Ushort =>
            declare
               Us : constant Unsigned_Short := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Us);
            end;

         when Tk_Ulong =>
            declare
               Ul : constant Unsigned_Long := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any: dealing with an ulong"));
               Set_Any_Value (Result, Ul);
            end;

         when Tk_Float =>
            declare
               F : constant PolyORB.Types.Float := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, F);
            end;

         when Tk_Double =>
            declare
               D : constant Double := Unmarshall (Buffer);
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any: dealing with a double = "
                     & Double'Image (D)));
               Set_Any_Value (Result, D);
            end;

         when Tk_Boolean =>
            declare
               B : constant PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, B);
            end;

         when Tk_Char =>
            declare
               C : constant Char := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, C);
            end;

         when Tk_Octet =>
            declare
               O : constant PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, O);
            end;

         when Tk_Any =>
            declare
               A : constant Any.Any := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, A);
            end;

         when Tk_TypeCode =>
            declare
               T : constant TypeCode.Object := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any: "
                                & "dealing with a TypeCode"));
               Set_Any_Value (Result, T);
            end;

         when Tk_Principal =>
            --  FIXME : to be done
            raise PolyORB.Not_Implemented;

         when Tk_Objref =>
            PolyORB.Any.ObjRef.Set_Any_Value
              (Result, Unmarshall (Buffer));

         when Tk_Struct | Tk_Except =>
            declare
               Nb : constant Unsigned_Long
                 := TypeCode.Member_Count (Tc);
               Arg : PolyORB.Any.Any := Get_Empty_Any_Aggregate
                 (Get_Type (Result));
               Val : PolyORB.Any.Any;
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a struct"
                                 & " or exception"));

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     pragma Debug (O ("Unmarshall_To_Any : get the element"));
                     Val := Get_Empty_Any (TypeCode.Member_Type (Tc, J));

                     pragma Debug (O ("Unmarshall_To_Any : about to "
                                      & "unmarshall a parameter"));
                     Unmarshall_To_Any (Buffer, Val);
                     Add_Aggregate_Element (Arg, Val);
                  end loop;
               end if;
               Copy_Any_Value (Result, Arg);
               --  XXX VERY inefficient if Result was initially
               --  not empty. In that case, should unmarshall
               --  directly into the already-allocate aggregate
               --  elements.
            end;

         when Tk_Union =>
            declare
               Nb : Unsigned_Long;
               Arg : PolyORB.Any.Any := Get_Empty_Any_Aggregate
                 (Get_Type (Result));
               Label, Val : PolyORB.Any.Any;
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with an union"));
               Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
               Unmarshall_To_Any (Buffer, Label);
               Add_Aggregate_Element (Arg, Label);

               pragma Debug (O ("Unmarshall_To_Any : about to call "
                                & "member_count_with_label"));
               Nb := PolyORB.Any.TypeCode.Member_Count_With_Label (Tc, Label);
               pragma Debug (O ("Now unmarshalling"
                                & Unsigned_Long'Image (Nb) & " elements"));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Val := Get_Empty_Any
                       (TypeCode.Member_Type_With_Label
                        (Tc, Label, J));
                     Unmarshall_To_Any (Buffer, Val);
                     Add_Aggregate_Element (Arg, Val);
                  end loop;
               end if;
               Copy_Any_Value (Result, Arg);
               --  XXX Inefficient, see comment for Tk_Struct above.
            end;

         when Tk_Enum =>
            declare
               Arg : PolyORB.Any.Any
                 := Get_Empty_Any_Aggregate (Get_Type (Result));
               Val : PolyORB.Any.Any
                 := Get_Empty_Any (TC_Unsigned_Long);
            begin
               Unmarshall_To_Any (Buffer, Val);
               Add_Aggregate_Element (Arg, Val);
               Copy_Any_Value (Result, Arg);
               --  XXX Inefficient, see comment for Tk_Struct above.
            end;

         when Tk_String =>
            declare
               S : constant PolyORB.Types.String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, S);
            end;

         when Tk_Sequence =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long
                 := Unmarshall (Buffer);
               Max_Nb : constant Unsigned_Long := TypeCode.Length (Tc);
               Arg : PolyORB.Any.Any
                 := Get_Empty_Any_Aggregate (Get_Type (Result));
               Val : PolyORB.Any.Any;
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any : dealing with a sequence"));
               if Max_Nb > 0 and then Nb > Max_Nb then
                  raise Constraint_Error;
               end if;

               pragma Debug
                 (O ("Unmarshall_To_Any: unmarshalling"
                     & Unsigned_Long'Image (Nb) & " elements"));
               Add_Aggregate_Element (Arg, To_Any (Nb));

               for J in 1 .. Nb loop
                  Val := Get_Empty_Any (TypeCode.Content_Type (Tc));
                  Unmarshall_To_Any (Buffer, Val);
                  Add_Aggregate_Element (Arg, Val);
               end loop;
               pragma Debug (O ("Unmarshalled sequence."));

               Copy_Any_Value (Result, Arg);
            end;

         when Tk_Array =>
            declare
               Nb : Unsigned_Long := TypeCode.Length (Tc);
               Content_True_Type : PolyORB.Any.TypeCode.Object
                 := TypeCode.Content_Type (Tc);
               Arg : PolyORB.Any.Any := Get_Empty_Any_Aggregate
                 (Get_Type (Result));
               Val : PolyORB.Any.Any;
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any : dealing with an array"));
               while PolyORB.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
               loop
                  Nb := Nb * TypeCode.Length (Content_True_Type);
                  Content_True_Type
                    := TypeCode.Content_Type (Content_True_Type);
               end loop;

               pragma Debug
                 (O ("Unmarshall_To_Any: unmarshalling"
                     & Unsigned_Long'Image (Nb) & " elements"));

               for J in 1 .. Nb loop
                  Val := Get_Empty_Any (Content_True_Type);
                  Unmarshall_To_Any (Buffer, Val);
                  Add_Aggregate_Element (Arg, Val);
               end loop;
               pragma Debug (O ("Unmarshall_To_Any: array elements done."));
               Copy_Any_Value (Result, Arg);
               pragma Debug (O ("Unmarshall_To_Any: Copy_Value done."));
            end;

         when Tk_Alias =>
            --  We should never reach this point
            raise Program_Error;

         when Tk_Longlong =>
            declare
               Ll : constant Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ll);
            end;

         when Tk_Ulonglong =>
            declare
               Ull : constant Unsigned_Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ull);
            end;

         when Tk_Longdouble =>
            declare
               Ld : constant Long_Double := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ld);
            end;

         when Tk_Widechar =>
            declare
               Wc : constant Wchar := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Wc);
            end;

         when Tk_Wstring =>
            declare
               Ws : constant PolyORB.Types.Wide_String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ws);
            end;

         when Tk_Fixed =>
            --  FIXME : to be done
            --  declare
            --   Arg1,Arg2:PolyORB.Any.Any;
            --  begin
            --    pragma Debug(0 ("unmarshall_to_any: dealing with a fixed"));
            --    Set_Any_Aggregate_Value(Result);
            --    if Is_Empty then
            --      Arg1:= Get_Empty_Any(TypeCode.Fixed_Digits(Tc));
            --    else
            --      Arg1:= Get_Aggregate_Element
            --             (Result,
            --              TypeCode.Fixed_Digits(Tc),
            --              PolyORB.Types.Unsigned_Long(0));
            --    end if;
            --    Unmarshall_To_Any(Buffer, Arg1);
            --    if Is_Empty then
            --      Add_Aggregate_Element(Result,Arg1);
            --    end if;

            --    if Is_Empty then
            --      Arg2:= Get_Empty_Any(TypeCode.Fixed_Scale(Tc));
            --    else
            --       Arg2:= Get_Aggregate_Element
            --             (Result,
            --              TypeCode.Fixed_Digits(Tc),
            --              PolyORB.Types.Unsigned_Long(0));
            --    end if;
            --    Unmarshall_To_Any(Buffer, Arg2);
            --    if Is_Empty then
            --      Add_Aggregate_Element(Result,Arg2);
            --    end if;
            --   end;
            raise PolyORB.Not_Implemented;

         when Tk_Value =>

            --  declare
            --   Val_Modifier,Arg: PolyORB.Any.Any;
            --   Nb: PolyORB.Types.Unsigned_Long:=
            --          TypeCode.Member_Count(Tc);

            --  begin
            --   Set_Any_Aggregate_Value(Result);
            --   if Is_Empty then
            --     Val_Modifier:= Get_Empty_Any(TypeCode.Type_Modifier(Tc));
            --   else
            --     Val_Modifier:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Discriminator_Type(Tc),
            --                PolyORB.Types.Unsigned_Long(0));
            --   end if;
            --   Unmarshall_To_Any(Buffer,Val_Modifier);
            --   if Is_Empty then
            --     Add_Aggregate_Element(Result,Val_Modifier);
            --   end if;

            --   if Nb /=0 then
            --    for I in 0 .. Nb-1 loop
            --     if Is_Empty then
            --        Arg:= Get_Empty_Any( TypeCode.Member_Visibility(Tc));
            --     else
            --        Arg:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Member_Visibility(Tc,I+1),
            --                I+1);
            --     end if;
            --     Unmarshall_To_Any(Buffer,Arg);
            --     if Is_Empty  then
            --       Add_Aggregate_Element(Result,Arg);
            --     end if;
            --    end loop;
            --   end if;
            --   end;
            raise PolyORB.Not_Implemented;

         when Tk_Valuebox =>
            --  declare
            --     Arg: Corba.Any;
            --  begin
            --     Set_Any_Aggregate_Value(Result);
            --     if Is_Empty then
            --       Arg:= Get_Empty_Any(TypeCode.Member_Type
            --              (Tc,PolyORB.Types.Unsigned_Long(0)));
            --     else
            --       Arg:= PolyORB.Any.Get_Aggregate_Element
            --                 (Result,
            --                  PolyORB.Any.TypeCode.Member_Type(Tc,
            --                  PolyORB.Types.Unsigned_Long(0)));
            --     end if;
            --     Unmarshall_To_Any(Buffer,Arg);
            --     if Is_Empty then
            --       Add_Aggregate_Element(Result, Arg);
            --     end if;
            --  end;
            raise PolyORB.Not_Implemented;

         when Tk_Native =>
            --  FIXME : to be done
            raise PolyORB.Not_Implemented;

         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            raise PolyORB.Not_Implemented;
      end case;
      pragma Debug (O ("Unmarshall_To_Any: end"));
   end Unmarshall_To_Any;

   --------------
   -- Marshall --
   --------------

   --  Marshall-by-copy subprograms for all elementary types.

   --  Marshalling of a Boolean.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Boolean) is
   begin
      pragma Debug (O ("Marshall (Boolean) : enter"));
      Marshall
        (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Boolean'Pos (Data)));
      pragma Debug (O ("Marshall (Boolean) : end"));
   end Marshall;

   --  Marshalling of a Character.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Char) is
   begin
      pragma Debug (O ("Marshall (Char) : enter"));
      Marshall (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Char'Pos (Data)));
      pragma Debug (O ("Marshall (Char) : end"));
   end Marshall;

   --  Marshalling of a Wide Character.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Wchar) is
   begin
      pragma Debug (O ("Marshall (WChar) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'
         (Stream_Element (PolyORB.Types.Wchar'Pos (Data) / 256),
         Stream_Element (PolyORB.Types.Wchar'Pos (Data) mod 256)), 2);
      pragma Debug (O ("Marshall (WChar) : end"));
   end Marshall;

   --  Marshalling of an Octet.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Octet) is
   begin
      pragma Debug (O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (PolyORB.Types.Octet'(Data))), 1);
      pragma Debug (O ("Marshall (Octet) : end"));
   end Marshall;

   --  Marshalling of an Unsigned Short.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Short) is
   begin
      pragma Debug (O ("Marshall (UShort) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'(Stream_Element (Data / 256),
          Stream_Element (Data mod 256)),
         2);
      pragma Debug (O ("Marshall (UShort) : end"));
   end Marshall;

   --  Marshalling of an Unsigned Long.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Long) is
   begin
      pragma Debug (O ("Marshall (ULong) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
          Stream_Element_Array'(Stream_Element (Data / 256**3),
          Stream_Element ((Data / 256**2) mod 256),
          Stream_Element ((Data / 256) mod 256),
          Stream_Element (Data mod 256)),
         4);
      pragma Debug (O ("Marshall (ULong) : end"));
   end Marshall;

   --  Marshalling of an Unsigned Long Long.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Long_Long) is
   begin
      pragma Debug (O ("Marshall (ULongLong) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'(Stream_Element (Data / 256**7),
          Stream_Element ((Data / 256**6) mod 256),
          Stream_Element ((Data / 256**5) mod 256),
          Stream_Element ((Data / 256**4) mod 256),
          Stream_Element ((Data / 256**3) mod 256),
          Stream_Element ((Data / 256**2) mod 256),
          Stream_Element ((Data / 256) mod 256),
          Stream_Element (Data mod 256)),
         8);
      pragma Debug (O ("Marshall (ULongLong) : end"));
   end Marshall;

   --  Marshalling of a Long Long.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long_Long) is
   begin
      pragma Debug (O ("Marshall (LongLong) : enter"));
      Marshall (Buffer, To_Unsigned_Long_Long (Data));
      pragma Debug (O ("Marshall (LongLong) : end"));
   end Marshall;

   --  Marshalling of a Long.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long) is
   begin
      pragma Debug (O ("Marshall (Long) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Long) : end"));
   end Marshall;

   --  Marshalling of a Short.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Short) is
   begin
      pragma Debug (O ("Marshall (Short) : enter"));
      Marshall (Buffer, To_Unsigned_Short (Data));
      pragma Debug (O ("Marshall (Short) : end"));
   end Marshall;

   --  Marshalling of a Float.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Float) is
   begin
      pragma Debug (O ("Marshall (Float) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Float) : end"));
   end Marshall;

   --  Marshalling of a Double.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Double)
   is
      Buf : constant Double_Buf := To_Double_Buf (Data);
   begin
      pragma Debug (O ("Marshall (Double): enter, Data = "
                       & PolyORB.Types.Double'Image (Data)));
      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
      pragma Debug (O ("Marshall (Double): end"));
   end Marshall;

   --  Marshalling of a Long Double.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long_Double)
   is
      --  FIXME LONG DOUBLE
      --   Buf : Long_Double_Buf := To_Long_Double_Buf (Data);
   begin
      raise Not_Implemented;
      --      pragma Debug (O ("Marshall (LongDouble) : enter"));
      --      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
      --      pragma Debug (O ("Marshall (LongDouble) : end"));
   end Marshall;

   --  Marshalling of a Standard String.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     Standard.String) is
   begin
      pragma Debug (O ("Marshall (String) : enter"));

      Marshall (Buffer, PolyORB.Types.Unsigned_Long'(Data'Length + 1));
      for J in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Char (Data (J)));
      end loop;
      Marshall (Buffer, PolyORB.Types.Char (ASCII.Nul));

      pragma Debug (O ("Marshall (String) : end"));
   end Marshall;

   --  Marshalling of a PolyORB.Types.String.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.String) is
   begin
      pragma Debug (O ("Marshall (PolyORB.Types.String) : enter"));
      Marshall (Buffer, PolyORB.Types.To_Standard_String (Data));
      pragma Debug (O ("Marshall (PolyORB.Types.String) : end"));
   end Marshall;

   --  XXX Marshall for PolyORB.Types.Wide_String could also
   --  be implemented as a call to a Marshall for
   --  Standard.Wide_String, just as PolyORB.Types.String/Standard.String.

   --  Marshalling of a Wide String.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Wide_String)
   is
      Equiv : constant Wide_String
        := PolyORB.Types.To_Wide_String (Data)
        & Standard.Wide_Character'Val (0);

      --  XXXXX: Val (0) is suspicious ...
   begin
      pragma Debug (O ("Marshall (PolyORB.Types.Wide_String) : enter"));
      pragma Debug (O ("Marshall (PolyORB.Types.Wide_String) : length is "
                       & PolyORB.Types.Unsigned_Long'Image (Equiv'Length)));

      Marshall (Buffer, PolyORB.Types.Unsigned_Long'(Equiv'Length));
      for J in Equiv'Range loop
         Marshall
           (Buffer, PolyORB.Types.Wchar'Val
            (Wide_Character'Pos (Equiv (J))));
      end loop;

      pragma Debug (O ("Marshall (PolyORB.Types.Wide_String) : end"));
   end Marshall;

   --  Marshalling of an Identifier.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Identifier) is
   begin
      pragma Debug (O ("Marshall (Identifier) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (Identifier) : end"));
   end Marshall;

   --  Marshalling of a Scoped Name.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.ScopedName) is
   begin
      pragma Debug (O ("Marshall (ScopedName) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (ScopedName) : end"));
   end Marshall;

   --  Marshalling of a Repository Identifier.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.RepositoryId) is
   begin
      pragma Debug (O ("Marshall (RepositoryId) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (RepositoryId) : end"));
   end Marshall;

   --  Marshalling of a Value Modifier type (short).
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.ValueModifier) is
   begin
      pragma Debug (O ("Marshall (ValueModifier) : enter"));
      Marshall (Buffer, PolyORB.Types.Short (Data));
      pragma Debug (O ("Marshall (ValueModifier) : end"));
   end Marshall;

   --  Marshalling of a Visibility Type (short).
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.Visibility) is
   begin
      pragma Debug (O ("Marshall (Visibility) : enter"));
      Marshall (Buffer, PolyORB.Types.Short (Data));
      pragma Debug (O ("Marshall (Visibility) : end"));
   end Marshall;

   --  Marshalling of Any Type.
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.Any) is
   begin
      pragma Debug (O ("Marshall (Any) : enter"));
      Marshall (Buffer, Get_Type (Data));
      pragma Debug (O ("Marshall (Any) : type marshalled"));
      Marshall_From_Any (Buffer, Data);
      pragma Debug (O ("Marshall (Any) : end"));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.TypeCode.Object)
   is
      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (O ("Marshall (Typecode) : enter"));
      pragma Debug (O ("Marshall (Typecode) : kind is " &
                       TCKind'Image (PolyORB.Any.TypeCode.Kind (Data))));

      case PolyORB.Any.TypeCode.Kind (Data) is

         when Tk_Null =>
            Marshall (Buffer, TC_Null_Id);

         when Tk_Void =>
            Marshall (Buffer, TC_Void_Id);

         when Tk_Short =>
            Marshall (Buffer, TC_Short_Id);

         when Tk_Long =>
            Marshall (Buffer, TC_Long_Id);

         when Tk_Ushort =>
            Marshall (Buffer, TC_Unsigned_Short_Id);


         when Tk_Ulong =>
            Marshall (Buffer, TC_Unsigned_Long_Id);

         when Tk_Float =>
            Marshall (Buffer, TC_Float_Id);

         when Tk_Double =>
            Marshall (Buffer, TC_Double_Id);

         when Tk_Boolean =>
            Marshall (Buffer, TC_Boolean_Id);

         when Tk_Char =>
            Marshall (Buffer, TC_Char_Id);

         when Tk_Octet =>
            Marshall (Buffer, TC_Octet_Id);

         when Tk_Any =>
            Marshall (Buffer, TC_Any_Id);

         when Tk_TypeCode =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a TypeCode"));
            Marshall (Buffer, TC_TypeCode_Id);

         when Tk_Principal =>
            Marshall (Buffer, TC_Principal_Id);

         when Tk_Objref =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with an ObjRef"));
            Marshall (Buffer, TC_Object_Id);
            pragma Debug (O ("Marshall (TypeCode) : it has "
                             & PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Parameter_Count (Data))
                             & " parameters"));
            Marshall (Buffer, PolyORB.Any.TypeCode.Id (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Name (Data));

         when Tk_Struct =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a struct"));
            Marshall (Buffer, TC_Struct_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (O ("Marshall (TypeCode) : marshalling the id"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            pragma Debug (O ("Marshall (TypeCode) : marshalling the name"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               pragma Debug (O ("Marshall (TypeCode) : " &
                                "marshalling the members. Nb = "
                                & PolyORB.Types.Unsigned_Long'Image (Nb)));
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     pragma Debug (O ("Marshall (TypeCode) : about "
                                      & "to marshall a new  member"));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                     pragma Debug
                       (O ("Marshall (TypeCode) : marshalling "
                           & "the type ("
                           & TCKind'Image
                           (TypeCode.Kind
                            (PolyORB.Any.TypeCode.Member_Type (Data, J)))
                           & ")"));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Type (Data, J));
                     pragma Debug (O ("Marshall (TypeCode) : "
                                      & "member marshalled"));
                  end loop;
               end if;
            end;
            pragma Debug (O ("Marshall : all members marshalled"));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Union =>
            Marshall (Buffer, TC_Union_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Id (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Discriminator_Type (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Default_Index (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall_From_Any
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Label (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Type (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Enum =>
            Marshall (Buffer, TC_Enum_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_String =>
            pragma Debug (O ("marshall (typecode) : dealing with a string"));
            Marshall (Buffer, TC_String_Id);
            pragma Debug (O ("marshall (typecode) : " &
                             "about to marshall length : " &
                             PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Length (Data))));
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
            pragma Debug (O ("marshall (typecode) : length marshalled"));

         when Tk_Sequence =>
            Marshall (Buffer, TC_Sequence_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Array =>
            Marshall (Buffer, TC_Array_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Alias =>
            Marshall (Buffer, TC_Alias_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Except =>
            Marshall (Buffer, TC_Except_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Type (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Longlong =>
            Marshall (Buffer, TC_Long_Long_Id);

         when Tk_Ulonglong =>
            Marshall (Buffer, TC_Unsigned_Long_Long_Id);

         when Tk_Longdouble =>
            Marshall (Buffer, TC_Long_Double_Id);

         when Tk_Widechar =>
            Marshall (Buffer, TC_Wchar_Id);

         when Tk_Wstring =>
            Marshall (Buffer, TC_Wide_String_Id);
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));

         when Tk_Fixed =>
            Marshall (Buffer, TC_Fixed_Id);
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Scale (Data));

         when Tk_Value =>
            Marshall (Buffer, TC_Value_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Id (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Type_Modifier (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Concrete_Base_Type (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Type (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Visibility (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Valuebox =>
            Marshall (Buffer, TC_Valuebox_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Native =>
            Marshall (Buffer, TC_Native_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Abstract_Interface =>
            Marshall (Buffer, TC_Abstract_Interface_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
      end case;
      pragma Debug (O ("Marshall (Typecode) : end"));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.NamedValue) is
   begin
      pragma Debug (O ("Marshall (NamedValue) : enter"));
      Marshall_From_Any (Buffer, Data.Argument);
      pragma Debug (O ("Marshall (NamedValue) : end"));
   end Marshall;

   --  Marshall a sequence of octets

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     Stream_Element_Array) is
   begin
      pragma Debug (O ("Marshall (Encapsulation) : enter"));
      Marshall (Buffer, PolyORB.Types.Unsigned_Long (Data'Length));
      for J in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Octet (Data (J)));
      end loop;
      pragma Debug (O ("Marshall (Encapsulation) : end"));
   end Marshall;

   --  procedure Marshall
   --    (Buffer : access Buffer_Type;
   --     Data   : in Encapsulation) is
   --  begin
   --     pragma Debug (O ("Marshall (Encapsulation) : enter"));
   --     Marshall (Buffer, PolyORB.Types.Unsigned_Long (Data'Length));
   --     for I in Data'Range loop
   --        Marshall (Buffer, PolyORB.Types.Octet (Data (I)));
   --     end loop;
   --     pragma Debug (O ("Marshall (Encapsulation) : end"));
   --  end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.References.Ref'Class) is
   begin
      --  !!!!!!!!!!!!!!!!!
      --  FIXME: I've just noticed that abstract interfaces must be
      --  encoded as unions
      --  with a boolean discriminator, cf spec and change code below.
      --  !!!!!!!!!!!!!!!!!

      --  ValueTypes are not implemented in PolyORB.

--      --  1. if Data is a valuetype, call the valuetype marshalling function
--      if Data in CORBA.Value.Base'Class then
--  --         PolyORB.CORBA_P.Value.Stream.Marshall
--  --           (Buffer, PolyORB.Types.Value.Base'Class (Data));
--         raise PolyORB.Not_Implemented;

--         --  2. check if Data is a nil ref, raise marshall if true
--      elsif CORBA.AbstractBase.Is_Nil (Data) then
--         raise Constraint_Error;

--         --  3. If Data is an abstract interface and the referenced object is
--         --     a valuetype, then call the valuetype marshalling function.
--         --  In practice, just check if the referenced object is a valuetype.
--      elsif CORBA.AbstractBase.Object_Of (Data).all
--        in CORBA.Value.Impl_Base'Class then
--         --  PolyORB.CORBA_P.Value.Stream.Marshall (Buffer,
--         --                             Data);
--         raise PolyORB.Not_Implemented;
--         --  Not implemented yet

--         --  4. Call the interface marshalling function
--      else
      declare
         use PolyORB.References;
         use PolyORB.References.IOR;
         IOR : IOR_Type;
      begin
         Set (IOR, PolyORB.References.Entity_Of (Data));
         Marshall_IOR (Buffer, IOR);
      end;
--      end if;
   end Marshall;

   ---------------------------------------------------
   -- Marshall-by-reference subprograms             --
   -- (for elementary types, these are placeholders --
   -- that actually perform marshalling by copy.    --
   ---------------------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Octet) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Char) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Wchar) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Boolean) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Float) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Double) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Double) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Standard.String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Wide_String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Identifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.ScopedName) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.RepositoryId) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.ValueModifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Visibility) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.TypeCode.Object) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.NamedValue) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Stream_Element_Array) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   --  procedure Marshall
   --   (Buffer : access Buffer_Type;
   --    Data   : access Encapsulation) is
   --  begin
   --    Marshall (Buffer, PolyORB.Types.Unsigned_Long (Data'Length));
   --    Insert_Raw_Data (Buffer, Data'Length,
   --    Opaque_Pointer'(Zone => Zone_Access(Data), Offset => 0));
   --  end Marshall;

   ------------------------------------
   -- Unmarshall-by-copy subprograms --
   ------------------------------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Boolean is
   begin
      pragma Debug (O ("Unmarshall (Boolean) : enter & end"));
      return PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Char is
   begin
      pragma Debug (O ("Unmarshall (Char) : enter & end"));
      return PolyORB.Types.Char'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Wchar
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      pragma Debug (O ("Unmarshall (WChar) : enter & end"));
      return PolyORB.Types.Wchar'Val
        (PolyORB.Types.Unsigned_Long (Octets (Octets'First)) * 256 +
         PolyORB.Types.Unsigned_Long (Octets (Octets'First + 1)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Octet
   is
      Result : constant Stream_Element_Array
        := Align_Unmarshall_Copy (Buffer, 1, 1);
   begin
      pragma Debug (O ("Unmarshall (Octet) : enter & end"));
      return PolyORB.Types.Octet (Result (Result'First));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Short
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      pragma Debug (O ("Unmarshall (UShort) : enter & end"));
      return PolyORB.Types.Unsigned_Short (Octets (Octets'First)) * 256 +
        PolyORB.Types.Unsigned_Short (Octets (Octets'First + 1));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 4, 4);
   begin
      pragma Debug (O ("Unmarshall (ULong) : enter & end"));
      return PolyORB.Types.Unsigned_Long (Octets (Octets'First)) * 256**3
        + PolyORB.Types.Unsigned_Long (Octets (Octets'First + 1)) * 256**2
        + PolyORB.Types.Unsigned_Long (Octets (Octets'First + 2)) * 256
        + PolyORB.Types.Unsigned_Long (Octets (Octets'First + 3));
      --  Hard-coded expression will be optimized by the compiler
      --  as shifts+adds.
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long_Long
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 8, 8);
   begin
      pragma Debug (O ("Unmarshall (ULongLong) : enter & end"));
      return PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First)) * 256**7
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 1)) * 256**6
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 2)) * 256**5
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 3)) * 256**4
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 4)) * 256**3
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 5)) * 256**2
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 6)) * 256
        + PolyORB.Types.Unsigned_Long_Long (Octets (Octets'First + 7));
      --  Hard-coded expression will be optimized by the compiler
      --  as shifts+adds.
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Long is
   begin
      pragma Debug (O ("Unmarshall (LongLong) : enter & end"));
      return To_Long_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long is
   begin
      pragma Debug (O ("Unmarshall (Long) : enter & end"));
      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Short is
   begin
      pragma Debug (O ("Unmarshall (Short) : enter & end"));
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Float is
   begin
      pragma Debug (O ("Unmarshall (Float) : enter & end"));
      return To_Float (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Double
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Host_Endian_Copy (Buffer, 8, 8);
   begin
      pragma Debug (O ("Unmarshall (Double): enter & end"));
      return To_Double (Double_Buf (Octets));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Double is
      --  Octets : constant Stream_Element_Array :=
      --  Align_Unmarshall_Host_Endian_Copy (Buffer, 12, 8);
   begin
      --  pragma Debug (O ("Unmarshall (LongDouble) : enter & end"));
      --  return To_Long_Double (Long_Double_Buf (Octets));
      raise Not_Implemented;
      pragma Warnings (Off);
      return Unmarshall (Buffer);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (O ("Unmarshall (String): enter"));
      pragma Debug (O ("Unmarshall (String): length is " &
                    PolyORB.Types.Unsigned_Long'Image (Length)));

      if Length = 0 then
         return "";
      end if;

      for J in Equiv'Range loop
         Equiv (J) := Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall (Buffer)));
      end loop;

      if Character'Val (PolyORB.Types.Char'Pos (Unmarshall (Buffer)))
        /= ASCII.Nul
      then
         raise Constraint_Error;
      end if;

      pragma Debug (O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.String is
   begin
      return PolyORB.Types.To_PolyORB_String (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Wide_String
   is
      Length : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : Wide_String (1 .. Natural (Length));
   begin
      pragma Debug (O ("Unmarshall (Wide_String) : enter"));
      pragma Debug (O ("Unmarshall (Wide_String) : length is " &
                    PolyORB.Types.Unsigned_Long'Image (Length)));
      for J in Equiv'Range loop
         Equiv (J) := Wide_Character'Val (PolyORB.Types.Wchar'Pos
                                          (Unmarshall (Buffer)));
      end loop;
      pragma Debug (O ("Unmarshall (Wide_String) : end"));
      return PolyORB.Types.To_PolyORB_Wide_String
        (Equiv (1 .. Equiv'Length - 1));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Identifier is
   begin
      pragma Debug (O ("Unmarshall (Identifier) : enter & end"));
      return PolyORB.Types.Identifier
        (PolyORB.Types.String'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.ScopedName is
   begin
      pragma Debug (O ("Unmarshall (ScopedName) : enter & end"));
      return PolyORB.Types.ScopedName
        (PolyORB.Types.String'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.RepositoryId is
   begin
      pragma Debug (O ("Unmarshall (RepositoryId) : enter & end"));
      return PolyORB.Types.RepositoryId
        (PolyORB.Types.String'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.ValueModifier is
   begin
      pragma Debug (O ("Unmarshall (ValueModifier) : enter & end"));
      return PolyORB.Any.ValueModifier
        (PolyORB.Types.Short'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.Visibility is
   begin
      pragma Debug (O ("Unmarshall (Visibility) : enter & end"));
      return PolyORB.Any.Visibility
        (PolyORB.Types.Short'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any;
      Tc     : constant PolyORB.Any.TypeCode.Object
        := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Any) : enter"));
      Result := Get_Empty_Any (Tc);
      Unmarshall_To_Any (Buffer, Result);
      pragma Debug (O ("Unmarshall (Any) : end"));
      return Result;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.TypeCode.Object
   is
      TypeCode_Id : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Result : PolyORB.Any.TypeCode.Object;
   begin
      pragma Debug (O ("Unmarshall (TypeCode) : enter"));

      case TypeCode_Id is
         when TC_Null_Id =>
            Result := PolyORB.Any.TypeCode.TC_Null;

         when TC_Void_Id =>
            Result := PolyORB.Any.TypeCode.TC_Void;

         when TC_Short_Id =>
            Result := PolyORB.Any.TypeCode.TC_Short;

         when TC_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long;

         when TC_Unsigned_Short_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Short;

         when TC_Unsigned_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long;

         when TC_Float_Id =>
            Result := PolyORB.Any.TypeCode.TC_Float;

         when TC_Double_Id =>
            Result := PolyORB.Any.TypeCode.TC_Double;

         when TC_Boolean_Id =>
            Result := PolyORB.Any.TypeCode.TC_Boolean;

         when TC_Char_Id =>
            Result := PolyORB.Any.TypeCode.TC_Char;

         when TC_Octet_Id =>
            Result := PolyORB.Any.TypeCode.TC_Octet;

         when TC_Any_Id =>
            Result := PolyORB.Any.TypeCode.TC_Any;

         when TC_TypeCode_Id =>
            Result := PolyORB.Any.TypeCode.TC_TypeCode;

         when TC_Principal_Id =>
            Result := PolyORB.Any.TypeCode.TC_Principal;

         when TC_Object_Id =>
            Result := PolyORB.Any.TypeCode.TC_Object;
            declare
               Id : PolyORB.Types.String := Unmarshall (Buffer);
               Name : PolyORB.Types.String := Unmarshall (Buffer);
            begin
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Struct_Id =>
            Result := PolyORB.Any.TypeCode.TC_Struct;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               pragma Debug (O ("unmarshall (TypeCode) : dealing "
                                & "with a struct"));
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id   := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb   := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer'Access);
                     Member_Type := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Union_Id =>
            Result := PolyORB.Any.TypeCode.TC_Union;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb, Default_Index : PolyORB.Types.Unsigned_Long;
               Discriminator_Type, Member_Type : PolyORB.Any.TypeCode.Object;
               Member_Label : PolyORB.Any.Any;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Discriminator_Type := Unmarshall (Complex_Buffer'Access);
               Default_Index := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Discriminator_Type));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Default_Index));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Label := Get_Empty_Any (Discriminator_Type);
                     Unmarshall_To_Any (Complex_Buffer'Access, Member_Label);
                     Member_Name := Unmarshall (Complex_Buffer'Access);
                     Member_Type := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, Member_Label);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Enum_Id =>
            Result := PolyORB.Any.TypeCode.TC_Enum;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_String_Id =>
            Result := PolyORB.Any.TypeCode.TC_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;

         when TC_Sequence_Id =>
            Result := PolyORB.Any.TypeCode.TC_Sequence;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               Length := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Array_Id =>
            Result := PolyORB.Any.TypeCode.TC_Array;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               Length := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Alias_Id =>
            Result := PolyORB.Any.TypeCode.TC_Alias;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Except_Id =>
            Result := PolyORB.Any.TypeCode.TC_Except;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer'Access);
                     Member_Type := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Long_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long_Long;

         when TC_Unsigned_Long_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long_Long;

         when TC_Long_Double_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long_Double;

         when TC_Wchar_Id =>
            Result := PolyORB.Any.TypeCode.TC_Wchar;

         when TC_Wide_String_Id =>
            Result := PolyORB.Any.TypeCode.TC_Wide_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;

         when TC_Fixed_Id =>
            Result := PolyORB.Any.TypeCode.TC_Fixed;
            declare
               Fixed_Digits : PolyORB.Types.Unsigned_Short;
               Fixed_Scale : PolyORB.Types.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Digits));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Scale));
            end;

         when TC_Value_Id =>
            Result := PolyORB.Any.TypeCode.TC_Value;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Concrete_Base_Type := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer'Access);
                     Member_Type := Unmarshall (Complex_Buffer'Access);
                     Visibility := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Visibility));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Valuebox_Id =>
            Result := PolyORB.Any.TypeCode.TC_Valuebox;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Native_Id =>
            Result := PolyORB.Any.TypeCode.TC_Native;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Abstract_Interface_Id =>
            Result := PolyORB.Any.TypeCode.TC_Abstract_Interface;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when others =>
            raise Constraint_Error;
      end case;

      pragma Debug (O ("Unmarshall (TypeCode) : end"));
      return Result;
   end Unmarshall;

   function  Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.NamedValue
   is
      NV  :  PolyORB.Any.NamedValue;
      pragma Warnings (Off, NV);
      --  Default initialization.
   begin
      pragma Debug (O ("Unmarshall (NamedValue) : enter"));
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      Unmarshall_To_Any (Buffer, NV.Argument);
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      pragma Debug (O ("Unmarshall (NamedValue) : end"));
      return NV;
   end Unmarshall;


   --   function Unmarshall (Buffer : access Buffer_Type)
   --     return Encapsulation
   --   is
   --      Length : constant PolyORB.Types.Unsigned_Long
   --        := Unmarshall (Buffer);
   --   begin
   --      pragma Debug (O ("Unmarshall (Encapsulation):
   --                length is" & Length'Img));
   --      declare
   --         E : Encapsulation (1 .. Stream_Element_Offset(Length));
   --      begin
   --         for I in E'Range loop
   --            E (I) := Stream_Element(PolyORB.Types.Octet'
   --                (Unmarshall (Buffer)));
   --         end loop;
   --         pragma Debug (O ("Unmarshall (Encapsulation): end"));
   --         return E;
   --      end;
   --   end Unmarshall;

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data   : in out PolyORB.References.Ref'Class)
   is
      use PolyORB.References;
      use PolyORB.References.IOR;
      IOR : constant IOR_Type := Unmarshall_IOR (Buffer);
   begin
      PolyORB.References.Set (Data, Entity_Of (IOR));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.References.Ref
   is
      Result : PolyORB.References.Ref;
   begin
      Unmarshall (Buffer, Result);
      return Result;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Stream_Element_Array
   is
      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Encapsulation): length is" & Length'Img));
      declare
         E : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      begin
         for J in E'Range loop
            E (J) := Stream_Element
                     (PolyORB.Types.Octet'(Unmarshall (Buffer)));
         end loop;
         pragma Debug (O ("Unmarshall (Encapsulation): end"));
         return E;
      end;
   end Unmarshall;

   -----------------
   -- Fixed_Point --
   -----------------

   package body Fixed_Point is

      Fixed_Positive_Zero : constant Stream_Element
        := 16#C#;
      Fixed_Negative : constant Stream_Element
        := 16#D#;

      Max_Digits : constant := 31;
      --  31 is the maximum number of digits for a fixed type

      --------------
      -- Marshall --
      --------------

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F) is
      begin
         Marshall (Buffer, Data.all);
      end Marshall;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : in     F) is
      begin
         Align_Marshall_Copy (Buffer, Fixed_To_Octets (Data), 1);
      end Marshall;

      ----------------
      -- Unmarshall --
      ----------------

      function Unmarshall
        (Buffer : access Buffer_Type)
        return F
      is
         Octets : Stream_Element_Array (1 .. Max_Digits) := (others => 0);
         I : Stream_Element_Count := 0;
      begin
         loop
            I := I + 1;
            Octets (I) := Stream_Element
              (PolyORB.Types.Octet'(Unmarshall (Buffer)));
            exit when Octets (I) mod 16 > 9;
         end loop;
         return Octets_To_Fixed (Octets (1 .. I));
      end Unmarshall;

      ---------------------
      -- Fixed_To_Octets --
      ---------------------

      function Fixed_To_Octets
        (Data : in F)
        return Stream_Element_Array
      is
         N_Digits : Integer := 0;
         Val : F := Data;
      begin
         loop
            N_Digits := N_Digits + 1;
            Val := Val * 0.1;
            exit when Val = 0.0;
         end loop;

         declare
            Octets : Stream_Element_Array
              (0 .. Stream_Element_Offset ((N_Digits + 2) / 2 - 1))
              := (others => 0);
            --  The size of the representation is
            --  at least 1, plus 1 nibble for the sign.

            Offset : Integer;
            Bias : F;
         begin
            if N_Digits mod 2 /= 0 then
               Offset := 0;
            else
               Offset := 1;
            end if;

            Val := Data;
            Bias := F (10.0 ** (N_Digits - F'Scale + 1));

            for J in Offset .. Offset + N_Digits loop
               declare
                  Digit : constant PolyORB.Types.Octet
                    := PolyORB.Types.Octet (Val / Bias);
               begin
                  if J mod 2 = 0 then
                     Octets (Stream_Element_Offset (J / 2))
                       := Stream_Element (Digit * 16);
                  else
                     Octets (Stream_Element_Offset (J / 2))
                       := Octets (Stream_Element_Offset (J / 2)) +
                       Stream_Element (Digit);
                  end if;

                  Val := Val - F (Digit) * Bias;
                  Bias := 0.1 * Bias;
               end;
            end loop;

            if Data >= 0.0 then
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Fixed_Positive_Zero;
            else
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Fixed_Negative;
            end if;

            return Octets;
         end;
      end Fixed_To_Octets;

      ---------------------
      -- Octets_To_Fixed --
      ---------------------

      function Octets_To_Fixed
        (Octets : Stream_Element_Array)
        return F
      is
         Result : F := 0.0;
      begin
         for J in Octets'Range loop
            if Octets (J) / 16 > 9
              or else (Octets (J) mod 16 > 9 and then J < Octets'Last)
              or else (Octets (J) mod 16 > 9 and then J = Octets'Last
                       and then (Octets (J) mod 16 /= Fixed_Positive_Zero)
                       and then (Octets (J) mod 16 /= Fixed_Negative))
            then
               pragma Debug
                 (O ("Octets_To_Fixed : exception raised, " &
                     "Octets (J) = " & Stream_Element'Image
                     (Octets (J))));
               raise Constraint_Error;
            end if;

            Result := Result * 10 + F (Octets (J) / 16) * F'Delta;
            if J < Octets'Last then
               Result := Result * 10 + F (Octets (J) mod 16) * F'Delta;
            else
               if Octets (J) mod 16 = Fixed_Negative then
                  Result := -Result;
               end if;
            end if;

         end loop;
         return Result;
      end Octets_To_Fixed;

   end Fixed_Point;

end PolyORB.Representations.CDR;
