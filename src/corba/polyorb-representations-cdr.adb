------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Exceptions;

with PolyORB.Any; use PolyORB.Any;

with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Opaque;  use PolyORB.Opaque;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Utils.Buffers; use PolyORB.Utils.Buffers;

with CORBA;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Exceptions.Stack;
with PolyORB.Types;

package body PolyORB.Representations.CDR is

   use PolyORB.Log;
   use PolyORB.CORBA_P.Exceptions;
   use PolyORB.CORBA_P.Exceptions.Stack;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.cdr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

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

   procedure Decapsulate
     (Octets : access Encapsulation;
      Buffer : access Buffer_Type)
   is
      Endianness : Endianness_Type;
      Z : constant Zone_Access
        := Zone_Access'(Octets.all'Unchecked_Access);
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
         Data                 =>
           (Zone   => Z,
            Offset => Z'First + 1),
         --  Bypass runtime accessibility check.
         Endianness           => Endianness,
         Initial_CDR_Position => 1);
   end Decapsulate;

   function Encapsulate
     (Buffer : access Buffer_Type)
     return Encapsulation is
   begin
      return Encapsulation'(To_Stream_Element_Array (Buffer));
   end Encapsulate;

   ------------------------------------------
   -- Conversions between CORBA signed and --
   -- unsigned integer types.              --
   ------------------------------------------

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

   -------------------------------------------
   --  Conversions for floating point types --
   -------------------------------------------

   subtype Double_Buf is Stream_Element_Array (1 .. 8);
   --  FIXME LONG DOUBLE
   subtype Long_Double_Buf is  Stream_Element_Array (1 .. 12);

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


   ----------------------------------
   -- Marshall-by-copy subprograms --
   -- for all elementary types     --
   ----------------------------------

   --  Marshalling of a Boolean
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Boolean) is
   begin
      pragma Debug (O ("Marshall (Boolean) : enter"));
      Marshall
        (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Boolean'Pos (Data)));
      pragma Debug (O ("Marshall (Boolean) : end"));
   end Marshall;

   --  Marshalling of a character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Char) is
   begin
      pragma Debug (O ("Marshall (Char) : enter"));
      Marshall (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Char'Pos (Data)));
      pragma Debug (O ("Marshall (Char) : end"));
   end Marshall;

   --  Marshalling of a wide character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Wchar)
   is

   begin
      pragma Debug (O ("Marshall (WChar) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'
         (Stream_Element (PolyORB.Types.Wchar'Pos (Data) / 256),
         Stream_Element (PolyORB.Types.Wchar'Pos (Data) mod 256)), 2);
      pragma Debug (O ("Marshall (WChar) : end"));
   end Marshall;

   --  Marshalling of a Octet
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Octet) is
   begin
      pragma Debug (O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (PolyORB.Types.Octet'(Data))), 1);
      pragma Debug (O ("Marshall (Octet) : end"));
   end Marshall;

   --  Marshalling of an unsigned short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Unsigned_Short)
   is

   begin
      pragma Debug (O ("Marshall (UShort) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'(Stream_Element (Data / 256),
          Stream_Element (Data mod 256)),
         2);
      pragma Debug (O ("Marshall (UShort) : end"));
   end Marshall;

   --  Marshalling of an unsigned long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Unsigned_Long)
   is

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

   --  Marshalling of an unsigned long long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Unsigned_Long_Long) is
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

   --  Marshalling of a long long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Long_Long) is
   begin
      pragma Debug (O ("Marshall (LongLong) : enter"));
      Marshall (Buffer, To_Unsigned_Long_Long (Data));
      pragma Debug (O ("Marshall (LongLong) : end"));
   end Marshall;

   --  Marshalling of a long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Long) is
   begin
      pragma Debug (O ("Marshall (Long) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Long) : end"));
   end Marshall;

   --  Marshalling of a short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Short) is
   begin
      pragma Debug (O ("Marshall (Short) : enter"));
      Marshall (Buffer, To_Unsigned_Short (Data));
      pragma Debug (O ("Marshall (Short) : end"));
   end Marshall;

   --  Marshalling of a float
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Float) is
   begin
      pragma Debug (O ("Marshall (Float) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Float) : end"));
   end Marshall;

   --  Marshalling of a double
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Double)
   is
      Buf : Double_Buf := To_Double_Buf (Data);
   begin
      pragma Debug (O ("Marshall (Double) : enter"));
      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
      pragma Debug (O ("Marshall (Double) : end"));
   end Marshall;

   --  Marshalling of a long double
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Long_Double)
   is
      --  FIXME LONG DOUBLE
      --   Buf : Long_Double_Buf := To_Long_Double_Buf (Data);
   begin
      raise Not_Implemented;
      --      pragma Debug (O ("Marshall (LongDouble) : enter"));
      --      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
      --      pragma Debug (O ("Marshall (LongDouble) : end"));
   end Marshall;

   --  Marshalling of a standard string
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Standard.String) is
   begin
      pragma Debug (O ("Marshall (String) : enter"));

      Marshall (Buffer, PolyORB.Types.Unsigned_Long'(Data'Length + 1));
      for I in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Char (Data (I)));
      end loop;
      Marshall (Buffer, PolyORB.Types.Char (ASCII.Nul));

      pragma Debug (O ("Marshall (String) : end"));
   end Marshall;

   --  Marshalling of a Corba String
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.String) is
   begin
      pragma Debug (O ("Marshall (PolyORB.Types.String) : enter"));
      Marshall (Buffer, PolyORB.Types.To_Standard_String (Data));
      pragma Debug (O ("Marshall (PolyORB.Types.String) : end"));
   end Marshall;

   --  Marshall for PolyORB.Types.Wide_String could also
   --  be implemented as a call to a Marshall for
   --  Standard.Wide_String, just as PolyORB.Types.String/Standard.String.

   --  Marshalling of a wide string
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Wide_String)
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
      for I in Equiv'Range loop
         Marshall
           (Buffer, PolyORB.Types.Wchar'Val
            (Wide_Character'Pos (Equiv (I))));
      end loop;

      pragma Debug (O ("Marshall (PolyORB.Types.Wide_String) : end"));
   end Marshall;

   --  Marshalling of a Corba Identifier
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Identifier) is
   begin
      pragma Debug (O ("Marshall (Identifier) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (Identifier) : end"));
   end Marshall;

   --  Marshalling of a Corba Scoped Name
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.ScopedName) is
   begin
      pragma Debug (O ("Marshall (ScopedName) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (ScopedName) : end"));
   end Marshall;

   --  Marshalling of a Corba Repository Identifier
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.RepositoryId) is
   begin
      pragma Debug (O ("Marshall (RepositoryId) : enter"));
      Marshall (Buffer, PolyORB.Types.String (Data));
      pragma Debug (O ("Marshall (RepositoryId) : end"));
   end Marshall;

   --  Marshalling of a Corba Value Modifier type (short)
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.ValueModifier) is
   begin
      pragma Debug (O ("Marshall (ValueModifier) : enter"));
      Marshall (Buffer, PolyORB.Types.Short (Data));
      pragma Debug (O ("Marshall (ValueModifier) : end"));
   end Marshall;

   --  Marshalling of a Corba Visibility Type (short)
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.Visibility) is
   begin
      pragma Debug (O ("Marshall (Visibility) : enter"));
      Marshall (Buffer, PolyORB.Types.Short (Data));
      pragma Debug (O ("Marshall (Visibility) : end"));
   end Marshall;


   --  Marshalling of Corba Any Type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.Any) is
   begin
      pragma Debug (O ("Marshall (Any) : enter"));
      Marshall (Buffer, Get_Type (Data));
      pragma Debug (O ("Marshall (Any) : type marshalled"));
      Marshall_From_Any (Buffer, Data);
      pragma Debug (O ("Marshall (Any) : end"));
   end Marshall;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.Any)
   is
      Data_Type : PolyORB.Any.TypeCode.Object
        := PolyORB.Any.Get_Precise_Type (Data);
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
            null;

         when Tk_Objref =>
            pragma Debug (O ("Marshall_From_Any : dealing with an objRef"));
            --  Marshall (Buffer, PolyORB.Types.Object.Helper.From_Any (Data));
            null;


         when Tk_Struct =>
            declare
                  Nb : constant PolyORB.Types.Unsigned_Long
                        := PolyORB.Any.Get_Aggregate_Count (Data);
                  Value : PolyORB.Any.Any;
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with a struct"));
               for I in 0 .. Nb - 1 loop
                  Value := PolyORB.Any.Get_Aggregate_Element
                     (Data,
                      PolyORB.Any.TypeCode.Member_Type (Data_Type, I), I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
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
                  for I in 1 .. Nb - 1 loop
                     pragma Debug (O ("Marshall_From_Any : inside loop, I = "
                                      & Unsigned_Long'Image (I)));
                     Value := PolyORB.Any.Get_Aggregate_Element
                       (Data,
                        PolyORB.Any.TypeCode.Member_Type_With_Label
                        (Data_Type, Label_Value, I - 1),
                        I);
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
                 PolyORB.Any.Get_Aggregate_Count (Data);
               Value : PolyORB.Any.Any;
            begin
               pragma Debug (O
                  ("Marshall_From_Any : dealing with a sequence"));
               Value := PolyORB.Any.Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.TC_Unsigned_Long,
                  PolyORB.Types.Unsigned_Long (0));
               Marshall_From_Any (Buffer, Value);
               if Nb /= 0 then
                  for I in 1 .. Nb - 1 loop
                     Value := PolyORB.Any.Get_Aggregate_Element
                       (Data,
                        PolyORB.Any.TypeCode.Content_Type (Data_Type),
                        I);
                     Marshall_From_Any (Buffer, Value);
                  end loop;
               end if;
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

               for I in 0 .. Nb - 1 loop
                  Value := PolyORB.Any.Get_Aggregate_Element
                    (Data,
                     Content_True_Type,
                     I);
                  pragma Debug (O ("Marshall_From_Any : value kind is "
                                   & PolyORB.Any.TCKind'Image
                                   (PolyORB.Any.TypeCode.Kind
                                    (PolyORB.Any.Get_Type (Value)))));
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;

         when Tk_Alias =>
            --  we should never reach this point
            pragma Debug (O ("Marshall_From_Any : dealing with an alias"));
            pragma Assert (False);
            raise Program_Error;

         when Tk_Except =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.Get_Aggregate_Count (Data);
               Value : PolyORB.Any.Any;
            begin
               pragma Debug
                 (O ("Marshall_From_Any : dealing with an exception"));
               for I in 0 .. Nb - 1 loop
                  Value := PolyORB.Any.Get_Aggregate_Element
                    (Data,
                     PolyORB.Any.TypeCode.Member_Type (Data_Type, I),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;

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
            null;

         when Tk_Value =>
            --  declare
            --     Nb: PolyORB.Types.Unsigned_Long;
            --     Value_Modifier, Value_TypeCode,
            --         Value_Visibility : PolyORB.Any.Any;
            --  begin
            --    pragma Debug
            --        (O ("Marshall_From_Any : dealing with a value"));
            --    Value_Modifier:= PolyORB.Any.Get_Aggregate_Element
            --         (Data,
            --          PolyORB.Any.TypeCode.Type_Modifier(Data_Type),
            --          PolyORB.Types.Unsigned_Long(0));
            --  pragma Debug (0 ("Marshall_From_Any: got the value_modifier"));
            --    Marshall_From_Any(Buffer,Val_Modifier);
            --    Nb := PolyORB.Any.Get_Aggregate_Count(Data);
            --    if Nb>1 then
            --     while I<Nb-1 loop

            --       Value_Value:= PolyORB.Any.Get_Aggregate_Element
            --         (Data,
            --          PolyORB.Any.TypeCode.Member_Type (Data_Type, I),
            --          I);
            --       I:=I+1;
            --       Value_Visibility:=  PolyORB.Any.Get_Aggregate_Element
            --         (Data,
            --          PolyORB.Any.TypeCode.Member_Visibility(Data_Type, I),
            --          I);
            --       Marshall_From_Any(Buffer, Value);
            --       I:=I+2;

            --     end loop;
            --    end if;
            --   end;
            null;


         when Tk_Valuebox =>
            pragma Debug (O ("Marshall_From_Any : dealing with a valuebox"));
            Marshall_From_Any (Buffer, PolyORB.Any.Get_Aggregate_Element
                 (Data, PolyORB.Any.TypeCode.Member_Type (Data_Type,
                 PolyORB.Types.Unsigned_Long (0)),
                 PolyORB.Types.Unsigned_Long (0)));

         when Tk_Native =>
            pragma Debug (O ("Marshall_From_Any : dealing with a native"));
            --  FIXME : to be done
            --  pragma Debug (O ("Marshall_From_Any : dealing with a native"));

            null;

         when Tk_Abstract_Interface =>
            pragma Debug (O
                 ("Marshall_From_Any : dealing with an abstract interface"));
            --  FIXME : to be done
            --  pragma Debug (O ("Marshall_From_Any : dealing with "
            --                 & "an abstract interface"));
            null;
      end case;
      pragma Debug (O ("Marshall_From_Any : end"));
   end Marshall_From_Any;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.TypeCode.Object)
   is
      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (O ("Marshall (Typecode) : enter"));
      pragma Debug (O ("Marshall (Typecode) : kind is " &
                       TCKind'Image (PolyORB.Any.TypeCode.Kind (Data))));
      case PolyORB.Any.TypeCode.Kind (Data) is
         when Tk_Null =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(0));
         when Tk_Void =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(1));
         when Tk_Short =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(2));
         when Tk_Long =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(3));
         when Tk_Ushort =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(4));
         when Tk_Ulong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(5));
         when Tk_Float =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(6));
         when Tk_Double =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(7));
         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(8));
         when Tk_Char =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(9));
         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(10));
         when Tk_Any =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(11));
         when Tk_TypeCode =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a TypeCode"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(12));
         when Tk_Principal =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(13));
         when Tk_Objref =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with an ObjRef"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(14));
            pragma Debug (O ("Marshall (TypeCode) : it has "
                             & PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Parameter_Count (Data))
                             & " parameters"));
            Marshall (Buffer, PolyORB.Any.TypeCode.Id (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Name (Data));
         when Tk_Struct =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a struct"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(15));
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (O ("Marshall (TypeCode) : marshalling the id"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            pragma Debug (O ("Marshall (TypeCode) : marshalling the name"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               pragma Debug (O ("Marshall (TypeCode) : " &
                                "marshalling the members. Nb = "
                                & PolyORB.Types.Unsigned_Long'Image (Nb)));
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     pragma Debug (O ("Marshall (TypeCode) : about "
                                      & "to marshall a new  member"));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, I));
                     pragma Debug
                       (O ("Marshall (TypeCode) : marshalling "
                           & "the type ("
                           & TCKind'Image
                           (TypeCode.Kind
                            (PolyORB.Any.TypeCode.Member_Type (Data, I)))
                           & ")"));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Type (Data, I));
                     pragma Debug (O ("Marshall (TypeCode) : "
                                      & "member marshalled"));
                  end loop;
               end if;
            end;
            pragma Debug (O ("Marshall : all members marshalled"));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Union =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(16));
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
               Nb : PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall_From_Any
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Label (Data, I));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, I));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Type (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Enum =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(17));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_String =>
            pragma Debug (O ("marshall (typecode) : dealing with a string"));
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(18));
            pragma Debug (O ("marshall (typecode) : " &
                             "about to marshall length : " &
                             PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Length (Data))));
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
            pragma Debug (O ("marshall (typecode) : length marshalled"));
         when Tk_Sequence =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(19));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Array =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(20));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Alias =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(21));
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
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(22));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, I));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Type (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Longlong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(23));
         when Tk_Ulonglong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(24));
         when Tk_Longdouble =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(25));
         when Tk_Widechar =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(26));
         when Tk_Wstring =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(27));
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
         when Tk_Fixed =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(28));
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Scale (Data));
         when Tk_Value =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(29));
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
               Nb : PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, I));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Type (Data, I));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Visibility (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Valuebox =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(30));
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
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(31));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Abstract_Interface =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(32));
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
      Data   : in PolyORB.Any.NamedValue) is
   begin
      pragma Debug (O ("Marshall (NamedValue) : enter"));
      Marshall_From_Any (Buffer, Data.Argument);
      pragma Debug (O ("Marshall (NamedValue) : end"));
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

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Any) is
   begin
      Marshall_From_Any (Buffer, Data.all);
   end Marshall_From_Any;

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

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Long is
   begin
      pragma Debug (O ("Unmarshall (LongLong) : enter & end"));
      return To_Long_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long
   is
   begin
      pragma Debug (O ("Unmarshall (Long) : enter & end"));
      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Short
   is
   begin
      pragma Debug (O ("Unmarshall (Short) : enter & end"));
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Float
   is
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
      pragma Debug (O ("Unmarshall (Double) : enter & end"));
      return To_Double (Double_Buf (Octets));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Double is
      --  Octets : constant Stream_Element_Array :=
      --  Align_Unmarshall_Host_Endian_Copy (Buffer, 12, 8);
   begin
      --  raise Not_Implemented;
      --  pragma Debug (O ("Unmarshall (LongDouble) : enter & end"));
      --  return To_Long_Double (Long_Double_Buf (Octets));
      return PolyORB.Types.Long_Double (0);
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
      for I in Equiv'Range loop
         Equiv (I) := Character'Val
           (PolyORB.Types.Char'Pos
            (Unmarshall (Buffer)));
      end loop;

      if Character'Val (PolyORB.Types.Char'Pos (Unmarshall (Buffer)))
        /= ASCII.Nul
      then
         PolyORB.CORBA_P.Exceptions.Raise_Marshal;
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

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Wide_String
   is
      Length : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : Wide_String (1 .. Natural (Length));
   begin
      pragma Debug (O ("Unmarshall (Wide_String) : enter"));
      pragma Debug (O ("Unmarshall (Wide_String) : length is " &
                    PolyORB.Types.Unsigned_Long'Image (Length)));
      for I in Equiv'Range loop
         Equiv (I) := Wide_Character'Val (PolyORB.Types.Wchar'Pos
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

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out PolyORB.Any.Any)
   is
      Tc       : constant PolyORB.Any.TypeCode.Object
        := Get_Precise_Type (Result);
      Is_Empty : constant Boolean
        := PolyORB.Any.Is_Empty (Result);

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
               S : Short := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a short"));
               pragma Debug (O ("Unmarshall_To_Any : its value is "
                                & PolyORB.Types.Short'Image (S)));
               Set_Any_Value (Result, S);
            end;
         when Tk_Long =>
            declare
               L : Long := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a long"));
               Set_Any_Value (Result, L);
            end;
         when Tk_Ushort =>
            declare
               Us : Unsigned_Short := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Us);
            end;
         when Tk_Ulong =>
            declare
               Ul : Unsigned_Long := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with an Ulong"));
               Set_Any_Value (Result, Ul);
            end;
         when Tk_Float =>
            declare
               F : PolyORB.Types.Float := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, F);
            end;
         when Tk_Double =>
            declare
               D : Double := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, D);
            end;
         when Tk_Boolean =>
            declare
               B : PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, B);
            end;
         when Tk_Char =>
            declare
               C : Char := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, C);
            end;
         when Tk_Octet =>
            declare
               O : PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, O);
            end;
         when Tk_Any =>
            declare
               A : Any.Any := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, A);
            end;
         when Tk_TypeCode =>
            declare
               T : TypeCode.Object := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : "
                                & "dealing with a TypeCode"));
               Set_Any_Value (Result, T);
            end;
         when Tk_Principal =>
            --  FIXME : to be done
            null;
         when Tk_Objref =>
            --  declare
            --     O : PolyORB.Types.Object.Ref := Unmarshall (Buffer);
            --  begin
            --     PolyORB.Types.Object.Helper.Set_Any_Value (Result, O);
            --  end;
            null;
         when Tk_Struct =>
            declare
               Nb : Unsigned_Long :=
                 TypeCode.Member_Count (Tc);
               Arg : PolyORB.Any.Any;
            begin
               pragma Debug (O ("unmarshall_to_any : dealing with a struct"));
               PolyORB.Any.Set_Any_Aggregate_Value (Result);
               pragma Debug (O ("unmarshall_to_any : about to "
                                & "unmarshall parameters"));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     pragma Debug (O ("unmarshall_to_any : get the element"));
                     if Is_Empty then
                        Arg := Get_Empty_Any (TypeCode.Member_Type (Tc, I));
                     else
                        Arg := Get_Aggregate_Element
                          (Result,
                           TypeCode.Member_Type (Tc, I),
                           I);
                     end if;
                     pragma Debug (O ("unmarshall_to_any : about to "
                                      & "unmarshall a parameter"));
                     Unmarshall_To_Any (Buffer,
                                        Arg);
                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;
         when Tk_Union =>
            declare
               Nb : Unsigned_Long;
               Label, Arg : PolyORB.Any.Any;
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with an union"));
               Set_Any_Aggregate_Value (Result);
               if Is_Empty then
                  Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
               else
                  Label := Get_Aggregate_Element
                    (Result,
                     TypeCode.Discriminator_Type (Tc),
                     PolyORB.Types.Unsigned_Long (0));
               end if;
               Unmarshall_To_Any (Buffer, Label);
               if Is_Empty then
                  pragma Debug (O ("Unmarshall_To_Any : about to call "
                                   & "add_aggregate"));
                  Add_Aggregate_Element (Result, Label);
               end if;
               pragma Debug (O ("Unmarshall_To_Any : about to call "
                                & "member_count_with_label"));
               Nb := PolyORB.Any.TypeCode.Member_Count_With_Label (Tc, Label);
               if Nb > 0 then
                  for I in 0 .. Nb - 1 loop
                     if Is_Empty then
                        Arg := Get_Empty_Any
                          (TypeCode.Member_Type_With_Label (Tc, Label, I));
                     else
                        Arg := Get_Aggregate_Element
                          (Result,
                           TypeCode.Member_Type_With_Label (Tc, Label, I),
                           I + 1);
                     end if;
                     Unmarshall_To_Any (Buffer, Arg);
                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;
         when Tk_Enum =>
            declare
               Arg : PolyORB.Any.Any;
            begin
               Set_Any_Aggregate_Value (Result);
               if Is_Empty then
                  Arg := Get_Empty_Any (TC_Unsigned_Long);
               else
                  Arg := Get_Aggregate_Element
                    (Result,
                     TC_Unsigned_Long,
                     PolyORB.Types.Unsigned_Long (0));
               end if;
               Unmarshall_To_Any (Buffer, Arg);
               if Is_Empty then
                  Add_Aggregate_Element (Result, Arg);
               end if;
            end;
         when Tk_String =>
            declare
               S : PolyORB.Types.String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, S);
            end;
         when Tk_Sequence =>
            declare
               Nb : Unsigned_Long := Unmarshall (Buffer);
               Max_Nb : Unsigned_Long := TypeCode.Length (Tc);
               Arg : PolyORB.Any.Any;
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any : dealing with a sequence"));
               if Max_Nb > 0 and then Nb > Max_Nb then
                  PolyORB.CORBA_P.Exceptions.Raise_Marshal;
               end if;
               Set_Any_Aggregate_Value (Result);
               pragma Debug (O ("Unmarshall_To_Any : aggregate value set"));
               if Is_Empty then
                  pragma Debug (O ("Unmarshall_To_Any : about to call"
                                   & " add_aggregate_element"));
                  Add_Aggregate_Element (Result, To_Any (Nb));
               else
                  Arg := Get_Aggregate_Element
                    (Result,
                     TC_Unsigned_Long,
                     PolyORB.Types.Unsigned_Long (0));
                  Set_Any_Value (Arg, Nb);
               end if;
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     if Is_Empty then
                        Arg := Get_Empty_Any (TypeCode.Content_Type (Tc));
                     else
                        Arg := Get_Aggregate_Element
                          (Result, TypeCode.Content_Type (Tc), I + 1);
                     end if;
                     Unmarshall_To_Any (Buffer, Arg);
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
                     Unmarshall_To_Any (Buffer, Arg);
                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;
         when Tk_Alias =>
            --  we should never reach this point
            raise Program_Error;
         when Tk_Except =>
            declare
               Nb : Unsigned_Long :=
                 TypeCode.Member_Count (Tc);
               Arg : PolyORB.Any.Any;
            begin
               Set_Any_Aggregate_Value (Result);
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
                     Unmarshall_To_Any (Buffer,
                                        Arg);
                     if Is_Empty then
                        Add_Aggregate_Element (Result, Arg);
                     end if;
                  end loop;
               end if;
            end;
         when Tk_Longlong =>
            declare
               Ll : Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ll);
            end;
         when Tk_Ulonglong =>
            declare
               Ull : Unsigned_Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ull);
            end;
         when Tk_Longdouble =>
            declare
               Ld : Long_Double := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ld);
            end;
         when Tk_Widechar =>
            declare
               Wc : Wchar := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Wc);
            end;
         when Tk_Wstring =>
            declare
               Ws : PolyORB.Types.Wide_String := Unmarshall (Buffer);
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
            null;


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
            null;

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
            null;
         when Tk_Native =>
            --  FIXME : to be done
            null;
         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            null;
      end case;
      pragma Debug (O ("Unmarshall_To_Any : end"));
   end Unmarshall_To_Any;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.TypeCode.Object
   is
      Nb : PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
      Result : PolyORB.Any.TypeCode.Object;
   begin
      --  XXX The hardcoded values in this case should be replaced
      --  by symbolic constants.
      pragma Debug (O ("Unmarshall (TypeCode) : enter"));
      case Nb is
         when 0 =>
            Result := PolyORB.Any.TypeCode.TC_Null;
         when 1 =>
            Result := PolyORB.Any.TypeCode.TC_Void;
         when 2 =>
            Result := PolyORB.Any.TypeCode.TC_Short;
         when 3 =>
            Result := PolyORB.Any.TypeCode.TC_Long;
         when 4 =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Short;
         when 5 =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long;
         when 6 =>
            Result := PolyORB.Any.TypeCode.TC_Float;
         when 7 =>
            Result := PolyORB.Any.TypeCode.TC_Double;
         when 8 =>
            Result := PolyORB.Any.TypeCode.TC_Boolean;
         when 9 =>
            Result := PolyORB.Any.TypeCode.TC_Char;
         when 10 =>
            Result := PolyORB.Any.TypeCode.TC_Octet;
         when 11 =>
            Result := PolyORB.Any.TypeCode.TC_Any;
         when 12 =>
            pragma Debug (O ("Unmarshall (TypeCode) : "
                             & "dealing with a TypeCode"));
            Result := PolyORB.Any.TypeCode.TC_TypeCode;
         when 13 =>
            Result := PolyORB.Any.TypeCode.TC_Principal;
         when 14 =>
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
         when 15 =>
            Result := PolyORB.Any.TypeCode.TC_Struct;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               pragma Debug (O ("unmarshall (TypeCode) : dealing "
                                & "with a struct"));
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id   := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb   := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 16 =>
            Result := PolyORB.Any.TypeCode.TC_Union;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb, Default_Index : PolyORB.Types.Unsigned_Long;
               Discriminator_Type, Member_Type : PolyORB.Any.TypeCode.Object;
               Member_Label : PolyORB.Any.Any;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Discriminator_Type := Unmarshall (Complex_Buffer);
               Default_Index := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Discriminator_Type));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Default_Index));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Label := Get_Empty_Any (Discriminator_Type);
                     Unmarshall_To_Any (Complex_Buffer, Member_Label);
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, Member_Label);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 17 =>
            Result := PolyORB.Any.TypeCode.TC_Enum;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer :  Buffer_Access := null;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 18 =>
            Result := PolyORB.Any.TypeCode.TC_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 19 =>
            Result := PolyORB.Any.TypeCode.TC_Sequence;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               Length := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 20 =>
            Result := PolyORB.Any.TypeCode.TC_Array;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               Length := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 21 =>
            Result := PolyORB.Any.TypeCode.TC_Alias;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 22 =>
            Result := PolyORB.Any.TypeCode.TC_Except;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 23 =>
            Result := PolyORB.Any.TypeCode.TC_Long_Long;
         when 24 =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long_Long;
         when 25 =>
            Result := PolyORB.Any.TypeCode.TC_Long_Double;
         when 26 =>
            Result := PolyORB.Any.TypeCode.TC_Wchar;
         when 27 =>
            Result := PolyORB.Any.TypeCode.TC_Wide_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 28 =>
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
         when 29 =>
            Result := PolyORB.Any.TypeCode.TC_Value;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Type_Modifier := Unmarshall (Complex_Buffer);
               Concrete_Base_Type := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     Visibility := Unmarshall (Complex_Buffer);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Visibility));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 30 =>
            Result := PolyORB.Any.TypeCode.TC_Valuebox;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 31 =>
            Result := PolyORB.Any.TypeCode.TC_Native;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer :  Buffer_Access := null;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 32 =>
            Result := PolyORB.Any.TypeCode.TC_Abstract_Interface;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when others =>
            PolyORB.CORBA_P.Exceptions.Raise_Marshal;
      end case;
      pragma Debug (O ("Unmarshall (TypeCode) : end"));
      return Result;
   end Unmarshall;

   function  Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Any.NamedValue
   is
      NV  :  PolyORB.Any.NamedValue;
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

   --------------
   -- Marshall --
   --------------

--   procedure Marshall
--     (Buffer : access Buffer_Type;
--      Data : in PolyORB.Types.AbstractBase.Ref'Class) is
--   begin
      --  !!!!!!!!!!!!!!!!!
      --  FIXME: I've just noticed that abstract interfaces must be
      --  encoded as unions
      --  with a boolean discriminator, cf spec and change code below.
      --  !!!!!!!!!!!!!!!!!

      --  1. if Data is a valuetype, call the valuetype marshalling function
--    if Data in PolyORB.Types.Value.Base'Class then
--       PolyORB.CORBA_P.Value.Stream.Marshall (Buffer,
--                                    PolyORB.Types.Value.Base'Class (Data));

         --  2. check if Data is a nil ref, raise marshall if true
--    elsif PolyORB.Types.AbstractBase.Is_Nil (Data) then
--       PolyORB.CORBA_P.Exceptions.Raise_Marshal;

         --  3. If Data is an abstract interface and the referenced object is
         --     a valuetype, then call the valuetype marshalling function.
         --  In practice, just check if the referenced object is a valuetype.
--    elsif PolyORB.Types.AbstractBase.Object_Of (Data).all
--   in PolyORB.Types.Value.Impl_Base'Class then
         --  PolyORB.CORBA_P.Value.Stream.Marshall (Buffer,
         --                             Data);
--       null;
         --  Not implemented yet

      --  4. Call the interface marshalling function
--    else
         --  Make a redispatching call on the designated
         --  object.
--       declare
--            P : constant PolyORB.Types.Impl.Object_Ptr
--              := PolyORB.Types.AbstractBase.Object_Of (Data);
--         begin
--            PolyORB.Types.Impl.Marshall
--              (Buffer,
--               PolyORB.Types.Impl.Object'Class (P.all));
--         end;
--      end if;
--   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

--   procedure Unmarshall
--     (Buffer : access Buffer_Type;
--      Data : in out PolyORB.Types.AbstractBase.Ref'Class) is
--      Obj : constant PolyORB.CORBA_P.Object.Object_Ptr
--        := new PolyORB.CORBA_P.Object.Object_Type (Local_Object => False);
--   begin
--      PolyORB.CORBA_P.Object.Unmarshall
--        (Buffer, PolyORB.CORBA_P.Object.Object_Type (Obj.all));
--      PolyORB.Types.AbstractBase.Set
--        (Data, PolyORB.Types.Impl.Object_Ptr (Obj));
--   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

--   function Unmarshall
--     (Buffer : access Buffer_Type)
--     return PolyORB.Types.Object.Ref is
--      New_Ref : PolyORB.Types.Object.Ref;
--   begin
--      Unmarshall (Buffer, New_Ref);
--*     return New_Ref;
--   end Unmarshall;

   -----------------------
   -- System exceptions --
   -----------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Excpt  :        CORBA.Exception_Occurrence)
   is
      Members : CORBA.System_Exception_Members;
   begin
      PolyORB.CORBA_P.Exceptions.Get_Members (Excpt, Members);
      Marshall
        (Buffer, PolyORB.Types.String
         (PolyORB.CORBA_P.Exceptions.Occurrence_To_Name (Excpt)));
      Marshall (Buffer, Types.Unsigned_Long (Members.Minor));
      Marshall
        (Buffer, Types.Unsigned_Long
         (PolyORB.CORBA_P.Exceptions.To_Unsigned_Long
          (Members.Completed)));
   end Marshall;

   procedure Unmarshall_And_Raise
     (Buffer : access Buffer_Type)
   is
      use Ada.Exceptions;

      Minor      : PolyORB.Types.Unsigned_Long;
      Status     : PolyORB.Types.Unsigned_Long;
      Identity   : Exception_Id;
      Repository : PolyORB.Types.String;

   begin
      Repository := Unmarshall (Buffer);
      Identity := PolyORB.CORBA_P.Exceptions.Get_ExcepId_By_RepositoryId
        (PolyORB.Types.To_Standard_String (Repository));

      if Identity = Null_Id then
         --  If not found, this is a marshal error.
         Identity := CORBA.Marshal'Identity;
         Minor := 0;
         Status := CORBA.Completion_Status'Pos (CORBA.Completed_Maybe);
      end if;

      Minor  := Unmarshall (Buffer);
      Status := Unmarshall (Buffer);

      --  Raise the exception

      PolyORB.CORBA_P.Exceptions.Stack.Raise_Exception
        (Identity,
         CORBA.System_Exception_Members'
         (CORBA.Unsigned_Long (Minor),
          PolyORB.CORBA_P.Exceptions.To_Completion_Status
          (CORBA.Unsigned_Long (Status))));
   end Unmarshall_And_Raise;


   -------------------------------
   --  Marshall a sequence of octets
   ---------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Stream_Element_Array)
   is

   begin
      pragma Debug (O ("Marshall (Encapsulation) : enter"));
      Marshall (Buffer, PolyORB.Types.Unsigned_Long (Data'Length));
      for I in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Octet (Data (I)));
      end loop;
      pragma Debug (O ("Marshall (Encapsulation) : end"));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Stream_Element_Array)
   is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;



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
         for I in E'Range loop
            E (I) := Stream_Element
                     (PolyORB.Types.Octet'(Unmarshall (Buffer)));
         end loop;
         pragma Debug (O ("Unmarshall (Encapsulation): end"));
         return E;
      end;
   end Unmarshall;


--   procedure Marshall
--     (Buffer : access Buffer_Type;
--      Data   : in Objects.Object_Id)
--   is
--   begin
--      Marshall (Buffer, Stream_Element_Array(Data));
--   end Marshall;



   --------------------------------
   --  Marshalling of Objects Ids
   ---------------------------------

--   procedure Marshall
--     (Buffer : access Buffer_Type;
--      Data   : access Objects.Object_Id)
--   is
--   begin
--      Marshall (Buffer, Data.all);
--   end Marshall;


--   function Unmarshall
--     (Buffer : access Buffer_Type)
--     return Objects.Object_Id
--   is
--     Octets : Stream_Element_Array;
--   begin
--      return  Objects.Object_Id(Octets);
--   end Unmarshall;

--   function Unmarshall
--     (Buffer : access Buffer_Type)
--     return Objects.Object_Id_Access
--   is
--     Obj : aliased Objects.Object_Id;
--   begin
--     Obj := Unmarshall(Buffer);
--     return Obj'Access;
--   end Unmarshall;


   -----------------
   -- Fixed_Point --
   -----------------

   package body Fixed_Point is

      Fixed_Positive_Zero : constant PolyORB.Types.Octet
        := 16#C#;
      Fixed_Negative : constant PolyORB.Types.Octet
        := 16#D#;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F) is
      begin
         Marshall (Buffer, Data.all);
      end Marshall;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   :        F)
      is
         N_Digits : Integer
           := 0;

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

            for I in Offset .. Offset + N_Digits loop
               declare
                  Digit : constant PolyORB.Types.Octet
                    := PolyORB.Types.Octet (Val / Bias);
               begin
                  if I mod 2 = 0 then
                     Octets (Stream_Element_Offset (I / 2))
                       := Stream_Element (Digit * 16);
                  else
                     Octets (Stream_Element_Offset (I / 2))
                       := Octets (Stream_Element_Offset (I / 2)) +
                       Stream_Element (Digit);
                  end if;
                  Val := Val - F (Digit) * Bias;
                  Bias := 0.1 * Bias;
               end;
            end loop;
            if Data >= 0.0 then
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Stream_Element (Fixed_Positive_Zero);
            else
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Stream_Element (Fixed_Negative);
            end if;
            Align_Marshall_Copy (Buffer, Octets, 1);
         end;
      end Marshall;

      function Unmarshall
        (Buffer : access Buffer_Type)
         return F
      is
         O : PolyORB.Types.Octet;
         Result : F := 0.0;
      begin
         loop
            O := Unmarshall (Buffer);
            if O / 16 > 9
              or else
              (O mod 16 > 9
               and then O mod 16 /= PolyORB.Types.Octet (Fixed_Positive_Zero)
               and then O mod 16 /= PolyORB.Types.Octet (Fixed_Negative))
            then
               PolyORB.CORBA_P.Exceptions.Raise_Marshal;
            end if;

            Result := Result * 10 + F (O / 16) * F'Delta;
            if O mod 16 < 10 then
               Result := Result * 10 + F (O mod 16) * F'Delta;
            else
               if O mod 16 = PolyORB.Types.Octet (Fixed_Negative) then
                  Result := -Result;
               end if;
               exit;
            end if;
         end loop;

         return Result;
      end Unmarshall;

   end Fixed_Point;

end PolyORB.Representations.CDR;
