------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                          C O R B A . C D R                               --
--                                                                          --
--                              B o d y                                     --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Exceptions;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Opaque;  use Droopi.Opaque;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with CORBA;
with Droopi.CORBA_P.Exceptions;
with Droopi.CORBA_P.Exceptions.Stack;

package body Droopi.Representations.CDR is

   use CORBA;

   use Droopi.Log;
   use Droopi.CORBA_P.Exceptions;
   use Droopi.CORBA_P.Exceptions.Stack;

   package L is new Droopi.Log.Facility_Log ("droopi.representations.cdr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Octet_Order : constant Integer := 256;

   -------------------------
   -- Start_Encapsulation --
   -------------------------

   procedure Start_Encapsulation
     (Buffer : access Buffer_Type) is
   begin
      Set_Initial_Position (Buffer, 0);
      Marshall
        (Buffer,
         CORBA.Boolean
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
      if CORBA.Boolean'Val
        (CORBA.Octet (Octets (Octets'First))) then
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
      return Encapsulation (To_Stream_Element_Array (Buffer));
   end Encapsulate;

   -------------------------
   -- Utility subprograms --
   -------------------------

   function Rev
     (Octets : Stream_Element_Array)
     return Stream_Element_Array;
   --  Reverse the order of an array of octets

   --  function Align_Octet
   --    (Data:in F;
   --     Order: Integer) return Stream_Element_Array;
   --  Align Octets on Array of octets

   function Sum_Octet
     (Octets : Stream_Element_Array;
      Order :  Integer) return CORBA.Unsigned_Long;

   function Sum_Octet
     (Octets  : Stream_Element_Array;
      Order   : Integer) return CORBA.Unsigned_Long_Long;


   --  Return the Ulong stored in a array of octets


   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in big-endian
   --  byte order.


   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in big-endian byte order.

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in the
   --  host's byte order.

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in the host's byte order

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : in Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy
   --  of Octets into Buffer, as is.

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy
   --  of Size octets from Buffer's data, as is.

   ------------------------------------------
   -- Conversions between CORBA signed and --
   -- unsigned integer types.              --
   ------------------------------------------

   function To_Long_Long is
      new Ada.Unchecked_Conversion
        (CORBA.Unsigned_Long_Long, CORBA.Long_Long);
   function To_Unsigned_Long_Long is
      new Ada.Unchecked_Conversion
        (CORBA.Long_Long, CORBA.Unsigned_Long_Long);
   function To_Long is
      new Ada.Unchecked_Conversion
        (CORBA.Unsigned_Long, CORBA.Long);
   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
        (CORBA.Long, CORBA.Unsigned_Long);
   function To_Short is
      new Ada.Unchecked_Conversion
        (CORBA.Unsigned_Short, CORBA.Short);
   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion
        (CORBA.Short, CORBA.Unsigned_Short);

   -------------------------------------------
   --  Conversions for floating point types --
   -------------------------------------------

   subtype Double_Buf is Stream_Element_Array (1 .. 8);
   --  FIXME LONG DOUBLE
   subtype Long_Double_Buf is  Stream_Element_Array (1 .. 12);

   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
        (CORBA.Float, CORBA.Unsigned_Long);
   function To_Float is
      new Ada.Unchecked_Conversion
        (CORBA.Unsigned_Long, CORBA.Float);
   function To_Double_Buf is
      new Ada.Unchecked_Conversion
        (CORBA.Double, Double_Buf);
   function To_Double is
      new Ada.Unchecked_Conversion
        (Double_Buf, CORBA.Double);
   --   function To_Long_Double_Buf is
   --      new Ada.Unchecked_Conversion
   --       (CORBA.Long_Double, Long_Double_Buf);
   --   function To_Long_Double is
   --      new Ada.Unchecked_Conversion
   --       (Long_Double_Buf, CORBA.Long_Double);


   ----------------------------------
   -- Marshall-by-copy subprograms --
   -- for all elementary types     --
   ----------------------------------

   --  Marshalling of a Boolean
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Boolean) is
   begin
      pragma Debug (O ("Marshall (Boolean) : enter"));
      Marshall (Buffer, CORBA.Octet'(CORBA.Boolean'Pos (Data)));
      pragma Debug (O ("Marshall (Boolean) : end"));
   end Marshall;

   --  Marshalling of a character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Char) is
   begin
      pragma Debug (O ("Marshall (Char) : enter"));
      Marshall (Buffer, CORBA.Octet'(CORBA.Char'Pos (Data)));
      pragma Debug (O ("Marshall (Char) : end"));
   end Marshall;

   --  Marshalling of a wide character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Wchar)
   is

   begin
      pragma Debug (O ("Marshall (WChar) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'(Stream_Element (CORBA.Wchar'Pos (Data) / 256),
         Stream_Element (CORBA.Wchar'Pos (Data) mod 256)), 2);
      pragma Debug (O ("Marshall (WChar) : end"));
   end Marshall;

   --  Marshalling of a Octet
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Octet) is
   begin
      pragma Debug (O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (CORBA.Octet'(Data))), 1);
      pragma Debug (O ("Marshall (Octet) : end"));
   end Marshall;

   --  Marshalling of an unsigned short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Short)
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
      Data   : in CORBA.Unsigned_Long)
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
      Data   : in CORBA.Unsigned_Long_Long)
   is

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
      Data   : in CORBA.Long_Long) is
   begin
      pragma Debug (O ("Marshall (LongLong) : enter"));
      Marshall (Buffer, To_Unsigned_Long_Long (Data));
      pragma Debug (O ("Marshall (LongLong) : end"));
   end Marshall;

   --  Marshalling of a long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long) is
   begin
      pragma Debug (O ("Marshall (Long) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Long) : end"));
   end Marshall;

   --  Marshalling of a short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Short) is
   begin
      pragma Debug (O ("Marshall (Short) : enter"));
      Marshall (Buffer, To_Unsigned_Short (Data));
      pragma Debug (O ("Marshall (Short) : end"));
   end Marshall;

   --  Marshalling of a float
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Float) is
   begin
      pragma Debug (O ("Marshall (Float) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Float) : end"));
   end Marshall;

   --  Marshalling of a double
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Double)
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
       Data   : in CORBA.Long_Double)
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
      Data   : in Standard.String) is
   begin
      pragma Debug (O ("Marshall (String) : enter"));

      Marshall (Buffer, CORBA.Unsigned_Long'(Data'Length + 1));
      for I in Data'Range loop
         Marshall (Buffer, CORBA.Char (Data (I)));
      end loop;
      Marshall (Buffer, CORBA.Char (ASCII.Nul));

      pragma Debug (O ("Marshall (String) : end"));
   end Marshall;

   --  Marshalling of a Corba String
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.String) is
   begin
      pragma Debug (O ("Marshall (CORBA.String) : enter"));
      Marshall (Buffer, CORBA.To_Standard_String (Data));
      pragma Debug (O ("Marshall (CORBA.String) : end"));
   end Marshall;

   --  Marshall for CORBA.Wide_String could also
   --  be implemented as a call to a Marshall for
   --  Standard.Wide_String, just as CORBA.String/Standard.String.

   --  Marshalling of a wide string
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Wide_String)
   is
      Equiv : constant Wide_String
        := CORBA.To_Wide_String (Data) & Standard.Wide_Character'Val (0);

      --  XXXXX: Val (0) is suspicious ...
   begin
      pragma Debug (O ("Marshall (CORBA.Wide_String) : enter"));
      pragma Debug (O ("Marshall (CORBA.Wide_String) : length is " &
                       CORBA.Unsigned_Long'Image (Equiv'Length)));

      Marshall (Buffer, CORBA.Unsigned_Long'(Equiv'Length));
      for I in Equiv'Range loop
         Marshall (Buffer, CORBA.Wchar'Val (Wide_Character'Pos (Equiv (I))));
      end loop;

      pragma Debug (O ("Marshall (CORBA.Wide_String) : end"));
   end Marshall;

   --  Marshalling of a Corba Identifier
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Identifier) is
   begin
      pragma Debug (O ("Marshall (Identifier) : enter"));
      Marshall (Buffer, CORBA.String (Data));
      pragma Debug (O ("Marshall (Identifier) : end"));
   end Marshall;

   --  Marshalling of a Corba Scoped Name
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.ScopedName) is
   begin
      pragma Debug (O ("Marshall (ScopedName) : enter"));
      Marshall (Buffer, CORBA.String (Data));
      pragma Debug (O ("Marshall (ScopedName) : end"));
   end Marshall;

   --  Marshalling of a Corba Repository Identifier
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.RepositoryId) is
   begin
      pragma Debug (O ("Marshall (RepositoryId) : enter"));
      Marshall (Buffer, CORBA.String (Data));
      pragma Debug (O ("Marshall (RepositoryId) : end"));
   end Marshall;

   --  Marshalling of a Corba Value Modifier type (short)
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.ValueModifier) is
   begin
      pragma Debug (O ("Marshall (ValueModifier) : enter"));
      Marshall (Buffer, CORBA.Short (Data));
      pragma Debug (O ("Marshall (ValueModifier) : end"));
   end Marshall;

   --  Marshalling of a Corba Visibility Type (short)
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Visibility) is
   begin
      pragma Debug (O ("Marshall (Visibility) : enter"));
      Marshall (Buffer, CORBA.Short (Data));
      pragma Debug (O ("Marshall (Visibility) : end"));
   end Marshall;


   --  Marshalling of Corba Any Type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Any) is
   begin
      pragma Debug (O ("Marshall (Any) : enter"));
      Marshall (Buffer, Get_Type (Data));
      pragma Debug (O ("Marshall (Any) : type marshalled"));
      Marshall_From_Any (Buffer, Data);
      pragma Debug (O ("Marshall (Any) : end"));
   end Marshall;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Any)
   is
      Data_Type : CORBA.TypeCode.Object := CORBA.Get_Precise_Type (Data);
   begin
      pragma Debug (O ("Marshall_From_Any : enter"));
      --  pragma Debug
      --  (0 (Debug_Any(CORBA.TypeCode.Kind (Data_Type)'Pos)))

      case CORBA.TypeCode.Kind (Data_Type) is

         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Marshall_From_Any : dealing with void or null"));
            null;

         when Tk_Short =>
            pragma Debug (O ("Marshall_From_Any : dealing with a short"));
            Marshall (Buffer, CORBA.Short'(From_Any (Data)));


         when Tk_Long =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long"));
            Marshall (Buffer, CORBA.Long'(From_Any (Data)));

         when Tk_Ushort =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ushort"));
            Marshall (Buffer, CORBA.Unsigned_Short'(From_Any (Data)));

         when Tk_Ulong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ulong"));
            Marshall (Buffer, CORBA.Unsigned_Long'(From_Any (Data)));

         when Tk_Float =>
            pragma Debug (O ("Marshall_From_Any : dealing with a float"));
            Marshall (Buffer, CORBA.Float'(From_Any (Data)));

         when Tk_Double =>
            pragma Debug (O ("Marshall_From_Any : dealing with a double"));
            Marshall (Buffer, CORBA.Double'(From_Any (Data)));

         when Tk_Boolean =>
            pragma Debug (O ("Marshall_From_Any : dealing with a boolean"));
            Marshall (Buffer, CORBA.Boolean'(From_Any (Data)));

         when Tk_Char =>
            pragma Debug (O ("Marshall_From_Any : dealing with a char"));
            Marshall (Buffer, CORBA.Char'(From_Any (Data)));

         when Tk_Octet =>
            pragma Debug (O ("Marshall_From_Any : dealing with an octet"));
            Marshall (Buffer, CORBA.Octet'(From_Any (Data)));

         when Tk_Any =>
            pragma Debug (O ("Marshall_From_Any : dealing with an any"));
            Marshall (Buffer, CORBA.Any'(From_Any (Data)));

         when Tk_TypeCode =>
            pragma Debug (O ("Marshall_From_Any : dealing with a typecode"));
            Marshall (Buffer, CORBA.TypeCode.Object'(From_Any (Data)));

         when Tk_Principal =>
            --  FIXME : to be done
            pragma Debug (O ("Marshall_From_Any : dealing with a principal"));
            null;

         when Tk_Objref =>
            pragma Debug (O ("Marshall_From_Any : dealing with an objRef"));
            --  Marshall (Buffer, CORBA.Object.Helper.From_Any (Data));
            null;


         when Tk_Struct =>
            declare
                  Nb : constant CORBA.Unsigned_Long
                        := CORBA.Get_Aggregate_Count (Data);
                  Value : CORBA.Any;
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with a struct"));
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                     (Data, CORBA.TypeCode.Member_Type (Data_Type, I), I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;

         when Tk_Union =>
            declare
               Nb : CORBA.Unsigned_Long;
               Value, Label_Value : CORBA.Any;
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with an union"));
               Label_Value := Get_Aggregate_Element
                 (Data,
                  CORBA.TypeCode.Discriminator_Type (Data_Type),
                  CORBA.Unsigned_Long (0));
               pragma Debug (O ("Marshall_From_Any : got the label"));
               Marshall_From_Any (Buffer, Label_Value);
               pragma Debug (O ("Marshall_From_Any : label marshalled"));
               Nb := CORBA.Get_Aggregate_Count (Data);
               pragma Debug (O ("Marshall_From_Any : aggregate count = "
                                & CORBA.Unsigned_Long'Image (Nb)));
               if Nb > 1 then
                  for I in 1 .. Nb - 1 loop
                     pragma Debug (O ("Marshall_From_Any : inside loop, I = "
                                      & Unsigned_Long'Image (I)));
                     Value := CORBA.Get_Aggregate_Element
                       (Data,
                        CORBA.TypeCode.Member_Type_With_Label
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
               CORBA.Get_Aggregate_Element
               (Data,
                CORBA.TypeCode.TC_Unsigned_Long,
                CORBA.Unsigned_Long (0)));

         when Tk_String =>
            pragma Debug (O ("Marshall_From_Any : dealing with a string"));
            Marshall (Buffer, CORBA.String'(From_Any (Data)));

         when Tk_Sequence =>
            declare
               Nb : constant CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               pragma Debug (O
                  ("Marshall_From_Any : dealing with a sequence"));
               Value := CORBA.Get_Aggregate_Element
                 (Data,
                  CORBA.TypeCode.TC_Unsigned_Long,
                  CORBA.Unsigned_Long (0));
               Marshall_From_Any (Buffer, Value);
               if Nb /= 0 then
                  for I in 1 .. Nb - 1 loop
                     Value := CORBA.Get_Aggregate_Element
                       (Data,
                        CORBA.TypeCode.Content_Type (Data_Type),
                        I);
                     Marshall_From_Any (Buffer, Value);
                  end loop;
               end if;
            end;

         when Tk_Array =>

            declare
               Nb : constant CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
               Content_True_Type : CORBA.TypeCode.Object :=
                 CORBA.TypeCode.Content_Type (Data_Type);
            begin
               pragma Debug (O ("Marshall_From_Any : dealing with an array"));
               while CORBA.TypeCode.Kind (Content_True_Type) = Tk_Array loop
                  Content_True_Type :=
                    CORBA.TypeCode.Content_Type (Content_True_Type);
               end loop;
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     Content_True_Type,
                     I);
                  pragma Debug (O ("Marshall_From_Any : value kind is "
                                   & CORBA.TCKind'Image
                                   (CORBA.TypeCode.Kind
                                    (CORBA.Get_Type (Value)))));
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
               Nb : constant CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               pragma Debug
                 (O ("Marshall_From_Any : dealing with an exception"));
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Member_Type (Data_Type, I),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;

         when Tk_Longlong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long long"));
            Marshall (Buffer, CORBA.Long_Long'(From_Any (Data)));

         when Tk_Ulonglong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a ULongLong"));
            Marshall (Buffer, CORBA.Unsigned_Long_Long'(From_Any (Data)));

         when Tk_Longdouble =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a long double"));
            Marshall (Buffer, CORBA.Long_Double'(From_Any (Data)));

         when Tk_Widechar =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Wchar"));
            Marshall (Buffer, CORBA.Wchar'(From_Any (Data)));

         when Tk_Wstring =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a wide string"));
            Marshall (Buffer, CORBA.Wide_String'(From_Any (Data)));

         when Tk_Fixed =>
            --  declare
            --   Digit,Scale: CORBA.Any;
            --  begin
            --   pragma Debug (O ("Marshall_From_Any : dealing with a fixed"));
            --   Digit:=Get_Aggregate_Element
            --           (Data,
            --            CORBA.TypeCode.TC_Unsigned_Long,
            --            CORBA.TypeCode.Fixed_Digits(Data_Type),
            --            CORBA.Unsigned_Long(0));
            --   Marshall_From_Any(Buffer,Digit);
            --   Scale:=Get_Aggregate_Element
            --           (Data,
            --            CORBA.TypeCode.Fixed_Scale(Data_Type),
            --            CORBA.Unsigned_Long(1));
            --   Marshall_From_Any(Buffer,Scale);
            --   end;
            null;

         when Tk_Value =>
            --  declare
            --     Nb: CORBA.Unsigned_Long;
            --     Value_Modifier, Value_TypeCode,
            --         Value_Visibility : CORBA.Any;
            --  begin
            --    pragma Debug
            --        (O ("Marshall_From_Any : dealing with a value"));
            --    Value_Modifier:= CORBA.Get_Aggregate_Element
            --         (Data,
            --          CORBA.TypeCode.Type_Modifier(Data_Type),
            --          CORBA.Unsigned_Long(0));
            --  pragma Debug (0 ("Marshall_From_Any: got the value_modifier"));
            --    Marshall_From_Any(Buffer,Val_Modifier);
            --    Nb := CORBA.Get_Aggregate_Count(Data);
            --    if Nb>1 then
            --     while I<Nb-1 loop

            --       Value_Value:= CORBA.Get_Aggregate_Element
            --         (Data,
            --          CORBA.TypeCode.Member_Type (Data_Type, I),
            --          I);
            --       I:=I+1;
            --       Value_Visibility:=  CORBA.Get_Aggregate_Element
            --         (Data,
            --          CORBA.TypeCode.Member_Visibility(Data_Type, I),
            --          I);
            --       Marshall_From_Any(Buffer, Value);
            --       I:=I+2;

            --     end loop;
            --    end if;
            --   end;
            null;


         when Tk_Valuebox =>
            pragma Debug (O ("Marshall_From_Any : dealing with a valuebox"));
            Marshall_From_Any (Buffer, CORBA.Get_Aggregate_Element
                 (Data, CORBA.TypeCode.Member_Type (Data_Type,
                 CORBA.Unsigned_Long (0)),
                 CORBA.Unsigned_Long (0)));

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
      Data   : in CORBA.TypeCode.Object) is
      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (O ("Marshall (Typecode) : enter"));
      pragma Debug (O ("Marshall (Typecode) : kind is " &
                       TCKind'Image (CORBA.TypeCode.Kind (Data))));
      case CORBA.TypeCode.Kind (Data) is
         when Tk_Null =>
            Marshall (Buffer, CORBA.Unsigned_Long'(0));
         when Tk_Void =>
            Marshall (Buffer, CORBA.Unsigned_Long'(1));
         when Tk_Short =>
            Marshall (Buffer, CORBA.Unsigned_Long'(2));
         when Tk_Long =>
            Marshall (Buffer, CORBA.Unsigned_Long'(3));
         when Tk_Ushort =>
            Marshall (Buffer, CORBA.Unsigned_Long'(4));
         when Tk_Ulong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(5));
         when Tk_Float =>
            Marshall (Buffer, CORBA.Unsigned_Long'(6));
         when Tk_Double =>
            Marshall (Buffer, CORBA.Unsigned_Long'(7));
         when Tk_Boolean =>
            Marshall (Buffer, CORBA.Unsigned_Long'(8));
         when Tk_Char =>
            Marshall (Buffer, CORBA.Unsigned_Long'(9));
         when Tk_Octet =>
            Marshall (Buffer, CORBA.Unsigned_Long'(10));
         when Tk_Any =>
            Marshall (Buffer, CORBA.Unsigned_Long'(11));
         when Tk_TypeCode =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a TypeCode"));
            Marshall (Buffer, CORBA.Unsigned_Long'(12));
         when Tk_Principal =>
            Marshall (Buffer, CORBA.Unsigned_Long'(13));
         when Tk_Objref =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with an ObjRef"));
            Marshall (Buffer, CORBA.Unsigned_Long'(14));
            pragma Debug (O ("Marshall (TypeCode) : it has "
                             & CORBA.Unsigned_Long'Image
                             (CORBA.TypeCode.Parameter_Count (Data))
                             & " parameters"));
            Marshall (Buffer, CORBA.TypeCode.Id (Data));
            Marshall (Buffer, CORBA.TypeCode.Name (Data));
         when Tk_Struct =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a struct"));
            Marshall (Buffer, CORBA.Unsigned_Long'(15));
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (O ("Marshall (TypeCode) : marshalling the id"));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            pragma Debug (O ("Marshall (TypeCode) : marshalling the name"));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Member_Count (Data);
            begin
               pragma Debug (O ("Marshall (TypeCode) : " &
                                "marshalling the members. Nb = "
                                & CORBA.Unsigned_Long'Image (Nb)));
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     pragma Debug (O ("Marshall (TypeCode) : about "
                                      & "to marshall a new  member"));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Name (Data, I));
                     pragma Debug (O ("Marshall (TypeCode) : marshalling "
                                      & "the type ("
                                      & TCKind'Image
                                      (TypeCode.Kind
                                       (CORBA.TypeCode.Member_Type (Data, I)))
                                      & ")"));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Type (Data, I));
                     pragma Debug (O ("Marshall (TypeCode) : "
                                      & "member marshalled"));
                  end loop;
               end if;
            end;
            pragma Debug (O ("Marshall : all members marshalled"));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Union =>
            Marshall (Buffer, CORBA.Unsigned_Long'(16));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Discriminator_Type (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Default_Index (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall_From_Any (Complex_Buffer,
                                        CORBA.TypeCode.Member_Label (Data, I));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Name (Data, I));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Type (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Enum =>
            Marshall (Buffer, CORBA.Unsigned_Long'(17));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Name (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_String =>
            pragma Debug (O ("marshall (typecode) : dealing with a string"));
            Marshall (Buffer, CORBA.Unsigned_Long'(18));
            pragma Debug (O ("marshall (typecode) : " &
                             "about to marshall length : " &
                             CORBA.Unsigned_Long'Image
                             (CORBA.TypeCode.Length (Data))));
            Marshall (Buffer, CORBA.TypeCode.Length (Data));
            pragma Debug (O ("marshall (typecode) : length marshalled"));
         when Tk_Sequence =>
            Marshall (Buffer, CORBA.Unsigned_Long'(19));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Array =>
            Marshall (Buffer, CORBA.Unsigned_Long'(20));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Alias =>
            Marshall (Buffer, CORBA.Unsigned_Long'(21));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Except =>
            Marshall (Buffer, CORBA.Unsigned_Long'(22));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Name (Data, I));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Type (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Longlong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(23));
         when Tk_Ulonglong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(24));
         when Tk_Longdouble =>
            Marshall (Buffer, CORBA.Unsigned_Long'(25));
         when Tk_Widechar =>
            Marshall (Buffer, CORBA.Unsigned_Long'(26));
         when Tk_Wstring =>
            Marshall (Buffer, CORBA.Unsigned_Long'(27));
            Marshall (Buffer, CORBA.TypeCode.Length (Data));
         when Tk_Fixed =>
            Marshall (Buffer, CORBA.Unsigned_Long'(28));
            Marshall (Buffer, CORBA.TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, CORBA.TypeCode.Fixed_Scale (Data));
         when Tk_Value =>
            Marshall (Buffer, CORBA.Unsigned_Long'(29));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Type_Modifier (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Concrete_Base_Type (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Name (Data, I));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Type (Data, I));
                     Marshall (Complex_Buffer,
                               CORBA.TypeCode.Member_Visibility (Data, I));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Valuebox =>
            Marshall (Buffer, CORBA.Unsigned_Long'(30));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Native =>
            Marshall (Buffer, CORBA.Unsigned_Long'(31));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
         when Tk_Abstract_Interface =>
            Marshall (Buffer, CORBA.Unsigned_Long'(32));
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      CORBA.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
      end case;
      pragma Debug (O ("Marshall (Typecode) : end"));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.NamedValue) is
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
   --     Marshall (Buffer, CORBA.Unsigned_Long (Data'Length));
   --     for I in Data'Range loop
   --        Marshall (Buffer, CORBA.Octet (Data (I)));
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
      Data   : access CORBA.Octet) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Char) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Wchar) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Boolean) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Long_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Float) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Double) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long_Double) is
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
      Data   : access CORBA.String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Wide_String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Identifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.ScopedName) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.RepositoryId) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.ValueModifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Visibility) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall_From_Any;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.TypeCode.Object) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.NamedValue) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   --  procedure Marshall
   --   (Buffer : access Buffer_Type;
   --    Data   : access Encapsulation) is
   --  begin
   --    Marshall (Buffer, CORBA.Unsigned_Long (Data'Length));
   --    Insert_Raw_Data (Buffer, Data'Length,
   --    Opaque_Pointer'(Zone => Zone_Access(Data), Offset => 0));
   --  end Marshall;

   ------------------------------------
   -- Unmarshall-by-copy subprograms --
   ------------------------------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Boolean is
   begin
      pragma Debug (O ("Unmarshall (Boolean) : enter & end"));
      return CORBA.Boolean'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Char is
   begin
      pragma Debug (O ("Unmarshall (Char) : enter & end"));
      return CORBA.Char'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Wchar is
      Octets : constant Stream_Element_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      pragma Debug (O ("Unmarshall (WChar) : enter & end"));
      return CORBA.Wchar'Val
        (CORBA.Unsigned_Long (Octets (Octets'First)) * 256 +
         CORBA.Unsigned_Long (Octets (Octets'First + 1)));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Octet is
      Result : constant Stream_Element_Array
        := Align_Unmarshall_Copy (Buffer, 1, 1);
   begin
      pragma Debug (O ("Unmarshall (Octet) : enter & end"));
      return CORBA.Octet (Result (Result'First));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Short
   is
      Octets : constant Stream_Element_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      pragma Debug (O ("Unmarshall (UShort) : enter & end"));
      return CORBA.Unsigned_Short (Octets (Octets'First)) * 256 +
        CORBA.Unsigned_Short (Octets (Octets'First + 1));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long
   is
      Octets : constant Stream_Element_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 4, 4);
   begin
      pragma Debug (O ("Unmarshall (ULong) : enter & end"));
      return Sum_Octet (Octets, 4);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long_Long is
      Octets : constant Stream_Element_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 8, 8);
   begin
      pragma Debug (O ("Unmarshall (ULongLong) : enter & end"));
      return Sum_Octet (Octets, 8);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Long is
   begin
      pragma Debug (O ("Unmarshall (LongLong) : enter & end"));
      return To_Long_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long
   is
   begin
      pragma Debug (O ("Unmarshall (Long) : enter & end"));
      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Short
   is
   begin
      pragma Debug (O ("Unmarshall (Short) : enter & end"));
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Float
   is
   begin
      pragma Debug (O ("Unmarshall (Float) : enter & end"));
      return To_Float (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Double is
      Octets : constant Stream_Element_Array :=
        Align_Unmarshall_Host_Endian_Copy (Buffer, 8, 8);
   begin
      pragma Debug (O ("Unmarshall (Double) : enter & end"));
      return To_Double (Double_Buf (Octets));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Double is
      --  Octets : constant Stream_Element_Array :=
      --  Align_Unmarshall_Host_Endian_Copy (Buffer, 12, 8);
   begin
      --  raise Not_Implemented;
      --  pragma Debug (O ("Unmarshall (LongDouble) : enter & end"));
      --  return To_Long_Double (Long_Double_Buf (Octets));
      return CORBA.Long_Double (0);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (O ("Unmarshall (String): enter"));
      pragma Debug (O ("Unmarshall (String): length is " &
                    CORBA.Unsigned_Long'Image (Length)));
      for I in Equiv'Range loop
         Equiv (I) := Character'Val (CORBA.Char'Pos
                                     (Unmarshall (Buffer)));
      end loop;

      if Character'Val (CORBA.Char'Pos (Unmarshall (Buffer)))
        /= ASCII.Nul then
         Droopi.CORBA_P.Exceptions.Raise_Marshal;
      end if;

      pragma Debug (O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return CORBA.String is
   begin
      return CORBA.To_CORBA_String (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Wide_String
   is
      Length : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : Wide_String (1 .. Natural (Length));
   begin
      pragma Debug (O ("Unmarshall (Wide_String) : enter"));
      pragma Debug (O ("Unmarshall (Wide_String) : length is " &
                    CORBA.Unsigned_Long'Image (Length)));
      for I in Equiv'Range loop
         Equiv (I) := Wide_Character'Val (CORBA.Wchar'Pos
                                          (Unmarshall (Buffer)));
      end loop;
      pragma Debug (O ("Unmarshall (Wide_String) : end"));
      return CORBA.To_CORBA_Wide_String
        (Equiv (1 .. Equiv'Length - 1));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Identifier is
      Result : CORBA.String := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Identifier) : enter & end"));
      return CORBA.Identifier (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.ScopedName is
      Result : CORBA.String := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (ScopedName) : enter & end"));
      return CORBA.ScopedName (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.RepositoryId is
      Result : CORBA.String := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (RepositoryId) : enter & end"));
      return CORBA.RepositoryId (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.ValueModifier is
      Result : CORBA.Short := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (ValueModifier) : enter & end"));
      return CORBA.ValueModifier (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Visibility is
      Result : CORBA.Short := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Visibility) : enter & end"));
      return CORBA.Visibility (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Any
   is
      Result : CORBA.Any;
      Tc : CORBA.TypeCode.Object := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Any) : enter"));
      Result := Get_Empty_Any (Tc);
      Unmarshall_To_Any (Buffer, Result);
      pragma Debug (O ("Unmarshall (Any) : end"));
      return Result;
   end Unmarshall;

   procedure Unmarshall_To_Any (Buffer : access Buffer_Type;
                                Result : in out CORBA.Any)
   is
      Tc : CORBA.TypeCode.Object := Get_Precise_Type (Result);
      Is_Empty : Boolean := CORBA.Is_Empty (Result);
      use CORBA;
   begin
      pragma Debug (O ("Unmarshall_To_Any : enter"));
      pragma Debug (O ("Unmarshall_To_Any : Any_Type is " &
                       CORBA.TCKind'Image (TypeCode.Kind (Tc))));
      case TypeCode.Kind (Tc) is
         when Tk_Null | Tk_Void =>
            null;
         when Tk_Short =>
            declare
               S : Short := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a short"));
               pragma Debug (O ("Unmarshall_To_Any : its value is "
                                & CORBA.Short'Image (S)));
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
               F : CORBA.Float := Unmarshall (Buffer);
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
               B : CORBA.Boolean := Unmarshall (Buffer);
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
               O : CORBA.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, O);
            end;
         when Tk_Any =>
            declare
               A : Any := Unmarshall (Buffer);
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
            --     O : CORBA.Object.Ref := Unmarshall (Buffer);
            --  begin
            --     CORBA.Object.Helper.Set_Any_Value (Result, O);
            --  end;
            null;
         when Tk_Struct =>
            declare
               Nb : Unsigned_Long :=
                 TypeCode.Member_Count (Tc);
               Arg : CORBA.Any;
            begin
               pragma Debug (O ("unmarshall_to_any : dealing with a struct"));
               CORBA.Set_Any_Aggregate_Value (Result);
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
               Label, Arg : CORBA.Any;
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with an union"));
               Set_Any_Aggregate_Value (Result);
               if Is_Empty then
                  Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
               else
                  Label := Get_Aggregate_Element
                    (Result,
                     TypeCode.Discriminator_Type (Tc),
                     CORBA.Unsigned_Long (0));
               end if;
               Unmarshall_To_Any (Buffer, Label);
               if Is_Empty then
                  pragma Debug (O ("Unmarshall_To_Any : about to call "
                                   & "add_aggregate"));
                  Add_Aggregate_Element (Result, Label);
               end if;
               pragma Debug (O ("Unmarshall_To_Any : about to call "
                                & "member_count_with_label"));
               Nb := CORBA.TypeCode.Member_Count_With_Label (Tc, Label);
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
               Arg : CORBA.Any;
            begin
               Set_Any_Aggregate_Value (Result);
               if Is_Empty then
                  Arg := Get_Empty_Any (TC_Unsigned_Long);
               else
                  Arg := Get_Aggregate_Element
                    (Result,
                     TC_Unsigned_Long,
                     CORBA.Unsigned_Long (0));
               end if;
               Unmarshall_To_Any (Buffer, Arg);
               if Is_Empty then
                  Add_Aggregate_Element (Result, Arg);
               end if;
            end;
         when Tk_String =>
            declare
               S : CORBA.String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, S);
            end;
         when Tk_Sequence =>
            declare
               Nb : Unsigned_Long := Unmarshall (Buffer);
               Max_Nb : Unsigned_Long := TypeCode.Length (Tc);
               Arg : CORBA.Any;
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any : dealing with a sequence"));
               if Max_Nb > 0 and then Nb > Max_Nb then
                  Droopi.CORBA_P.Exceptions.Raise_Marshal;
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
                     CORBA.Unsigned_Long (0));
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
               Content_True_Type : CORBA.TypeCode.Object :=
                 TypeCode.Content_Type (Tc);
               Arg : CORBA.Any;
            begin
               while CORBA.TypeCode.Kind (Content_True_Type) = Tk_Array loop
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
               Arg : CORBA.Any;
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
               Ws : CORBA.Wide_String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, Ws);
            end;
         when Tk_Fixed =>
            --  FIXME : to be done
            --  declare
            --   Arg1,Arg2:CORBA.Any;
            --  begin
            --    pragma Debug(0 ("unmarshall_to_any: dealing with a fixed"));
            --    Set_Any_Aggregate_Value(Result);
            --    if Is_Empty then
            --      Arg1:= Get_Empty_Any(TypeCode.Fixed_Digits(Tc));
            --    else
            --      Arg1:= Get_Aggregate_Element
            --             (Result,
            --              TypeCode.Fixed_Digits(Tc),
            --              CORBA.Unsigned_Long(0));
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
            --              CORBA.Unsigned_Long(0));
            --    end if;
            --    Unmarshall_To_Any(Buffer, Arg2);
            --    if Is_Empty then
            --      Add_Aggregate_Element(Result,Arg2);
            --    end if;
            --   end;
            null;


         when Tk_Value =>

            --  declare
            --   Val_Modifier,Arg: CORBA.Any;
            --   Nb: CORBA.Unsigned_Long:=
            --          TypeCode.Member_Count(Tc);

            --  begin
            --   Set_Any_Aggregate_Value(Result);
            --   if Is_Empty then
            --     Val_Modifier:= Get_Empty_Any(TypeCode.Type_Modifier(Tc));
            --   else
            --     Val_Modifier:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Discriminator_Type(Tc),
            --                CORBA.Unsigned_Long(0));
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
            --              (Tc,CORBA.Unsigned_Long(0)));
            --     else
            --       Arg:= CORBA.Get_Aggregate_Element
            --                 (Result,
            --                  CORBA.TypeCode.Member_Type(Tc,
            --                  CORBA.Unsigned_Long(0)));
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

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.TypeCode.Object is
      Nb : CORBA.Unsigned_Long := Unmarshall (Buffer);
      Result : CORBA.TypeCode.Object;
   begin
      pragma Debug (O ("Unmarshall (TypeCode) : enter"));
      case Nb is
         when 0 =>
            Result := CORBA.TypeCode.TC_Null;
         when 1 =>
            Result := CORBA.TypeCode.TC_Void;
         when 2 =>
            Result := CORBA.TypeCode.TC_Short;
         when 3 =>
            Result := CORBA.TypeCode.TC_Long;
         when 4 =>
            Result := CORBA.TypeCode.TC_Unsigned_Short;
         when 5 =>
            Result := CORBA.TypeCode.TC_Unsigned_Long;
         when 6 =>
            Result := CORBA.TypeCode.TC_Float;
         when 7 =>
            Result := CORBA.TypeCode.TC_Double;
         when 8 =>
            Result := CORBA.TypeCode.TC_Boolean;
         when 9 =>
            Result := CORBA.TypeCode.TC_Char;
         when 10 =>
            Result := CORBA.TypeCode.TC_Octet;
         when 11 =>
            Result := CORBA.TypeCode.TC_Any;
         when 12 =>
            pragma Debug (O ("Unmarshall (TypeCode) : "
                             & "dealing with a TypeCode"));
            Result := CORBA.TypeCode.TC_TypeCode;
         when 13 =>
            Result := CORBA.TypeCode.TC_Principal;
         when 14 =>
            Result := CORBA.TypeCode.TC_Object;
            declare
               Id : CORBA.String := Unmarshall (Buffer);
               Name : CORBA.String := Unmarshall (Buffer);
            begin
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 15 =>
            Result := CORBA.TypeCode.TC_Struct;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
               Member_Type : CORBA.TypeCode.Object;
            begin
               pragma Debug (O ("unmarshall (TypeCode) : dealing "
                                & "with a struct"));
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id   := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb   := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 16 =>
            Result := CORBA.TypeCode.TC_Union;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : CORBA.String;
               Nb, Default_Index : CORBA.Unsigned_Long;
               Discriminator_Type, Member_Type : CORBA.TypeCode.Object;
               Member_Label : CORBA.Any;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Discriminator_Type := Unmarshall (Complex_Buffer);
               Default_Index := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Discriminator_Type));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Default_Index));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Label := Get_Empty_Any (Discriminator_Type);
                     Unmarshall_To_Any (Complex_Buffer, Member_Label);
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     CORBA.TypeCode.Add_Parameter
                       (Result, Member_Label);
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 17 =>
            Result := CORBA.TypeCode.TC_Enum;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer :  Buffer_Access := null;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 18 =>
            Result := CORBA.TypeCode.TC_String;
            declare
               Length : CORBA.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 19 =>
            Result := CORBA.TypeCode.TC_Sequence;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Length : CORBA.Unsigned_Long;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               Length := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 20 =>
            Result := CORBA.TypeCode.TC_Array;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Length : CORBA.Unsigned_Long;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               Length := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 21 =>
            Result := CORBA.TypeCode.TC_Alias;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : CORBA.String;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 22 =>
            Result := CORBA.TypeCode.TC_Except;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
               Member_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 23 =>
            Result := CORBA.TypeCode.TC_Long_Long;
         when 24 =>
            Result := CORBA.TypeCode.TC_Unsigned_Long_Long;
         when 25 =>
            Result := CORBA.TypeCode.TC_Long_Double;
         when 26 =>
            Result := CORBA.TypeCode.TC_Wchar;
         when 27 =>
            Result := CORBA.TypeCode.TC_Wide_String;
            declare
               Length : CORBA.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 28 =>
            Result := CORBA.TypeCode.TC_Fixed;
            declare
               Fixed_Digits : CORBA.Unsigned_Short;
               Fixed_Scale : CORBA.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Digits));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Scale));
            end;
         when 29 =>
            Result := CORBA.TypeCode.TC_Value;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name, Member_Name : CORBA.String;
               Type_Modifier, Visibility : CORBA.Short;
               Nb : CORBA.Unsigned_Long;
               Concrete_Base_Type, Member_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Type_Modifier := Unmarshall (Complex_Buffer);
               Concrete_Base_Type := Unmarshall (Complex_Buffer);
               Nb := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               if Nb /= 0 then
                  for I in 0 .. Nb - 1 loop
                     Member_Name := Unmarshall (Complex_Buffer);
                     Member_Type := Unmarshall (Complex_Buffer);
                     Visibility := Unmarshall (Complex_Buffer);
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Visibility));
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     CORBA.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;
         when 30 =>
            Result := CORBA.TypeCode.TC_Valuebox;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : CORBA.String;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               Content_Type := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 31 =>
            Result := CORBA.TypeCode.TC_Native;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer :  Buffer_Access := null;
               Id, Name : CORBA.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 32 =>
            Result := CORBA.TypeCode.TC_Abstract_Interface;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : Buffer_Access := null;
               Id, Name : CORBA.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer);
               Id := Unmarshall (Complex_Buffer);
               Name := Unmarshall (Complex_Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when others =>
            Droopi.CORBA_P.Exceptions.Raise_Marshal;
      end case;
      pragma Debug (O ("Unmarshall (TypeCode) : end"));
      return Result;
   end Unmarshall;

   procedure Unmarshall (Buffer : access Buffer_Type;
                         NV : in out CORBA.NamedValue) is
   begin
      pragma Debug (O ("Unmarshall (NamedValue) : enter"));
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (CORBA.Is_Empty
                                        (NV.Argument))));
      Unmarshall_To_Any (Buffer, NV.Argument);
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (CORBA.Is_Empty
                                        (NV.Argument))));
      pragma Debug (O ("Unmarshall (NamedValue) : end"));
   end Unmarshall;

   --   function Unmarshall (Buffer : access Buffer_Type)
   --     return Encapsulation
   --   is
   --      Length : constant CORBA.Unsigned_Long := Unmarshall (Buffer);
   --   begin
   --      pragma Debug (O ("Unmarshall (Encapsulation):
   --                length is" & Length'Img));
   --      declare
   --         E : Encapsulation (1 .. Stream_Element_Offset(Length));
   --      begin
   --         for I in E'Range loop
   --            E (I) := Stream_Element(CORBA.Octet'
   --                (Unmarshall (Buffer)));
   --         end loop;
   --         pragma Debug (O ("Unmarshall (Encapsulation): end"));
   --         return E;
   --      end;
   --   end Unmarshall;

   ---------
   --  Rev : reverse the order of the bytes
   ---------

   function Rev (Octets : Stream_Element_Array) return Stream_Element_Array is
      Result :  Stream_Element_Array (Octets'Range);
   begin
      for I in Octets'Range loop
         Result (Octets'Last - I + Octets'First) := Octets (I);
      end loop;
      return Result;
   end Rev;

   -----------------------------------------------------
   --  return the Ulong stored in a array of octets
   ---------------------------------------------------

   function Sum_Octet
     (Octets  : Stream_Element_Array;
      Order   : Integer) return CORBA.Unsigned_Long
   is
      use CORBA;
      Result : CORBA.Unsigned_Long := 0;
   begin
      for I in 0 .. Order - 1 loop
         Result := Result + CORBA.Unsigned_Long (Octets
                   (Octets'First + Stream_Element_Offset (I)))
                   * CORBA.Unsigned_Long (Octet_Order **
                   (Order - I - 1));
      end loop;
      return Result;
   end Sum_Octet;


   function Sum_Octet
     (Octets  : Stream_Element_Array;
      Order   : Integer) return CORBA.Unsigned_Long_Long
   is
      use CORBA;
      Result : CORBA.Unsigned_Long_Long  := 0;
   begin
      for I in 0 .. Order - 1 loop
         Result := Result + CORBA.Unsigned_Long_Long (Octets
                 (Octets'First + Stream_Element_Offset (I)))
                 * CORBA.Unsigned_Long_Long
                  (Octet_Order ** (Order - I - 1));
      end loop;
      return Result;
   end Sum_Octet;


   ------------------------------------
   -- Align_Marshall_Big_Endian_Copy --
   ------------------------------------

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Big_Endian_Copy;

   -------------------------------------
   -- Align_Marshall_Host_Endian_Copy --
   -------------------------------------

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Host_Endian_Copy;

   -------------------------
   -- Align_Marshall_Copy --
   -------------------------

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : in Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is

      --  subtype Data is Stream_Element_Array (Octets'Range);
      Data_Address : Opaque_Pointer;

   begin
      Align (Buffer, Alignment);
      Allocate_And_Insert_Cooked_Data
        (Buffer,
         Octets'Length,
         Data_Address);

      Data_Address.Zone (Data_Address.Offset
        .. Data_Address.Offset + Octets'Length - 1)
        := Octets;

   end Align_Marshall_Copy;

   --------------------------------------
   -- Align_Unmarshall_Big_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Big_Endian_Copy;

   --------------------------------------
   -- Align_Unmarshall_Host_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Host_Endian_Copy;

   ---------------------------
   -- Align_Unmarshall_Copy --
   ---------------------------

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array is

      Data_Address : Opaque_Pointer;

   begin
      Align (Buffer, Alignment);
      Extract_Data (Buffer, Data_Address, Size);
      return Data_Address.Zone
            (Data_Address.Offset .. Data_Address.Offset + Size - 1);
   end Align_Unmarshall_Copy;

   --------------
   -- Marshall --
   --------------

--   procedure Marshall
--     (Buffer : access Buffer_Type;
--      Data : in CORBA.AbstractBase.Ref'Class) is
--   begin
      --  !!!!!!!!!!!!!!!!!
      --  FIXME: I've just noticed that abstract interfaces must be
      --  encoded as unions
      --  with a boolean discriminator, cf spec and change code below.
      --  !!!!!!!!!!!!!!!!!

      --  1. if Data is a valuetype, call the valuetype marshalling function
--    if Data in CORBA.Value.Base'Class then
--       Droopi.CORBA_P.Value.Stream.Marshall (Buffer,
--                                    CORBA.Value.Base'Class (Data));

         --  2. check if Data is a nil ref, raise marshall if true
--    elsif CORBA.AbstractBase.Is_Nil (Data) then
--       Droopi.CORBA_P.Exceptions.Raise_Marshal;

         --  3. If Data is an abstract interface and the referenced object is
         --     a valuetype, then call the valuetype marshalling function.
         --  In practice, just check if the referenced object is a valuetype.
--    elsif CORBA.AbstractBase.Object_Of (Data).all
--   in CORBA.Value.Impl_Base'Class then
         --  Droopi.CORBA_P.Value.Stream.Marshall (Buffer,
         --                             Data);
--       null;
         --  Not implemented yet

      --  4. Call the interface marshalling function
--    else
         --  Make a redispatching call on the designated
         --  object.
--       declare
--            P : constant CORBA.Impl.Object_Ptr
--              := CORBA.AbstractBase.Object_Of (Data);
--         begin
--            CORBA.Impl.Marshall
--              (Buffer,
--               CORBA.Impl.Object'Class (P.all));
--         end;
--      end if;
--   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

--   procedure Unmarshall
--     (Buffer : access Buffer_Type;
--      Data : in out CORBA.AbstractBase.Ref'Class) is
--      Obj : constant Droopi.CORBA_P.Object.Object_Ptr
--        := new Droopi.CORBA_P.Object.Object_Type (Local_Object => False);
--   begin
--      Droopi.CORBA_P.Object.Unmarshall
--        (Buffer, Droopi.CORBA_P.Object.Object_Type (Obj.all));
--      CORBA.AbstractBase.Set (Data, CORBA.Impl.Object_Ptr (Obj));
--   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

--   function Unmarshall
--     (Buffer : access Buffer_Type)
--     return CORBA.Object.Ref is
--      New_Ref : CORBA.Object.Ref;
--   begin
--      Unmarshall (Buffer, New_Ref);
--*     return New_Ref;
--   end Unmarshall;

   -----------------------
   -- System exceptions --
   -----------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Excpt  : in CORBA.Exception_Occurrence)
   is
      Members : CORBA.System_Exception_Members;
   begin
      Droopi.CORBA_P.Exceptions.Get_Members (Excpt, Members);
      Marshall
        (Buffer, CORBA.String
         (Droopi.CORBA_P.Exceptions.Occurrence_To_Name (Excpt)));
      Marshall (Buffer, Members.Minor);
      Marshall
        (Buffer, Droopi.CORBA_P.Exceptions.To_Unsigned_Long
         (Members.Completed));
   end Marshall;

   procedure Unmarshall_And_Raise
     (Buffer : access Buffer_Type)
   is
      use Ada.Exceptions;

      Minor      : CORBA.Unsigned_Long;
      Status     : CORBA.Unsigned_Long;
      Identity   : Exception_Id;
      Repository : CORBA.String;

   begin
      Repository := Unmarshall (Buffer);
      Identity := Droopi.CORBA_P.Exceptions.Get_ExcepId_By_RepositoryId
        (CORBA.To_Standard_String (Repository));

      if Identity = Null_Id then
         --  If not found, this is a marshal error.
         Identity := CORBA.Marshal'Identity;
         Minor := 0;
         Status := Completion_Status'Pos (Completed_Maybe);
      end if;

      Minor  := Unmarshall (Buffer);
      Status := Unmarshall (Buffer);

      --  Raise the exception

      Droopi.CORBA_P.Exceptions.Stack.Raise_Exception
        (Identity,
         CORBA.System_Exception_Members'
         (Minor,
          Droopi.CORBA_P.Exceptions.To_Completion_Status (Status)));
   end Unmarshall_And_Raise;


   -------------------------------
   --  Marshall a sequence of octets
   ---------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Stream_Element_Array)
   is

   begin
      pragma Debug (O ("Marshall (Encapsulation) : enter"));
      Marshall (Buffer, CORBA.Unsigned_Long (Data'Length));
      for I in Data'Range loop
         Marshall (Buffer, CORBA.Octet (Data (I)));
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
      Length : constant CORBA.Unsigned_Long := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Encapsulation): length is" & Length'Img));
      declare
         E : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      begin
         for I in E'Range loop
            E (I) := Stream_Element
                     (CORBA.Octet'(Unmarshall (Buffer)));
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

      Fixed_Positive_Zero : constant CORBA.Octet
        := 16#C#;
      Fixed_Negative : constant CORBA.Octet
        := 16#D#;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F) is
      begin
         Marshall (Buffer, Data.all);
      end Marshall;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : in F)
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
                  Digit : constant CORBA.Octet
                    := CORBA.Octet (Val / Bias);
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
         use CORBA;

         O : CORBA.Octet;
         Result : F := 0.0;
      begin
         loop
            O := Unmarshall (Buffer);
            if O / 16 > 9
              or else
              (O mod 16 > 9
               and then O mod 16 /= CORBA.Octet (Fixed_Positive_Zero)
               and then O mod 16 /= CORBA.Octet (Fixed_Negative))
            then
               Droopi.CORBA_P.Exceptions.Raise_Marshal;
            end if;

            Result := Result * 10 + F (O / 16) * F'Delta;
            if O mod 16 < 10 then
               Result := Result * 10 + F (O mod 16) * F'Delta;
            else
               if O mod 16 = CORBA.Octet (Fixed_Negative) then
                  Result := -Result;
               end if;
               exit;
            end if;
         end loop;

         return Result;
      end Unmarshall;

   end Fixed_Point;

end Droopi.Representations.CDR;
