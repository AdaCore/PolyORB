with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Broca.Exceptions;

package body Broca.Marshalling is
   subtype Ushort_Buffer_Type is Buffer_Type (0 .. 1);
   function Buffer_Type_To_Unsigned_Short is new Ada.Unchecked_Conversion
     (Source => Ushort_Buffer_Type, Target => CORBA.Unsigned_Short);
   function Unsigned_Short_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Short, Target => Ushort_Buffer_Type);

   subtype Ulong_Buffer_Type is Buffer_Type (0 .. 3);
   function Buffer_Type_To_Unsigned_Long is new Ada.Unchecked_Conversion
     (Source => Ulong_Buffer_Type, Target => CORBA.Unsigned_Long);
   function Unsigned_Long_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Long, Target => Ulong_Buffer_Type);

   procedure Align
     (Buffer : in out Buffer_Descriptor; Alignment : Buffer_Index_Type);
   pragma Inline (Align);

   procedure Align
     (Buffer : in out Buffer_Descriptor; Alignment : Buffer_Index_Type) is
   begin
      Buffer.Pos := Buffer.Pos +
        ((Alignment - (Buffer.Pos mod Alignment)) mod Alignment);
   end Align;

   procedure Marshall_Align_2 (Buffer : in out Buffer_Descriptor) is
   begin
      Align (Buffer, 2);
   end Marshall_Align_2;

   procedure Marshall_Align_4 (Buffer : in out Buffer_Descriptor) is
   begin
      Align (Buffer, 4);
   end Marshall_Align_4;

   procedure Marshall_Align_8 (Buffer : in out Buffer_Descriptor) is
   begin
      Align (Buffer, 8);
   end Marshall_Align_8;

   procedure Marshall_Align_16 (Buffer : in out Buffer_Descriptor) is
   begin
      Align (Buffer, 16);
   end Marshall_Align_16;


   --  Append the contents of SOURCE to TARGET.
   --  TARGET size must be big enough.
   procedure Marshall_Append (Target : in out Buffer_Descriptor;
                              Source : in Buffer_Descriptor) is
   begin
      Target.Buffer (Target.Pos .. Target.Pos + Source.Pos - 1)
        := Source.Buffer (0 .. Source.Pos - 1);
      Target.Pos := Target.Pos + Source.Pos;
   end Marshall_Append;

   procedure Marshall_Size_Append (Target : in out Buffer_Descriptor;
                                   Source : in Buffer_Descriptor) is
   begin
      Target.Pos := Target.Pos + Source.Pos;
   end Marshall_Size_Append;

   procedure Unmarshall_Extract (Target : in out Buffer_Descriptor;
                                 Source : in out Buffer_Descriptor;
                                 Length : Buffer_Index_Type) is
   begin
      Target.Buffer (Target.Pos .. Target.Pos + Length - 1) :=
        Source.Buffer (Source.Pos .. Source.Pos + Length - 1);
      Source.Pos := Source.Pos + Length;
      Target.Pos := 0;
      Target.Little_Endian := Source.Little_Endian;
   end Unmarshall_Extract;

   --  Skip a string in a buffer.
   procedure Unmarshall_Skip_String (Buffer : in out Buffer_Descriptor)
   is
      Length : CORBA.Unsigned_Long;
   begin
      Unmarshall (Buffer, Length);
      Buffer.Pos := Buffer.Pos + Buffer_Index_Type (Length);
   end Unmarshall_Skip_String;

   --  Unmarshall procedure
   --  Extract a type from BUFFER starting at POS, which maybe aligned before,
   --  and update the position POS.

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Octet) is
   begin
      Res := Stream.Buffer (Stream.Pos);
      Stream.Pos := Stream.Pos + 1;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Boolean) is
   begin
      case Stream.Buffer (Stream.Pos) is
         when 0 =>
            Res := False;
         when 1 =>
            Res := True;
         when others =>
            Broca.Exceptions.Raise_Marshal;
      end case;
      Stream.Pos := Stream.Pos + 1;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Unsigned_Short) is
   begin
      Marshall_Align_2 (Stream);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Unsigned_Short
           ((Stream.Buffer (Stream.Pos + 1), Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Unsigned_Short
           (Stream.Buffer (Stream.Pos .. Stream.Pos + 1));
      end if;
      Stream.Pos := Stream.Pos + 2;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Unsigned_Long) is
   begin
      Marshall_Align_4 (Stream);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Unsigned_Long
           ((Stream.Buffer (Stream.Pos + 3),
             Stream.Buffer (Stream.Pos + 2),
             Stream.Buffer (Stream.Pos + 1),
             Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Unsigned_Long
           (Stream.Buffer (Stream.Pos .. Stream.Pos + 3));
      end if;
      Stream.Pos := Stream.Pos + 4;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.String)
   is
      use Ada.Strings.Unbounded;
      use CORBA;
      Tmp_Res : Unbounded_String;
      Length : CORBA.Unsigned_Long;
   begin
      Unmarshall (Stream, Length);
      Tmp_Res := To_Unbounded_String (Natural (Length) - 1);
      for I in 1 .. Length - 1 loop
         Replace_Element (Tmp_Res, Natural (I),
                          Character'Val (Stream.Buffer (Stream.Pos)));
         Stream.Pos := Stream.Pos + 1;
      end loop;
      if Stream.Buffer (Stream.Pos) /= 0 then
         Broca.Exceptions.Raise_Marshal;
      else
         Stream.Pos := Stream.Pos + 1;
      end if;
      Res := CORBA.String (Tmp_Res);
   end Unmarshall;

   --  Size procedure
   --  Only adjust stream.pos according to alignment.
   procedure Marshall_Size_Primitive_Sequence
     (Stream : in out Buffer_Descriptor;
      Element_Size : Natural;
      Nbr_Elements : Natural) is
   begin
      --  Align for number of element (which is a unsigned long).
      Marshall_Align_4 (Stream);
      Stream.Pos := Stream.Pos + 4;
      Align (Stream, Buffer_Index_Type (Element_Size));
      Stream.Pos :=
        Stream.Pos + Buffer_Index_Type (Nbr_Elements * Element_Size);
   end Marshall_Size_Primitive_Sequence;

   procedure Marshall_Size_Octet (Stream : in out Buffer_Descriptor) is
   begin
      Stream.Pos := Stream.Pos + 1;
   end Marshall_Size_Octet;

   procedure Marshall_Size_Unsigned_Long (Stream : in out Buffer_Descriptor) is
   begin
      Marshall_Align_4 (Stream);
      Stream.Pos := Stream.Pos + 4;
   end Marshall_Size_Unsigned_Long;

   procedure Marshall_Size_Unsigned_Short (Stream : in out Buffer_Descriptor)
   is
   begin
      Marshall_Align_2 (Stream);
      Stream.Pos := Stream.Pos + 2;
   end Marshall_Size_Unsigned_Short;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.String) is
      use CORBA;
   begin
      Marshall_Align_4 (Stream);
      Stream.Pos := Stream.Pos + 4 + Buffer_Index_Type (Length (Val)) + 1;
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : String) is
      use CORBA;
   begin
      Marshall_Align_4 (Stream);
      Stream.Pos := Stream.Pos + 4 + Buffer_Index_Type (Val'Length) + 1;
   end Marshall_Size;

   --  Allocate or reallocate Stream.Buffer to that its length is at least
   --  equal to SIZE.
   procedure Increase_Buffer
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type);

   procedure Increase_Buffer
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type) is
   begin
      if Stream.Buffer = null or else Stream.Buffer.all'Last < Size - 1 then
         Unchecked_Deallocation (Stream.Buffer);
         Stream.Buffer := new Buffer_Type (0 .. Size - 1);
      end if;
   end Increase_Buffer;

   procedure Increase_Buffer_And_Set_Pos
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type) is
   begin
      Increase_Buffer (Stream, Size);
      Stream.Pos := Size;
   end Increase_Buffer_And_Set_Pos;

   procedure Increase_Buffer_And_Clear_Pos
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type) is
   begin
      Increase_Buffer (Stream, Size);
      Stream.Pos := 0;
   end Increase_Buffer_And_Clear_Pos;

   --  For an outcoming stream, allocate the buffer and clear pos.
   procedure Allocate_Buffer (Stream : in out Buffer_Descriptor) is
   begin
      Increase_Buffer (Stream, Stream.Pos);
      Stream.Pos := 0;
      Stream.Little_Endian := Is_Little_Endian;
   end Allocate_Buffer;

   --  Marshall
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Octet) is
   begin
      Stream.Buffer (Stream.Pos) := Val;
      Stream.Pos := Stream.Pos + 1;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Char) is
   begin
      Stream.Buffer (Stream.Pos) := CORBA.Char'Pos (Val);
      Stream.Pos := Stream.Pos + 1;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Boolean) is
   begin
      Stream.Buffer (Stream.Pos) := CORBA.Boolean'Pos (Val);
      Stream.Pos := Stream.Pos + 1;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Short) is
   begin
      Marshall_Align_2 (Stream);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 1) :=
        Unsigned_Short_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 2;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Long) is
   begin
      Marshall_Align_4 (Stream);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 3) :=
        Unsigned_Long_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 4;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.String) is
      use Ada.Strings.Unbounded;
      Val_Length : Natural;
   begin
      Val_Length := Length (Unbounded_String (Val));
      Marshall (Stream, CORBA.Unsigned_Long (Val_Length + 1));
      for I in 0 .. Val_Length - 1 loop
         --  Low bound is 1.
         Stream.Buffer (Stream.Pos + Buffer_Index_Type (I)) :=
           Character'Pos (Element (Unbounded_String (Val), 1 + I));
      end loop;
      Stream.Buffer (Stream.Pos + Buffer_Index_Type (Val_Length)) := 0;

      Stream.Pos := Stream.Pos + Buffer_Index_Type (Val_Length + 1);
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : String) is
      use Ada.Strings.Unbounded;
      Val_Length : Natural;
   begin
      Val_Length := Val'Length;
      Marshall (Stream, CORBA.Unsigned_Long (Val_Length + 1));
      for I in 0 .. Val_Length - 1 loop
         --  Low bound is 1.
         Stream.Buffer (Stream.Pos + Buffer_Index_Type (I)) :=
           Character'Pos (Val (Val'First + I));
      end loop;
      Stream.Buffer (Stream.Pos + Buffer_Index_Type (Val_Length)) := 0;

      Stream.Pos := Stream.Pos + Buffer_Index_Type (Val_Length + 1);
   end Marshall;

begin
   declare
      use CORBA;

      Buf : Buffer_Type (0 .. 1) := (16, 8);
      Us : CORBA.Unsigned_Short := Buffer_Type_To_Unsigned_Short (Buf);
   begin
      Is_Little_Endian := "/=" (Us, 16 * 256 + 8);
   end;
end Broca.Marshalling;
