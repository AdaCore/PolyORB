with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Broca.Exceptions;
with CORBA;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Marshalling is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.marshalling");
   procedure O is new Broca.Debug.Output (Flag);

   subtype Short_Buffer_Type is Buffer_Type (0 .. 1);
   function Buffer_Type_To_Unsigned_Short is new Ada.Unchecked_Conversion
     (Source => Short_Buffer_Type, Target => CORBA.Unsigned_Short);
   function Unsigned_Short_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Short, Target => Short_Buffer_Type);
   function Buffer_Type_To_Short is new Ada.Unchecked_Conversion
     (Source => Short_Buffer_Type, Target => CORBA.Short);
   function Short_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Short, Target => Short_Buffer_Type);

   subtype Long_Buffer_Type is Buffer_Type (0 .. 3);
   function Buffer_Type_To_Unsigned_Long is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Unsigned_Long);
   function Unsigned_Long_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Long, Target => Long_Buffer_Type);
   function Buffer_Type_To_Long is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Long);
   function Long_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Long, Target => Long_Buffer_Type);
   function Buffer_Type_To_Float is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Float);
   function Float_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Float, Target => Long_Buffer_Type);

   subtype Very_Long_Buffer_Type is Buffer_Type (0 .. 7);
   function Buffer_Type_To_Double is new Ada.Unchecked_Conversion
     (Source => Very_Long_Buffer_Type, Target => CORBA.Double);
   function Double_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Double, Target => Very_Long_Buffer_Type);

   --  Unmarshall procedure
   --  Extract a type from BUFFER starting at POS, which maybe aligned before,
   --  and update the position POS.

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Octet) is
   begin
      Res := CORBA.Octet (Stream.Buffer (Stream.Pos));
      Stream.Pos := Stream.Pos + 1;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Char) is
   begin
      Res := CORBA.Char'Val (Stream.Buffer (Stream.Pos));
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
      Align_Size (Stream, 2);
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
      Align_Size (Stream, 4);
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
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Short) is
   begin
      Align_Size (Stream, 2);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Short
           ((Stream.Buffer (Stream.Pos + 1), Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Short
           (Stream.Buffer (Stream.Pos .. Stream.Pos + 1));
      end if;
      Stream.Pos := Stream.Pos + 2;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Long) is
   begin
      Align_Size (Stream, 4);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Long
           ((Stream.Buffer (Stream.Pos + 3),
             Stream.Buffer (Stream.Pos + 2),
             Stream.Buffer (Stream.Pos + 1),
             Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Long
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

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Float) is
   begin
      Align_Size (Stream, 4);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Float
           ((Stream.Buffer (Stream.Pos + 3),
             Stream.Buffer (Stream.Pos + 2),
             Stream.Buffer (Stream.Pos + 1),
             Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Float
           (Stream.Buffer (Stream.Pos .. Stream.Pos + 3));
      end if;
      Stream.Pos := Stream.Pos + 4;
   end Unmarshall;

   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Double) is
   begin
      Align_Size (Stream, 8);
      if Stream.Little_Endian /= Is_Little_Endian then
         Res := Buffer_Type_To_Double
           ((Stream.Buffer (Stream.Pos + 7),
             Stream.Buffer (Stream.Pos + 6),
             Stream.Buffer (Stream.Pos + 5),
             Stream.Buffer (Stream.Pos + 4),
             Stream.Buffer (Stream.Pos + 3),
             Stream.Buffer (Stream.Pos + 2),
             Stream.Buffer (Stream.Pos + 1),
             Stream.Buffer (Stream.Pos)));
      else
         Res := Buffer_Type_To_Double
           (Stream.Buffer (Stream.Pos .. Stream.Pos + 7));
      end if;
      Stream.Pos := Stream.Pos + 8;
   end Unmarshall;

   --  Size procedure
   --  Only adjust stream.pos according to alignment.
   procedure Marshall_Size_Primitive_Sequence
     (Stream : in out Buffer_Descriptor;
      Element_Size : Natural;
      Nbr_Elements : Natural) is
   begin
      --  Align for number of element (which is a unsigned long).
      Align_Size (Stream, 4);
      Stream.Pos := Stream.Pos + 4;
      Align_Size (Stream, Alignment_Type (Element_Size));
      Stream.Pos :=
        Stream.Pos + Buffer_Index_Type (Nbr_Elements * Element_Size);
   end Marshall_Size_Primitive_Sequence;

   procedure Marshall_Size_Octet (Stream : in out Buffer_Descriptor) is
   begin
      Stream.Pos := Stream.Pos + 1;
   end Marshall_Size_Octet;

   procedure Marshall_Size_Long (Stream : in out Buffer_Descriptor) is
   begin
      Align_Size (Stream, 4);
      Stream.Pos := Stream.Pos + 4;
   end Marshall_Size_Long;

   procedure Marshall_Size_Unsigned_Long (Stream : in out Buffer_Descriptor)
     renames Marshall_Size_Long;

   procedure Marshall_Size_Short (Stream : in out Buffer_Descriptor)
   is
   begin
      Align_Size (Stream, 2);
      Stream.Pos := Stream.Pos + 2;
   end Marshall_Size_Short;

   procedure Marshall_Size_Unsigned_Short (Stream : in out Buffer_Descriptor)
     renames Marshall_Size_Short;

   procedure Marshall_Size_Float (Stream : in out Buffer_Descriptor) is
   begin
      Align_Size (Stream, 4);
      Stream.Pos := Stream.Pos + 4;
   end Marshall_Size_Float;

   procedure Marshall_Size_Double (Stream : in out Buffer_Descriptor) is
   begin
      Align_Size (Stream, 8);
      Stream.Pos := Stream.Pos + 8;
   end Marshall_Size_Double;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.String) is
      use CORBA;
   begin
      Align_Size (Stream, 4);
      Stream.Pos := Stream.Pos + 4 + Buffer_Index_Type (Length (Val)) + 1;
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : String) is
      use CORBA;
   begin
      Align_Size (Stream, 4);
      Stream.Pos := Stream.Pos + 4 + Buffer_Index_Type (Val'Length) + 1;
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Octet) is
   begin
      Marshall_Size_Octet (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Char) is
   begin
      Marshall_Size_Octet (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Boolean) is
   begin
      Marshall_Size_Octet (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Long) is
   begin
      Marshall_Size_Long (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Short) is
   begin
      Marshall_Size_Short (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Long) is
   begin
      Marshall_Size_Long (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Short) is
   begin
      Marshall_Size_Short (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Float) is
   begin
      Marshall_Size_Float (Stream);
   end Marshall_Size;

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Double) is
   begin
      Marshall_Size_Double (Stream);
   end Marshall_Size;

   --  Marshall
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Octet) is
   begin
      Stream.Buffer (Stream.Pos) := Byte (Val);
      Stream.Pos := Stream.Pos + 1;
   end Marshall;

   -------------
   -- Compare --
   -------------

   function Compare (Buffer  : in Buffer_Descriptor;
                     Pattern : in String) return Boolean
   is
      use CORBA;
      Old : Buffer_Descriptor;
      Len : CORBA.Unsigned_Long;
      Pos : Buffer_Index_Type;
      C   : Character;
   begin
      --  FIXME: not as efficient as possible.
      Old := Buffer;
      Unmarshall (Old, Len);
      Pos := Old.Pos;
      if Len /= Pattern'Length + 1 then
         return False;
      end if;
      for I in 0 .. Unsigned_Long'Pos (Len - 2) loop
         C := Character'Val (Buffer.Buffer (Pos + Buffer_Index_Type (I)));
         if C /= Pattern (Pattern'First + I) then
            return False;
         end if;
      end loop;
      return True;
   end Compare;

   -----------------
   -- Skip_String --
   -----------------

   procedure Skip_String (Buffer : in out Buffer_Descriptor)
   is
      Length : CORBA.Unsigned_Long;
   begin
      Unmarshall (Buffer, Length);
      Buffer.Pos := Buffer.Pos + Buffer_Index_Type (Length);
   end Skip_String;

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
      Align_Size (Stream, 2);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 1) :=
        Unsigned_Short_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 2;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Long) is
   begin
      Align_Size (Stream, 4);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 3) :=
        Unsigned_Long_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 4;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Short) is
   begin
      Align_Size (Stream, 2);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 1) :=
        Short_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 2;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Long) is
   begin
      Align_Size (Stream, 4);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 3) :=
        Long_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 4;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Float) is
   begin
      Align_Size (Stream, 4);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 3) :=
        Float_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 4;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Double) is
   begin
      Align_Size (Stream, 8);
      Stream.Buffer (Stream.Pos .. Stream.Pos + 7) :=
        Double_To_Buffer_Type (Val);
      Stream.Pos := Stream.Pos + 8;
   end Marshall;

   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.String)
   is
      package U renames Ada.Strings.Unbounded;
      Val_Length : Natural;
   begin
      Val_Length := U.Length (U.Unbounded_String (Val));
      Marshall (Stream, CORBA.Unsigned_Long (Val_Length + 1));
      for I in 0 .. Val_Length - 1 loop
         --  Low bound is 1.
         Stream.Buffer (Stream.Pos + Buffer_Index_Type (I)) :=
           Character'Pos (U.Element (U.Unbounded_String (Val), 1 + I));
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

end Broca.Marshalling;
