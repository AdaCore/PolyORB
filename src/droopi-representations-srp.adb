with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);
with Droopi.Utils;

with Interfaces;

package body Droopi.Representations.SRP is

   use Ada;
   use Ada.Strings.Unbounded;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.representations.srp");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------------

   ------------------------------------------
   -- Part taken from AWS (Ada Web Server) --
   ------------------------------------------
   ----------------
   -- Decode_URL --
   ----------------

   function Decode_URL (Str : in String) return String is
      I, K   : Positive := Str'First;
      Result : String (Str'Range);
   begin
      while I <= Str'Last loop
         if Str (I) = '+' then
            Result (K) := ' ';
            I := I + 1;

         elsif Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            declare
               Hex_Num : constant String := "16#" & Str (I + 1 .. I + 2) & '#';
            begin
               Result (K) := Character'Val (Natural'Value (Hex_Num));
               I := I + 3;
            end;

         else
            Result (K) := Str (I);
            I := I + 1;
         end if;

         K := K + 1;
      end loop;

      return Result (Result'First .. K - 1);
   end Decode_URL;

   -------------------
   -- Base64_Encode --
   -------------------

   function Base64_Encode (Data : Streams.Stream_Element_Array)
                          return String
   is
      use Streams;
      use type Streams.Stream_Element;

      function Base64 (E : in Stream_Element) return Character;
      --  returns the base64 character given a number

      function Shift_Left (Value  : in Stream_Element;
                           Amount : in Natural) return Stream_Element;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right (Value  : in Stream_Element;
                            Amount : in Natural) return Stream_Element;
      pragma Import (Intrinsic, Shift_Right);

      Result : Unbounded_String;
      Length : Natural := 0;
      State  : Positive range 1 .. 3 := 1;
      E, Old : Stream_Element := 0;

      function Base64 (E : in Stream_Element) return Character is
         V : Natural := Natural (E);
      begin
         if V in 0 .. 25 then
            return Character'Val (V + Character'Pos ('A'));
         elsif V in 26 .. 51 then
            return Character'Val (V - 26 + Character'Pos ('a'));
         elsif V in 52 .. 61 then
            return Character'Val (V - 52 + Character'Pos ('0'));
         elsif V = 62 then
            return '+';
         else
            return '/';
         end if;
      end Base64;

   begin
      for C in Data'Range loop
         E := Data (C);

         case State is
            when 1 =>
               Append (Result, Base64 (Shift_Right (E, 2) and 16#3F#));
               State := 2;
            when 2 =>
               Append (Result, Base64 ((Shift_Left (Old, 4) and 16#30#)
                                        or (Shift_Right (E, 4) and 16#F#)));
               State := 3;
            when 3 =>
               Append (Result, Base64 ((Shift_Left (Old, 2) and 16#3C#)
                                       or (Shift_Right (E, 6) and 16#3#)));
               Append (Result, Base64 (E and 16#3F#));
               State := 1;
         end case;

         Old := E;

         Length := Length + 1;

         if Length >= 72 then
            Append (Result, ASCII.LF);
            Length := 0;
         end if;
      end loop;

      case State is
         when 1 =>
            null;
         when 2 =>
            Append (Result, Base64 (Shift_Left (Old, 4) and 16#30#) & "==");
         when 3 =>
            Append (Result, Base64 (Shift_Left (Old, 2) and 16#3C#) & '=');
      end case;

      return To_String (Result);
   end Base64_Encode;

   function Base64_Encode (Data : in String) return String is
      use type Streams.Stream_Element_Offset;
      Stream_Data : Streams.Stream_Element_Array
        (1 .. Streams.Stream_Element_Offset (Data'Length));
      I : Streams.Stream_Element_Offset := 1;
   begin
      for K in Data'Range loop
         Stream_Data (I) := Character'Pos (Data (K));
         I := I + 1;
      end loop;
      return Base64_Encode (Stream_Data);
   end Base64_Encode;

   -------------------
   -- Base64_Decode --
   -------------------

   function Base64_Decode (B64_Data : in String)
                          return Streams.Stream_Element_Array
   is
      use Streams;
      use type Interfaces.Unsigned_32;
      use type Streams.Stream_Element_Offset;

      function Base64 (C : in Character) return Interfaces.Unsigned_32;
      --  returns the base64 stream element given a character

      function Shift_Left (Value  : in Interfaces.Unsigned_32;
                           Amount : in Natural) return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right (Value  : in Interfaces.Unsigned_32;
                            Amount : in Natural) return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Right);

      Result : Stream_Element_Array
        (Stream_Element_Offset range 1 .. B64_Data'Length);
      R      : Stream_Element_Offset := 1;

      Group  : Interfaces.Unsigned_32 := 0;
      J      : Integer := 18;

      Pad    : Stream_Element_Offset := 0;

      function Base64 (C : in Character) return Interfaces.Unsigned_32 is
      begin
         if C in 'A' .. 'Z' then
            return Character'Pos (C) - Character'Pos ('A');
         elsif C in 'a' .. 'z' then
            return Character'Pos (C) - Character'Pos ('a') + 26;
         elsif C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0') + 52;
         elsif C = '+' then
            return 62;
         else
            return 63;
         end if;
      end Base64;

   begin
      for C in B64_Data'Range loop

         if B64_Data (C) = ASCII.LF or else B64_Data (C) = ASCII.CR then
            null;

         else
            case B64_Data (C) is
               when '=' =>
                  Pad := Pad + 1;

               when others =>
                  Group := Group or Shift_Left (Base64 (B64_Data (C)), J);
            end case;

            J := J - 6;

            if J < 0 then
               Result (R .. R + 2) :=
                 (Stream_Element (Shift_Right (Group and 16#FF0000#, 16)),
                  Stream_Element (Shift_Right (Group and 16#00FF00#, 8)),
                  Stream_Element (Group and 16#0000FF#));

               R := R + 3;

               Group := 0;
               J     := 18;
            end if;

         end if;
      end loop;

      return Result (1 .. R - 1 - Pad);
   end Base64_Decode;


   -----------------------------------

   ----------------
   -- Encode_URL --
   ----------------

   function Encode_URL (Str : in String) return String
   is
      use Characters.Handling;

      subtype Character_SEA is Stream_Element_Array
        (1 .. Character'Size / Stream_Element'Size);
      --  SEA means Stream_Element_Array

      function Char_To_SEA is
         new Ada.Unchecked_Conversion (Character, Character_SEA);
      --  SEA means Stream_Element_Array

      Encoded_URL : Unbounded_String;
   begin
      for I in Str'Range loop
         if Is_Alphanumeric (Str (I)) = False then
            Append (Encoded_URL,
                    "%" & Droopi.Utils.To_String
                    (Char_To_SEA (Str (I))));
         else
            Append (Encoded_URL, Str (I));
         end if;
      end loop;

      return To_String (Encoded_URL);
   end Encode_URL;



   procedure Marshall_From_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : CORBA.Any)
   is
      URL : CORBA.String := CORBA.From_Any (Data);
      Coded_URL : String_Ptr;
      --  R_Access : Rep_SRP;
   begin
      --  R_Access := R;

      --  ??? For now we don't use the Base64 coding
      --  Coded_URL :=
      --       new String'(Base64_Encode (CORBA.To_Standard_String (URL)));
      Coded_URL := new String'(Encode_URL (CORBA.To_Standard_String (URL)));
      pragma Debug (O ("Coded URL : " & Coded_URL.all));

      for I in Coded_URL.all'Range loop
         Align_Marshall_Copy
           (Buffer, Stream_Element_Array'
            (1 => Stream_Element (Character'Pos (Coded_URL.all (I)))));
      end loop;

   end Marshall_From_Any;

   procedure Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out CORBA.Any)
   is
      Encoded_URL : String_Ptr;
      Decoded_URL : String_Ptr;
   begin
      --  raise Not_Implemented;
      Encoded_URL := new String'(Unmarshall_String (R, Buffer));
      Decoded_URL := new String'(Decode_URL (Encoded_URL.all));
      Data := CORBA.To_Any
        (CORBA.To_CORBA_String (Decode_URL (Decoded_URL.all)));
   end Unmarshall_To_Any;

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'
         (1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character
   is
      A : constant Stream_Element_Array
        := Align_Unmarshall_Copy (B, 1);
   begin
      return Character'Val (A (A'First));
   end Unmarshall_Char;

   procedure Marshall_String
     (R : access Rep_SRP;
      B : access Buffer_Type;
      S : String)
   is
   begin
      for I in S'Range loop
         Marshall_Char (B, S (I));
      end loop;
   end Marshall_String;

   function Unmarshall_String
     (R : Rep_SRP;
      B : access Buffer_Type)
     return String
   is
      S : String (1 .. 1024);
      C : Character;
      Last : Integer := S'First - 1;
      Max : constant Stream_Element_Count
        := Length (B);
   begin
      loop
         exit when Last - S'First + 1 = Integer (Max);
         C := Unmarshall_Char (B);
         if C = ASCII.CR then
            C := Unmarshall_Char (B);
            pragma Assert (C = ASCII.LF);

            exit;
         end if;
         Last := Last + 1;
         S (Last) := C;
         exit when Last = S'Last;
      end loop;

      return S (S'First .. Last);
   end Unmarshall_String;

   function Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type) return CORBA.Any
   is
      Data : CORBA.Any;
   begin
      Unmarshall_To_Any (R, Buffer, Data);
      return Data;
   end Unmarshall_To_Any;

end Droopi.Representations.SRP;
