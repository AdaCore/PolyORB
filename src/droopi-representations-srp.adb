with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Droopi.Any;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);
with Droopi.Objects;
with Droopi.Types;
with Droopi.Utils;
with Droopi.Utils.SRP; use Droopi.Utils.SRP;

with Interfaces;

package body Droopi.Representations.SRP is

   use Ada;
   use Ada.Strings.Unbounded;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.representations.srp");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

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
   --  ??? Should be removed
   function Encode_URL (Str : in String) return String
   is
      use Characters.Handling;

--       Split_URL : Split_SRP := Split (Str);
--       Current_Arg : Arg_Info_Ptr := Split_URL.Args;
   begin
      raise Deprecated;
--       while Current_Arg /= null loop
--          --  ??? Free mem ?
--          Current_Arg.all.Value :=
--            new String'(Encode_String (Current_Arg.all.Value.all));
--          Current_Arg := Current_Arg.all.Next;
--       end loop;

--       return Types.To_Standard_String (Any.From_Any (Join (Split_URL)));
      return "";
   end Encode_URL;

   ----------------
   -- Encode_URL --
   ----------------

   function Encode_URL (SRP_Info : Split_SRP) return Types.String
   is
      use Droopi.Objects;
      Result : Types.String;
      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
   begin
      Append (Result, SRP_Info.Method.all & " " &
              Objects.To_String (SRP_Info.Oid.all));
      if Current_Arg /= null then
         Append (Result, "?");
      end if;

      while Current_Arg /= null loop
         --  ??? Free mem ?
         Append (Result, Current_Arg.all.Name.all & "=");
         Append (Result, Encode_String
                 (To_Standard_String (Current_Arg.all.Value.all)));
         if Current_Arg.all.Next /= null then
            Append (Result, "&");
         end if;
         Current_Arg := Current_Arg.all.Next;
      end loop;

--      return Types.To_Standard_String (Any.From_Any (Join (Split_URL)));
      return Result;
   end Encode_URL;


   ----------------
   -- Encode_URL --
   ----------------

   procedure Encode_URL (SRP_Info : in out Split_SRP)
   is
      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
   begin
      raise Deprecated;
--       while Current_Arg /= null loop
--          --  ??? Free mem ?
--          Current_Arg.all.Value :=
--            new String'(Encode_String (Current_Arg.all.Value.all));
--          Current_Arg := Current_Arg.all.Next;
--       end loop;
   end Encode_URL;

   -------------------
   -- Encode_String --
   -------------------

   function Encode_String (Str : String) return String
   is
      subtype Character_SEA is Stream_Element_Array
        (1 .. Character'Size / Stream_Element'Size);
      --  SEA means Stream_Element_Array

      function Char_To_SEA is
         new Ada.Unchecked_Conversion (Character, Character_SEA);
      --  SEA means Stream_Element_Array

      Encoded_String : Unbounded_String;
   begin
      for I in Str'Range loop
         if Characters.Handling.Is_Alphanumeric (Str (I)) = False then
            Append (Encoded_String,
                    "%" & Droopi.Utils.To_String
                    (Char_To_SEA (Str (I))));
         else
            Append (Encoded_String, Str (I));
         end if;
      end loop;
      return To_String (Encoded_String);
   end Encode_String;

   ----------------------
   -- Utility programs --
   ----------------------

   -----------
   -- SPECS --
   -----------

--    function Rev
--      (Octets : Stream_Element_Array)
--      return Stream_Element_Array;
--    --  Reverse the order of an array of octets

--    --  function Align_Octet
--    --    (Data:in F;
--    --     Order: Integer) return Stream_Element_Array;
--    --  Align Octets on Array of octets

--    procedure Align_Marshall_Big_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    :        Stream_Element_Array;
--       Alignment :        Alignment_Type       := 1);
--    --  Align Buffer on Alignment, then marshall a copy of
--    --  Octets into it.
--    --  The data in Octets shall be presented in big-endian
--    --  byte order.


   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Alignment :        Alignment_Type       := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in big-endian byte order.

--    procedure Align_Marshall_Host_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    :        Stream_Element_Array;
--       Alignment :        Alignment_Type       := 1);
--    --  Align Buffer on Alignment, then marshall a copy of
--    --  Octets into it.
--    --  The data in Octets shall be presented in the
--    --  host's byte order.

--    function Align_Unmarshall_Host_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Size      : Stream_Element_Count;
--       Alignment : Alignment_Type := 1)
--      return Stream_Element_Array;
--    --  Align Buffer on Alignment, then unmarshall a copy of
--    --  Size octets from it.
--    --  The data is returned in the host's byte order

--    procedure Align_Marshall_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    :        Stream_Element_Array;
--       Alignment :        Alignment_Type       := 1);
--    --  Align Buffer on Alignment, then marshall a copy
--    --  of Octets into Buffer, as is.

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Alignment :        Alignment_Type       := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy
   --  of Size octets from Buffer's data, as is.

   ---------------------
   -- IMPLEMENTATIONS --
   ---------------------

   ------------------------------------
   -- Align_Marshall_Big_Endian_Copy --
   ------------------------------------

--    procedure Align_Marshall_Big_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    :        Stream_Element_Array;
--       Alignment :        Alignment_Type       := 1)
--    is
--    begin
--       if Endianness (Buffer.all) = Big_Endian then
--          Align_Marshall_Copy (Buffer, Octets, Alignment);
--       else
--          Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
--       end if;
--    end Align_Marshall_Big_Endian_Copy;

--    -------------------------------------
--    -- Align_Marshall_Host_Endian_Copy --
--    -------------------------------------

--    procedure Align_Marshall_Host_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    :        Stream_Element_Array;
--       Alignment :        Alignment_Type       := 1)
--    is
--    begin
--       if Endianness (Buffer.all) = Host_Order then
--          Align_Marshall_Copy (Buffer, Octets, Alignment);
--       else
--          Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
--       end if;
--    end Align_Marshall_Host_Endian_Copy;

--    -------------------------
--    -- Align_Marshall_Copy --
--    -------------------------

--    procedure Align_Marshall_Copy
--      (Buffer    : access Buffer_Type;
--       Octets    : Stream_Element_Array;
--       Alignment : Alignment_Type := 1)
--    is

--       --  subtype Data is Stream_Element_Array (Octets'Range);
--       Data_Address : Opaque_Pointer;

--    begin
--       Align (Buffer, Alignment);
--       Allocate_And_Insert_Cooked_Data
--         (Buffer,
--          Octets'Length,
--          Data_Address);

--       Data_Address.Zone (Data_Address.Offset
--         .. Data_Address.Offset + Octets'Length - 1)
--         := Octets;

--    end Align_Marshall_Copy;

   --------------------------------------
   -- Align_Unmarshall_Big_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Alignment :        Alignment_Type       := 1)
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

--    --------------------------------------
--    -- Align_Unmarshall_Host_Endian_Copy --
--    --------------------------------------

--    function Align_Unmarshall_Host_Endian_Copy
--      (Buffer    : access Buffer_Type;
--       Size      : Stream_Element_Count;
--       Alignment : Alignment_Type := 1)
--      return Stream_Element_Array
--    is
--    begin
--       if Endianness (Buffer.all) = Host_Order then
--          return Align_Unmarshall_Copy (Buffer, Size, Alignment);
--       else
--          return Rev (Align_Unmarshall_Copy
--                      (Buffer, Size, Alignment));
--       end if;
--    end Align_Unmarshall_Host_Endian_Copy;

   ---------------------------
   -- Align_Unmarshall_Copy --
   ---------------------------

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Alignment :        Alignment_Type       := 1)
     return Stream_Element_Array is

      Data_Address : Opaque_Pointer;

   begin
      Align (Buffer, Alignment);
      Extract_Data (Buffer, Data_Address, Size);
      return Data_Address.Zone
            (Data_Address.Offset .. Data_Address.Offset + Size - 1);
   end Align_Unmarshall_Copy;


   -------------------------------------------
   -- Conversions between DROOPI signed and --
   -- unsigned integer types.               --
   -------------------------------------------

   function To_Long_Long is
      new Ada.Unchecked_Conversion
        (Droopi.Types.Unsigned_Long_Long, Droopi.Types.Long_Long);
   function To_Unsigned_Long_Long is
      new Ada.Unchecked_Conversion
        (Droopi.Types.Long_Long, Droopi.Types.Unsigned_Long_Long);
   function To_Long is
      new Ada.Unchecked_Conversion
        (Droopi.Types.Unsigned_Long, Droopi.Types.Long);
   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
        (Droopi.Types.Long, Droopi.Types.Unsigned_Long);
   function To_Short is
      new Ada.Unchecked_Conversion
        (Droopi.Types.Unsigned_Short, Droopi.Types.Short);
   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion
     (Droopi.Types.Short, Droopi.Types.Unsigned_Short);


   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      NV     : in out NamedValue) is
   begin
      pragma Debug (O ("Unmarshall (NamedValue) : enter"));
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (Droopi.Any.Is_Empty
                                        (NV.Argument))));
      Unmarshall_To_Any (Buffer, NV.Argument);
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (Droopi.Any.Is_Empty
                                        (NV.Argument))));
      pragma Debug (O ("Unmarshall (NamedValue) : end"));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Types.Boolean is
   begin
      pragma Debug (O ("Unmarshall (Boolean) : enter & end"));
      return Droopi.Types.Boolean'Val
        (Droopi.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Types.Octet
   is
      Result : constant Stream_Element_Array
        := Align_Unmarshall_Copy (Buffer, 1, 1);
   begin
      pragma Debug (O ("Unmarshall (Octet) : enter & end"));
      return Droopi.Types.Octet (Result (Result'First));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Types.Unsigned_Short
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      pragma Debug (O ("Unmarshall (UShort) : enter & end"));
      return Droopi.Types.Unsigned_Short (Octets (Octets'First)) * 256 +
        Droopi.Types.Unsigned_Short (Octets (Octets'First + 1));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Types.Char is
   begin
      pragma Debug (O ("Unmarshall (Char) : enter & end"));
      return Droopi.Types.Char'Val
        (Droopi.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Types.Unsigned_Long
   is
      Octets : constant Stream_Element_Array
        := Align_Unmarshall_Big_Endian_Copy (Buffer, 4, 4);
   begin
      pragma Debug (O ("Unmarshall (ULong) : enter & end"));
      return Droopi.Types.Unsigned_Long (Octets (Octets'First)) * 256**3
        + Droopi.Types.Unsigned_Long (Octets (Octets'First + 1)) * 256**2
        + Droopi.Types.Unsigned_Long (Octets (Octets'First + 2)) * 256
        + Droopi.Types.Unsigned_Long (Octets (Octets'First + 3));
      --  Hard-coded expression will be optimized by the compiler
      --  as shifts+adds.
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Short
   is
   begin
      pragma Debug (O ("Unmarshall (Short) : enter & end"));
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Stream_Element_Array
   is
      Length : constant Droopi.Types.Unsigned_Long := Unmarshall (Buffer);
      E : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
   begin
      for I in E'Range loop
         E (I) := Stream_Element
           (Droopi.Types.Octet'(Unmarshall (Buffer)));
      end loop;

      return E;
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Long
   is
      Long_String : Types.String := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Long) : enter & end"));
      return Long'Value (To_Standard_String (Long_String));
--      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant Droopi.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (O ("Unmarshall (String): enter"));
      pragma Debug (O ("Unmarshall (String): length is " &
                    Droopi.Types.Unsigned_Long'Image (Length)));
      for I in Equiv'Range loop
         Equiv (I) := Character'Val
           (Droopi.Types.Char'Pos
            (Unmarshall (Buffer)));
      end loop;

      if Character'Val (Droopi.Types.Char'Pos (Unmarshall (Buffer)))
        /= ASCII.Nul
      then
         raise Unmarshall_Error;
      end if;

      pragma Debug (O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall;

--    function Unmarshall
--      (Buffer : access Buffer_Type)
--      return Types.String is
--    begin
--       return Droopi.Types.To_Droopi_String (Unmarshall (Buffer));
--    end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Any.TypeCode.Object
   is
      Nb : Droopi.Types.Unsigned_Long := Unmarshall (Buffer);
      Result : Droopi.Any.TypeCode.Object;
   begin
      --  XXX The hardcoded values in this case should be replaced
      --  by symbolic constants.
      pragma Debug (O ("Unmarshall (TypeCode) : enter"));
      case Nb is
         when 0 =>
            Result := Droopi.Any.TypeCode.TC_Null;
         when 1 =>
            Result := Droopi.Any.TypeCode.TC_Void;
         when 2 =>
            Result := Droopi.Any.TypeCode.TC_Short;
         when 3 =>
            Result := Droopi.Any.TypeCode.TC_Long;
         when 4 =>
            Result := Droopi.Any.TypeCode.TC_Unsigned_Short;
         when 5 =>
            Result := Droopi.Any.TypeCode.TC_Unsigned_Long;
         when 6 =>
            Result := Droopi.Any.TypeCode.TC_Float;
         when 7 =>
            Result := Droopi.Any.TypeCode.TC_Double;
         when 8 =>
            Result := Droopi.Any.TypeCode.TC_Boolean;
         when 9 =>
            Result := Droopi.Any.TypeCode.TC_Char;
         when 10 =>
            Result := Droopi.Any.TypeCode.TC_Octet;
         when 11 =>
            Result := Droopi.Any.TypeCode.TC_Any;
         when 12 =>
            pragma Debug (O ("Unmarshall (TypeCode) : "
                             & "dealing with a TypeCode"));
            Result := Droopi.Any.TypeCode.TC_TypeCode;
         when 13 =>
            Result := Droopi.Any.TypeCode.TC_Principal;
         when 14 =>
            Result := Droopi.Any.TypeCode.TC_Object;
            declare
               Id : Droopi.Types.String := Unmarshall (Buffer);
               Name : Droopi.Types.String := Unmarshall (Buffer);
            begin
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 15 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Struct;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : Droopi.Types.String;
--                Nb : Droopi.Types.Unsigned_Long;
--                Member_Type : Droopi.Any.TypeCode.Object;
--             begin
--                pragma Debug (O ("unmarshall (TypeCode) : dealing "
--                                 & "with a struct"));
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id   := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb   := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 16 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Union;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : Droopi.Types.String;
--                Nb, Default_Index : Droopi.Types.Unsigned_Long;
--                Discriminator_Type, Member_Type : Droopi.Any.TypeCode.Object;
--                Member_Label : Droopi.Any.Any;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Discriminator_Type := Unmarshall (Complex_Buffer);
--                Default_Index := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Discriminator_Type));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Default_Index));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Label := Get_Empty_Any (Discriminator_Type);
--                      Unmarshall_To_Any (Complex_Buffer, Member_Label);
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, Member_Label);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 17 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Enum;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer :  Buffer_Access := null;
--                Id, Name, Member_Name : Droopi.Types.String;
--                Nb : Droopi.Types.Unsigned_Long;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 18 =>
            Result := Droopi.Any.TypeCode.TC_String;
            declare
               Length : Droopi.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 19 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Sequence;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Length : Droopi.Types.Unsigned_Long;
--                Content_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Length := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Length));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 20 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Array;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Length : Droopi.Types.Unsigned_Long;
--                Content_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Length := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Length));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 21 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Alias;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : Droopi.Types.String;
--                Content_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 22 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Except;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : Droopi.Types.String;
--                Nb : Droopi.Types.Unsigned_Long;
--                Member_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 23 =>
            Result := Droopi.Any.TypeCode.TC_Long_Long;
         when 24 =>
            Result := Droopi.Any.TypeCode.TC_Unsigned_Long_Long;
         when 25 =>
            Result := Droopi.Any.TypeCode.TC_Long_Double;
         when 26 =>
            Result := Droopi.Any.TypeCode.TC_Wchar;
         when 27 =>
            Result := Droopi.Any.TypeCode.TC_Wide_String;
            declare
               Length : Droopi.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 28 =>
            Result := Droopi.Any.TypeCode.TC_Fixed;
            declare
               Fixed_Digits : Droopi.Types.Unsigned_Short;
               Fixed_Scale : Droopi.Types.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale := Unmarshall (Buffer);
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Digits));
               Droopi.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Scale));
            end;
         when 29 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Value;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : Droopi.Types.String;
--                Type_Modifier, Visibility : Droopi.Types.Short;
--                Nb : Droopi.Types.Unsigned_Long;
--                Concrete_Base_Type, Member_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Type_Modifier := Unmarshall (Complex_Buffer);
--                Concrete_Base_Type := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Type_Modifier));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Concrete_Base_Type));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      Visibility := Unmarshall (Complex_Buffer);
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Visibility));
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      Droopi.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 30 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Valuebox;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : Droopi.Types.String;
--                Content_Type : Droopi.Any.TypeCode.Object;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 31 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Native;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer :  Buffer_Access := null;
--                Id, Name : Droopi.Types.String;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--             end;
         when 32 =>
            raise Not_Implemented;
--             Result := Droopi.Any.TypeCode.TC_Abstract_Interface;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : Droopi.Types.String;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                Droopi.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--             end;
         when others =>
            raise Unmarshall_Error;
      end case;
      pragma Debug (O ("Unmarshall (TypeCode) : end"));
      return Result;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Droopi.Any.Any
   is
      Result : Droopi.Any.Any;
      Tc     : constant Droopi.Any.TypeCode.Object
        := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall (Any) : enter"));
      Result := Get_Empty_Any (Tc);
      Unmarshall_To_Any (Buffer, Result);
      pragma Debug (O ("Unmarshall (Any) : end"));
      return Result;
   end Unmarshall;

   -----------------
   -- MARSHALLING --
   -----------------

   procedure Marshall (Buffer : access Buffer_Type; Info_SRP : Split_SRP)
   is
      Current_Arg : Arg_Info_Ptr := Info_SRP.Args;
   begin
      Marshall (Buffer, Info_SRP.Method.all);
      --  ??? This Marshall seems to marshall a String
      --  should marshall a Char
      Marshall (Buffer, " ");
      Marshall (Buffer, Stream_Element_Array (Info_SRP.Oid.all));


      if Current_Arg /= null then
         Marshall (Buffer, "?");
      end if;

      while Current_Arg /= null loop
         Marshall (Buffer, Current_Arg.all.Name.all);
         Marshall (Buffer, "=");
         Marshall (Buffer, Current_Arg.all.Value.all);
         if Current_Arg.all.Next /= null then
            Marshall (Buffer, "&");
         end if;
         Current_Arg := Current_Arg.all.Next;
      end loop;

      Marshall (Buffer, ASCII.CR & ASCII.LF);
   end Marshall;

   --  Marshalling of a Boolean
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Boolean) is
   begin
      pragma Debug (O ("Marshall (Boolean) : enter"));
      Marshall (Buffer, Droopi.Types.Octet'(Droopi.Types.Boolean'Pos (Data)));
      pragma Debug (O ("Marshall (Boolean) : end"));
   end Marshall;

   --  Marshalling of a character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Char) is
   begin
      pragma Debug (O ("Marshall (Char) : enter"));
      Marshall (Buffer, Droopi.Types.Octet'(Droopi.Types.Char'Pos (Data)));
      pragma Debug (O ("Marshall (Char) : end"));
   end Marshall;

   --  Marshalling of a wide character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Wchar)
   is

   begin
      pragma Debug (O ("Marshall (WChar) : enter"));
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         Stream_Element_Array'
         (Stream_Element (Droopi.Types.Wchar'Pos (Data) / 256),
         Stream_Element (Droopi.Types.Wchar'Pos (Data) mod 256)), 2);
      pragma Debug (O ("Marshall (WChar) : end"));
   end Marshall;

   --  Marshalling of a Octet
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Octet) is
   begin
      pragma Debug (O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (Droopi.Types.Octet'(Data))), 1);
      pragma Debug (O ("Marshall (Octet) : end"));
   end Marshall;

   --  Marshalling of an unsigned short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Unsigned_Short)
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
      Data   :        Droopi.Types.Unsigned_Long)
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

   --  Marshalling of a short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Short) is
   begin
      pragma Debug (O ("Marshall (Short) : enter"));
      Marshall (Buffer, To_Unsigned_Short (Data));
      pragma Debug (O ("Marshall (Short) : end"));
   end Marshall;

   --  Marshalling of a long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Long) is
   begin
      pragma Debug (O ("Marshall (Long) : enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (O ("Marshall (Long) : end"));
   end Marshall;

   --  Marshalling of a standard string
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Standard.String) is
   begin
      pragma Debug (O ("Marshall (String) : enter"));

      Marshall (Buffer, Droopi.Types.Unsigned_Long'(Data'Length + 1));
      for I in Data'Range loop
         Marshall (Buffer, Droopi.Types.Char (Data (I)));
      end loop;
      Marshall (Buffer, Droopi.Types.Char (ASCII.Nul));

      pragma Debug (O ("Marshall (String) : end"));
   end Marshall;

   --  Marshalling of Droopi.Types.String
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.String) is
   begin
      pragma Debug (O ("Marshall (Droopi.Types.String) : enter"));
      Marshall (Buffer, Droopi.Types.To_Standard_String (Data));
      pragma Debug (O ("Marshall (Droopi.Types.String) : end"));
   end Marshall;

   --  Marshalling a sequence of octets
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Stream_Element_Array)
   is

   begin
      pragma Debug (O ("Marshall (Encapsulation) : enter"));
      Marshall (Buffer, Droopi.Types.Unsigned_Long (Data'Length));
      for I in Data'Range loop
         Marshall (Buffer, Droopi.Types.Octet (Data (I)));
      end loop;
      pragma Debug (O ("Marshall (Encapsulation) : end"));
   end Marshall;

   --  Marshalling of an Any
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.Any)
   is
   begin
      pragma Debug (O ("Marshall (Any) : enter"));
      Marshall (Buffer, Get_Type (Data));
      pragma Debug (O ("Marshall (Any) : type marshalled"));
      Marshall_From_Any (Buffer, Data);
      pragma Debug (O ("Marshall (Any) : end"));
   end Marshall;

   --  Puts the right TypeCode in the buffer
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Any.TypeCode.Object)
   is
--      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (O ("Marshall (Typecode) : enter"));
      pragma Debug (O ("Marshall (Typecode) : kind is " &
                       TCKind'Image (Droopi.Any.TypeCode.Kind (Data))));
      case Droopi.Any.TypeCode.Kind (Data) is
         when Tk_Null =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(0));
         when Tk_Void =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(1));
         when Tk_Short =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(2));
         when Tk_Long =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(3));
         when Tk_Ushort =>
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(4));
            raise Not_Implemented;
         when Tk_Ulong =>
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(5));
            raise Not_Implemented;
         when Tk_Float =>
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(6));
            raise Not_Implemented;
         when Tk_Double =>
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(7));
            raise Not_Implemented;
         when Tk_Boolean =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(8));
         when Tk_Char =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(9));
         when Tk_Octet =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(10));
         when Tk_Any =>
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(11));
         when Tk_TypeCode =>
            pragma Debug (O ("Marshall (TypeCode) : dealing with a TypeCode"));
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(12));
         when Tk_Principal =>
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(13));
            raise Not_Implemented;
         when Tk_Objref =>
--           pragma Debug (O ("Marshall (TypeCode) : dealing with an ObjRef"));
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(14));
--             pragma Debug (O ("Marshall (TypeCode) : it has "
--                              & Droopi.Types.Unsigned_Long'Image
--                              (Droopi.Any.TypeCode.Parameter_Count (Data))
--                              & " parameters"));
--             Marshall (Buffer, Droopi.Any.TypeCode.Id (Data));
--             Marshall (Buffer, Droopi.Any.TypeCode.Name (Data));
            raise Not_Implemented;
         when Tk_Struct =>
            raise Not_Implemented;
--           pragma Debug (O ("Marshall (TypeCode) : dealing with a struct"));
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(15));
--             Start_Encapsulation (Complex_Buffer);
--             pragma Debug (O ("Marshall (TypeCode) : marshalling the id"));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             pragma Debug (O ("Marshall (TypeCode) : marshalling the name"));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             declare
--                Nb : Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.TypeCode.Member_Count (Data);
--             begin
--                pragma Debug (O ("Marshall (TypeCode) : " &
--                                 "marshalling the members. Nb = "
--                                 & Droopi.Types.Unsigned_Long'Image (Nb)));
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      pragma Debug (O ("Marshall (TypeCode) : about "
--                                       & "to marshall a new  member"));
--                      Marshall (Complex_Buffer,
--                                Droopi.Any.TypeCode.Member_Name (Data, I));
--                      pragma Debug
--                        (O ("Marshall (TypeCode) : marshalling "
--                            & "the type ("
--                            & TCKind'Image
--                            (TypeCode.Kind
--                             (Droopi.Any.TypeCode.Member_Type (Data, I)))
--                            & ")"));
--                      Marshall (Complex_Buffer,
--                                Droopi.Any.TypeCode.Member_Type (Data, I));
--                      pragma Debug (O ("Marshall (TypeCode) : "
--                                       & "member marshalled"));
--                   end loop;
--                end if;
--             end;
--             pragma Debug (O ("Marshall : all members marshalled"));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Union =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(16));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Id (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Name (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Discriminator_Type (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Default_Index (Data));
--             declare
--                Nb : Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall_From_Any
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Label (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Name (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Type (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Enum =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(17));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             declare
--                Nb : Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall (Complex_Buffer,
--                                Droopi.Any.TypeCode.Member_Name (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_String =>
            pragma Debug (O ("marshall (typecode) : dealing with a string"));
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(18));
            pragma Debug (O ("marshall (typecode) : " &
                             "about to marshall length : " &
                             Droopi.Types.Unsigned_Long'Image
                             (Droopi.Any.TypeCode.Length (Data))));
            Marshall (Buffer, Droopi.Any.TypeCode.Length (Data));
            pragma Debug (O ("marshall (typecode) : length marshalled"));
         when Tk_Sequence =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(19));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Content_Type (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Length (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Array =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(20));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Content_Type (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Length (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Alias =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(21));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Content_Type (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Except =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(22));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             declare
--                Nb : Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall (Complex_Buffer,
--                                Droopi.Any.TypeCode.Member_Name (Data, I));
--                      Marshall (Complex_Buffer,
--                                Droopi.Any.TypeCode.Member_Type (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Longlong =>
            raise Not_Implemented;
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(23));
         when Tk_Ulonglong =>
            raise Not_Implemented;
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(24));
         when Tk_Longdouble =>
            raise Not_Implemented;
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(25));
         when Tk_Widechar =>
            raise Not_Implemented;
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(26));
         when Tk_Wstring =>
            raise Not_Implemented;
--            Marshall (Buffer, Droopi.Types.Unsigned_Long'(27));
--            Marshall (Buffer, Droopi.Any.TypeCode.Length (Data));
         when Tk_Fixed =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(28));
--             Marshall (Buffer, Droopi.Any.TypeCode.Fixed_Digits (Data));
--             Marshall (Buffer, Droopi.Any.TypeCode.Fixed_Scale (Data));
         when Tk_Value =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(29));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Id (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Name (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Type_Modifier (Data));
--             Marshall
--               (Complex_Buffer,
--                Droopi.Any.TypeCode.Concrete_Base_Type (Data));
--             declare
--                Nb : Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Name (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Type (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         Droopi.Any.TypeCode.Member_Visibility (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Valuebox =>
            raise Not_Implemented;
--          Marshall (Buffer, Droopi.Types.Unsigned_Long'(30));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Content_Type (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Native =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(31));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Abstract_Interface =>
            raise Not_Implemented;
--             Marshall (Buffer, Droopi.Types.Unsigned_Long'(32));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       Droopi.Any.TypeCode.Name (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
      end case;
      pragma Debug (O ("Marshall (Typecode) : end"));
   end Marshall;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   procedure Marshall_From_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any)
   is
   begin
      raise Not_Implemented;
   end Marshall_From_Any;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Any.Any)
   is
      Data_Type : Droopi.Any.TypeCode.Object
        := Droopi.Any.Get_Precise_Type (Data);
   begin
      pragma Debug (O ("Marshall_From_Any : enter"));
      --  pragma Debug
      --  (0 (Debug_Any(Droopi.Any.TypeCode.Kind (Data_Type)'Pos)))

      case Droopi.Any.TypeCode.Kind (Data_Type) is

         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Marshall_From_Any : dealing with void or null"));
            null;

         when Tk_Short =>
            pragma Debug (O ("Marshall_From_Any : dealing with a short"));
            Marshall (Buffer, Droopi.Types.Short'(From_Any (Data)));


         when Tk_Long =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long"));
            Marshall (Buffer, Droopi.Types.Long'(From_Any (Data)));

         when Tk_Ushort =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ushort"));
            Marshall (Buffer, Droopi.Types.Unsigned_Short'(From_Any (Data)));

         when Tk_Ulong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Ulong"));
            Marshall (Buffer, Droopi.Types.Unsigned_Long'(From_Any (Data)));

         when Tk_Float =>
            pragma Debug (O ("Marshall_From_Any : dealing with a float"));
--            Marshall (Buffer, Droopi.Types.Float'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Double =>
            pragma Debug (O ("Marshall_From_Any : dealing with a double"));
--            Marshall (Buffer, Droopi.Types.Double'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Boolean =>
            pragma Debug (O ("Marshall_From_Any : dealing with a boolean"));
            Marshall (Buffer, Droopi.Types.Boolean'(From_Any (Data)));

         when Tk_Char =>
            pragma Debug (O ("Marshall_From_Any : dealing with a char"));
            Marshall (Buffer, Droopi.Types.Char'(From_Any (Data)));

         when Tk_Octet =>
            pragma Debug (O ("Marshall_From_Any : dealing with an octet"));
            Marshall (Buffer, Droopi.Types.Octet'(From_Any (Data)));

         when Tk_Any =>
            pragma Debug (O ("Marshall_From_Any : dealing with an any"));
            Marshall (Buffer, Droopi.Any.Any'(From_Any (Data)));

         when Tk_TypeCode =>
            pragma Debug (O ("Marshall_From_Any : dealing with a typecode"));
--            Marshall (Buffer, Droopi.Any.TypeCode.Object'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Principal =>
            --  FIXME : to be done
            pragma Debug (O ("Marshall_From_Any : dealing with a principal"));
            raise Not_Implemented;

         when Tk_Objref =>
            pragma Debug (O ("Marshall_From_Any : dealing with an objRef"));
            --  Marshall (Buffer, Droopi.Types.Object.Helper.From_Any (Data));
            raise Not_Implemented;

         when Tk_Struct =>
--             declare
--                   Nb : constant Droopi.Types.Unsigned_Long
--                         := Droopi.Any.Get_Aggregate_Count (Data);
--                   Value : Droopi.Any.Any;
--             begin
--              pragma Debug (O ("Marshall_From_Any : dealing with a struct"));
--                for I in 0 .. Nb - 1 loop
--                   Value := Droopi.Any.Get_Aggregate_Element
--                  (Data, Droopi.Any.TypeCode.Member_Type (Data_Type, I), I);
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Not_Implemented;

         when Tk_Union =>
--             declare
--                Nb : Droopi.Types.Unsigned_Long;
--                Value, Label_Value : Droopi.Any.Any;
--             begin
--              pragma Debug (O ("Marshall_From_Any : dealing with an union"));
--                Label_Value := Get_Aggregate_Element
--                  (Data,
--                   Droopi.Any.TypeCode.Discriminator_Type (Data_Type),
--                   Droopi.Types.Unsigned_Long (0));
--                pragma Debug (O ("Marshall_From_Any : got the label"));
--                Marshall_From_Any (Buffer, Label_Value);
--                pragma Debug (O ("Marshall_From_Any : label marshalled"));
--                Nb := Droopi.Any.Get_Aggregate_Count (Data);
--                pragma Debug (O ("Marshall_From_Any : aggregate count = "
--                                 & Droopi.Types.Unsigned_Long'Image (Nb)));
--                if Nb > 1 then
--                   for I in 1 .. Nb - 1 loop
--                    pragma Debug (O ("Marshall_From_Any : inside loop, I = "
--                                       & Unsigned_Long'Image (I)));
--                      Value := Droopi.Any.Get_Aggregate_Element
--                        (Data,
--                         Droopi.Any.TypeCode.Member_Type_With_Label
--                         (Data_Type, Label_Value, I - 1),
--                         I);
--                      pragma Debug (O ("Marshall_From_Any : about "
--                                       & "to marshall from any"));
--                      Marshall_From_Any (Buffer, Value);
--                   end loop;
--                end if;
--             end;
            raise Not_Implemented;

         when Tk_Enum =>
            pragma Debug (O ("Marshall_From_Any : dealing with an enum"));
--             Marshall_From_Any
--               (Buffer,
--                Droopi.Any.Get_Aggregate_Element
--                (Data,
--                 Droopi.Any.TypeCode.TC_Unsigned_Long,
--                 Droopi.Types.Unsigned_Long (0)));
            raise Not_Implemented;

         when Tk_String =>
            pragma Debug (O ("Marshall_From_Any : dealing with a string"));
            Marshall (Buffer, Droopi.Types.String'(From_Any (Data)));

         when Tk_Sequence =>
--             declare
--                Nb : constant Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.Get_Aggregate_Count (Data);
--                Value : Droopi.Any.Any;
--             begin
--                pragma Debug (O
--                   ("Marshall_From_Any : dealing with a sequence"));
--                Value := Droopi.Any.Get_Aggregate_Element
--                  (Data,
--                   Droopi.Any.TypeCode.TC_Unsigned_Long,
--                   Droopi.Types.Unsigned_Long (0));
--                Marshall_From_Any (Buffer, Value);
--                if Nb /= 0 then
--                   for I in 1 .. Nb - 1 loop
--                      Value := Droopi.Any.Get_Aggregate_Element
--                        (Data,
--                         Droopi.Any.TypeCode.Content_Type (Data_Type),
--                         I);
--                      Marshall_From_Any (Buffer, Value);
--                   end loop;
--                end if;
--             end;
            raise Not_Implemented;

         when Tk_Array =>
--             declare
--                Nb : constant Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.Get_Aggregate_Count (Data);
--                Value : Droopi.Any.Any;
--                Content_True_Type : Droopi.Any.TypeCode.Object :=
--                  Droopi.Any.TypeCode.Content_Type (Data_Type);
--             begin
--              pragma Debug (O ("Marshall_From_Any : dealing with an array"));

--                while Droopi.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
--                loop
--                   Content_True_Type :=
--                     Droopi.Any.TypeCode.Content_Type (Content_True_Type);
--                end loop;

--                for I in 0 .. Nb - 1 loop
--                   Value := Droopi.Any.Get_Aggregate_Element
--                     (Data,
--                      Content_True_Type,
--                      I);
--                   pragma Debug (O ("Marshall_From_Any : value kind is "
--                                    & Droopi.Any.TCKind'Image
--                                    (Droopi.Any.TypeCode.Kind
--                                     (Droopi.Any.Get_Type (Value)))));
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Not_Implemented;

         when Tk_Alias =>
            --  we should never reach this point
            pragma Debug (O ("Marshall_From_Any : dealing with an alias"));
            pragma Assert (False);
            raise Program_Error;

         when Tk_Except =>
--             declare
--                Nb : constant Droopi.Types.Unsigned_Long :=
--                  Droopi.Any.Get_Aggregate_Count (Data);
--                Value : Droopi.Any.Any;
--             begin
--                pragma Debug
--                  (O ("Marshall_From_Any : dealing with an exception"));
--                for I in 0 .. Nb - 1 loop
--                   Value := Droopi.Any.Get_Aggregate_Element
--                     (Data,
--                      Droopi.Any.TypeCode.Member_Type (Data_Type, I),
--                      I);
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Not_Implemented;

         when Tk_Longlong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a long long"));
--            Marshall (Buffer, Droopi.Types.Long_Long'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Ulonglong =>
            pragma Debug (O ("Marshall_From_Any : dealing with a ULongLong"));
--             Marshall
--               (Buffer,
--                Droopi.Types.Unsigned_Long_Long'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Longdouble =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a long double"));
--            Marshall (Buffer, Droopi.Types.Long_Double'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Widechar =>
            pragma Debug (O ("Marshall_From_Any : dealing with a Wchar"));
--            Marshall (Buffer, Droopi.Types.Wchar'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Wstring =>
            pragma Debug
              (O ("Marshall_From_Any : dealing with a wide string"));
--            Marshall (Buffer, Droopi.Types.Wide_String'(From_Any (Data)));
            raise Not_Implemented;

         when Tk_Fixed =>
            --  declare
            --   Digit,Scale: Droopi.Any.Any;
            --  begin
            --   pragma Debug (O ("Marshall_From_Any : dealing with a fixed"));
            --   Digit:=Get_Aggregate_Element
            --           (Data,
            --            Droopi.Any.TypeCode.TC_Unsigned_Long,
            --            Droopi.Any.TypeCode.Fixed_Digits(Data_Type),
            --            Droopi.Types.Unsigned_Long(0));
            --   Marshall_From_Any(Buffer,Digit);
            --   Scale:=Get_Aggregate_Element
            --           (Data,
            --            Droopi.Any.TypeCode.Fixed_Scale(Data_Type),
            --            Droopi.Types.Unsigned_Long(1));
            --   Marshall_From_Any(Buffer,Scale);
            --   end;
            raise Not_Implemented;

         when Tk_Value =>
            --  declare
            --     Nb: Droopi.Types.Unsigned_Long;
            --     Value_Modifier, Value_TypeCode,
            --         Value_Visibility : Droopi.Any.Any;
            --  begin
            --    pragma Debug
            --        (O ("Marshall_From_Any : dealing with a value"));
            --    Value_Modifier:= Droopi.Any.Get_Aggregate_Element
            --         (Data,
            --          Droopi.Any.TypeCode.Type_Modifier(Data_Type),
            --          Droopi.Types.Unsigned_Long(0));
            --  pragma Debug (0 ("Marshall_From_Any: got the value_modifier"));
            --    Marshall_From_Any(Buffer,Val_Modifier);
            --    Nb := Droopi.Any.Get_Aggregate_Count(Data);
            --    if Nb>1 then
            --     while I<Nb-1 loop

            --       Value_Value:= Droopi.Any.Get_Aggregate_Element
            --         (Data,
            --          Droopi.Any.TypeCode.Member_Type (Data_Type, I),
            --          I);
            --       I:=I+1;
            --       Value_Visibility:=  Droopi.Any.Get_Aggregate_Element
            --         (Data,
            --          Droopi.Any.TypeCode.Member_Visibility(Data_Type, I),
            --          I);
            --       Marshall_From_Any(Buffer, Value);
            --       I:=I+2;

            --     end loop;
            --    end if;
            --   end;
            raise Not_Implemented;

         when Tk_Valuebox =>
            pragma Debug (O ("Marshall_From_Any : dealing with a valuebox"));
--             Marshall_From_Any (Buffer, Droopi.Any.Get_Aggregate_Element
--                  (Data, Droopi.Any.TypeCode.Member_Type (Data_Type,
--                  Droopi.Types.Unsigned_Long (0)),
--                  Droopi.Types.Unsigned_Long (0)));
            raise Not_Implemented;

         when Tk_Native =>
            pragma Debug (O ("Marshall_From_Any : dealing with a native"));
            --  FIXME : to be done
            --  pragma Debug (O ("Marshall_From_Any : dealing with a native"));

            raise Not_Implemented;

         when Tk_Abstract_Interface =>
            pragma Debug (O
                 ("Marshall_From_Any : dealing with an abstract interface"));
            --  FIXME : to be done
            --  pragma Debug (O ("Marshall_From_Any : dealing with "
            --                 & "an abstract interface"));
            raise Not_Implemented;
      end case;
      pragma Debug (O ("Marshall_From_Any : end"));
   end Marshall_From_Any;

--    procedure Marshall_From_Any
--      (R      : Rep_SRP;
--       Buffer : access Buffers.Buffer_Type;
--       Data   : Any.Any)
--    is
--       URL : Types.String := Any.From_Any (Data);
--       Coded_URL : String_Ptr;
--    begin
--       --  ??? For now we don't use the Base64 coding
--       --  Coded_URL :=
--       --       new String'(Base64_Encode (CORBA.To_Standard_String (URL)));
--       Coded_URL := new String'(Encode_URL (Types.To_Standard_String (URL)));
--       pragma Debug (O ("Coded URL : " & Coded_URL.all));

--       for I in Coded_URL.all'Range loop
--          Align_Marshall_Copy
--            (Buffer, Stream_Element_Array'
--             (1 => Stream_Element (Character'Pos (Coded_URL.all (I)))));
--       end loop;

--    end Marshall_From_Any;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   procedure Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any)
   is
      Encoded_URL : String_Ptr;
      Decoded_URL : String_Ptr;
   begin
      raise Not_Implemented;
--       Encoded_URL := new Types.String'(Unmarshall_String (R, Buffer));
--       Decoded_URL := new Types.String'(Decode_URL (Encoded_URL.all));
--       Data := Any.To_Any
--         (Types.To_Droopi_String (Decode_URL (Decoded_URL.all)));
   end Unmarshall_To_Any;

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out Droopi.Any.Any)
   is
      Tc       : constant Droopi.Any.TypeCode.Object
        := Get_Precise_Type (Result);
--       Is_Empty : constant Boolean
--         := Droopi.Any.Is_Empty (Result);

   begin
      pragma Debug (O ("Unmarshall_To_Any : enter"));
      pragma Debug
        (O ("Unmarshall_To_Any : Any_Type is " &
            Droopi.Any.TCKind'Image (TypeCode.Kind (Tc))));

      case Any.TypeCode.Kind (Tc) is
         when Tk_Null | Tk_Void =>
            null;
         when Tk_Short =>
            declare
               S : Short := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any : dealing with a short"));
               pragma Debug (O ("Unmarshall_To_Any : its value is "
                                & Droopi.Types.Short'Image (S)));
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
            raise Not_Implemented;
--             declare
--                Us : Unsigned_Short := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Us);
--             end;
         when Tk_Ulong =>
            raise Not_Implemented;
--             declare
--                Ul : Unsigned_Long := Unmarshall (Buffer);
--             begin
--              pragma Debug (O ("Unmarshall_To_Any : dealing with an Ulong"));
--                Set_Any_Value (Result, Ul);
--             end;
         when Tk_Float =>
            raise Not_Implemented;
--             declare
--                F : Droopi.Types.Float := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, F);
--             end;
         when Tk_Double =>
            raise Not_Implemented;
--             declare
--                D : Double := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, D);
--             end;
         when Tk_Boolean =>
            declare
               B : Droopi.Types.Boolean := Unmarshall (Buffer);
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
               O : Droopi.Types.Octet := Unmarshall (Buffer);
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
            raise Not_Implemented;
         when Tk_Objref =>
            --  declare
            --     O : Droopi.Types.Object.Ref := Unmarshall (Buffer);
            --  begin
            --     Droopi.Types.Object.Helper.Set_Any_Value (Result, O);
            --  end;
            raise Not_Implemented;
         when Tk_Struct =>
            raise Not_Implemented;
--             declare
--                Nb : Unsigned_Long :=
--                  TypeCode.Member_Count (Tc);
--                Arg : Droopi.Any.Any;
--             begin
--              pragma Debug (O ("unmarshall_to_any : dealing with a struct"));
--                Droopi.Any.Set_Any_Aggregate_Value (Result);
--                pragma Debug (O ("unmarshall_to_any : about to "
--                                 & "unmarshall parameters"));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                    pragma Debug (O ("unmarshall_to_any : get the element"));
--                      if Is_Empty then
--                         Arg := Get_Empty_Any (TypeCode.Member_Type (Tc, I));
--                      else
--                         Arg := Get_Aggregate_Element
--                           (Result,
--                            TypeCode.Member_Type (Tc, I),
--                            I);
--                      end if;
--                      pragma Debug (O ("unmarshall_to_any : about to "
--                                       & "unmarshall a parameter"));
--                      Unmarshall_To_Any (Buffer,
--                                         Arg);
--                      if Is_Empty then
--                         Add_Aggregate_Element (Result, Arg);
--                      end if;
--                   end loop;
--                end if;
--             end;
         when Tk_Union =>
            raise Not_Implemented;
--             declare
--                Nb : Unsigned_Long;
--                Label, Arg : Droopi.Any.Any;
--             begin
--              pragma Debug (O ("Unmarshall_To_Any : dealing with an union"));
--                Set_Any_Aggregate_Value (Result);
--                if Is_Empty then
--                   Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
--                else
--                   Label := Get_Aggregate_Element
--                     (Result,
--                      TypeCode.Discriminator_Type (Tc),
--                      Droopi.Types.Unsigned_Long (0));
--                end if;
--                Unmarshall_To_Any (Buffer, Label);
--                if Is_Empty then
--                   pragma Debug (O ("Unmarshall_To_Any : about to call "
--                                    & "add_aggregate"));
--                   Add_Aggregate_Element (Result, Label);
--                end if;
--                pragma Debug (O ("Unmarshall_To_Any : about to call "
--                                 & "member_count_with_label"));
--              Nb := Droopi.Any.TypeCode.Member_Count_With_Label (Tc, Label);
--                if Nb > 0 then
--                   for I in 0 .. Nb - 1 loop
--                      if Is_Empty then
--                         Arg := Get_Empty_Any
--                           (TypeCode.Member_Type_With_Label (Tc, Label, I));
--                      else
--                         Arg := Get_Aggregate_Element
--                           (Result,
--                            TypeCode.Member_Type_With_Label (Tc, Label, I),
--                            I + 1);
--                      end if;
--                      Unmarshall_To_Any (Buffer, Arg);
--                      if Is_Empty then
--                         Add_Aggregate_Element (Result, Arg);
--                      end if;
--                   end loop;
--                end if;
--             end;
         when Tk_Enum =>
            raise Not_Implemented;
--          declare
--                Arg : Droopi.Any.Any;
--             begin
--                Set_Any_Aggregate_Value (Result);
--                if Is_Empty then
--                   Arg := Get_Empty_Any (TC_Unsigned_Long);
--                else
--                   Arg := Get_Aggregate_Element
--                     (Result,
--                      TC_Unsigned_Long,
--                      Droopi.Types.Unsigned_Long (0));
--                end if;
--                Unmarshall_To_Any (Buffer, Arg);
--                if Is_Empty then
--                   Add_Aggregate_Element (Result, Arg);
--                end if;
--             end;
         when Tk_String =>
            declare
               S : Droopi.Types.String := Unmarshall (Buffer);
            begin
               Set_Any_Value (Result, S);
            end;
         when Tk_Sequence =>
            raise Not_Implemented;
--             declare
--                Nb : Unsigned_Long := Unmarshall (Buffer);
--                Max_Nb : Unsigned_Long := TypeCode.Length (Tc);
--                Arg : Droopi.Any.Any;
--             begin
--                pragma Debug
--                  (O ("Unmarshall_To_Any : dealing with a sequence"));
--                if Max_Nb > 0 and then Nb > Max_Nb then
--                   Droopi.CORBA_P.Exceptions.Raise_Marshal;
--                end if;
--                Set_Any_Aggregate_Value (Result);
--                pragma Debug (O ("Unmarshall_To_Any : aggregate value set"));
--                if Is_Empty then
--                   pragma Debug (O ("Unmarshall_To_Any : about to call"
--                                    & " add_aggregate_element"));
--                   Add_Aggregate_Element (Result, To_Any (Nb));
--                else
--                   Arg := Get_Aggregate_Element
--                     (Result,
--                      TC_Unsigned_Long,
--                      Droopi.Types.Unsigned_Long (0));
--                   Set_Any_Value (Arg, Nb);
--                end if;
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      if Is_Empty then
--                         Arg := Get_Empty_Any (TypeCode.Content_Type (Tc));
--                      else
--                         Arg := Get_Aggregate_Element
--                           (Result, TypeCode.Content_Type (Tc), I + 1);
--                      end if;
--                      Unmarshall_To_Any (Buffer, Arg);
--                      if Is_Empty then
--                         Add_Aggregate_Element (Result, Arg);
--                      end if;
--                   end loop;
--                end if;
--             end;
         when Tk_Array =>
            raise Not_Implemented;
--             declare
--                Nb : Unsigned_Long := TypeCode.Length (Tc);
--                Content_True_Type : Droopi.Any.TypeCode.Object :=
--                  TypeCode.Content_Type (Tc);
--                Arg : Droopi.Any.Any;
--             begin
--                while Droopi.Any.TypeCode.Kind (Content_True_Type) = Tk_Array
--                loop
--                   Nb := Nb * TypeCode.Length (Content_True_Type);
--                   Content_True_Type :=
--                     TypeCode.Content_Type (Content_True_Type);
--                end loop;

--                Set_Any_Aggregate_Value (Result);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      if Is_Empty then
--                         Arg := Get_Empty_Any (Content_True_Type);
--                      else
--                         Arg := Get_Aggregate_Element
--                           (Result, Content_True_Type, I);
--                      end if;
--                      Unmarshall_To_Any (Buffer, Arg);
--                      if Is_Empty then
--                         Add_Aggregate_Element (Result, Arg);
--                      end if;
--                   end loop;
--                end if;
--             end;
         when Tk_Alias =>
            --  we should never reach this point
            raise Program_Error;
         when Tk_Except =>
            raise Not_Implemented;
--             declare
--                Nb : Unsigned_Long :=
--                  TypeCode.Member_Count (Tc);
--                Arg : Droopi.Any.Any;
--             begin
--                Set_Any_Aggregate_Value (Result);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      if Is_Empty then
--                         Arg := Get_Empty_Any (TypeCode.Member_Type (Tc, I));
--                      else
--                         Arg := Get_Aggregate_Element
--                           (Result,
--                            TypeCode.Member_Type (Tc, I),
--                            I);
--                      end if;
--                      Unmarshall_To_Any (Buffer,
--                                         Arg);
--                      if Is_Empty then
--                         Add_Aggregate_Element (Result, Arg);
--                      end if;
--                   end loop;
--                end if;
--             end;
         when Tk_Longlong =>
            raise Not_Implemented;
--             declare
--                Ll : Long_Long := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ll);
--             end;
         when Tk_Ulonglong =>
            raise Not_Implemented;
--             declare
--                Ull : Unsigned_Long_Long := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ull);
--             end;
         when Tk_Longdouble =>
            raise Not_Implemented;
--             declare
--                Ld : Long_Double := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ld);
--             end;
         when Tk_Widechar =>
            raise Not_Implemented;
--             declare
--                Wc : Wchar := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Wc);
--             end;
         when Tk_Wstring =>
            raise Not_Implemented;
--             declare
--                Ws : Droopi.Types.Wide_String := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ws);
--             end;
         when Tk_Fixed =>
            --  FIXME : to be done
            --  declare
            --   Arg1,Arg2:Droopi.Any.Any;
            --  begin
            --    pragma Debug(0 ("unmarshall_to_any: dealing with a fixed"));
            --    Set_Any_Aggregate_Value(Result);
            --    if Is_Empty then
            --      Arg1:= Get_Empty_Any(TypeCode.Fixed_Digits(Tc));
            --    else
            --      Arg1:= Get_Aggregate_Element
            --             (Result,
            --              TypeCode.Fixed_Digits(Tc),
            --              Droopi.Types.Unsigned_Long(0));
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
            --              Droopi.Types.Unsigned_Long(0));
            --    end if;
            --    Unmarshall_To_Any(Buffer, Arg2);
            --    if Is_Empty then
            --      Add_Aggregate_Element(Result,Arg2);
            --    end if;
            --   end;
            raise Not_Implemented;


         when Tk_Value =>

            --  declare
            --   Val_Modifier,Arg: Droopi.Any.Any;
            --   Nb: Droopi.Types.Unsigned_Long:=
            --          TypeCode.Member_Count(Tc);

            --  begin
            --   Set_Any_Aggregate_Value(Result);
            --   if Is_Empty then
            --     Val_Modifier:= Get_Empty_Any(TypeCode.Type_Modifier(Tc));
            --   else
            --     Val_Modifier:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Discriminator_Type(Tc),
            --                Droopi.Types.Unsigned_Long(0));
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
            --              (Tc,Droopi.Types.Unsigned_Long(0)));
            --     else
            --       Arg:= Droopi.Any.Get_Aggregate_Element
            --                 (Result,
            --                  Droopi.Any.TypeCode.Member_Type(Tc,
            --                  Droopi.Types.Unsigned_Long(0)));
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

   -------------------
   -- Marshall_Char --
   -------------------

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'
         (1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   ---------------------
   -- Unmarshall_Char --
   ---------------------

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character
   is
      A : constant Stream_Element_Array
        := Align_Unmarshall_Copy (B, 1);
   begin
      return Character'Val (A (A'First));
   end Unmarshall_Char;

   ---------------------
   -- Marshall_String --
   ---------------------

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

   -----------------------
   -- Unmarshall_String --
   -----------------------

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

   --  Specific to SRP
   function Unmarshall (Buffer : access Buffer_Type)
                       return Types.String
   is
      Result : Types.String;
      C : Types.Char;
   begin
      pragma Debug (O ("Marshall (Droopi.Types.String) : enter"));

      C := Unmarshall (Buffer);
      while C /= ASCII.CR and C /= ASCII.nul loop
         Append (Result, C);
         C := Unmarshall (Buffer);
      end loop;
      if C = ASCII.CR then
         C := Unmarshall (Buffer);
         pragma Assert (C = ASCII.LF);
      end if;

      pragma Debug (O ("Marshall (Droopi.Types.String) : end"));

      return Result;
   end Unmarshall;


   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   function Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type) return Any.Any
   is
      Data : Any.Any;
   begin
      Unmarshall_To_Any (R, Buffer, Data);
      return Data;
   end Unmarshall_To_Any;

   -------------------------
   -- Marshall_From_Split --
   -------------------------
   --  Temporary procedure. Should be replaces by Marshall_From_Any when
   --  we will be able to [un]marshall Split_SRP [from] to Any
   procedure Marshall_From_Split_SRP
     (R       : Rep_SRP;
      Buffer  : access Buffers.Buffer_Type;
      SRP_Info : Split_SRP)
   is
      use Any;

      Local_SRP_Info : Split_SRP := SRP_Info;
      Coded_URL : String_Ptr;
   begin
--       Encode_URL (Local_SRP_Info);
--       Coded_URL :=
--         new Types.String'((From_Any (Join (Local_SRP_Info))));
      Coded_URL := new Types.String'(Encode_URL (Local_SRP_Info));
      pragma Debug (O ("Coded URL : " & To_Standard_String (Coded_URL.all)));

      for I in To_Standard_String (Coded_URL.all)'Range loop
         Align_Marshall_Copy
           (Buffer, Stream_Element_Array'
            (1 => Stream_Element
               (Character'Pos (To_Standard_String (Coded_URL.all) (I)))));
      end loop;

   end Marshall_From_Split_SRP;

end Droopi.Representations.SRP;
