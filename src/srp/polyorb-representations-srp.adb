------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . S R P           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Representations.CDR.Common;
--  ??? reusing elementary types' marshalling/unmarshalling functions
with PolyORB.Utils;
with PolyORB.Utils.Buffers; use PolyORB.Utils.Buffers;

with Interfaces;

package body PolyORB.Representations.SRP is

   use Ada;
   use Ada.Strings.Unbounded;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.srp");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------------
   -- Decode_URL --
   ----------------

   function Decode_URL (Str : String) return String
     renames PolyORB.Utils.URI_Decode;

   ------------------------------------------
   -- Part taken from AWS (Ada Web Server) --
   ------------------------------------------

   -------------------
   -- Base64_Encode --
   -------------------

   function Base64_Encode (Data : Streams.Stream_Element_Array)
                          return String
   is
      function Base64 (E : Stream_Element) return Character;
      --  returns the base64 character given a number

      function Shift_Left
        (Value : Stream_Element; Amount : Natural)
        return Stream_Element;
      function Shift_Right
        (Value : Stream_Element; Amount : Natural)
        return Stream_Element;
      pragma Inline (Shift_Left);
      pragma Inline (Shift_Right);

      function Shift_Left
        (Value : Stream_Element; Amount : Natural)
        return Stream_Element is
      begin
         return Stream_Element
           (Interfaces.Shift_Left
            (Interfaces.Unsigned_8 (Value), Amount));
      end Shift_Left;

      function Shift_Right
        (Value : Stream_Element; Amount : Natural)
        return Stream_Element is
      begin
         return Stream_Element
           (Interfaces.Shift_Right
            (Interfaces.Unsigned_8 (Value), Amount));
      end Shift_Right;

      Result : Unbounded_String;
      Length : Natural := 0;
      State  : Positive range 1 .. 3 := 1;
      E, Old : Stream_Element := 0;

      function Base64 (E : Stream_Element) return Character is
         V : constant Natural := Natural (E);
      begin
         case V is
            when 0 .. 25 =>
               return Character'Val (V + Character'Pos ('A'));
            when 26 .. 51 =>
               return Character'Val (V - 26 + Character'Pos ('a'));
            when 52 .. 61 =>
               return Character'Val (V - 52 + Character'Pos ('0'));
            when 62 =>
               return '+';
            when others =>
               return '/';
         end case;
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

   function Base64_Encode (Data : String) return String is
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

   function Base64_Decode (B64_Data : String)
                          return Streams.Stream_Element_Array
   is
      use type Interfaces.Unsigned_32;

      function Base64 (C : Character) return Interfaces.Unsigned_32;
      --  returns the base64 stream element given a character

      function Shift_Left
                 (Value : Interfaces.Unsigned_32; Amount : Natural)
                 return Interfaces.Unsigned_32 renames Interfaces.Shift_Left;

      function Shift_Right
                 (Value : Interfaces.Unsigned_32; Amount : Natural)
                 return Interfaces.Unsigned_32
        renames Interfaces.Shift_Right;

      Result : Stream_Element_Array
        (Stream_Element_Offset range 1 .. B64_Data'Length);
      R      : Stream_Element_Offset := 1;

      Group  : Interfaces.Unsigned_32 := 0;
      J      : Integer := 18;

      Pad    : Stream_Element_Offset := 0;

      function Base64 (C : Character) return Interfaces.Unsigned_32 is
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

   ----------------
   -- Encode_URL --
   ----------------

   function Encode_URL (SRP_Info : Split_SRP) return Types.String
   is
      use PolyORB.Objects;
      Result : Types.String;
      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
   begin
      Append (Result, SRP_Info.Method.all & " " &
              Objects.Oid_To_Hex_String (SRP_Info.Oid.all));
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
--      Current_Arg : Arg_Info_Ptr := SRP_Info.Args;
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

   function Encode_String
     (Str : String; Also_Escape : String := "/") return String
     renames Utils.URI_Encode;

   -------------------------------------------
   -- Conversions between PolyORB signed and --
   -- unsigned integer types.               --
   -------------------------------------------

   function To_Long_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Long_Long, PolyORB.Types.Long_Long);
   pragma Warnings (Off);
   pragma Unreferenced (To_Long_Long);
   pragma Warnings (On);
   function To_Unsigned_Long_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Long_Long, PolyORB.Types.Unsigned_Long_Long);
   pragma Warnings (Off);
   pragma Unreferenced (To_Unsigned_Long_Long);
   pragma Warnings (On);
   function To_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Long, PolyORB.Types.Long);
   pragma Warnings (Off);
   pragma Unreferenced (To_Long);
   pragma Warnings (On);
   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Long, PolyORB.Types.Unsigned_Long);
   function To_Short is
      new Ada.Unchecked_Conversion
        (PolyORB.Types.Unsigned_Short, PolyORB.Types.Short);
   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion
     (PolyORB.Types.Short, PolyORB.Types.Unsigned_Short);

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      NV     : in out NamedValue) is
   begin
      pragma Debug (C, O ("Unmarshall (NamedValue): enter"));
      pragma Debug (C, O ("Unmarshall (NamedValue): is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      Unmarshall_To_Any (Buffer, Get_Container (NV.Argument).all);
      pragma Debug (C, O ("Unmarshall (NamedValue): is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      pragma Debug (C, O ("Unmarshall (NamedValue): end"));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Boolean is
   begin
      pragma Debug (C, O ("Unmarshall (Boolean): enter & end"));
      return PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Octet
   is
      Result : Stream_Element_Array (1 .. 1);
   begin
      Align_Unmarshall_Copy (Buffer, Align_1, Result);
      pragma Debug (C, O ("Unmarshall (Octet): enter & end"));
      return PolyORB.Types.Octet (Result (Result'First));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Short
     renames CDR.Common.Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Char is
   begin
      pragma Debug (C, O ("Unmarshall (Char): enter & end"));
      return PolyORB.Types.Char'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Long
     renames CDR.Common.Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Short
   is
   begin
      pragma Debug (C, O ("Unmarshall (Short): enter & end"));
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Stream_Element_Array
   is
      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
      E : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
   begin
      for I in E'Range loop
         E (I) := Stream_Element
           (PolyORB.Types.Octet'(Unmarshall (Buffer)));
      end loop;

      return E;
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long
   is
      Long_String : constant Types.String := Unmarshall (Buffer);
   begin
      pragma Debug (C, O ("Unmarshall (Long): enter & end"));
      return Long'Value (To_Standard_String (Long_String));
--      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (C, O ("Unmarshall (String): enter"));
      pragma Debug (C, O ("Unmarshall (String): length is " &
                    PolyORB.Types.Unsigned_Long'Image (Length)));
      for I in Equiv'Range loop
         Equiv (I) := Character'Val
           (PolyORB.Types.Char'Pos
            (Unmarshall (Buffer)));
      end loop;

      if Character'Val (PolyORB.Types.Char'Pos (Unmarshall (Buffer)))
        /= ASCII.NUL
      then
         raise Unmarshall_Error;
      end if;

      pragma Debug (C, O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall;

--    function Unmarshall
--      (Buffer : access Buffer_Type)
--      return Types.String is
--    begin
--       return PolyORB.Types.To_PolyORB_String (Unmarshall (Buffer));
--    end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.TypeCode.Local_Ref
   is
      Nb : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
      Result : PolyORB.Any.TypeCode.Local_Ref;
   begin
      --  XXX The hardcoded values in this case should be replaced
      --  by symbolic constants.
      pragma Debug (C, O ("Unmarshall (TypeCode): enter"));
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
            Result := PolyORB.Any.TypeCode.TC_TypeCode;
         when 13 =>
            Result := PolyORB.Any.TypeCode.TC_Principal;
         when 14 =>
            Result := PolyORB.Any.TypeCode.TC_Object;
            declare
               Id : constant PolyORB.Types.String := Unmarshall (Buffer);
               Name : constant PolyORB.Types.String := Unmarshall (Buffer);
            begin
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 15 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Struct;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : PolyORB.Types.String;
--                Nb : PolyORB.Types.Unsigned_Long;
--                Member_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                pragma Debug (C, O ("unmarshall (TypeCode): dealing "
--                                 & "with a struct"));
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id   := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb   := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 16 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Union;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : PolyORB.Types.String;
--                Nb, Default_Index : PolyORB.Types.Unsigned_Long;
--                Discriminator_Type,
--                  Member_Type : PolyORB.Any.TypeCode.Local_Ref;
--                Member_Label : PolyORB.Any.Any;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Discriminator_Type := Unmarshall (Complex_Buffer);
--                Default_Index := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Discriminator_Type));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Default_Index));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Label := Get_Empty_Any (Discriminator_Type);
--                      Unmarshall_To_Any (Complex_Buffer, Member_Label);
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, Member_Label);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 17 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Enum;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer :  Buffer_Access := null;
--                Id, Name, Member_Name : PolyORB.Types.String;
--                Nb : PolyORB.Types.Unsigned_Long;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
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
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Sequence;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Length : PolyORB.Types.Unsigned_Long;
--                Content_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Length := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Length));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 20 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Array;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Length : PolyORB.Types.Unsigned_Long;
--                Content_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                Length := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Length));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 21 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Alias;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : PolyORB.Types.String;
--                Content_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 22 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Except;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : PolyORB.Types.String;
--                Nb : PolyORB.Types.Unsigned_Long;
--                Member_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
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
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Value;
--             declare
--                Complex_Encap : aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name, Member_Name : PolyORB.Types.String;
--                Type_Modifier, Visibility : PolyORB.Types.Short;
--                Nb : PolyORB.Types.Unsigned_Long;
--                Concrete_Base_Type,
--                  Member_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Type_Modifier := Unmarshall (Complex_Buffer);
--                Concrete_Base_Type := Unmarshall (Complex_Buffer);
--                Nb := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Type_Modifier));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Concrete_Base_Type));
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Member_Name := Unmarshall (Complex_Buffer);
--                      Member_Type := Unmarshall (Complex_Buffer);
--                      Visibility := Unmarshall (Complex_Buffer);
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Visibility));
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Type));
--                      PolyORB.Any.TypeCode.Add_Parameter
--                        (Result, To_Any (Member_Name));
--                   end loop;
--                end if;
--             end;
         when 30 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Valuebox;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : PolyORB.Types.String;
--                Content_Type : PolyORB.Any.TypeCode.Local_Ref;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                Content_Type := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Content_Type));
--             end;
         when 31 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Native;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer :  Buffer_Access := null;
--                Id, Name : PolyORB.Types.String;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--             end;
         when 32 =>
            raise Program_Error;
--             Result := PolyORB.Any.TypeCode.TC_Abstract_Interface;
--             declare
--                Complex_Encap :  aliased Encapsulation
--                  := Unmarshall (Buffer);
--                Complex_Buffer : Buffer_Access := null;
--                Id, Name : PolyORB.Types.String;
--             begin
--                Decapsulate (Complex_Encap'Access, Complex_Buffer);
--                Id := Unmarshall (Complex_Buffer);
--                Name := Unmarshall (Complex_Buffer);
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Name));
--                PolyORB.Any.TypeCode.Add_Parameter
--                  (Result, To_Any (Id));
--             end;
         when others =>
            raise Unmarshall_Error;
      end case;
      pragma Debug (C, O ("Unmarshall (TypeCode): end"));
      return Result;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any;
      Tc     : constant PolyORB.Any.TypeCode.Local_Ref
        := Unmarshall (Buffer);
   begin
      pragma Debug (C, O ("Unmarshall (Any): enter"));
      Result := Get_Empty_Any (Tc);
      Unmarshall_To_Any (Buffer, Get_Container (Result).all);
      pragma Debug (C, O ("Unmarshall (Any): end"));
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
      Data   :        PolyORB.Types.Boolean) is
   begin
      pragma Debug (C, O ("Marshall (Boolean): enter"));
      Marshall
        (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Boolean'Pos (Data)));
      pragma Debug (C, O ("Marshall (Boolean): end"));
   end Marshall;

   --  Marshalling of a character
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Char) is
   begin
      pragma Debug (C, O ("Marshall (Char): enter"));
      Marshall (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Char'Pos (Data)));
      pragma Debug (C, O ("Marshall (Char): end"));
   end Marshall;

   --  Marshalling of a Octet
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Octet) is
   begin
      pragma Debug (C, O ("Marshall (Octet): enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (PolyORB.Types.Octet'(Data))), Align_1);
      pragma Debug (C, O ("Marshall (Octet): end"));
   end Marshall;

   --  Marshalling of an unsigned short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Unsigned_Short)
      renames CDR.Common.Marshall;

   --  Marshalling of an unsigned long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Unsigned_Long)
      renames CDR.Common.Marshall;

   --  Marshalling of a short
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Short) is
   begin
      pragma Debug (C, O ("Marshall (Short): enter"));
      Marshall (Buffer, To_Unsigned_Short (Data));
      pragma Debug (C, O ("Marshall (Short): end"));
   end Marshall;

   --  Marshalling of a long
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Long) is
   begin
      pragma Debug (C, O ("Marshall (Long): enter"));
      Marshall (Buffer, To_Unsigned_Long (Data));
      pragma Debug (C, O ("Marshall (Long): end"));
   end Marshall;

   --  Marshalling of a standard string
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Standard.String) is
   begin
      pragma Debug (C, O ("Marshall (String): enter"));

      Marshall (Buffer, PolyORB.Types.Unsigned_Long'(Data'Length + 1));
      for I in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Char (Data (I)));
      end loop;
      Marshall (Buffer, PolyORB.Types.Char (ASCII.NUL));

      pragma Debug (C, O ("Marshall (String): end"));
   end Marshall;

   --  Marshalling of PolyORB.Types.String
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.String) is
   begin
      pragma Debug (C, O ("Marshall (PolyORB.Types.String): enter"));
      Marshall (Buffer, PolyORB.Types.To_Standard_String (Data));
      pragma Debug (C, O ("Marshall (PolyORB.Types.String): end"));
   end Marshall;

   --  Marshalling a sequence of octets
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Stream_Element_Array)
   is

   begin
      pragma Debug (C, O ("Marshall (Encapsulation): enter"));
      Marshall (Buffer, PolyORB.Types.Unsigned_Long (Data'Length));
      for I in Data'Range loop
         Marshall (Buffer, PolyORB.Types.Octet (Data (I)));
      end loop;
      pragma Debug (C, O ("Marshall (Encapsulation): end"));
   end Marshall;

   --  Marshalling of an Any
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Any.Any)
   is
   begin
      pragma Debug (C, O ("Marshall (Any): enter"));
      Marshall (Buffer, Get_Type (Data));
      pragma Debug (C, O ("Marshall (Any): type marshalled"));
      Marshall_From_Any (Buffer, Get_Container (Data).all);
      pragma Debug (C, O ("Marshall (Any): end"));
   end Marshall;

   --  Puts the right TypeCode in the buffer
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.TypeCode.Local_Ref)
   is
--      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (C, O ("Marshall (Typecode): enter"));
      pragma Debug (C, O ("Marshall (Typecode): kind is " &
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
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(4));
            raise Program_Error;
         when Tk_Ulong =>
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(5));
            raise Program_Error;
         when Tk_Float =>
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(6));
            raise Program_Error;
         when Tk_Double =>
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(7));
            raise Program_Error;
         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(8));
         when Tk_Char =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(9));
         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(10));
         when Tk_Any =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(11));
         when Tk_TypeCode =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(12));
         when Tk_Principal =>
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(13));
            raise Program_Error;
         when Tk_Objref =>
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(14));
--             pragma Debug (C, O ("Marshall (TypeCode): it has "
--                              & PolyORB.Types.Unsigned_Long'Image
--                              (PolyORB.Any.TypeCode.Parameter_Count (Data))
--                              & " parameters"));
--             Marshall (Buffer, PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Buffer, PolyORB.Any.TypeCode.Name (Data));
            raise Program_Error;
         when Tk_Struct =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(15));
--             Start_Encapsulation (Complex_Buffer);
--             pragma Debug (C, O ("Marshall (TypeCode): marshalling the id"));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             declare
--                Nb : PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.TypeCode.Member_Count (Data);
--             begin
--                pragma Debug (C, O ("Marshall (TypeCode): " &
--                                 "marshalling the members. Nb = "
--                                 & PolyORB.Types.Unsigned_Long'Image (Nb)));
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      pragma Debug (C, O ("Marshall (TypeCode): about "
--                                       & "to marshall a new  member"));
--                      Marshall (Complex_Buffer,
--                                PolyORB.Any.TypeCode.Member_Name (Data, I));
--                      pragma Debug
--                        (C, O ("Marshall (TypeCode): marshalling "
--                            & "the type ("
--                            & TCKind'Image
--                            (TypeCode.Kind
--                             (PolyORB.Any.TypeCode.Member_Type (Data, I)))
--                            & ")"));
--                      Marshall (Complex_Buffer,
--                                PolyORB.Any.TypeCode.Member_Type (Data, I));
--                      pragma Debug (C, O ("Marshall (TypeCode): "
--                                       & "member marshalled"));
--                   end loop;
--                end if;
--             end;
--             pragma Debug (C, O ("Marshall: all members marshalled"));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Union =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(16));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Id (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Name (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Discriminator_Type (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Default_Index (Data));
--             declare
--                Nb : PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall_From_Any
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Label (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Name (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Type (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Enum =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(17));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             declare
--                Nb : PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall (Complex_Buffer,
--                                PolyORB.Any.TypeCode.Member_Name (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_String =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(18));
            pragma Debug (C, O ("marshall (typecode): " &
                             "about to marshall length: " &
                             PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Length (Data))));
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
            pragma Debug (C, O ("marshall (typecode): length marshalled"));
         when Tk_Sequence =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(19));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Content_Type (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Length (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Array =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(20));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Content_Type (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Length (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Alias =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(21));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Content_Type (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Except =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(22));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             declare
--                Nb : PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall (Complex_Buffer,
--                                PolyORB.Any.TypeCode.Member_Name (Data, I));
--                      Marshall (Complex_Buffer,
--                                PolyORB.Any.TypeCode.Member_Type (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Longlong =>
            raise Program_Error;
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(23));
         when Tk_Ulonglong =>
            raise Program_Error;
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(24));
         when Tk_Longdouble =>
            raise Program_Error;
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(25));
         when Tk_Widechar =>
            raise Program_Error;
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(26));
         when Tk_Wstring =>
            raise Program_Error;
--            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(27));
--            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
         when Tk_Fixed =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(28));
--             Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Digits (Data));
--             Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Scale (Data));
         when Tk_Value =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(29));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Id (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Name (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Type_Modifier (Data));
--             Marshall
--               (Complex_Buffer,
--                PolyORB.Any.TypeCode.Concrete_Base_Type (Data));
--             declare
--                Nb : PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.TypeCode.Member_Count (Data);
--             begin
--                Marshall (Complex_Buffer, Nb);
--                if Nb /= 0 then
--                   for I in 0 .. Nb - 1 loop
--                      Marshall
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Name (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Type (Data, I));
--                      Marshall
--                        (Complex_Buffer,
--                         PolyORB.Any.TypeCode.Member_Visibility (Data, I));
--                   end loop;
--                end if;
--             end;
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Valuebox =>
            raise Program_Error;
--          Marshall (Buffer, PolyORB.Types.Unsigned_Long'(30));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Content_Type (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Native =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(31));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Abstract_Interface =>
            raise Program_Error;
--             Marshall (Buffer, PolyORB.Types.Unsigned_Long'(32));
--             Start_Encapsulation (Complex_Buffer);
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Id (Data));
--             Marshall (Complex_Buffer,
--                       PolyORB.Any.TypeCode.Name (Data));
--             Marshall (Buffer, Encapsulate (Complex_Buffer));
--             Release (Complex_Buffer);
         when Tk_Local_Interface =>
            raise Program_Error;
         when Tk_Component =>
            raise Program_Error;
         when Tk_Home =>
            raise Program_Error;
         when Tk_Event =>
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Marshall (Typecode): end"));
   end Marshall;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   procedure Marshall_From_Any
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
   begin
      raise Program_Error;
   end Marshall_From_Any;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Any.Any_Container'Class)
   is
      Data_Type : constant PolyORB.Any.TypeCode.Local_Ref :=
                    Unwind_Typedefs (Get_Type (Data));
   begin
      pragma Debug (C, O ("Marshall_From_Any: enter"));
      --  pragma Debug
      --  (0 (Debug_Any(PolyORB.Any.TypeCode.Kind (Data_Type)'Pos)))

      case PolyORB.Any.TypeCode.Kind (Data_Type) is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            Marshall (Buffer, PolyORB.Types.Short'(From_Any (Data)));

         when Tk_Long =>
            Marshall (Buffer, PolyORB.Types.Long'(From_Any (Data)));

         when Tk_Ushort =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Short'(From_Any (Data)));

         when Tk_Ulong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(From_Any (Data)));

         when Tk_Float =>
--            Marshall (Buffer, PolyORB.Types.Float'(From_Any (Data)));
            raise Program_Error;

         when Tk_Double =>
--            Marshall (Buffer, PolyORB.Types.Double'(From_Any (Data)));
            raise Program_Error;

         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Boolean'(From_Any (Data)));

         when Tk_Char =>
            Marshall (Buffer, PolyORB.Types.Char'(From_Any (Data)));

         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Octet'(From_Any (Data)));

         when Tk_Any =>
            Marshall (Buffer, PolyORB.Any.Any'(From_Any (Data)));

         when Tk_TypeCode =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Principal =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Objref =>
            --  Marshall (Buffer, PolyORB.Types.Object.Helper.From_Any (Data));
            raise Program_Error;

         when Tk_Struct =>
--             declare
--                   Nb : constant PolyORB.Types.Unsigned_Long
--                         := PolyORB.Any.Get_Aggregate_Count (Data);
--                   Value : PolyORB.Any.Any;
--             begin
--                for I in 0 .. Nb - 1 loop
--                   Value := PolyORB.Any.Get_Aggregate_Element
--                  (Data, PolyORB.Any.TypeCode.Member_Type (Data_Type, I), I);
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Program_Error;

         when Tk_Union =>
--             declare
--                Nb : PolyORB.Types.Unsigned_Long;
--                Value, Label_Value : PolyORB.Any.Any;
--             begin
--                Label_Value := Get_Aggregate_Element
--                  (Data,
--                   PolyORB.Any.TypeCode.Discriminator_Type (Data_Type),
--                   PolyORB.Types.Unsigned_Long (0));
--                pragma Debug (C, O ("Marshall_From_Any: got the label"));
--                Marshall_From_Any (Buffer, Label_Value);
--                pragma Debug (C, O ("Marshall_From_Any: label marshalled"));
--                Nb := PolyORB.Any.Get_Aggregate_Count (Data);
--                pragma Debug (C, O ("Marshall_From_Any: aggregate count = "
--                                 & PolyORB.Types.Unsigned_Long'Image (Nb)));
--                if Nb > 1 then
--                   for I in 1 .. Nb - 1 loop
--                      Value := PolyORB.Any.Get_Aggregate_Element
--                        (Data,
--                         PolyORB.Any.TypeCode.Member_Type_With_Label
--                         (Data_Type, Label_Value, I - 1),
--                         I);
--                      pragma Debug (C, O ("Marshall_From_Any: about "
--                                       & "to marshall from any"));
--                      Marshall_From_Any (Buffer, Value);
--                   end loop;
--                end if;
--             end;
            raise Program_Error;

         when Tk_Enum =>
--             Marshall_From_Any
--               (Buffer,
--                PolyORB.Any.Get_Aggregate_Element
--                (Data,
--                 PolyORB.Any.TypeCode.TC_Unsigned_Long,
--                 PolyORB.Types.Unsigned_Long (0)));
            raise Program_Error;

         when Tk_String =>
            Marshall (Buffer, PolyORB.Types.String'(From_Any (Data)));

         when Tk_Sequence =>
--             declare
--                Nb : constant PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.Get_Aggregate_Count (Data);
--                Value : PolyORB.Any.Any;
--             begin
--                Value := PolyORB.Any.Get_Aggregate_Element
--                  (Data,
--                   PolyORB.Any.TypeCode.TC_Unsigned_Long,
--                   PolyORB.Types.Unsigned_Long (0));
--                Marshall_From_Any (Buffer, Value);
--                if Nb /= 0 then
--                   for I in 1 .. Nb - 1 loop
--                      Value := PolyORB.Any.Get_Aggregate_Element
--                        (Data,
--                         PolyORB.Any.TypeCode.Content_Type (Data_Type),
--                         I);
--                      Marshall_From_Any (Buffer, Value);
--                   end loop;
--                end if;
--             end;
            raise Program_Error;

         when Tk_Array =>
--             declare
--                Nb : constant PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.Get_Aggregate_Count (Data);
--                Value : PolyORB.Any.Any;
--                Content_True_Type : PolyORB.Any.TypeCode.Local_Ref :=
--                  PolyORB.Any.TypeCode.Content_Type (Data_Type);
--             begin

--                while PolyORB.Any.TypeCode.Kind
--                        (Content_True_Type) = Tk_Array
--                loop
--                   Content_True_Type :=
--                     PolyORB.Any.TypeCode.Content_Type (Content_True_Type);
--                end loop;

--                for I in 0 .. Nb - 1 loop
--                   Value := PolyORB.Any.Get_Aggregate_Element
--                     (Data,
--                      Content_True_Type,
--                      I);
--                   pragma Debug (C, O ("Marshall_From_Any: value kind is "
--                                    & PolyORB.Any.TCKind'Image
--                                    (PolyORB.Any.TypeCode.Kind
--                                     (PolyORB.Any.Get_Type (Value)))));
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Program_Error;

         when Tk_Alias =>
            --  we should never reach this point
            pragma Assert (False);
            raise Program_Error;

         when Tk_Except =>
--             declare
--                Nb : constant PolyORB.Types.Unsigned_Long :=
--                  PolyORB.Any.Get_Aggregate_Count (Data);
--                Value : PolyORB.Any.Any;
--             begin
--                pragma Debug
--                for I in 0 .. Nb - 1 loop
--                   Value := PolyORB.Any.Get_Aggregate_Element
--                     (Data,
--                      PolyORB.Any.TypeCode.Member_Type (Data_Type, I),
--                      I);
--                   Marshall_From_Any (Buffer, Value);
--                end loop;
--             end;
            raise Program_Error;

         when Tk_Longlong =>
--            Marshall (Buffer, PolyORB.Types.Long_Long'(From_Any (Data)));
            raise Program_Error;

         when Tk_Ulonglong =>
--             Marshall
--               (Buffer,
--                PolyORB.Types.Unsigned_Long_Long'(From_Any (Data)));
            raise Program_Error;

         when Tk_Longdouble =>
--            Marshall (Buffer, PolyORB.Types.Long_Double'(From_Any (Data)));
            raise Program_Error;

         when Tk_Widechar =>
--            Marshall (Buffer, PolyORB.Types.Wchar'(From_Any (Data)));
            raise Program_Error;

         when Tk_Wstring =>
--            Marshall (Buffer, PolyORB.Types.Wide_String'(From_Any (Data)));
            raise Program_Error;

         when Tk_Fixed =>
            --  declare
            --   Digit,Scale: PolyORB.Any.Any;
            --  begin
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
            raise Program_Error;

         when Tk_Value =>
            --  declare
            --     Nb: PolyORB.Types.Unsigned_Long;
            --     Value_Modifier, Value_TypeCode,
            --         Value_Visibility : PolyORB.Any.Any;
            --  begin
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
            raise Program_Error;

         when Tk_Valuebox =>
--             Marshall_From_Any (Buffer, PolyORB.Any.Get_Aggregate_Element
--                  (Data, PolyORB.Any.TypeCode.Member_Type (Data_Type,
--                  PolyORB.Types.Unsigned_Long (0)),
--                  PolyORB.Types.Unsigned_Long (0)));
            raise Program_Error;

         when Tk_Native =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Local_Interface =>
            raise Program_Error;

         when Tk_Component =>
            raise Program_Error;

         when Tk_Home =>
            raise Program_Error;

         when Tk_Event =>
            raise Program_Error;

      end case;
      pragma Debug (C, O ("Marshall_From_Any: end"));
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
--       pragma Debug (C, O ("Coded URL: " & Coded_URL.all));

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
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
--      Encoded_URL : String_Ptr;
--      Decoded_URL : String_Ptr;
   begin
      raise Program_Error;
--       Encoded_URL := new Types.String'(Unmarshall_String (R, Buffer));
--       Decoded_URL := new Types.String'(Decode_URL (Encoded_URL.all));
--       Data := Any.To_Any
--         (Types.To_PolyORB_String (Decode_URL (Decoded_URL.all)));
   end Unmarshall_To_Any;

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out PolyORB.Any.Any_Container'Class)
   is
      Tc       : constant PolyORB.Any.TypeCode.Local_Ref :=
                   Unwind_Typedefs (Get_Type (Result));

   begin
      pragma Debug (C, O ("Unmarshall_To_Any: enter"));
      pragma Debug
        (C, O ("Unmarshall_To_Any: Any_Type is " &
            PolyORB.Any.TCKind'Image (TypeCode.Kind (Tc))));

      case Any.TypeCode.Kind (Tc) is
         when Tk_Null | Tk_Void =>
            null;
         when Tk_Short =>
            declare
               S : constant Short := Unmarshall (Buffer);
            begin
               pragma Debug (C, O ("Unmarshall_To_Any: its value is "
                                & PolyORB.Types.Short'Image (S)));
               Set_Any_Value (S, Result);
            end;
         when Tk_Long =>
            declare
               L : constant Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (L, Result);
            end;
         when Tk_Ushort =>
            raise Program_Error;
--             declare
--                Us : Unsigned_Short := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Us);
--             end;
         when Tk_Ulong =>
            raise Program_Error;
--             declare
--                Ul : Unsigned_Long := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ul);
--             end;
         when Tk_Float =>
            raise Program_Error;
--             declare
--                F : PolyORB.Types.Float := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, F);
--             end;
         when Tk_Double =>
            raise Program_Error;
--             declare
--                D : Double := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, D);
--             end;
         when Tk_Boolean =>
            declare
               B : constant PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (B, Result);
            end;
         when Tk_Char =>
            declare
               C : constant Char := Unmarshall (Buffer);
            begin
               Set_Any_Value (C, Result);
            end;
         when Tk_Octet =>
            declare
               O : constant PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (O, Result);
            end;
         when Tk_Any =>
            declare
               A : constant Any.Any := Unmarshall (Buffer);
            begin
               Set_Any_Value (A, Result);
            end;
         when Tk_TypeCode =>
            declare
               T : constant TypeCode.Local_Ref := Unmarshall (Buffer);
            begin
               Set_Any_Value (T, Result);
            end;
         when Tk_Principal =>
            --  FIXME : to be done
            raise Program_Error;
         when Tk_Objref =>
            --  declare
            --     O : PolyORB.Types.Object.Ref := Unmarshall (Buffer);
            --  begin
            --     PolyORB.Types.Object.Helper.Set_Any_Value (Result, O);
            --  end;
            raise Program_Error;
         when Tk_Struct =>
            raise Program_Error;
--             declare
--                Nb : Unsigned_Long :=
--                  TypeCode.Member_Count (Tc);
--                Arg : PolyORB.Any.Any;
--             begin
--                PolyORB.Any.Set_Any_Aggregate_Value (Result);
--                pragma Debug (C, O ("unmarshall_to_any: about to "
--                                 & "unmarshall parameters"));
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
--                      pragma Debug (C, O ("unmarshall_to_any: about to "
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
            raise Program_Error;
--             declare
--                Nb : Unsigned_Long;
--                Label, Arg : PolyORB.Any.Any;
--             begin
--                Set_Any_Aggregate_Value (Result);
--                if Is_Empty then
--                   Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
--                else
--                   Label := Get_Aggregate_Element
--                     (Result,
--                      TypeCode.Discriminator_Type (Tc),
--                      PolyORB.Types.Unsigned_Long (0));
--                end if;
--                Unmarshall_To_Any (Buffer, Label);
--                if Is_Empty then
--                   pragma Debug (C, O ("Unmarshall_To_Any: about to call "
--                                    & "add_aggregate"));
--                   Add_Aggregate_Element (Result, Label);
--                end if;
--                pragma Debug (C, O ("Unmarshall_To_Any: about to call "
--                                 & "member_count_with_label"));
--              Nb := PolyORB.Any.TypeCode.Member_Count_With_Label (Tc, Label);
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
            raise Program_Error;
--          declare
--                Arg : PolyORB.Any.Any;
--             begin
--                Set_Any_Aggregate_Value (Result);
--                if Is_Empty then
--                   Arg := Get_Empty_Any (TC_Unsigned_Long);
--                else
--                   Arg := Get_Aggregate_Element
--                     (Result,
--                      TC_Unsigned_Long,
--                      PolyORB.Types.Unsigned_Long (0));
--                end if;
--                Unmarshall_To_Any (Buffer, Arg);
--                if Is_Empty then
--                   Add_Aggregate_Element (Result, Arg);
--                end if;
--             end;
         when Tk_String =>
            declare
               S : constant PolyORB.Types.String := Unmarshall (Buffer);
            begin
               Set_Any_Value (S, Result);
            end;
         when Tk_Sequence =>
            raise Program_Error;
--             declare
--                Nb : Unsigned_Long := Unmarshall (Buffer);
--                Max_Nb : Unsigned_Long := TypeCode.Length (Tc);
--                Arg : PolyORB.Any.Any;
--             begin
--                if Max_Nb > 0 and then Nb > Max_Nb then
--                   PolyORB.CORBA_P.Exceptions.Raise_Marshal;
--                end if;
--                Set_Any_Aggregate_Value (Result);
--                if Is_Empty then
--                   Add_Aggregate_Element (Result, To_Any (Nb));
--                else
--                   Arg := Get_Aggregate_Element
--                     (Result,
--                      TC_Unsigned_Long,
--                      PolyORB.Types.Unsigned_Long (0));
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
            raise Program_Error;
--             declare
--                Nb : Unsigned_Long := TypeCode.Length (Tc);
--                Content_True_Type : PolyORB.Any.TypeCode.Local_Ref :=
--                  TypeCode.Content_Type (Tc);
--                Arg : PolyORB.Any.Any;
--             begin
--                while PolyORB.Any.TypeCode.Kind
--                        (Content_True_Type) = Tk_Array
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
            raise Program_Error;
--             declare
--                Nb : Unsigned_Long :=
--                  TypeCode.Member_Count (Tc);
--                Arg : PolyORB.Any.Any;
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
            raise Program_Error;
--             declare
--                Ll : Long_Long := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ll);
--             end;
         when Tk_Ulonglong =>
            raise Program_Error;
--             declare
--                Ull : Unsigned_Long_Long := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ull);
--             end;
         when Tk_Longdouble =>
            raise Program_Error;
--             declare
--                Ld : Long_Double := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ld);
--             end;
         when Tk_Widechar =>
            raise Program_Error;
--             declare
--                Wc : Wchar := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Wc);
--             end;
         when Tk_Wstring =>
            raise Program_Error;
--             declare
--                Ws : PolyORB.Types.Wide_String := Unmarshall (Buffer);
--             begin
--                Set_Any_Value (Result, Ws);
--             end;
         when Tk_Fixed =>
            --  FIXME : to be done
            --  declare
            --   Arg1,Arg2:PolyORB.Any.Any;
            --  begin
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
            raise Program_Error;

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
         when Tk_Local_Interface =>
            --  FIXME : to be done
            null;
         when Tk_Component =>
            --  FIXME : to be done
            null;
         when Tk_Home =>
            --  FIXME : to be done
            null;
         when Tk_Event =>
            --  FIXME : to be done
            null;
      end case;
      pragma Debug (C, O ("Unmarshall_To_Any: end"));
   end Unmarshall_To_Any;

   -------------------
   -- Marshall_Char --
   -------------------

   procedure Marshall_Char (B : access Buffer_Type; C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'(1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   ---------------------
   -- Unmarshall_Char --
   ---------------------

   function Unmarshall_Char (B : access Buffer_Type) return Character
   is
      A : Stream_Element_Array (1 .. 1);
   begin
      Align_Unmarshall_Copy (B, Align_1, A);
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
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
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
      Max : constant Stream_Element_Count := Length (B.all);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
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
      Ch : Types.Char;
   begin
      pragma Debug (C, O ("Marshall (PolyORB.Types.String): enter"));

      Ch := Unmarshall (Buffer);
      while Ch /= ASCII.CR and then Ch /= ASCII.NUL loop
         Append (Result, Ch);
         Ch := Unmarshall (Buffer);
      end loop;
      if Ch = ASCII.CR then
         Ch := Unmarshall (Buffer);
         pragma Assert (Ch = ASCII.LF);
      end if;

      pragma Debug (C, O ("Marshall (PolyORB.Types.String): end"));

      return Result;
   end Unmarshall;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   function Unmarshall_To_Any
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type) return Any.Any
   is
      Data  : Any.Any;
      Error : Errors.Error_Container;
   begin
      Unmarshall_To_Any (R, Buffer, Get_Container (Data).all, Error);
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
      Local_SRP_Info : constant Split_SRP := SRP_Info;
      Coded_URL : String_Ptr;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
--       Encode_URL (Local_SRP_Info);
--       Coded_URL :=
--         new Types.String'((From_Any (Join (Local_SRP_Info))));
      Coded_URL := new Types.String'(Encode_URL (Local_SRP_Info));
      pragma Debug (C, O ("Coded URL: " & To_Standard_String (Coded_URL.all)));

      for I in To_Standard_String (Coded_URL.all)'Range loop
         Align_Marshall_Copy
           (Buffer, Stream_Element_Array'
            (1 => Stream_Element
               (Character'Pos (To_Standard_String (Coded_URL.all) (I)))));
      end loop;

   end Marshall_From_Split_SRP;

end PolyORB.Representations.SRP;
