------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Unchecked_Conversion;

with PolyORB.Log;

package body PolyORB.POA_Types is

   use Ada.Streams;

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.poa_types");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  Object ids are represented as stream element arrays
   --  using a private representation, that need not be
   --  compatible with anything external. The only constraint
   --  is that the Get_* and Put_* subprograms below be
   --  consistent.

   --  We assume that a time stamp's size is always an integral multiple of
   --  the size of an unsigned long integer.

   ULongs_In_Time_Stamp : constant :=
                            Time_Stamp'Size / Types.Unsigned_Long'Size;
   type Time_Stamp_As_ULongs is array (1 .. ULongs_In_Time_Stamp)
     of Types.Unsigned_Long;

   function To_ULongs is
     new Ada.Unchecked_Conversion (Time_Stamp, Time_Stamp_As_ULongs);
   function From_ULongs is
     new Ada.Unchecked_Conversion (Time_Stamp_As_ULongs, Time_Stamp);

   --  The Get_* procedures operate at index SEI in array SEA,
   --  and advance SEI by the number of consumed Stream_Elements.

   procedure Get_Time_Stamp
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      TS    :    out Time_Stamp;
      Error : in out PolyORB.Errors.Error_Container);
   --  Extract a time stamp

   function Put_Time_Stamp
     (TS : Time_Stamp)
     return Object_Id;
   --  Store a time stamp

   procedure Get_ULong
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      ULo   :    out Types.Unsigned_Long;
      Error : in out PolyORB.Errors.Error_Container);
   --  Extract an unsigned long.

   function Put_ULong
     (ULo : Types.Unsigned_Long)
     return Object_Id;
   --  Store an unsigned long as 8 hexadecimal digits

   procedure Get_Boolean
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      Boo   :    out Types.Boolean;
      Error : in out PolyORB.Errors.Error_Container);
   --  Extract a boolean

   function Put_Boolean
     (Boo : Types.Boolean)
     return Object_Id;
   --  Store a boolean

   procedure Get_String_With_Length
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      Str   :    out Types.String;
      Error : in out PolyORB.Errors.Error_Container);
   --  Extract a string stored with prefixed U_Long length

   function Put_String
     (Str         : Types.String;
      With_Length : Boolean := True)
     return Object_Id;
   --  Store a string (with optional U_Long length prefixed)
   --  If a string is stored without length, it is the caller's responsibility
   --  to delimit it as appropriate.

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : Unmarshalled_Oid)
     return Standard.Boolean is
   begin
      return True
        and then Left.Id = Right.Id
        and then Left.System_Generated = Right.System_Generated
        and then Left.Persistency_Flag = Right.Persistency_Flag;
   end "=";

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Types.Boolean;
      Persistency_Flag : Lifespan_Cookie;
      Creator          : Standard.String)
     return Unmarshalled_Oid_Access is
   begin
      return new Unmarshalled_Oid'
        (Id               => To_PolyORB_String (Name),
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => To_PolyORB_String (Creator));
   end Create_Id;

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Boolean;
      Persistency_Flag : Time_Stamp;
      Creator          : Standard.String)
     return Unmarshalled_Oid is
   begin
      return Unmarshalled_Oid'
        (Id               => To_PolyORB_String (Name),
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => To_PolyORB_String (Creator));
   end Create_Id;

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Types.Boolean;
      Persistency_Flag : Lifespan_Cookie;
      Creator          : Standard.String)
     return Object_Id_Access
   is
   begin
      return U_Oid_To_Oid
        (Unmarshalled_Oid'
         (Id               => To_PolyORB_String (Name),
          System_Generated => System_Generated,
          Persistency_Flag => Persistency_Flag,
          Creator          => To_PolyORB_String (Creator)));
   end Create_Id;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   procedure Get_Time_Stamp
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      TS    :    out Time_Stamp;
      Error : in out PolyORB.Errors.Error_Container)
   is
      ULongs : Time_Stamp_As_ULongs;
   begin
      for J in ULongs'Range loop
         Get_ULong (SEA, SEI, ULongs (J), Error);
         if Errors.Found (Error) then
            pragma Warnings (Off);  --  "TS" not set before return
            return;
            pragma Warnings (On);
         end if;
      end loop;
      TS := From_ULongs (ULongs);
   end Get_Time_Stamp;

   --------------------
   -- Put_Time_Stamp --
   --------------------

   function Put_Time_Stamp
     (TS : Time_Stamp)
     return Object_Id
   is
      ULongs : constant Time_Stamp_As_ULongs := To_ULongs (TS);
      Result : Object_Id (1 .. 8 * ULongs'Length);
      First  : Stream_Element_Offset := Result'First;

   begin
      for J in ULongs'Range loop
         Result (First .. First + 7) := Put_ULong (ULongs (J));
         First := First + 8;
      end loop;
      return Result;
   end Put_Time_Stamp;

   ---------------
   -- Get_ULong --
   ---------------

   Hex_Val : constant array (Stream_Element) of Types.Unsigned_Long :=
     (Character'Pos ('0') => 0,
      Character'Pos ('1') => 1,
      Character'Pos ('2') => 2,
      Character'Pos ('3') => 3,
      Character'Pos ('4') => 4,
      Character'Pos ('5') => 5,
      Character'Pos ('6') => 6,
      Character'Pos ('7') => 7,
      Character'Pos ('8') => 8,
      Character'Pos ('9') => 9,
      Character'Pos ('A') => 10,
      Character'Pos ('a') => 10,
      Character'Pos ('B') => 11,
      Character'Pos ('b') => 11,
      Character'Pos ('C') => 12,
      Character'Pos ('c') => 12,
      Character'Pos ('D') => 13,
      Character'Pos ('d') => 13,
      Character'Pos ('E') => 14,
      Character'Pos ('e') => 14,
      Character'Pos ('F') => 15,
      Character'Pos ('f') => 15,
      others => 0);

   procedure Get_ULong
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      ULo   :    out Types.Unsigned_Long;
      Error : in out PolyORB.Errors.Error_Container)
   is
      R : Types.Unsigned_Long := 0;
   begin
      if SEI + 7 > SEA'Last then
         PolyORB.Errors.Throw
           (Error,
            PolyORB.Errors.Invalid_Object_Id_E,
            PolyORB.Errors.Null_Member);

         ULo := 0;
         return;
      end if;

      for J in Stream_Element_Offset range 0 .. 7 loop
         R := R * 16 + Hex_Val (SEA (SEI + J));
      end loop;

      ULo := R;
      SEI := SEI + 8;
   end Get_ULong;

   ---------------
   -- Put_ULong --
   ---------------

   Hex : constant array (Types.Unsigned_Long range 0 .. 15) of Stream_Element
     := (Character'Pos ('0'),
         Character'Pos ('1'),
         Character'Pos ('2'),
         Character'Pos ('3'),
         Character'Pos ('4'),
         Character'Pos ('5'),
         Character'Pos ('6'),
         Character'Pos ('7'),
         Character'Pos ('8'),
         Character'Pos ('9'),
         Character'Pos ('a'),
         Character'Pos ('b'),
         Character'Pos ('c'),
         Character'Pos ('d'),
         Character'Pos ('e'),
         Character'Pos ('f'));

   function Put_ULong
     (ULo : Types.Unsigned_Long)
     return Object_Id
   is
      R : Object_Id (0 .. 7);
      U : Types.Unsigned_Long := ULo;
   begin
      for J in reverse R'Range loop
         R (J) := Hex (U mod 16);
         U := U / 16;
      end loop;

      return R;
   end Put_ULong;

   -----------------
   -- Get_Boolean --
   -----------------

   procedure Get_Boolean
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      Boo   :    out Types.Boolean;
      Error : in out PolyORB.Errors.Error_Container) is
   begin
      case SEA (SEI) is
         when Character'Pos ('F') =>
            Boo := False;

         when Character'Pos ('T') =>
            Boo := True;

         when others =>
            Boo := False;

            PolyORB.Errors.Throw
              (Error,
               PolyORB.Errors.Invalid_Object_Id_E,
               PolyORB.Errors.Null_Member);
      end case;

      SEI := SEI + 1;
   end Get_Boolean;

   -----------------
   -- Put_Boolean --
   -----------------

   Bool_To_SE : constant array (Boolean) of Stream_Element :=
     (False => Character'Pos ('F'), True => Character'Pos ('T'));

   function Put_Boolean
     (Boo : Types.Boolean)
     return Object_Id
   is
   begin
      return Object_Id'(0 .. 0 => Bool_To_SE (Boo));
   end Put_Boolean;

   ----------------------------
   -- Get_String_With_Length --
   ----------------------------

   procedure Get_String_With_Length
     (SEA   : Object_Id;
      SEI   : in out Stream_Element_Offset;
      Str   :    out Types.String;
      Error : in out PolyORB.Errors.Error_Container)
   is
      Len : Types.Unsigned_Long;
   begin
      Get_ULong (SEA, SEI, Len, Error);

      if SEI + Stream_Element_Offset (Len) >
        SEA'Last + Stream_Element_Offset (1)
        or else PolyORB.Errors.Found (Error)
      then
         Str := Types.To_PolyORB_String ("");

         PolyORB.Errors.Throw
           (Error,
            PolyORB.Errors.Invalid_Object_Id_E,
            PolyORB.Errors.Null_Member);
         return;
      end if;

      if Len > 0 then
         declare
            S : Standard.String (1 .. Integer (Len));
            pragma Import (Ada, S);
            for S'Address use SEA (SEI)'Address;
         begin
            Str := To_PolyORB_String (S);
         end;
      end if;

      SEI := SEI + Stream_Element_Offset (Len);
   end Get_String_With_Length;

   ----------------
   -- Put_String --
   ----------------

   function Put_String
     (Str         : Types.String;
      With_Length : Boolean := True)
     return Object_Id
   is
      S : constant Standard.String := To_Standard_String (Str);
   begin
      if S'Length = 0 then
         if With_Length then
            return Put_ULong (0);
         else
            return Object_Id'(1 .. 0 => 0);
         end if;
      end if;

      declare
         R : Object_Id (1 .. S'Length);
         pragma Import (Ada, R);
         for R'Address use S (S'First)'Address;
      begin
         if With_Length then
            return Put_ULong (S'Length) & R;
         else
            return R;
         end if;
      end;
   end Put_String;

   -----------------
   -- Get_Creator --
   -----------------

   function Get_Creator (Oid : Object_Id) return String is
      Sep : Integer;
      Oid_Str : String (1 .. Oid'Length);
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use Oid (Oid'First)'Address;

   begin
      --  Determine last character of Creator by looking for last
      --  occurrence of POA_Path_Separator. If there is no occurrence,
      --  the whole string is the Creator.

      Sep := Utils.Find
        (Oid_Str, Oid_Str'Last, POA_Path_Separator,
         Skip => False, Direction => Utils.Backward);
      if Sep < Oid_Str'First then
         --  No POA_Path_Separator: the whole string is the Creator
         Sep := Oid_Str'Last + 1;
      end if;

      if Sep = Oid_Str'First then
         --  Empty creator, we may not index Oid_Str with Sep - 1 as it is
         --  out of range.
         return "";
      else
         return Oid_Str (Oid_Str'First .. Sep - 1);
      end if;
   end Get_Creator;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   procedure Oid_To_U_Oid
     (Oid   :        Object_Id;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container)
   is
      Index : Stream_Element_Offset := Oid'First;

      Creator : constant String := Get_Creator (Oid);

   begin
      U_Oid.System_Generated := False;
      U_Oid.Persistency_Flag := Null_Time_Stamp;

      U_Oid.Creator := To_PolyORB_String (Creator);
      Index := Oid'First + Stream_Element_Offset (Creator'Length) + 1;

      if Index <= Oid'Last then
         Get_String_With_Length (Oid, Index, U_Oid.Id, Error);
         if PolyORB.Errors.Found (Error) then
            return;
         end if;

         Get_Boolean (Oid, Index, U_Oid.System_Generated, Error);
         if PolyORB.Errors.Found (Error) then
            return;
         end if;

         Get_Time_Stamp (Oid, Index, U_Oid.Persistency_Flag, Error);
         if PolyORB.Errors.Found (Error) then
            return;
         end if;
      end if;
      pragma Assert (Index > Oid'Last);
   end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid) return Object_Id_Access is
      Oid   : constant Object_Id := U_Oid_To_Oid (U_Oid);

      Oid_A : constant Object_Id_Access := new Object_Id'(Oid);

   begin
      pragma Debug (C, O ("Oid is " & Image (Oid)));
      return Oid_A;
   end U_Oid_To_Oid;

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid) return Object_Id is
   begin
      return Put_String  (U_Oid.Creator, With_Length => False)
        & Character'Pos  (POA_Path_Separator)
        & Put_String     (U_Oid.Id)
        & Put_Boolean    (U_Oid.System_Generated)
        & Put_Time_Stamp (U_Oid.Persistency_Flag);
   end U_Oid_To_Oid;

end PolyORB.POA_Types;
