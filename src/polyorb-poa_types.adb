------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.POA_Types is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.poa_types");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --  Object ids are represented as stream element arrays
   --  using a private representation, that need not be
   --  compatible with anything external. The only constraint
   --  is that the Get_* and Put_* subprograms below be
   --  consistent.

   --  The Get_* procedures operate at index SEI in array SEA,
   --  and advance SEI by the number of consumed Stream_Elements.

   procedure Get_ULong
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      ULo   :    out Types.Unsigned_Long;
      Error : in out PolyORB.Exceptions.Error_Container);
   --  Extract an unsigned long.

   function Put_ULong
     (ULo : Types.Unsigned_Long)
     return Object_Id;
   --  Store an unsigned long.

   procedure Get_Boolean
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      Boo   :    out Types.Boolean;
      Error : in out PolyORB.Exceptions.Error_Container);
   --  Extract a boolean.

   function Put_Boolean
     (Boo : Types.Boolean)
     return Object_Id;
   --  Store a boolean.

   procedure Get_String_With_Length
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      Str   :    out Types.String;
      Error : in out PolyORB.Exceptions.Error_Container);
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
     (Left, Right : in Unmarshalled_Oid)
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
     (Name             : in Standard.String;
      System_Generated : in Types.Boolean;
      Persistency_Flag : in Lifespan_Cookie;
      Creator          : in Standard.String)
     return Unmarshalled_Oid_Access is
   begin
      return new Unmarshalled_Oid'
        (Id               => To_PolyORB_String (Name),
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => To_PolyORB_String (Creator));
   end Create_Id;

   function Create_Id
     (Name             : in Standard.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Standard.String)
     return Unmarshalled_Oid is
   begin
      return Unmarshalled_Oid'
        (Id               => To_PolyORB_String (Name),
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => To_PolyORB_String (Creator));
   end Create_Id;

   function Create_Id
     (Name             : in Standard.String;
      System_Generated : in Types.Boolean;
      Persistency_Flag : in Lifespan_Cookie;
      Creator          : in Standard.String)
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

   ---------------
   -- Get_ULong --
   ---------------

   procedure Get_ULong
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      ULo   :    out Types.Unsigned_Long;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      R : Types.Unsigned_Long := 0;
   begin
      if SEI + 4 > SEA'Last + 1 then
         PolyORB.Exceptions.Throw
           (Error,
            PolyORB.Exceptions.Invalid_Object_Id_E,
            PolyORB.Exceptions.Null_Member);

         ULo := 0;
         return;
      end if;

      for J in 0 .. 3 loop
         R := R * 256 + Types.Unsigned_Long
           (SEA (SEI + Stream_Element_Offset (J)));
      end loop;

      ULo := R;
      SEI := SEI + 4;
   end Get_ULong;

   ---------------
   -- Put_ULong --
   ---------------

   function Put_ULong
     (ULo : Types.Unsigned_Long)
     return Object_Id
   is
      R : Object_Id (0 .. 3);
      U : Types.Unsigned_Long := ULo;
   begin
      for J in reverse R'Range loop
         R (J) := Stream_Element (U mod 256);
         U := U / 256;
      end loop;

      return R;
   end Put_ULong;

   -----------------
   -- Get_Boolean --
   -----------------

   procedure Get_Boolean
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      Boo   :    out Types.Boolean;
      Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      case SEA (SEI) is
         when 0 =>
            Boo := False;

         when 1 =>
            Boo := True;

         when others =>
            Boo := False;

            PolyORB.Exceptions.Throw
              (Error,
               PolyORB.Exceptions.Invalid_Object_Id_E,
               PolyORB.Exceptions.Null_Member);
      end case;

      SEI := SEI + 1;
   end Get_Boolean;

   -----------------
   -- Put_Boolean --
   -----------------

   function Put_Boolean
     (Boo : Types.Boolean)
     return Object_Id
   is
   begin
      return Object_Id'(0 .. 0 => Boolean'Pos (Boo));
   end Put_Boolean;

   ----------------------------
   -- Get_String_With_Length --
   ----------------------------

   procedure Get_String_With_Length
     (SEA   : in     Object_Id;
      SEI   : in out Stream_Element_Offset;
      Str   :    out Types.String;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      Len : Types.Unsigned_Long;
   begin
      Get_ULong (SEA, SEI, Len, Error);

      if SEI + Stream_Element_Offset (Len) >
        SEA'Last + Stream_Element_Offset (1)
        or else PolyORB.Exceptions.Found (Error)
      then
         Str := Types.To_PolyORB_String ("");

         PolyORB.Exceptions.Throw
           (Error,
            PolyORB.Exceptions.Invalid_Object_Id_E,
            PolyORB.Exceptions.Null_Member);
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

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   procedure Oid_To_U_Oid
     (Oid   :        Object_Id;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      Index : Stream_Element_Offset;
      Oid_Str : String (1 .. Oid'Length);
      pragma Import (Ada, Oid_Str);
      for Oid_Str'Address use Oid (Oid'First)'Address;

      Creator          : PolyORB.Types.String;
      Id               : PolyORB.Types.String;
      System_Generated : PolyORB.Types.Boolean       := False;
      Persistency_Flag : PolyORB.Types.Unsigned_Long := 0;
   begin
      Index := Oid'First;

      declare
         Creator_Last : constant Integer := Utils.Find
                          (Oid_Str, Oid_Str'First, POA_Path_Separator) - 1;
      begin
         Creator :=
           To_PolyORB_String (Oid_Str (Oid_Str'First .. Creator_Last));
         Index := Oid'First
           + Stream_Element_Offset (Creator_Last - Oid_Str'First)
           + 2; --  Skip POA_Path_Separator
      end;

      if Index <= Oid'Last then
         Get_String_With_Length (Oid, Index, Id, Error);
         if PolyORB.Exceptions.Found (Error) then
            return;
         end if;

         Get_Boolean (Oid, Index, System_Generated, Error);
         if PolyORB.Exceptions.Found (Error) then
            return;
         end if;

         Get_ULong (Oid, Index, Persistency_Flag, Error);
         if PolyORB.Exceptions.Found (Error) then
            return;
         end if;
      end if;
      pragma Assert (Index > Oid'Last);

      U_Oid := Unmarshalled_Oid'
        (Creator          => Creator,
         Id               => Id,
         System_Generated => System_Generated,
         Persistency_Flag => Lifespan_Cookie (Persistency_Flag));
   end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
     return Object_Id_Access
   is
      Oid   : constant Object_Id := U_Oid_To_Oid (U_Oid);

      Oid_A : constant Object_Id_Access := new Object_Id'(Oid);

   begin
      pragma Debug (O ("Oid is " & Image (Oid)));
      return Oid_A;
   end U_Oid_To_Oid;

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
         return Object_Id
   is
   begin
      return Object_Id'
        (Object_Id
           (Put_String    (U_Oid.Creator, With_Length => False)
            & Stream_Element (Character'Pos (POA_Path_Separator))
            & Put_String  (U_Oid.Id)
            & Put_Boolean (U_Oid.System_Generated)
            & Put_ULong   (U_Oid.Persistency_Flag)));
   end U_Oid_To_Oid;

end PolyORB.POA_Types;
