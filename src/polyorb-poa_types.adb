------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
     (SEA   : in     Stream_Element_Array;
      SEI   : in out Stream_Element_Offset;
      ULo   :    out Types.Unsigned_Long;
      Error : in out PolyORB.Exceptions.Error_Container);
   --  Extract an unsigned long.

   function Put_ULong
     (ULo : Types.Unsigned_Long)
     return Stream_Element_Array;
   --  Store an unsigned long.

   procedure Get_Boolean
     (SEA   : in     Stream_Element_Array;
      SEI   : in out Stream_Element_Offset;
      Boo   :    out Types.Boolean;
      Error : in out PolyORB.Exceptions.Error_Container);
   --  Extract a boolean.

   function Put_Boolean
     (Boo : Types.Boolean)
     return Stream_Element_Array;
   --  Store a boolean.

   procedure Get_String
     (SEA   : in     Stream_Element_Array;
      SEI   : in out Stream_Element_Offset;
      Str   :    out Types.String;
      Error : in out PolyORB.Exceptions.Error_Container);
   --  Extract a string.

   function Put_String
     (Str : Types.String)
     return Stream_Element_Array;
   --  Store a string.

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

   -----------
   -- Image --
   -----------

   function Image
     (Oid : Object_Id)
     return Types.String is
   begin
      return To_PolyORB_String
        (PolyORB.Objects.To_String (PolyORB.Objects.Object_Id (Oid)));
   end Image;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Types.Boolean;
      Persistency_Flag : in Lifespan_Cookie;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access is
   begin
      return new Unmarshalled_Oid'
        (Id               => Name,
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => Creator);
   end Create_Id;

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid is
   begin
      return Unmarshalled_Oid'
        (Id               => Name,
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag,
         Creator          => Creator);
   end Create_Id;

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Types.Boolean;
      Persistency_Flag : in Lifespan_Cookie;
      Creator          : in Types.String)
     return Object_Id_Access
   is
   begin
      return U_Oid_To_Oid
        (Unmarshalled_Oid'
         (Id               => Name,
          System_Generated => System_Generated,
          Persistency_Flag => Persistency_Flag,
          Creator          => Creator));
   end Create_Id;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Oid : Object_Id)
     return Types.String
   is
      Index : Stream_Element_Offset;

      Creator : PolyORB.Types.String;
      Id      : PolyORB.Types.String;
      Error   : PolyORB.Exceptions.Error_Container;

   begin
      Index := Oid'First;

      Get_String
        (Stream_Element_Array (Oid), Index, Creator, Error);

      if PolyORB.Exceptions.Found (Error) then
         raise Constraint_Error;
      end if;

      Get_String
        (Stream_Element_Array (Oid), Index, Id, Error);

      if PolyORB.Exceptions.Found (Error) then
         raise Constraint_Error;
      end if;

      return Id;
   end Get_Name;

   ---------------
   -- Get_ULong --
   ---------------

   procedure Get_ULong
     (SEA   : in     Stream_Element_Array;
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
     return Stream_Element_Array
   is
      R : Stream_Element_Array (0 .. 3);
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
     (SEA   : in     Stream_Element_Array;
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
     return Stream_Element_Array
   is
      R : Stream_Element_Array (0 .. 0);
   begin
      if Boo then
         R (0) := 1;
      else
         R (0) := 0;
      end if;

      return R;
   end Put_Boolean;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String
     (SEA   : in     Stream_Element_Array;
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

      declare
         S : Standard.String (1 .. Integer (Len));
      begin
         for J in S'Range loop
            S (J) := Standard.Character'Val
              (SEA (SEI + Stream_Element_Offset (J - S'First)));

         end loop;

         Str := To_PolyORB_String (S);
      end;

      SEI := SEI + Stream_Element_Offset (Len);
   end Get_String;

   ----------------
   -- Put_String --
   ----------------

   function Put_String
     (Str : Types.String)
     return Stream_Element_Array
   is
      S : constant Standard.String := To_Standard_String (Str);
      R : Stream_Element_Array
        (Stream_Element_Offset (S'First) .. Stream_Element_Offset (S'Last));

   begin
      for J in S'Range loop
         R (Stream_Element_Offset (J)) :=
           Stream_Element (Standard.Character'Pos (S (J)));
      end loop;

      return Put_ULong (S'Length) & R;
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

      Creator          : PolyORB.Types.String;
      Id               : PolyORB.Types.String;
      System_Generated : PolyORB.Types.Boolean;
      Persistency_Flag : PolyORB.Types.Unsigned_Long;
   begin
      Index := Oid'First;

      Get_String
        (Stream_Element_Array (Oid), Index, Creator, Error);

      if PolyORB.Exceptions.Found (Error) then
         return;
      end if;

      Get_String
        (Stream_Element_Array (Oid), Index, Id, Error);

      if PolyORB.Exceptions.Found (Error) then
         return;
      end if;

      Get_Boolean
        (Stream_Element_Array (Oid), Index, System_Generated, Error);

      if PolyORB.Exceptions.Found (Error) then
         return;
      end if;

      Get_ULong
        (Stream_Element_Array (Oid), Index, Persistency_Flag, Error);

      if PolyORB.Exceptions.Found (Error) then
         return;
      end if;

      pragma Assert (Index = Oid'Last + 1);

      U_Oid := Unmarshalled_Oid'
        (Creator          => Creator,
         Id               => Id,
         System_Generated => System_Generated,
         Persistency_Flag => Lifespan_Cookie (Persistency_Flag));
   end Oid_To_U_Oid;

   function Oid_To_U_Oid
     (Oid : Object_Id)
     return Unmarshalled_Oid
   is
      U_Oid : Unmarshalled_Oid;
      Error : PolyORB.Exceptions.Error_Container;
   begin

      Oid_To_U_Oid (Oid, U_Oid, Error);

      if PolyORB.Exceptions.Found (Error) then
         raise Constraint_Error;
      end if;

      return U_Oid;
   end Oid_To_U_Oid;

   function Oid_To_U_Oid
     (Oid : access Object_Id)
     return Unmarshalled_Oid is
   begin
      return Oid_To_U_Oid (Oid.all);
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
     return Object_Id is
   begin
      return Object_Id'
        (Object_Id
         (Put_String    (U_Oid.Creator)
          & Put_String  (U_Oid.Id)
          & Put_Boolean (U_Oid.System_Generated)
          & Put_ULong   (U_Oid.Persistency_Flag)));
   end U_Oid_To_Oid;

end PolyORB.POA_Types;
