------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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

with Ada.Streams;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Types;

package body PolyORB.POA_Types is

   use Ada.Streams;
   use PolyORB.Types;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.poa_types");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ----------
   -- Left --
   ----------

   function "="
     (Left, Right : in Unmarshalled_Oid)
     return Standard.Boolean
   is
   begin
      if Left.Id = Right.Id
        and then Left.System_Generated = Right.System_Generated
        and then Left.Persistency_Flag = Right.Persistency_Flag
      then
         return True;
      end if;
      return False;
   end "=";

   -----------
   -- Image --
   -----------

   function Image
     (Oid : Object_Id)
     return Types.String
   is
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
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access
   is
   begin
      return new Unmarshalled_Oid'
        (Creator => Creator, Id => Name,
         System_Generated => System_Generated,
         Persistency_Flag => Persistency_Flag);
   end Create_Id;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Types.Boolean;
      Persistency_Flag : in Time_Stamp;
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

   --  Object ids are represented as stream element arrays
   --  using a private representation, that need not be
   --  compatible with anything external. The only constraint
   --  is that the Get_* and Put_* subprograms below be
   --  consistent.

   --  The Get_* procedures operate at index SEI in array SEA,
   --  and advance SEI by the number of consumed Stream_Elements.

   procedure Get_ULong
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      ULo :    out Types.Unsigned_Long);
   --  Extract an unsigned long.

   function Put_ULong (ULo : Types.Unsigned_Long)
     return Stream_Element_Array;
   --  Store an unsigned long.

   procedure Get_Boolean
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      Boo :    out Types.Boolean);
   --  Extract a boolean.

   function Put_Boolean (Boo : Types.Boolean)
     return Stream_Element_Array;
   --  Store a boolean.

   procedure Get_String
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      Str :    out Types.String);
   --  Extract a string.

   function Put_String (Str : Types.String)
     return Stream_Element_Array;
   --  Store a string.

   ---------------------
   -- Implementations --
   ---------------------

   procedure Get_ULong
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      ULo :    out Types.Unsigned_Long)
   is
      R : Types.Unsigned_Long := 0;
   begin
      for I in 0 .. 3 loop
         R := R * 256 + Types.Unsigned_Long
           (SEA (SEI + Stream_Element_Offset (I)));
      end loop;
      ULo := R;
      SEI := SEI + 4;
   end Get_ULong;

   function Put_ULong (ULo : Types.Unsigned_Long)
     return Stream_Element_Array
   is
      R : Stream_Element_Array (0 .. 3);
      U : Types.Unsigned_Long := ULo;
   begin
      for I in reverse R'Range loop
         R (I) := Stream_Element (U mod 256);
         U := U / 256;
      end loop;
      return R;
   end Put_ULong;

   procedure Get_Boolean
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      Boo :    out Types.Boolean) is
   begin
      case SEA (SEI) is
         when 0 =>
            Boo := False;
         when 1 =>
            Boo := True;
         when others =>
            raise Constraint_Error;
      end case;
      SEI := SEI + 1;
   end Get_Boolean;

   function Put_Boolean (Boo : Types.Boolean)
     return Stream_Element_Array
   is
      R : Stream_Element_Array (0 .. 0);
   begin
      if Boo then R (0) := 1; else R (0) := 0; end if;
      return R;
   end Put_Boolean;

   procedure Get_String
     (SEA : in     Stream_Element_Array;
      SEI : in out Stream_Element_Offset;
      Str :    out Types.String)
   is
      Len : Types.Unsigned_Long;
   begin
      Get_ULong (SEA, SEI, Len);
      declare
         S : Standard.String (1 .. Integer (Len));
      begin
         for I in S'Range loop
            S (I) := Standard.Character'Val
              (SEA (SEI + Stream_Element_Offset
                    (I - S'First)));
         end loop;
         Str := To_PolyORB_String (S);
      end;
      SEI := SEI + Stream_Element_Offset (Len);
   end Get_String;

   function Put_String (Str : Types.String)
     return Stream_Element_Array
   is
      S : constant Standard.String := To_Standard_String (Str);
      R : Stream_Element_Array
        (Stream_Element_Offset (S'First)
         .. Stream_Element_Offset (S'Last));
   begin
      for I in S'Range loop
         R (Stream_Element_Offset (I))
           := Stream_Element (Standard.Character'Pos (S (I)));
      end loop;
      return Put_ULong (S'Length) & R;
   end Put_String;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   function Oid_To_U_Oid
     (Oid : access Object_Id)
     return Unmarshalled_Oid
   is
      Index : Stream_Element_Offset;

      Creator          : PolyORB.Types.String;
      Id               : PolyORB.Types.String;
      System_Generated : PolyORB.Types.Boolean;
      Persistency_Flag : PolyORB.Types.Unsigned_Long;
   begin
      Index := Oid'First;
      Get_String
        (Stream_Element_Array (Oid.all), Index, Creator);
      Get_String
        (Stream_Element_Array (Oid.all), Index, Id);
      Get_Boolean
        (Stream_Element_Array (Oid.all), Index, System_Generated);
      Get_ULong
        (Stream_Element_Array (Oid.all), Index, Persistency_Flag);
      pragma Assert (Index = Oid'Last + 1);

      return Unmarshalled_Oid'
        (Creator => Creator,
         Id => Id,
         System_Generated => System_Generated,
         Persistency_Flag => Time_Stamp (Persistency_Flag));
   end Oid_To_U_Oid;

--    ------------------
--    -- Oid_To_U_Oid --
--    ------------------

--    function Oid_To_U_Oid
--      (Oid : Object_Id)
--      return Unmarshalled_Oid_Access
--    is
--       Oid_Access : Object_Id_Access;
--       U_Oid      : Unmarshalled_Oid_Access;
--    begin
--       Oid_Access := new Object_Id'(Oid);
--       --  Oid_Access.all := Oid;
--       U_Oid := Oid_To_U_Oid (Oid_Access);
--       Free (Oid_Access);
--       return U_Oid;
--       --  ??? Does this work? Not tested yet.
--    end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
     return Object_Id_Access
   is
      Oid                : Object_Id_Access;
   begin
      Oid := new Object_Id'
        (Object_Id
         (Put_String    (U_Oid.Creator)
          & Put_String  (U_Oid.Id)
          & Put_Boolean (U_Oid.System_Generated)
          & Put_ULong   (U_Oid.Persistency_Flag)));

      pragma Debug (O ("Oid is " & Image (Oid.all)));
      return Oid;
   end U_Oid_To_Oid;

end PolyORB.POA_Types;
