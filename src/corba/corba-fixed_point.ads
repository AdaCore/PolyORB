------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . F I X E D _ P O I N T                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

--  This unit provides generic helper routines for fixed point numeric types

with Ada.Streams;

with PolyORB.Any;
with PolyORB.Types;

generic
   type F is delta <> digits <>;
package CORBA.Fixed_Point is

   function To_Any (Item : F)  return Any;
   function From_Any (Item : Any) return F;

   function Wrap (X : access F) return PolyORB.Any.Content'Class;

private

   use type PolyORB.Types.Unsigned_Long;

   Fixed_Content_Count : constant PolyORB.Types.Unsigned_Long :=
                           (F'Digits + 2) / 2;

   type F_Ptr is access all F;
   subtype F_Repr is Ada.Streams.Stream_Element_Array
     (0 .. Ada.Streams.Stream_Element_Offset (Fixed_Content_Count - 1));

   type Fixed_Content is new PolyORB.Any.Aggregate_Content with record
      V          : F_Ptr;

      Repr_Cache : F_Repr;
      --  We cache a representation of a fixed point value as an array of
      --  BCD octets, similar to the CDR encoding. This allows efficient
      --  access to these octets as aggregate elements. Modifications to
      --  the cache are reflected to V.all upon setting the last element of
      --  the array.

   end record;

   function Clone
     (ACC  : Fixed_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;

   procedure Finalize_Value
     (ACC : in out Fixed_Content);

   function Get_Aggregate_Element
     (ACC   : not null access Fixed_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;

   procedure Set_Aggregate_Element
     (ACC    : in out Fixed_Content;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);

   function Get_Aggregate_Count
     (ACC : Fixed_Content) return PolyORB.Types.Unsigned_Long;

   procedure Set_Aggregate_Count
     (ACC   : in out Fixed_Content;
      Count : PolyORB.Types.Unsigned_Long);

end CORBA.Fixed_Point;
