------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               C O R B A . V A L U E . B O X . H E L P E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2011, Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

generic
   with function Element_From_Any (Item : CORBA.Any) return Boxed;
   with function Element_To_Any   (Item : Boxed) return CORBA.Any;
   with function Element_Wrap
     (X : access Boxed) return PolyORB.Any.Content'Class;

package CORBA.Value.Box.Helper is

   function From_Any (Item : CORBA.Any) return Box_Ref;
   function To_Any   (Item : Box_Ref) return CORBA.Any;
   function Wrap (X : access Box_Ref) return PolyORB.Any.Content'Class;

   procedure Initialize (Element_TC, Box_Ref_TC : CORBA.TypeCode.Object);

private

   --  Aggregate container

   type Box_Ref_Ptr is access all Box_Ref;
   type Box_Ref_Content is new PolyORB.Any.Aggregate_Content with record
      V : Box_Ref_Ptr;
   end record;

   --  Aggregate container primitives

   function Get_Aggregate_Element
     (ACC   : not null access Box_Ref_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;

--   procedure Set_Aggregate_Element
--     (ACC    : in out Box_Ref_Content;
--      TC     : PolyORB.Any.TypeCode.Object;
--      Index  : Types.Unsigned_Long;
--      From_C : in out PolyORB.Any.Any_Container'Class);

   function Get_Aggregate_Count
     (ACC : Box_Ref_Content) return PolyORB.Types.Unsigned_Long;

   procedure Set_Aggregate_Count
     (ACC   : in out Box_Ref_Content;
      Count : PolyORB.Types.Unsigned_Long);

   function Clone
     (ACC  : Box_Ref_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;

   procedure Finalize_Value (ACC : in out Box_Ref_Content);

end CORBA.Value.Box.Helper;
