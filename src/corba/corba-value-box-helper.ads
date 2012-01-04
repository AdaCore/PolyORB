------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               C O R B A . V A L U E . B O X . H E L P E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2011-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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
