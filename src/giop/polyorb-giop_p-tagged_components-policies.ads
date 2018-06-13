------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

--  TAG_POLICIES tagged component

with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.GIOP_P.Tagged_Components.Policies is

   type TC_Policies is new Tagged_Component
     (Tag => Tag_Policies, At_Most_Once => True) with private;
   --  Note: the at-most-once semantics of this component is not
   --  specified in the CORBA specification, par. 22.3.2, use default
   --  value.

   overriding procedure Marshall_Component_Data
     (C : access TC_Policies; Buffer : access Buffer_Type);

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Policies;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   overriding procedure Release_Contents (C : access TC_Policies);

   overriding function Duplicate
     (C : TC_Policies)
     return Tagged_Component_Access;

private

   type Encapsulation_Access is access all
     PolyORB.Representations.CDR.Common.Encapsulation;

   type Policy_Value is record
      P_Type  : PolyORB.Types.Unsigned_Long;
      P_Value : Encapsulation_Access;
   end record;

   Invalid_Policy_Type : constant := PolyORB.Types.Unsigned_Long'Last;

   type Fetch_Sub_Component_Func_Access is access
     function (Oid : access PolyORB.Objects.Object_Id) return Policy_Value;

   procedure Register (Fetch_Sub_Component : Fetch_Sub_Component_Func_Access);
   --  Register an allocator for a TAG_POLICIES sub component

   package Policy_Value_Seq is new PolyORB.Utils.Chained_Lists (Policy_Value);
   --  Implementation Note: CORBA/GIOP defines Policy_Value_Seq as an
   --  unbounded sequence. We implement it using as a chain list to
   --  avoid dragging unbounded sequences, which is unneeded.

   type TC_Policies is new Tagged_Component
     (Tag => Tag_Policies, At_Most_Once => True)
     with record
        Policies : Policy_Value_Seq.List;
     end record;

end PolyORB.GIOP_P.Tagged_Components.Policies;
