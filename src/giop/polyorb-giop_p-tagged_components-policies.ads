------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  TAG_POLICIES tagged component

with PolyORB.Representations.CDR;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.GIOP_P.Tagged_Components.Policies is

   use PolyORB.GIOP_P.Tagged_Components;

   type TC_Policies is new Tagged_Component (Tag_Policies) with private;

   type TC_Policies_Access is access all TC_Policies;

   procedure Marshall (C : access TC_Policies; Buffer : access Buffer_Type);

   procedure Unmarshall (C : access TC_Policies; Buffer : access Buffer_Type);

   procedure Release_Contents (C : access TC_Policies);

private

   type Encapsulation_Access is access all
     PolyORB.Representations.CDR.Encapsulation;

   type Policy_Value is record
      P_Type  : PolyORB.Types.Unsigned_Long;
      P_Value : Encapsulation_Access;
   end record;

   Invalid_Policy_Type : constant := PolyORB.Types.Unsigned_Long'Last;

   type Fetch_Sub_Component_Func_Access is access
     function (Oid : access PolyORB.Objects.Object_Id)
              return Policy_Value;

   procedure Register (Fetch_Sub_Component : Fetch_Sub_Component_Func_Access);
   --  Register an allocator for a TAG_POLICIES sub component

   package Policy_Value_Seq is new PolyORB.Utils.Chained_Lists (Policy_Value);
   --  Implementation Note: CORBA/GIOP defines Policy_Value_Seq as an
   --  unbounded sequence. We implement it using as a chain list to
   --  avoid dragging unbounded sequences, which is unneeded.

   type TC_Policies is new Tagged_Component (Tag_Policies) with record
      Policies : Policy_Value_Seq.List;
   end record;

end PolyORB.GIOP_P.Tagged_Components.Policies;
