------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.LIFESPAN_POLICY.TRANSIENT               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

package PolyORB.POA_Policies.Lifespan_Policy.Transient is

   type Transient_Policy is new LifespanPolicy with null record;
   type Transient_Policy_Access is access all Transient_Policy;

   function Create return Transient_Policy_Access;

   procedure Check_Compatibility
     (Self : Transient_Policy;
      Other_Policies   : AllPolicies);

   function Policy_Id
     (Self : Transient_Policy)
     return String;

   function Get_Lifespan_Cookie
     (Self : Transient_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
     return Lifespan_Cookie;

   procedure Ensure_Lifespan
     (Self  : Transient_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid);

end PolyORB.POA_Policies.Lifespan_Policy.Transient;
