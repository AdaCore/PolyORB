------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY                 --
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

with PolyORB.POA_Types;               use PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.POA_Policies.Id_Uniqueness_Policy is

   type IdUniquenessPolicy is abstract new Policy with null record;
   subtype Id_Uniqueness_Policy is IdUniquenessPolicy;
   type IdUniquenessPolicy_Access is access all IdUniquenessPolicy'Class;
   subtype Id_Uniqueness_Policy_Access is IdUniquenessPolicy_Access;

   procedure Ensure_Servant_Uniqueness
     (Self      : IdUniquenessPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access)
     is abstract;
   --  Case UNIQUE_ID:
   --  Checks that the specified servant is not yet in the Active Objects Map.
   --  If not, throws a ServantAlreadyActive exception.
   --  Case MULTIPLE_ID:
   --  Does nothing

   function Activate_Again
     (Self      : IdUniquenessPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access;
      Oid : Object_Id_Access)
     return Object_Id_Access
      is abstract;
   --  Case UNIQUE_ID:
   --    if Oid is not null, return Oid, else try implicit
   --    activation and return the created Oid.
   --  Case MULTIPLE_ID: try implicit activation and return
   --    the created oid ((regardless of the Oid argument).

end PolyORB.POA_Policies.Id_Uniqueness_Policy;
