------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY.MULTIPLE             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

package PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple is

   type Multiple_Id_Policy is new IdUniquenessPolicy with null record;
   type Multiple_Id_Policy_Access is access all Multiple_Id_Policy;

   function Create return Multiple_Id_Policy_Access;

   procedure Check_Compatibility
     (Self : Multiple_Id_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access);

   function Policy_Id
     (Self : Multiple_Id_Policy)
     return String;

   procedure Ensure_Servant_Uniqueness
     (Self      : Multiple_Id_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Objects.Servant_Access);

   function Activate_Again
     (Self      : Multiple_Id_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Objects.Servant_Access;
      Oid       : Object_Id_Access)
     return Object_Id_Access;

end PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
