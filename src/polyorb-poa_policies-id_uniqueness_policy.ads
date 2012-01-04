------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.POA_POLICIES.ID_UNIQUENESS_POLICY                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.POA_Policies.Id_Uniqueness_Policy is

   use PolyORB.POA_Types;

   type IdUniquenessPolicy is abstract new Policy with null record;

   type IdUniquenessPolicy_Access is access all IdUniquenessPolicy'Class;

   procedure Ensure_Servant_Uniqueness
     (Self      :        IdUniquenessPolicy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Error     : in out PolyORB.Errors.Error_Container)
     is abstract;
   --  Case UNIQUE_ID:
   --  Checks that the specified servant is not yet in the Active Objects Map.
   --  If not, throws a ServantAlreadyActive exception.
   --  Case MULTIPLE_ID:
   --  Does nothing

   procedure Activate_Again
     (Self      :        IdUniquenessPolicy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Oid       :        Object_Id_Access;
      Result    :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Case UNIQUE_ID:
   --    if Oid is not null, return Oid, else try implicit
   --    activation and return the created Oid.
   --  Case MULTIPLE_ID: try implicit activation and return
   --    the created oid ((regardless of the Oid argument).

end PolyORB.POA_Policies.Id_Uniqueness_Policy;
