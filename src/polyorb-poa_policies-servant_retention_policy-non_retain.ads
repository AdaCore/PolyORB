------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.POA_POLICIES.SERVANT_RETENTION_POLICY.NON_RETAIN          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

package PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain is

   type Non_Retain_Policy is new ServantRetentionPolicy with null record;

   type Non_Retain_Policy_Access is access all Non_Retain_Policy;

   function Create
     return Non_Retain_Policy_Access;

   procedure Check_Compatibility
     (Self           :        Non_Retain_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Exceptions.Error_Container);

   function Policy_Id
     (Self : Non_Retain_Policy)
     return String;

   procedure Retain_Servant_Association
     (Self      :        Non_Retain_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      U_Oid     :        Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container);

   procedure Forget_Servant_Association
     (Self  :        Non_Retain_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid :        Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container);

   function Retained_Servant_To_Id
     (Self      : Non_Retain_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servants.Servant_Access)
     return Object_Id_Access;

   procedure Retained_Id_To_Servant
     (Self    :        Non_Retain_Policy;
      OA      :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid   :        Unmarshalled_Oid;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Ensure_Servant_Manager_Type
     (Self    :        Non_Retain_Policy;
      Manager :        ServantManager'Class;
      Error   : in out PolyORB.Exceptions.Error_Container);

end PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
