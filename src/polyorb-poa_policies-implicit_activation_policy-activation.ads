------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.ACTIVATION         --
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

package PolyORB.POA_Policies.Implicit_Activation_Policy.Activation is

   type Activation_Policy is new ImplicitActivationPolicy with null record;

   type Activation_Policy_Access is access all Activation_Policy;

   function Create
     return Activation_Policy_Access;

   procedure Check_Compatibility
     (Self           :        Activation_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Exceptions.Error_Container);

   function Policy_Id
     (Self : Activation_Policy)
     return String;

   procedure Implicit_Activate_Servant
     (Self      :        Activation_Policy;
      OA        :        PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant :        Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Exceptions.Error_Container);

   procedure Ensure_No_Implicit_Activation
     (Self      :        Activation_Policy;
      Error     : in out PolyORB.Exceptions.Error_Container);

end PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;