------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

with PolyORB.POA_Types;     use PolyORB.POA_Types;

package PolyORB.POA_Policies.Implicit_Activation_Policy is

   type ImplicitActivationPolicy is abstract new Policy with null record;
   subtype Implicit_Activation_Policy is ImplicitActivationPolicy;
   type ImplicitActivationPolicy_Access is
     access all ImplicitActivationPolicy'Class;
   subtype Implicit_Activation_Policy_Access is
     ImplicitActivationPolicy_Access;

   function Create return ImplicitActivationPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   function Activate_Servant (Self      : ImplicitActivationPolicy;
                              OA        : PolyORB.POA_Types.Obj_Adapter_Access;
                              P_Servant : Servant_Access)
                             return Object_Id_Access
      is abstract;
   --  Case NO_ACTIVATION:
   --    Returns null
   --  Case ACTIVATION:
   --    Activates the servant in the Active Object Map.

   procedure Free (P   : in     ImplicitActivationPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

end PolyORB.POA_Policies.Implicit_Activation_Policy;
