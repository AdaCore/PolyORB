------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      POLYORB.POA_POLICIES.IMPLICIT_ACTIVATION_POLICY.NO_ACTIVATION       --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

package body PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation is

   ------------
   -- Create --
   ------------

   function Create return No_Activation_Policy_Access is
   begin
      return new No_Activation_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : No_Activation_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : No_Activation_Policy)
     return String is
   begin
      return "IMPLICIT_ACTIVATION_POLICY.NO_ACTIVATION";
   end Policy_Id;

   ----------------------
   -- Activate_Servant --
   ----------------------

   function Activate_Servant
     (Self      : No_Activation_Policy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
   begin
      return null;
   end Activate_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     No_Activation_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (No_Activation_Policy_Access (Ptr));
   end Free;

end PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;
