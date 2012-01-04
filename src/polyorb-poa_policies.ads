------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . P O A _ P O L I C I E S                  --
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

--  Base types for the various configuration axes (policies)
--  of the PolyORB Portable Object Adapter (liberally inspired from
--  the POA specification in CORBA).

with PolyORB.Errors;
with PolyORB.Utils.Chained_Lists;

package PolyORB.POA_Policies is

   --  No proper body: no elaboration control.

   type Policy is abstract tagged limited private;
   type Policy_Access is access all Policy'Class;

   package Policy_Lists is new PolyORB.Utils.Chained_Lists (Policy_Access);
   subtype PolicyList is Policy_Lists.List;

   type AllPolicies is array (1 .. 7) of Policy_Access;

   function Policy_Id (Self : Policy) return String is abstract;
   --  Return Policy name

   procedure Check_Compatibility
     (Self           :        Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

private

   type Policy is abstract tagged limited null record;

end PolyORB.POA_Policies;
