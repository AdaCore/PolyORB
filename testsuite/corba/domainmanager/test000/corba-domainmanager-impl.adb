------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O R B A . D O M A I N M A N A G E R . I M P L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with CORBA.DomainManager.Skel;
pragma Warnings (Off, CORBA.DomainManager.Skel);

package body CORBA.DomainManager.Impl is

   -----------------------
   -- Get_Domain_Policy --
   -----------------------

   function Get_Domain_Policy
     (Self        : access Object;
      Policy_Type : CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Policy_Type);

      Result : CORBA.Policy.Ref;
   begin
      CORBA.Raise_Inv_Policy (CORBA.Default_Sys_Member);
      return Result;
   end Get_Domain_Policy;

end CORBA.DomainManager.Impl;
