------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.CORBA_P.INTERCEPTORS_POLICIES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2023, Free Software Foundation, Inc.          --
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

with CORBA.Policy;
with PolyORB.CORBA_P.Policy_Management;

package body PolyORB.CORBA_P.Interceptors_Policies is

   Registry : array (CORBA.PolicyType range 1 .. 60)
     of PortableInterceptor.PolicyFactory.Local_Ref;

   function Create_Policy
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref
   is
   begin
      pragma Assert
       (not PortableInterceptor.PolicyFactory.Is_Null (Registry (The_Type)));

      return
        PortableInterceptor.PolicyFactory.create_policy
         (Registry (The_Type), The_Type, Value);
   end Create_Policy;

   -----------------------------
   -- Register_Policy_Factory --
   -----------------------------

   procedure Register_Policy_Factory
     (IDL_Type       : CORBA.PolicyType;
      Policy_Factory : PortableInterceptor.PolicyFactory.Local_Ref)
   is
   begin
      if Policy_Management.Is_Registered (IDL_Type) then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.System_Exception_Members'(16, CORBA.Completed_No));
      end if;

      Registry (IDL_Type) := Policy_Factory;

      Policy_Management.Register
       (The_Type        => IDL_Type,
        POA_Level       => True,
        ORB_Level       => True,
        Thread_Level    => True,
        Reference_Level => True,
        Factory         => Create_Policy'Access);
      --  Implementation Note: we don't known real allowed levels for policy
      --  registered throught PortableInterceptor infrastructure, thus we
      --  always allow it usage on all levels.
   end Register_Policy_Factory;

end PolyORB.CORBA_P.Interceptors_Policies;
