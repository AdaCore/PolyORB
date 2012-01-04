------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        T E S T 0 0 0 _ S E T U P                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PortableServer.POA;
with Echo;

package Test000_Setup is

   use PortableServer;
   use PortableServer.Internals;
   use PortableServer.POA;

   -----------------------
   -- Utility functions --
   -----------------------

   procedure Init_Test;
   --  Initialize test.

   procedure Attach_Servant
     (To_POA  : PortableServer.POA.Local_Ref;
      Obj_Ref : out Echo.Ref);
   --  Attach an 'Echo' servant to 'To_POA' POA.

   procedure Invoke_On_Servant
     (Obj_Ref   : Echo.Ref;
      Reentrant : Boolean := False;
      Verbose   : Boolean := True);

   function Invoke_On_Servant
     (Obj_Ref   : Echo.Ref)
     return Boolean;
   --  Invoke on Servant 'Obj_Ref'.

   ------------------------
   -- POA Test functions --
   ------------------------

   procedure Test_Root_POA;
   --  Test Root_POA.

   procedure Test_POAManager;
   --  Test POA Manager behavior.

   procedure Test_Single_Thread_Policy;
   --  Test POA Single_Thread Thread Policy.

   procedure Test_Main_Thread_Policy;
   --  Test POA Main_Thread Thread Policy.

   procedure Test_Conversion (POA : PortableServer.POA.Local_Ref);
   --  Test Conversion functions under POA's configuration.

   function Create_POA_With_Policies
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return PortableServer.POA.Local_Ref;
   --  Regiter a Child POA of the RootPOA with the given policies.

   function Create_And_Destroy_POA
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Boolean;
   --  Create and destroy a POA, return 'True' if the operation was
   --  succesful.

   function Policies_Image
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return String;
   --  Image of this policies list.

   function Are_Policies_Valid
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Boolean;
   --  Return 'True' iff this policies list is an acceptable
   --  configuration for a CORBA POA.

   procedure Test_POA_Creation;
   --  Test PortableServer accepts only valid POA policies combination,

   procedure Test_POA_API;
   --  Test full POA API.

   procedure Test_POA_Hierarchy;
   --  Tests on POA trees

   procedure Test_OID;
   --  Tests on OID

end Test000_Setup;
