------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . P O L I C Y _ M A N A G E M E N T     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Annotations;
with PolyORB.Errors;

package PolyORB.CORBA_P.Policy_Management is

   type Policy_List is
     array (CORBA.PolicyType range 1 .. 60) of CORBA.Policy.Ref;

   type Policy_Manager_Note is new Annotations.Note with record
      Overrides : Policy_List;
   end record;

   Empty_Policy_Manager_Note : constant Policy_Manager_Note;

   Null_Policy : CORBA.Policy.Ref;

   type Policy_Override_Level is
     (POA_Level, Domain_Level, ORB_Level, Thread_Level, Reference_Level);
   --  Level of policy overrides

   type Policy_Factory is
     access function
       (IDL_Type : CORBA.PolicyType;
        Value    : CORBA.Any)
       return CORBA.Policy.Ref;
   --  Factory function may raise CORBA::PolicyError exception with
   --  BAD_POLICY_TYPE, BAD_POLICY_VALUE and UNSUPPORTED_POLICY_VALUE reasons.

   function Is_Registered (The_Type : CORBA.PolicyType) return Boolean;
   --  Return True iff policy have been registered

   function Get_Policy_Factory
     (The_Type : CORBA.PolicyType)
     return Policy_Factory;
   --  Return policy factory

   function Is_POA_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is a POA level policy

   function Is_Domain_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is a Domain level policy

   function Is_ORB_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is an ORB level policy

   function Is_Thread_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is a thread level policy

   function Is_Reference_Policy
     (The_Type : CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is an object reference policy

   function Policy_System_Default_Value
     (The_Type : CORBA.PolicyType)
     return CORBA.Policy.Ref;
   --  Return system default value for given policy type

   procedure Add_Policy_Overrides
     (To       : in out Policy_List;
      Policies : CORBA.Policy.PolicyList;
      Level    : Policy_Override_Level);
   --  Add policy overrides to exists policies.
   --  Raise BAD_PARAM with Minor code 30 iff Policies contents two
   --  policies of the same type.
   --  Raise NO_PERMISSION system exception iff policy override is not
   --  allowed at given level.

   function Get_Policy_Overrides
     (From : Policy_List;
      TS   : CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList;
   --  Return the list of overriden policies for requested policy
   --  types. Return all overriden policies if policy type sequence
   --  is empty. If there is no overriden policies then return empty list.

   procedure Check_Compatibility
     (Policies : Policy_List;
      Indexes  : out CORBA.Unsigned_Short);
   --  Check compatibility of policies, defined in Policies list.
   --  If the incompatibility check failed then Indexes contain the
   --  index of the first incompatible policy.
   --  XXX After the implementation of CORBA::InvalidPolicies exception,
   --  Indexes will be a sequence of incompatible policies indexes.

   type Compatibility_Check_Proc is
      access procedure
        (The_Policy : CORBA.Policy.Ref;
         Policies   : Policy_List;
         Indexes    :    out CORBA.Unsigned_Short);
   --  XXX Indexes type must be replaced by sequence<unsigned short> after
   --  the implementation of CORBA.InvalidPolicies exception is done.

   type Reconciliation_Proc is
      access procedure
        (Server_Policy : CORBA.Policy.Ref;
         Client_Policy : CORBA.Policy.Ref;
         Result_Policy :    out CORBA.Policy.Ref;
         Error         : in out Errors.Error_Container);

   procedure Register
     (The_Type            : CORBA.PolicyType;
      POA_Level           : Boolean                  := False;
      ORB_Level           : Boolean                  := False;
      Thread_Level        : Boolean                  := False;
      Reference_Level     : Boolean                  := False;
      Domain_Level        : Boolean                  := False;
      Factory             : Policy_Factory           := null;
      Compatibility_Check : Compatibility_Check_Proc := null;
      Reconciliation      : Reconciliation_Proc      := null;
      System_Default      : CORBA.Policy.Ref         := Null_Policy);
   --  Register CORBA Policy and define allowed policy usage.
   --   - The_Type            : policy id
   --   - POA_Level           : policy is allowed at POA level
   --   - Domain_Level        : policy is allowed at Domain level
   --   - ORB_Level           : policy is allowed at ORB level
   --   - Thread_Level        : policy is allowed at thread level
   --   - Reference_Level     : policy is allowed at object reference level
   --   - Factory             : policy factory
   --   - Compatibility_Check : subprogram used to check policy
   --                           compatibility with over policies at same level
   --   - Reconciliation      : subprogram used to perform policy
   --                           reconciliation for policies defined at both
   --                           client and server levels
   --   - Default             : system default value; may be null reference

   --  Exception helpers

   procedure Raise_PolicyError (Members : CORBA.PolicyError_Members);
   pragma No_Return (Raise_PolicyError);

private

   Empty_Policy_Manager_Note : constant Policy_Manager_Note
     := (Annotations.Note with Overrides => (others => Null_Policy));

end PolyORB.CORBA_P.Policy_Management;
