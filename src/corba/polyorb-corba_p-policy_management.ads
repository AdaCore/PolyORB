------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . P O L I C Y _ M A N A G E M E N T     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with CORBA.Policy;
with PolyORB.Annotations;
with PolyORB.Exceptions;

package PolyORB.CORBA_P.Policy_Management is

   type Policy_List is
     array (CORBA.PolicyType range 1 .. 60) of CORBA.Policy.Ref;

   type Policy_Manager_Note is new Annotations.Note with record
      Overrides : Policy_List;
   end record;

   Empty_Note : constant Policy_Manager_Note;

   Null_Policy : CORBA.Policy.Ref;

   type Policy_Factory is
     access function
       (IDL_Type : in CORBA.PolicyType;
        Value    : in CORBA.Any)
       return CORBA.Policy.Ref;
   --  Factory function may raise CORBA::PolicyError exception with
   --  BAD_POLICY_TYPE, BAD_POLICY_VALUE and UNSUPPORTED_POLICY_VALUE reasons.

   function Is_Registered (The_Type : in CORBA.PolicyType) return Boolean;
   --  Return True iff policy have been registered

   function Get_Policy_Factory
     (The_Type : in CORBA.PolicyType)
     return Policy_Factory;
   --  Return policy factory

   function Is_POA_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is a POA level policy

   function Is_ORB_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is an ORB level policy

   function Is_Thread_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is a thread level policy

   function Is_Reference_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff The_Type is an object reference policy

   type Compatibility_Check_Proc is
      access procedure
        (The_Policy : in     CORBA.Policy.Ref;
         Policies   : in     Policy_List;
         Indexes    :    out CORBA.Unsigned_Short);
   --  XXX Indexes type must be replaced by sequence<unsigned short> after
   --  implementation of CORBA.InvalidPolicies exception done.

   type Reconciliation_Proc is
      access procedure
        (Server_Policy : in     CORBA.Policy.Ref;
         Client_Policy : in     CORBA.Policy.Ref;
         Result_Policy :    out CORBA.Policy.Ref;
         Error         : in out Exceptions.Error_Container);

   procedure Register
     (The_Type            : in CORBA.PolicyType;
      POA_Level           : in Boolean                  := False;
      ORB_Level           : in Boolean                  := False;
      Thread_Level        : in Boolean                  := False;
      Reference_Level     : in Boolean                  := False;
      Factory             : in Policy_Factory           := null;
      Compatibility_Check : in Compatibility_Check_Proc := null;
      Reconciliation      : in Reconciliation_Proc      := null;
      System_Default      : in CORBA.Policy.Ref         := Null_Policy);
   --  Register CORBA Policy and define allowed policy usage.
   --   - The_Type            : policy id
   --   - POA_Level           : policy is allowed at POA level
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

   procedure Raise_PolicyError (Members : in CORBA.PolicyError_Members);
   pragma No_Return (Raise_PolicyError);

private

   Empty_Note : constant Policy_Manager_Note
     := (Annotations.Note with Overrides => (others => Null_Policy));

end PolyORB.CORBA_P.Policy_Management;
