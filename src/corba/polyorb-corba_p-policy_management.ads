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

--  $Id$

with CORBA.Policy;
--  with PolyORB.Annotations;

package PolyORB.CORBA_P.Policy_Management is

--   type Policy_List is
--     array (CORBA.PolicyType range 1 .. 60) of CORBA.Policy.Ref;
--
--   type Policy_Manager_Note is new Annotations.Note with record
--      Overrides : Policy_List;
--   end record;

   type Policy_Factory is
     access function
       (IDL_Type : in CORBA.PolicyType;
        Value    : in CORBA.Any)
       return CORBA.Policy.Ref;
   --  Factory function may raise CORBA::PolicyError exception with
   --  BAD_POLICY_TYPE, BAD_POLICY_VALUE and UNSUPPORTED_POLICY_VALUE reasons.

   function Is_Registered (The_Type : in CORBA.PolicyType) return Boolean;
   --  Return True iff policy have been registered.

   function Get_Policy_Factory
     (The_Type : in CORBA.PolicyType)
     return Policy_Factory;
   --  Return policy factory.

   function Is_Client_Side_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff policy is a client-side override

   function Is_Server_Side_Policy
     (The_Type : in CORBA.PolicyType)
     return Boolean;
   --  Return True iff policy is a server-side override

   procedure Register
     (The_Type    : in CORBA.PolicyType;
      Default     : in CORBA.Policy.Ref;
      Factory     : in Policy_Factory;
      Client_Side : in Boolean;
      Server_Side : in Boolean);
   --  Register CORBA Policy and define allowed policy usage.
   --  Paramters:
   --   - The_Type           : policy id
   --   - Default            : system default value; may be null reference
   --   - Factory            : policy factory
   --   - Client_Side        : policy is client-side override policy
   --   - Server_Side        : policy is server-side override policy

   --  Exception helpers

   procedure Raise_PolicyError (Members : in CORBA.PolicyError_Members);
   pragma No_Return (Raise_PolicyError);

end PolyORB.CORBA_P.Policy_Management;
