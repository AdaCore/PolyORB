------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . P O L I C Y                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

pragma Ada_2005;

with CORBA.Object;
with CORBA.Sequences.Unbounded;

package CORBA.Policy is

   --  Implementation note: The Ada mapping defines
   --     type Ref is new abstract CORBA.Object.Ref with null record;
   --  This raises Ada semantics error when defining IDL_SEQUENCE_Policy
   --  and CORBA.ORB.Create_Policy. We modified type Ref to reduce impacts on
   --  others parts of this CORBA implementation

   type Ref is new CORBA.Object.Ref with null record;

   function Get_Policy_Type (Self : Ref) return PolicyType;

   function Copy (Self : Ref'Class) return Ref'Class;

   --  Destroy unneeded
   --    procedure Destroy (Self : Ref);

   --  Implementation note: these two Sequence types should be defined
   --  in package CORBA. Yet, this would create circular dependencies
   --  between CORBA and CORBA.Sequences.

   package IDL_SEQUENCE_Policy is new CORBA.Sequences.Unbounded (Ref);

   subtype PolicyList is IDL_SEQUENCE_Policy.Sequence;
   --  Implementation Note: the IDL-to-Ada mapping defines the
   --  PolicyList type as:
   --    type PolicyList is new IDL_SEQUENCE_Policy.Sequence;
   --
   --  This adds new primitives to Ref that requires overriding for
   --  any derivation of Ref. We define PolicyList as a subtype to
   --  avoid this behavior.

   package IDL_SEQUENCE_PolicyType is new
     CORBA.Sequences.Unbounded (PolicyType);

   subtype PolicyTypeSeq is IDL_SEQUENCE_PolicyType.Sequence;

   Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/Policy:1.0";

end CORBA.Policy;
