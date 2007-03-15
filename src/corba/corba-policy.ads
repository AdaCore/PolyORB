------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . P O L I C Y                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with CORBA.Sequences.Unbounded;

package CORBA.Policy is

   --  Implementation note: The Ada mapping defines
   --     type Ref is new abstract CORBA.Object.Ref with null record;
   --  This raises Ada semantics error when defining IDL_SEQUENCE_Policy
   --  and CORBA.ORB.Create_Policy. We modified type Ref to reduce impacts on
   --  others parts of this CORBA implementation

   type Ref is new CORBA.Object.Ref with null record;

   function Get_Policy_Type
     (Self : Ref)
     return PolicyType;

   function Copy
     (Self : Ref'Class)
     return Ref'Class;

   --  Destroy unneeded
   --    procedure Destroy (Self : Ref);

   --  Implementation note: these two Sequence types should be defined
   --  in package CORBA. Yet, this would create circular dependencies
   --  between CORBA and CORBA.Sequences.

   package IDL_SEQUENCE_Policy is new
     CORBA.Sequences.Unbounded (Ref);

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
