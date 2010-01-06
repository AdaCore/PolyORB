------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . D O M A I N M A N A G E R                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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
with CORBA.Policy;
with CORBA.Sequences.Unbounded;

package CORBA.DomainManager is

   type Ref is new CORBA.Object.Ref with null record;

   function Get_Domain_Policy
     (Self        : Ref;
      Policy_Type : CORBA.PolicyType)
      return CORBA.Policy.Ref;

   function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

   --  Implementation note: this Sequence type should be defined in
   --  package CORBA. Yet, this would create circular dependencies
   --  between CORBA and CORBA.Sequences.

   package IDL_SEQUENCE_DomainManager is
     new CORBA.Sequences.Unbounded (Ref);

   subtype DomainManagersList is IDL_SEQUENCE_DomainManager.Sequence;
   --  Implementation Note: the IDL-to-Ada mapping defines the
   --  DomainManagersList type as:
   --    type DomainManagersList is
   --         new IDL_SEQUENCE_CORBA_DomainManager.Sequence;
   --
   --  This adds new primitives to Ref that requires overriding for
   --  any derivation of Ref. We define DomainManagersList as a
   --  subtype to avoid this behavior.

   Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/DomainManager:1.0";

private

   function Is_A (Logical_Type_Id : Standard.String) return CORBA.Boolean;

end CORBA.DomainManager;
