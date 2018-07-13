------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . D O M A I N M A N A G E R                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

pragma Ada_2012;

with CORBA.Object;
with CORBA.Policy;
with CORBA.Sequences.Unbounded;

package CORBA.DomainManager is

   type Ref is new CORBA.Object.Ref with null record;

   function Get_Domain_Policy
     (Self        : Ref;
      Policy_Type : CORBA.PolicyType) return CORBA.Policy.Ref;

   overriding function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean;

   --  Implementation note: this Sequence type should be defined in
   --  package CORBA. Yet, this would create circular dependencies
   --  between CORBA and CORBA.Sequences.

   package IDL_SEQUENCE_DomainManager is new CORBA.Sequences.Unbounded (Ref);

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
