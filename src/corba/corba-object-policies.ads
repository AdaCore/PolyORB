------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . O B J E C T . P O L I C I E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.DomainManager;
with CORBA.Policy;

package CORBA.Object.Policies is

   function Get_Policy
     (Self        : in Ref;
      Policy_Type : in PolicyType)
      return CORBA.Policy.Ref;

   function Get_Domain_Managers
     (Self : in Ref'Class)
      return CORBA.DomainManager.DomainManagersList;

   procedure Set_Policy_Overrides
     (Self     : in Ref'Class;
      Policies : in CORBA.Policy.PolicyList;
      Set_Add  : in SetOverrideType);

   function Get_Client_Policy
     (Self     : in Ref'Class;
      The_Type : in PolicyType)
      return CORBA.Policy.Ref;

   function Get_Policy_Overrides
     (Self  : in Ref'Class;
      Types : in CORBA.Policy.PolicyTypeSeq)
      return CORBA.Policy.PolicyList;

--   procedure Validate_Connection
--     (Self                  : in     Ref;
--      Inconsistent_Policies :    out CORBA.Policy.PolicyList;
--      Result                :    out Boolean);

end CORBA.Object.Policies;
