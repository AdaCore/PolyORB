------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O R T A B L E S E R V E R . I D U N I Q U E N E S S P O L I C Y     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with CORBA.Policy;

package PortableServer.IdUniquenessPolicy is

   type Ref is new CORBA.Policy.Ref with private;

   function Create_Id_Uniqueness_Policy
     (Value : in PortableServer.IdUniquenessPolicyValue)
     return CORBA.Policy.Ref'Class;
   --  XXX This function should be in PortableServer,
   --  yet this would create a circular dependency ...

   function Create_Policy
     (The_Type : in CORBA.PolicyType;
      Val      : CORBA.Any)
     return PortableServer.IdUniquenessPolicy.Ref;
   --  Implementation of CORBA.ORB.Create_Policy.

   function Get_Value
     (Self : Ref)
     return PortableServer.IdUniquenessPolicyValue;

private

   type Ref is new CORBA.Policy.Ref with record
      IdUniquenessPolicy : PortableServer.IdUniquenessPolicyValue;
   end record;

end PortableServer.IdUniquenessPolicy;
