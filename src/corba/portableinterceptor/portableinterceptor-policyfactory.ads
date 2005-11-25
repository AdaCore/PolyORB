------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O R T A B L E I N T E R C E P T O R . P O L I C Y F A C T O R Y     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with CORBA.Policy;

package PortableInterceptor.PolicyFactory is

   type Local_Ref is new CORBA.Object.Ref with null record;

   function Create_Policy
     (Self     : in Local_Ref;
      IDL_Type : in CORBA.PolicyType;
      Value    : in CORBA.Any)
     return CORBA.Policy.Ref;

   --  Repository Ids

   Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/PolicyFactory:1.0";

   Create_Policy_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/PolicyFactory/create_policy:1.0";

end PortableInterceptor.PolicyFactory;
