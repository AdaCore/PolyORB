------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 R T P O R T A B L E S E R V E R . P O A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

with CORBA.Object;
with PortableServer.POA;
with RTCORBA;

package RTPortableServer.POA is

   type Ref is new PortableServer.POA.Ref with private;

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref;

   function Create_Reference_With_Priority
     (Self      : in Ref;
      Intf      : in CORBA.RepositoryId;
      Priority  : in RTCORBA.Priority)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id_And_Priority
     (Self      : in Ref;
      Oid       : in PortableServer.ObjectId;
      Intf      : in CORBA.RepositoryId;
      Priority  : in RTCORBA.Priority)
     return CORBA.Object.Ref;

   function Activate_Object_With_Priority
     (Self       : in Ref;
      P_Servant  : in PortableServer.Servant;
      Priority   : in RTCORBA.Priority)
     return PortableServer.ObjectId;

   procedure Activate_Object_With_Id_And_Priority
     (Self      : in Ref;
      Oid       : in PortableServer.ObjectId;
      P_Servant : in PortableServer.Servant;
      Priority  : in RTCORBA.Priority);

private

   type Ref is new PortableServer.POA.Ref with null record;

end RTPortableServer.POA;
