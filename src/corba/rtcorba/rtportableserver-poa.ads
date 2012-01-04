------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 R T P O R T A B L E S E R V E R . P O A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with PortableServer.POA;
with RTCORBA;

package RTPortableServer.POA is

   --  Implementation Note: RT-CORBA specifications states these
   --  functions may fail and raise CORBA.BAD_PARAM if the priority
   --  parameter doest not match the priority configuration for
   --  ressources assigned to the POA.
   --
   --  As a consequence, PolyORB will raise BAD_PARAM if either the
   --  POA doest not support a ThreadPoolPolicy or if the set up of
   --  attached ThreadPoolPolicy doest not match the priority
   --  parameter.

   type Local_Ref is new PortableServer.POA.Local_Ref with private;

   function Create_Reference_With_Priority
     (Self      : Local_Ref;
      Intf      : CORBA.RepositoryId;
      Priority  : RTCORBA.Priority)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id_And_Priority
     (Self      : Local_Ref;
      Oid       : PortableServer.ObjectId;
      Intf      : CORBA.RepositoryId;
      Priority  : RTCORBA.Priority)
     return CORBA.Object.Ref;

   function Activate_Object_With_Priority
     (Self       : Local_Ref;
      P_Servant  : PortableServer.Servant;
      Priority   : RTCORBA.Priority)
     return PortableServer.ObjectId;

   procedure Activate_Object_With_Id_And_Priority
     (Self      : Local_Ref;
      Oid       : PortableServer.ObjectId;
      P_Servant : PortableServer.Servant;
      Priority  : RTCORBA.Priority);

   Repository_Id : constant Standard.String
     := "IDL:omg.org/RTPortableServer/POA:1.0";

private

   type Local_Ref is new PortableServer.POA.Local_Ref with null record;

end RTPortableServer.POA;
