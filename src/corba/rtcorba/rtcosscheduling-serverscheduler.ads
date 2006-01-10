------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      R T C O S S C H E D U L I N G . S E R V E R S C H E D U L E R       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Policy;
with PortableServer.POAManager;
with CORBA;
with PortableServer.POA;
with CORBA.Object;

package RTCosScheduling.ServerScheduler is

   type Local_Ref is new CORBA.Object.Ref with null record;

   function Create_POA
     (Self         : Local_Ref;
      Parent       : PortableServer.POA.Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Policies     : CORBA.Policy.PolicyList)
     return PortableServer.POA.Ref;

   create_POA_Repository_Id : constant Standard.String
     := "IDL:omg.org/RTCosScheduling/ServerScheduler/create_POA:1.0";

   procedure Schedule_Object
     (Self : Local_Ref;
      Obj  : CORBA.Object.Ref;
      Name : CORBA.String);

   schedule_object_Repository_Id : constant Standard.String
     := "IDL:omg.org/RTCosScheduling/ServerScheduler/schedule_object:1.0";

   Repository_Id : constant Standard.String
     := "IDL:omg.org/RTCosScheduling/ServerScheduler:1.0";

end RTCosScheduling.ServerScheduler;
