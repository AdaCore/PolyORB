------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  RTCOSSCHEDULING.SERVERSCHEDULER.IMPL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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
with PortableServer.POAManager;
with CORBA;
with PortableServer.POA;
with CORBA.Local;

package RTCosScheduling.ServerScheduler.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function Create_POA
     (Self         : access Object;
      Parent       : PortableServer.POA.Local_Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Local_Ref;
      Policies     : CORBA.Policy.PolicyList)
     return PortableServer.POA.Local_Ref;
   --  Implementation Note: this function may use any POA RT policies,
   --  as listed in ServerScheduler configuration file.

   procedure Schedule_Object
     (Self : access Object;
      Obj  : CORBA.Object.Ref;
      Name : CORBA.String);

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

   --  Implementation Note: We take advantage of the permissions
   --  detailled in RT-CORBA 1.1 (3.1) to add a PolyORB specific
   --  interface for initialization.

   procedure Load_Configuration_File (Conf_File_Name : String);
   --  Load the content of Conf_File_Name into PolyORB configuration table

private

   type Object is new CORBA.Local.Object with null record;

end RTCosScheduling.ServerScheduler.Impl;
