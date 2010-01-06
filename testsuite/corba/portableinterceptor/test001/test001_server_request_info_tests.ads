------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ S E R V E R _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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

with PortableInterceptor.ServerRequestInfo;

with Test001_Globals;

package Test001_Server_Request_Info_Tests is

   procedure Test_Sending_Exception
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Object_Id
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Adapter_Id
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Server_Id
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_ORB_Id
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Adapter_Name
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Target_Most_Derived_Interface
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Get_Server_Policy
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Set_Slot
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Target_Is_A
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Test_Add_Reply_Service_Context
     (Point  : Test001_Globals.Server_Interception_Point;
      Info   : PortableInterceptor.ServerRequestInfo.Local_Ref);

end Test001_Server_Request_Info_Tests;
