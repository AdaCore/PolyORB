------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ C L I E N T _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PortableInterceptor.ClientRequestInfo;

with Test001_Globals;

package Test001_Client_Request_Info_Tests is

   procedure Test_Target
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Effective_Target
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Effective_Profile
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Received_Exception
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Received_Exception_Id
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Effective_Component
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Effective_Components
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Request_Policy
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Add_Request_Service_Context
     (Point : in     Test001_Globals.Client_Interception_Point;
      Info  : in     PortableInterceptor.ClientRequestInfo.Local_Ref);

end Test001_Client_Request_Info_Tests;
