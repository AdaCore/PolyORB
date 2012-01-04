------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ C L I E N T _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PortableInterceptor.ClientRequestInfo;

with Test001_Globals;

package Test001_Client_Request_Info_Tests is

   procedure Test_Target
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Effective_Target
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Effective_Profile
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Received_Exception
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Received_Exception_Id
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Effective_Component
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Effective_Components
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Get_Request_Policy
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Test_Add_Request_Service_Context
     (Point : Test001_Globals.Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref);

end Test001_Client_Request_Info_Tests;
