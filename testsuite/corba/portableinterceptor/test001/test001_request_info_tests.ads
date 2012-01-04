------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           T E S T 0 0 1 _ R E Q U E S T _ I N F O _ T E S T S            --
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

with PortableInterceptor.RequestInfo;

with Test001_Globals;

package Test001_Request_Info_Tests is

   procedure Test_Request_Id
     (Point    : Test001_Globals.Interception_Point;
      Info     : PortableInterceptor.RequestInfo.Local_Ref'Class;
      Suppress : Boolean := False);

   procedure Test_Operation
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Arguments
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Exceptions
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Contexts
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Operation_Context
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Result
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Response_Expected
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Sync_Scope
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Reply_Status
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Forward_Reference
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Get_Slot
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Get_Request_Service_Context
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

   procedure Test_Get_Reply_Service_Context
     (Point : Test001_Globals.Interception_Point;
      Info  : PortableInterceptor.RequestInfo.Local_Ref'Class);

end Test001_Request_Info_Tests;
