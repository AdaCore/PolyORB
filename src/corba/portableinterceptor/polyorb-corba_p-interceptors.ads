------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S          --
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

with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.IORInterceptor;
with PortableInterceptor.ORBInitializer;
with PortableInterceptor.ServerRequestInterceptor;

package PolyORB.CORBA_P.Interceptors is

   --  Client interceptors

   type Client_Interception_Point is
     (Send_Request,
      Send_Poll,
      Receive_Reply,
      Receive_Exception,
      Receive_Other);

   function Is_Client_Request_Interceptor_Exists
     (Name : String)
      return Boolean;

   procedure Add_Client_Request_Interceptor
     (Interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   --  Server interceptors

   type Server_Interception_Point is
     (Receive_Request_Service_Contexts,
      Receive_Request,
      Send_Reply,
      Send_Exception,
      Send_Other);

   function Is_Server_Request_Interceptor_Exists
     (Name : String)
      return Boolean;

   procedure Add_Server_Request_Interceptor
     (Interceptor : PortableInterceptor.ServerRequestInterceptor.Local_Ref);

   --  IOR interceptors

   function Is_IOR_Interceptor_Exists
     (Name : String)
      return Boolean;

   procedure Add_IOR_Interceptor
     (Interceptor : PortableInterceptor.IORInterceptor.Local_Ref);

   --  ORB Initializers

   procedure Register_ORB_Initializer
     (Init : PortableInterceptor.ORBInitializer.Local_Ref);
   --  Register Interceptor initializer object

   procedure Call_ORB_Initializers;
   --  Call pre_init and post_init operations for all registered initializers.
   --  XXX  This is a temporary workaround, and after improvement of
   --  PolyORB initialization these operations must be called from ORB_init.

--   procedure Pre_Init_Interceptors
--     (Info : PortableInterceptor.ORBInitInfo.Local_Ref);
--   --  Call Pre_Init method on all registered initializers.
--
--   procedure Post_Init_Interceptors
--     (Info : PortableInterceptor.ORBInitInfo.Local_Ref);
--   --  Call Post_Init method on all registered initializers.

end PolyORB.CORBA_P.Interceptors;
