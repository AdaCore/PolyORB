------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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
