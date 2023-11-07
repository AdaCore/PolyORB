------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 0 _ I D L . O R B I N I T I A L I Z E R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2023, Free Software Foundation, Inc.          --
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

with CORBA;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.ServerRequestInterceptor;
with PolyORB.Smart_Pointers;

with Test000_Idl.ClientInterceptor.Impl;
with Test000_Idl.ServerInterceptor.Impl;
with Test000_Globals;

package body Test000_Idl.ORBInitializer.Impl is

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, Test000_Idl.ORBInitializer.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, PortableInterceptor.ORBInitializer.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ---------------
   -- Post_Init --
   ---------------

   procedure Post_Init
     (Self : access Object;
      Info : PortableInterceptor.ORBInitInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Client_A_Ptr : constant Test000_Idl.ClientInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ClientInterceptor.Impl.Object;
      Client_B_Ptr : constant Test000_Idl.ClientInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ClientInterceptor.Impl.Object;
      Client_C_Ptr : constant Test000_Idl.ClientInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ClientInterceptor.Impl.Object;

      Server_A_Ptr : constant Test000_Idl.ServerInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ServerInterceptor.Impl.Object;
      Server_B_Ptr : constant Test000_Idl.ServerInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ServerInterceptor.Impl.Object;
      Server_C_Ptr : constant Test000_Idl.ServerInterceptor.Impl.Object_Ptr
        := new Test000_Idl.ServerInterceptor.Impl.Object;
   begin
      Test000_Idl.ClientInterceptor.Impl.Init (Client_A_Ptr, "A");
      Test000_Idl.ClientInterceptor.Impl.Init (Client_B_Ptr, "B");
      Test000_Idl.ClientInterceptor.Impl.Init (Client_C_Ptr, "C");

      Test000_Idl.ClientInterceptor.Set
       (Test000_Globals.Client_A,
        PolyORB.Smart_Pointers.Entity_Ptr (Client_A_Ptr));
      Test000_Idl.ClientInterceptor.Set
       (Test000_Globals.Client_B,
        PolyORB.Smart_Pointers.Entity_Ptr (Client_B_Ptr));
      Test000_Idl.ClientInterceptor.Set
       (Test000_Globals.Client_C,
        PolyORB.Smart_Pointers.Entity_Ptr (Client_C_Ptr));

      Test000_Globals.Disable_Client_Interceptors;

      PortableInterceptor.ORBInitInfo.add_client_request_interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test000_Globals.Client_A));
      PortableInterceptor.ORBInitInfo.add_client_request_interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test000_Globals.Client_B));
      PortableInterceptor.ORBInitInfo.add_client_request_interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test000_Globals.Client_C));

      Test000_Idl.ServerInterceptor.Impl.Init (Server_A_Ptr, "A");
      Test000_Idl.ServerInterceptor.Impl.Init (Server_B_Ptr, "B");
      Test000_Idl.ServerInterceptor.Impl.Init (Server_C_Ptr, "C");

      Test000_Idl.ServerInterceptor.Set
       (Test000_Globals.Server_A,
        PolyORB.Smart_Pointers.Entity_Ptr (Server_A_Ptr));
      Test000_Idl.ServerInterceptor.Set
       (Test000_Globals.Server_B,
        PolyORB.Smart_Pointers.Entity_Ptr (Server_B_Ptr));
      Test000_Idl.ServerInterceptor.Set
       (Test000_Globals.Server_C,
        PolyORB.Smart_Pointers.Entity_Ptr (Server_C_Ptr));

      Test000_Globals.Disable_Server_Interceptors;

      PortableInterceptor.ORBInitInfo.add_server_request_interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_A));
      PortableInterceptor.ORBInitInfo.add_server_request_interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_B));
      PortableInterceptor.ORBInitInfo.add_server_request_interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_C));
   end Post_Init;

end Test000_Idl.ORBInitializer.Impl;
