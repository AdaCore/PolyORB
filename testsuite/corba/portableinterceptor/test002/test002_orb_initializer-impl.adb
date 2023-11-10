------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         T E S T 0 0 2 _ O R B _ I N I T I A L I Z E R . I M P L          --
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
with PortableInterceptor.ORBInitializer;
with PortableInterceptor.ServerRequestInterceptor;

with PolyORB.Smart_Pointers;

with Test002_Globals;
with Test002_Client_Interceptor.Impl;
with Test002_Server_Interceptor.Impl;

package body Test002_ORB_Initializer.Impl is

   use PortableInterceptor.ORBInitInfo;
   use Test002_Globals;

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
          (Logical_Type_Id, Test002_ORB_Initializer.Repository_Id)
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

      Client_Ptr : constant Test002_Client_Interceptor.Impl.Object_Ptr
        := new Test002_Client_Interceptor.Impl.Object;
      Server_Ptr : constant Test002_Server_Interceptor.Impl.Object_Ptr
        := new Test002_Server_Interceptor.Impl.Object;

   begin
      Test_Slot := allocate_slot_id (Info);

      Test002_Client_Interceptor.Set
       (Test002_Globals.Client,
        PolyORB.Smart_Pointers.Entity_Ptr (Client_Ptr));
      PortableInterceptor.ORBInitInfo.add_client_request_interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test002_Globals.Client));

      Test002_Server_Interceptor.Set
       (Test002_Globals.Server,
        PolyORB.Smart_Pointers.Entity_Ptr (Server_Ptr));
      PortableInterceptor.ORBInitInfo.add_server_request_interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test002_Globals.Server));
   end Post_Init;

end Test002_ORB_Initializer.Impl;
