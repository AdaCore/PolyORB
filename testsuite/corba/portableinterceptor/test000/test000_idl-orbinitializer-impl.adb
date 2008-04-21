------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      T E S T 0 0 0 _ I D L . O R B I N I T I A L I Z E R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
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
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test000_Idl.ORBInitializer.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.ORBInitializer.Repository_Id)
        or else False;

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

      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test000_Globals.Client_A));
      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test000_Globals.Client_B));
      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
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

      PortableInterceptor.ORBInitInfo.Add_Server_Request_Interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_A));
      PortableInterceptor.ORBInitInfo.Add_Server_Request_Interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_B));
      PortableInterceptor.ORBInitInfo.Add_Server_Request_Interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test000_Globals.Server_C));
   end Post_Init;

end Test000_Idl.ORBInitializer.Impl;
