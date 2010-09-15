------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         T E S T 0 0 2 _ O R B _ I N I T I A L I Z E R . I M P L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
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
      Test_Slot := Allocate_Slot_Id (Info);

      Test002_Client_Interceptor.Set
       (Test002_Globals.Client,
        PolyORB.Smart_Pointers.Entity_Ptr (Client_Ptr));
      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
       (Info,
        PortableInterceptor.ClientRequestInterceptor.Local_Ref
         (Test002_Globals.Client));

      Test002_Server_Interceptor.Set
       (Test002_Globals.Server,
        PolyORB.Smart_Pointers.Entity_Ptr (Server_Ptr));
      PortableInterceptor.ORBInitInfo.Add_Server_Request_Interceptor
       (Info,
        PortableInterceptor.ServerRequestInterceptor.Local_Ref
         (Test002_Globals.Server));
   end Post_Init;

end Test002_ORB_Initializer.Impl;
