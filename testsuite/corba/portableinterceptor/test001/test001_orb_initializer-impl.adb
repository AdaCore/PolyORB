------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         T E S T 0 0 1 _ O R B _ I N I T I A L I Z E R . I M P L          --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Impl;
with PolyORB.Utils.Report;

with IOP.Codec;
with IOP.CodecFactory;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.ORBInitializer;
with PortableInterceptor.ServerRequestInterceptor;

with Test001_Client_Interceptor.Impl;
with Test001_Globals;
with Test001_Server_Interceptor.Impl;

package body Test001_ORB_Initializer.Impl is

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
          (Logical_Type_Id, Test001_ORB_Initializer.Repository_Id)
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

      Client_Ref : PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Client_Ptr : Test001_Client_Interceptor.Impl.Object_Ptr;

      Server_Ref : PortableInterceptor.ServerRequestInterceptor.Local_Ref;
      Server_Ptr : Test001_Server_Interceptor.Impl.Object_Ptr;

      Factory    : IOP.CodecFactory.Local_Ref;

   begin
      PolyORB.Utils.Report.New_Test ("ORBInitInfo Interface");

      Client_Ptr := new Test001_Client_Interceptor.Impl.Object;
      PortableInterceptor.ClientRequestInterceptor.Set
        (Client_Ref, CORBA.Impl.Object_Ptr (Client_Ptr));
      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
        (Info, Client_Ref);

      Server_Ptr := new Test001_Server_Interceptor.Impl.Object;
      PortableInterceptor.ServerRequestInterceptor.Set
        (Server_Ref, CORBA.Impl.Object_Ptr (Server_Ptr));
      PortableInterceptor.ORBInitInfo.Add_Server_Request_Interceptor
        (Info, Server_Ref);

      begin
         Factory := PortableInterceptor.ORBInitInfo.Get_Codec_Factory (Info);
         Test001_Globals.Test_Codec :=
           IOP.CodecFactory.Create_Codec
           (Factory, (IOP.Encoding_CDR_Encaps, 1, 2));
         PolyORB.Utils.Report.Output
           ("[post_init] ORBInitInfo::codec_factory", True);
      exception
         when others =>
            PolyORB.Utils.Report.Output
              ("[post_init] ORBInitInfo::codec_factory", False);
      end;

      Test001_Globals.Test_Request_Context :=
        (654321,
         IOP.ContextData
         (IOP.Codec.Encode_Value
          (Test001_Globals.Test_Codec,
           CORBA.To_Any (CORBA.Unsigned_Long'(1)))));
      Test001_Globals.Test_Reply_Context :=
        (765432,
         IOP.ContextData
         (IOP.Codec.Encode_Value
          (Test001_Globals.Test_Codec,
           CORBA.To_Any (CORBA.Unsigned_Long'(2)))));
   end Post_Init;

end Test001_ORB_Initializer.Impl;
