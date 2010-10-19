------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.ORBINITINFO.IMPL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.Local;

package PortableInterceptor.ORBInitInfo.Impl is

   pragma Elaborate_Body;

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   procedure Init (Self : access Object);

   procedure Post_Init_Done (Self : access Object);
   --  Called once the initialization is complete. It is required for
   --  raising exception in case of call object operations after
   --  initialization complete. XXX reword this comment

   function Get_Arguments
     (Self : access Object)
     return CORBA.IDL_SEQUENCES.StringSeq;

   function Get_ORB_Id (Self : access Object) return CORBA.String;

   function Get_Codec_Factory
     (Self : access Object)
      return IOP.CodecFactory.Local_Ref;

   procedure Register_Initial_Reference
     (Self : access Object;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId;
      Obj  : CORBA.Object.Ref);

   function Resolve_Initial_References
     (Self : access Object;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId)
      return CORBA.Object.Ref;

   procedure Add_Client_Request_Interceptor
     (Self        : access Object;
      Interceptor :
        PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   procedure Add_Server_Request_Interceptor
     (Self        : access Object;
      Interceptor :
        PortableInterceptor.ServerRequestInterceptor.Local_Ref);

   procedure Add_IOR_Interceptor
     (Self        : access Object;
      Interceptor : PortableInterceptor.IORInterceptor.Local_Ref);

   function Allocate_Slot_Id
     (Self : access Object)
     return PortableInterceptor.SlotId;

   procedure Register_Policy_Factory
     (Self           : access Object;
      IDL_Type       : CORBA.PolicyType;
      Policy_Factory : PortableInterceptor.PolicyFactory.Local_Ref);

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : String) return Boolean;

private

   type Object is new CORBA.Local.Object with record
      Post_Init_Done : Boolean := False;
   end record;

end PortableInterceptor.ORBInitInfo.Impl;
