------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               PORTABLEINTERCEPTOR.SERVERREQUESTINFO.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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
with CORBA.Policy;
with PolyORB.Binding_Data;
with PolyORB.CORBA_P.Interceptors;
with PolyORB.Requests;
with PortableServer;

with Dynamic;
with PortableInterceptor.RequestInfo.Impl;

package PortableInterceptor.ServerRequestInfo.Impl is

   type Object is
     new PortableInterceptor.RequestInfo.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  Initialize object implementation.
   procedure Init
     (Self         : access Object;
      Point        :
        PolyORB.CORBA_P.Interceptors.Server_Interception_Point;
      Servant      : PortableServer.Servant;
      Request      : PolyORB.Requests.Request_Access;
      Request_Id   : CORBA.Unsigned_Long;
      Profile      : PolyORB.Binding_Data.Profile_Access;
      Args_Present : Boolean);

   function Get_Sending_Exception (Self : access Object) return CORBA.Any;

   function Get_Server_Id (Self : access Object) return ServerId;

   function Get_ORB_Id (Self : access Object) return ORBId;

   function Get_Adapter_Name (Self : access Object) return AdapterName;

   function Get_Object_Id (Self : access Object) return ObjectId;

   function Get_Adapter_Id (Self : access Object)
                           return CORBA.IDL_SEQUENCES.OctetSeq;

   function Get_Target_Most_Derived_Interface
     (Self : access Object)
      return CORBA.RepositoryId;

   function Get_Server_Policy
     (Self   : access Object;
      A_Type : CORBA.PolicyType)
      return CORBA.Policy.Ref;

   procedure Set_Slot
     (Self : access Object;
      Id   : PortableInterceptor.SlotId;
      Data : CORBA.Any);

   function Target_Is_A
     (Self : access Object;
      Id   : CORBA.RepositoryId)
      return CORBA.Boolean;

   procedure Add_Reply_Service_Context
     (Self            : access Object;
      Service_Context : IOP.ServiceContext;
      Replace         : CORBA.Boolean);

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean;

private

   type Object is
     new PortableInterceptor.RequestInfo.Impl.Object with
   record
      Point        : PolyORB.CORBA_P.Interceptors.Server_Interception_Point;
      Servant      : PortableServer.Servant;
      Request      : PolyORB.Requests.Request_Access;
      Profile      : PolyORB.Binding_Data.Profile_Access;
      Args_Present : Boolean;
   end record;

   --  Derived from RequestInfo.

   function Get_Arguments (Self : access Object) return Dynamic.ParameterList;

   function Get_Exceptions (Self : access Object) return Dynamic.ExceptionList;

   function Get_Contexts (Self : access Object) return Dynamic.ContextList;

   function Get_Operation_Context
     (Self : access Object)
      return Dynamic.RequestContext;

   function Get_Result
     (Self : access Object)
      return CORBA.Any;

   function Get_Reply_Status
     (Self : access Object)
      return ReplyStatus;

   function Get_Forward_Reference
     (Self : access Object)
     return CORBA.Object.Ref;

   function Get_Reply_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext;

end PortableInterceptor.ServerRequestInfo.Impl;
