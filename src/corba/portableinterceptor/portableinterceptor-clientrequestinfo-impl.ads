------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               PORTABLEINTERCEPTOR.CLIENTREQUESTINFO.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with PolyORB.CORBA_P.Interceptors;
with PolyORB.Requests;

with Dynamic;
with Messaging;
with PortableInterceptor.RequestInfo.Impl;

package PortableInterceptor.ClientRequestInfo.Impl is

   type Object is new PortableInterceptor.RequestInfo.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   function Get_Target
     (Self : access Object)
      return CORBA.Object.Ref;

   function Get_Effective_Target
     (Self : access Object)
      return CORBA.Object.Ref;

   function Get_Effective_Profile
     (Self : access Object)
      return IOP.TaggedProfile;

   function Get_Received_Exception
     (Self : access Object)
      return CORBA.Any;

   function Get_Received_Exception_Id
     (Self : access Object)
      return CORBA.RepositoryId;

   function Get_Effective_Component
     (Self : access Object;
      Id   : IOP.ComponentId)
      return IOP.TaggedComponent;

   function Get_Effective_Components
     (Self : access Object;
      Id   : IOP.ComponentId)
      return IOP.TaggedComponentSeq;

   function Get_Request_Policy
     (Self     : access Object;
      IDL_Type : CORBA.PolicyType)
      return CORBA.Policy.Ref;

   procedure Add_Request_Service_Context
     (Self            : access Object;
      Service_Context : IOP.ServiceContext;
      Replace         : CORBA.Boolean);

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : String) return Boolean;

   procedure Init
     (Self       : access Object;
      Point      :
        PolyORB.CORBA_P.Interceptors.Client_Interception_Point;
      Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long;
      Target     : CORBA.Object.Ref);
   --  Implementation Note: This procedure initialize a ClientRequestInfo
   --  object. It is specific to PolyORB. You should not use it.

private

   type Object is new PortableInterceptor.RequestInfo.Impl.Object with record
      Point   : PolyORB.CORBA_P.Interceptors.Client_Interception_Point;
      Target  : CORBA.Object.Ref;
      Request : PolyORB.Requests.Request_Access;
   end record;

   --  Derived from RequestInfo

   function Get_Arguments
     (Self : access Object)
      return Dynamic.ParameterList;

   function Get_Exceptions
     (Self : access Object)
      return Dynamic.ExceptionList;

   function Get_Contexts
     (Self : access Object)
      return Dynamic.ContextList;

   function Get_Operation_Context
     (Self : access Object)
      return Dynamic.RequestContext;

   function Get_Result
     (Self : access Object)
      return CORBA.Any;

   function Get_Sync_Scope
     (Self : access Object)
      return Messaging.SyncScope;

   function Get_Reply_Status
     (Self : access Object)
      return ReplyStatus;

   function Get_Forward_Reference
     (Self : access Object)
     return CORBA.Object.Ref;

   function Get_Request_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext;

   function Get_Reply_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext;

end PortableInterceptor.ClientRequestInfo.Impl;
