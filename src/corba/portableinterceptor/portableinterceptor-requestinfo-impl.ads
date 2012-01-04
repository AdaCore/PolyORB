------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.REQUESTINFO.IMPL                    --
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

with CORBA.Local;
with Dynamic;
with Messaging;
with PolyORB.Requests;

package PortableInterceptor.RequestInfo.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function Get_Request_Id
     (Self : access Object)
      return CORBA.Unsigned_Long;

   function Get_Operation
     (Self : access Object)
      return CORBA.String;

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

   function Get_Response_Expected
     (Self : access Object)
      return CORBA.Boolean;

   function Get_Sync_Scope
     (Self : access Object)
      return Messaging.SyncScope;

   function Get_Reply_Status
     (Self : access Object)
      return ReplyStatus;

   function Get_Forward_Reference
     (Self : access Object)
     return CORBA.Object.Ref;

   function Get_Slot
     (Self : access Object;
      Id   : SlotId)
      return CORBA.Any;

   function Get_Request_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext;

   function Get_Reply_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext;

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : String) return Boolean;

   procedure Init
     (Self       : access Object;
      Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long);
   --  Implementation Note: This procedure initialize a RequestInfo
   --  object. It is specific to PolyORB. You should not use it.

private

   type Object is new CORBA.Local.Object with record
      Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long;
   end record;

end PortableInterceptor.RequestInfo.Impl;
