------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E I N T E R C E P T O R . R E Q U E S T I N F O       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with CORBA.Object;
with Dynamic;

package PortableInterceptor.RequestInfo is

   type Local_Ref is new CORBA.Object.Ref with null record;

   function Get_Request_Id
     (Self : in Local_Ref)
      return CORBA.Unsigned_Long;

   function Get_Operation
     (Self : in Local_Ref)
      return CORBA.String;

   function Get_Arguments
     (Self : in Local_Ref)
      return Dynamic.ParameterList;

   function Get_Exceptions
     (Self : in Local_Ref)
      return Dynamic.ExceptionList;

   function Get_Contexts
     (Self : in Local_Ref)
      return Dynamic.ContextList;

   function Get_Operation_Context
     (Self : in Local_Ref)
      return Dynamic.RequestContext;

   function Get_Result
     (Self : in Local_Ref)
      return CORBA.Any;

   function Get_Response_Expected
     (Self : in Local_Ref)
      return CORBA.Boolean;

--   function Get_Sync_Scope
--     (Self : in Local_Ref)
--      return Messaging.SyncScope;

   function Get_Reply_Status
     (Self : in Local_Ref)
      return ReplyStatus;

   function Get_Forward_Reference
     (Self : in Local_Ref)
      return CORBA.Object.Ref;

   function Get_Slot
     (Self : in Local_Ref;
      Id   : in SlotId)
      return CORBA.Any;

--   function Get_Request_Service_Context
--     (Self : in Local_Ref;
--      Id   : in IOP.ServiceId)
--      return IOP.ServiceContext;
--
--   function Get_Reply_Service_Context
--     (Self : in Local_Ref;
--      Id   : in IOP.ServiceId)
--      return IOP.ServiceContext;

   --  Repository_Ids

   RequestInfo_Root_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/RequestInfo";

   Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & ":1.0";

   Request_Id_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/request_id:1.0";

   Operation_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/operation:1.0";

   Arguments_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/arguments:1.0";

   Exceptions_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/exceptions:1.0";

   Contexts_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/contexts:1.0";

   Operation_Context_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/operation_context:1.0";

   Result_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/result:1.0";

   Response_Expected_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/response_expected:1.0";

   Sync_Scope_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/sync_scope:1.0";

   Reply_Status_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/reply_status:1.0";

   Forward_Reference_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/forward_reference:1.0";

   Get_Slot_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/get_slot:1.0";

   Get_Request_Service_Context_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/get_request_service_context:1.0";

   Get_Reply_Service_Context_Repository_Id : constant Standard.String
     := RequestInfo_Root_Repository_Id & "/get_reply_service_context:1.0";

end PortableInterceptor.RequestInfo;
