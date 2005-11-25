------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.SERVERREQUESTINFO                   --
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

with CORBA.Policy;
with IOP;
with PortableInterceptor.RequestInfo;

package PortableInterceptor.ServerRequestInfo is

   type Local_Ref is
     new PortableInterceptor.RequestInfo.Local_Ref with null record;

   function Get_Sending_Exception (Self : in Local_Ref) return CORBA.Any;

   function Get_Server_Id (Self : in Local_Ref) return ServerId;
   --  Not implemented.

   function Get_ORB_Id (Self : in Local_Ref) return ORBId;
   --  Not implemented.

   function Get_Adapter_Name (Self : in Local_Ref) return AdapterName;

   function Get_Object_Id (Self : in Local_Ref) return ObjectId;

   function Get_Adapter_Id (Self : in Local_Ref) return AdapterId;
   --  Not implemented.

   function Get_Target_Most_Derived_Interface
     (Self : in Local_Ref)
      return CORBA.RepositoryId;
   --  Not implemented.

   function Get_Server_Policy
     (Self   : in Local_Ref;
      A_Type : in CORBA.PolicyType)
      return CORBA.Policy.Ref;
   --  Not implemented.

   procedure Set_Slot
     (Self : in Local_Ref;
      Id   : in PortableInterceptor.SlotId;
      Data : in CORBA.Any);
   --  Not implemented.

   function Target_Is_A
     (Self : in Local_Ref;
      Id   : in CORBA.RepositoryId)
      return CORBA.Boolean;
   --  Not implemented.

   procedure Add_Reply_Service_Context
     (Self            : in Local_Ref;
      Service_Context : in IOP.ServiceContext;
      Replace         : in CORBA.Boolean);

   --  Repository_Ids

   ServerRequestInfo_Root_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ServerRequestInfo";

   Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & ":1.0";

   Sending_Exception_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/sending_exception:1.0";

   Server_Id_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/server_id:1.0";

   ORB_Id_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/orb_id:1.0";

   Adapter_Name_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/adapter_name:1.0";

   Object_Id_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/object_id:1.0";

   Adapter_Id_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/adapter_id:1.0";

   Target_Most_Derived_Interface_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id
          & "/target_most_derived_interface:1.0";

   Get_Server_Policy_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/get_server_policy:1.0";

   Set_Slot_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/set_slot:1.0";

   Target_Is_A_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id & "/target_is_a:1.0";

   Add_Reply_Service_Context_Repository_Id : constant Standard.String
     := ServerRequestInfo_Root_Repository_Id
          & "/add_reply_service_context:1.0";

end PortableInterceptor.ServerRequestInfo;
