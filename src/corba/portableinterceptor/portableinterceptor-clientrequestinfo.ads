------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.CLIENTREQUESTINFO                   --
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
with CORBA.Policy;

with IOP;
with PortableInterceptor.RequestInfo;

package PortableInterceptor.ClientRequestInfo is

   type Local_Ref is
      new PortableInterceptor.RequestInfo.Local_Ref with null record;

   function Get_Target
     (Self : in Local_Ref)
      return CORBA.Object.Ref;

   function Get_Effective_Target
     (Self : in Local_Ref)
      return CORBA.Object.Ref;

--   function Get_Effective_Profile
--     (Self : in Local_Ref)
--      return IOP.TaggedProfile;

   function Get_Received_Exception
     (Self : in Local_Ref)
      return CORBA.Any;

   function Get_Received_Exception_Id
     (Self : in Local_Ref)
      return CORBA.RepositoryId;

--   function Get_Effective_Component
--     (Self : in Local_Ref;
--      Id   : in IOP.ComponentId)
--      return IOP.TaggedComponent;
--
--   function Get_Effective_Components
--     (Self : in Local_Ref;
--      Id   : in IOP.ComponentId)
--      return IOP.TaggedComponentSeq;

   function Get_Request_Policy
     (Self     : in Local_Ref;
      IDL_Type : in CORBA.PolicyType)
      return CORBA.Policy.Ref;

   procedure Add_Request_Service_Context
     (Self            : in Local_Ref;
      Service_Context : in IOP.ServiceContext;
      Replace         : in CORBA.Boolean);

   --  Repository_Ids

   ClientRequestInfo_Root_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ClientRequestInfo";

   Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & ":1.0";

   Target_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/target:1.0";

   Effective_Target_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/effective_target:1.0";

   Effective_Profile_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/effective_profile:1.0";

   Received_Exception_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/received_exception:1.0";

   Received_Exception_Id_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/received_exception_id:1.0";

   Get_Effective_Component_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/get_effective_component:1.0";

   Get_Effective_Components_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/get_effective_components:1.0";

   Get_Request_Policy_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id & "/get_request_policy:1.0";

   Add_Request_Service_Context_Repository_Id : constant Standard.String
     := ClientRequestInfo_Root_Repository_Id
     & "/add_request_service_context:1.0";

end PortableInterceptor.ClientRequestInfo;
