------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              PORTABLEINTERCEPTOR.SERVERREQUESTINTERCEPTOR                --
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

with PortableInterceptor.ServerRequestInfo;
with PortableInterceptor.Interceptor;

package PortableInterceptor.ServerRequestInterceptor is

   type Local_Ref is
     new PortableInterceptor.Interceptor.Local_Ref with null record;

   procedure Receive_Request_Service_Contexts
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Receive_Request
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Send_Reply
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Send_Exception
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref);

   procedure Send_Other
     (Self : in Local_Ref;
      RI   : in PortableInterceptor.ServerRequestInfo.Local_Ref);

   --  Repository_Ids

   ServerRequestInterceptor_Root_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ServerRequestInterceptor";

   Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id & ":1.0";

   Receive_Request_Service_Contexts_Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id
          & "/receive_request_service_contexts:1.0";

   Receive_Request_Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id & "/receive_request:1.0";

   Send_Reply_Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id & "/send_reply:1.0";

   Send_Exception_Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id & "/send_exception:1.0";

   Send_Other_Repository_Id : constant Standard.String
     := ServerRequestInterceptor_Root_Repository_Id & "/send_other:1.0";

end PortableInterceptor.ServerRequestInterceptor;
